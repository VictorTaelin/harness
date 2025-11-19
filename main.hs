{-# LANGUAGE BangPatterns #-}

import Control.Monad (forM_)
import Data.Bits (shiftL)
import Data.IORef
import Data.List (foldl', elemIndex)
import System.CPUTime
import Text.ParserCombinators.ReadP
import Text.Printf
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M

debug :: Bool
debug = False

-- Types
-- =====

type Name = Int

data Term
  = Var !Name
  | Ref !Name
  | Lam !Name !Term
  | App !Term !Term
  | Ctr !Name ![Term]
  | Mat !Name !Term !Term
  deriving (Eq)

data Book = Book (M.Map Name Term)

data Env = Env
  { env_book    :: !Book
  , env_inters  :: !(IORef Int)
  , env_new_id  :: !(IORef Int)
  , env_sub_map :: !(IORef (IM.IntMap Term))
  }

-- Showing
-- =======

instance Show Term where
  show (Var k)       = int_to_name k
  show (Ref k)       = "@" ++ int_to_name k
  show (Lam k f)     = "λ" ++ int_to_name k ++ "." ++ show f
  show (App f x)     = app f [x] where
    app (App f x) xs = app f (x : xs)
    app f         xs = "(" ++ unwords (map show (f : xs)) ++ ")"
  show (Ctr k xs)    = "#" ++ int_to_name k ++ "{" ++ unwords (map show xs) ++ "}"
  show (Mat k c d)   = "λ{#" ++ int_to_name k ++ ":" ++ show c ++ ";" ++ show d ++ "}"

instance Show Book where
  show (Book m) = unlines ["@" ++ int_to_name k ++ " = " ++ show ct | (k, ct) <- M.toList m]

-- Name Encoding/Decoding
-- ======================

alphabet :: String
alphabet = "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789$"

alphabet_first :: String
alphabet_first = filter (`notElem` "_0123456789") alphabet

name_to_int :: String -> Int
name_to_int = foldl' (\acc c -> (acc `shiftL` 6) + idx c) 0
  where idx c = maybe (error "bad name char") id (elemIndex c alphabet)

int_to_name :: Int -> String
int_to_name 0 = "_"
int_to_name n = reverse (go n)
  where go 0 = ""
        go m = let (q,r) = m `divMod` 64 in alphabet !! r : go q

-- Parsing
-- =======

parse_lexeme :: ReadP a -> ReadP a
parse_lexeme p = skipSpaces *> p

parse_name :: ReadP String
parse_name = parse_lexeme $ do
  head <- satisfy (`elem` alphabet_first)
  tail <- munch (`elem` alphabet)
  return (head : tail)

parse_term :: ReadP Term
parse_term = parse_term_base

parse_term_base :: ReadP Term
parse_term_base = parse_lexeme $ choice
  [ parse_lam
  , parse_par
  , parse_ctr
  , parse_mat
  , parse_ref
  , parse_var
  ]

parse_par :: ReadP Term
parse_par = do
  parse_lexeme (char '(')
  t <- parse_term
  ts <- many parse_term
  parse_lexeme (char ')')
  return (foldl' App t ts)

parse_lam :: ReadP Term
parse_lam = do
  parse_lexeme (char 'λ')
  k <- parse_name
  parse_lexeme (char '.')
  t <- parse_term
  return (Lam (name_to_int k) t)

parse_ctr :: ReadP Term
parse_ctr = do
  parse_lexeme (char '#')
  k <- parse_name
  between (parse_lexeme (char '{')) (parse_lexeme (char '}')) $ do
    args <- sepBy parse_term (optional (parse_lexeme (char ',')))
    return (Ctr (name_to_int k) args)

parse_mat :: ReadP Term
parse_mat = do
  parse_lexeme (char 'λ')
  between (parse_lexeme (char '{')) (parse_lexeme (char '}')) $ do
    parse_lexeme (char '#')
    k <- parse_name
    parse_lexeme (char ':')
    c <- parse_term
    optional (parse_lexeme (char ';'))
    d <- parse_term
    optional (parse_lexeme (char ';'))
    return (Mat (name_to_int k) c d)

parse_ref :: ReadP Term
parse_ref = do
  parse_lexeme (char '@')
  k <- parse_name
  return (Ref (name_to_int k))

parse_var :: ReadP Term
parse_var = do
  k <- parse_name
  return (Var (name_to_int k))

parse_func :: ReadP (Name, Term)
parse_func = do
  parse_lexeme (char '@')
  k <- parse_name
  parse_lexeme (char '=')
  f <- parse_term
  return (name_to_int k, f)

parse_book :: ReadP Book
parse_book = do
  skipSpaces
  funcs <- many parse_func
  skipSpaces
  eof
  return $ Book (M.fromList funcs)

read_term :: String -> Term
read_term s = case readP_to_S (parse_term <* skipSpaces <* eof) s of
  [(t, "")] -> t
  _         -> error "bad-parse"

read_book :: String -> Book
read_book s = case readP_to_S parse_book s of
  [(b, "")] -> b
  _         -> error "bad-parse"

-- WNF
-- ===

data Frame
  = FApp Term
  | FMat Term
  deriving Show

type Stack = [Frame]

wnf :: Env -> Stack -> Term -> IO Term
wnf e s (Var k)   = wnf_sub e s k
wnf e s (App f x) = wnf e (FApp x : s) f
wnf e s (Ref k)   = wnf_ref e s k
wnf e s f         = wnf_ret e s f

wnf_ret :: Env -> Stack -> Term -> IO Term
wnf_ret e []      v = return v
wnf_ret e (x : s) v = case x of
  FApp a   -> wnf_app e s v a
  FMat f  -> wnf_app_mat e s f v

wnf_sub :: Env -> Stack -> Name -> IO Term
wnf_sub e s k = do
  mt <- take_sub e k
  case mt of
    Just t  -> wnf e s t
    Nothing -> wnf_ret e s (Var k)

wnf_app :: Env -> Stack -> Term -> Term -> IO Term
wnf_app e s f a = do
  case f of
    Lam {} -> wnf_app_lam e s f a
    Mat {} -> wnf e (FMat f : s) a
    _      -> wnf_ret e s (App f a)

wnf_app_lam :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_lam e s (Lam fx ff) v = do
  inc_inters e
  subst e fx v
  wnf e s ff

wnf_app_mat :: Env -> Stack -> Term -> Term -> IO Term
wnf_app_mat e s f@(Mat mn mc md) a = case a of
  Ctr cn args -> do
    inc_inters e
    if mn == cn
      then app e s mc args
      else app e s md args
      where app e s f args = wnf e (map FApp args ++ s) f
  _ -> do
    wnf_ret e s (App f a)


wnf_ref :: Env -> Stack -> Name -> IO Term
wnf_ref e s k = do
  let (Book m) = env_book e
  case M.lookup k m of
    Just f  -> do
      inc_inters e
      g <- alloc e f
      wnf e s g
    Nothing -> error $ "UndefinedReference: " ++ int_to_name k

-- Environment
-- ===========

new_env :: Book -> IO Env
new_env bk = do
  itr <- newIORef 0
  ids <- newIORef 1
  sub <- newIORef IM.empty
  return $ Env bk itr ids sub

inc_inters :: Env -> IO ()
inc_inters e = do
  !n <- readIORef (env_inters e)
  writeIORef (env_inters e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (env_new_id e)
  writeIORef (env_new_id e) (n + 1)
  return ((n `shiftL` 6) + 63)

subst :: Env -> Name -> Term -> IO ()
subst e k v = modifyIORef' (env_sub_map e) (IM.insert (k) v)

take_sub :: Env -> Name -> IO (Maybe Term)
take_sub e k = do
  !m <- readIORef (env_sub_map e)
  case IM.lookup k m of
    Nothing -> do
      return Nothing
    Just v  -> do
      writeIORef (env_sub_map e) (IM.delete k m)
      return (Just v)


-- Allocation
-- ==========

alloc :: Env -> Term -> IO Term
alloc e term = go IM.empty term where
  go m (Var k) = return $ Var (IM.findWithDefault k k m)
  go m (App f x) = App <$> go m f <*> go m x
  go m (Lam k f) = do
    k' <- fresh e
    f' <- go (IM.insert k k' m) f
    return $ Lam k' f'
  go m (Ctr k args) = Ctr k <$> mapM (go m) args
  go m (Mat k c d) = Mat k <$> go m c <*> go m d
  go m (Ref k) = return (Ref k)

-- Normalization
-- =============

snf :: Env -> Int -> Term -> IO Term
snf e d x = do
  !x' <- wnf e [] x
  case x' of
    Var k      -> return $ Var k
    Lam k f    -> Lam d <$> snf e d f
    App f x    -> App <$> snf e d f <*> snf e d x
    Ctr k xs   -> Ctr k <$> mapM (snf e d) xs
    Mat k c d' -> Mat k <$> snf e d c <*> snf e d d'
    Ref k      -> return (Ref k)

-- Tests
-- =====

book :: String
book = unlines
  [ "@id  = λa.a"
  , "@not = λ{#Z: #S{#Z{}}; λp. #Z{}}"
  , "@dbl = λ{#Z: #Z{}; λp. #S{#S{(@dbl p)}}}"
  , "@add = λ{#Z: λb.b; λa. λb. #S{(@add a b)}}"
  , "@prd = λ{#Z: #Z{}; λp. p}"
  ]

tests :: [(String,String)]
tests =
  [ ("#Z{}", "#Z{}")
  , ("(@dbl #S{#S{#Z{}}})", "#S{#S{#S{#S{#Z{}}}}}")
  , ("(@add #S{#S{#Z{}}} #S{#S{#Z{}}})", "#S{#S{#S{#S{#Z{}}}}}")
  ]

test :: IO ()
test = forM_ tests $ \ (src, exp) -> do
  !env <- new_env $ read_book book
  !det <- show <$> snf env 1 (read_term src)
  !itr <- readIORef (env_inters env)
  if det == exp then do
    putStrLn $ "[PASS] " ++ src ++ " → " ++ det ++ " | #" ++ show itr
  else do
    putStrLn $ "[FAIL] " ++ src
    putStrLn $ "  - expected: " ++ exp
    putStrLn $ "  - detected: " ++ det

-- Main
-- ====

-- run :: String -> String -> IO ()
-- run book_src term_src = do
  -- !env <- new_env $ read_book book_src
  -- !ini <- getCPUTime
  -- !val <- alloc env $ read_term term_src
  -- !val <- snf env 1 val
  -- !end <- getCPUTime
  -- !itr <- readIORef (env_inters env)
  -- !dt  <- return $ fromIntegral (end - ini) / (10^12)
  -- !ips <- return $ fromIntegral itr / dt
  -- putStrLn $ show val
  -- putStrLn $ "- Itrs: " ++ show itr ++ " interactions"
  -- printf "- Time: %.3f seconds\n" (dt :: Double)
  -- printf "- Perf: %.2f M interactions/s\n" (ips / 1000000 :: Double)

main :: IO ()
main = test

{-# LANGUAGE BangPatterns #-}

import Control.Monad (forM_)
import Data.Bits (shiftL)
import Data.IORef
import Data.List (foldl', elemIndex, intercalate)
import System.CPUTime
import System.Environment (getArgs)
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
  { env_book  :: !Book
  , env_count :: !(IORef Int)
  , env_fresh :: !(IORef Int)
  , env_subst :: !(IORef (IM.IntMap Term))
  }

-- Showing
-- =======

instance Show Term where
  show (Var k)         = int_to_name k
  show (Ref k)         = "@" ++ int_to_name k
  show (Lam k f)       = "λ" ++ int_to_name k ++ "." ++ show f
  show (App f@App{} x) = init (show f) ++ "," ++ show x ++ ")"
  show (App f@Lam{} x) = "(" ++ show f ++ ")(" ++ show x ++ ")"
  show (App f x)       = show f ++ "(" ++ show x ++ ")"
  show (Ctr k xs)      = "#" ++ int_to_name k ++ "{" ++ unwords (map show xs) ++ "}"
  show (Mat k c d)     = "λ{#" ++ int_to_name k ++ ":" ++ show c ++ ";" ++ show d ++ "}"

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
parse_term = do
  t <- parse_term_base
  parse_term_suff t

parse_term_base :: ReadP Term
parse_term_base = parse_lexeme $ choice
  [ parse_lam
  , parse_ctr
  , parse_par
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

parse_term_suff :: Term -> ReadP Term
parse_term_suff t = loop <++ return t where
  loop = do
    parse_lexeme (char '(')
    args <- sepBy parse_term (parse_lexeme (char ','))
    parse_lexeme (char ')')
    let t' = foldl' App t args
    parse_term_suff t'

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

-- Environment
-- ===========

new_env :: Book -> IO Env
new_env bk = do
  itr <- newIORef 0
  ids <- newIORef 1
  sub <- newIORef IM.empty
  return $ Env bk itr ids sub

inter :: Env -> IO ()
inter e = do
  !n <- readIORef (env_count e)
  writeIORef (env_count e) (n + 1)

fresh :: Env -> IO Name
fresh e = do
  !n <- readIORef (env_fresh e)
  writeIORef (env_fresh e) (n + 1)
  return $ n

subst :: Env -> Name -> Term -> IO ()
subst e k v = modifyIORef' (env_subst e) (IM.insert (k) v)

take_sub :: Env -> Name -> IO (Maybe Term)
take_sub e k = do
  !m <- readIORef (env_subst e)
  case IM.lookup k m of
    Nothing -> do
      return Nothing
    Just v  -> do
      writeIORef (env_subst e) (IM.delete k m)
      return (Just v)

-- WNF
-- ===

data Frame
  = FApp Term
  | FMat Term
  deriving Show

type Stack = [Frame]

wnf :: Env -> Stack -> Term -> IO Term
wnf = reduce

reduce :: Env -> Stack -> Term -> IO Term
reduce e s (App f x) = reduce e (FApp x : s) f
reduce e s (Var k)   = var e s (Var k)
reduce e s (Ref k)   = ref e s (Ref k)
reduce e s t         = unwind e s t

unwind :: Env -> Stack -> Term -> IO Term
unwind e (FApp a : s) (Lam x f)   = app e s (Lam x f) a
unwind e (FApp a : s) (Mat k f g) = reduce e (FMat (Mat k f g) : s) a
unwind e (FApp a : s) x           = unwind e s (App x a)
unwind e (FMat f : s) x           = mat e s f x
unwind e []           x           = return x

var :: Env -> Stack -> Term -> IO Term
var e s (Var k) = do
  mt <- take_sub e k
  case mt of
    Just t' -> wnf e s t'
    Nothing -> unwind e s (Var k)

ref :: Env -> Stack -> Term -> IO Term
ref e s (Ref k) = do
  let (Book m) = env_book e
  case M.lookup k m of
    Just f -> do
      inter e
      g <- alloc e f
      wnf e s g
    Nothing -> do
      error $ "UndefinedReference: " ++ int_to_name k

app :: Env -> Stack -> Term -> Term -> IO Term
app e s (Lam x f) a = do
  inter e
  subst e x a
  wnf e s f

mat :: Env -> Stack -> Term -> Term -> IO Term
mat e s (Mat k f g) (Ctr c xs) = do
  if k == c then do
    inter e
    wnf e (map FApp xs ++ s) f
  else do
    inter e
    wnf e s (App g (Ctr c xs))
mat e s f x = do
  unwind e s (App f x)

-- Allocation
-- ==========

alloc :: Env -> Term -> IO Term
alloc e term = go IM.empty term where
  go m (Var k) = do
    return $ Var (IM.findWithDefault k k m)
  go m (App f x) = do
    f' <- go m f
    x' <- go m x
    return (App f' x')
  go m (Lam k f) = do
    k' <- fresh e
    f' <- go (IM.insert k k' m) f
    return $ Lam k' f'
  go m (Ctr k args) = do
    args' <- mapM (go m) args
    return (Ctr k args')
  go m (Mat k c d) = do
    c' <- go m c
    d' <- go m d
    return (Mat k c' d')
  go m (Ref k) = do
    return (Ref k)

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
  [ "@id          = λa.a"
  , "@nat_mul2    = λ{#Z:#Z{};λ{#S:λp.#S{#S{@nat_mul2(p)}};λa.a}}"
  , "@nat_add     = λ{#Z:λb.b;λ{#S:λa.λb.#S{@nat_add(a,b)};λa.a}}"
  , "@pred        = λ{#Z:#Z{};λ{#S:λp.p;λa.a}}"
  , "@bin_inc     = λ{#O:λp.#I{p};λ{#I:λp.#O{@bin_inc(p)};λp.p}}"
  , "@bin_dec     = λ{#O:λp.#I{@bin_dec(p)};λ{#I:λp.#O{p};λp.p}}"
  , "@bin_is_zero = λ{#O:λp.@bin_is_zero(p);λ{#I:λp.#F{};λp.#T{}}}"
  , "@bin_dup     = λ{#O:λp.@bin_dup_o(@bin_dup(p));λ{#I:λp.@bin_dup_i(@bin_dup(p));λp.#P{#E{},#E{}}}}"
  , "@bin_dup_o   = λ{#P:λx0.λx1.#P{#O{x0},#O{x1}};λx.x}"
  , "@bin_dup_i   = λ{#P:λx0.λx1.#P{#I{x0},#I{x1}};λx.x}"
  , "@bin_busy    = λx.@bin_busy_0(@bin_dup(@bin_dec(x)))"
  , "@bin_busy_0  = λ{#P:λx0.λx1.@bin_busy_1(@bin_is_zero(x0),x1);λx.x}"
  , "@bin_busy_1  = λ{#T:λx.#T{};λ{#F:λx.@bin_busy(x);λx.x}}"
  ]

tests :: [(String,String)]
tests =
  [ ("#Z{}", "#Z{}")
  , ("@nat_mul2(#S{#S{#Z{}}})", "#S{#S{#S{#S{#Z{}}}}}")
  , ("@nat_add(#S{#S{#Z{}}}, #S{#S{#Z{}}})", "#S{#S{#S{#S{#Z{}}}}}")
  , ("@bin_inc(@bin_inc(@bin_inc(@bin_inc(#O{#O{#O{#O{#E{}}}}}))))", "#O{#O{#I{#O{#E{}}}}}")
  , ("@bin_is_zero(#O{#O{#O{#I{#O{#E{}}}}}})", "#F{}")
  , ("@bin_is_zero(#O{#O{#O{#O{#O{#E{}}}}}})", "#T{}")
  , ("@bin_dup(#O{#I{#O{#O{#E{}}}}})", "#P{#O{#I{#O{#O{#E{}}}}} #O{#I{#O{#O{#E{}}}}}}")
  , ("@bin_busy(#I{#I{#I{#I{#I{#I{#I{#I{#I{#I{#I{#I{#I{#I{#E{}}}}}}}}}}}}}}})", "#T{}")
  ]

test :: IO ()
test = forM_ tests $ \ (src, exp) -> do
  !env <- new_env $ read_book book
  !det <- show <$> snf env 1 (read_term src)
  !itr <- readIORef (env_count env)
  if det == exp then do
    putStrLn $ "[PASS] " ++ src ++ " → " ++ det ++ " | #" ++ show itr
  else do
    putStrLn $ "[FAIL] " ++ src
    putStrLn $ "  - expected: " ++ exp
    putStrLn $ "  - detected: " ++ det

-- Main
-- ====

run :: String -> String -> IO ()
run book_src term_src = do
  !env <- new_env $ read_book book_src
  !ini <- getCPUTime
  !val <- alloc env $ read_term term_src
  !val <- snf env 1 val
  !end <- getCPUTime
  !itr <- readIORef (env_count env)
  !dt  <- return $ fromIntegral (end - ini) / (10^12)
  !ips <- return $ fromIntegral itr / dt
  putStrLn $ show val
  putStrLn $ "- Itrs: " ++ show itr ++ " interactions"
  printf "- Time: %.3f seconds\n" (dt :: Double)
  printf "- Perf: %.2f M interactions/s\n" (ips / 1000000 :: Double)

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fname:_) -> do
      book <- readFile fname
      run book "@main"
    _ -> test

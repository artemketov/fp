{-# LANGUAGE RecordWildCards #-}
                                 -- they make your code clean and clear.
                                 -- Read about this extension here:
                                 -- https://ocharles.org.uk/blog/posts/2014-12-04-record-wildcards.html
 
 
 
module Construction.Internal.Functions
  ( Context (..)        -- make restrictions is good practice. As you can see here,
  , fresh, free, bound  -- we make "public" not all functions, but only Context, fresh, ...
  , reduce, substitute, alpha, beta, eta
  )where
 
import           Construction.Internal.Types (Name, Term (..))
import           Data.Set                    (Set, delete, empty, insert,
                                              member, notMember, singleton,
                                              union)
import           Data.Text                   (pack)
 
 
-- Context is just set of names that are in our context.
type Context = Set Name
 
-- | @fresh@ generates new variable different from every variables in set.
fresh :: Set Name -> Name
fresh conflicts = head . dropWhile (`member` conflicts) $ nameGen -- This is ugly name generator. Make it better.
  where nameGen = [pack $ 'x' : show ind | ind <- [0..] :: [Int]]
 
-- | @free@ finds all free (Amazing!) variables from given term.
free :: Term -> Set Name
free (Var var)           = singleton var
free (App algo arg)      = free algo `union` free arg
free (Lam variable body) = variable `delete` free body
 
-- | @bound@ finds all bounded variables from given term.
-- This function uses RecordWildCards.
-- If you like it refactor @free@ function.
bound :: Term -> Set Name
bound Var{}   = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` bound body
 
-- a[n := b] - substitution
substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n b | var == n = b
                         | otherwise = v
substitute a@App{..} n b = App (substitute algo n b) (substitute arg n b)
substitute lam@Lam{..} n b | variable == n = lam
                           | variable `member` (free b) = substitute (alpha lam (free b)) n b
                           | otherwise = Lam variable (substitute body n b)

-- | alpha reduction
alpha :: Term -> Set Name -> Term
alpha v@Var{..} s = v
alpha a@App{..} s = App (alpha algo s) (alpha arg s)
alpha lam@Lam{..} s | variable `notMember` s = Lam variable (alpha body (variable `insert` s))
                    | otherwise = Lam freshVariable (alpha (substitute body variable (Var freshVariable)) s) where
                                      freshVariable = fresh (s `union` (free lam))



-- | beta reduction
beta :: Term -> Term
beta (App (Lam variable body) arg) = substitute (beta body) variable (beta arg)
beta a@App{..} = App (beta algo) (beta arg)
beta lam@Lam{..} = Lam variable (beta body)
beta t = t

-- | eta reduction
eta :: Term -> Term
eta lam@(Lam var1 (App algo (Var var2))) | var1 == var2 && var1 `notMember` (free algo) = algo
                                         | otherwise = lam
eta t = t

instance Eq Term where

  (==) (Var v) (Var b) | v == b = True
                       | otherwise = False
  (==) (App a a') (App b b') = (a == b) && (a' == b')
  (==) (Lam var1 var2) (Lam var3 var4) = (substitute var2 var1 freshVariable) == (substitute var4 var3 freshVariable) where freshVariable = Var $ fresh (free var2 `union` free var4)
  (==) a b = False
                                              
                                               

-- | reduce term
reduce :: Term -> Term
reduce term = let term' = beta term
              in if term' == term
                 then eta term
                 else reduce term'

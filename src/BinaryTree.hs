module BinaryTree (Bin(..), insert) where

data Bin a
    = Branch a (Bin a) (Bin a) 
    | Nil
    deriving (Eq, Show)

insert :: Ord a => a -> Bin a -> Bin a
insert x Nil = Branch x Nil Nil
insert x (Branch a l r)
    | x < a = Branch a (insert x l) r
    | x > a = Branch a l (insert x r)
    | otherwise = Branch a l r

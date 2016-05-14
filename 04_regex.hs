import Data.List

data Reg = Chr Char
         | Seq Reg Reg
         | Alt Reg Reg
         | Rep Reg
         | Eps

parts :: [a] -> [([a],[a])]
parts s = zip (inits s) (tails s)

match :: Reg -> String -> Bool
match (Chr c) s = [c] == s
match (Seq l r) s = any (\(x,y) -> match l x && match r y) $ parts s
match (Alt l r) s = match l s || match r s
match (Rep e) s = any (all $ match e) $ cuts s where
  cuts [] = [[]]
  cuts xs = tail (parts xs) >>= \(as,bs) -> (as:) <$> cuts bs
match Eps s = null s

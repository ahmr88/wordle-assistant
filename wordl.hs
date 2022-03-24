import Data.List

allW = lines <$> readFile "wordle.txt"

sol = "renew"

guesses = ["slate", "hound"]

data Guess a  = Elim a | Contains a | At a Int deriving (Show, Eq)

unwrap (Elim a) = Just a
unwrap (Contains a) = Just a
unwrap _ = Nothing

unwrapAt (At a i) = Just (a, i)
unwrapAt _ = Nothing

isElim (Elim a) = True
isElim _ = False

isCont (Contains a) = True
isCont _ = False

isAt (At a i) = True
isAt _ = False

getInfo sol guess = zipWith aux guess $ zip [0..] sol
  where aux g (i, s)
          | g == s = At g i
          | g `elem` sol = Contains g
          | otherwise = Elim g

guessesInfo sol = concatMap (getInfo sol)


poss :: [String] -> [Guess Char] -> [String]
poss wrds gs = filter (\cand -> all (`elem` zip [0..] cand) ats) 
             $ filter (\cand -> all (`elem` cand) conts) 
             $ filter (\cand -> not $ any (`elem` cand) elims) 
             $ sort wrds
  where elims = [x | Just x <- unwrap <$> filter isElim gs]
        conts = [x | Just x <- unwrap <$> filter isCont gs]
        ats   = [(i,c) | Just (c,i) <- unwrapAt <$> filter isAt   gs]


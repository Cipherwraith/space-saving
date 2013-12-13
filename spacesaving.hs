module Main where
import qualified Data.Map.Lazy as M
import Data.Char (toLower)
import Data.List (minimumBy, maximumBy)


spaceSaving :: (Ord a) 
            => Int              
            -> [a]              
            -> M.Map a Integer  


spaceSaving k = spaceSave M.empty 
  where
    spaceSave m [] = m
    spaceSave m (s:ss) 
      | M.member s m = update $ M.adjust (+ 1) s m 
      | M.size m < k = update $ M.insert s 1 m 
      | otherwise    = update $ updateLowestKey m s
     where
      update f = spaceSave f ss


updateLowestKey :: (Ord k) 
                 => M.Map k Integer 
                 -> k               
                 -> M.Map k Integer 


updateLowestKey m s = M.insert s (lowestValue + 1) lowestKeyRemoved
  where
    (lowestKey, lowestValue) = minimum' m
    lowestKeyRemoved = M.delete lowestKey m
   


minimum' :: (Ord a) => M.Map k a -> (k, a)
minimum' = minimumBy comp . M.toList

maximum' :: (Ord a) => M.Map k a -> (k, a)
maximum' = maximumBy comp . M.toList

comp (_, x) (_, y) = compare x y


main :: IO ()
main = do
  contents <- getContents
  let pFile = words . map toLower $ contents
  print . maximum' $ spaceSaving 100 pFile


import Data.List (sortBy, maximumBy)
import Data.Ord (comparing)

tpl :: [(Int, (Int, Int))]
tpl = [(200, (20,10)), (100, (10,10))]

main = do
  -- sort by tuple first element
  print $ sortBy (comparing fst) tpl
  -- maximum by tuple first element
  print $ maximumBy (comparing fst) tpl

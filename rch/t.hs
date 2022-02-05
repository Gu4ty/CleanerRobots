import Data.List 
import Data.Function 

f = minimumBy (compare `on` (\x -> sum x)) [[1,1], [2,2]]
import Data.Char(isSpace)
swapwords w1 w2 = unwords.map (\x -> if x == w1 then w2 else x) . words

swapwords' w1 w2 s = foldl (\x y -> if x == w1 then w2++y else x++y) [] $ split s

split = split'.(span isSpace)
split' ("","")= []
split' ("",t) = split' $ break isSpace t
split' (h,t)  = h:(split' $ span isSpace t)

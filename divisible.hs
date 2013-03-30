divisible op (x:xs) = if op x
                       then t (divisible op xs)
                       else t1 (divisible op xs)
                       where t (k, p) = (x:k, p+1)
                             t1 (k, p) = (k, p)
divisible _ [] = ([],0)

ddn n = (==0).(`mod` n)
dd11 = ddn 11
dd7 = ddn 7
dd7not11 s = dd7 s && (not $ dd11 s)
dd7and11 s = dd7 s && dd11 s
dd7or11 s = dd7 s || dd11 s
dd7xor11 s = dd7or11 s && (not $ dd7and11 s)
ddn7n11 = not.dd7or11

d7 = divisible dd7
d11 = divisible dd11

d7not11 = divisible dd7not11
d7and11 = divisible dd7and11
d7or11 = divisible dd7or11
d7xor11 = divisible dd7xor11
dn7n11 = divisible ddn7n11

dist lst (x:xs) = if x `elem` lst
                  then False
                  else dist (x:lst) xs
dist _ [] = True

distDigits :: (Integral a, Show a) => a -> Bool
distDigits = (dist []).show

countDistDig = divisible distDigits

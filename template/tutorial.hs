{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Control.Monad (replicateM, liftM)

tupleReplicate :: Int -> Q Exp
tupleReplicate n = do id <- newName "x"
                      return $ LamE [VarP id]
                                    (TupE $ replicate n $ VarE id)

summ ::  (Eq a, Num a) => a -> Q Exp
summ n = summ' n [| 0 |]
summ' 0 code = code
summ' n code = [| \x -> $(summ' (n-1) [|$code+x|]) |]

data Format = D
            | S
            | L String

parse :: String -> String -> [Format]
parse ('%':'s':xs) rest = L rest : S : parse xs ""
parse ('%':'d':xs) rest = L rest : D : parse xs ""
parse ""           rest = [L rest]
parse (x:xs)       rest = parse xs (rest ++ [x])

gen :: [Format] -> ExpQ -> ExpQ
gen [] code = code
gen (D:xs) code = [| \x -> $(gen xs [| $code++show (x::Integer) |]) |]
gen (S:xs) code = [| \x -> $(gen xs [| $code++x |]) |]
gen (L s:xs) code = gen xs [| $code++s |]

printf :: String -> ExpQ
printf s = gen (parse s "") [| "" |]

data T1 = T1
data T2 a = T2 a

deriveShow t = do
  TyConI (DataD _ _ _ constructors _) <- reify t
  let showClause (NormalC name fields) = do
        (pats, vars) <- genPE (length fields)
        let f [] = [| "" |]
            f (v:vs) = [| " " ++ show $v ++ $(f vs) |]
        clause [conP name pats]
               (normalB [| constructorName ++ $(f vars) |]) []
        where constructorName = nameBase name

  showbody <- mapM showClause constructors
  d <- [d| instance Show T1 where
            show x = "text"
      |]
  let    [InstanceD [] (AppT showt (ConT _T1)) [FunD showf _text]] = d
  return [InstanceD [] (AppT showt (ConT t  )) [FunD showf showbody]]

genPE ::  Int -> Q ([PatQ], [ExpQ])
genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)

cnst :: Lift t => Int -> t -> ExpQ
cnst n m = liftM (LamE (replicate n WildP)) (lift m)

sel :: Int -> Int -> ExpQ
sel i n = return (LamE buildPat (VarE $ mkName "x"))
    where buildPat = replicate (i-1) WildP ++ [VarP $ mkName "x"] ++ replicate (n+(-i)) WildP

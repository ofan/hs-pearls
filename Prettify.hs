
data Doc = Empty
        |  Char Char
        |  Text String
        |  Line
        |  Concat Doc Doc
        |  Union Doc Doc
           deriving (Show, Eq)

string :: String -> Doc
string str = undefined

text :: String -> Doc
text str = undefined

double :: Double -> Doc
double num = undefined

(<>) :: Doc -> Doc -> Doc
a <> b = undefined

char :: Char -> Doc
char c = undefined

renderJValue (JArray ary) = series '[' ']' renderJValue ary
renderJValue (JObject obj) = series '{' '}' field obj
  where field (name, val)  = string name
                            <> text ": "
                            <> renderJValue val

pretty :: Int -> Doc -> String
pretty width x = best 0 [x]
  where best col (d:ds) =
          case d of
            Empty   -> best col ds
            Char c  -> c : best (col + 1) ds
            Text s  -> s ++ best (col + length s) ds
            Line    -> '\n' : best 0 ds
            a `Concat` b -> best col (a:b:ds)
            a `Union` b -> nicest col (best col (a:ds))
                                     (best col (b:ds))
        best _ _ = ""

        nicest col a b | (width - least) `fits` a = a
                       | otherwise
                       where least = min width col

fits :: Int -> String -> Bool
w `fits` _ | w < 0 = False
w `fits` ""        = True
w `fits` ('\n':_)  = True
w `fits` (c:cs)    = (w - 1) `fits` cs


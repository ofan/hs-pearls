import Control.Monad (filterM)
import System.Directory (Permissions(..), getModificationTime, getPermissions)
import System.Time (ClockTime(..))
import Control.Exception (handle, SomeException)
import System.IO (IOMode(..), hClose, hFileSize, openFile, withFile)

import RecursiveContents (getRecursiveContents)

type Predicate =  FilePath      -- path to directory entry
               -> Permissions   -- Permissions
               -> Maybe Integer -- file size
               -> ClockTime     -- last modified
               -> Bool

getFileSize :: FilePath -> IO (Maybe Integer)
getFileSize path = handle handler $
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return (Just size)
  where handler :: SomeException -> IO (Maybe Integer)
        handler _ = return Nothing

betterFind :: Predicate -> FilePath -> IO [FilePath]
betterFind p path = getRecursiveContents path >>= filterM check
  where check name = do
          perms <- getPermissions name
          size <- getFileSize name
          modified <- getModificationTime name
          return (p name perms size modified)

saferFileSize :: FilePath -> IO (Maybe Integer)
saferFileSize path = handle handler $ do
  h <- openFile path ReadMode
  size <- hFileSize h
  hClose h
  return (Just size)
  where handler :: SomeException -> IO (Maybe Integer)
        handler _ = return Nothing

type InfoP a =  FilePath
             -> Permissions
             -> Maybe Integer
             -> ClockTime
             -> a

pathP :: InfoP FilePath
pathP path _ _ _ = path

sizeP :: InfoP Integer
sizeP _ _ (Just size) _ = size
sizeP _ _ Nothing     _ = -1

liftPath :: (FilePath -> a) -> InfoP a
liftPath f w _ _ _ = f w

equalP :: (Eq a) => InfoP a -> a -> InfoP Bool
equalP f k w x y z = f w x y z == k

liftP :: (a -> b -> c) -> InfoP a -> b -> InfoP c
liftP q f k w x y z = f w x y z `q` constP k w x y z

constP :: a -> InfoP a
constP k _ _ _ _ = k

greaterP, lesserP :: (Ord a) => InfoP a -> a -> InfoP Bool
greaterP = liftP (>)
lesserP = liftP (<)

liftP2 :: (a -> b -> c) -> InfoP a -> InfoP b -> InfoP c
liftP2 q f g w x y z = f w x y z `q` g w x y z

andP, orP :: InfoP Bool -> InfoP Bool -> InfoP Bool
andP = liftP2 (&&)
orP  = liftP2 (||)

(==?) :: (Eq a) => InfoP a -> a -> InfoP Bool
(==?) = equalP
(&&?) :: InfoP Bool -> InfoP Bool -> InfoP Bool
(&&?) = andP
(>?), (<?):: (Ord a) => InfoP a -> a -> InfoP Bool
(>?)  = greaterP
(<?)  = lesserP

-- Define precedence and associativity
infix 4 ==?
infixr 3 &&?
infix 4 >?
infix 4 <?

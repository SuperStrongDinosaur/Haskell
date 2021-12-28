{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE FlexibleContexts      #-}

module FileSystemLenses where 

import Control.Lens  
import Data.List 
import System.FilePath
import System.Directory.Tree(AnchoredDirTree (dirTree), readDirectoryWith)
import qualified System.Directory.Tree as D 

data FS = Dir { _name     :: FilePath  -- название папки, не полный путь
              , _contents :: [FS] }
  | File { _name     :: FilePath } -- название файла, не полный путь
  deriving (Eq, Show)

getDir :: FilePath -> IO FS
getDir root = mapTree . dirTree <$> readDirectoryWith (\_ -> return ()) root
  where
    mapTree :: D.DirTree () -> FS
    mapTree (D.File n _) = File n
    mapTree (D.Dir n content) = Dir n (map mapTree content)
    mapTree (D.Failed _ err) = error (show err)

makeLenses ''FS
makePrisms ''FS

cd :: String -> Traversal' FS FS
cd path = contents.traversed.filtered(\d -> isn't _File d && _name d == path)     

ls :: Traversal' FS FS
ls = contents.each

file :: String -> Traversal' FS FS
file fname = contents.traversed.filtered(\d -> isn't _Dir d && _name d == fname)

renameExt :: String -> FS -> FS
renameExt new f = f & contents.traversed._File %~ replaceExtension new

getAllNames :: FS -> [String]
getAllNames f = f^.name : Data.List.concatMap getAllNames (f^.contents)

delEmptyDyr :: String -> FS -> FS
delEmptyDyr dirname root = root & contents %~ filter (\fs -> isn't _Dir fs || (_name fs /= dirname) || not (null (fs^.contents)))



{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module TemplateHaskell where 

import Language.Haskell.TH
import qualified Data.Text as T
import Control.Monad

chooseByIndices :: Int -> [Int] -> Q Exp
chooseByIndices sz i = do
  x <- replicateM sz (newName "var")
  lamE [tupP $ zipWith (curry createPat) x (map (`elem` i) [0 .. sz - 1])] (tupE $ map (\j -> varE $ x !! j) i) where
    createPat :: (Name, Bool) ->  Q Pat
    createPat (s, True) = varP s
    createPat (_, False) = wildP


class (Show a) => ShowText a where
    showText :: a -> T.Text

genShowText :: Name -> Q [Dec]
genShowText s = [d|instance ShowText $(conT s) where
                    showText x = T.pack (show x) |]
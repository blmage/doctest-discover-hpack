module Main where

import Hpack.Config (decodeResultPackage, defaultDecodeOptions, readPackageConfig)
import System.Environment (getArgs)

import Test.Doctest.Discover.Config (findTestedPathsByTestMain)
import Test.Doctest.Discover.Generation (mkMainModuleSource)


main :: IO ()
main = do
    (mainPath : _ : outPath : _) <- getArgs
    result  <- readPackageConfig defaultDecodeOptions
    package <- either error (pure . decodeResultPackage) result

    writeFile outPath
        $ mkMainModuleSource
        $ findTestedPathsByTestMain mainPath package

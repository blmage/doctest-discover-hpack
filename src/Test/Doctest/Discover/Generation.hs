module Test.Doctest.Discover.Generation
       (
         mkMainModuleSource
       ) where

import Data.Map.Lazy (Map)
import Data.Set (Set)
import Test.Doctest.Discover.Config (ExtensionFlag (..))

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set


-- | Generates the source code of the /Main/ module usable to run the /Doctests/
-- contained under some 'FilePath's.
mkMainModuleSource :: Map FilePath (Set ExtensionFlag) -> String
mkMainModuleSource testedPaths = unlines $
     [ "module Main (main) where"
     , ""
     , "import Prelude"
     , "import Test.DocTest (doctest)"
     , ""
     , "main :: IO ()"
     , "main = do"
     ]
    <>
     ( fmap indent
     $ if Map.size testedPaths > 0
       then Map.foldMapWithKey mkPathTestCode testedPaths
       else [ "putStrLn \"Could not find any testable path.\"" ]
     )

-- | Generates the source code usable to run the /Doctests/ contained under a 'FilePath'.
mkPathTestCode :: FilePath -> Set ExtensionFlag -> [String]
mkPathTestCode path flags =
     [ "putStrLn \"Running tests for path: " <> showEscaped path <> "\""
     , "doctest"
     ]
    <>
     ( fmap indent
     $ zipWith (<>) ("[ " : repeat ", ")
     $ reverse
     $ show path : (show <$> Set.toList flags)
     )
    <>
     [ indent "]"
     , "putStrLn \"\""
     ]


-- | Indents a string by one level.
--
-- >>> indent "test"
-- "    test"
indent :: String -> String
indent = (replicate 4 ' ' <>)

-- | Variant of 'show' adapted for nesting within escaped strings.
--
-- >>> showEscaped "path"
-- "\\\"path\\\""
showEscaped :: String -> String
showEscaped = init . tail . show . show

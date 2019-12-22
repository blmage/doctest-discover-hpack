{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

module Test.Doctest.Discover.Config
       (
         ExtensionFlag(..)
       , findTestedPathsByTestMain
       ) where

import Control.Applicative (liftA2)
import Data.Coerce (coerce)
import Data.Foldable (fold)
import Data.Map.Lazy (Map)
import Data.Monoid ((<>))
import Data.Set (Set)
import Hpack.Config
import System.FilePath (FilePath, equalFilePath, (</>))

import qualified Data.Map.Lazy as Map
import qualified Data.Set as Set


-- | A CLI extension flag.
newtype ExtensionFlag = ExtensionFlag
    { -- | The corresponding language extension.
      unExtensionFlag :: String
    } deriving (Eq, Ord)

instance Show ExtensionFlag where
    show = show . ("-X" <>) . unExtensionFlag


-- | A generalized 'Section'.
data SomeSection where
    SomeSection :: Section a -> SomeSection

-- | Applies a function to the 'Section' wrapped by 'SomeSection'.
withSomeSection :: (forall a. Section a -> b) -> SomeSection -> b
withSomeSection f (SomeSection section) = f section


-- | Returns the 'Set' of 'ExtensionFlag's corresponding to the default language
-- extensions used by a 'Section'.
sectionExtensionFlags :: Section a -> Set ExtensionFlag
sectionExtensionFlags = Set.fromList . coerce . sectionDefaultExtensions

-- | Returns a 'Map' from the source directories of a (generalized) 'Section' to the
-- 'ExtensionFlag's necessary to run the corresponding /Doctests/.
sectionTestablePaths :: SomeSection -> Map FilePath (Set ExtensionFlag)
sectionTestablePaths
    = withSomeSection
    $ Map.fromList
    . liftA2 zip sectionSourceDirs (repeat . sectionExtensionFlags)


-- | Finds all the testable 'Section's of a 'Package'.
findTestableSections :: Package -> Map String [SomeSection]
findTestableSections package
    = Map.unionsWith (<>)
    [ wrapSection <$> packageExecutables package
    , wrapSection <$> packageInternalLibraries package
    , fold $ Map.singleton (packageName package) . wrapSection <$> packageLibrary package
    ]
  where
    wrapSection :: Section a -> [SomeSection]
    wrapSection = pure . SomeSection

-- | Finds all the test 'Section's of a 'Package' that are based on a given main file.
findTestSectionsByMain :: FilePath -> Package -> [Section Executable]
findTestSectionsByMain mainPath = filter isMatchingTest . Map.elems . packageTests
  where
    isMatchingTest :: Section Executable -> Bool
    isMatchingTest test = case executableMain (sectionData test) of
        Nothing   -> False
        Just path -> any (equalFilePath mainPath . (</> path)) $ sectionSourceDirs test

-- | Finds all the 'FilePath's covered by the test 'Section's of a 'Package' that are
-- based on a given main file.
findTestedPathsByTestMain :: FilePath -> Package -> Map FilePath (Set ExtensionFlag)
findTestedPathsByTestMain mainPath package
    = fmap (globalFlags <>)
    $ foldMap (foldMap sectionTestablePaths)
    $ Map.elems
    $ Map.restrictKeys (findTestableSections package)
    $ Set.fromList
    $ Map.keys
    $ unDependencies
    $ foldMap sectionDependencies testSections
  where
    testSections = findTestSectionsByMain mainPath package
    globalFlags  = foldMap sectionExtensionFlags testSections

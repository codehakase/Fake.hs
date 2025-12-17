{-# LANGUAGE ScopedTypeVariables #-}

module Fake.Data.Loader (
    loadSeedData,
    parseCsv,
) where

import Control.Exception (SomeException, catch)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

loadSeedData :: FilePath -> IO (Either String [String])
loadSeedData path = do
    catch
        ( do
            content <- TIO.readFile path
            return $ Right (parseCsv content)
        )
        (\(_ :: SomeException) -> return $ Left "Failed to load seed data")

parseCsv :: T.Text -> [String]
parseCsv content =
    filter (not . null) $
        map T.unpack $
            filter (not . T.isPrefixOf (T.pack "#")) $
                map T.strip $
                    T.lines content

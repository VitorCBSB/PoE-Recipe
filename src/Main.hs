{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Wreq
import Control.Lens
import Control.Monad (void, when)
import Data.Aeson hiding (Options)
import Data.Aeson.Lens (_String, key)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes)
import Data.Either (partitionEithers)
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString as Strict
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.List (find, isInfixOf)
import GHC.Generics
import Text.Parsec
import Text.Parsec.Text (Parser)

data Config = Config
  { accountName :: T.Text
  , league :: T.Text
  , sessId :: T.Text
  , stashTabName :: T.Text
  } deriving (Generic, Show)

instance FromJSON Config

newtype Account = Acc T.Text
newtype League = L T.Text
newtype TabIdx = TI T.Text
newtype SessId = SI T.Text

data Stash = Stash
  { numTabs :: Int
  , tabs :: [Tab]
  , items :: [Item]
  } deriving (Generic, Show)

data Tab = Tab
  { i :: Int -- Index
  , n :: T.Text -- Name
  } deriving (Generic, Show)

data Item = Item
  { descrText :: T.Text
  , typeLine :: T.Text
  , properties :: [Property]
  } deriving (Generic, Show)

data Property = Property
  { name :: T.Text
  , values :: [(T.Text, Int)]
  } deriving (Generic, Show)

instance FromJSON Stash
instance FromJSON Tab
instance FromJSON Item
instance FromJSON Property 

main :: IO ()
main = do
  putStrLn "Reading config."
  maybeConf <- readConfig
  case maybeConf of
    Left e -> error $ "Could not read config: " <> e
    Right conf -> 
      do  putStrLn "Fetching items from your stash."
          qItems <- getQualityTabItems conf
          let (noQ, qs) = partitionEithers $ map getItemQuality $ filter itemIsGem qItems
          mapM_ (putStrLn . show) qs
          when (not $ null qs) $
            do  putStrLn "Some items are missing quality:"
                mapM_ (putStrLn . T.unpack) noQ
          putStrLn ""
          putStrLn "Press Enter to exit..."
          void getChar

readConfig :: IO (Either String Config)
readConfig = eitherDecodeFileStrict' "config.json" 

requestStash :: Account -> League -> TabIdx -> SessId -> IO Stash
requestStash (Acc acc) (L league_) (TI tabIdx) (SI sessId_) = 
  do  r <- getWith (Main.params acc league_ tabIdx (encodeUtf8 sessId_)) url
      case r ^? responseBody of
        Nothing -> error "Could not fetch stash."
        Just body -> 
          case eitherDecode body of
            Left e -> error $ "Stash was fetched, but could not be parsed: " <> e
            Right stash -> return stash

getQualityTabItems :: Config -> IO [Item]
getQualityTabItems config =
  do  initStash <- requestStash (Acc $ accountName config) (L $ league config) (TI "0") (SI $ sessId config)
      case findCorrectTabIndex (stashTabName config) initStash of
        Left e -> error (T.unpack e)
        Right idx -> 
          do  qualStash <- requestStash (Acc $ accountName config) (L $ league config) (TI $ (T.pack . show) idx) (SI $ sessId config)
              return $ items qualStash

itemIsGem :: Item -> Bool
itemIsGem item =
  "socket" `T.isInfixOf` (descrText item)

findCorrectTabIndex :: T.Text -> Stash -> Either T.Text Int
findCorrectTabIndex name stash =
  case Data.List.find (\t -> n t == name) allTabs of
    Nothing -> Left $ "Could not find stash tab named '" <> name <> "'"
    Just tab -> Right $ i tab
  where
    allTabs = tabs stash

getItemQuality :: Item -> Either T.Text Int
getItemQuality item =
  case Data.List.find (\p -> name p == "Quality") allProps of
    Nothing -> Left $ "Item '" <> typeLine item <> "' has no quality."
    Just prop -> 
      case parse parseQuality "" ((fst . Prelude.head) (values prop)) of
        Left e -> Left $ "Could not parse quality '" <> fst (Prelude.head (values prop)) <> "' of item '" <> typeLine item <> "'."
        Right qual -> Right qual
  where
    allProps = properties item

url :: String
url = "https://www.pathofexile.com/character-window/get-stash-items"

params :: T.Text -> T.Text -> T.Text -> Strict.ByteString -> Options
params accountName leagueName tabIdx sessId =
  defaults & param "accountName" .~ [accountName]
           & param "tabIndex" .~ [tabIdx]
           & param "league" .~ [leagueName]
           & param "tabs" .~ ["1"]
           & header "Cookie" .~ ["POESESSID=" <> sessId]
           & header "Referer" .~ ["https://www.pathofexile.com"]

parseQuality :: Parser Int
parseQuality =
  do  char '+'
      qual <- read <$> many1 digit
      char '%'
      return qual
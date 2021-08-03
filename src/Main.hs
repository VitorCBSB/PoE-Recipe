{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens
import Control.Monad (unless, void)
import Data.Aeson
  ( FromJSON,
    eitherDecode,
    eitherDecodeFileStrict',
  )
import qualified Data.ByteString as B
import Data.Either (partitionEithers)
import Data.List (find, isInfixOf, partition, sort, (\\))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Text.IO as TIO
import GHC.Generics (Generic)
import Network.Wreq
  ( Options,
    checkResponse,
    defaults,
    getWith,
    header,
    param,
    responseBody,
  )
import System.IO (hPutStrLn, stderr)
import Text.Parsec (char, digit, many1, parse)
import Text.Parsec.Text (Parser)

data Config = Config
  { accountName :: T.Text,
    league :: T.Text,
    sessId :: T.Text,
    stashTabName :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON Config

newtype Account = Acc T.Text

newtype League = L T.Text

newtype TabIdx = TI T.Text

newtype SessId = SI T.Text

data Stash = Stash
  { numTabs :: Int,
    tabs :: [Tab],
    items :: [Item]
  }
  deriving (Generic, Show)

data Tab = Tab
  { i :: Int, -- Index
    n :: T.Text -- Name
  }
  deriving (Generic, Show)

data Item = Item
  { descrText :: Maybe T.Text,
    typeLine :: T.Text,
    properties :: Maybe [Property]
  }
  deriving (Generic, Show)

data Property = Property
  { name :: T.Text,
    values :: [(T.Text, Int)]
  }
  deriving (Generic, Show)

data ItemType
  = Gem
  | Flask
  | Map

instance FromJSON Stash

instance FromJSON Tab

instance FromJSON Item

instance FromJSON Property

main :: IO ()
main = do
  putStrLn "Reading config."
  maybeConf <- readConfig
  case maybeConf of
    Left e -> hPutStrLn stderr $ "Could not read config: " <> e
    Right conf ->
      do
        putStrLn "Fetching items from your stash."
        maybeQItems <- getQualityTabItems conf
        case maybeQItems of
          Left e -> TIO.hPutStrLn stderr $ "Error while retrieving items from stash: " <> e
          Right qItems -> do
            noQG <- printQualities Gem qItems
            noQF <- printQualities Flask qItems
            noQM <- printQualities Map qItems
            unless (null (noQG ++ noQF ++ noQM)) $
              do
                putStrLn "The following items have no quality:"
                mapM_ TIO.putStrLn (noQG ++ noQF ++ noQM)
  putStrLn ""
  putStrLn "Press Enter to exit..."
  void getChar

-- Returns no quality items so they can be logged at the end
-- If no quality items of that particular type are found,
-- ignore the no quality ones.
printQualities :: ItemType -> [Item] -> IO [T.Text]
printQualities itemType allItems =
  if null qs
    then return []
    else do
      TIO.putStrLn $ qualityCurrency itemType <> " combinations:"
      let (sets, out) = qualities trueQs
      mapM_ (\(ix, is) -> putStrLn $ "  " ++ show ix ++ ". " ++ show (sort is)) $ zip [1 ..] (map (: []) twentyQs ++ sets)
      putStr "Items left out: "
      print (sort out)
      putStrLn ""
      return noQ
  where
    (noQ, qs) = partitionEithers $ map getItemQuality $ filter (itemDeterminer itemType) allItems
    (twentyQs, trueQs) = partition (>= 20) qs

readConfig :: IO (Either String Config)
readConfig = eitherDecodeFileStrict' "config.json"

requestStash :: Account -> League -> TabIdx -> SessId -> IO (Either T.Text Stash)
requestStash (Acc acc) (L league_) (TI tabIdx) (SI sessId_) =
  do
    r <- getWith (Main.params acc league_ tabIdx (encodeUtf8 sessId_)) url
    case r ^? responseBody of
      Nothing -> return $ Left "Could not fetch stash."
      Just body ->
        case eitherDecode body of
          Left e ->
            return $
              Left $
                "Stash could not be parsed: " <> T.pack e <> "\n\nIs your config.json correctly configured?"
          Right stash -> return (Right stash)

getQualityTabItems :: Config -> IO (Either T.Text [Item])
getQualityTabItems config =
  do
    initStash <- requestStash (Acc $ accountName config) (L $ league config) (TI "0") (SI $ sessId config)
    case initStash of
      Left e -> return $ Left e
      Right iniStash ->
        case findCorrectTabIndex (stashTabName config) iniStash of
          Left e -> return $ Left e
          Right idx ->
            do
              qualStash <- requestStash (Acc $ accountName config) (L $ league config) (TI $ (T.pack . show) idx) (SI $ sessId config)
              case qualStash of
                Left e -> return $ Left e
                Right qualityStash -> return $ Right $ items qualityStash

itemDeterminer :: ItemType -> (Item -> Bool)
itemDeterminer itemType =
  case itemType of
    Gem -> maybe False ("socket" `T.isInfixOf`) . descrText
    Flask -> \item -> "Flask" `T.isInfixOf` typeLine item
    Map -> \item -> "Map" `T.isInfixOf` typeLine item

qualityCurrency :: ItemType -> T.Text
qualityCurrency itemType =
  case itemType of
    Gem -> "GCP"
    Flask -> "Glassblower"
    Map -> "Chisel"

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
      case parse parseQuality "" ((fst . head) (values prop)) of
        Left e -> Left $ "Could not parse quality '" <> fst (head (values prop)) <> "' of item '" <> typeLine item <> "'."
        Right qual -> Right qual
  where
    allProps = fromMaybe [] (properties item)

url :: String
url = "https://www.pathofexile.com/character-window/get-stash-items"

params :: T.Text -> T.Text -> T.Text -> B.ByteString -> Options
params accountName leagueName tabIdx sessId =
  defaults & param "accountName" .~ [accountName]
    & param "tabIndex" .~ [tabIdx]
    & param "league" .~ [leagueName]
    & param "tabs" .~ ["1"]
    & header "Cookie" .~ ["POESESSID=" <> sessId]
    & header "Referer" .~ ["https://www.pathofexile.com"]
    & checkResponse ?~ \_ _ -> return ()

parseQuality :: Parser Int
parseQuality =
  do
    char '+'
    qual <- read <$> many1 digit
    char '%'
    return qual

subsum :: Int -> [Int] -> [Int]
subsum w = snd . head . filter ((== w) . fst) . (++ [(w, [])]) . foldl s [(0, [])]
  where
    s a x = merge a $ map f a
      where
        f (a, l) = (a + x, l ++ [x])

    -- keep list of sums sorted and unique
    merge [] a = a
    merge a [] = a
    merge a@((av, al) : as) b@((bv, bl) : bs)
      | av < bv = (av, al) : merge as b
      | av == bv = (bv, bl) : merge as bs
      | otherwise = (bv, bl) : merge a bs

qualities :: [Int] -> ([[Int]], [Int])
qualities items =
  case subsum 40 items of
    [] -> ([], items)
    set ->
      let (sset, sitems) = qualities (items \\ set)
       in (set : sset, sitems)
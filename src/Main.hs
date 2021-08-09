{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((&), (.~), (?~), (^?))
import Control.Monad (unless, void)
import Data.Aeson
  ( FromJSON,
    eitherDecode,
    eitherDecodeFileStrict',
  )
import qualified Data.ByteString as B
import Data.Either (partitionEithers, isLeft, rights)
import Data.Function (on)
import Data.List (find, isInfixOf, partition, sort, sortBy, (\\))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
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
import Prelude hiding (error)

newtype Account = Acc T.Text
  deriving (Generic, Show)

newtype League = L T.Text
  deriving (Generic, Show)

newtype TabIdx = TI T.Text
  deriving (Generic, Show)

newtype SessId = SI T.Text
  deriving (Generic, Show)

instance FromJSON Account

instance FromJSON League

instance FromJSON SessId

data Config = Config
  { accountName :: Account,
    league :: League,
    sessId :: SessId,
    stashTabName :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON Config

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

newtype URL = U T.Text
  deriving (Generic, Show, Eq)

instance FromJSON URL

data Item = Item
  { descrText :: Maybe T.Text,
    typeLine :: T.Text,
    properties :: Maybe [Property],
    x :: Int,
    y :: Int,
    icon :: URL
  }
  deriving (Generic, Show, Eq)

data Property = Property
  { name :: T.Text,
    values :: [(T.Text, Int)]
  }
  deriving (Generic, Show, Eq)

data ItemType
  = Gem
  | Flask
  | Map

data Error = Error
  { error :: ErrorMessage
  }
  deriving (Generic, Show)

data ErrorMessage = ErrorMessage
  { code :: Int,
    message :: T.Text
  }
  deriving (Generic, Show)

instance FromJSON Stash

instance FromJSON Tab

instance FromJSON Item

instance FromJSON Property

instance FromJSON Error

instance FromJSON ErrorMessage

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
                mapM_ (TIO.putStrLn . typeLine) (noQG ++ noQF ++ noQM)
  putStrLn ""
  putStrLn "Press Enter to exit..."
  void getChar

-- Returns no quality items so they can be logged at the end
-- If no quality items of that particular type are found,
-- ignore the no quality ones.
printQualities :: ItemType -> [Item] -> IO [Item]
printQualities itemType allItems =
  if null qs
    then return []
    else do
      TIO.putStrLn $ qualityCurrency itemType <> " combinations:"
      let (sets, out) = qualities trueQs
      mapM_ (\(ix, is) -> putStrLn $ "  " ++ show ix ++ ". " ++ show (sort $ map fst is)) $ zip [1 ..] (map (: []) twentyQs ++ sets)
      putStr "Items left out: "
      print (sort $ map fst out)
      putStrLn ""
      return noQ
  where
    (noQ, qs) = partitionQuality $ filter (itemDeterminer itemType) allItems
    (twentyQs, trueQs) = partition (\i -> fst i >= 20) qs

-- Returns a tuple with: (items with quality, items without quality)
partitionQuality :: [Item] -> ([Item], [(Int, Item)])
partitionQuality items =
  (noQ, qs)
  where
    (noQ, qs) = partitionEithers $ map qualityCheck items
    qualityCheck item =
      case getItemQuality item of
        Nothing -> Left item
        Just q -> Right (q, item)

readConfig :: IO (Either String Config)
readConfig = eitherDecodeFileStrict' "config.json"

requestStash :: Account -> League -> TabIdx -> SessId -> IO (Either T.Text Stash)
requestStash account league tabIdx sessId =
  do
    r <- getWith (Main.params account league tabIdx sessId) url
    case r ^? responseBody of
      Nothing -> return $ Left "Could not fetch stash."
      Just body ->
        case eitherDecode body of
          Left _ ->
            let errorMess =
                  case eitherDecode body of
                    Left e -> "Unknown error: " <> T.pack e <> ". Is your config.json correctly configured?"
                    Right err ->
                      let advice =
                            case (code . error) err of
                              1 -> "\n\nCheck if your account name is correct and try again."
                              2 -> "\n\nCheck if the league name is correct and try again."
                              6 -> "\n\nIt's possible your POESESSID is incorrect or outdated. Try grabbing a new one and trying again."
                              _ -> ""
                       in (message . error) err <> " (code " <> (T.pack . show) ((code . error) err) <> ")" <> advice
             in return (Left $ "Stash could not be parsed: " <> errorMess)
          Right stash -> return (Right stash)

getQualityTabItems :: Config -> IO (Either T.Text [Item])
getQualityTabItems config =
  do
    initStash <- requestStash (accountName config) (league config) (TI "0") (sessId config)
    case initStash of
      Left e -> return $ Left e
      Right iniStash ->
        case findCorrectTabIndex (stashTabName config) iniStash of
          Left e -> return $ Left e
          Right idx ->
            do
              qualStash <- requestStash (accountName config) (league config) (TI $ (T.pack . show) idx) (sessId config)
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

getItemQuality :: Item -> Maybe Int
getItemQuality item =
  case Data.List.find (\p -> name p == "Quality") allProps of
    Nothing -> Nothing
    Just prop ->
      case parse parseQuality "" ((fst . head) (values prop)) of
        Left e -> Nothing
        Right qual -> Just qual
  where
    allProps = fromMaybe [] (properties item)

url :: String
url = "https://www.pathofexile.com/character-window/get-stash-items"

params :: Account -> League -> TabIdx -> SessId -> Options
params (Acc accountName) (L leagueName) (TI tabIdx) (SI sessId) =
  defaults & param "accountName" .~ [accountName]
    & param "tabIndex" .~ [tabIdx]
    & param "league" .~ [leagueName]
    & param "tabs" .~ ["1"]
    & header "Cookie" .~ ["POESESSID=" <> encodeUtf8 sessId]
    & header "Referer" .~ ["https://www.pathofexile.com"]
    & checkResponse ?~ \_ _ -> return ()

parseQuality :: Parser Int
parseQuality =
  do
    char '+'
    qual <- read <$> many1 digit
    char '%'
    return qual

subsum :: Int -> (a -> Int) -> [a] -> [a]
subsum w getInt = snd . head . filter ((== w) . fst) . (++ [(w, [])]) . foldl s [(0, [])]
  where
    s a x = merge a $ map f a
      where
        f (a, l) = (a + getInt x, l ++ [x])

    -- keep list of sums sorted and unique
    merge [] a = a
    merge a [] = a
    merge a@((av, al) : as) b@((bv, bl) : bs)
      | av < bv = (av, al) : merge as b
      | av == bv = (bv, bl) : merge as bs
      | otherwise = (bv, bl) : merge a bs

-- Returns a tuple with:
--  (Sets of 40 sum items, items left out of combinations)
qualities :: [(Int, Item)] -> ([[(Int, Item)]], [(Int, Item)])
qualities qAndItems =
  case subsum 40 fst qAndItems of
    [] -> ([], qAndItems)
    set ->
      let (sset, sitems) = qualities (qAndItems \\ set)
      in (set : sset, sitems)
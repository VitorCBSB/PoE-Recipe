{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import Network.Wreq
import Control.Lens
import Control.Monad (void, unless)
import Data.Aeson hiding (Options)
import Data.Monoid ((<>))
import Data.Maybe (catMaybes, fromMaybe)
import Data.Either (partitionEithers)
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Text.Encoding (encodeUtf8)
import Data.List (find, isInfixOf, (\\))
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
  { descrText :: Maybe T.Text
  , typeLine :: T.Text
  , properties :: Maybe [Property]
  } deriving (Generic, Show)

data Property = Property
  { name :: T.Text
  , values :: [(T.Text, Int)]
  } deriving (Generic, Show)

data ItemType =
  Gem
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
    Left e -> error $ "Could not read config: " <> e
    Right conf -> 
      do  putStrLn "Fetching items from your stash."
          qItems <- getQualityTabItems conf
          noQG <- printQualities Gem qItems
          noQF <- printQualities Flask qItems
          noQM <- printQualities Map qItems
          unless (null (noQG ++ noQF ++ noQM)) $
            do  putStrLn "The following items have no quality:"
                mapM_ TIO.putStrLn (noQG ++ noQF ++ noQM)
          putStrLn ""
          putStrLn "Press Enter to exit..."
          void getChar

-- Returns no quality items so they can be logged at the end
printQualities :: ItemType -> [Item] -> IO [T.Text] 
printQualities itemType allItems =
  do  let (noQ, qs) = partitionEithers $ map getItemQuality $ filter (itemDeterminer itemType) allItems
      if null qs then
        return []
      else
        do  TIO.putStrLn $ "The following combinations result in exactly one " <> qualityCurrency itemType <> " each:"
            let (sets, out) = qualities qs
            mapM_ (\(ix, is) -> putStrLn $ "  " ++ show ix ++ ". " ++ show is) $ zip [1..] sets
            putStr "Items left out: "
            print out
            putStrLn ""
            return noQ

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
      case parse parseQuality "" ((fst . Prelude.head) (values prop)) of
        Left e -> Left $ "Could not parse quality '" <> fst (Prelude.head (values prop)) <> "' of item '" <> typeLine item <> "'."
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

parseQuality :: Parser Int
parseQuality =
  do  char '+'
      qual <- read <$> many1 digit
      char '%'
      return qual

subsum :: Int -> [Int] -> [Int]
subsum w = snd . head . filter ((==w) . fst) . (++[(w,[])]) . foldl s [(0,[])]
  where
  s a x = merge a $ map f a 
    where
      f (a,l) = (a + x, l ++ [x])

  -- keep list of sums sorted and unique
  merge [] a = a
  merge a [] = a
  merge a@((av,al):as) b@((bv,bl):bs)
    | av <  bv  = (av,al) : merge as b
    | av == bv  = (bv,bl) : merge as bs
    | otherwise = (bv,bl) : merge a bs
 
qualities :: [Int] -> ([[Int]], [Int])
qualities items =
  case subsum 40 items of
    [] -> ([], items)
    set -> 
      let (sset, sitems) = qualities (items \\ set)
      in
        (set : sset, sitems)
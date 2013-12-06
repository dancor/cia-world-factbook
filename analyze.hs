{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Data.Monoid
import qualified Data.Text as DT
import qualified Data.Text.IO as DTI

pullPart :: DT.Text -> DT.Text -> DT.Text -> DT.Text
pullPart pre post =
    fst . DT.breakOn post .
    DT.drop (DT.length pre) . snd . DT.breakOn pre

grabName :: [DT.Text] -> DT.Text
grabName =
    pullPart pre post . head . filter (pre `DT.isInfixOf`)
  where
    pre = "region_name1\">"
    post = "</span>"

grabLanguages :: [DT.Text] -> DT.Text
grabLanguages c = langLine
  where
    langLine =
      pullPart ">" "</div>" .
      head . dropWhile (not . ("category_data" `DT.isInfixOf`)) .
      tail $ dropWhile (not . ("Languages:" `DT.isInfixOf`)) c

readHtml :: DT.Text -> IO [DT.Text]
readHtml nationCode =
    DT.lines <$>
    DTI.readFile ("html/countrytemplate_" ++ DT.unpack nationCode ++ ".html")

data Nation
    = Nation
    { nName :: DT.Text
    , nLang :: DT.Text
    }

main :: IO ()
main = do
    nationCodes <- DT.lines <$> DTI.readFile "nation-codes"
    nations <- forM nationCodes $ \nationCode -> do
        c <- readHtml nationCode
        return $ Nation (grabName c) (grabLanguages c)
    {-
    DTI.putStr . DT.unlines . map nName $
        filter ((== "English") . head . DT.words . nLang) nations
    -}
    forM_ nations $ \nation -> do
        DTI.putStrLn $ nName nation
        DTI.putStrLn $ "- " <> nLang nation
        DTI.putStrLn ""

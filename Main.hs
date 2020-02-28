{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import qualified Data.Text.IO as IOText
import qualified Data.Map as Map
import Data.Char
import Data.List
import Data.Maybe
import Text.Read (readMaybe)
import Control.Applicative
import Control.Monad
import Web.Scotty
import System.Environment

data PackageInfo = PackageInfo {
      packageName        :: T.Text
    , packageDescription :: T.Text
    , packageDepends     :: [[T.Text]]
    , packageRDepends    :: [T.Text]
} deriving Show


newtype Parser a = Parser (T.Text -> Maybe (a, T.Text))

instance Functor Parser where
    fmap f p = Parser $ \input -> do
        (x, input') <- runParser p input
        Just (f x, input')

instance Applicative Parser where
    pure a = Parser $ \input -> Just (a, input)
    p1 <*> p2 = Parser $ \input1 -> do
        (f, input2) <- runParser p1 input1
        (x, input3) <- runParser p2 input2
        Just (f x, input3)

instance Alternative Parser where
    empty = Parser $ \_ -> Nothing
    p1 <|> p2 = Parser $ \input -> runParser p1 input <|> runParser p2 input

runParser :: Parser a -> T.Text ->  Maybe (a, T.Text)
runParser (Parser p) txt = p txt

breakOnAndRemoveNeedle :: T.Text -> T.Text -> (T.Text, T.Text)
breakOnAndRemoveNeedle needle haystack = (first, T.drop (T.length needle) second)
    where
        (first, second) = T.breakOn needle haystack


splitNamesAndValues :: [[T.Text]] -> [[(T.Text, T.Text)]]
splitNamesAndValues paragraphs = (map . map) (breakOnAndRemoveNeedle ": ") paragraphs

paragraphsToMap :: [[T.Text]] -> [Map.Map T.Text T.Text]
paragraphsToMap paragraphs = map Map.fromList (splitNamesAndValues paragraphs)

paragraphToMap :: [T.Text] -> Map.Map T.Text T.Text
paragraphToMap [] = Map.empty
paragraphToMap (x:xs) = Map.union (Map.singleton name value) (paragraphToMap xs)
    where
        (name, value) = breakOnAndRemoveNeedle ": " x

mapToPackageInfo :: Map.Map T.Text T.Text -> PackageInfo
mapToPackageInfo pinfomap = case parsedDepends of
                                Just (d, _) -> PackageInfo name description d []
                                Nothing -> PackageInfo name description [] []
    where
        name = pinfomap Map.! "Package"
        description = pinfomap Map.! "Description"
        depends = Map.lookup "Depends" pinfomap
        parsedDepends = runParser dependsP =<< depends

packageInfoListToMap :: [PackageInfo] -> Map.Map T.Text PackageInfo
packageInfoListToMap pinfos = foldr f Map.empty pinfos
    where
        f pinfo pinfomap = Map.union (Map.singleton (packageName pinfo) pinfo) pinfomap

ws :: Parser T.Text
ws = spanP isSpace

wsNotNL :: Parser T.Text
wsNotNL = spanP spaceNotNL

untilNL :: Parser T.Text
untilNL = spanP (/= '\n')

spaceNotNL :: Char -> Bool
spaceNotNL '\n' = False
spaceNotNL c = isSpace c

singleLine :: Parser T.Text
singleLine = T.snoc <$> notEmpty untilNL <*> charP '\n'

multiLine :: Parser T.Text
multiLine = T.append <$> (notEmpty allButLast) <*> lineAndNL
    where
        lineAndNL = T.snoc <$> untilNL <*> charP '\n'
        allButLast = foldr T.append "" <$> many (T.append <$> lineAndNL <*> (notEmpty $ wsNotNL))

-- Using nub to remove dublicates because we ignore version numbers of dependencies.
dependsP :: Parser [[T.Text]]
dependsP = map (T.splitOn " | ") <$> nub <$> commaSeparated
    where
        commaSeparated = sepBy oneDependsP (textP ", ")

withVersionNr :: Parser T.Text
withVersionNr = spanP (\c -> ((not . isSpace) c) && c /= ',')
                        <* ws
                        <* charP '('
                        <* spanP (\c -> if c == ')' then False else True)
                        <* charP ')'

withVersionNrPipe :: Parser T.Text
withVersionNrPipe = T.append <$> withVersionNr <*> optionalTextP " | "

withoutVersionNr :: Parser T.Text
withoutVersionNr = spanP (\c -> ((not . isSpace) c) && c /= ',')

withoutVersionNrPipe :: Parser T.Text
withoutVersionNrPipe = T.append <$> (notEmpty withoutVersionNr) <*> optionalTextP " | "

oneDependsP :: Parser T.Text
oneDependsP = T.concat <$> many ((withVersionNrPipe) <|> (withoutVersionNrPipe))

optionalTextP :: T.Text -> Parser T.Text
optionalTextP text = Parser $ \input -> if T.take lenText input == text
    then Just (text, T.drop lenText input)
    else Just ("", input)
        where lenText = T.length text

notEmpty :: Parser T.Text -> Parser T.Text
notEmpty p = Parser $ \input1 -> do
    (x, input2) <- runParser p input1
    if x == T.empty
        then Nothing
        else Just (x, input2)

paragraphsP :: Parser [[T.Text]]
paragraphsP = (map . map) T.strip <$> paragraphs
    where
        paragraph = many (multiLine <|> singleLine)
        paragraphs = filter (not . null) <$> sepBy paragraph paragraphSep

paragraphSep :: Parser T.Text
paragraphSep = T.snoc <$> wsNotNL <*> charP '\n'

sepBy :: Parser a -> Parser b -> Parser [a]
sepBy p sep = (:) <$> p <*> many (sep *> p) <|> pure []

charP :: Char -> Parser Char
charP x = Parser $ \input -> do
    (first, rest) <- T.uncons input
    if first == x
        then Just (first, rest)
        else Nothing

spanP :: (Char -> Bool) -> Parser T.Text
spanP f = Parser $ \input -> Just $ T.span f input

textP :: T.Text -> Parser T.Text
textP text = Parser $ \input -> if T.take lenText input == text
    then Just (text, T.drop lenText input)
    else Nothing
        where lenText = T.length text

calculateRDepends :: [PackageInfo] -> [PackageInfo]
calculateRDepends pinfos = map (selectRDep depMap) pinfos
    where
        depMaps = map findOneRDep pinfos
        depMap = foldr (Map.unionWith (++)) Map.empty depMaps
        selectRDep dm pinfo = 
            case Map.lookup (packageName pinfo) dm of
                Just rd -> setRDepends pinfo rd
                Nothing -> setRDepends pinfo []

setRDepends :: PackageInfo -> [T.Text] -> PackageInfo
setRDepends (PackageInfo name description depends rd) newrd = PackageInfo name description depends newrd

findRDependends :: [PackageInfo] -> Map.Map T.Text [T.Text]
findRDependends pinfos = foldr (Map.unionWith (++)) Map.empty depMaps
    where
        depMaps = map findOneRDep pinfos

findOneRDep :: PackageInfo -> Map.Map T.Text [T.Text]
findOneRDep pinfo = foldr (Map.unionWith (++)) Map.empty depMaps
    where
        flatDeps = concat $ packageDepends pinfo
        depMaps = map (\x -> Map.singleton x [packageName pinfo]) flatDeps

htmlListFromKeys :: [T.Text] -> T.Text
htmlListFromKeys keys = T.concat ["<ol>", foldr f T.empty keys, "</ol>"]
    where
        f key listHtml = T.append (T.concat ["<li><a href=\"", key, "\">", key, "</a></li>\n"]) listHtml

htmlPackageInfo :: PackageInfo -> Map.Map T.Text PackageInfo -> T.Text
htmlPackageInfo pinfo pinfomap = T.concat [ "<table border=\"1\" style=\"width:50%\">"
                                          , "<tr><th>Package</th><td>"
                                          , packageName pinfo
                                          , "</td></tr>"
                                          , "<tr><th>Description</th><td><pre>"
                                          , packageDescription pinfo
                                          , "</pre></td></tr>"
                                          , "<tr><th>Depends</th><td>"
                                          , htmlDepends (packageDepends pinfo) pinfomap
                                          , "</td></tr>"
                                          , "<tr><th>Reverse depends</th><td>"
                                          , htmlRDepends (packageRDepends pinfo) pinfomap
                                          , "</td></tr>"
                                          , "</table>" ]

htmlDepends :: [[T.Text]] -> Map.Map T.Text PackageInfo -> T.Text
htmlDepends pdepends pinfomap = T.intercalate ", " $ (map (T.intercalate " | ")) dependsWithLinks
    where
        linkIfFound name = case Map.lookup name pinfomap of
                            Just _ -> packageLink name
                            Nothing -> name
        dependsWithLinks = (map . map) linkIfFound pdepends

packageLink :: T.Text -> T.Text
packageLink name = T.concat ["<a href=\"", name, "\">", name, "</a>"]

htmlRDepends :: [T.Text] -> Map.Map T.Text PackageInfo -> T.Text
htmlRDepends prdepends pinfomap = T.intercalate ", " rdependsWithLinks
    where
        linkIfFound name = case Map.lookup name pinfomap of
                            Just _ -> packageLink name
                            Nothing -> name
        rdependsWithLinks = map linkIfFound prdepends



startServer :: Map.Map T.Text PackageInfo -> IO ()
startServer pinfomap = do
                   -- Read port from environment variable for Heroku support
                   port <- fromMaybe 3000
                        . join
                        . fmap readMaybe <$> lookupEnv "PORT"
                   scotty port $
                    get "/:package" $ do
                        package <- param "package" :: ActionM T.Text
                        case package of
                            "" -> html $ (L.fromStrict $ htmlListFromKeys (Map.keys pinfomap))
                            name -> case Map.lookup name pinfomap of
                                        Just pinfo -> html $ L.fromStrict $ htmlPackageInfo pinfo pinfomap
                                        Nothing -> html "<h1>404: No such package exists</h1>"
                            

main :: IO ()
main = do
    --args <- getArgs
    --let filePath = head args
    txt <- IOText.readFile "/app/mockdata"
    let parserResult = runParser paragraphsP txt
    case parserResult of
        Just (paragraphList, _) -> do
            let packageInfoMap = (packageInfoListToMap . calculateRDepends . (map mapToPackageInfo) . paragraphsToMap) paragraphList
            startServer packageInfoMap
        Nothing -> print "Error: Failed to parse file"

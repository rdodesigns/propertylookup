{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Text.HTML.TagSoup
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)
import Data.Char (isDigit)
import Data.String.Utils (rstrip)
import Control.Monad
import Data.Maybe (catMaybes)
import qualified Data.Csv as CSV
import Control.Exception
import Data.Typeable
import qualified Data.ByteString.Lazy as BS
import qualified Data.Vector as V
import System.Process (proc, waitForProcess, createProcess)
import System.FilePath ((</>))
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)

type PropertyRecord = String

data Property = Property {
                    record :: PropertyRecord,
                    footage :: Int
                } deriving (Show, Eq)

data PropertyError = ZeroLength
                   | PrefixNotR
                   | IDNotSevenDigits
                   | IDNotInteger
                   | NoImprovements
                   | ZeroImprovements
    deriving (Typeable)

instance Show PropertyError where
    show ZeroLength       = "Property has zero length record ID"
    show PrefixNotR       = "Property record does not start with R"
    show IDNotSevenDigits = "Property record does not have 7 digit suffix"
    show IDNotInteger     = "Property record does not have 7 digit integer suffix"
    show NoImprovements   = "Property record improperly formatted"
    show ZeroImprovements = "Property record invalid (zero improvements)"

instance Exception PropertyError

instance CSV.ToRecord Property where
    toRecord (Property r ft) = CSV.record [
        CSV.toField r, CSV.toField ft ]

instance CSV.ToNamedRecord Property where
    toNamedRecord (Property r ft) = CSV.namedRecord
        [ CSV.toField "Record ID" CSV..= r
        , CSV.toField "Total Square Feet" CSV..= ft ]

validRecord :: String -> PropertyRecord
validRecord ""             = throw ZeroLength
validRecord s@(r:rs)
    | r /= 'R'             = throw PrefixNotR
    | length rs /= 7       = throw IDNotSevenDigits
    | not $ all isDigit rs = throw IDNotSevenDigits
    | otherwise            = s


recordURL :: PropertyRecord -> String
recordURL = (++)
    "http://maps.bouldercounty.org/boco/emapping/PrintAssessmentReport.aspx?AccountNumber="

getPropertyHTML :: PropertyRecord -> IO String
getPropertyHTML r = simpleHTTP  (getRequest $ recordURL r) >>= getResponseBody

getInnerText :: String -> [Tag String] -> Maybe String
getInnerText str tags = case result of
    [TagOpen _ _, TagText _] -> Just $ innerText result
    _ -> Nothing
    where
        result = take 2 . dropWhile (~/= str) $ tags

numImprovements :: [Tag String] -> Maybe Int
numImprovements tags = liftM read $ getInnerText "<span id=lblNoImprovements>" tags

nextTagInnerText :: String -> [Tag String] -> Maybe String
nextTagInnerText str tags = case result of
    [TagOpen _ _, TagText _] -> Just $ innerText result
    _ -> Nothing
    where
        result = take 2 . drop 2 . dropWhile (~/= str) $ tags

squareFootage :: [Tag String] -> String -> Maybe Int
squareFootage tags str = liftM read $ nextTagInnerText str tags

property :: String -> IO Property
property str = do
    let r = validRecord str
    h <- getPropertyHTML r
    let tags = parseTags  h
    return $! case  numImprovements tags of
                Nothing -> throw NoImprovements
                Just x -> if x == 0
                            then throw ZeroImprovements
                            else Property r (ft tags)

    where
        ft tags = sum . catMaybes $ map (squareFootage tags)
            [ "FIRST FLOOR (ABOVE GROUND) FINISHED AREA"
            , "2ND FLOOR AND HIGHER FINISHED AREA"
            , "OFFICE"
            , "RETAIL"
            , "RESTAURANT" ]


main :: IO ()
main = do
    args <- getArgs
    let [infile, outdir] = args
	
    createDirectoryIfMissing True outdir
    propertyStr <- liftM (map rstrip . lines ) $ readFile infile
    propertiesMaybe <- mapM (\s -> (processRecord outdir s) `catch`
                                   recordErrors) propertyStr
    
	let properties = catMaybes propertiesMaybe
    let csv = CSV.encode . V.fromList $ properties
    BS.writeFile (outdir </> "footage.csv") csv
    
	where
		
        processRecord :: FilePath -> String -> IO (Maybe Property)
        processRecord outdir str = do
            putStr $ "Processing Record " ++ str
            p <- property str
            (_, _, _, ph) <- createProcess (proc
                "wkhtmltopdf" ["-q", recordURL str,
                outdir </> (str ++ ".pdf")])
            waitForProcess ph
            putStr $ "\n"
            return $ Just p
			
        recordErrors :: PropertyError -> IO (Maybe Property)
        recordErrors e = do
            putStr $ " ! Error: " ++ show e ++ "\n"
            return Nothing

module Graphics.Wayland.Scanner.Protocol (
  readProtocol, parseFile
  ) where

import Data.Functor
import Data.Maybe
import Data.Char
import Text.XML.Light
import System.Process
import Language.Haskell.TH (mkName)

import Graphics.Wayland.Scanner.Types
import Graphics.Wayland.Scanner.Names

interface  = QName "interface"  Nothing Nothing
request    = QName "request"    Nothing Nothing
event      = QName "event"      Nothing Nothing
enum       = QName "enum"       Nothing Nothing
entry      = QName "entry"      Nothing Nothing
arg        = QName "arg"        Nothing Nothing
namexml    = QName "name"       Nothing Nothing
version    = QName "version"    Nothing Nothing
allow_null = QName "allow-null" Nothing Nothing
typexml    = QName "type"       Nothing Nothing
value      = QName "value"      Nothing Nothing

parseInterface :: ProtocolName -> Element -> Interface
parseInterface pname elt =
  let iname = fromJust $ findAttr namexml elt

      parseMessage :: Element -> Maybe Message
      parseMessage msgelt = do -- we're gonna do some fancy construction to skip messages we can't deal with yet
        let name = fromJust $ findAttr namexml msgelt
        arguments <- mapM parseArgument (findChildren arg msgelt)
        let destructorVal = findAttr typexml msgelt
        let isDestructor = case destructorVal of
                             Nothing -> False
                             Just str -> str=="destructor"

        return Message {messageName = name, messageArguments = arguments, messageIsDestructor = isDestructor} where
          parseArgument argelt = do
            let msgname = fromJust $ findAttr namexml argelt
            let argtypecode = fromJust $ findAttr typexml argelt
            argtype <- case argtypecode of
              "object" -> ObjectArg . mkName . interfaceTypeName pname <$> findAttr interface argelt
              "new_id" -> (\iname -> NewIdArg (mkName $ interfaceTypeName pname iname) iname) <$> findAttr interface argelt
              _ -> lookup argtypecode argConversionTable
            let allowNull = fromMaybe False (read <$> capitalize <$> findAttr allow_null argelt)
            return (msgname, argtype, allowNull)

      parseEnum enumelt =
        let enumname = fromJust $ findAttr namexml enumelt
            entries = map parseEntry $ findChildren entry enumelt
        in WLEnum {enumName = enumname, enumEntries = entries} where
          parseEntry entryelt = (fromJust $ findAttr namexml entryelt,
                                 read $ fromJust $ findAttr value entryelt :: Int)
  in Interface {
     interfaceName = iname,
     interfaceVersion = read $ fromJust $ findAttr version elt, -- unused atm
     interfaceRequests = mapMaybe parseMessage (findChildren request elt),
     interfaceEvents = mapMaybe parseMessage (findChildren event elt),
     interfaceEnums = map parseEnum $ findChildren enum elt
     }

parseProtocol :: [Content] -> ProtocolSpec
parseProtocol xmlTree =
  let subTree = (!!1) $ onlyElems xmlTree -- cut off XML header stuff
      pname = fromJust $ findAttr namexml subTree
      interfaces = map (parseInterface pname) $ findChildren interface subTree
  in ProtocolSpec pname interfaces

parseFile :: FilePath -> IO ProtocolSpec
parseFile filename = do
  fileContents <- readFile filename
  return $ parseProtocol $ parseXML fileContents

-- | locate wayland.xml on disk and parse it
readProtocol :: IO ProtocolSpec
readProtocol = do
  datadir <- figureOutWaylandDataDir
  parseFile (datadir ++ "/" ++ protocolFile)


-- TODO move this into some pretty Setup.hs thing as soon as someone complains about portability
figureOutWaylandDataDir :: IO String
figureOutWaylandDataDir =
  head <$> lines <$> readProcess "pkg-config" ["wayland-server", "--variable=pkgdatadir"] []

protocolFile = "wayland.xml"

module Graphics.Wayland.Scanner.Types where

import Foreign
import Language.Haskell.TH (Name)

#include <wayland-util.h>

{#context prefix="wl"#}

data ServerClient = Server | Client  deriving (Eq)

-- | wayland-style interface name (e.g. wl_display)
type InterfaceName = String
data Interface = Interface {
  interfaceName :: InterfaceName,
  interfaceVersion :: Int,
  interfaceRequests :: [Message], -- ^ aka requests
  interfaceEvents :: [Message],
  interfaceEnums :: [WLEnum]
  } deriving (Show)

type EnumName = String
-- | wayland style enum specification (not Prelude)
data WLEnum = WLEnum {
  enumName :: EnumName,
  enumEntries :: [(String,Int)]
  } deriving (Show)

-- | wayland wire protocol argument type. we can't deal with untyped object/new-id arguments.
data ArgumentType = IntArg | UIntArg | FixedArg | StringArg | ObjectArg Name | NewIdArg Name MessageName | ArrayArg | FdArg deriving (Show)
argConversionTable :: [(String, ArgumentType)] -- for all easy argument types
argConversionTable = [
  ("int", IntArg),
  ("uint", UIntArg),
  ("fixed", FixedArg),
  ("string", StringArg),
  ("fd", FdArg),
  ("array", ArrayArg)]

type Argument = (String, ArgumentType, Bool) -- name, argument type, allow-null

type MessageName = String
data Message = Message {
  messageName :: MessageName,
  messageArguments :: [Argument],
  messageIsDestructor :: Bool
  } deriving (Show)

type ProtocolName = String
data ProtocolSpec = ProtocolSpec {
  protocolName :: ProtocolName,
  protocolInterfaces :: [Interface]
  } deriving (Show)

{#pointer * array as WLArray#}

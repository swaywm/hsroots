{-# LANGUAGE TemplateHaskell, DeriveDataTypeable #-}
module Graphics.Wayland.Scanner.Marshaller (
  argTypeToCType, argTypeToHaskType, argTypeToWeirdInterfaceCType,

  argTypeMarshaller, argTypeUnmarshaller,

  funcSize, funcAlign
  ) where

import Control.Exception.Base (bracket)
import Data.Functor
import Data.Fixed (Fixed(..))
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Process
import System.IO
import System.Posix.Types
import Language.Haskell.TH

import Graphics.Wayland.Internal.Util (Fixed256, Time, millisecondsToTime, timeToMilliseconds)
import Graphics.Wayland.Scanner.Protocol
import Graphics.Wayland.Scanner.Names
import Graphics.Wayland.Scanner.Types

#include <wayland-server.h>

{#context prefix="wl"#}


wlFixedToFixed256 :: CInt -> Fixed256
wlFixedToFixed256 = MkFixed . fromIntegral
fixed256ToWlFixed :: Fixed256 -> CInt
fixed256ToWlFixed (MkFixed a) = fromIntegral a

-- {#pointer * array as WLArray nocode#}

argTypeToCType :: Argument -> TypeQ
argTypeToCType (_,IntArg,_) = [t| {#type int32_t#} |]
argTypeToCType (_,UIntArg,_) = [t| {#type uint32_t#} |]
argTypeToCType (_,FixedArg,_) = [t|{#type fixed_t#}|]
argTypeToCType (_,StringArg,_) = [t| Ptr CChar |]
argTypeToCType (_,(ObjectArg iname),_) = return $ ConT iname
argTypeToCType (_,(NewIdArg iname _),_) = return $ ConT iname
argTypeToCType (_,ArrayArg,_) = [t|WLArray|]
argTypeToCType (_,FdArg,_) = [t| {#type int32_t#} |]

argTypeToHaskType :: Argument -> TypeQ
argTypeToHaskType (_,IntArg,_) = [t|Int|]
argTypeToHaskType (name,UIntArg,_)
  | name == "time" = [t|Time|]
  | otherwise      = [t|Word|]
argTypeToHaskType (_,FixedArg,_) = [t|Fixed256|]
argTypeToHaskType (_,StringArg,False) = [t|String|]
argTypeToHaskType (_,(ObjectArg iname),False) = return $ ConT iname
argTypeToHaskType (_,(NewIdArg iname _),False) = return $ ConT iname
argTypeToHaskType (_,StringArg,True) = [t|Maybe String|]
argTypeToHaskType (_,(ObjectArg iname),True) = [t|Maybe $(conT iname) |]
argTypeToHaskType (_,(NewIdArg iname _),True) = [t|Maybe $(conT iname) |]
argTypeToHaskType (_,ArrayArg,True) = [t|Maybe (Int, Ptr ())|] -- size_t size and void*
argTypeToHaskType (_,ArrayArg,False) = [t|(Int, Ptr ())|] -- size_t size and void*
argTypeToHaskType (_,FdArg,_) = [t|Fd|]

argTypeToWeirdInterfaceCType :: Argument -> TypeQ
argTypeToWeirdInterfaceCType (_,(NewIdArg iname _),_) = [t|{#type uint32_t#}|]
argTypeToWeirdInterfaceCType x = argTypeToCType x

marshallerVar :: Argument -> Name
marshallerVar (name, _, _) = mkName name

-- | Allows a C function to receive Haskell data
argTypeMarshaller :: [Argument] -> ExpQ -> ([Pat], ExpQ)
argTypeMarshaller args fun =
  let vars = map marshallerVar args
      mk = return . VarE . marshallerVar
      applyMarshaller :: [Argument] -> ExpQ -> ExpQ
      applyMarshaller (arg@(_, IntArg, _):as) fun = [|$(applyMarshaller as [|$fun (fromIntegral ($(mk arg) :: Int) )|])|]
      applyMarshaller (arg@(name, UIntArg, _):as) fun
        | name == "time" = [|$(applyMarshaller as [|$fun (timeToMilliseconds ($(mk arg) :: Time))|]) |]
        | otherwise      = [|$(applyMarshaller as [|$fun (fromIntegral ($(mk arg) :: Word))|]) |]
      applyMarshaller (arg@(_, FixedArg, _):as) fun = [|$(applyMarshaller as [|$fun (fixed256ToWlFixed $(mk arg))|]) |]
      applyMarshaller (arg@(_, StringArg, False):as) fun = [|withCString $(mk arg) (\cstr -> $(applyMarshaller as [|$fun cstr|]))|]
      applyMarshaller (arg@(_, (ObjectArg iname), False):as) fun = [|$(applyMarshaller as [|$fun $(mk arg)|]) |]
      applyMarshaller (arg@(_, (NewIdArg iname _), False):as) fun = [|$(applyMarshaller as [|$fun $(mk arg) |])|]
      applyMarshaller (arg@(_, StringArg, True):as) fun = [|
           case $(mk arg) of
             Nothing  -> $(applyMarshaller as [|$fun nullPtr|])
             Just str -> withCString str (\cstr -> $(applyMarshaller as [|$fun cstr|]))
           |]
      applyMarshaller (arg@(_, (ObjectArg iname), True):as) fun = [|
           case $(mk arg) of
             Nothing  -> $(applyMarshaller as [|$fun ($(conE iname) nullPtr)|])
             Just obj -> $(applyMarshaller as [|$fun obj|])
           |]
      applyMarshaller (arg@(_, (NewIdArg iname _), True):as) fun = [|
           case $(mk arg) of
             Nothing  -> $(applyMarshaller as [|$fun ($(conE iname) nullPtr)|])
             Just obj -> $(applyMarshaller as [|$fun obj|])
           |]
      applyMarshaller (arg@(_, ArrayArg, True):as) fun = [|
           case $(mk arg) of
             Nothing -> $(applyMarshaller as [|$fun nullPtr|])
             Just (size, dat) -> bracket
               (mallocBytes (2*{#sizeof size_t#}+ 4)) -- FIXME prettify / make portable. is this even right?
               (free)
               (\arrayPtr -> do
                 {#set array.size#} arrayPtr size
                 {#set array.alloc#} arrayPtr size -- FIXME or should we force powers of 2?
                 {#set array.data#} arrayPtr dat
                 $(applyMarshaller as [|$fun arrayPtr|])
                 )
           |]
      applyMarshaller (arg@(_, ArrayArg, False):as) fun = [|
           do
             let (size, dat) = $(mk arg)
             bracket
               (mallocBytes (2*{#sizeof size_t#}+ 4)) -- FIXME prettify / make portable. is this even right?
               (free)
               (\arrayPtr -> do
                 {#set array.size#} arrayPtr size
                 {#set array.alloc#} arrayPtr size -- FIXME or should we force powers of 2?
                 {#set array.data#} arrayPtr dat
                 $(applyMarshaller as [|$fun arrayPtr|])
                 )
           |]
      applyMarshaller (arg@(_, FdArg, _):as) fun = [|$(applyMarshaller as [|$fun (unFd ($(mk arg)))|]) |]
      applyMarshaller [] fun = fun
  in  (map VarP vars, applyMarshaller args fun)

unFd (Fd k) = k

-- | Opposite of argTypeMarshaller: allows a Haskell function to receive C data.
argTypeUnmarshaller :: [Argument] -> ExpQ -> ([Pat], ExpQ)
argTypeUnmarshaller args fun =
  let vars = map marshallerVar args
      mk = return . VarE . marshallerVar
      applyUnmarshaller :: [Argument] -> ExpQ -> ExpQ
      applyUnmarshaller (arg@(_, IntArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (fromIntegral ($(mk arg) :: CInt) )|])|]
      applyUnmarshaller (arg@(name, UIntArg, _):as) fun
        | name == "time" = [|$(applyUnmarshaller as [|$fun (millisecondsToTime ($(mk arg) :: CUInt))|]) |]
        | otherwise      = [|$(applyUnmarshaller as [|$fun (fromIntegral ($(mk arg) :: CUInt))|]) |]
      applyUnmarshaller (arg@(_, FixedArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (wlFixedToFixed256 $(mk arg))|]) |]
      applyUnmarshaller (arg@(_, StringArg, False):as) fun = [|do str <- peekCString $(mk arg); $(applyUnmarshaller as [|$fun str|])|]
      applyUnmarshaller (arg@(_, (ObjectArg iname), False):as) fun = [|$(applyUnmarshaller as [|$fun $(mk arg)|]) |]
      applyUnmarshaller (arg@(_, (NewIdArg iname _), False):as) fun = [|$(applyUnmarshaller as [|$fun $(mk arg) |])|]
      applyUnmarshaller (arg@(_, StringArg, True):as) fun = [|do
               str <- if $(mk arg) == nullPtr
                         then return Nothing
                         else Just <$> peekCString $(mk arg)
               $(applyUnmarshaller as [|$fun str|])
               |]
      applyUnmarshaller (arg@(_, (ObjectArg iname), True):as) fun = [|$(applyUnmarshaller as [|$fun $
               let $(conP iname ([varP $ mkName "ptr___"])) = $(mk arg)
               in if $(varE $ mkName "ptr___") == nullPtr
                   then Nothing
                   else Just $(mk arg)|]) |]
      applyUnmarshaller (arg@(_, (NewIdArg iname _), True):as) fun = [|$(applyUnmarshaller as [|$fun $
               if $(mk arg) == nullPtr
                  then Nothing
                  else Just $ $(conE iname) $(mk arg)|]) |]
      applyUnmarshaller (arg@(_, ArrayArg, True):as) fun = [|
               if $(mk arg) == nullPtr
                 then $(applyUnmarshaller as [|$fun Nothing|])
                 else do
                   size <- fromIntegral <$> {#get array->size#} ($(mk arg) :: WLArray)
                   dat <- {#get array->data#} ($(mk arg) :: WLArray)
                   $(applyUnmarshaller as [|$fun (Just (size, dat)) |])
               |]
      applyUnmarshaller (arg@(_, ArrayArg, False):as) fun = [|do
               size <- fromIntegral <$> {#get array->size#} ($(mk arg) :: WLArray)
               dat <- {#get array->data#} ($(mk arg) :: WLArray)
               $(applyUnmarshaller as [|$fun (size, dat) |])
               |]
      applyUnmarshaller (arg@(_, FdArg, _):as) fun = [|$(applyUnmarshaller as [|$fun (Fd ($(mk arg)))|]) |]
      applyUnmarshaller [] fun = fun
  in  (map VarP vars, applyUnmarshaller args fun)


-- compute FunPtr size and alignment based on some dummy C type
funcSize = {#sizeof notify_func_t#} :: Integer
funcAlign = {#alignof notify_func_t#} :: Integer

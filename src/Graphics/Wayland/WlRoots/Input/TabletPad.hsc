module Graphics.Wayland.WlRoots.Input.TabletPad
    ( WlrTabletPad
    , PadEvents (..)
    , getPadEvents
    , peekPadData
    , pokePadData

    , PadButtonEvent (..)

    , PadRingSource (..)
    , PadRingEvent (..)

    , PadStripSource (..)
    , PadStripEvent (..)
    )
where

#define WLR_USE_UNSTABLE
#include <wlr/types/wlr_tablet_pad.h>

import Data.Word (Word32)
import Foreign.C.Types (CInt)
import Foreign.Ptr (Ptr, plusPtr, castPtr)
import Foreign.Storable

import Graphics.Wayland.Signal (WlSignal)

-- import {-# SOURCE #-} Graphics.Wayland.WlRoots.Input (InputDevice)
import Graphics.Wayland.WlRoots.Input.Buttons

data WlrTabletPad

data PadEvents = PadEvents
    { padEventButton :: Ptr (WlSignal PadButtonEvent)
    , padEventRing   :: Ptr (WlSignal PadRingEvent)
    , padEventStrip  :: Ptr (WlSignal PadStripEvent)
    }


getPadEvents :: Ptr WlrTabletPad -> PadEvents
getPadEvents ptr = PadEvents
    { padEventButton = #{ptr struct wlr_tablet_pad, events.button} ptr
    , padEventRing  = #{ptr struct wlr_tablet_pad, events.ring} ptr
    , padEventStrip = #{ptr struct wlr_tablet_pad, events.strip} ptr
    }

peekPadData :: Ptr WlrTabletPad -> IO (Ptr a)
peekPadData = #{peek struct wlr_tablet_pad, data}

pokePadData :: Ptr WlrTabletPad -> Ptr a -> IO ()
pokePadData = #{poke struct wlr_tablet_pad, data}

data PadButtonEvent = PadButtonEvent
    { padButtonEvtTime   :: Word32
    , padButtonEvtButton :: Word32
    , padButtonEvtState  :: ButtonState
    } deriving (Show)

instance Storable PadButtonEvent where
    sizeOf _ = #{size struct wlr_event_tablet_pad_button}
    alignment _ = #{alignment struct wlr_event_tablet_pad_button}
    peek ptr = PadButtonEvent
        <$> #{peek struct wlr_event_tablet_pad_button, time_msec} ptr
        <*> #{peek struct wlr_event_tablet_pad_button, button} ptr
        <*> #{peek struct wlr_event_tablet_pad_button, state} ptr
    poke ptr evt = do
        #{poke struct wlr_event_tablet_pad_button, time_msec} ptr $ padButtonEvtTime evt
        #{poke struct wlr_event_tablet_pad_button, button} ptr $ padButtonEvtButton evt
        #{poke struct wlr_event_tablet_pad_button, state} ptr $ padButtonEvtState evt

data PadRingSource
    = RingSourceUnknown
    | RingSourceFinger
    deriving (Show, Eq, Read)

ringSourceToInt :: Num a => PadRingSource -> a
ringSourceToInt RingSourceUnknown = #{const WLR_TABLET_PAD_RING_SOURCE_UNKNOWN}
ringSourceToInt RingSourceFinger  = #{const WLR_TABLET_PAD_RING_SOURCE_FINGER}

intToRingSource :: (Eq a, Num a, Show a) => a -> PadRingSource
intToRingSource #{const WLR_TABLET_PAD_RING_SOURCE_UNKNOWN} = RingSourceUnknown
intToRingSource #{const WLR_TABLET_PAD_RING_SOURCE_FINGER} = RingSourceFinger
intToRingSource x = error $ "Got an an unknown PadRingSource: " ++ show x

instance Storable PadRingSource where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToRingSource :: CInt -> PadRingSource) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (ringSourceToInt val :: CInt)

data PadRingEvent = PadRingEvent
    { padRingEvtTime     :: Word32
    , padRingEvtSource   :: PadRingSource
    , padRingEvtRing     :: Word32
    , padRingEvtPosition :: Double
    } deriving (Show)

instance Storable PadRingEvent where
    sizeOf _ = #{size struct wlr_event_tablet_pad_ring}
    alignment _ = #{alignment struct wlr_event_tablet_pad_ring}
    peek ptr = PadRingEvent
        <$> #{peek struct wlr_event_tablet_pad_ring, time_msec} ptr
        <*> #{peek struct wlr_event_tablet_pad_ring, source} ptr
        <*> #{peek struct wlr_event_tablet_pad_ring, ring} ptr
        <*> #{peek struct wlr_event_tablet_pad_ring, position} ptr
    poke ptr evt = do
        #{poke struct wlr_event_tablet_pad_ring, time_msec} ptr $ padRingEvtTime evt
        #{poke struct wlr_event_tablet_pad_ring, source} ptr $ padRingEvtSource evt
        #{poke struct wlr_event_tablet_pad_ring, ring} ptr $ padRingEvtRing evt
        #{poke struct wlr_event_tablet_pad_ring, position} ptr $ padRingEvtPosition evt

data PadStripSource
    = StripSourceUnknown
    | StripSourceFinger
    deriving (Show, Eq, Read)

stripSourceToInt :: Num a => PadStripSource -> a
stripSourceToInt StripSourceUnknown = #{const WLR_TABLET_PAD_STRIP_SOURCE_UNKNOWN}
stripSourceToInt StripSourceFinger  = #{const WLR_TABLET_PAD_STRIP_SOURCE_FINGER}

intToStripSource :: (Eq a, Num a, Show a) => a -> PadStripSource
intToStripSource #{const WLR_TABLET_PAD_STRIP_SOURCE_UNKNOWN} = StripSourceUnknown
intToStripSource #{const WLR_TABLET_PAD_STRIP_SOURCE_FINGER}  = StripSourceFinger
intToStripSource x = error $ "Got an an unknown PadStripSource: " ++ show x

instance Storable PadStripSource where
    sizeOf _ = #{size int}
    alignment _ = #{alignment int}
    peek = fmap (intToStripSource :: CInt -> PadStripSource) . peek . castPtr
    poke ptr val = poke (castPtr ptr) (stripSourceToInt val :: CInt)

data PadStripEvent = PadStripEvent
    { padStripEvtTime     :: Word32
    , padStripEvtSource   :: PadStripSource
    , padStripEvtStrip    :: Word32
    , padStripEvtPosition :: Double
    } deriving (Show)

instance Storable PadStripEvent where
    sizeOf _ = #{size struct wlr_event_tablet_pad_strip}
    alignment _ = #{alignment struct wlr_event_tablet_pad_strip}
    peek ptr = PadStripEvent
        <$> #{peek struct wlr_event_tablet_pad_strip, time_msec} ptr
        <*> #{peek struct wlr_event_tablet_pad_strip, source} ptr
        <*> #{peek struct wlr_event_tablet_pad_strip, strip} ptr
        <*> #{peek struct wlr_event_tablet_pad_strip, position} ptr
    poke ptr evt = do
        #{poke struct wlr_event_tablet_pad_strip, time_msec} ptr $ padStripEvtTime evt
        #{poke struct wlr_event_tablet_pad_strip, source} ptr $ padStripEvtSource evt
        #{poke struct wlr_event_tablet_pad_strip, strip} ptr $ padStripEvtStrip evt
        #{poke struct wlr_event_tablet_pad_strip, position} ptr $ padStripEvtPosition evt

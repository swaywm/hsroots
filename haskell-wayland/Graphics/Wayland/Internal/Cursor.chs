-- | This is client-side code for loading cursor themes. Provided for convenience only.
module Graphics.Wayland.Internal.Cursor (
  CursorTheme, CursorImage, Cursor,
  cursorImageSize, cursorImageHotspot, cursorImageDelay,
  cursorName, cursorImages,

  cursorThemeLoad, cursorThemeDestroy, cursorThemeGetCursor, cursorImageGetBuffer, cursorFrame
  ) where

import Control.Monad (liftM)
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.IO.Unsafe (unsafePerformIO)

import Graphics.Wayland.Internal.SpliceClientTypes (Shm(..), Buffer(..))

#include <wayland-cursor.h>

{#context prefix="wl"#}


-- | struct wl_cursor_theme;
{#pointer * cursor_theme as CursorTheme newtype#}


-- | struct wl_cursor_image {
-- 	uint32_t width;		/* actual width */
-- 	uint32_t height;	/* actual height */
-- 	uint32_t hotspot_x;	/* hot spot x (must be inside image) */
-- 	uint32_t hotspot_y;	/* hot spot y (must be inside image) */
-- 	uint32_t delay;		/* animation delay to next frame (ms) */
-- };
{#pointer * cursor_image as CursorImage newtype#}

cursorImageSize :: CursorImage -> (Word, Word)
cursorImageSize (CursorImage ci) = unsafePerformIO $ do -- CursorImages are immutable
  width <- {#get cursor_image->width#} ci
  height <- {#get cursor_image->height#} ci
  return (fromIntegral width, fromIntegral height)

cursorImageHotspot :: CursorImage -> (Word, Word)
cursorImageHotspot (CursorImage ci) = unsafePerformIO $ do -- CursorImages are immutable
  x <- {#get cursor_image->hotspot_x#} ci
  y <- {#get cursor_image->hotspot_y#} ci
  return (fromIntegral x, fromIntegral y)

cursorImageDelay :: CursorImage -> Word
cursorImageDelay (CursorImage ci) = unsafePerformIO $ liftM fromIntegral $ {#get cursor_image->delay#} ci -- CursorImages are immutable

-- | struct wl_cursor {
-- 	unsigned int image_count;
-- 	struct wl_cursor_image **images;
-- 	char *name;
-- };
{#pointer * cursor as Cursor newtype#}
cursorName :: Cursor -> String
cursorName (Cursor c) = unsafePerformIO $ do
  cstr <- {#get cursor->name#} c
  peekCString cstr

cursorImages :: Cursor -> [CursorImage]
cursorImages (Cursor c) = unsafePerformIO $ do
  imagesPtr <- (\ ptr -> (peekByteOff ptr {#offsetof cursor->images#} :: IO (Ptr (Ptr CursorImage)))) c
  count <- {#get cursor->image_count#} c
  return imagesPtr
  ptrs <- peekArray (fromIntegral count) imagesPtr
  return $ map CursorImage ptrs

-- struct wl_shm;
{#pointer * shm as Shm nocode#}

-- | struct wl_cursor_theme *
-- wl_cursor_theme_load(const char *name, int size, struct wl_shm *shm);
{#fun unsafe cursor_theme_load as cursorThemeLoad {`String', `Int', `Shm'} -> `CursorTheme'#}

-- | void
-- wl_cursor_theme_destroy(struct wl_cursor_theme *theme);
{#fun unsafe cursor_theme_destroy as cursorThemeDestroy {`CursorTheme'} -> `()' #}

-- | struct wl_cursor *
-- wl_cursor_theme_get_cursor(struct wl_cursor_theme *theme,
-- 			   const char *name);
{#fun unsafe cursor_theme_get_cursor as cursorThemeGetCursor {`CursorTheme', `String'} -> `Cursor' #}

{#pointer * buffer as Buffer nocode#}
-- | struct wl_buffer *
-- wl_cursor_image_get_buffer(struct wl_cursor_image *image);
--
-- From the wayland docs: do not destroy the returned buffer.
{#fun unsafe cursor_image_get_buffer as cursorImageGetBuffer {`CursorImage'} -> `Buffer' #}

-- | int
-- wl_cursor_frame(struct wl_cursor *cursor, uint32_t time);
{#fun unsafe cursor_frame as cursorFrame {`Cursor', `Int'} -> `Int' #}

-- | Client-side
module Graphics.Wayland.Internal.EGL (
  EGLWindow, eglWindowCreate, eglWindowDestroy, eglWindowResize, eglWindowGetAttachedSize
  ) where


import Control.Monad
import Foreign
import Foreign.C.Types
import Foreign.C.String

import Graphics.Wayland.Internal.SpliceClientTypes (Surface(..))

#include <wayland-egl.h>

{#context prefix="wl"#}


-- lol this is 100% unused.
-- #define WL_EGL_PLATFORM 1

{#pointer * surface as Surface nocode#}

-- struct wl_egl_window;
{#pointer * egl_window as EGLWindow newtype#}

-- struct wl_egl_window *
-- wl_egl_window_create(struct wl_surface *surface,
-- 		     int width, int height);
{#fun unsafe egl_window_create as eglWindowCreate {`Surface', `Int', `Int'} -> `EGLWindow' #}

-- void
-- wl_egl_window_destroy(struct wl_egl_window *egl_window);
{#fun unsafe egl_window_destroy as eglWindowDestroy {`EGLWindow'} -> `()' #}

-- void
-- wl_egl_window_resize(struct wl_egl_window *egl_window,
-- 		     int width, int height,
-- 		     int dx, int dy);
{#fun unsafe egl_window_resize as eglWindowResize {`EGLWindow', `Int', `Int', `Int', `Int'} -> `()' #}


-- void
-- wl_egl_window_get_attached_size(struct wl_egl_window *egl_window,
-- 				int *width, int *height);
-- withInt = with.fromIntegral 0
peekInt = liftM fromIntegral . peek
{#fun unsafe egl_window_get_attached_size as eglWindowGetAttachedSize {`EGLWindow', alloca- `Int' peekInt*, alloca- `Int' peekInt*} -> `()' #}

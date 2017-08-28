module Graphics.Wayland.Internal.Client (
  Result(..),

  displayConnect, displayConnectName, displayConnectFd, displayDisconnect, displayGetFd,
  displayDispatch, displayDispatchPending,

  displayGetError, displayFlush, displayRoundtrip,

  displayPrepareRead, displayCancelRead, displayReadEvents
  ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Posix.Types

import Graphics.Wayland.Internal.SpliceClientTypes (Display(..))
import Graphics.Wayland.Internal.SpliceClientInternal
import Graphics.Wayland.Internal.SpliceClient
import Graphics.Wayland


#include <wayland-client.h>

{#context prefix="wl"#}

unFd :: Fd -> CInt
unFd (Fd k) = k

makeWith' :: b -> (b -> IO c) -> IO c
makeWith' b f = f b

withNullPtr = makeWith' nullPtr

codeToNothing :: Int -> Int -> Maybe Int
codeToNothing j k
  | j == k    = Nothing
  | otherwise = Just k

codeNeg1ToNothing :: CInt -> Maybe Int
codeNeg1ToNothing = codeToNothing (-1) . fromIntegral

code0ToNothing    :: CInt -> Maybe Int
code0ToNothing    = codeToNothing 0    . fromIntegral

-- Data types

-- In the case of the Client side, these are all just abstract pointer objects.

-- struct wl_display pointer (nocode since its interface is generated in SpliceProtocol)
{#pointer * display as Display nocode#}

-- -- | struct wl_event_queue pointer (generate type since this is not an interface)
-- {#pointer * event_queue as EventQueue newtype#}

-- -- | struct wl_interface pointer. for internal use only. (proxy typing)
-- {#pointer * interface as Interface newtype#}



-- Functions/methods


-- -- void wl_event_queue_destroy(struct wl_event_queue *queue);
-- {#fun unsafe event_queue_destroy as ^ {`EventQueue'} -> `()'#}

-- void wl_proxy_marshal(struct wl_proxy *p, uint32_t opcode, ...);

-- void wl_proxy_marshal_array(struct wl_proxy *p, uint32_t opcode,
--                               union wl_argument *args);

-- struct wl_proxy *wl_proxy_create(struct wl_proxy *factory,
--                                    const struct wl_interface *interface);

-- struct wl_proxy *wl_proxy_marshal_constructor(struct wl_proxy *proxy,
--                                               uint32_t opcode,
--                                               const struct wl_interface *interface,
--                                               ...);
-- struct wl_proxy *
-- wl_proxy_marshal_array_constructor(struct wl_proxy *proxy,
--                                    uint32_t opcode, union wl_argument *args,
--                                    const struct wl_interface *interface);

-- void wl_proxy_destroy(struct wl_proxy *proxy);
-- int wl_proxy_add_listener(struct wl_proxy *proxy,
--                           void (**implementation)(void), void *data);
-- const void *wl_proxy_get_listener(struct wl_proxy *proxy);
-- int wl_proxy_add_dispatcher(struct wl_proxy *proxy,
--                             wl_dispatcher_func_t dispatcher_func,
--                             const void * dispatcher_data, void *data);
-- void wl_proxy_set_user_data(struct wl_proxy *proxy, void *user_data);
-- void *wl_proxy_get_user_data(struct wl_proxy *proxy);
-- uint32_t wl_proxy_get_id(struct wl_proxy *proxy);
-- const char *wl_proxy_get_class(struct wl_proxy *proxy);
-- void wl_proxy_set_queue(struct wl_proxy *proxy, struct wl_event_queue *queue);


receiveMaybeDisplay :: Display -> Maybe Display
receiveMaybeDisplay (Display x)
  | x == nullPtr = Nothing
  | otherwise    = Just (Display x)

-- struct wl_display *wl_display_connect(const char *name);
-- | Connect to a display with a specified name
{#fun unsafe display_connect as displayConnectName {`String'} -> `Maybe Display' receiveMaybeDisplay #}

-- | Connect to the default display by passing a null pointer
{#fun unsafe display_connect as displayConnect {withNullPtr- `Ptr CChar'} -> `Maybe Display' receiveMaybeDisplay #}

-- struct wl_display *wl_display_connect_to_fd(int fd);
-- | Connect to a display by file descriptor
{#fun unsafe display_connect_to_fd as displayConnectFd {unFd `Fd'} -> `Maybe Display' receiveMaybeDisplay #}

-- void wl_display_disconnect(struct wl_display *display);
{#fun unsafe display_disconnect as displayDisconnect {`Display'} -> `()' #}

-- int wl_display_get_fd(struct wl_display *display);
{#fun unsafe display_get_fd as displayGetFd {`Display'} -> `Fd' Fd #}

-- int wl_display_dispatch(struct wl_display *display);
-- | wl_display_dispatch. Returns @Nothing@ on failure or @Just k@ if k events were processed.
--
-- Strictly safe!!! This *will* call back into Haskell code!
{#fun display_dispatch as displayDispatch {`Display'} -> `Maybe Int' codeNeg1ToNothing #}

-- -- int wl_display_dispatch_queue(struct wl_display *display,
-- --                               struct wl_event_queue *queue);
-- -- | wl_display_dispatch_queue. Returns @Nothing@ on failure or @Just k@ if k events were processed.
-- --
-- -- Strictly safe!!! This *will* call back into Haskell code!
-- {#fun display_dispatch_queue as displayDispatchQueue {`Display', `EventQueue'} -> `Maybe Int' codeNeg1ToNothing #}

-- -- int wl_display_dispatch_queue_pending(struct wl_display *display,
-- --                                       struct wl_event_queue *queue);
-- -- | wl_display_dispatch_queue_pending. Returns @Nothing@ on failure or @Just k@ if k events were processed.
-- --
-- -- Strictly safe!!! This *will* call back into Haskell code!
-- {#fun display_dispatch_queue_pending as displayDispatchQueuePending {`Display', `EventQueue'} -> `Maybe Int' codeNeg1ToNothing #}

-- int wl_display_dispatch_pending(struct wl_display *display);
-- | wl_display_dispatch_pending. Returns @Nothing@ on failure or @Just k@ if k events were processed.
--
-- Strictly safe!!! This *will* call back into Haskell code!
{#fun display_dispatch_pending as displayDispatchPending {`Display'} -> `Maybe Int' codeNeg1ToNothing #}

-- int wl_display_get_error(struct wl_display *display);
-- | @Nothing@ if no error occurred or @Just k@ if the latest error had code k
--
-- Note (from the wayland documentation): errors are fatal. If this function returns a @Just@ value, the display can no longer be used.
{#fun unsafe display_get_error as displayGetError {`Display'} -> `Maybe Int' code0ToNothing #}

-- int wl_display_flush(struct wl_display *display);
-- | @Nothing@ on failure or @Just k@ if k bytes were sent
--
-- __It is not clear to me if this is can be unsafe (ie. can this call back into haskell code?).__
{#fun display_flush as displayFlush {`Display'} -> `Maybe Int' codeNeg1ToNothing #}

-- int wl_display_roundtrip(struct wl_display *display);
-- | @Nothing@ on failure or @Just k@ if k events were dispatched.
--
-- __It is not clear to me if this is can be unsafe (ie. can this call back into haskell code?).__
{#fun display_roundtrip as displayRoundtrip {`Display'} -> `Maybe Int' codeNeg1ToNothing #}

-- -- struct wl_event_queue *wl_display_create_queue(struct wl_display *display);
-- -- | Docs say that wl_display_create_queue may return NULL on failure, but that only happens when it's out of memory
-- {#fun unsafe display_create_queue as displayCreateQueue {`Display'} -> `EventQueue' #}

-- -- int wl_display_prepare_read_queue(struct wl_display *display,
-- --                                   struct wl_event_queue *queue);
-- {#fun unsafe display_prepare_read_queue as displayPrepareReadQueue {`Display', `EventQueue'} -> `Result' errToResult #}

-- int wl_display_prepare_read(struct wl_display *display);
{#fun unsafe display_prepare_read as displayPrepareRead {`Display'} -> `Result' errToResult #}

-- void wl_display_cancel_read(struct wl_display *display);
{#fun unsafe display_cancel_read as displayCancelRead {`Display'} -> `()' #}

-- int wl_display_read_events(struct wl_display *display);
-- | This will read events from the file descriptor for the display.
--   This function does not dispatch events, it only reads and queues events into their corresponding event queues.
--
--   Before calling this function, wl_display_prepare_read() must be called first.
{#fun unsafe display_read_events as displayReadEvents {`Display'} -> `Result' errToResult #}

-- void wl_log_set_handler_client(wl_log_func_t handler);

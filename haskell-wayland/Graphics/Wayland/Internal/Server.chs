-- Trying my best to piss off the Safe Haskell guys
{-# LANGUAGE TemplateHaskell, GeneralizedNewtypeDeriving #-}
module Graphics.Wayland.Internal.Server (
  ClientState(..), clientStateReadable, clientStateWritable, clientStateHangup,
  clientStateError,

  EventLoop, EventSource,

  EventLoopFdFunc, EventLoopTimerFunc, EventLoopSignalFunc, EventLoopIdleFunc,

  eventLoopCreate, eventLoopDestroy, eventLoopAddFd, eventSourceFdUpdate,
  eventLoopAddTimer, eventLoopAddSignal, eventSourceTimerUpdate, eventSourceRemove,
  eventSourceCheck, eventLoopDispatch, eventLoopDispatchIdle, eventLoopAddIdle, eventLoopGetFd,

  DisplayServer(..), displayCreate, displayDestroy, displayGetEventLoop, displayAddSocket,
  displayTerminate, displayRun, displayFlushClients, displayGetSerial, displayNextSerial,

  clientCreate, clientDestroy, clientFlush, clientGetCredentials, clientPostNoMemory,

  ShmBuffer, shmBufferBeginAccess, shmBufferEndAccess, shmBufferGet, shmBufferGetData,
  shmBufferGetStride, shmBufferGetFormat, shmBufferGetWidth, shmBufferGetHeight,
  displayInitShm, displayAddShmFormat, shmBufferCreate
  ) where

import Control.Monad (liftM)
import Data.Functor ((<$>))
import Data.Flags
import Data.Flags.TH
import Foreign
import Foreign.C.Types
import Foreign.C.String
import System.Posix.Types

import Graphics.Wayland.Internal.ServerClientState -- for the WL_EVENT_* constants
import Graphics.Wayland.Internal.SpliceServerTypes (Buffer(..))
import Graphics.Wayland.Internal.SpliceServerInternal
import Graphics.Wayland.Internal.SpliceServer
import Graphics.Wayland.Internal.Util (Client(..))
import Graphics.Wayland


#include <wayland-server.h>

{#context prefix="wl"#}





boolToCInt :: Bool -> CInt
boolToCInt True = 1
boolToCInt False = 0



unFd :: Fd -> CInt
unFd (Fd n) = n

makeWith :: (a -> IO b) -> (a -> (b -> IO c) -> IO c)
makeWith func = \ a f -> do
  b <- func a
  f b

makeWith' :: b -> (b -> IO c) -> IO c
makeWith' b f = f b

withNullPtr = makeWith' nullPtr

-- | enum {
-- 	WL_EVENT_READABLE = 0x01,
-- 	WL_EVENT_WRITABLE = 0x02,
-- 	WL_EVENT_HANGUP   = 0x04,
-- 	WL_EVENT_ERROR    = 0x08
-- };
--
-- The "uint32_t mask" argument passed to a variety of functions in this file is a bitmask
-- detailing the state of the client.
$(bitmaskWrapper "ClientState" ''CUInt [''Num, ''Integral, ''Real, ''Enum, ''Ord] [
  ("clientStateReadable", fromIntegral $ fromEnum ClientReadable),
  ("clientStateWritable", fromIntegral $  fromEnum ClientWritable),
  ("clientStateHangup",   fromIntegral $  fromEnum ClientHangup),
  ("clientStateError",    fromIntegral $  fromEnum ClientError)
  ])

-- | struct wl_event_loop;
{#pointer * event_loop as EventLoop newtype#}

-- | struct wl_event_source;
{#pointer * event_source as EventSource newtype#}

type CEventLoopFdFunc = CInt -> {#type uint32_t#} -> Ptr () -> IO CInt
-- | typedef int (*wl_event_loop_fd_func_t)(int fd, uint32_t mask, void *data);
type EventLoopFdFunc = Int -> ClientState -> IO Bool
foreign import ccall unsafe "wrapper" makeFdFunPtr :: CEventLoopFdFunc -> IO (FunPtr CEventLoopFdFunc)
marshallEventLoopFdFunc :: EventLoopFdFunc -> IO (FunPtr CEventLoopFdFunc)
marshallEventLoopFdFunc func = makeFdFunPtr $ \ fd mask _ -> boolToCInt <$> func (fromIntegral fd) (fromIntegral mask)
melff = makeWith marshallEventLoopFdFunc

type CEventLoopTimerFunc = Ptr () -> IO CInt
-- | typedef int (*wl_event_loop_timer_func_t)(void *data);
type EventLoopTimerFunc = IO Bool
foreign import ccall unsafe "wrapper" makeTimerFunPtr :: CEventLoopTimerFunc -> IO (FunPtr CEventLoopTimerFunc)
marshallEventLoopTimerFunc :: EventLoopTimerFunc -> IO (FunPtr CEventLoopTimerFunc)
marshallEventLoopTimerFunc func = makeTimerFunPtr $ \ _ -> boolToCInt <$> func
meltf = makeWith marshallEventLoopTimerFunc

type CEventLoopSignalFunc = CInt -> Ptr () -> IO CInt
-- | typedef int (*wl_event_loop_signal_func_t)(int signal_number, void *data);
type EventLoopSignalFunc = Int -> IO Bool
foreign import ccall unsafe "wrapper" makeSignalFunPtr :: CEventLoopSignalFunc -> IO (FunPtr CEventLoopSignalFunc)
marshallEventLoopSignalFunc :: EventLoopSignalFunc -> IO (FunPtr CEventLoopSignalFunc)
marshallEventLoopSignalFunc func = makeSignalFunPtr $ \ x _ -> boolToCInt <$> func (fromIntegral x)
melsf = makeWith marshallEventLoopSignalFunc

-- typedef void (*wl_event_loop_idle_func_t)(void *data);
type CEventLoopIdleFunc = Ptr () -> IO ()
type EventLoopIdleFunc = IO ()
foreign import ccall unsafe "wrapper" makeIdleFunPtr :: CEventLoopIdleFunc -> IO (FunPtr CEventLoopIdleFunc)
marshallEventLoopIdleFunc :: EventLoopIdleFunc -> IO (FunPtr CEventLoopIdleFunc)
marshallEventLoopIdleFunc func = makeIdleFunPtr $ \ _ -> func
melif = makeWith marshallEventLoopIdleFunc

-- |struct wl_event_loop *wl_event_loop_create(void);
{#fun unsafe event_loop_create as eventLoopCreate {} -> `EventLoop' #}

-- |void wl_event_loop_destroy(struct wl_event_loop *loop);
{#fun unsafe event_loop_destroy as eventLoopDestroy {`EventLoop'} -> `()' #}

-- | struct wl_event_source *wl_event_loop_add_fd(struct wl_event_loop *loop,
-- 					     int fd, uint32_t mask,
-- 					     wl_event_loop_fd_func_t func,
-- 					     void *data);
{#fun unsafe event_loop_add_fd as eventLoopAddFd {`EventLoop', unFd `Fd', fromIntegral `ClientState', melff* `EventLoopFdFunc', withNullPtr- `Ptr ()'} -> `EventSource' #}

-- | int wl_event_source_fd_update(struct wl_event_source *source, uint32_t mask);
{#fun unsafe event_source_fd_update as eventSourceFdUpdate {`EventSource', fromIntegral `ClientState'} -> `Result' errToResult #}

-- | struct wl_event_source *wl_event_loop_add_timer(struct wl_event_loop *loop,
-- 						wl_event_loop_timer_func_t func,
-- 						void *data);
{#fun unsafe event_loop_add_timer as eventLoopAddTimer {`EventLoop', meltf* `EventLoopTimerFunc', withNullPtr- `Ptr ()'} -> `EventSource'#}

-- | struct wl_event_source *
-- wl_event_loop_add_signal(struct wl_event_loop *loop,
-- 			int signal_number,
-- 			wl_event_loop_signal_func_t func,
-- 			void *data);
{#fun unsafe event_loop_add_signal as eventLoopAddSignal {`EventLoop', `Int', melsf* `EventLoopSignalFunc', withNullPtr- `Ptr ()'} -> `EventSource' #}

-- | int wl_event_source_timer_update(struct wl_event_source *source,
-- 				 int ms_delay);
{#fun unsafe event_source_timer_update as eventSourceTimerUpdate {`EventSource', `Int'} -> `Result' errToResult #}

-- | int wl_event_source_remove(struct wl_event_source *source);
{#fun unsafe event_source_remove as eventSourceRemove {`EventSource'} -> `()' #}

-- | void wl_event_source_check(struct wl_event_source *source);
{#fun unsafe event_source_check as eventSourceCheck {`EventSource'} -> `()' #}

-- | int wl_event_loop_dispatch(struct wl_event_loop *loop, int timeout);
--
-- SAFE!!
{#fun event_loop_dispatch as eventLoopDispatch {`EventLoop', `Int'} -> `Result' errToResult#}

-- | void wl_event_loop_dispatch_idle(struct wl_event_loop *loop);
{#fun event_loop_dispatch_idle as eventLoopDispatchIdle {`EventLoop'} -> `()' #}

-- | struct wl_event_source *wl_event_loop_add_idle(struct wl_event_loop *loop,
-- 					       wl_event_loop_idle_func_t func,
-- 					       void *data);
{#fun event_loop_add_idle as eventLoopAddIdle {`EventLoop', melif* `EventLoopIdleFunc', withNullPtr- `Ptr ()'} -> `EventSource' #}

-- | int wl_event_loop_get_fd(struct wl_event_loop *loop);
{#fun unsafe event_loop_get_fd as eventLoopGetFd {`EventLoop'} -> `Fd' Fd #}


-- EXPOSED UNTIL HERE


-- struct wl_client;
-- defined in .Util
{#pointer * client as Client newtype nocode#}

receiveMaybeClient :: Client -> Maybe Client
receiveMaybeClient (Client x)
  | x == nullPtr = Nothing
  | otherwise    = Just (Client x)

-- |struct wl_display;
--
-- this is called a Compositor in e.g weston, QtWayland
--
-- this is NOT an instance of a wl_resource or a wl_proxy! it is a global server status singleton listing e.g. connected clients.
{#pointer * display as DisplayServer newtype #}

-- struct wl_listener;
-- struct wl_resource;
-- struct wl_global;

-- typedef void (*wl_notify_func_t)(struct wl_listener *listener, void *data);
-- void wl_event_loop_add_destroy_listener(struct wl_event_loop *loop,
-- 					struct wl_listener * listener);
-- struct wl_listener *wl_event_loop_get_destroy_listener(
-- 					struct wl_event_loop *loop,
-- 					wl_notify_func_t notify);

-- | struct wl_display *wl_display_create(void);
{#fun unsafe display_create as displayCreate {} -> `DisplayServer' #}

-- | void wl_display_destroy(struct wl_display *display);
{#fun unsafe display_destroy as displayDestroy {`DisplayServer'} -> `()' #}

-- | struct wl_event_loop *wl_display_get_event_loop(struct wl_display *display);
{#fun unsafe display_get_event_loop as displayGetEventLoop {`DisplayServer'} -> `EventLoop' #}

-- | int wl_display_add_socket(struct wl_display *display, const char *name);
withMaybeCString :: Maybe String -> (CString -> IO a) -> IO a
withMaybeCString Nothing fun = fun nullPtr
withMaybeCString (Just str) fun = withCString str fun
{#fun unsafe display_add_socket as displayAddSocket {`DisplayServer', withMaybeCString* `Maybe String'} -> `Result' errToResult #}

-- | void wl_display_terminate(struct wl_display *display);
{#fun unsafe display_terminate as displayTerminate {`DisplayServer'} -> `()' #}

-- | void wl_display_run(struct wl_display *display);
--
-- STRICTLY SAFE!!!
{#fun display_run as displayRun {`DisplayServer'} -> `()' #}

-- | void wl_display_flush_clients(struct wl_display *display);
{#fun display_flush_clients as displayFlushClients {`DisplayServer'} -> `()' #}

-- typedef void (*wl_global_bind_func_t)(struct wl_client *client, void *data,
-- 				      uint32_t version, uint32_t id);

-- not sure what these two functions are for
-- | uint32_t wl_display_get_serial(struct wl_display *display);
{#fun unsafe display_get_serial as displayGetSerial {`DisplayServer'} -> `Word' fromIntegral #}

-- | uint32_t wl_display_next_serial(struct wl_display *display);
{#fun unsafe display_next_serial as displayNextSerial {`DisplayServer'} -> `Word' fromIntegral #}

-- void wl_display_add_destroy_listener(struct wl_display *display,
-- 				     struct wl_listener *listener);
-- struct wl_listener *wl_display_get_destroy_listener(struct wl_display *display,
-- 						    wl_notify_func_t notify);

-- struct wl_global *wl_global_create(struct wl_display *display,
-- 				   const struct wl_interface *interface,
-- 				   int version,
-- 				   void *data, wl_global_bind_func_t bind);
-- void wl_global_destroy(struct wl_global *global);

-- | struct wl_client *wl_client_create(struct wl_display *display, int fd);
{#fun unsafe client_create as clientCreate {`DisplayServer', unFd `Fd'} -> `Maybe Client' receiveMaybeClient #}

-- | void wl_client_destroy(struct wl_client *client);
{#fun unsafe client_destroy as clientDestroy {`Client'} -> `()' #}

-- | void wl_client_flush(struct wl_client *client);
{#fun unsafe client_flush as clientFlush {`Client'} -> `()' #}

peekPid = liftM CPid . liftM fromIntegral . peek
peekUid = liftM CUid . liftM fromIntegral . peek
peekGid = liftM CGid . liftM fromIntegral . peek
-- | void wl_client_get_credentials(struct wl_client *client,
-- 			       pid_t *pid, uid_t *uid, gid_t *gid);
{#fun unsafe client_get_credentials as clientGetCredentials {`Client', alloca- `ProcessID' peekPid*, alloca- `UserID' peekUid*, alloca- `GroupID' peekGid*} -> `()' #}

-- void wl_client_add_destroy_listener(struct wl_client *client,
-- 				    struct wl_listener *listener);
-- struct wl_listener *wl_client_get_destroy_listener(struct wl_client *client,
-- 						   wl_notify_func_t notify);

-- this function should not be needed
-- struct wl_resource *
-- wl_client_get_object(struct wl_client *client, uint32_t id);
-- void
-- | wl_client_post_no_memory(struct wl_client *client);
{#fun unsafe client_post_no_memory as clientPostNoMemory {`Client'} -> `()' #}

-- /** \class wl_listener
--  *
--  * \brief A single listener for Wayland signals
--  *
--  * wl_listener provides the means to listen for wl_signal notifications. Many
--  * Wayland objects use wl_listener for notification of significant events like
--  * object destruction.
--  *
--  * Clients should create wl_listener objects manually and can register them as
--  * listeners to signals using #wl_signal_add, assuming the signal is
--  * directly accessible. For opaque structs like wl_event_loop, adding a
--  * listener should be done through provided accessor methods. A listener can
--  * only listen to one signal at a time.
--  *
--  * ~~~
--  * struct wl_listener your_listener;
--  *
--  * your_listener.notify = your_callback_method;
--  *
--  * \comment{Direct access}
--  * wl_signal_add(&some_object->destroy_signal, &your_listener);
--  *
--  * \comment{Accessor access}
--  * wl_event_loop *loop = ...;
--  * wl_event_loop_add_destroy_listener(loop, &your_listener);
--  * ~~~
--  *
--  * If the listener is part of a larger struct, #wl_container_of can be used
--  * to retrieve a pointer to it:
--  *
--  * \code
--  * void your_listener(struct wl_listener *listener, void *data)
--  * {
--  * 	struct your_data *data;
--  *
--  * 	your_data = wl_container_of(listener, data, your_member_name);
--  * }
--  * \endcode
--  *
--  * If you need to remove a listener from a signal, use #wl_list_remove.
--  *
--  * \code
--  * wl_list_remove(&your_listener.link);
--  * \endcode
--  *
--  * \sa wl_signal
--  */
-- struct wl_listener {
-- 	struct wl_list link;
-- 	wl_notify_func_t notify;
-- };

-- /** \class wl_signal
--  *
--  * \brief A source of a type of observable event
--  *
--  * Signals are recognized points where significant events can be observed.
--  * Compositors as well as the server can provide signals. Observers are
--  * wl_listener's that are added through #wl_signal_add. Signals are emitted
--  * using #wl_signal_emit, which will invoke all listeners until that
--  * listener is removed by #wl_list_remove (or whenever the signal is
--  * destroyed).
--  *
--  * \sa wl_listener for more information on using wl_signal
--  */
-- struct wl_signal {
-- 	struct wl_list listener_list;
-- };

-- /** Initialize a new \ref wl_signal for use.
--  *
--  * \param signal The signal that will be initialized
--  *
--  * \memberof wl_signal
--  */
-- static inline void
-- wl_signal_init(struct wl_signal *signal)
-- {
-- 	wl_list_init(&signal->listener_list);
-- }

-- /** Add the specified listener to this signal.
--  *
--  * \param signal The signal that will emit events to the listener
--  * \param listener The listener to add
--  *
--  * \memberof wl_signal
--  */
-- static inline void
-- wl_signal_add(struct wl_signal *signal, struct wl_listener *listener)
-- {
-- 	wl_list_insert(signal->listener_list.prev, &listener->link);
-- }

-- /** Gets the listener struct for the specified callback.
--  *
--  * \param signal The signal that contains the specified listener
--  * \param notify The listener that is the target of this search
--  * \return the list item that corresponds to the specified listener, or NULL
--  * if none was found
--  *
--  * \memberof wl_signal
--  */
-- static inline struct wl_listener *
-- wl_signal_get(struct wl_signal *signal, wl_notify_func_t notify)
-- {
-- 	struct wl_listener *l;

-- 	wl_list_for_each(l, &signal->listener_list, link)
-- 		if (l->notify == notify)
-- 			return l;

-- 	return NULL;
-- }

-- /** Emits this signal, notifying all registered listeners.
--  *
--  * \param signal The signal object that will emit the signal
--  * \param data The data that will be emitted with the signal
--  *
--  * \memberof wl_signal
--  */
-- static inline void
-- wl_signal_emit(struct wl_signal *signal, void *data)
-- {
-- 	struct wl_listener *l, *next;

-- 	wl_list_for_each_safe(l, next, &signal->listener_list, link)
-- 		l->notify(l, data);
-- }

-- typedef void (*wl_resource_destroy_func_t)(struct wl_resource *resource);

-- NOTE not binding to deprecated stuff, cause wayland is bad enough as it is.

-- /*
--  * Post an event to the client's object referred to by 'resource'.
--  * 'opcode' is the event number generated from the protocol XML
--  * description (the event name). The variable arguments are the event
--  * parameters, in the order they appear in the protocol XML specification.
--  *
--  * The variable arguments' types are:
--  * - type=uint: 	uint32_t
--  * - type=int:		int32_t
--  * - type=fixed:	wl_fixed_t
--  * - type=string:	(const char *) to a nil-terminated string
--  * - type=array:	(struct wl_array *)
--  * - type=fd:		int, that is an open file descriptor
--  * - type=new_id:	(struct wl_object *) or (struct wl_resource *)
--  * - type=object:	(struct wl_object *) or (struct wl_resource *)
--  */
-- void wl_resource_post_event(struct wl_resource *resource,
-- 			    uint32_t opcode, ...);
-- void wl_resource_post_event_array(struct wl_resource *resource,
-- 				  uint32_t opcode, union wl_argument *args);
-- void wl_resource_queue_event(struct wl_resource *resource,
-- 			     uint32_t opcode, ...);
-- void wl_resource_queue_event_array(struct wl_resource *resource,
-- 				   uint32_t opcode, union wl_argument *args);

-- /* msg is a printf format string, variable args are its args. */
-- void wl_resource_post_error(struct wl_resource *resource,
-- 			    uint32_t code, const char *msg, ...)
-- 	__attribute__ ((format (printf, 3, 4)));
-- void wl_resource_post_no_memory(struct wl_resource *resource);

-- #include "wayland-server-protocol.h"

-- struct wl_display *
-- wl_client_get_display(struct wl_client *client);

-- struct wl_resource *
-- wl_resource_create(struct wl_client *client,
-- 		   const struct wl_interface *interface,
-- 		   int version, uint32_t id);
-- void
-- wl_resource_set_implementation(struct wl_resource *resource,
-- 			       const void *implementation,
-- 			       void *data,
-- 			       wl_resource_destroy_func_t destroy);
-- void
-- wl_resource_set_dispatcher(struct wl_resource *resource,
-- 			   wl_dispatcher_func_t dispatcher,
-- 			   const void *implementation,
-- 			   void *data,
-- 			   wl_resource_destroy_func_t destroy);

-- void
-- wl_resource_destroy(struct wl_resource *resource);
-- uint32_t
-- wl_resource_get_id(struct wl_resource *resource);
-- struct wl_list *
-- wl_resource_get_link(struct wl_resource *resource);
-- struct wl_resource *
-- wl_resource_from_link(struct wl_list *resource);
-- struct wl_resource *
-- wl_resource_find_for_client(struct wl_list *list, struct wl_client *client);
-- struct wl_client *
-- wl_resource_get_client(struct wl_resource *resource);
-- void
-- wl_resource_set_user_data(struct wl_resource *resource, void *data);
-- void *
-- wl_resource_get_user_data(struct wl_resource *resource);
-- int
-- wl_resource_get_version(struct wl_resource *resource);
-- void
-- wl_resource_set_destructor(struct wl_resource *resource,
-- 			   wl_resource_destroy_func_t destroy);
-- int
-- wl_resource_instance_of(struct wl_resource *resource,
-- 			const struct wl_interface *interface,
-- 			const void *implementation);

-- void
-- wl_resource_add_destroy_listener(struct wl_resource *resource,
-- 				 struct wl_listener * listener);
-- struct wl_listener *
-- wl_resource_get_destroy_listener(struct wl_resource *resource,
-- 				 wl_notify_func_t notify);

-- #define wl_resource_for_each(resource, list)					\
-- 	for (resource = 0, resource = wl_resource_from_link((list)->next);	\
-- 	     wl_resource_get_link(resource) != (list);				\
-- 	     resource = wl_resource_from_link(wl_resource_get_link(resource)->next))

-- #define wl_resource_for_each_safe(resource, tmp, list)					\
-- 	for (resource = 0, tmp = 0,							\
-- 	     resource = wl_resource_from_link((list)->next),	\
-- 	     tmp = wl_resource_from_link((list)->next->next);	\
-- 	     wl_resource_get_link(resource) != (list);				\
-- 	     resource = tmp,							\
-- 	     tmp = wl_resource_from_link(wl_resource_get_link(resource)->next))


-- this is a dirty hack to make shmBufferGet accept a Buffer
{#pointer * resource as Buffer newtype nocode#}
-- struct wl_shm_buffer;
{#pointer * shm_buffer as ShmBuffer newtype#}
receiveMaybeShmBuffer :: ShmBuffer -> Maybe ShmBuffer
receiveMaybeShmBuffer (ShmBuffer x)
  | x == nullPtr = Nothing
  | otherwise    = Just (ShmBuffer x)

-- | void
-- wl_shm_buffer_begin_access(struct wl_shm_buffer *buffer);
--
-- Lock the memory for reading. Needed to protect the server against SIGBUS signals
-- caused by the client resizing the buffer.
{#fun unsafe shm_buffer_begin_access as shmBufferBeginAccess {`ShmBuffer'} -> `()' #}

-- |void
-- wl_shm_buffer_end_access(struct wl_shm_buffer *buffer);
--
-- Unlock the memory.
{#fun unsafe shm_buffer_end_access as shmBufferEndAccess {`ShmBuffer'} -> `()' #}

-- | struct wl_shm_buffer *
-- wl_shm_buffer_get(struct wl_resource *resource);
{#fun unsafe shm_buffer_get as shmBufferGet {`Buffer'} -> `Maybe ShmBuffer' receiveMaybeShmBuffer #}

-- | void *
-- wl_shm_buffer_get_data(struct wl_shm_buffer *buffer);
{#fun unsafe shm_buffer_get_data as shmBufferGetData {`ShmBuffer'} -> `Ptr ()' id #}

-- | int32_t
-- wl_shm_buffer_get_stride(struct wl_shm_buffer *buffer);
{#fun unsafe shm_buffer_get_stride as shmBufferGetStride {`ShmBuffer'} -> `Int' #}

-- | uint32_t
-- wl_shm_buffer_get_format(struct wl_shm_buffer *buffer);
{#fun unsafe shm_buffer_get_format as shmBufferGetFormat {`ShmBuffer'} -> `Word' fromIntegral#}

-- | int32_t
-- wl_shm_buffer_get_width(struct wl_shm_buffer *buffer);
{#fun unsafe shm_buffer_get_width as shmBufferGetWidth {`ShmBuffer'} -> `Int' #}

-- | int32_t
-- wl_shm_buffer_get_height(struct wl_shm_buffer *buffer);
{#fun unsafe shm_buffer_get_height as shmBufferGetHeight {`ShmBuffer'} -> `Int' #}

-- | int
-- wl_display_init_shm(struct wl_display *display);
{#fun unsafe display_init_shm as displayInitShm {`DisplayServer'} -> `Result' errToResult #}

-- | uint32_t *
-- wl_display_add_shm_format(struct wl_display *display, uint32_t format);
{#fun unsafe display_add_shm_format as displayAddShmFormat {`DisplayServer', fromIntegral `Word'} -> `()' #}

-- | struct wl_shm_buffer *
-- wl_shm_buffer_create(struct wl_client *client,
-- 		     uint32_t id, int32_t width, int32_t height,
-- 		     int32_t stride, uint32_t format);
{#fun unsafe shm_buffer_create as shmBufferCreate {`Client', fromIntegral `Word', fromIntegral `Word', `Int', `Int', fromIntegral `Word'} -> `Maybe ShmBuffer' receiveMaybeShmBuffer#}

-- void wl_log_set_handler_server(wl_log_func_t handler);

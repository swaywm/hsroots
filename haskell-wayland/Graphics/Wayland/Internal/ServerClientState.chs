module Graphics.Wayland.Internal.ServerClientState where

#include <wayland-server.h>

{#enum define ClientStateNums {
  WL_EVENT_READABLE as ClientReadable,
  WL_EVENT_WRITABLE as ClientWritable,
  WL_EVENT_HANGUP as ClientHangup,
  WL_EVENT_ERROR as ClientError} #}
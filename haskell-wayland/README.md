## Notice (2015-12-06) ##
I am not happy with these bindings and have started writing a [new](https://github.com/tulcod/sudbury) Haskell wayland interface. It is far from usable. If you want to start a project based on these bindings: I would advise you to wait for that new project to be at a further stage instead.

# Haskell Wayland bindings #
Uh... these are what you'd expect.

> NOTE: obviously this thing is incomplete, and needs more documentation and stuff. I'm very happy to work on that, but please let me know what **you** think requires attention.

Refer to `NOTES.md` for more notes on wayland terminology, how it works, and ways to shoot yourself in the foot.


## Quick example ##

> This paragraph is literate haskell.

    In this example (available as the `wayland-list-globals` executable), we are going to list the objects that the server allows us to construct directly (ie. without a relevant parent object).

    Let's start with some imports. We'll need to poll for a connection, so import Fd waiting tools.

    > import Control.Concurrent (threadWaitRead)

    We are going to write a Client-side program, so:

    > import Graphics.Wayland.Client

    > main = do

    Let's connect to the display server (e.g.) weston. displayConnect takes no arguments and tries to connect to a server based on environment variables.

    >   connect <- displayConnect
    >   let display = case connect of
    >                   Just x -> x
    >                   Nothing -> error "couldn't connect to a wayland server"

    We'll need to poll for the status of the socket connection between this client and the server, so let's go ahead and store the file descriptor.

    >   fd <- displayGetFd display
    >   putStrLn $ "Using file descriptor " ++ show fd
    >   putStrLn $ "Display at " ++ show display

    The "registry" is the entry point to get access to the server's useful objects.

    >   registry <- displayGetRegistry display
    >   putStrLn $ "Registry at "++ show registry

    You can't just arbitrarily create objects via wayland: the registry has to let you know that a certain "global" has become available for construction.
	It does so by sending an "event" (ie. a message from the server to the client) that it has.
	So let's go ahead and write code that can listen to such events.
	The registry offers two events:
	- a "global" event indicating that an object has become available for construction, and
	- a "global_remove" event indicating that an object is no longer constructible.

    >   let listener = RegistryListener {
    >     registryGlobal = \reg name ifacename version -> putStrLn $ "Received global " ++ show name ++ " (" ++ ifacename ++ ") version " ++ show version,
    >     registryGlobalRemove = \ _ _ -> return ()
    >     }

    Now we need to activate this listener.
	Client-side, you can only set a specific object's listener once, so this operation might fail if you already set one previously.

    >   errorCode <- registrySetListener registry listener
    >   putStrLn $ "Setting registry listener... " ++ show errorCode

	We are now ready to start receiving the "global" events.
	Before reading, we need to let wayland know we want to read - essentially meaning we lock (in the sense of mutex) the reading mechanism.

    >   res <- displayPrepareRead display
    >   putStrLn $ "Preparing read... " ++ show res

    As long as you stick to the protocol rules (which, as far as I'm away, aren't formally laid down anywhere), wayland object construction is free from failure.
	This allows the client to not have to wait for the server to do its part - it can just pretend everything worked and steam ahead.
	But that means that the construction of the registry we did earlier might actually not have reached the server yet.
	So before we do anything else, we should flush the write buffer.

    >   flushed <- displayFlush display
    >   putStrLn $ "Flushed " ++ show flushed

    Now poll for the socket to have data available.

    >   putStrLn "polling"
    >   threadWaitRead fd
    >   putStrLn $ "Ready to read."

    We can now process the data that's waiting for us on the socket, and dispatch the event listeners.
	The latter will call e.g. our registryGlobal function with the incoming event's parameters.

    >   events <- displayReadEvents display
    >   putStrLn $ "Read display events: " ++ show events
    >   dispatched <- displayDispatchPending display
    >   putStrLn $ "Dispatched events: " ++ show dispatched

    All wayland objects we constructed are automatically destroyed when we disconnect.

    >   displayDisconnect display


## API design and symbol naming ##

The majority of the Wayland API is based on an object-oriented event framework.
The objects have a type, which wayland calls an _interface_.
A _protocol_ defines a list of such interfaces.

Haskell renames these interfaces by, if possible, removing `wl_`, and, if possible, removing `<name of the protocol>_`, and then converting to CamelCase.
For example:

- `wl_display` is called `Display` in haskell-wayland
- `wl_registry` -> `Registry`
- `xdg_shell` -> `XdgShell` (as of this writing, however, `xdg_shell` is not in the default wayland protocol - but you can access it by generating the haskell-wayland API using the corresponding protocol XML files)
- `wl_text_input` (in the `text.xml` protocol) -> `Input` (which for semantic reasons should be placed in a Haskell module whose name makes it clear that it corresponds to text input)
- ...

Wayland names the actions on these interfaces e.g. `wl_display_connect` or `wl_compositor_create_region`. haskell-wayland converts these names into camelCase, so that you would call `displayConnect` or `compositorCreateRegion`.


## Splicing a different protocol XML file ##

Wayland has a "core" API (specified by `wayland-{client,server,util,egl,cursor}.h`, bound in `Graphics.Wayland.Internal.{Client,Server,...}`), and on top of that generates two header files (`wayland-{client,server}-protocol.h`) from an XML file detailing the wayland wire protocol.
A wayland compositor might support several such protocols (e.g. as of this writing, weston supports the core `wayland.xml` protocol, as well as `desktop-shell.xml`, `fullscreen-shell.xml`, `input-method.xml`, `screenshooter.xml`, ...).

The program that generates these protocol header files is called a _scanner_, and wayland ships with `wayland-scanner`.
For haskell-wayland, you can find the equivalent in `Graphics.Wayland.Scanner`.
Its purpose is to bind to the C wayland interface and marshall all values.

To have haskell-wayland generate a haskell API to other such XML files (the `wayland.xml` is always generated), you'll want to copy what I did in `Graphics.Wayland.Internal.SpliceClient`, `Graphics.Wayland.Internal.SpliceClientInternal` and `Graphics.Wayland.Internal.SpliceClientTypes`.
The modules `Graphics.Wayland.Internal.SpliceClient` and `Graphics.Wayland.Internal.SpliceClientTypes` should be wholly exposed to the user.
(See `Graphics.Wayland.Client` and notice that `Graphics.Wayland.Internal.SpliceClientInternal` is absent.)
(Ditto for the Server-side.)

## Value marshalling ##

Wherever possible, C values are marshalled to Haskell equivalents.
For the protocol API, this is done by `Graphics.Wayland.Scanner.Marshall`, and for the fixed api manually (but that's mostly trivial).

The exceptions to this are e.g. the methods that give you access to the memory contained by a buffer (which as of writing I haven't implemented yet).


## Technical notices ##

In theory, the symbols exposed by the C scanner (`wayland-scanner`) are off-limits for us: every language is supposed to only bind to the C library functions in `libwayland-client` and `libwayland-server`. In other words, the C library functions exposed in `wayland-{client,server,util,egl,cursor}.h`, plus the protocol XML files, should suffice to bind to all of wayland. However, in one occasion we do make us of them (binding a list of `struct wl_interface`s).

In terms of safety, there are plenty of opportunities with this library to shoot yourself in the foot.
For the most part, on the client side you'll want to stick to C-style event loops with appropriate polls: an example is (somewhat) provided.
Also please don't destroy/release/destruct/... objects more than once.


## Debugging ##

Try using [wayland-tracker](https://github.com/01org/wayland-tracker) if your code won't work at all: it is a program that can dump the connection between a server and a client.


## TODO ##

- prettify binding to wl_registry.bind (ie make more type-safe, add haskell documentation, etc)
- some kind of fancy FRP library binding?
- write documentation strings from .protocol files into haddock???
- allow easy building of other .protocol files into haskell bindings
- protocol version checker function

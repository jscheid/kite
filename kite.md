% kite - Browser Inspector/Debugger for Emacs
% Julian Scheid
% 2012-07-04

_kite_ is an Emacs package for developing front-end Web applications.
It enables inspecting, debugging, and live-editing Web pages and any
associated resources, such as scripts and stylesheets.

First and foremost, _kite_ aims to be a convenient alternative to
in-browser debuggers such as the WebKit inspector frontend.  However,
the long-term vision for _kite_ is for it to be a live-editing
environment in the spirit of [Bret Victor's "Inventing on
Principle"](https://vimeo.com/36579366).

# Kite sessions

## Kite buffers

Kite provides a number of special buffers (major modes), each of which
allows for interaction with one aspect of a debugging session.

Accessing a Kite buffer when there is no debugging session active yet
will prompt you to connect to one.  See *Connecting to a WebKit page*.

You can access Kite buffers using global key bindings or with an M-x
incantation.  The following buffers are available:

Key Binding   Incantation        Description
------------  -----------------  ---------------------------------------
C-c k c       kite-console       View console messages
C-c k d       kite-debugger      Manage breakpoints
C-c k h       kite-heap          Analyze heap usage
C-c k k       kite-repl          Execute JavaScript code in page context
C-c k m       kite-dom           Inspect and manipulate the DOM
C-c k n       kite-network       Analyze HTTP requests and responses
C-c k r       kite-resources     View external resources, such as images
C-c k t       kite-timeline      View page-global events
C-c k p j     kite-profile-js    Profile JavaScript performance
C-c k p c     kite-profile-css   Profile CSS selector performance
C-c k p h     kite-profile-heap  Profile Heap usage

There are also secondary buffer types for inspection of JavaScript
objects, stack frames, messages, per-DOM element CSS properties, and
more.  How such buffers are accessed depends on context, but is
usually achieved by moving the point onto the textual representation
of an entity and hitting RET.

## Establishing a debugging session

When you access a Kite buffer and there is no debugging session active
yet, an attempt will be made to establish a connection to a WebKit
remote debugger interface on localhost port 9222.

If the connection succeeds you will be presented with a prompt that
allows you to choose which of the pages currently open in the WebKit
instance you want to debug.  You can enter either the page URL or its
title, and there are completions available for both.

In case that the same page is open multiple times in the WebKit
instance, the individual instances of the page are disambiguated using
a numeric suffix derived from an internal WebKit identifier.
Unfortunately, there is currently no easy way to know which suffix
corresponds to which WebKit tab.  However, pages opened later in time
usually have higher suffix numbers.

## Remote sessions and multiple debugging sessions

In some situations you might want to debug more than one page
simultaneously.  Use a prefix argument of (4) (the default when you
use the C-u modifier key) for any of the Kite buffer commands and Kite
will prompt you for a new session rather than using the existing one.

You might also want to connect to a remote WebKit instance, i.e. one
not running on localhost or not running on the default port 9222.  By
giving a prefix argument of (16) (by using the C-u modifier key twice)
you'll force Kite to ask you for the host and port to connect to.

## Navigating Kite buffers with multiple sessions

If you have multiple debugging sessions active, Kite will normally
attempt to keep you within the current session.  For example, if you
have two session active and you are currently visiting the *kite
console* buffer for the first session, then doing `M-x kite-repl' will
take you to the REPL buffer for the first session.

If you want to switch to a Kite buffer for a different session, there
are three ways of doing so.

Firstly, you can simply use the mechanics afforded by M-x
switch-to-buffer et.al.: when multiple sessions are active, Kite
buffer names carry a suffix derived from the (possibly disambiguated)
page title.

Secondly, by using the Kite command to switch to a buffer while
visiting a buffer of the same type, Kite will cycle through the
buffers for all open debugging sessions.  For example, if you are
currently visiting the *kite console* buffer for the first session,
then doing `M-x kite-console' will take you to the *kite console*
buffer for the second session.

Finally, you can use a numeric prefix with the buffer access commands
and Kite will take you straight to the buffer for the corresponding
session, where the first session you opened is numbered 1, the second
session 2, and so on.  For example, `M-2 C-c k c' will take you to the
console buffer for the second session.  Note that numeric session
designators will change as you close debugging sessions.  Use the
`Kite Session List' if you ever lose track.

## Managing Kite sessions

Use `M-x kite-sessions' or `C-c k s' to access the Kite session list
buffer.  Here you can get an overview of which debugging sessions are
active and which numeric designator is assigned to them.  You can also
use the following key bindings to manage sessions:

g - refresh the session list
q - bury the session list
z - kill the session list only, leaving all session intact
DEL - kill the session under point and all its buffers
c - visit the console buffer for the session under point
d - visit the session's debugger buffer
r - visit the session's resources buffer
n - visit the session's network buffer
h - visit the session's heap analysis buffer
t - visit the session's timeline buffer
p j - visit the session's javascript profiler buffer
p c - visit the session's CSS profiler buffer
p h - visit the session's heap profiler buffer

As with the normal kite buffer access commands, each of these key
bindings can be prefixed in order to create new sessions and to
connect to remote WebKit instances.

## Motivation

The advantages of Kite over using WebKit's default browser-based
inspector front-end are threefold, from the perspective of an Emacs
user:

1. Access to Emacs facilities

Apart from the obvious benefits (such as the ability to step through
JavaScript code right in your buffer), here is a small selection of
other things that are hard to do in the browser-based front-end, yet
trivial with Kite:

* Incremental search (or search for regular expressions) in console
  messages, the DOM, profiler output, etc.

* Copy-and-paste using only the keyboard

* Dumping two JavaScript objects and taking a diff between them

2. Scripting

While the WebKit inspector is open source, it is currently not easy to
extend with custom functionality.  Kite aims to be easy to extend.
Say, if you wanted to add a specialized inspector that renders
Backbone.js collection objects in a table view, this wouldn't take
more than a few dozen lines with Kite.

3. Live Coding

Although not yet implemented, Kite aims to provide you with tools that
enable an experience similar to that provided by swank-js.

## Supported Browsers

_kite_ currently only works with Web pages running in a browser supporting the
[WebKit remote debugging protocol](http://www.webkit.org/blog/1620/webkit-remote-debugging/).

This includes recent versions of Google Chrome and Mobile Safari, as
well as WebKitGTK+ when embedded in Emacs via
[XWidget](http://emacswiki.org/emacs/EmacsXWidgets).

### Remote Inspection

_kite_ can connect to supported browsers via sockets.  This means it
can connect to both browsers running on your local machine (the same
machine Emacs is running on) and to remote browsers, running on other
hosts or mobile devices.

There is currently some work involved in starting browsers in a way
such that they can be connected to.  See below for instructions on how
to prepare each browser type for remote inspection.

#### Chromium

    Chromium --remote-debugging-port=9222

#### Mobile Safari

See http://atnan.com/blog/2011/11/17/enabling-remote-debugging-via-private-apis-in-mobile-safari/

### Embedded Inspection

_kite_ also supports connecting to a WebKit browser embedded in Emacs.
However, embedding WebKit is not supported by stock Emacs.  At the
time of this writing, you have to jump through some hoops to make this
work:

* Download, build and install a recent version of WebKitGTK+.  At the
  time of this writing, the required features are not part of any
  release so you will have to fetch trunk via version control.

# Views

## Session View

## Console View

The console view shows any messages which scripts on the page have
emitted using the
[console](https://developer.mozilla.org/en/DOM/console) object.

The console view can be opened using `M-x kite-console' or by typing
`C-c C' in another kite view.

### Special items in messages

Some parts of console messages have special key bindings:

Object representations are rendered with `kite-object-face' and show
an abbreviated representation of a JavaScript object.  Typing RET
(`kite-inspect') with point over the object representation brings up
an Object Inspector view for that object.

DOM node representation are rendered with `kite-node-face' and show an
abbreviated representation of a DOM node.  Typing RET (`kite-inspect')
with point over the DOM node representation brings up the
corresponding DOM view and moves point to the beginning of the node in
question.

### Message Details

In the console view, type `i' to bring up an detail view for the
message under point (`kite-console-details').  The detail view show
all available information on the log message, some of which is elided
from the console view for brevity's sake.

### Message Source

In the console view, type `g' to go to the source location at which
the message was emitted.  See *going to source locations*.

### Tail Follow

If point is at the end of the console view buffer when a new message
arrives, it will be moved to the end again.  In other words, if point
is at the end of the buffer it will stay at the end of the buffer.
This allows you to always see the most recent messages, but at the
same time leaves point alone when you examine other parts of the
buffer.

### Page Reloads

When the browser page is reloaded (either via `kite-reload-page' or by
any other means, such as using the browser UI or by JavaScript code),
`kite-console-page-reloaded-function' is executed with the console
buffer current and point at the end of the buffer.  By default, this
function only inserts a page break followed by a line feed.  In other
words, messages from previous incarnations of the page are not cleared
by default.

This default behaviour is useful because it gives convenient access to
past messages.  You can bind `kite-console-page-reloaded-function' to
`erase-buffer' if you prefer the behaviour of the default Webkit
Inspector Frontend, which clears past messages on page reload.

### Narrowing to Groups

### Collapsing Groups

### Filtering Levels

### Clearing Messages

## Network View
## DOM View
## Memory View

# Global Key Bindings

The following key bindings can be used in any Kite buffer, or in any
buffer that is associated with a Kite session by one of Kite's minor
modes.

  C-c s     switch to the session view buffer

  C-c c     switch to the console view buffer

  C-c d     switch to the DOM view buffer

  C-c m     switch to the memory view buffer

  C-c n     switch to the network view buffer

  C-c p     switch to the profiler view buffer

  C-c r     switch to the resource view buffer

  C-c e     switch to the REPL view buffer

  C-!       reload the page in the browser.  With prefix arg, ignore the browser cache when reloading the page.

  C-c C-e   evaluate some JavaScript code in the page context

  C-c C-p   go to the source location of the previous error message

  C-c C-p   go to the source location of the next error message

  C-c C-u   navigate to a different URL

  C-c C-r   toggle showing paint rects

  C-c C-x   clear console messages

  C-c C-i   enable DOM inspect mode in the browser (pick a node with the mouse)

# Source views

  C-c C-s   "save" a modified resource by pushing it to the browser

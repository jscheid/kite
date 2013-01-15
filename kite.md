% _Kite_ - WebKit Remote Debugger Front-end
% Julian Scheid
% 2012-08-30

<!--

  kite.md -- User manual for Kite, a WebKit inspector front-end

  Copyright (C) 2012 Julian Scheid

  This file is formatted in pandoc-flavoured Markdown.  Use the
  accompanying Makefile to translate into Info or HTML formats.

  This file is not part of GNU Emacs.

  Kite is free software: you can redistribute it and/or modify it
  under the terms of the GNU General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  Kite is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
  or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
  License for more details.

  You should have received a copy of the GNU General Public License
  along with Kite.  If not, see <http://www.gnu.org/licenses/>.

-->

_Kite_ is an Emacs front-end for the WebKit debugger.  It enables
inspecting, debugging, and live-editing Web pages and associated
resources, such as scripts and stylesheets.

# License

Kite is free software: you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation, either version 3 of the License, or (at your
option) any later version.

Kite is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Kite.  If not, see <http://www.gnu.org/licenses/>.

# Installation

## Compatibility

Kite is known to be compatible with Emacs version 24.1.  There are
currently no plans for compatibility with older releases.

## Automatic Installation

_Kite_ comes as a package conforming to Tromey's _package.el_, and
is available via the [MELPA](http://melpa.milkbox.net/) package
repository.

Ensure that MELPA is listed in `package-archives` and
execute `M-x package-install RET kite RET`.

## Manual Installation

Ensure that all kite source files as well as
[json.el](http://edward.oconnor.cx/2006/03/json.el) and a recent
version of [websocket.el](https://github.com/ahyatt/emacs-websocket)
are on `load-path` and add `(require 'kite)` to your `.emacs` file.

# Starting the Host Browser

Kite currently only supports the WebKit Remote Debugging API and thus
won't work with browsers not based on WebKit, such as Firefox or
Opera.  Support for other remote debugging interfaces may be added in
the future.

Many WebKit-based browsers do not yet expose the remote debugging
interface and thus can't be used with Kite.  At the time of this
writing, the only browsers supported are recent versions of Chromium,
Google Chrome, and Mobile Safari.

We are working on adding a remote debugging interface to WebKitGTK+.
This will enable debugging of WebKit instances embedded via the Emacs
[xwidgets branch](http://emacswiki.org/emacs/EmacsXWidgets).

## Enabling Remote Debugging on Chromium and Google Chrome

You can enable remote debugging for Chromium and Google Chrome by
passing the `--remote-debugging-port=<port>` command line option.
You may have to use the Google Chrome beta or development channel for
this to work.

The recommended default port is 9222.  Thus, to start Google Chrome
with the default port on Mac OS X, you would execute the following
command:

~~~~

    open /Applications/Google\ Chrome.app --args --remote-debugging-port=9222

~~~~

For more information, see here:

<https://developers.google.com/chrome-developer-tools/docs/remote-debugging#remote>

## Enabling Remote Debugging on Mobile Safari

Remote debugging a Mobile Safari instance has not yet been tested by
us.  If you want to give it a try, follow the instructions here:

<http://atnan.com/blog/2011/11/17/enabling-remote-debugging-via-private-apis-in-mobile-safari/>

# Kite Sessions

Kite's most fundamental concept is that of a _debugging session_,
which corresponds to an open connection to the remote debugging API
for one specific open browser tab.  Kite supports multiple
simultaneous debugging sessions, but usually you will only have a
single session active.

A single WebKit tab can currently only be connected to one debugger,
so if you are using the default WebKit debugging front-end you won't
be able to use Kite, and vice versa.

## Kite Buffers Overview

Kite provides a number of special buffers (major modes), each of which
allows for interaction with one aspect of a debugging session.

Accessing a Kite buffer when there is no debugging session active yet
will prompt you to establish a new one.  See instructions on
[establishing a debugging session](#establishing-a-debugging-session).

You can access Kite buffers using global key bindings or with an `M-x`
incantation.  The following buffers are available:

Key Binding     Incantation          Description
--------------  -------------------  ---------------------------------------
`C-c C-k c`     `kite-console`       View console messages
`C-c C-k d`     `kite-debugger`      View and manage breakpoints
`C-c C-k h`     `kite-heap`          Analyze heap usage
`C-c C-k s`     `kite-scratch`       Evaluate JavaScript code in page context
`C-c C-k m`     `kite-dom`           Inspect and manipulate the DOM
`C-c C-k n`     `kite-network`       Analyze HTTP requests and responses

<!-- NOT YET IMPLEMENTED
`C-c C-k r`     `kite-resources`     Access resources, such as images
`C-c C-k t`     `kite-timeline`      View page-global events
`C-c C-k p j`   `kite-profile-js`    Profile JavaScript performance
`C-c C-k p c`   `kite-profile-css`   Profile CSS selector performance
`C-c C-k p h`   `kite-profile-heap`  Profile Heap usage
-->

There are also secondary buffer types for inspection of JavaScript
objects, stack frames, messages, per-DOM element CSS properties, and
more.  How such buffers are accessed depends on context, but is
usually achieved by moving the point onto the textual representation
of an entity and hitting `RET`.

## Establishing a Debugging Session

When you access a Kite buffer and there is no debugging session active
yet, an attempt will be made to establish a connection to a WebKit
remote debugger interface on `kite-default-remote-host` (default:
`127.0.0.1`) and `kite-default-remote-port` (default: 9222).

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

## Remote Sessions and Multiple Debugging Sessions

In some situations you might want to debug more than one page
simultaneously.  Use a prefix argument of `(4)` (the default when you
use the `C-u` modifier key) for any of the Kite buffer commands and
Kite will prompt you for a new session rather than using the existing
one.

You might also want to connect to a remote WebKit instance, i.e. one
not running on `kite-default-remote-host` or `kite-default-remote-port`.
By giving a prefix argument of `(16)` (by using the `C-u` modifier key
twice) you'll force Kite to ask you for the host and port to connect
to.

## Navigating Kite Buffers with Multiple Sessions

If you have multiple debugging sessions active, Kite will normally
attempt to keep you within the current session.  For example, if you
have two session active and you are currently visiting the *kite
console* buffer for the first session, then doing `M-x kite-scratch`
will take you to the scratch buffer for the first session.

If you want to switch to a Kite buffer for a different session, there
are three ways of doing so.

Firstly, you can simply use the mechanics afforded by `M-x
switch-to-buffer` et.al.: when multiple sessions are active, Kite
buffer names carry a suffix derived from the (possibly disambiguated)
page title.

Secondly, by using the Kite command to switch to a buffer while
visiting a buffer of the same type, Kite will cycle through the
buffers for all open debugging sessions.  For example, if you are
currently visiting the `*kite console*` buffer for the first session,
then doing `M-x kite-console` will take you to the `*kite console*`
buffer for the second session.

Finally, you can use a numeric prefix with the buffer access commands
and Kite will take you straight to the buffer for the corresponding
session, where the first session you opened is numbered 1, the second
session 2, and so on.  For example, `M-2 C-c C-k c` will take you to
the console buffer for the second session.  Note that numeric session
designators will change as you close debugging sessions.  Use the
`Kite Session List` if you ever lose track.

<!-- NOT YET IMPLEMENTED

## Managing Kite Sessions

Use `M-x kite-sessions` or `C-c C-k s` to access the Kite session list
buffer.  Here you can get an overview of which debugging sessions are
active and which numeric designator is assigned to them.  You can also
use the following key bindings to manage sessions:

Key    Action
-----  ---------------------------------------------------------------
`g`    Refresh the session list
`q`    Bury the session list
`z`    Kill the session list only, leaving all session intact
`DEL`  Kill the session under point and all its buffers
`c`    Visit the console buffer for the session under point
`d`    Visit the session's debugger buffer
`r`    Visit the session's resources buffer
`n`    Visit the session's network buffer
`h`    Visit the session's heap analysis buffer
`t`    Visit the session's timeline buffer
`p j`  Visit the session's javascript profiler buffer
`p c`  Visit the session's CSS profiler buffer
`p h`  Visit the session's heap profiler buffer

As with the normal kite buffer access commands, each of these key
bindings can be prefixed in order to create new sessions and to
connect to remote WebKit instances.

-->

# Kite Buffers

## Console Buffer

The console buffer shows any messages which scripts on the page have
emitted using the
[console](https://developer.mozilla.org/en/DOM/console) object.

The console buffer can be opened using `M-x kite-console` or by typing
`C-c C-k c` with default key bindings.

### Evaluating JavaScript code

You can type JavaScript code at the prompt and hit `RET` or `C-j` to
send it to the remote debugger.  The result of the evaluation or the
stack trace (in case of error) will be displayed.  The prompt supports
basic identifier completion via the `TAB` key.

### Special Items in Messages

Some parts of console messages have special key bindings:

Object representations are rendered with `kite-object-face` and show
an abbreviated representation of a JavaScript object.  Typing `RET`
(`M-x kite-inspect`) with point over the object representation brings
up an Object Inspector view for that object.

DOM node representation are rendered with `kite-node-face` and show an
abbreviated representation of a DOM node.  Typing `RET` (`M-x
kite-inspect`) with point over the DOM node representation brings up
the corresponding DOM view and moves point to the beginning of the
node in question.

### Message Details

In the console view, type `C-c i` to bring up an detail view for the
message under point (`kite-show-log-entry`).  The detail view show all
available information on the log message, some of which is elided from
the console view for brevity's sake.

### Message Source

In the console view, type `C-c g` to go to the source location at
which the message at point was emitted.  If point is on a stack trace,
go to the source location for the stack frame at point.

### Tail Follow

If point is at the end of the console view buffer when a new message
arrives, it will be moved to the end again.  In other words, if point
is at the end of the buffer it will stay at the end of the buffer.
This allows you to always see the most recent messages, but at the
same time leaves point alone when you examine other parts of the
buffer.

### Page Reloads

When the browser page is reloaded (either via `kite-reload-page` or by
any other means, such as using the browser UI or by JavaScript code),
`kite-console-page-reloaded-function` is executed with the console
buffer current and point at the end of the buffer.  By default, this
function only inserts a page break followed by a line feed.  In other
words, messages from previous incarnations of the page are not cleared
by default.

This default behaviour is useful because it gives convenient access to
past messages.  You can bind `kite-console-page-reloaded-function` to
`erase-buffer` if you prefer the behaviour of the default Webkit
Inspector Frontend, which clears past messages on page reload.

<!-- NOT YET IMPLEMENTED
### Narrowing to Groups

### Collapsing Groups

### Filtering Levels

### Clearing Messages
-->

## Debug Buffer

The debug buffer shows any breakpoints currently set and allows to
clear breakpoints.  It also shows whether JavaScript execution is
currently paused.

Sorry, no further documentation available yet.  Use `M-x
describe-mode` in a _Kite Debug_ buffer to learn about available key
bindings.

**TODO**:

* Overall, the JavaScript debugger is not very usable yet.
* When a breakpoint is hit, Emacs shows the source file and jumps to
  the break location; however, it doesn't show the stack trace or any
  variable bindings.
* There should be a way to tell Emacs which local file corresponds to
  a network resource, so that Emacs can open the local file for
  editing.  [Source
  map](http://www.html5rocks.com/en/tutorials/developertools/sourcemaps/)
  support should also be implemented.
* A number of basic debugger commands, such as "execute until here",
  are not yet implemented.

## Scratch Buffer

Sorry, no documentation available yet.  Use `M-x describe-mode` in a
_Kite Scratch_ buffer to learn about available key bindings.

**TODO**:

* This mode is currently very experimental.

## Network Buffer

Sorry, no documentation available yet.  Use `M-x describe-mode` in a
_Kite Network_ buffer to learn about available key bindings.

**TODO**:

* The Network inspector doesn't yet sorting the table by criteria
  other than resource load order.

* The Network inspector doesn't provide access to request and response
  headers, cookies, etc.

* The code for the Network inspector is pretty convoluted; it should
  be rewritten using CL structs so that the intermediate
  representation is less obtuse and can be more easily reused for
  alternate display representations.

## DOM Buffer

Sorry, no documentation available yet.  Use `M-x describe-mode` in a
_Kite DOM_ buffer to learn about available key bindings.

**TODO**:

* The DOM inspector doesn't support DOM mutation yet: although local
  modification is partially implemented, changes can not yet be sent
  to the remote debugger.

* Likewise, CSS mutation isn't only partially implemented.

## Heap Buffer

Sorry, no documentation available yet.  Use `M-x describe-mode` in a
_Kite Heap_ buffer to learn about available key bindings.

# Global Key Bindings

In addition to the key bindings listed in section [Kite Buffers
Overview](#kite-buffers-overview), the following keys are bound by
default:

Key Binding     Incantation                Description
--------------  -------------------------  ---------------------------------------
`C-c C-k !`     `kite-reload-page`         Reload the page; ignore cache with prefix

<!-- NOT YET IMPLEMENTED
`C-c C-k C-r`   `kite-resume`              Resume execution
`C-c C-k C-e`   `kite-eval-expr`           Evaluate JavaScript code in page context
`C-c C-k C-p`   `kite-previous-error`      Go to source for previous error
`C-c C-k C-n`   `kite-next-error`          Go to source for next error
`C-c C-k C-u`   `kite-goto-url`            Navigate to URL
`C-c C-k C-x`   `kite-clear-console`       Clear console messages
`C-c C-k C-r`   `kite-toggle-paint-rects`  Toggle showing paint rects
`C-c C-k C-i`   `kite-pick-dom-node`       Pick a DOM node with mouse in browser
`C-c C-k C-s`   `kite-upload-buffer`       Upload a modified resource to the remote debugger
-->

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

# Introduction

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

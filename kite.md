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

## Console View

The console view shows the messages emitted by scripts on the page
using the [console](https://developer.mozilla.org/en/DOM/console)
object.

The console view can be opened using `kite-console' or by typing `C'
in most other kite views.

### Message Details

In the console view, you can hit RET to bring up the message detail
view for the message under point (`kite-console-details').  Some of
the text in the message detail view may be hyperlinked to other views.

### Tail Follow

### Narrowing to Groups

### Collapsing Groups

### Filtering Levels

### Clearing Messages

## Network View
## DOM View
## Memory View
## Connection View


#### Test1

##### Test2

## Header Level 2

Blah blah

### Header Level 3

Blah

# Header Level 1 - B

Blah blah blah

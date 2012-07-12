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




#### Test1

##### Test2

## Header Level 2

Blah blah

### Header Level 3

Blah

# Header Level 1 - B

Blah blah blah

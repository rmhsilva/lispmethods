# A Common Lisp Development Environment

Installing Common Lisp is tricky, and probably one of the reasons people are
turned off by the language in general. Which is a shame really. It's like being
put off desert because there are too many choices on the menu (the correct
choice is always tiramisou).

Read on to find out how to:
- Install a Common Lisp implementation
- Add some useful libraries
- Install a modern lisp development environment

The entire process should take less than 5 minutes (I'm ignoring download time -
convenient right?).


## Installing Lisp

Let's answer the immediate practical question:

> How do I even install Lisp!?

And for most people, the answer is this:

> Google "Steel Bank Common Lisp", and install it. Now.

Or just click this link to the download page: [XXX]().

There you go, you've installed Common Lisp (the SBCL implementation, to be
precise - see §XXX). Fire it up:

```
$ sbcl
This is SBCL 1.3.12, an implementation of ANSI Common Lisp.
More information about SBCL is available at <http://www.sbcl.org/>.

SBCL is free software, provided as is, with absolutely no warranty.
It is mostly in the public domain; some portions are provided under
BSD-style licenses.  See the CREDITS and COPYING files in the
distribution for more information.
*
```

You're now sitting in a Read-Eval-Print-Loop (REPL), a fully functional CL
environment, **congratulations**. You can do everything you'd expect, and a lot
more you wouldn't, like intalling libraries (see below).

Note: the default SBCL prompt is not too useful (it's the single "*"
you see above). We'll make it better later on.

Next up - getting libraries.


## Libraries

Currently the best way to download and manage Common Lisp libraries is
Quicklisp. Think of it like Python's `pip`, but with more parentheses.

Download Quicklisp and follow the install instructions here XXX. Make sure you
do the bit that adds quicklisp to SBCL's startup script (`ql:add-to-init-file`)
as this makes development way easier...

For more details about how libraries work, see XXX. Assuming we're in SBCL and
Quicklisp has been installed, lets just install a few useful ones now.

- Regular expressions `(ql:quickload "cl-ppcre")`
- Clojure's arrow syntax `(ql:quickload "XXX")`
- CL21 for modern things


## IDE

Download and install the following:
- Emacs (version >25)
- Spacemacs

And then enable the 'common-lisp' layer in Spacemacs.

The most productive IDE I know about for writing Lisp is Emacs. Nothing else
I've used comes close. Got something better? Let me know! There are loads of
other articles around describing how to setup Emacs for Lisp development, so go
away and read:
- XXX
- XXX


## Done!

You now have a Common Lisp implementation installed, a few useful libraries, and
an IDE to develop in. YAY.

---


## Extra Stuff

### SBCL customisation

- rlwrap
- quicklisp

### Notes on CL Implementations

"Lisp" is a large set of languages, that, as far as I can tell, includes
anything that is written using s-expressions. Opinions vary. Anyway, the
important thing is that Common Lisp is a particualr variant, which was
standardised by ANSI in XXX. The standard describes the functionality that must
be present in a Common Lisp implementation, many (many) of which exist.

There are open source (e.g., SBCL, Clasp) and closed source implementations
(Allegro, Lispworks), and they all must implement the features specified by
ANSI, so you are guaranteed some level of portability between implementations.
However, all the extra stuff bundled with implementations varies, and so you
cannot expect any non-standard features to be portable. Notably:
- Threading??
- XXX

SBCL is an open source, popular implementation with a focus on speed. It
integrates well with Emacs via SLIME, which is the main reason for preferring
it, frankly.

Other interesting implementations:
- ECL, as it compiles down to C
- Clasp, for its C/C++ interop

Other popular lisps:
- Clojure (and Clojurescript)
- Scheme
- Guile


# Libraries in Common Lisp

> How do I find libraries other people have written?
> How do I use someone else's library?
> How do I make and share one myself?

Most languages deal with this stuff quite differently. Python has pip, which is
fairly well established as the Python package manager. If you want to share some
code, you put it on PyPi, and people can find it with `pip search`. Node.js
programmers use NPM (or yarn, if they're cool), and the NPM package registry. C
and C++ programmers must rely on their system's package manager to provide
development libraries, or just download them manually.

Common lisp has traditionally been more like C/C++ in this respect - if you
wanted to use someone's libary you downloaded it and put it somewhere your lisp
implementation could find it.

These days we have [Quicklisp](http://quicklisp.org)


## Essential reading

- Practical Common Lisp's chapter on packages:


## ASDF

ASDF (Another System Definition Facility) is included in most popular CL
implementations, including SBCL.

---
layout: post
title: Libraries
---

Here be answers to the inevitable questions asked, by people new to Common Lisp,
about how to find, use and create 'libraries'.

* toc
{:toc}

{::comment}

Interesting:
<https://www.reddit.com/r/lisp/comments/47kzfo/help_with_asdf_and_quicklisp/>

Essentially, people have no idea how to use libraries, and systems and all that
**blah** in lisp.

<https://news.ycombinator.com/item?id=3809062> - serial port comms is severly
lacking. cl-serial exists.. but is a bit barebones and meh.

See http://tychoish.com/post/using-asdf-install-with-sbcl/ as well.

{:/comment}


Read on for the info, or go straight to the [examples](#examples).


## Definitions

For the purposes of this article, a **library** is a collection of code for
doing something specific, designed to be used as part of a larger library or
application.

Common Lisp:
- **Packages** are containers for Common Lisp symbols, and are defined by the
  standard. Similar to C++ namespaces. Key point - one does not 'install' a
  package; that'd be silly, it's just a group of names. Instead, the code that
  defines the package is loaded, and then the symbols in the package can be
  accessed.
- **Systems** are *not* defined in the CL standard, but essentially they are
  groups of code (usually including one or more package definitions), and any
  information required to build and run the code (dependencies, etc). They
  typically also include 'extra' information about the code (author, license,
  system name, etc), 

Need more? Go and read <http://weitz.de/packages.html>, a fantastic and detailed
description of how all this stuff works. Also useful is
the [PCL chapter on packages][pcl-packages].

[pcl-packages]: http://gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html


## Tools

You'll definitely come across these at some point.

- [ASDF][asdf] is a kind of build tool on steroids. You can use it to do lots of
  things, but it's commonly used to define how to build and load Common Lisp
  libraries or applications.
- [Quicklisp][quicklisp] is a library manager for Common Lisp that lets you
  easily install libraries.
  
[asdf]: https://common-lisp.net/project/asdf/
[quicklisp]: https://www.quicklisp.org/



## Quicklisp

> Experimenting with and using open-source libraries is so easy in <insert
> favourite language here>. I want that in Common Lisp!

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

Process (draft)
- create the system definition file (.asd extension)
- add the containing folder to asdf's load path. The easiest way is to do `(push
  *default-pathname-defaults* asdf:*central-registry*)`
- then you can do `(asdf:oos 'asdf:load-op 'package-name)`


The organisation style I prefer is to use one single `package.lisp` file which
contains the package defintions. Then each file simply calls `in-package`. The
other option is to use a single package definition per file. This means packages
never span multiple files, and dependencies are more clearly indicated. However
it can feel a little cumbersome. There's something nice about specifying a
package somewhere else. Feels like a C header file.

The Slime interactivity with ASDF isn't amazing. The current path has to be
pushed onto the central repo, and load op has to be called manually. There
should kinda be a slime-load-project type command...

## Examples

### Packages

Creating, specifying dependencies, importing names

### Quicklisp

### System definitions

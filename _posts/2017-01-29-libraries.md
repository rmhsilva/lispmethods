---
layout: post
title: Source Code Organisation
updated: 2017-04-19
---

Answers to some commonly asked questions about how to organise Common Lisp code,
and use or create libraries.

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

"Libraries" are a tricky concept in Common Lisp, probably as the language has
been around for ages and the preferred way of doing things has evolved. This
article aims to give you some insight into how things are done these days, and
hopefully clear up some confusion.

We'll start with an overview of some fundamental concepts and definitions, then
look at source code organisation styles, and finally talk about using open
source libraries. Read on for the info, or go straight to
the [examples](#examples).


## Definitions

{::comment}
TODO:
This really needs to be more detailed. Link to other articles about packages, in
particular, what `CL-USER` is, etc.
{:/comment}

For the purposes of this article, a **library** is a collection of code for
doing something specific, designed to be used as part of a larger library or
application. This is a pretty vague definition. Common Lisp has a few ways of
grouping code together to create libraries and applications. Here is a brief
overview of the main concepts you'll need.

For more detailed info, go and read <http://weitz.de/packages.html>, a fantastic
and detailed description of how all this stuff works. Also useful is
the [PCL chapter on packages][pcl-packages].


### Packages

"Packages" are defined by the Common Lisp standard as **containers for symbols**
(see [Symbols](/symbols.html) for more info about what these magical things
are), similar to C++ namespaces. Key point: one does not 'install' a package;
that'd be silly, it's just a group of (effectively) names. Instead, the code
that *defines* the package is loaded ([more details](#loading-code)), and then
the symbols in the package can be accessed. We'll see how to do that later.

When you start your Lisp REPL, you probably end up with a prompt that contains
something like `CL-USER`. This indicates that you are in the `CL-USER`
*package*, which is the default package intended for general experimentation,
and has all the standard Common Lisp symbols available. See [PCL][pcl-packages]
for more details.

Packages are not essential for writing Lisp (you could just split up files and
LOAD each one individually), but packages offer far better code organisation.


### Loading Code

Developing and running Common Lisp code is different from most languages in use
today. We develop applications by building upon a Lisp "image", and we use
source code files as a way to record the building steps. Sort of.

Anyway, the thing to remember is that when you start a new Lisp instance (for
example, by running `sbcl` at the command line), it comes up blank, with no user
code. In order to run things, you must *load* code into it (or define code
directly from the REPL, of course). This is typically done by either using the
Common Lisp [LOAD][clhs-load] function, or using ASDF to load a system (see
below).

[clhs-load]: http://clhs.lisp.se/Body/f_load.htm


### Systems

"Systems" are *not* defined in the CL standard, but they have been around as a
concept for decades. Essentially they are groups of code (usually including one
or more package definitions), and any information required to build and run the
code (dependencies, unit tests, etc). They typically also include 'extra'
information about the code (author, license, system name, etc).

[pcl-packages]: http://gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html


## Using Packages

Remember that Common Lisp packages are similar to namespaces, or python modules.
Knowing this, you will almost always want to define a package to hold code you
write.

When your code is spread across multiple files, the preferable
organisational style is to use one single `package.lisp` file which contains the
package definitions. Then each source file simply calls `in-package` to declare
which package its code lives in. The other option is to use a single package
definition per file. This means packages never span multiple files, and
dependencies are more clearly indicated. However it can feel a little
cumbersome. There's something nice about specifying a package somewhere else.
And it functions as a useful "API" / dependency tree for all the files.


## ASDF

[ASDF][asdf] (Another System Definition Facility) is a kind of build tool on
steroids. You can use it to do lots of things, but it's commonly used to define
how to build and load Common Lisp libraries or applications.

ASDF is included in most popular CL implementations, including SBCL. It is the
de-facto system definition tool, and most open source projects will come with an
ASDF system definition. This definition, contained in a `.asd` file, probably
includes a number of things, such as:

- The project author, description, license, and other similar data
- A list of files in the system
- Any dependencies between files
- A description of how to run unit tests

[asdf]: https://common-lisp.net/project/asdf/

### Developing with ASDF

Process:

- create the system definition file (.asd extension)
- add the containing folder to asdf's load path. The easiest way is to do `(push
  *default-pathname-defaults* asdf:*central-registry*)`
- then you can do `(asdf:oos 'asdf:load-op 'system-name)` to "load" the system

The Slime interactivity with ASDF isn't amazing. The current path has to be
pushed onto the central repo, and load op has to be called manually. There
should be a `slime-load-project` type command.


### Quicklisp

> Experimenting with and using open-source libraries is so easy in
> favourite-language-here. I want that in Common Lisp!

Most languages deal with this stuff quite differently. Python has pip, which is
fairly well established as the Python package manager. If you want to share some
code, you put it on PyPI, and people can find it with `pip search`. Node.js
programmers use NPM (or yarn, if they're cool), and the NPM package registry. C
and C++ programmers must rely on their system's package manager to provide
development libraries, or just download them manually.

Common lisp has traditionally been more like C/C++ in this respect - if you
wanted to use someone's libary you downloaded it and put it somewhere your lisp
implementation could find it.

These days we have [Quicklisp](http://quicklisp.org).


## Examples

How to create and use Packages, define Systems, and install stuff with
Quicklisp!

*In progress...*

We will build a very small web application based on [ningle][ningle-web] to
demonstrate how everything hangs together in Common Lisp. This web app will do
just one thing: give us a (probably not cryptographically secure) random number.

We shall call our the application... `bobbio`. You'll need to create a few
files:

* `bobbio.asd` (the bobbio system definition)
* `bobbio.lisp` (the main lisp source file)
* `web.lisp` (the web application code)

[ningle-web]: https://github.com/fukamachi/ningle


### Packages

We'll split up our little web app into two packages -- one to hold the web
stuff, and one to hold the backend functionality.

Put the following into `bobbio.lisp`:

~~~ common_lisp
;;;; bobbio.lisp
(defpackage :bobbio
  (:use #:cl)
  (:export
   :get-message))

(in-package :bobbio)

(defun random-char ()
  "Return a random character"
  (code-char (+ 60 (random 26))))

(defun get-message ()
  (format nil "Your random number is: ~a" (random-char)))
~~~

This is a fairly minimal package defintion possible; it creates a package with
the `:bobbio` keyword using the (defpackage)[clhs-defpackage] macro, and then
changes the current "namespace" to the package, using
(in-package)[clhs-defpackage]. One symbol, `get-message`, is exported by the
package -- this is **the only** symbol that will be visible to other packages.

After that, the file just defines a couple of functions we'll use later on.

However, `defpackage` can do *lots* of other things as well, much of which
you'll probably need at some point. For example, you'll probably want to import
things.

Put the following into `web.lisp`:

~~~ common_lisp
;;;; web.lisp
(defpackage :bobbio.web
  (:use #:cl)
  (:export
   :start-server)
  (:import-from :bobbio :get-message))

(in-package :bobbio.web)

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      (get-message))

(defun start-server ()
  (clack:clackup *app*))
~~~

This one is slightly more complicated - 



### System definitions

Create an ASDF file!


### Quicklisp

Quicklisp uses ASDF definitions to load things!

Install and search for packages!

Symlink bobbio into ~/quicklisp/local-projects


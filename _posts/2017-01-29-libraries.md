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

"Packages" are defined by the Common Lisp standard as **containers for
symbols**, similar to C++ namespaces (see [Symbols](/symbols.html) for more info
about what these magical "symbol" things are). Key point: one does not "install"
a package; that'd be silly, it's (effectively) just a group of names. Instead,
the code that *defines* the package is loaded ([more details](#loading-code)),
and then the symbols in the package can be accessed. We'll see how to do that
later.

When you start your Lisp REPL, you probably end up with a prompt that contains
something like `CL-USER`. This indicates that your REPL input is evaluated in
the context of the `CL-USER` *package*, which is the default package intended
for general experimentation, and has all the standard Common Lisp symbols
available. See [PCL][pcl-packages] for more details.

Normally when writing an application or library, it is advisable to create a
package to contain the application symbols, rather than definiing everything in
the `CL-USER` package.


### Systems

"Systems" are *not* defined in the CL standard, but they have been around as a
concept for decades. Essentially they are groups of code (usually including one
or more package definitions), and any information required to build and run the
code (dependencies, unit tests, etc). They typically also include 'extra'
information about the code (author, license, system name, etc).

[pcl-packages]: http://gigamonkeys.com/book/programming-in-the-large-packages-and-symbols.html


### Loading Code

Developing and running Common Lisp code is different from most languages in use
today. We develop applications by building upon a Lisp "image" (blank canvas),
and we use source code files to record the building steps. Sort of.

Anyway, the thing to remember is that when you start a new Lisp instance (for
example, by running `sbcl` at the command line), it comes up blank, with no user
code. In order to run things, you must *load* definitions into it (or define
code directly from the REPL, of course). This is typically done by either using
the Common Lisp [LOAD][clhs-load] function, or using ASDF to load a system (see
below).

[clhs-load]: http://clhs.lisp.se/Body/f_load.htm


## Using Packages

Remember that Common Lisp packages are similar to namespaces, or python modules.
Knowing this, you will almost always want to define a package to hold code you
write.

When your code is spread across multiple files, the preferred organisational
style is to use one single `package.lisp` file at the top level of your project
which contains the package definitions. Then each source file simply declares
which package its symbols lives in. The other option is to use a single package
definition per file. This means packages never span multiple files, and
dependencies are more clearly indicated. However it can feel a little cumbersome
as the number of files (and hence) packages grows. Using a single file to define
all packages often feels easier to organise, and also provides a basic but
useful "API" / dependency tree for all the files.


### Package Naming

Packages must be named with a so-called "string-designator", which can be quite
confusing, as it results in several different styles of working with packages.

### Accessing Symbols in Other Packages

: or ::


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
should be a `slime-load-project` command...


### Quicklisp

> Experimenting with and using open-source libraries is so easy in
> <favourite-language-here>. I want that in Common Lisp!

Most languages deal with this stuff quite differently. Python has pip, which is
fairly well established as the Python package manager. If you want to share some
code, you put it on PyPI, and people can find it with `pip search`. Node.js
programmers use NPM (or yarn, if they're cool), and the NPM package registry. C
and C++ programmers must rely on their system's package manager to provide
development libraries, or just download them manually.

Common lisp has traditionally been more like C/C++ in this respect - if you
wanted to use someone's libary you downloaded it and put it somewhere your lisp
implementation could find it, typically on the ASDF load path somewhere.

These days we have [Quicklisp](http://quicklisp.org)!


## Examples

How to create and use Packages, define Systems, and install stuff with
Quicklisp!

*In progress...*

We will build a very small web application using [ningle][ningle-web] to
demonstrate how everything hangs together in Common Lisp. This web app will do
just one thing: generate us a (probably not cryptographically secure) random
character on every page refresh.

We shall call our the application... `bobbio`. You'll need to create a few
files:

* `bobbio.asd` (the bobbio system definition)
* `package.lisp` (the package definitions)
* `bobbio.lisp` (the main lisp source file)
* `web.lisp` (the web application code)

[ningle-web]: https://github.com/fukamachi/ningle


### Packages

We'll split up our little web app into two packages -- one to hold the web
stuff, and one to hold the backend functionality. Two packages is overkill for
something this small, but lets go with it anyway.

Put the following into `package.lisp`:

~~~ common_lisp
;;;; package.lisp
(defpackage :bobbio
  (:use #:cl)
  (:export
   :get-message))

(defpackage :bobbio.web
  (:use #:cl)
  (:export
   :start-server)
  (:import-from :bobbio :get-message))
~~~

This defines our two packages -- they are both quite minimal. The first form
defines a package named `:bobbio` using the (defpackage)[clhs-defpackage] macro.
One symbol, `get-message`, is exported by the package -- this is the only symbol
that will be "visible" to other packages. The `:bobbio` package also *uses* all
of the symbols from the `:cl` package (a nickname for `:common-lisp`), which
means all standard Common Lisp symbols (i.e. `defun`, `setf`, etc) will be
available.

The `:bobbio.web` package also uses `cl` (most packages will), exports the
`start-server` symbol, and also *imports* the `get-message` symbol from
`bobbio`. This means that code in the `:bobbio.web` will be able to use
`get-message` without including `bobbio:` before it.

### bobbio.lisp

Next, put the following into `bobbio.lisp`:

~~~ common_lisp
;;;; bobbio.lisp
(in-package :bobbio)

(defconstant alphabet-length 26)
(defconstant ascii-value-A 65)

(defun random-char ()
  "Return a random character"
  (code-char ( ascii-value-A
                (random alphabet-length))))

(defun get-message ()
  (format nil "Your random letter is: ~a" (random-char)))
~~~

The call to (in-package)[clhs-defpackage] changes the current "namespace" to
`:bobbio`. After that, the file just defines a couple of functions we'll use
later on. 

### web.lisp

Put the following into `web.lisp`:

~~~ common_lisp
;;;; web.lisp
(in-package :bobbio.web)

(defvar *app* (make-instance 'ningle:<app>))

(setf (ningle:route *app* "/")
      get-message)

(defun start-server ()
  (clack:clackup *app*))
~~~

This creates an instance of the `ningle:<app>` class, which is a very minimal
framework for writing web applications. `get-message` is set as the handler for
the "/" route, and the server is started using `clack`, an HTTP server
abstraction.

At this point, if you started a lisp image and evaluated the code in all of
these files, you would be able to evaluate `(bobbio.web:start-server)` and your
shiny new web application would spring into existence. Assuming you had the
required dependencies (ningle, clack) installed.

### bobbio.asd

Put the following into `bobbio.asd`:

~~~ common_lisp
;;;; bobbio.asd
(in-package :cl-user)

(asdf:defsystem #:bobbio
  :version "0.1"
  :description "Something epic"
  :author "You"
  :depends-on (#:ningle
               #:clack)
  :serial t
  :components ((:file "package")
               (:file "bobbio")
               (:file "web")))
~~~

This defines the `bobbio` system and tells ASDF a few things about it
- which other systems it depends on
- which components (files) are required to build it
- build order -- each component depends on the previous one (the `:serial t` line)

Now, from a fresh lisp image, you can evaluate `(asdf:oos 'asdf:load-op
'bobbio)`, and ASDF will use the above definition to load all dependencies and
build the source files.


### Quicklisp

If you don't have the two dependencies required for `bobbio`, they can be easily
installed with Quicklisp. Either install them manually:

~~~ common_lisp
(ql:quickload :ningle)
(ql:quickload :clack)
~~~

Or get Quicklisp to load all dependencies automatically! Quicklisp has a "local
projects" directory, in which it will search for ASDF system definitions,
letting you load your projects as if they were in the official Quicklisp system
repository. This includes loading their dependencies.

You have two options -- either do all of your development in the Quicklisp local
projects directory (`~/quicklisp/local-projects` by default), or symlink your
projects into it manually. Once there, you can evaluate:

~~~ common_lisp
(ql:quickload :bobbio)
~~~


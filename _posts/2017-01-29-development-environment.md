---
layout: post
title: Getting Started With Common Lisp
---

An overview of how to quickly set up a development environment and get started
with Common Lisp.

Getting started developing with Common Lisp is tricky, and probably one of the
reasons people are turned off by the language in general. Which is a shame
really. It's like being put off desert because there are too many choices on the
menu (the correct choice is always tiramisu).

* toc
{:toc}

Read on to find out how to:
- Install a Common Lisp implementation
- Add some useful libraries
- Install a development environment

The entire process should take less than 5 minutes (I'm ignoring download time
-- convenient right?).


{:comment}
TODO: give the brief overview of the essential parts. Then talk about the
details, using Roswell, etc. Make it something that will have lasting value.
{:endcomment}


## Installing Lisp

Let's answer the immediate practical question:

> How do I even install Lisp!?

And in most cases, the answer is this:

> Google "Steel Bank Common Lisp", and install it. Now.

Or just click [this link](http://www.sbcl.org/platform-table.html) to go
straight to the download page. Or use your favourite package manager, which will
probably have an `sbcl` package available.

There you go, you've installed Common Lisp (the SBCL implementation, to be
precise - [there are others](#notes-on-implementations)). Fire it up in a
terminal:

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
environment, **congratulations**. You can do everything you'd expect in a REPL,
and a lot more you wouldn't, like intalling libraries.

Lets try something silly:

~~~ common_lisp
* (princ "Hello")
Hello
"Hello"
* (quit)
~~~

Next up - getting libraries.


## Libraries

Currently the best way to explore and experiment with Common Lisp libraries is
[Quicklisp][ql-main]. Think of it like Python's `pip`, but with more parentheses.

Download Quicklisp and follow the install instructions [here][ql-inst]. Make sure you
do the bit that adds Quicklisp to SBCL's startup script (`ql:add-to-init-file`)
as this makes development way easier...

For more details about how libraries work, see [Libraries]({% post_url
2017-01-29-libraries %}). Assuming we're in SBCL and Quicklisp has been
installed, lets just install a few useful ones now:
- Loads of utilities ([alexandria][alex]) `(ql:quickload :alexandria)`
- Regular expressions ([cl-ppcre][ppcre]) `(ql:quickload :cl-ppcre)`
- Clojure-like arrow macros ([cl-arrows][arrows]) `(ql:quickload :cl-arrows)`
- String manipulation ([cl-strings][strings]) `(ql:quickload :cl-strings)`

I'll probably talk about these libraries in future articles, and will link back
here when they're ready!

[ql-inst]: https://www.quicklisp.org/beta/#installation
[ql-main]: https://www.quicklisp.org/beta/
[alex]: https://common-lisp.net/project/alexandria/
[ppcre]: http://weitz.de/cl-ppcre/
[arrows]: https://github.com/nightfly19/cl-arrows
[strings]: https://github.com/diogoalexandrefranco/cl-strings


## IDE

Now that "Common Lisp" is installed, you'll want something to develop with. If
this was a C# tutorial, this is where you'd be told to install Microsoft Visual
C#, or maybe Mono. Really all you need is a programming editor, but what you
actually *want* is a tool that integrates tightly with Common Lisp, greatly
enhancing the development experience.

*Opinionated stuff here, feel free to ignore it if that bothers you.*

Download and install the following:
- Emacs (version >25)
- Spacemacs

And then enable the 'common-lisp' layer in Spacemacs.

_That is all_.

(If you have now idea what Spacemacs is, "enabling a layer" probably means
nothing to you, in which case you should have a look at the Spacemacs
[documentation][spacemacs-doc], it's really good.)

The most productive IDE I know of for developing with Lisp is Emacs. Nothing
else I've used comes close. For any language in fact. Got something better? Let
me know! There are loads of other articles around describing how to setup Emacs
for Lisp development, so go and read:
- <http://nullprogram.com/blog/2010/01/15/>
- <http://emacsrocks.com/>
- <https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Blang/common-lisp>

Alternatively, you could use Vim + [Slimv](https://github.com/kovisoft/slimv).
I've never done so, and can't comment.

Even more alternatively, use a full-blown CL IDE, such as Allegro or LispWorks.
If you're used to using something like Visual Studio, or Eclipse, you'll
probably want this option.

[spacemacs-doc]: http://spacemacs.org/doc/QUICK_START


## Done!

You now have a Common Lisp implementation installed, a few useful libraries, and
an IDE to develop in. YAY.

Was that less than 5 minutes?

---

## Recap

Great, so now you're totally set up for developing Common Lisp. Lets recap what
you actually have installed:

* SBCL, a popular Common Lisp "implementation"
* Quicklisp, a library manager
* Spacemacs, a modern Emacs configuration

SBCL is the thing that "runs" CL code. It contains, among other things, an
interpreter, a compiler, and a REPL interface. But SBCL is more than that --
the [docs][sbcl-docs] are pretty good. Quicklisp lets you easily install
libraries. Spacemacs provides a Common Lisp development environment,
including [SLIME][slime], with enhanced SBCL support -- type `M-x slime` to run
it!


[sbcl-docs]: http://www.sbcl.org/manual/index.html
[slime]: https://common-lisp.net/project/slime/


## Notes on Implementations

I told you to install SBCL. This was not the only option.

"Lisp" is a large set of languages, that, as far as I can tell, includes
anything that is written using s-expressions. Opinions vary. Anyway, the
important thing is that Common Lisp is a particualar variant, which was
standardised by ANSI in [1994][cl-ansi]. The standard describes the
functionality that must be present in a Common Lisp implementation, many (many)
of which exist.

There are open source (e.g., SBCL, Clasp) and closed source implementations, and
as they all must implement the features specified by ANSI, you are guaranteed
some level of portability between implementations. However, the extra stuff
bundled with implementations varies, and so you cannot expect any non-standard
features to be portable. Notably:
- Multi-threading ability
- Unicode support
- Executable format
- Execution speed

SBCL is an open source, popular implementation with lots of features that make
it great. It has been under development for a long time, and hence is very
stable, it has fantastic editor integration (e.g. via SLIME in Emacs), and
compiles to well optimised native code. It also runs on lots of platforms.

Other interesting Common Lisps:
- [ECL](https://common-lisp.net/project/ecl/), which compiles down to C
- [Clasp](https://github.com/drmeister/clasp), for its C/C++ inter-operability


### Roswell

[Roswell](https://github.com/roswell/roswell) is an actively developed "Lisp
implementation installer/manager, launcher, and much more", which is becoming
more popular for full stack Common Lisp development. Among other things, it lets
you install multiple Lisp implementations on one machine.


[cl-ansi]: https://standards.incits.org/apps/group_public/project/details.php?project_id=1012

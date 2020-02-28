---
layout: draft_post
title: Web Scraping
---

The Lisp REPL lends itself well to developing web scrapers -- it is easy to
quickly examine and extract information from documents.

* toc
{:toc}

You'll want to install [Plump][plump] and [CLSS][clss], the two libraries that
make this all really easy. Also you'll want [Dexador](dexador) to retrieve web
pages. Install them with Quicklisp:

~~~ common_lisp
(ql:quickload '(:plump :clss :dexador))
~~~

You will probably also want to install utility libraries such
as [Alexandria][alexandria], [cl-arrows][cl-arrows] and [cl-ppcre][cl-ppcre].

~~~ common_lisp
(ql:quickload '(:alexandria :cl-arrows :cl-ppcre))
~~~

[dexador]: https://github.com/fukamachi/dexador
[plump]: https://shinmera.github.io/plump
[clss]: https://github.com/Shinmera/CLSS
[alexandria]: https://common-lisp.net/project/alexandria
[cl-arrows]: https://github.com/nightfly19/cl-arrows


# General Techniques



# Helpers and Utilities

Here are some useful bits of code. They're mainly obvious, but useful to have
around rather than installing some large CL utility library. Then again, none of
these (as implemented at this time) exist in the commonly known CL utility
libraries.


### Random Sleep

~~~ common_lisp
(defun sleep-random ()
  "Sleep 1-2 seconds"
  (sleep (+ 1 (/ (random 1000) 1000))))
~~~

Often, when scraping a large website, you don't want to hit their servers with a
load of requests all at once. This will probably get your requests blocked or
ignored. Instead, call this between requests to space them out.


### with-url

~~~ common_lisp
(defmacro with-url ((doc url) &body body)
  "Execute `body' with `url' as a plump document bound to `doc'"
  `(let ((,doc (plump:parse (dex:get url))))
     ,@body))
     
;; Example
(with-url (doc "http://scraped.com")
  (format t "#title text: ~A" (plump:text (clss:select "#title" doc))))
~~~

A tiny bit of syntax to make working with pages slightly nicer. If you often
need to work with multiple pages, another neat macro might let you do something
like:

~~~ common_lisp
(let-docs ((doc1 "http://site1.com")
           (doc2 "http://site2.com"))
  ;; Do things
  )
~~~


### Element Selection

~~~ common_lisp
(defmacro clss-let (doc bindings &body body)
  "Uses CLSS to select something and assign it to a variable"
  (let ((bindings-let (iter
                        (for (var sel) in bindings)
                        (collecting `(,var (clss:select ,sel ,doc))))))
    `(let (,@bindings-let)
       ,@body)))
       
;; Example
(clss-let page-doc
    ((el1 "#element1")
     (special-links "a.special"))
  (format t "element text: ~A~%" (plump:text el1))
  (format t "number of .special links: ~A~%" (length special-links)))
~~~

This is a neat macro for working with pages where you want to pull out lots of
different elements and work with the data.

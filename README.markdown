[![Build Status](https://travis-ci.org/diagrams/diagrams-builder.png?branch=master)](http://travis-ci.org/diagrams/diagrams-builder)

`diagrams-builder` provides backend-agnostic tools for dynamically
turning code into rendered
[diagrams](http://projects.haskell.org/diagrams), using the
[hint](http://hackage.haskell.org/package/hint) wrapper to the GHC
API.  It supports conditional recompilation using hashing of diagrams
source code, to avoid recompiling code that has not changed.  It is
useful for creating tools which compile diagrams code embedded in
other documents.  For example, it is used by the
[BlogLiterately-diagrams](http://hackage.haskell.org/package/BlogLiterately%2Ddiagrams)
package (a plugin for
[BlogLiterately](http://hackage.haskell.org/package/BlogLiterately))
to compile diagrams embedded in
[Markdown](http://daringfireball.net/projects/markdown/)-formatted
blog posts, and by the
[diagrams-latex.sty](https://github.com/diagrams/diagrams-builder/blob/master/latex/diagrams-latex.sty)
package for embedding diagrams in LaTeX documents (see below).

Executables specific to the
[cairo](http://github.com/diagrams/diagrams-cairo), [svg](http://github.com/diagrams/diagrams-svg),
[postscript](http://github.com/diagrams/diagrams-postscript), [rasterific](http://github.com/diagrams/diagrams-rasterific), and [PGF](http://github.com/diagrams/diagrams-pgf) backends are included.  Each
takes an input file and an expression to render and outputs an image
file using the appropriate backend.  You must explicitly enable
whichever executables you want using flags like `-fcairo`, `-fsvg`,
and so on.

A LaTeX package, `diagrams-latex.sty`, is also provided in the
`latex/` directory of the source distribution, which renders diagrams
code found within `diagram` environments.  It can make use of any of
the `diagrams-builder-cairo`, `diagrams-builder-postscript`, or
`diagrams-builder-pgf` executables, so if you want to use
`diagrams-latex.sty` you should install `diagrams-builder` with the
appropriate option.  Note that `diagrams-latex.sty` is licensed under
the GPL.  For more information on using `diagrams-latex.sty`, see
[this tutorial](http://projects.haskell.org/diagrams/doc/latex.html).

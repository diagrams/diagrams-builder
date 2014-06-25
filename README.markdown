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
blog posts.

An executable specific to the
[cairo backend](http://github.com/diagrams/diagrams-cairo) is included
(more executables specific to other backends will be included in the
future).  It takes an input file and an expression to render and
outputs an image file, using the cairo backend.  If you want it you
must explicitly enable the cairo flag with `-fcairo`.
		     
A LaTeX package, `diagrams-latex.sty`, is also provided in the
`latex/` directory of the source distribution, which renders diagrams
code found within `diagram` environments.  It makes use of the
`diagrams-builder-cairo` executable, so if you want to use
`diagrams-latex.sty` you should install `diagrams-builder` with the
`-fcairo` option.  Note that `diagrams-latex.sty` is licensed under
the GPL.

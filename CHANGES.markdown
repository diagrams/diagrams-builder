0.3: 3 Feburary 2013
--------------------

+ new `diagrams-builder-postscript` tool
+ miscellaneous improvements to `diagrams-latex.sty`

0.2.1.0: 11 December 2012
-------------------------

+ new `diagrams-builder-svg` tool
+ deal properly with an empty list of sources ([\#2](https://github.com/diagrams/diagrams-builder/issues/2))
+ put cached diagrams in `.diagrams_cache` instead of `diagrams` by default
+ bug fix: cached files should use same extension as requested output, not "png"
+ bug fix: create output directory for cached images if it doesn't exist
+ new module `Diagrams.Builder.CmdLine`; factor out common utilities
  for command-line tools
+ depend on 0.6 versions of diagrams libs

0.2.0.0: 26 August 2012
-----------------------

Initial release

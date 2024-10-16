## [v0.8.0.6](https://github.com/diagrams/diagrams-builder/tree/v0.8.0.6) (2023-07-10)

- Bump upper bounds: allow
    - `base-4.18`
    - `base-orphans-0.9`
    - `mtl-2.3`
    - `transformers-0.6`
    - `lens-5.2`
    - `diagrams-postscript-1.5`
    - `bytestring-0.11`
- Test on GHC 9.4 and 9.6
- Drop support for GHC < 8.10

- r1 (2024-02-06):
    - allow `bytestring-0.12` and `base-4.19`
    - Test on GHC 9.8
    - Note as of this release the `-fcairo` flag is not supported on
      GHC 9.8.
- r2 (2024-10-16):
    - allow `filepath-1.5`, `lens-5.3`, and `hashable-1.5`
    - Note it still does not support GHC 9.10 due to dependency issues.

## [v0.8.0.5](https://github.com/diagrams/diagrams-builder/tree/v0.8.0.5) (2020-01-21)

- Bump upper bounds: allow
    - `base-4.13` (GHC 8.8)
    - `haskell-src-exts-1.22`
    - `haskell-src-exts-simple-1.22`
    - `lens-4.18`
    - `hashable-1.3`

- r1 (2021-08-09):
    - allow `base-4.15` (GHC 8.10 and 9.0)
    - allow `haskell-src-exts-1.23` and `haskell-src-exts-simple-1.23`
    - allow `lens-5.0`
- r2 (2021-08-19): update URL
- r3 (2021-12-30):
    - allow `base-4.16` (GHC 9.2)
    - allow `lens-5.1`
    - allow `hashable-1.4`
- r4 (2021-12-31):
    - allow `lens-5.1` and `bytestring-0.11` in executables

## [v0.8.0.4](https://github.com/diagrams/diagrams-builder/tree/v0.8.0.4) (2019-02-08)

- Bump upper bounds: allow
    - `base-4.12`
    - `lens-4.17`
    - `hint-0.9`
    - `JuicyPixels-3.3`
- New options added to `diagrams-latex.sty`

## [v0.8.0.3](https://github.com/diagrams/diagrams-builder/tree/v0.8.0.3) (2018-06-09)

- Bump upper bounds: allow `exceptions-0.10`, `lens-4.16`, `hint-0.8`,
  `base-4.11`, `base-orphans-0.7`
- Test with GHC 8.4
- Drop GHC 7.8 support

## [v0.8.0.2](https://github.com/diagrams/diagrams-builder/tree/v0.8.0.2) (2018-01-21)

- Allow `haskell-src-exts-1.20` and `haskell-src-exts-simple-1.20`
- Bug fix for GHC 8.2: temp module generation now uses a valid name ([#32](https://github.com/diagrams/diagrams-builder/issues/32))

## [v0.8.0.1](https://github.com/diagrams/diagrams-builder/tree/v0.8.0.1) (2016-11-28)

- Allow `haskell-src-exts-1.19` and `haskell-src-exts-simple-1.19`

- Allow `hint-0.7` in Hackage revision 2.
- Allow `base-4.10` in Hackage revision 3.

## [v0.8](https://github.com/diagrams/diagrams-builder/tree/v0.8) (2016-10-26)

- `diagrams-builder-pgf`: add catch-all case for file extension
- Better defaults ([#29](https://github.com/diagrams/diagrams-builder/issues/29)):
    - default extension will be chosen based on backend
    - `input` option automatically selected when using `pgf` backend
    - `diagrams` is now default output directory
- Modules [can now be imported qualified](https://github.com/diagrams/diagrams-builder/pull/17)
- allow `lens-4.15`
- Require `haskell-src-exts-1.18` and `haskell-src-exts-simple`
- Require `diagrams-*-1.4`

## [v0.7.2.4](https://github.com/diagrams/diagrams-builder/tree/v0.7.2.4) (2016-09-05)

Update upper bounds, and build with GHC 8.  Allow:

- `transformers-0.5`
- `lens-4.14`
- `base-4.9`
- `hint-0.6`

Now requires `diagrams-svg-1.4` and `svg-builder`.

## [v0.7.2.3](https://github.com/diagrams/diagrams-builder/tree/v0.7.2.3) (2016-03-19)

- allow `hint-0.5`

## [v0.7.2.2](https://github.com/diagrams/diagrams-builder/tree/v0.7.2.2) (2016-01-16)

- allow `base-orphans-0.5`

## [v0.7.2.1](https://github.com/diagrams/diagrams-builder/tree/v0.7.2.1) (2015-12-06)

- allow `haskell-src-exts-1.17.*`

## [v0.7.2.0](https://github.com/diagrams/diagrams-builder/tree/v0.7.2.0) (2015-09-22)

- allow `lens-4.13`
- Add `TypeFamilies` and `FlexibleContexts` extensions automatically
  to diagrams code, so users do not have to add them manually when
  building with GHC 7.10

[Full Changelog](https://github.com/diagrams/diagrams-builder/compare/v0.7.1.1...v0.7.2.0)

## [v0.7.1.1](https://github.com/diagrams/diagrams-builder/tree/v0.7.1.1) (2015-07-19)

[Full Changelog](https://github.com/diagrams/diagrams-builder/compare/v0.7.1...v0.7.1.1)

0.7.1 (16 June 2015)
--------------------

- allow `base-orphans-0.4`
- add `diagrams-pgf` executable
- add diagrams-latex option to use \input instead of \includegraphics,
  for use with diagrams-pgf

0.7.0.4 (1 June 2015)
---------------------

- `diagrams-builder-svg` now builds again (does not build in any other
  `0.7.0.x` releases)

0.7.0.3 (1 June 2015)
---------------------

- get `Typeable Any` instance from `base-orphans` package

[v0.7.0.2](https://github.com/diagrams/diagrams-builder/tree/v0.7.0.2) (2015-05-26)
-----------------------------------------------------------------------------------

- allow `lens-4.11`

0.7.0.1 (20 April 2015)
-----------------------

- Fix compilation errors in diagrams-builder-rasterific executable

0.7 (19 April 2015)
-------------------

- Update to `diagrams-lib-1.3`.
- Add `diagrams-rasterific` support.
- Add support for `DIAGRAMS_SANDBOX` environment variable.
- Allow `exceptions-0.8`
- Fix `diagrams-latex` directory creation on Windows

0.6.0.4 (2 April 2015)
----------------------

- allow `lens-4.9`

0.6.0.3 (13 Jan 2015)
---------------------

- Allow `lens-4.7`

0.6.0.2 (20 November 2014)
--------------------------

- Allow `lens-4.6`
- Allow and require `haskell-src-exts-1.16`

0.6.0.1 (22 August 2014)
------------------------

  - Allow lens-4.4

0.6 (5 June 2014)
-----------------

  - Require `diagrams-lib-1.2`
  - Change to the type of `buildDiagram` in `0.5.0.11` actually required a major
    version bump.

0.5.0.11 (2 June 2014) (BROKEN)
----------------------

  - Allow `transformers-0.4`
  - Allow `lens-4.2`
  - Allow `mtl-2.2`

0.5.0.10 (21 May 2014)
----------------------

  - Module parse error messages now include the error location
    (thanks to Yiding Jia)

0.5.0.9 (15 April 2014)
-----------------------

  - Fix bug introduced by 0.5.0.8 release (accidentally included
    commit meant for compatibility with unreleased versions of other
    diagrams packages)

0.5.0.8 (15 April 2014) (BROKEN)
--------------------------------

  - Allow `haskell-src-exts-1.15`

0.5.0.7 (7 April 2014)
----------------------

  - Allow `exceptions-0.5`

0.5.0.6 (20 March 2014)
----------------------

  - Allow `lens-4.1` in executables also

0.5.0.5 (19 March 2014)
----------------------

  - Allow `lens-4.1`

0.5.0.4 (15 March 2014)
-----------------------

    - Update for `hint-0.4`
	- Allow `diagrams-lib-1.1` and `diagrams-cairo-1.1`
	- Allow `lens-4.0`

0.5.0.3 (8 March 2014)
----------------------

    - Allow `base-4.7`

0.5.0.2 (6 March 2014)
----------------------

    - Allow `diagrams-lib-1.1`
	- Allow `lens-4.0`

0.5.0.1 (27 January 2014)
-------------------------

    - Allow hashable-1.1

0.5 (27 January 2014)
---------------------

    - Consolidate buildDiagram arguments into a single record of
      arguments.
    - Add a `postProcess` argument, a function to be applied to the
      interpreted diagram.

0.4.2 (26 November 2013)
------------------------

    - `buildDiagram` now accepts expressions of type `Diagram` or `IO
      Diagram`: it just tries both.
    - Add dependency on `mtl`.

0.4.1 (24 November 2013)
------------------------

	- bug fix: avoid ambiguity errors when parsing unknown operators
    - update to work with diagrams-1.0

0.4.0.6 (2 October 2013)
------------------------

    - allow cryptohash-0.11

0.4.0.5 (11 September 2013)
---------------------------

    - require diagrams-svg >= 0.8.0.1

0.4.0.4 (11 September 2013)
---------------------------

    - fix compile error in diagrams-builder-svg executable
    - require diagrams-svg >= 0.8

0.4.0.3 (10 September 2013) [BROKEN]
------------------------------------

    - allow diagrams-svg-0.8

0.4.0.2 (31 August 2013)
------------------------

    - allow cryptohash-0.10

0.4.0.1 (22 August 2013)
------------------------

    - allow haskell-src-exts 1.14

0.4: 9 August 2013
------------------

* Add hsenv compatibility.
* Big improvements in the way rebuilding is handled:
    - Strip comments before deciding whether to rebuild, so
      changing only comments does not trigger a rebuild
    - Take local imports into account: if a diagram has an import
      which corresponds to a local file, rebuild if that file has
      changed
    - Rebuild when options (e.g. size) change

0.3.0.1 (3 April 2013)
----------------------

* depend on cryptohash >= 0.8 && < 0.10 (use new unified API)
* remove base16-bytestring dependency

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

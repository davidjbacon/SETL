# GNU SETL - an extension and implementation of SETL

SETL is a high-level programming language based on sets, especially
mappings.  GNU SETL is an implementation of that language, with some
extensions.

Bleeding-edge builds can be made by cloning this repository and doing a
"make" if you have Automake, Texinfo, and other tools of that ilk
installed.

Otherwise, the path of least resistance is to grab a release bundle,
posted here as `setl-X.Y.Z.tgz` and mirrored at <https://setl.org/setl/>,
and unpack that.  A "make" in the resulting `setl-X.Y.Z` dir,
in a POSIXish environment such as Linux or Cygwin, requires
a C compiler (usually gcc) and Yacc-compatible parser generator
(usually bison), but not the more exotic developer tools.

Please see also [README](./README).

<David.Bacon@nyu.edu>

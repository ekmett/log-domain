next
----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

0.11
----
* Replace use of `Hashable1` from `hashable-extras` in favor of `Hashable` from
  `hashable-1.2.5.0`. As a result, the `hashable-extras` dependency has been removed.
* On Windows, we now use the FFI to link against the C math library if building with
  GHC 8.0 or later, which features a much improved runtime linker story.
* Remove `generic-deriving` dependency

0.10.3.1
--------
* Support `safecopy` 0.9

0.10.3
------
* Work around an issue with `safecopy` on GHC 7.10
* Changed the repository link to my `ekmett` github account from `analytics`.

0.10.2.1
--------
* Add `vector` 0.11 support.

0.10.2
------
* Add `generic-deriving` 1.8 support. We also no longer incur a `generic-deriving` dependency at all on GHC 7.6+

0.10.1.1
--------
* Compiles warning-free on GHC 7.10

0.10.1
------
* `semigroupoids` 5 support.

0.10.0.1
--------
* Improved the stability and portability of the `doctest` test suite

0.10
----
* `(**)` is now much more accurately defined.
* We now avoid comparisons for equality with infinities.
* Fixed a bug in `negate`.
* On windows we avoid FFI into the math library, and accept less accurate results. (Sorry!)

0.9.3
-------
* Fixed subtraction again. For real this time.

0.9.2.1
-------
* Support `generic-deriving` 1.7

0.9.2
-----
* Fixed subtraction better.

0.9.1
-----
* Fixed subtraction.

0.8
---
* Updated to `comonad` and `semigroupoids` 4.

0.7.2
-----
* Dependency bump to allow `comonad` and `semigroupoids` 4.0

0.7.1
-----
* Marked `Numeric.Log` `Trustworthy`.

0.6
---
* Renamed the data constructor to `Exp` and the field accessor to `ln` per issue #1.

0.5.0.1
-------
* Wider bounds for `generic-deriving` so we can build with GHC HEAD.

0.5
---
* Switched the `Hashable1` instance to use the new, lighter, `hashable-extras`

0.4
---
* `instance Hashable1 Log`

0.3.0.1
-------
* Wider `binary` version bound

0.3
---
* Added support for `cereal`.

0.2
---
* Added an `Enum` instance.
* Added `sum` to calculate using the `log-sum-exp` trick.

0.1.0.1
-------
* Minor packaging changes

0.1
---
* Renamed from `log` to `log-domain` due to internal hackage issues rendering that name inaccessible.
* Ported `Numeric.Log` from [analytics](http://github.com/analytics) at the request of @bgamari

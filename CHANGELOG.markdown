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

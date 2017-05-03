A QuickCheck model-based test of ptrees (Patricia trees)
========================================================

This is an example of building a model-based QuickCheck testsuite with
OCaml's QCheck library.

It tests whether the Patricia tree implementation 'ptrees' agrees with
a simple model (read: an abstract specification or semantics) of sets,
represented as sorted lists. Interestingly it doesn't.

It turns out to be a problem due to signed integer comparison
inherited from Okasaki and Gill's paper "Fast mergable integer maps"
(ML'98).

To recreate the bug the directory 'ptrees' contains an old copy of the
library.


Building
--------

The `Makefile` includes two targets: `old` and `new`.
To recreate the problem run `make old` and then `./qctest.byte`
This should illustrate the issue:

    random seed: 413762386
    law empty: 1 relevant cases (1 total)
    law singleton test: 2500 relevant cases (2500 total)
    law mem test: 2500 relevant cases (2500 total)
    law add test: 2500 relevant cases (2500 total)
    law remove test: 2500 relevant cases (2500 total)
    law union test: 1776 relevant cases (1776 total)
      test `union test`
      failed on >= 1 cases:
      (Union (Singleton -4611686018427387904, Singleton 0), Add (1, Singleton -4611686018427387904)) (after 33 shrink steps)
      
    law inter test: 2500 relevant cases (2500 total)
    failure (1 tests failed, ran 7 tests)


To confirm that the issue has been fixed, run `make new` and
`./qctest.byte` again.

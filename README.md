A QuickCheck model-based test of ptrees (Patricia trees)
========================================================

This is an example of building a model-based QuickCheck testsuite with
OCaml's [QCheck](https://github.com/c-cube/qcheck/) library.

It tests whether the Patricia tree implementation 'ptrees' agrees with
a simple model (read: an abstract specification or semantics) of sets,
represented as sorted lists. Interestingly it doesn't.

It turns out to be a problem due to signed integer comparison
inherited from Okasaki and Gill's paper ["Fast mergable integer maps"](http://ittc.ku.edu/~andygill/papers/IntMap98.pdf) (ML'98).

For full details please see my paper draft ["QuickChecking Patricia Trees"](http://janmidtgaard.dk/papers/Midtgaard%3a17.pdf).

To recreate the bug the directory 'ptrees' contains an old copy of the
library. Note that 'ptrees' has since been split into separate libraries:
  - ['ptset'](https://github.com/backtracking/ptset) implementing integer sets and
  - ['ptmap'](https://github.com/backtracking/ptmap) implementing maps with integer keys.


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
`./qctest.byte` again. This will instead test the newer (patched)
'ptset' submodule against the model.

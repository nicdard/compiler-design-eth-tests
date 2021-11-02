# compiler-design-eth-tests
Shared tests for the compiler design projects, HS21.

## Use

The repository contains a copy of the tests I personally use for the projects.
Each folder contains a whole copy of the file where we can write our own tests.

To use them, you can simply replace the whole *studenttests.ml* with the one provided here.

## Contribute

Please, if you find any problem or bug in the provided tests, open an *issue*.
If you know how to fix them, you can also go ahead and open a PR with the fix.

If you have some other tests you have already written, you can either open a PR or contact me.
In any case, I will keep an eye at the course's forum in orther to gather all tests that are published there.

## Setup
If you want to integrate these tests into your project instead of copying the files you can follow these steps:

1. Clone this repo into the folder containing the `Makefile` of hw3.
2. Adapt the Makefile to include these tests:
```
     main.native: gradedtests.ml studenttests.ml main.ml driver.ml backend.ml util/platform.ml
-       ocamlbuild -Is util,x86,ll,grading -libs unix,str,nums main.native -use-menhir
+       ocamlbuild -Is util,x86,ll,grading,compiler-design-eth-tests/03 -libs unix,str,nums main.native -use-menhir
 
 main.byte: gradedtests.ml studenttests.ml main.ml driver.ml backend.ml util/platform.ml
-       ocamlbuild -Is util,x86,ll,grading -libs unix,str,nums main.byte -use-menhir
+       ocamlbuild -Is util,x86,ll,grading,compiler-design-eth-tests/03 -libs unix,str,nums main.byte -use-menhir
```

2a. *Optional*: If you use merlin you should add these lines to the `.merlin` file:
```
+B _build/compiler-design-eth-tests
+B _build/compiler-design-eth-tests/03
```
3. Add our shared suite to `main.ml`:
```
-let suite = ref (Studenttests.provided_tests @ Gradedtests.graded_tests)
+let suite = ref (
+  Studenttests.provided_tests @
+  Gradedtests.graded_tests @ 
+  Sharedtests.shared_suite)
 ```

And now `make test` should also run our tests :) 
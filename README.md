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

1. Clone this repo anywhere you like (let the path to the cloned repo be called `<repo_path>` and let `<project_path>` be the directory, which contains the makefile for the current homework)
2. Create a symbolic link to `<repo_path>` called `sharedtests` in `<project_path>`
3. Adapt the Makefile to include these tests:
```diff
     main.native: gradedtests.ml studenttests.ml main.ml driver.ml backend.ml util/platform.ml
-       ocamlbuild -Is util,x86,ll,grading -libs unix,str,nums main.native -use-menhir
+       ocamlbuild -Is util,x86,ll,grading,sharedtests,sharedtests/0{2,3,4,5,6} -libs unix,str,nums main.native -use-menhir
 
 main.byte: gradedtests.ml studenttests.ml main.ml driver.ml backend.ml util/platform.ml
-       ocamlbuild -Is util,x86,ll,grading -libs unix,str,nums main.byte -use-menhir
+       ocamlbuild -Is util,x86,ll,grading,sharedtests,sharedtests/0{2,3,4,5,6} -libs unix,str,nums main.byte -use-menhir
```
(Note: depending on the makefile, the additional directories might need to be added at the top in a _DIRS_ variable or similar)
4. Add our shared suite to `main.ml`:
```diff
-let suite = ref (Studenttests.provided_tests @ Gradedtests.graded_tests)
+let suite = ref (
+  Studenttests.provided_tests @
+  Gradedtests.graded_tests @ 
+  Sharedtests.shared_suite)
```
5. Edit the `global_prefix` in the file `Test_config` of this repo to represent the relative path from inside `<project_path>` to `<repo_path>`. By default it is setup such that `<repo_path>` and `<project_path>` have the same parent directory.
6. *Optional*: If you use merlin you should add these lines to the `.merlin` file:
```diff
+B _build/sharedtests/**
+S sharedtests/**
```

And now `make test` should also run our tests :) 

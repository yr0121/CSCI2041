+ Pass: Check that directory "Lab_15" exists.

+ Pass: Change into directory "Lab_15".

+ Pass: Check that file "src/translate.ml" exists.

## Map

+ Fail: Check that the result of executing build.sh on `examples/three.src.txt` matches the pattern `Returned \[2, 4, 6, 8\]`.

   

   Test failed. The following errors were reported:

   ```
[1;36mBuilding OCaml code...[0m
00:00:00 0    (0   ) STARTING                                        -------- |[K00:00:00 7    (0   ) src/parser.mly                                  O------- |[K[K+ /usr/bin/ocamlyacc src/parser.mly
6 shift/reduce conflicts.
00:00:00 14   (0   ) src/src_lang_tyck.mli.depends                   O-b---i- |[K00:00:00 21   (0   ) src/src_lang_pp.cmi                             O-B---I- |[K00:00:00 30   (0   ) src/common.ml.depends                           O-b---i- |[K00:00:00 35   (0   ) src/parser.cmx                                  ONb---i- /[K00:00:00 40   (0   ) src/tgt_lang.cmx                                ONb---i- /[K[K+ /usr/bin/ocamldep.opt -modules src/translate.ml > src/translate.ml.depends
File "src/translate.ml", line 148, characters 4-6:
Error: Syntax error
[KCommand exited with code 2.
Compilation unsuccessful after building 43 targets (0 cached) in 00:00:00.[K

   ```


+  _0_ / _5_ : Skip: Check that the result of executing build.sh on `examples/four.src.txt` matches the pattern `Returned \[2, 4, 6, 8\]`.

   

  This test was not run because of an earlier failing test.

+  _0_ / _10_ : Fail: Check that the result of executing build.sh on `examples/five.src.txt` matches the pattern `Returned 16`.

   

   Test failed. The following errors were reported:

   ```
[1;36mBuilding OCaml code...[0m
00:00:00 1    (1   ) src/compiler.ml.depends                       * O------- |[K[K+ /usr/bin/ocamldep.opt -modules src/translate.ml > src/translate.ml.depends
File "src/translate.ml", line 148, characters 4-6:
Error: Syntax error
[KCommand exited with code 2.
Compilation unsuccessful after building 43 targets (42 cached) in 00:00:00.[K

   ```


## Extlint

Various code style and organization checks are run on your code to detect common errors.

A description of the checks can be found here:  https://github.umn.edu/umn-csci-2041-S18/public-class-repo/blob/master/Course%20Info/extlint.md

### `src/translate.ml`

+ Pass: Check that file "src/translate.ml" exists.

+ Fail: 

```
    [1;34m-->[0m src/translate.ml:148:4
[1;34m     | [0m
[1;34m 147 | [0m    *)
[1;34m 148 | [0m    in failwith "TODO translate_map"
[1;34m     | [0m[1;31m    ^^ syntax error[0m
[1;34m 149 | [0m
[1;34m     | [0m
```



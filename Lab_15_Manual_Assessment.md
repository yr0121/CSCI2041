+ Pass: Check that directory "Lab_15" exists.

+ Pass: Change into directory "Lab_15".

+ Pass: Check that file "src/translate.ml" exists.

## Map

+ Fail: Check that the result of executing build.sh on `examples/three.src.txt` matches the pattern `Returned \[2, 4, 6, 8\]`.

   

   Test failed. The following errors were reported:

   ```
[1;36mBuilding OCaml code...[0m

6 shift/reduce conflicts.

File "src/translate.ml", line 148, characters 4-6:
Error: Syntax error



   ```


+  _0_ / _5_ : Skip: Check that the result of executing build.sh on `examples/four.src.txt` matches the pattern `Returned \[2, 4, 6, 8\]`.

   

  This test was not run because of an earlier failing test.

+  _0_ / _10_ : Fail: Check that the result of executing build.sh on `examples/five.src.txt` matches the pattern `Returned 16`.

   

   Test failed. The following errors were reported:

   ```
[1;36mBuilding OCaml code...[0m

File "src/translate.ml", line 148, characters 4-6:
Error: Syntax error



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


### Assessment for Lab 12

#### Total score: _32_ / _50_

Run on April 14, 22:47:46 PM.

+ Pass: Change into directory "Lab_12".

+ Pass: Check that file "interpreter.ml" exists.

+ Pass: Check that an OCaml file "interpreter.ml" has no syntax or type errors.

    OCaml file "interpreter.ml" has no syntax or type errors.



+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   num_sum
   ```
   matches the pattern `11`.

   




+  _0_ / _4_ : Fail: 
Check that the result of evaluating
   ```
   val_sum_evens
   ```
   matches the pattern `56`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 
   ```
 ;;
[24mError: This expression has type int but an expression was expected of type
         value

   ```



+  _0_ / _4_ : Fail: 
Check that the result of evaluating
   ```
   val_sum_odds
   ```
   matches the pattern `49`.

   


   Your solution evaluated incorrectly and produced some part of the following:

 
   ```
 ;;
[24mError: This expression has type int but an expression was expected of type
         value

   ```



+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   num_sum_evens
   ```
   matches the pattern `9`.

   




+  _4_ / _4_ : Pass: 
Check that the result of evaluating
   ```
   num_sum_odds
   ```
   matches the pattern `8`.

   




+  _10_ / _10_ : Pass: 
Check that the result of evaluating
   ```
   lookup "sum_evens" (exec program_while_ifthenelse_test [])
   ```
   matches the pattern `Int 30`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   lookup "y" (exec program_ifthen [("x",Int 4)])
   ```
   matches the pattern `Int 6`.

   




+  _5_ / _5_ : Pass: 
Check that the result of evaluating
   ```
   lookup "sum" (exec program_sum_10 [])
   ```
   matches the pattern `Int 55`.

   




These tests use `program_sum_10_n` from the lab protocol.

+  _0_ / _5_ : Fail: 
Check that the result of evaluating
   ```
   lookup "sum" (exec program_sum_10_n [])
   ```
   matches the pattern `Int 55`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    1
    1
    - : value = Int 1
    ```


+  _0_ / _5_ : Fail: 
Check that the result of evaluating
   ```
   lookup "n" (exec program_sum_10_n [])
   ```
   matches the pattern `Int 55`.

   


   Your solution evaluated incorrectly and produced some part of the following:

    ```
    1
    1
    - : value = Int 1
    ```



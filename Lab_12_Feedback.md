### Feedback for Lab 12

Run on April 12, 11:52:58 AM.

+ Pass: Change into directory "Lab_12".

+ Pass: Check that file "interpreter.ml" exists.

+ Pass: Check that an OCaml file "interpreter.ml" has no syntax or type errors.

    OCaml file "interpreter.ml" has no syntax or type errors.



+ Pass: 
Check that the result of evaluating
   ```
   lookup "sum_evens" (exec program_while_ifthenelse_test [])
   ```
   matches the pattern `Int 30`.

   




+ Pass: 
Check that the result of evaluating
   ```
   lookup "y" (exec program_ifthen [("x",Int 4)])
   ```
   matches the pattern `Int 6`.

   




+ Pass: 
Check that the result of evaluating
   ```
   lookup "sum" (exec program_sum_10 [])
   ```
   matches the pattern `Int 55`.

   





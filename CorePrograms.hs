module CorePrograms where

exercise1_21 =
  "f=3;\n\
    \g x y = let z = x in z;\n\
    \h x = case (let y = x in y) of\n\
    \    <1> -> 2;\n\
    \    <2> -> 5"

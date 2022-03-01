open import Four renaming (_∨_ to _∨₄_ ; _∧_ to _∧₄_)
open import Two

module Convert where

pos : B4 → B2 
pos ⊥ = f
pos t = t
pos f = f
pos ⊤ = t

neg : B4 → B2 
neg ⊥ = f
neg t = f
neg f = t
neg ⊤ = t



open import Data.Nat using (ℕ) renaming (zero to zero ; suc to succ)
open import Data.Fin using (Fin ; fold) renaming (zero to fzero ; suc to fsucc ; inject₁ to up)
open import Data.Fin.Patterns
open import Data.Bool using (Bool ; true ; false ; if_then_else_)
open import Data.List using (foldr ; tabulate)
open import Values

open import Four using (B4 ; ⊥ ; t ; f ; ⊤)


module Table where

record row {A : Set} (V : B A) (m n : ℕ) : Set where
    field
        inputs : Fin m → A
        outputs : Fin n → A 

open row

open boollike

translate-row-to-binary : {A : Set} {V : B A} {m n : ℕ} (bl : boollike A) → row V m n → row b m n
inputs (translate-row-to-binary bl r) i = tob bl (inputs r i)
outputs (translate-row-to-binary bl r) i = tob bl (outputs r i)

record table {A : Set} (V : B A) (m n k : ℕ) : Set where
    field
        rows : Fin k → row V m n

open table

translate-table-to-binary : {A : Set} {V : B A} {m n k : ℕ} (bl : boollike A) → table V m n k → table b m n k
rows (translate-table-to-binary bl x) i = translate-row-to-binary bl (rows x i)

smaller-row : {A : Set} {V : B A} {m n k : ℕ} → row V (succ m) n → row V m n
inputs (smaller-row r) i = inputs r (up i)
outputs (smaller-row r) = outputs r


row-to-exp : {m n : ℕ} → row b m n → exp b m
row-to-exp r = foldr (λ acc cur → acc and cur) (const true) 
    (tabulate λ x → if inputs r x then var x else (not (var x)))

table-to-exp : {m n k : ℕ} → table b m n k → Fin n → exp b m
table-to-exp trt i = foldr (λ acc cur → acc or cur) (const false) 
    (tabulate (λ x → if (outputs (rows trt x) i) then row-to-exp (rows trt x) else const false))

ex : table b4 2 1 2
inputs (rows ex 0F) 0F = ⊤
inputs (rows ex 0F) 1F = t
outputs (rows ex 0F) 0F = ⊤
inputs (rows ex 1F) 0F = ⊤
inputs (rows ex 1F) 1F = f
outputs (rows ex 1F) 0F = f

to-positive-table : {m n k : ℕ} → table b4 m n k → table b m n k
to-positive-table trt = translate-table-to-binary b4-pos-boollike trt

to-negative-table : {m n k : ℕ} → table b4 m n k → table b m n k
to-negative-table trt = translate-table-to-binary b4-neg-boollike trt

-- to-function : {m n k : ℕ} → table b4 m n k → exp b4
-- to-function {m} {n} {k} tt = {!   !}
--     where
--         pos : table b2l m n k
--         pos = to-positive-table tt
--         neg : table b2r m n k
--         neg = to-negative-table tt  
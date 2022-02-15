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
    (tabulate λ x → if inputs r x then var x else var x)

table-to-exp : {m n k : ℕ} → table b m n k → Fin n → exp b m
table-to-exp trt i = foldr (λ acc cur → acc or cur) (const false) 
    (tabulate (λ x → if (outputs (rows trt x) i) then row-to-exp (rows trt x) else const false))

raiseₗ : {m : ℕ} → exp b m → exp b4 m
raiseₗ (const false) = const ⊥
raiseₗ (const true) = const t
raiseₗ (var i) = var i
raiseₗ (x and y) = raiseₗ x and raiseₗ y
raiseₗ (x or y) = raiseₗ x or raiseₗ y
raiseₗ (not x) = not (raiseₗ x)
raiseₗ (x join y) = raiseₗ x join raiseₗ y

raiseᵣ : {m : ℕ} → exp b m → exp b4 m
raiseᵣ (const false) = const ⊥
raiseᵣ (const true) = const f
raiseᵣ (var i) = var i
raiseᵣ (x and y) = raiseᵣ x and raiseᵣ y
raiseᵣ (x or y) = raiseᵣ x or raiseᵣ y
raiseᵣ (not x) = not (raiseᵣ x)
raiseᵣ (x join y) = raiseᵣ x join raiseₗ y

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

tidy : {m : ℕ} → exp b m → exp b m
tidy (const v) = const v
tidy (var i) = var i
tidy (x and const false) = const false
tidy (x and const true) = x
tidy (x and y) = tidy x and tidy y
tidy (x or const false) = x
tidy (x or const true) = const true
tidy (x or y) = tidy x or tidy y
tidy (not x) = not (tidy x)
tidy (x join y) = tidy x join tidy y

combine-pos-and-neg : {m n k : ℕ} → table b4 m n k → (Fin n) → exp b4 m
combine-pos-and-neg {m} {n} {k} trt i = raiseₗ poss-exp join raiseᵣ negg-exp where
    poss : table b m n k 
    poss = to-positive-table trt
    negg : table b m n k 
    negg = to-negative-table trt 
    poss-exp : exp b m
    poss-exp = tidy (table-to-exp poss i)
    negg-exp : exp b m
    negg-exp = tidy (table-to-exp negg i)

ex-exp : exp b4 2
ex-exp = combine-pos-and-neg ex fzero

ex-assg-1 : Fin 2 → B4
ex-assg-1 0F = ⊤
ex-assg-1 1F = t

ex-assg-2 : Fin 2 → B4
ex-assg-2 0F = ⊤
ex-assg-2 1F = f

test-1 : B4
test-1 = eval {B4} {b4} (((var 0F and (var 1F and const t)) or (const ⊥ or const ⊥))) (ex-assg-1)

test-2 : B4
test-2 = eval {B4} {b4} ((var 0F and (not (var 1F) and const f)) or ((var 0F and (var 1F and const f)) or const ⊥)) (ex-assg-1)


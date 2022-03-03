open import Data.Nat using (ℕ) renaming (zero to zero ; suc to succ)
open import Data.Fin using (Fin ; fold) renaming (zero to fzero ; suc to fsucc ; inject₁ to up)
open import Data.Fin.Patterns
open import Data.Bool using (Bool ; true ; false ; if_then_else_ ; _∧_)
open import Data.List using (List ; foldr ; tabulate ; [] ; _∷_ ; [_] )
open import Data.Vec.Functional using (Vector ; foldl ; zip) renaming (map to mapv)
open import Data.Maybe hiding (zip)
open import Data.Product using (_×_ ; _,_) renaming (map to map× ; proj₁ to fst ; proj₂ to snd)

open import Two
open import Four using (B4 ; ⊥ ; t ; f ; ⊤)
open import Expression
open import Values

open is-bool

module Table where

record row {A : Set} (V : B A) (m n : ℕ) : Set where
    field
        inputs : Fin m → A
        outputs : Fin n → A 

open row

vec-eq : {V : Set} {m : ℕ} → (V → V → Bool) → (vs ws : Vector V m) → Bool
vec-eq eq vs ws = foldl (λ acc → λ (fst , snd) → acc ∧ (eq fst snd)) true (zip vs ws)

table : (V : Set) (m n k : ℕ) → Set
table V m n k = Vector (Vector V m × Vector V n) k

convert-table : {V W : Set} {m n k : ℕ} → (V → W) → table V m n k → table W m n k
convert-table {V} {W} {m} {n} {k} fn trt i = map× (mapv fn) (mapv fn) v-row where
    v-row : Vector V m × Vector V n
    v-row = trt i

to-pos-table : {m n k : ℕ} → table B4 m n k → table B2ₗ m n k
to-pos-table tt = convert-table pos tt

to-neg-table : {m n k : ℕ} → table B4 m n k → table B2ᵣ m n k
to-neg-table tt = convert-table neg tt

eval-inputs-to-table : {V : Set} {m n k : ℕ} → (V → V → Bool) → table V m n k → Vector V m → Maybe (Vector V n)
eval-inputs-to-table eq tt vs = foldl (λ acc → λ (fst , snd) → if (vec-eq eq fst vs) then (just snd) else acc) nothing tt


table-to-exp : {V : Set} {A : B V} {m n k : ℕ} → (col-unit : V) (row-unit : V) → is-bool V → table V m n k → Vector (exp A m) n
table-to-exp {V} {A} {m} {n} {k} col-unit row-unit ib trt i = {!   !} where
    rows : Vector (exp A m) k
    rows j = if (fn ib ((snd the-row) i)) then fold-row else (const (ff ib)) where
        the-row : Vector V m × Vector V n
        the-row = trt j
        tabulated : Vector (exp A m) m
        tabulated = λ i → if (fn ib (fst the-row i)) then var i else not (var i)
        fold-row : exp A m 
        fold-row = foldl (λ acc cur → acc and cur) (const col-unit) tabulated
    fold-rows : Vector (exp A m) k → (exp A m)
    fold-rows x = foldl (λ acc cur → acc or cur) (const row-unit) rows

ex : table B4 2 1 3
ex 0F = ((λ { 0F → ⊤ ; 1F → t }) , λ { 0F → t })
ex 1F = ((λ { 0F → t ; 1F → ⊥ }) , λ { 0F → ⊥ })
ex 2F = ((λ { 0F → f ; 1F → f }) , λ { 0F → ⊤ })

ex-vec : Vector B4 2
ex-vec 0F = ⊤
ex-vec 1F = t

-- ex : table b4 2 1 2
-- inputs (rows ex 0F) 0F = ⊤
-- inputs (rows ex 0F) 1F = t
-- outputs (rows ex 0F) 0F = ⊤
-- inputs (rows ex 1F) 0F = ⊤
-- inputs (rows ex 1F) 1F = f
-- outputs (rows ex 1F) 0F = f

-- ex-assg-1 : Fin 2 → B4
-- ex-assg-1 0F = ⊤
-- ex-assg-1 1F = t

-- ex-assg-2 : Fin 2 → B4
-- ex-assg-2 0F = ⊤
-- ex-assg-2 1F = f

-- test-1 : B4
-- test-1 = eval {B4} {b4} (((var 0F and (var 1F and const t)) or (const ⊥ or const ⊥))) (ex-assg-1)

-- test-2 : B4
-- test-2 = eval {B4} {b4} ((var 0F and (not (var 1F) and const f)) or ((var 0F and (var 1F and const f)) or const ⊥)) (ex-assg-1)

   
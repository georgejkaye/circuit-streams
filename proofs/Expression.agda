open import Data.Nat using (ℕ)
open import Data.Fin using (Fin)
open import Data.Bool using (Bool ; _∨_ ; _∧_) renaming (not to ¬)

open import Four renaming (_∨_ to _∨₄_ ; _∧_ to _∧₄_ ; ¬ to ¬₄ ; _⊔_ to _⊔₄_)
open import Two
open import Values

module Expression where

data exp {A : Set} (V : B A) (n : ℕ) : Set where
    const : (v : A) → exp V n
    var : (i : Fin n) → exp V n
    _and_ : exp V n → exp V n → exp V n
    _or_ : exp V n → exp V n → exp V n
    not : exp V n → exp V n
    _join_ : exp V n → exp V n → exp V n

eval : {A : Set} {V : B A} {n : ℕ} → (φ : exp V n) → (env : Fin n → A) → A
eval (const v) env = v
eval (var i) env = env i
eval {.Bool} {b} (φ and ψ) env = (eval φ env) ∧ (eval ψ env)
eval {.B4} {b4} (φ and ψ) env = (eval φ env) ∧₄ (eval ψ env)
eval {.B2ₗ} {b2l} (φ and ψ) env = (eval φ env) ∧ₗ (eval ψ env)
eval {.B2ᵣ} {b2r} (φ and ψ) env = (eval φ env) ∧ᵣ (eval ψ env)
eval {.B4} {b4} (φ or ψ) env = (eval φ env) ∨₄ (eval ψ env)
eval {.Bool} {b} (φ or ψ) env = (eval φ env) ∨ (eval ψ env)
eval {.B2ₗ} {b2l} (φ or ψ) env = (eval φ env) ∨ₗ (eval ψ env)
eval {.B2ᵣ} {b2r} (φ or ψ) env = (eval φ env) ∨ᵣ (eval ψ env)
eval {.Bool} {b} (not φ) env = ¬ (eval φ env)
eval {.B4} {b4} (not φ) env = ¬₄ (eval φ env)
eval {.B2ₗ} {b2l} (not φ) env = ¬ₗ (eval φ env)
eval {.B2ᵣ} {b2r} (not φ) env = ¬ᵣ (eval φ env)
eval {.Bool} {b} (φ join ψ) env = (eval φ env) ∨ (eval ψ env)
eval {.B4} {b4} (φ join ψ) env = (eval φ env) ⊔₄ (eval ψ env)
eval {.B2ₗ} {b2l} (φ join ψ) env = (eval φ env) ⊔ₗ (eval ψ env)
eval {.B2ᵣ} {b2r} (φ join ψ) env = (eval φ env) ⊔ᵣ (eval ψ env) 

test : exp b4 0
test = (const t) and (const f)

exp-pos : {n : ℕ} → exp b4 n → exp b2l n 
exp-pos (const v) = const (pos v)
exp-pos (var n) = var n
exp-pos (x and y) = exp-pos x and exp-pos y
exp-pos (x or y) = exp-pos x or exp-pos y
exp-pos (not x) = not (exp-pos x)
exp-pos (x join y) = (exp-pos x) join (exp-pos y)

exp-neg : {n : ℕ} → exp b4 n → exp b2r n
exp-neg (const v) = const (neg v)
exp-neg (var n) = var n
exp-neg (x and y) = exp-neg x and exp-neg y
exp-neg (x or y) = exp-neg x or exp-neg y
exp-neg (not x) = not (exp-neg x)
exp-neg (x join y) = (exp-neg x) join (exp-neg y)
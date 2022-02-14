{-# OPTIONS --allow-unsolved-metas #-}

open import Data.Nat using (ℕ ; _<_ ; _≤_ ; z≤n ; zero ) renaming (suc to succ ; _≡ᵇ_ to _≡ℕ_ ; _≤ᵇ_ to _≤ℕ_ ; _⊔_ to max )
open import Data.List using (List ; _∷_ ; [] ; [_] ; length )
open import Data.Bool using (Bool ; true ; false ; _∧_ ; _∨_) renaming (not to ¬)
open import Data.Fin using (Fin)

open import Four renaming (_∧_ to _∧₄_ ; _∨_ to _∨₄_ ; ¬ to ¬₄ ; _⊔_ to _⊔₄_)
open import Two
open import Circuits

open import Relation.Binary.PropositionalEquality

module Values where

𝓵 : B4 → B2ₗ
𝓵 ⊥ = ₀₀
𝓵 t = ₁₀
𝓵 f = ₀₀
𝓵 ⊤ = ₁₀

𝓻 : B4 → B2ᵣ
𝓻 ⊥ = ₀₀
𝓻 t = ₀₀
𝓻 f = ₀₁
𝓻 ⊤ = ₀₁

𝓵⁻¹ : B2ₗ → B4
𝓵⁻¹ ₀₀ = ⊥
𝓵⁻¹ ₁₀ = t

𝓻⁻¹ : B2ᵣ → B4
𝓻⁻¹ ₀₀ = ⊥
𝓻⁻¹ ₀₁ = f

pos : B4 → Bool
pos ⊥ = false
pos t = true
pos f = false
pos ⊤ = true

neg : B4 → Bool
neg ⊥ = false
neg t = false
neg f = true
neg ⊤ = true

data B : Set → Set where
    b : B Bool
    b4 : B B4
    b2l : B B2ₗ
    b2r : B B2ᵣ

record boollike (A : Set) : Set where
    field
        tt : A
        ff : A
        tob : A → Bool
        truth-yes : tob tt ≡ true
        truth-no : tob ff ≡ false

open boollike

b4-pos-boollike : boollike B4 
tt b4-pos-boollike = t
ff b4-pos-boollike = ⊥
tob b4-pos-boollike ⊥ = false
tob b4-pos-boollike t = true
tob b4-pos-boollike f = false
tob b4-pos-boollike ⊤ = true
truth-yes b4-pos-boollike = refl
truth-no b4-pos-boollike = refl

b4-neg-boollike : boollike B4 
tt b4-neg-boollike = f
ff b4-neg-boollike = ⊥
tob b4-neg-boollike ⊥ = false
tob b4-neg-boollike t = false
tob b4-neg-boollike f = true
tob b4-neg-boollike ⊤ = true
truth-yes b4-neg-boollike = refl
truth-no b4-neg-boollike = refl

b2l-is-boollike : boollike B2ₗ 
b2l-is-boollike = record { 
    tt = ₁₀ ; 
    ff = ₀₀ ; 
    tob = λ { ₁₀ → true ; ₀₀ → false}; 
    truth-yes = refl ; 
    truth-no = refl 
    }

b2r-is-boollike : boollike B2ᵣ
b2r-is-boollike = record { 
    tt = ₀₁ ; 
    ff = ₀₀ ; 
    tob = λ { ₀₁ → true ; ₀₀ → false}; 
    truth-yes = refl ; 
    truth-no = refl 
    }

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

exp-𝓵 : {n : ℕ} → exp b4 n → exp b2l n 
exp-𝓵 (const v) = const (𝓵 v)
exp-𝓵 (var n) = var n
exp-𝓵 (x and y) = exp-𝓵 x and exp-𝓵 y
exp-𝓵 (x or y) = exp-𝓵 x or exp-𝓵 y
exp-𝓵 (not x) = not (exp-𝓵 x)
exp-𝓵 (x join y) = (exp-𝓵 x) join (exp-𝓵 y)

exp-𝓻 : {n : ℕ} → exp b4 n → exp b2r n
exp-𝓻 (const v) = const (𝓻 v)
exp-𝓻 (var n) = var n
exp-𝓻 (x and y) = exp-𝓻 x and exp-𝓻 y
exp-𝓻 (x or y) = exp-𝓻 x or exp-𝓻 y
exp-𝓻 (not x) = not (exp-𝓻 x)
exp-𝓻 (x join y) = (exp-𝓻 x) join (exp-𝓻 y)

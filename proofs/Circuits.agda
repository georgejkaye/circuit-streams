open import Data.Nat using (ℕ ; _<_ ; _≤_ ; s≤s ; zero ; z≤n ) renaming (suc to succ ; _≡ᵇ_ to _≡ℕ_ ; _≤ᵇ_ to _≤ℕ_ ; _⊔_ to max )
open import Data.List using (List ; _∷_ ; [] ; [_] ; length ; any ; foldl ; map )
open import Data.Sum using () renaming (_⊎_ to _+_)
open import Data.Product using (_×_ ; _,_) renaming (proj₁ to fst ; proj₂ to snd)
open import Data.Bool using (if_then_else_ ; true ; false)
open import Relation.Binary.PropositionalEquality

module Circuits where

≡≤ : {m n k : ℕ} → m ≤ n → m ≡ k → k ≤ n
≡≤ z≤n refl = z≤n
≡≤ (s≤s p) refl = s≤s p

nth : {A : Set} → (n : ℕ) (xs : List A) → n < length xs → A
nth zero (x ∷ xs) p = x
nth (succ n) (x ∷ xs) (s≤s p) = nth n xs p 

succ≡ : {a b : ℕ} → a ≡ b → succ a ≡ succ b
succ≡ refl = refl 

bool-to-≡ : {m n : ℕ} → (m ≡ℕ n) ≡ true → m ≡ n
bool-to-≡ {zero} {zero} refl = refl
bool-to-≡ {succ m} {succ n} x = succ≡ IH where
    IH : m ≡ n
    IH = bool-to-≡ x



postulate max-l : {m n k : ℕ} → max m n < k → m < k
postulate max-r : {m n k : ℕ} → max m n < k → n < k

data B4 : Set where
    ⊥ : B4
    t : B4
    f : B4
    ⊤ : B4

_∧₄_ : B4 → B4 → B4
⊥ ∧₄ ⊥ = ⊥
⊤ ∧₄ ⊥ = f
⊤ ∧₄ ⊤ = ⊤
⊥ ∧₄ ⊤ = f
⊥ ∧₄ t = ⊥
⊥ ∧₄ f = f
t ∧₄ x = x
f ∧₄ x = f
⊤ ∧₄ t = ⊤
⊤ ∧₄ f = f

_∨₄_ : B4 → B4 → B4
⊥ ∨₄ ⊥ = ⊥
⊥ ∨₄ ⊤ = t
⊤ ∨₄ ⊤ = ⊤
⊤ ∨₄ ⊥ = t
t ∨₄ x = t
f ∨₄ x = x
⊥ ∨₄ t = t
⊥ ∨₄ f = ⊥
⊤ ∨₄ t = t
⊤ ∨₄ f = ⊤

_⊔₄_ : B4 → B4 → B4 
⊥ ⊔₄ x = x
t ⊔₄ ⊥ = t
t ⊔₄ t = t
t ⊔₄ f = ⊤
t ⊔₄ ⊤ = ⊤
f ⊔₄ ⊥ = f
f ⊔₄ t = ⊤
f ⊔₄ f = f
f ⊔₄ ⊤ = ⊤
⊤ ⊔₄ ⊥ = ⊤
⊤ ⊔₄ t = ⊤
⊤ ⊔₄ f = ⊤
⊤ ⊔₄ ⊤ = ⊤

¬₄ : B4 → B4
¬₄ ⊥ = ⊥
¬₄ t = t
¬₄ f = f
¬₄ ⊤ = ⊤

-- 2 elements

data B2ₗ : Set where
    ₀₀ : B2ₗ
    ₁₀ : B2ₗ

data B2ᵣ : Set where
    ₀₀ : B2ᵣ
    ₀₁ : B2ᵣ

_∧ₗ_ : B2ₗ → B2ₗ → B2ₗ
₀₀ ∧ₗ ₀₀ = ₀₀
₀₀ ∧ₗ ₁₀ = ₀₀
₁₀ ∧ₗ ₀₀ = ₀₀
₁₀ ∧ₗ ₁₀ = ₁₀

_∨ₗ_ : B2ₗ → B2ₗ → B2ₗ
₀₀ ∨ₗ ₀₀ = ₀₀
₀₀ ∨ₗ ₁₀ = ₁₀
₁₀ ∨ₗ ₀₀ = ₁₀
₁₀ ∨ₗ ₁₀ = ₁₀

¬ₗ : B2ₗ → B2ₗ 
¬ₗ ₀₀ = ₁₀
¬ₗ ₁₀ = ₀₀

_⊔ₗ_ : B2ₗ → B2ₗ → B2ₗ
₀₀ ⊔ₗ ₀₀ = ₀₀
₀₀ ⊔ₗ ₁₀ = ₁₀
₁₀ ⊔ₗ ₀₀ = ₁₀
₁₀ ⊔ₗ ₁₀ = ₁₀

_∧ᵣ_ : B2ᵣ → B2ᵣ → B2ᵣ
₀₀ ∧ᵣ ₀₀ = ₀₀
₀₀ ∧ᵣ ₀₁ = ₀₁
₀₁ ∧ᵣ ₀₀ = ₀₁
₀₁ ∧ᵣ ₀₁ = ₀₁

_∨ᵣ_ : B2ᵣ → B2ᵣ → B2ᵣ
₀₀ ∨ᵣ ₀₀ = ₀₀
₀₀ ∨ᵣ ₀₁ = ₀₀
₀₁ ∨ᵣ ₀₀ = ₀₀
₀₁ ∨ᵣ ₀₁ = ₀₁

¬ᵣ : B2ᵣ → B2ᵣ
¬ᵣ ₀₀ = ₀₁
¬ᵣ ₀₁ = ₀₀ 

_⊔ᵣ_ : B2ᵣ → B2ᵣ → B2ᵣ
₀₀ ⊔ᵣ ₀₀ = ₀₀
₀₀ ⊔ᵣ ₀₁ = ₀₁
₀₁ ⊔ᵣ ₀₀ = ₀₁
₀₁ ⊔ᵣ ₀₁ = ₀₁

↓ₗ : B4 → B2ₗ
↓ₗ ⊥ = ₀₀
↓ₗ t = ₁₀
↓ₗ f = ₀₀
↓ₗ ⊤ = ₁₀

↓ᵣ : B4 → B2ᵣ
↓ᵣ ⊥ = ₀₀
↓ᵣ t = ₀₀
↓ᵣ f = ₀₁
↓ᵣ ⊤ = ₀₁

↑ₗ : B2ₗ → B4
↑ₗ ₀₀ = ⊥
↑ₗ ₁₀ = t

↑ᵣ : B2ᵣ → B4
↑ᵣ ₀₀ = ⊥
↑ᵣ ₀₁ = f

data B : Set → Set where
    b4 : B B4
    b2l : B B2ₗ
    b2r : B B2ᵣ

data exp {A : Set} (V : B A) : Set where
    const : (v : A) → exp V
    var : (n : ℕ) → exp V
    and : exp V → exp V → exp V
    or : exp V → exp V → exp V
    not : exp V → exp V
    join : exp V → exp V → exp V

free-variables : {A : Set} {V : B A} → (ϕ : exp V) → ℕ
free-variables (const v) = 0
free-variables (var n) = n
free-variables (and l r) = max (free-variables l) (free-variables r)
free-variables (or l r) = max (free-variables l) (free-variables r)
free-variables (not exp) = free-variables exp
free-variables (join l r) = max (free-variables l) (free-variables r)

eval : {A : Set} {V : B A} → (φ : exp V) → (env : List A) → free-variables φ < length env → A
eval (const v) env p = v
eval (var n) env p = nth n env p
eval {.B4} {b4} (and φ ψ) env p = (eval φ env (max-l p)) ∧₄ (eval ψ env (max-r p))
eval {.B2ₗ} {b2l} (and φ ψ) env p = (eval φ env (max-l p)) ∧ₗ (eval ψ env (max-r p))
eval {.B2ᵣ} {b2r} (and φ ψ) env p = (eval φ env (max-l p)) ∧ᵣ (eval ψ env (max-r p))
eval {.B4} {b4} (or φ ψ) env p = (eval φ env (max-l p)) ∨₄ (eval ψ env (max-r p))
eval {.B2ₗ} {b2l} (or φ ψ) env p = (eval φ env (max-l p)) ∨ₗ (eval ψ env (max-r p))
eval {.B2ᵣ} {b2r} (or φ ψ) env p = (eval φ env (max-l p)) ∨ᵣ (eval ψ env (max-r p))
eval {.B4} {b4} (not φ) env p = ¬₄ (eval φ env p)
eval {.B2ₗ} {b2l} (not φ) env p = ¬ₗ (eval φ env p)
eval {.B2ᵣ} {b2r} (not φ) env p = ¬ᵣ (eval φ env p)
eval {.B4} {b4} (join φ ψ) env p = (eval φ env (max-l p)) ⊔₄ (eval ψ env (max-r p))
eval {.B2ₗ} {b2l} (join φ ψ) env p = (eval φ env (max-l p)) ⊔ₗ (eval ψ env (max-r p))
eval {.B2ᵣ} {b2r} (join φ ψ) env p = (eval φ env (max-l p)) ⊔ᵣ (eval ψ env (max-r p)) 

open import Data.Nat using (ℕ ; _<_ ; _≤_ ; z≤n ; zero ) renaming (suc to succ ; _≡ᵇ_ to _≡ℕ_ ; _≤ᵇ_ to _≤ℕ_ ; _⊔_ to max )
open import Data.List using (List ; _∷_ ; [] ; [_] ; length )

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

data B : Set → Set where
    b4 : B B4
    b2l : B B2ₗ
    b2r : B B2ᵣ

data exp {A : Set} (V : B A) : Set where
    const : (v : A) → exp V
    var : (n : ℕ) → exp V
    _and_ : exp V → exp V → exp V
    _or_ : exp V → exp V → exp V
    not : exp V → exp V
    _join_ : exp V → exp V → exp V

free-variables : {A : Set} {V : B A} → (ϕ : exp V) → ℕ
free-variables (const v) = 0
free-variables (var n) = succ n
free-variables (l and r) = max (free-variables l) (free-variables r)
free-variables (l or r) = max (free-variables l) (free-variables r)
free-variables (not exp) = free-variables exp
free-variables (l join r) = max (free-variables l) (free-variables r)

eval : {A : Set} {V : B A} → (φ : exp V) → (env : List A) → free-variables φ ≤ length env → A
eval (const v) env p = v
eval (var n) env p = nth n env p
eval {.B4} {b4} (φ and ψ) env p = (eval φ env (max-l p)) ∧₄ (eval ψ env (max-r p))
eval {.B2ₗ} {b2l} (φ and ψ) env p = (eval φ env (max-l p)) ∧ₗ (eval ψ env (max-r p))
eval {.B2ᵣ} {b2r} (φ and ψ) env p = (eval φ env (max-l p)) ∧ᵣ (eval ψ env (max-r p))
eval {.B4} {b4} (φ or ψ) env p = (eval φ env (max-l p)) ∨₄ (eval ψ env (max-r p))
eval {.B2ₗ} {b2l} (φ or ψ) env p = (eval φ env (max-l p)) ∨ₗ (eval ψ env (max-r p))
eval {.B2ᵣ} {b2r} (φ or ψ) env p = (eval φ env (max-l p)) ∨ᵣ (eval ψ env (max-r p))
eval {.B4} {b4} (not φ) env p = ¬₄ (eval φ env p)
eval {.B2ₗ} {b2l} (not φ) env p = ¬ₗ (eval φ env p)
eval {.B2ᵣ} {b2r} (not φ) env p = ¬ᵣ (eval φ env p)
eval {.B4} {b4} (φ join ψ) env p = (eval φ env (max-l p)) ⊔₄ (eval ψ env (max-r p))
eval {.B2ₗ} {b2l} (φ join ψ) env p = (eval φ env (max-l p)) ⊔ₗ (eval ψ env (max-r p))
eval {.B2ᵣ} {b2r} (φ join ψ) env p = (eval φ env (max-l p)) ⊔ᵣ (eval ψ env (max-r p)) 

test : exp b4
test = (const t) and (const f)

exp-𝓵 : exp b4 → exp b2l 
exp-𝓵 (const v) = const (𝓵 v)
exp-𝓵 (var n) = var n
exp-𝓵 (x and y) = exp-𝓵 x and exp-𝓵 y
exp-𝓵 (x or y) = exp-𝓵 x or exp-𝓵 y
exp-𝓵 (not x) = not (exp-𝓵 x)
exp-𝓵 (x join y) = (exp-𝓵 x) join (exp-𝓵 y)

exp-𝓻 : exp b4 → exp b2r
exp-𝓻 (const v) = const (𝓻 v)
exp-𝓻 (var n) = var n
exp-𝓻 (x and y) = exp-𝓻 x and exp-𝓻 y
exp-𝓻 (x or y) = exp-𝓻 x or exp-𝓻 y
exp-𝓻 (not x) = not (exp-𝓻 x)
exp-𝓻 (x join y) = (exp-𝓻 x) join (exp-𝓻 y)

max-l-r-≡ : {a b c d : ℕ} → a ≡ c → b ≡ d → max a b ≡ max c d
max-l-r-≡ refl refl = refl

preserves-free-vars-𝓵 : { ϕ : exp b4 } → free-variables ϕ ≡ free-variables (exp-𝓵 ϕ)
preserves-free-vars-𝓵 {const v} = refl
preserves-free-vars-𝓵 {var n} = refl
preserves-free-vars-𝓵 {ϕ and ψ} = max-l-r-≡ (preserves-free-vars-𝓵 {ϕ}) (preserves-free-vars-𝓵 {ψ})
preserves-free-vars-𝓵 {ϕ or ψ} = max-l-r-≡ (preserves-free-vars-𝓵 {ϕ}) (preserves-free-vars-𝓵 {ψ})
preserves-free-vars-𝓵 {not ϕ} = preserves-free-vars-𝓵 {ϕ}
preserves-free-vars-𝓵 {ϕ join ψ} = max-l-r-≡ (preserves-free-vars-𝓵 {ϕ}) (preserves-free-vars-𝓵 {ψ})

preserves-free-vars-𝓻 : { ϕ : exp b4 } → free-variables ϕ ≡ free-variables (exp-𝓻 ϕ)
preserves-free-vars-𝓻 {const v} = refl
preserves-free-vars-𝓻 {var n} = refl
preserves-free-vars-𝓻 {ϕ and ψ} = max-l-r-≡ (preserves-free-vars-𝓻 {ϕ}) (preserves-free-vars-𝓻 {ψ})
preserves-free-vars-𝓻 {ϕ or ψ} = max-l-r-≡ (preserves-free-vars-𝓻 {ϕ}) (preserves-free-vars-𝓻 {ψ})
preserves-free-vars-𝓻 {not ϕ} = preserves-free-vars-𝓻 {ϕ}
preserves-free-vars-𝓻 {ϕ join ψ} = max-l-r-≡ (preserves-free-vars-𝓻 {ϕ}) (preserves-free-vars-𝓻 {ψ})


round-trip : (ϕ : exp b4) → (free-variables ϕ ≡ zero) → B4
round-trip ϕ p = {!   !} where
    exp-l : exp b2l
    exp-l = exp-𝓵 ϕ
    eval-l : B2ₗ
    eval-l = eval exp-l [] (≡≤ z≤n (sym p0)) where
        p0 : free-variables exp-l ≡ zero
        p0 = trans (sym (preserves-free-vars-𝓵 {ϕ})) p
    exp-r : exp b2r
    exp-r = exp-𝓻 ϕ
    

  
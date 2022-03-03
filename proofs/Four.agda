open import Relation.Binary.PropositionalEquality
open import Data.Bool using (Bool ; true ; false)

module Four where

data B4 : Set where
    ⊥ : B4
    t : B4
    f : B4
    ⊤ : B4

_∧_ : B4 → B4 → B4
⊥ ∧ ⊥ = ⊥
⊤ ∧ ⊥ = f
⊤ ∧ ⊤ = ⊤
⊥ ∧ ⊤ = f
⊥ ∧ t = ⊥
⊥ ∧ f = f
t ∧ x = x
f ∧ x = f
⊤ ∧ t = ⊤
⊤ ∧ f = f

_∨_ : B4 → B4 → B4
⊥ ∨ ⊥ = ⊥
⊥ ∨ ⊤ = t
⊤ ∨ ⊤ = ⊤
⊤ ∨ ⊥ = t
t ∨ x = t
f ∨ x = x
⊥ ∨ t = t
⊥ ∨ f = ⊥
⊤ ∨ t = t
⊤ ∨ f = ⊤

_⊔_ : B4 → B4 → B4 
⊥ ⊔ x = x
t ⊔ ⊥ = t
t ⊔ t = t
t ⊔ f = ⊤
t ⊔ ⊤ = ⊤
f ⊔ ⊥ = f
f ⊔ t = ⊤
f ⊔ f = f
f ⊔ ⊤ = ⊤
⊤ ⊔ ⊥ = ⊤
⊤ ⊔ t = ⊤
⊤ ⊔ f = ⊤
⊤ ⊔ ⊤ = ⊤

¬ : B4 → B4
¬ ⊥ = ⊥
¬ t = f
¬ f = t
¬ ⊤ = ⊤

_↓_ : B4 → B4 → B4
x ↓ y = ¬ (x ∨ y)
 
_↑_ : B4 → B4 → B4 
x ↑ y = ¬ (x ∧ y)

nor : B4 → B4 → B4
nor x y = x ↓ y

nand : B4 → B4 → B4
nand x y = x ↑ y

↓⊥-↓⊥ : {a : B4} → (a ↓ ⊥) ↓ ⊥ ≡ ⊥
↓⊥-↓⊥ {⊥} = refl
↓⊥-↓⊥ {t} = refl
↓⊥-↓⊥ {f} = refl
↓⊥-↓⊥ {⊤} = refl

B4-eq : B4 → B4 → Bool
B4-eq ⊥ ⊥ = true
B4-eq ⊥ t = false
B4-eq ⊥ f = false
B4-eq ⊥ ⊤ = false
B4-eq t ⊥ = false
B4-eq t t = true
B4-eq t f = false
B4-eq t ⊤ = false
B4-eq f ⊥ = false
B4-eq f t = false
B4-eq f f = true
B4-eq f ⊤ = false
B4-eq ⊤ ⊥ = false
B4-eq ⊤ t = false
B4-eq ⊤ f = false
B4-eq ⊤ ⊤ = true
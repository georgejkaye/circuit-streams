module Two where

data B2 : Set where
    f : B2
    t : B2

data B2ₗ : Set where
    f : B2ₗ
    t : B2ₗ

data B2ᵣ : Set where
    f : B2ᵣ
    t : B2ᵣ

_∧ₗ_ : B2ₗ → B2ₗ → B2ₗ
f ∧ₗ f = f
f ∧ₗ t = f
t ∧ₗ f = f
t ∧ₗ t = t

_∨ₗ_ : B2ₗ → B2ₗ → B2ₗ
f ∨ₗ f = f
f ∨ₗ t = t
t ∨ₗ f = t
t ∨ₗ t = t

¬ₗ : B2ₗ → B2ₗ 
¬ₗ f = t
¬ₗ t = f

_⊔ₗ_ : B2ₗ → B2ₗ → B2ₗ
f ⊔ₗ f = f
f ⊔ₗ t = t
t ⊔ₗ f = t
t ⊔ₗ t = t

_∧ᵣ_ : B2ᵣ → B2ᵣ → B2ᵣ
f ∧ᵣ f = f
f ∧ᵣ t = t
t ∧ᵣ f = t
t ∧ᵣ t = t

_∨ᵣ_ : B2ᵣ → B2ᵣ → B2ᵣ
f ∨ᵣ f = f
f ∨ᵣ t = f
t ∨ᵣ f = f
t ∨ᵣ t = t

¬ᵣ : B2ᵣ → B2ᵣ
¬ᵣ f = t
¬ᵣ t = f 

_⊔ᵣ_ : B2ᵣ → B2ᵣ → B2ᵣ
f ⊔ᵣ f = f
f ⊔ᵣ t = t
t ⊔ᵣ f = t
t ⊔ᵣ t = t
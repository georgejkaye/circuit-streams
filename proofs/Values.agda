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

pos : B4 → B2ₗ
pos ⊥ = f
pos t = t
pos f = f
pos ⊤ = t

neg : B4 → B2ᵣ
neg ⊥ = f
neg t = f
neg f = t
neg ⊤ = t

unpos : B2ₗ → B4
unpos f = ⊥
unpos t = t

unneg : B2ᵣ → B4
unneg f = ⊥
unneg t = f

data B : Set → Set where
    b : B Bool
    b4 : B B4
    b2l : B B2ₗ
    b2r : B B2ᵣ
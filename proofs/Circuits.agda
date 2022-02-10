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



postulate max-l : {m n k : ℕ} → max m n ≤ k → m ≤ k
postulate max-r : {m n k : ℕ} → max m n ≤ k → n ≤ k
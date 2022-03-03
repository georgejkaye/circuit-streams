open import Data.Nat using (ℕ)
open import Data.Vec.Functional
open import Data.Fin using (Fin)

open import Four
open import Two
open import Table
open import Values 
open import Expression

module Compute where 

get-function : {m n k : ℕ} → table b4 m n k → (Vector B4 m → Vector B4 n)  
get-function {m} {n} {k} tt vs i = comb-res i
    where 
        pos-tt : table b2l m n k
        pos-tt = to-pos-table tt
        neg-tt : table b2r m n k
        neg-tt = to-neg-table tt
        pos-exp : Fin n → exp b2l m
        pos-exp = table-to-exp b2l-isbool pos-tt
        neg-exp : Fin n → exp b2r m
        neg-exp = table-to-exp b2r-isbool neg-tt
        pos-vs : Vector B2ₗ m
        pos-vs = map pos vs
        neg-vs : Vector B2ᵣ m
        neg-vs = map neg vs
        pos-res : Fin n → B2ₗ 
        pos-res n = eval (pos-exp n) pos-vs
        neg-res : Fin n → B2ᵣ 
        neg-res n = eval (neg-exp n) neg-vs
        comb-res : Fin n → B4
        comb-res n = combine (pos-res n) (neg-res n)



proof : {m n k : ℕ} → table b4 m n k → 
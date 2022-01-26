type value =
    | Bot
    | True
    | False
    | Top

type gate = 
    | And
    | Or
    | Not

let eval_and x y = match (x, y) with
| (True, x) -> x
| (x, True) -> x
| (False, _) -> False
| (_, False) -> False
| (Bot, Bot) -> Bot
| (Bot, Top) -> False
| (Top, Bot) -> False
| (Top, Top) -> Top

let eval_or x y = match (x, y) with
| (True, _) -> True
| (_, True) -> True
| (False, x) -> x
| (x, False) -> x
| (Bot, Bot) -> Bot
| (Bot, Top) -> True
| (Top, Bot) -> True
| (Top, Top) -> Top

let eval_not x = match x with
| Bot -> Bot
| True -> False
| False -> True
| Top -> Top

let eval_gate g xs = match g with
| And -> eval_and (List.nth xs 0) (List.nth xs 1)
| Or  -> eval_or (List.nth xs 0) (List.nth xs 1)
| Not -> eval_not (List.nth xs 0)
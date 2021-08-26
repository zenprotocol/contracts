module Empty

open Zen.Cost
open Zen.Types

module CR = Zen.ContractResult

let main txSkel context contractId command sender messageBody w state = // 15
    CR.ofTxSkel txSkel

val cf:
       txSkel     : txSkeleton
    -> context    : context
    -> command    : string
    -> sender     : sender
    -> messageBody: option data
    -> w          : wallet
    -> state      : option data
    -> nat `cost` 1
let cf _ _ command _ _ w _ =
    ret (4 <: nat)
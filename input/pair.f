typealias Pair = \\X, \\Y, @R, (X->Y->R) -> R;

let pair = \\X. \\Y. \x:X. \y:Y. \\R. \s:X->Y->R. s x y in
let first = \\X. \\Y. \p:Pair[X][Y]. p[X] (\x:X. \y:Y. x) in
let p = pair[Nat][Bool] 10 false in
first[Nat][Bool] p;

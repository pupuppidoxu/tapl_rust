let plus3 = \n: Nat. ++(++(++n)) in
let not = \b: Bool. if b { false } else { true } in
let double = \\T. \f: T->T. \t: T. f (f t) in
let doubleBool = double #Bool in
let quad = \\T. double #T->T (double #T) in
if doubleBool not true { quad #Nat plus3 0 } else { 99 };

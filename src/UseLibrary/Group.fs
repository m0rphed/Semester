module Group

type Monoid<'t> =
    val binaryOp: 't -> 't -> 't
    val neutral: 't
    new (first, second) = { binaryOp = first; neutral = second }

type SemiRing<'t> =
    val monoid: Monoid<'t>
    val multiply: 't -> 't -> 't    
    new (first, second, third) = { monoid = Monoid (first, third); multiply = second; }

type Group<'t> =
    | Monoid of Monoid<'t>
    | SemiRing of SemiRing<'t>

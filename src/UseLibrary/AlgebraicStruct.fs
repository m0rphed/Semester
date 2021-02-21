module AlgebraicStruct

type Monoid<'t> =
    val sum: 't -> 't -> 't
    val neutral: 't
    new (x, y) = { neutral = y; sum = x }

type SemiRing<'t>  =
    val monoid: Monoid<'t>
    val mul: 't -> 't -> 't
    new (mon, mul) = { monoid = mon; mul = mul }

type AlgebraicStruct<'t> =
    | Monoid of Monoid<'t>
    | SemiRing of SemiRing<'t>

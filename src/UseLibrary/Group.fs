module Group

type Monoid<'t> =
    val sum: 't -> 't -> 't
    val neutral: 't
    new (first, second) = {sum = first; neutral = second}

type SemiRing<'t>  =
    val sum: 't -> 't -> 't
    val multiply : 't -> 't -> 't
    val neutral: 't
    new (first, second, third) = {sum = first; multiply = second; neutral = third}

type Group<'t> =
    | Monoid of Monoid<'t>
    | SemiRing of SemiRing<'t>

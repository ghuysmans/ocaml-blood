include Sig.S

val a : ([`A] * [`Nil], [< `A | `Nil] * [`Nil]) t
val b : ([`Nil] * [`B], [`Nil] * [< `B | `Nil]) t
val ab : ([`A] * [`B], [< `A | `Nil] * [< `B | `Nil]) t
val o : ([`Nil] * [`Nil], [< `A | `Nil] * [< `B | `Nil]) t

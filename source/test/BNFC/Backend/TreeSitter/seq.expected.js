
module.exports = grammar({
  name: "seq",
  extras: $ =>[
    /\s/,
  ],
  rules: {
    BNFCStart: $ =>
      optional(
        // BNFCStart_Start. BNFCStart ::= Start ;
        $.Start
      ),
    Start: $ =>
      choice(
        // StartTransitivelyOptional. Start ::= TransitivelyOptional ;
        $.TransitivelyOptional,
        // StartNonOptional. Start ::= NonOptional ;
        $.NonOptional
      ),
    TransitivelyOptional: $ =>
      // TransitivelyOptional1. TransitivelyOptional ::= A B C ;
      choice(
        seq($.A, optional($.B), optional($.C)),
        seq($.B, optional($.C)),
        $.C
      ),
    A: $ =>
      choice(
        // A_a. A ::= "a" ;
        "a",
        // A1. A ::= ;
        choice()
      ),
    B: $ =>
      choice(
        // B_b. B ::= "b" ;
        "b",
        // B1. B ::= ;
        choice()
      ),
    C: $ =>
      choice(
        // C_c. C ::= "c" ;
        "c",
        // C1. C ::= ;
        choice()
      ),
    NonOptional: $ =>
      // NonOptional1. NonOptional ::= "x" A B C ;
      seq("x", optional($.A), optional($.B), optional($.C)),
  },
});

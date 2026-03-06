
module.exports = grammar({
  name: "list_trailing",
  extras: $ =>[
    /\s/,
  ],
  rules: {
    BNFCStart: $ =>
      // BNFCStart_Start. BNFCStart ::= Start ;
      $.Start,
    Start: $ =>
      // Start1. Start ::= "1" [A] ;
      seq("1", optional($.list_A)),
    Opt: $ =>
      choice(
        // Opt_X. Opt ::= "X" ;
        "X",
        // Opt1. Opt ::= ;
        choice()
      ),
    A: $ =>
      // AOpt. A ::= Opt ;
      $.Opt,
    list_A: $ =>
      choice(
        // []. [A] ::= ;
        choice(),
        // (:[]). [A] ::= A ;
        $.A,
        // (:). [A] ::= A "," [A] ;
        seq(optional($.A), ",", optional($.list_A))
      ),
  },
});

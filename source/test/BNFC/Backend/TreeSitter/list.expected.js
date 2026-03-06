
module.exports = grammar({
  name: "list",
  extras: $ =>[
    /\s/,
  ],
  rules: {
    BNFCStart: $ =>
      // BNFCStart_Start. BNFCStart ::= Start ;
      $.Start,
    Start: $ =>
      choice(
        // Start1. Start ::= "1" [NonEmptySeparator] ;
        seq("1", $.list_NonEmptySeparator),
        // Start2. Start ::= "2" [MaybeEmptySeparator] ;
        seq("2", optional($.list_MaybeEmptySeparator)),
        // Start3. Start ::= "3" [NonEmptyTerminator] ;
        seq("3", $.list_NonEmptyTerminator),
        // Start4. Start ::= "4" [MaybeEmptyTerminator] ;
        seq("4", optional($.list_MaybeEmptyTerminator)),
        // Start5. Start ::= "5" [MaybeEmptySeparatorEmptySep] ;
        seq("5", optional($.list_MaybeEmptySeparatorEmptySep)),
        // Start6. Start ::= "6" [MaybeEmptyTerminatorEmptySep] ;
        seq("6", optional($.list_MaybeEmptyTerminatorEmptySep)),
        // Start7. Start ::= "7" [NonEmptySeparatorEmptySep] ;
        seq("7", $.list_NonEmptySeparatorEmptySep),
        // Start8. Start ::= "8" [NonEmptyTerminatorEmptySep] ;
        seq("8", $.list_NonEmptyTerminatorEmptySep)
      ),
    NonEmptySeparator: $ =>
      // NonEmptySeparator_a. NonEmptySeparator ::= "a" ;
      "a",
    list_NonEmptySeparator: $ =>
      choice(
        // (:[]). [NonEmptySeparator] ::= NonEmptySeparator ;
        $.NonEmptySeparator,
        // (:). [NonEmptySeparator] ::= NonEmptySeparator "," [NonEmptySeparator] ;
        seq($.NonEmptySeparator, ",", $.list_NonEmptySeparator)
      ),
    MaybeEmptySeparator: $ =>
      // MaybeEmptySeparator_a. MaybeEmptySeparator ::= "a" ;
      "a",
    list_MaybeEmptySeparator: $ =>
      choice(
        // []. [MaybeEmptySeparator] ::= ;
        choice(),
        // (:[]). [MaybeEmptySeparator] ::= MaybeEmptySeparator ;
        $.MaybeEmptySeparator,
        // (:). [MaybeEmptySeparator] ::= MaybeEmptySeparator "," [MaybeEmptySeparator] ;
        seq($.MaybeEmptySeparator, ",", optional($.list_MaybeEmptySeparator))
      ),
    NonEmptyTerminator: $ =>
      // NonEmptyTerminator_a. NonEmptyTerminator ::= "a" ;
      "a",
    list_NonEmptyTerminator: $ =>
      choice(
        // (:[]). [NonEmptyTerminator] ::= NonEmptyTerminator "," ;
        seq($.NonEmptyTerminator, ","),
        // (:). [NonEmptyTerminator] ::= NonEmptyTerminator "," [NonEmptyTerminator] ;
        seq($.NonEmptyTerminator, ",", $.list_NonEmptyTerminator)
      ),
    MaybeEmptyTerminator: $ =>
      // MaybeEmptyTerminator_a. MaybeEmptyTerminator ::= "a" ;
      "a",
    list_MaybeEmptyTerminator: $ =>
      choice(
        // []. [MaybeEmptyTerminator] ::= ;
        choice(),
        // (:). [MaybeEmptyTerminator] ::= MaybeEmptyTerminator "," [MaybeEmptyTerminator] ;
        seq($.MaybeEmptyTerminator, ",", optional($.list_MaybeEmptyTerminator))
      ),
    MaybeEmptySeparatorEmptySep: $ =>
      // MaybeEmptySeparatorEmptySep_a. MaybeEmptySeparatorEmptySep ::= "a" ;
      "a",
    list_MaybeEmptySeparatorEmptySep: $ =>
      choice(
        // []. [MaybeEmptySeparatorEmptySep] ::= ;
        choice(),
        // (:). [MaybeEmptySeparatorEmptySep] ::= MaybeEmptySeparatorEmptySep [MaybeEmptySeparatorEmptySep] ;
        seq($.MaybeEmptySeparatorEmptySep, optional($.list_MaybeEmptySeparatorEmptySep))
      ),
    MaybeEmptyTerminatorEmptySep: $ =>
      // MaybeEmptyTerminatorEmptySep_a. MaybeEmptyTerminatorEmptySep ::= "a" ;
      "a",
    list_MaybeEmptyTerminatorEmptySep: $ =>
      choice(
        // []. [MaybeEmptyTerminatorEmptySep] ::= ;
        choice(),
        // (:). [MaybeEmptyTerminatorEmptySep] ::= MaybeEmptyTerminatorEmptySep [MaybeEmptyTerminatorEmptySep] ;
        seq($.MaybeEmptyTerminatorEmptySep, optional($.list_MaybeEmptyTerminatorEmptySep))
      ),
    NonEmptySeparatorEmptySep: $ =>
      // NonEmptySeparatorEmptySep_a. NonEmptySeparatorEmptySep ::= "a" ;
      "a",
    list_NonEmptySeparatorEmptySep: $ =>
      choice(
        // (:[]). [NonEmptySeparatorEmptySep] ::= NonEmptySeparatorEmptySep ;
        $.NonEmptySeparatorEmptySep,
        // (:). [NonEmptySeparatorEmptySep] ::= NonEmptySeparatorEmptySep [NonEmptySeparatorEmptySep] ;
        seq($.NonEmptySeparatorEmptySep, $.list_NonEmptySeparatorEmptySep)
      ),
    NonEmptyTerminatorEmptySep: $ =>
      // NonEmptyTerminatorEmptySep_a. NonEmptyTerminatorEmptySep ::= "a" ;
      "a",
    list_NonEmptyTerminatorEmptySep: $ =>
      choice(
        // (:[]). [NonEmptyTerminatorEmptySep] ::= NonEmptyTerminatorEmptySep ;
        $.NonEmptyTerminatorEmptySep,
        // (:). [NonEmptyTerminatorEmptySep] ::= NonEmptyTerminatorEmptySep [NonEmptyTerminatorEmptySep] ;
        seq($.NonEmptyTerminatorEmptySep, $.list_NonEmptyTerminatorEmptySep)
      ),
  },
});

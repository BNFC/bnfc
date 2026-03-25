
module.exports = grammar({
  name: "basic",
  extras: $ =>[
    /\s/,
    $.token_CommentSingle,
    $.token_CommentMulti,
  ],
  rules: {
    BNFCStart: $ =>
      optional(
        // BNFCStart_Start. BNFCStart ::= Start ;
        $.Start
      ),
    Start: $ =>
      choice(
        // Start1. Start ::= MyIdent Start ;
        seq($.token_MyIdent, optional($.Start)),
        // Start2. Start ::= ;
        choice()
      ),
    token_MyIdent: $ =>
      /a+/,
    token_CommentSingle: $ =>
      /\/\/[^\r\n]*/,
    token_CommentMulti: $ =>
      /\/\*[^*]*\*([^\*\/][^*]*\*|\*)*\//,
  },
});

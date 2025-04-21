final case class Token(
  // consider merging with TokenType
  // to make a proper ADT later
  tokenType: TokenType,
  lexeme: String,
  literal: Boolean | Double | String | Null,
  line: Int,
) {
  override def toString: String = s"$tokenType $lexeme${if literal == null then "" else s" $literal"}"
}

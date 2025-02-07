final case class Token(
  // consider merging with TokenType
  // to make a proper ADT later
  tokenType: TokenType,
  lexeme: String,
  literal: Any,
  line: Int,
) {
  override def toString: String = s"$tokenType $lexeme $literal"
}

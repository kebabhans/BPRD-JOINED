// Signature file for parser generated by fsyacc
module RePar
type token = 
  | LPAR
  | RPAR
  | CHOICE
  | SPACE
  | STAR
  | EPS
  | CHAR of (char)
type tokenId = 
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_CHOICE
    | TOKEN_SPACE
    | TOKEN_STAR
    | TOKEN_EPS
    | TOKEN_CHAR
    | TOKEN_end_of_input
    | TOKEN_error
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main
    | NONTERM_RegExpr
/// This function maps tokens to integer indexes
val tagOfToken: token -> int

/// This function maps integer indexes to symbolic token ids
val tokenTagToTokenId: int -> tokenId

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
val prodIdxToNonTerminal: int -> nonTerminalId

/// This function gets the name of a token as a string
val token_to_string: token -> string
val Main : (Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> token) -> Microsoft.FSharp.Text.Lexing.LexBuffer<'cty> -> (Absyn.re) 

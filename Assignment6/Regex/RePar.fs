// Implementation file for parser generated by fsyacc
module RePar
#nowarn "64";; // turn off warnings that type variables used in production annotations are instantiated to concrete type
open Microsoft.FSharp.Text.Lexing
open Microsoft.FSharp.Text.Parsing.ParseHelpers
# 1 "RePar.fsy"

 open Absyn;

# 10 "RePar.fs"
// This type is the type of tokens accepted by the parser
type token = 
  | LPAR
  | RPAR
  | CHOICE
  | STAR
  | EOF
  | CHAR of (char)
// This type is used to give symbolic names to token indexes, useful for error messages
type tokenId = 
    | TOKEN_LPAR
    | TOKEN_RPAR
    | TOKEN_CHOICE
    | TOKEN_STAR
    | TOKEN_EOF
    | TOKEN_CHAR
    | TOKEN_end_of_input
    | TOKEN_error
// This type is used to give symbolic names to token indexes, useful for error messages
type nonTerminalId = 
    | NONTERM__startMain
    | NONTERM_Main

// This function maps tokens to integer indexes
let tagOfToken (t:token) = 
  match t with
  | LPAR  -> 0 
  | RPAR  -> 1 
  | CHOICE  -> 2 
  | STAR  -> 3 
  | EOF  -> 4 
  | CHAR _ -> 5 

// This function maps integer indexes to symbolic token ids
let tokenTagToTokenId (tokenIdx:int) = 
  match tokenIdx with
  | 0 -> TOKEN_LPAR 
  | 1 -> TOKEN_RPAR 
  | 2 -> TOKEN_CHOICE 
  | 3 -> TOKEN_STAR 
  | 4 -> TOKEN_EOF 
  | 5 -> TOKEN_CHAR 
  | 8 -> TOKEN_end_of_input
  | 6 -> TOKEN_error
  | _ -> failwith "tokenTagToTokenId: bad token"

/// This function maps production indexes returned in syntax errors to strings representing the non terminal that would be produced by that production
let prodIdxToNonTerminal (prodIdx:int) = 
  match prodIdx with
    | 0 -> NONTERM__startMain 
    | 1 -> NONTERM_Main 
    | _ -> failwith "prodIdxToNonTerminal: bad production index"

let _fsyacc_endOfInputTag = 8 
let _fsyacc_tagOfErrorTerminal = 6

// This function gets the name of a token as a string
let token_to_string (t:token) = 
  match t with 
  | LPAR  -> "LPAR" 
  | RPAR  -> "RPAR" 
  | CHOICE  -> "CHOICE" 
  | STAR  -> "STAR" 
  | EOF  -> "EOF" 
  | CHAR _ -> "CHAR" 

// This function gets the data carried by a token as an object
let _fsyacc_dataOfToken (t:token) = 
  match t with 
  | LPAR  -> (null : System.Object) 
  | RPAR  -> (null : System.Object) 
  | CHOICE  -> (null : System.Object) 
  | STAR  -> (null : System.Object) 
  | EOF  -> (null : System.Object) 
  | CHAR _fsyacc_x -> Microsoft.FSharp.Core.Operators.box _fsyacc_x 
let _fsyacc_gotos = [| 0us; 65535us; 1us; 65535us; 0us; 1us; |]
let _fsyacc_sparseGotoTableRowOffsets = [|0us; 1us; |]
let _fsyacc_stateToProdIdxsTableElements = [| 1us; 0us; 1us; 0us; 1us; 1us; |]
let _fsyacc_stateToProdIdxsTableRowOffsets = [|0us; 2us; 4us; |]
let _fsyacc_action_rows = 3
let _fsyacc_actionTableElements = [|1us; 32768us; 4us; 2us; 0us; 49152us; 0us; 16385us; |]
let _fsyacc_actionTableRowOffsets = [|0us; 2us; 3us; |]
let _fsyacc_reductionSymbolCounts = [|1us; 1us; |]
let _fsyacc_productionToNonTerminalTable = [|0us; 1us; |]
let _fsyacc_immediateActions = [|65535us; 49152us; 16385us; |]
let _fsyacc_reductions ()  =    [| 
# 97 "RePar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            let _1 = (let data = parseState.GetInput(1) in (Microsoft.FSharp.Core.Operators.unbox data : Absyn.re)) in
            Microsoft.FSharp.Core.Operators.box
                (
                   (
                      raise (Microsoft.FSharp.Text.Parsing.Accept(Microsoft.FSharp.Core.Operators.box _1))
                   )
                 : '_startMain));
# 106 "RePar.fs"
        (fun (parseState : Microsoft.FSharp.Text.Parsing.IParseState) ->
            Microsoft.FSharp.Core.Operators.box
                (
                   (
# 17 "RePar.fsy"
                             (* ... *) 
                   )
# 17 "RePar.fsy"
                 : Absyn.re));
|]
# 117 "RePar.fs"
let tables () : Microsoft.FSharp.Text.Parsing.Tables<_> = 
  { reductions= _fsyacc_reductions ();
    endOfInputTag = _fsyacc_endOfInputTag;
    tagOfToken = tagOfToken;
    dataOfToken = _fsyacc_dataOfToken; 
    actionTableElements = _fsyacc_actionTableElements;
    actionTableRowOffsets = _fsyacc_actionTableRowOffsets;
    stateToProdIdxsTableElements = _fsyacc_stateToProdIdxsTableElements;
    stateToProdIdxsTableRowOffsets = _fsyacc_stateToProdIdxsTableRowOffsets;
    reductionSymbolCounts = _fsyacc_reductionSymbolCounts;
    immediateActions = _fsyacc_immediateActions;
    gotos = _fsyacc_gotos;
    sparseGotoTableRowOffsets = _fsyacc_sparseGotoTableRowOffsets;
    tagOfErrorTerminal = _fsyacc_tagOfErrorTerminal;
    parseError = (fun (ctxt:Microsoft.FSharp.Text.Parsing.ParseErrorContext<_>) -> 
                              match parse_error_rich with 
                              | Some f -> f ctxt
                              | None -> parse_error ctxt.Message);
    numTerminals = 9;
    productionToNonTerminalTable = _fsyacc_productionToNonTerminalTable  }
let engine lexer lexbuf startState = (tables ()).Interpret(lexer, lexbuf, startState)
let Main lexer lexbuf : Absyn.re =
    Microsoft.FSharp.Core.Operators.unbox ((tables ()).Interpret(lexer, lexbuf, 0))

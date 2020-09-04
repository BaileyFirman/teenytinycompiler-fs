namespace TennyTiny

open System.Collections.Generic
open TeenyTiny.Types

module Parser =
    let getType token = token.Type

    let next pointer = pointer + 1

    let toString x = x.ToString()

    let parse (tokenStream: Token []) =
        let symbols = new List<string>()
        let labelsDeclared = new List<string>()
        let labelsGotoed = new List<string>()

        let getToken pointer = tokenStream.[pointer]
        let getNextToken pointer = pointer |> next |> getToken

        let rec newline streamPointer =
            printfn "NEWLINE"
            let nextPointer = next streamPointer
            let nextToken = getToken nextPointer

            match nextToken.Type with
            | NEWLINE -> newline nextPointer
            | _ -> nextPointer

        let primary streamPointer =
            let currentToken = getToken streamPointer
            printfn "PRIMARY \"%s\"" currentToken.Text

            match currentToken.Type with
            | NUMBER
            | IDENT -> next streamPointer
            | _ -> failwith ("UNEXPECTED TOKEN" + currentToken.Text)

        let unary streamPointer =
            printfn "UNARY"
            let optionalToken = getToken streamPointer

            let newOffset =
                match optionalToken.Type with
                | PLUS
                | MINUS -> next streamPointer
                | _ -> streamPointer

            primary newOffset

        let term streamPointer =
            printfn "TERM"
            let unaryOffset = unary streamPointer

            let rec unaryLoop pointer =
                let currentToken = getToken pointer
                match currentToken.Type with
                | ASTERISK
                | SLASH ->
                    let nextPointer = next pointer
                    let nextUnaryOffset = unary nextPointer
                    unaryLoop nextUnaryOffset
                | _ -> pointer

            unaryLoop unaryOffset

        let expression streamPointer =
            printfn "EXPRESSION"
            let termOffset = term streamPointer

            let rec termLoop pointer =
                let currentToken = getToken pointer
                match currentToken.Type with
                | PLUS
                | MINUS ->
                    let nextPointer = next pointer
                    let nextTermOffset = term nextPointer
                    termLoop nextTermOffset
                | _ -> pointer

            termLoop termOffset

        let printStatement streamPointer =
            printfn "STATEMENT-PRINT"
            let nextPointer = next streamPointer
            let nextToken = getToken nextPointer

            let printPointer =
                match nextToken.Type with
                | STRING -> next nextPointer
                | _ ->
                    let pointer = expression nextPointer
                    pointer

            newline printPointer

        let comparison streamPointer =
            printfn "COMPARISON"
            let operatorPointer = expression streamPointer
            let operatorToken = getToken operatorPointer
            let operatorTokenType = getType operatorToken

            let nextPointer =
                match isTypeComparisonOperator operatorTokenType with
                | true ->
                    let expressionPointer = next operatorPointer
                    expression expressionPointer
                | false -> failwith "EXPECTED A COMPARISION OPERATOR"

            let rec additionalComparisonLoop pointer =
                let nextToken = getToken pointer
                let nextTokenType = getType nextToken
                match isTypeComparisonOperator nextTokenType with
                | true ->
                    pointer
                    |> next
                    |> expression
                    |> additionalComparisonLoop
                | _ -> pointer

            additionalComparisonLoop nextPointer

        let matchThen streamPointer =
            printfn "THEN"
            next streamPointer

        let matchRepeat streamPointer =
            printfn "REPEAT"
            next streamPointer

        let labelStatement streamPointer =
            printfn "STATEMENT-LABEL"
            let nextPointer = next streamPointer
            let identityToken = getToken nextPointer

            match labelsDeclared.Contains identityToken.Text with
            | true ->
                failwith
                <| "Label already exists: "
                + identityToken.Text
            | false -> labelsDeclared.Add identityToken.Text

            let newlinePointer =
                match identityToken.Type with
                | IDENT -> next nextPointer
                | _ -> failwith "EXPECTED IDENTIFIER"

            newline newlinePointer

        let letStatement streamPointer =
            printfn "STATEMENT-LET"
            let nextPointer = next streamPointer
            let identityToken = getToken nextPointer

            match symbols.Contains identityToken.Text with
            | true -> ()
            | false -> labelsDeclared.Add identityToken.Text

            let assignmentPointer =
                match identityToken.Type with
                | IDENT -> next nextPointer
                | _ -> failwith "EXPECTED IDENTIFIER"

            let expressionToken = getToken assignmentPointer

            let expressionPointer =
                match expressionToken.Type with
                | EQ -> next assignmentPointer
                | _ -> failwith "EXPECTED EQ"

            let newlintPointer = expression expressionPointer
            newline newlintPointer

        let gotoStatement streamPointer =
            printfn "STATEMENT-GOTO"
            let nextPointer = next streamPointer
            let identityToken = getToken nextPointer

            labelsDeclared.Add(identityToken.Text)

            match identityToken.Type with
            | IDENT -> next nextPointer
            | _ -> failwith "EXPECTED IDENTIFIER"

        let inputStatement streamPointer =
            printfn "STATEMENT-INPUT"
            let nextPointer = next streamPointer
            let identityToken = getToken nextPointer

            match symbols.Contains identityToken.Text with
            | true -> ()
            | false -> labelsDeclared.Add identityToken.Text

            let newlinePointer =
                match identityToken.Type with
                | IDENT -> next nextPointer
                | _ -> failwith "EXPECTED IDENTIFIER"

            newline newlinePointer

        let rec ifStatement streamPointer =
            printfn "STATEMENT-IF"
            let nextPointer = next streamPointer
            let comparisonPointer = comparison nextPointer
            let comparisonToken = getToken comparisonPointer

            let thenPointer =
                match comparisonToken.Type with
                | THEN -> matchThen comparisonPointer
                | _ -> failwith "EXPECTED THEN"

            let newlinePointer = newline thenPointer

            let rec ifLoop pointer =
                let currentToken = getToken pointer

                match currentToken.Type with
                | ENDIF -> pointer
                | _ -> pointer |> parseStatement |> ifLoop

            let statementPointer = ifLoop newlinePointer

            let endifToken = getToken statementPointer

            match endifToken.Type with
            | ENDIF -> next statementPointer
            | _ -> failwith "EXPECTED ENDIF"

        and whileStatement streamPointer =
            printfn "STATEMENT-WHILE"
            let nextPointer = next streamPointer
            let comparisonPointer = comparison nextPointer
            let comparisonToken = getToken comparisonPointer

            let thenPointer =
                match comparisonToken.Type with
                | REPEAT -> matchRepeat comparisonPointer
                | _ -> failwith "EXPECTED REPEAT"

            let newlinePointer = newline thenPointer

            let rec whileLoop pointer =
                let currentToken = getToken pointer

                match currentToken.Type with
                | ENDWHILE -> pointer
                | _ -> pointer |> parseStatement |> whileLoop

            let statementPointer = whileLoop newlinePointer

            let endwhileToken = getToken statementPointer

            match endwhileToken.Type with
            | ENDWHILE -> next statementPointer
            | _ -> failwith "EXPECTED ENDWHILE"

        and parseStatement streamPointer =
            let currentToken = getToken streamPointer

            let statementFunction =
                match currentToken.Type with
                | PRINT -> printStatement
                | IF -> ifStatement
                | WHILE -> whileStatement
                | LABEL -> labelStatement
                | GOTO -> gotoStatement
                | LET -> letStatement
                | INPUT -> inputStatement
                | NEWLINE -> newline
                | _ ->
                    failwith
                    <| "NOT IMPLEMENTED: "
                    + toString currentToken.Type

            statementFunction streamPointer

        let rec parseLoop streamPointer =
            let currentToken = getToken streamPointer

            match currentToken.Type with
            | EOF -> ()
            | _ -> streamPointer |> parseStatement |> parseLoop

        printfn "Teeny Tiny Compiler"
        parseLoop 0
        labelsGotoed
        |> Seq.forall (fun x ->
            match labelsDeclared.Contains x with
            | true -> true
            | _ ->
                failwith
                <| "Attempting to GOTO to undeclared label: "
                + x)
        |> ignore
        printfn "Parsing completed"

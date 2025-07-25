module Parser where

import           Text.ParserCombinators.Parsec
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST


-- Analizador de Tokens
sca :: TokenParser u
sca = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "then", "else", "not"
                         "alive", "dead"
                         "topleft", "top", "topright",
                         "left", "self", "right",
                         "bottomleft", "bottom", "bottomright"]
    , reservedOpNames = [ "+"
                        , "-"
                        , "*"
                        , "/"
                        , "<"
                        , ">"
                        , "&&"
                        , "||"
                        , "!"
                        , "="
                        , "=="
                        , "!="
                        , ";"
                        , ","
                        , "++"
                        , "--"
                        ]
    }
  )



-----------------------------------
--- Parser de expresiones enteras
-----------------------------------

intexp :: Parser (Exp Int)
intexp = chainl1 termParser addMinusOp

addMinusOp :: Parser (Exp Int -> Exp Int -> Exp Int)
addMinusOp = try ( do { reservedOp sca "+" ; return Plus } ) <|>
                   do { reservedOp sca "-" ; return Minus }


termParser :: Parser (Exp Int)
termParser = chainl1 factorParser mulDivOp

mulDivOp :: Parser (Exp Int -> Exp Int -> Exp Int)
mulDivOp = try ( do { reservedOp sca "*" ; return Times } ) <|>
                 do { reservedOp sca "/" ; return Div }
                 

factorParser :: Parser (Exp Int)
factorParser =  try (do reservedOp sca "-"
                        f <- factorParser
                        return (UMinus f))
                <|> atomParser

atomParser :: Parser (Exp Int)
atomParser = try countParser
         <|> try (parens sca intexp)
         <|> (do { n <- natural sca ; return (Const (fromInteger n))})

countParser :: Parser (Exp Int)
countParser = do
    reserved sca "count"   
    e <- stateexp         
    return (Count e)


------------------------------------
--- Parser de expresiones booleanas
------------------------------------

boolexp :: Parser (Exp Bool)
boolexp = chainl1 orTermParser ( do { reservedOp sca "||" ; return Or } )

orTermParser :: Parser (Exp Bool)
orTermParser = chainl1 andTermParser ( do { reservedOp sca "&&" ; return And } )

andTermParser :: Parser (Exp Bool)
andTermParser = try (do reservedOp sca "!"
                        ap <- andTermParser
                        return (Not ap))
                
                <|> atomBoolParser

atomBoolParser :: Parser (Exp Bool)
atomBoolParser = (try (parens sca boolexp))                                                              <|> 
                 (try ( do { e1 <- intexp ; reservedOp sca "==" ; e2 <- intexp ; return (Eq e1 e2)  } )) <|>
                 (try ( do { e1 <- intexp ; reservedOp sca "!=" ; e2 <- intexp ; return (NEq e1 e2) } )) <|>
                 (try ( do { e1 <- intexp ; reservedOp sca "<"  ; e2 <- intexp ; return (Lt e1 e2)  } )) <|>
                 (try ( do { e1 <- intexp ; reservedOp sca ">"  ; e2 <- intexp ; return (Gt e1 e2)  } )) <|>
                 (try ( do { reserved sca "false" ; return BFalse } ))                                   <|>
                 (try ( do { reserved sca "true"  ; return BTrue  } ))                                   <|>
                 (try isNeighborParser)                                                                    

isNeighborParser :: Parser (Exp Bool)
isNeighborParser = do n <- neighborParser
              reserved sca "is"
              s <- stateexp
              return (IsNeighbor n s)


------------------------------------------
--- Parser de expresiones probabilisticas
------------------------------------------

stateexp :: Parser (Exp State)
stateexp = 
    try aliveDeadParser
    <|> try ifThenElseParser
    <|> try notParser
    <|> try withProbabilityParser
    <|> parens sca stateexp

-- Estados basicos
aliveDeadParser :: Parser (Exp State)
aliveDeadParser = 
    (reserved sca "alive" >> return Alive)
    <|> 
    (reserved sca "dead" >> return Dead)

-- Expresion IfThenElse para estados.
ifThenElseParser :: Parser (Exp State)
ifThenElseParser = do
    reserved sca "if"
    cond <- boolexp
    reserved sca "then"
    thenExp <- stateexp
    reserved sca "else"
    elseExp <- stateexp
    return (SIfThenElse cond thenExp elseExp)

-- Negacion de estados
notParser :: Parser (Exp State)
notParser = do
    reserved sca "not"
    s <- stateexp
    return (SNot s)

-- Expresion de estado probabilistica
withProbabilityParser :: Parser (Exp State)
withProbabilityParser = do
    reserved sca "withProbability"
    p <- probParser
    reserved sca "do"
    thenExp <- stateexp
    reserved sca "else"
    elseExp <- stateexp
    return (WithProbability p thenExp elseExp)

-- Parser de probabilidad; permitimos que introduzca un numero natural como porcentaje o un decimal.
-- Obs. que ese numero natural debe ser una constante y no una expresion entera. 
-- Tampoco chequeamos aca que el valor de p tenga sentido, simplemente parseamos. 
probParser :: Parser Double
probParser = do try percentageParser <|> decimalParser
  where
    percentageParser  = do  n <- natural sca  
                            optional (symbol sca "%")
                            return (fromInteger n / 100)
    
    decimalParser     = do  n <- float sca    
                            return n      

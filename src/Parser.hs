module Parser where

import           Text.ParserCombinators.Parsec hiding (State) -- Por mi tipo State
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import Prelude 

-- Analizador de Tokens
sca :: TokenParser u
sca = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "then", "else", "not", "is",
                         "withProbability", "do", "random", "count",
                         "defineGrid", "defineNeighborhood", "defineStep", "defineLayout",
                         "alive", "dead",
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
                        , ","
                        ]
    }
  )

-----------------------------------
--- Parser del programa
-----------------------------------

programParser :: Parser Program
programParser = do
    size    <- gridSizeParser
    neigh   <- neighborhoodParser
    step    <- stepParser
    layout  <- layoutParser
    eof 
    return (Program size neigh step layout)

gridSizeParser :: Parser (Int, Int)
gridSizeParser = do
    reserved sca "defineGridSize"
    w <- natural sca
    h <- natural sca
    return (fromInteger w, fromInteger h)

neighborhoodParser :: Parser Neighborhood
neighborhoodParser = do
    reserved sca "defineNeighborhood"
    choice [ reserved sca "moore" >> return Moore
           , reserved sca "vonNeumann" >> return VonNeumann
           ]

stepParser :: Parser Step
stepParser = do reserved sca "defineStep"
                s <- stateexp
                return s

-- Es una lista de las posiciones que arrancaran Alive
layoutParser :: Parser Layout
layoutParser = do reserved sca "defineLayout"
                  pairs <- brackets sca pairList
                  return pairs

pairList :: Parser [Position]
pairList = sepBy1 pair (comma sca)  -- Al menos un par separados por coma

pair :: Parser (Int, Int)
pair = parens sca $ do
    x <- natural sca
    comma sca
    y <- natural sca
    return (fromInteger x, fromInteger y)

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

neighborParser :: Parser Neighbor
neighborParser = 
      (reserved sca "topLeft"     >> return TopLeft)
  <|> (reserved sca "topRight"    >> return TopRight)
  <|> (reserved sca "top"         >> return Top)
  <|> (reserved sca "left"        >> return LeftNeigh)
  <|> (reserved sca "self"        >> return Self)
  <|> (reserved sca "right"       >> return RightNeigh)
  <|> (reserved sca "bottomLeft"  >> return BottomLeft)
  <|> (reserved sca "bottomRight" >> return BottomRight)
  <|> (reserved sca "bottom"      >> return Bottom)

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
    (reserved sca "alive" >> return EAlive)
    <|> 
    (reserved sca "dead" >> return EDead)

-- Expresion IfThenElse para estados.
ifThenElseParser :: Parser (Exp State)
ifThenElseParser = do
    reserved sca "if"
    cond <- boolexp
    reserved sca "then"
    thenExp <- stateexp
    reserved sca "else"
    elseExp <- stateexp
    return (IfThenElse cond thenExp elseExp)

-- Negacion de estados
notParser :: Parser (Exp State)
notParser = do
    reserved sca "not"
    s <- stateexp
    return (NotState s)

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
probParser :: Parser Probability
probParser = try percentageParser <|>
             try decimalParser <|>
             try randomParser
  where
    percentageParser  = do  n <- natural sca  
                            optional (symbol sca "%")
                            return (Prob (fromInteger n / 100))
    
    decimalParser     = do  n <- float sca    
                            return (Prob n)   

    randomParser = do reserved sca "random"
                      return Random   


------------------------------------
-- Función de parseo
------------------------------------

parseComm :: SourceName -> String -> Either ParseError Comm
parseComm = parse (totParser comm)

----------------------------------------------
-- Función para facilitar el testing del parser.
----------------------------------------------

totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace lis
  t <- p
  eof
  return t
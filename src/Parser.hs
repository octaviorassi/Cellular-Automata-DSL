module Parser
  ( parseFile 
  , programParser
  ) where

import           Text.ParserCombinators.Parsec hiding (State) -- Por mi tipo State
import           Text.Parsec.Token
import           Text.Parsec.Language           ( emptyDef )
import           AST
import Prelude 


-------------------------------------------------------------------------------
-- * Analizador de tokens
-------------------------------------------------------------------------------

sca :: TokenParser u
sca = makeTokenParser
  (emptyDef
    { commentStart    = "/*"
    , commentEnd      = "*/"
    , commentLine     = "//"
    , opLetter        = char '='
    , reservedNames   = ["true", "false", "if", "then", "else", "not", "is",
                         "withProbability", "become", "random", "count",
                         "defineGrid", "defineNeighborhood", "defineStep", "defineLayout", "defineSeed",
                         "moore", "vonNeumann",
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
                        , "."
                        ]
    }
  )


-------------------------------------------------------------------------------
-- * Parser del programa
-------------------------------------------------------------------------------

programParser :: Parser Program
programParser = do
    size    <- option defaultGridSize gridSizeParser
    neigh   <- option defaultNeighborhood neighborhoodParser
    layout  <- option defaultLayout layoutParser
    seed    <- option defaultSeed seedParser
    step    <- stepParser  -- No permitimos que la regla sea vacia.
    return (Program size neigh step layout seed)

stepParser :: Parser Step
stepParser = do reserved sca "defineStep"
                s <- stateexp
                return s
                
gridSizeParser :: Parser (Int, Int)
gridSizeParser = try $ do  
    reserved sca "defineGridSize"
    w <- natural sca
    h <- natural sca
    return (fromInteger w, fromInteger h)

neighborhoodParser :: Parser Neighborhood
neighborhoodParser = try $ do
    reserved sca "defineNeighborhood"
    choice [ reserved sca "moore" >> return Moore
           , reserved sca "vonNeumann" >> return VonNeumann
           ]

seedParser :: Parser Int
seedParser = try $ do
    reserved sca "defineSeed"
    s <- natural sca
    return (fromInteger s)

layoutParser :: Parser Layout
layoutParser = try $ do
    reserved sca "defineLayout"
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


-------------------------------------------------------------------------------
-- * Valores por defecto
-------------------------------------------------------------------------------

defaultGridSize :: (Int, Int)
defaultGridSize = (25, 25)  

defaultNeighborhood :: Neighborhood
defaultNeighborhood = Moore 

defaultSeed :: Int
defaultSeed = 42  

defaultLayout :: Layout
defaultLayout = []


-------------------------------------------------------------------------------
-- * Parser de expresiones numericas
-------------------------------------------------------------------------------

floatexp :: Parser (Exp Double)
floatexp = chainl1 termParser addMinusOp

addMinusOp :: Parser (Exp Double -> Exp Double -> Exp Double)
addMinusOp = try ( do { reservedOp sca "+" ; return Plus } ) <|>
                   do { reservedOp sca "-" ; return Minus }


termParser :: Parser (Exp Double)
termParser = chainl1 factorParser mulDivOp

mulDivOp :: Parser (Exp Double -> Exp Double -> Exp Double)
mulDivOp = try ( do { reservedOp sca "*" ; return Times } ) <|>
                 do { reservedOp sca "/" ; return Div }
                 

factorParser :: Parser (Exp Double)
factorParser =  try (do reservedOp sca "-"
                        f <- factorParser
                        return (UMinus f))
                <|> atomParser


atomParser :: Parser (Exp Double)
atomParser = try countParser
         <|> try (parens sca floatexp)
         <|> numericLiteral  
  where
    numericLiteral = do
        n <- try floatNumber <|> naturalNumber
        return (Const n)
    
    floatNumber = float sca 

    naturalNumber = do
        i <- natural sca
        return (fromIntegral i)  

countParser :: Parser (Exp Double)
countParser = do
    reserved sca "count"   
    e <- stateexp         
    return (Count e)



-------------------------------------------------------------------------------
-- * Parser de expresiones booleanas
-------------------------------------------------------------------------------

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
                 (try ( do { e1 <- floatexp ; reservedOp sca "==" ; e2 <- floatexp ; return (Eq e1 e2)  } )) <|>
                 (try ( do { e1 <- floatexp ; reservedOp sca "!=" ; e2 <- floatexp ; return (NEq e1 e2) } )) <|>
                 (try ( do { e1 <- floatexp ; reservedOp sca "<"  ; e2 <- floatexp ; return (Lt e1 e2)  } )) <|>
                 (try ( do { e1 <- floatexp ; reservedOp sca ">"  ; e2 <- floatexp ; return (Gt e1 e2)  } )) <|>
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


-------------------------------------------------------------------------------
-- * Parser de expresiones de estado
-------------------------------------------------------------------------------

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
    reserved sca "become"
    thenExp <- stateexp
    reserved sca "else"
    elseExp <- stateexp
    return (WithProbability p thenExp elseExp)

probParser :: Parser Probability
probParser = try arithmeticProbParser <|>
             try percentageParser <|>
             try decimalParser <|>
             try randomParser <|>
             parens sca probParser
  where
    arithmeticProbParser = do
        expr <- parens sca floatexp  
        return (Prob expr) 

    percentageParser  = do  n <- natural sca  
                            optional (symbol sca "%")
                            return (Prob (Const (fromInteger n / 100))) 
    
    decimalParser     = do  n <- float sca    
                            return (Prob (Const n))   

    randomParser = do reserved sca "random"
                      return Random   



-------------------------------------------------------------------------------
-- * Funcion de parseo
-------------------------------------------------------------------------------

parseFile :: String -> Either ParseError Program
parseFile input = parse (totParser programParser) "" input

totParser :: Parser a -> Parser a
totParser p = do
  whiteSpace sca
  t <- p
  eof
  return t
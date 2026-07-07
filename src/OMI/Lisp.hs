{-# LANGUAGE NoImplicitPrelude #-}

module OMI.Lisp where

import OMI.Kernel

data Bool = Fls | Tru

ite :: Bool -> a -> a -> a
ite Tru x _ = x
ite Fls _ x = x

notB :: Bool -> Bool
notB Fls = Tru
notB Tru = Fls

orB :: Bool -> Bool -> Bool
orB Tru _ = Tru
orB _ Tru = Tru
orB _ _ = Fls

andB :: Bool -> Bool -> Bool
andB Tru Tru = Tru
andB _ _ = Fls

--
-- S-expression AST
--

data SExpr = SNil | SSym [Byte] | SStr [Byte] | SCons SExpr SExpr

--
-- Decision table AST (sugar over SCons)
--

data DecisionTable = DecisionTable
  { dtName    :: SExpr
  , dtInputs  :: [SExpr]
  , dtOutput  :: SExpr
  , dtOperator :: SExpr
  , dtRules   :: [(SExpr, SExpr)]
  }

--
-- Keyword byte sequences for decision-table recognition
--

dtTag :: [Byte]
dtTag = decisionTableSym

decisionTableSym :: [Byte]
decisionTableSym = [mkByte O I I O O I O O,  -- d
                    mkByte O I I O O I O I,  -- e
                    mkByte O I I O O O I I,  -- c
                    mkByte O I I O I O I O,  -- i
                    mkByte O I I O I O O O,  -- s
                    mkByte O I I O I O I O,  -- i
                    mkByte O I I O O O I O,  -- n
                    mkByte O O I O I I O I,  -- -
                    mkByte O I I O I O O O,  -- t
                    mkByte O I I O O I O O,  -- a
                    mkByte O I I O O I O I,  -- b
                    mkByte O I I O O I O I,  -- e
                    mkByte O I I O I I O O]  -- l

nameTag :: [Byte]
nameTag = [mkByte O I I O I I O O,  -- n
           mkByte O I I O O I O O,  -- a
           mkByte O I I O I I O O,  -- m
           mkByte O I I O O I O I]  -- e

inputsTag :: [Byte]
inputsTag = [mkByte O I I O I O I O,  -- i
             mkByte O I I O I I O O,  -- n
             mkByte O I I O I O O O,  -- p
             mkByte O I I O I O I O,  -- u
             mkByte O I I O I O O O,  -- t
             mkByte O I I O O I O O]  -- s

outputTag :: [Byte]
outputTag = [mkByte O I I O I I O O,  -- o
             mkByte O I I O I O I O,  -- u
             mkByte O I I O I O O O,  -- t
             mkByte O I I O I O O O,  -- p
             mkByte O I I O I O I O,  -- u
             mkByte O I I O O I O O]  -- t

operatorTag :: [Byte]
operatorTag = [mkByte O I I O I O I O,  -- t
               mkByte O I I O I I O O,  -- r
               mkByte O I I O I O I O,  -- u
               mkByte O I I O O I O I,  -- t
               mkByte O I I O O I O O,  -- h
               mkByte O O I O I I O I,  -- -
               mkByte O I I O I O O O,  -- o
               mkByte O I I O I O O O,  -- p
               mkByte O I I O I O I O,  -- e
               mkByte O I I O I I O O,  -- r
               mkByte O I I O O I O O,  -- a
               mkByte O I I O O I O I,  -- t
               mkByte O I I O O I O O,  -- b
               mkByte O I I O I O O O]  -- r

rulesTag :: [Byte]
rulesTag = [mkByte O I I O I I O O,  -- r
            mkByte O I I O I O I O,  -- u
            mkByte O I I O I I O O,  -- l
            mkByte O I I O O I O I,  -- e
            mkByte O I I O O I O O]  -- s

--
-- Recognize and extract a decision table from an SExpr
--

isDecisionTable :: SExpr -> Bool
isDecisionTable (SCons (SSym sym) _) = eqByteList sym dtTag
isDecisionTable _ = Fls

asDecisionTable :: SExpr -> DecisionTable
asDecisionTable (SCons _ body) = extractDT body
asDecisionTable _ = DecisionTable SNil [] SNil SNil []

extractDT :: SExpr -> DecisionTable
extractDT SNil = DecisionTable SNil [] SNil SNil []
extractDT (SCons (SCons (SSym k) v) rest) =
  let dt = extractDT rest
  in ite (eqByteList k nameTag)
     (dt { dtName = v })
  (ite (eqByteList k inputsTag)
     (dt { dtInputs = listToExprs v })
  (ite (eqByteList k outputTag)
     (dt { dtOutput = v })
  (ite (eqByteList k operatorTag)
     (dt { dtOperator = v })
  (ite (eqByteList k rulesTag)
     (dt { dtRules = extractRules v })
  dt))))
extractDT _ = DecisionTable SNil [] SNil SNil []

extractRules :: SExpr -> [(SExpr, SExpr)]
extractRules SNil = []
extractRules (SCons (SCons cond act) rest) = (cond, act) : extractRules rest
extractRules _ = []

listToExprs :: SExpr -> [SExpr]
listToExprs SNil = []
listToExprs (SCons x xs) = x : listToExprs xs
listToExprs _ = []

--
-- Rule and RuleSet types
--

data Rule = Rule
  { ruleQuestion   :: Byte
  , ruleAlgorithm  :: Byte
  , ruleSlot       :: Byte
  , ruleProofForm  :: Byte
  , ruleOperator   :: Byte
  , ruleTruthCode  :: [Byte]
  , ruleGate       :: [Byte]
  }

data RuleSet = RuleSet
  { rsName    :: [Byte]
  , rsRules   :: [Rule]
  , rsGauge   :: Byte
  , rsReceipt :: [Byte]
  }

emptyRule :: Rule
emptyRule = Rule byteNull byteNull byteNull byteNull byteNull [] []

emptyRuleSet :: RuleSet
emptyRuleSet = RuleSet [] [] byteNull []

--
-- Gauge family: F* Wittgenstein operator dialect
--

gaugeF0 :: Byte
gaugeF0 = mkByte I I I I O O O O  -- 0xF0
gaugeF1 :: Byte
gaugeF1 = mkByte I I I I O O O I  -- 0xF1
gaugeF2 :: Byte
gaugeF2 = mkByte I I I I O O I O  -- 0xF2
gaugeF3 :: Byte
gaugeF3 = mkByte I I I I O O I I  -- 0xF3
gaugeF4 :: Byte
gaugeF4 = mkByte I I I I O I O O  -- 0xF4
gaugeF5 :: Byte
gaugeF5 = mkByte I I I I O I O I  -- 0xF5
gaugeF6 :: Byte
gaugeF6 = mkByte I I I I O I I O  -- 0xF6
gaugeF7 :: Byte
gaugeF7 = mkByte I I I I O I I I  -- 0xF7
gaugeF8 :: Byte
gaugeF8 = mkByte I I I I I O O O  -- 0xF8
gaugeF9 :: Byte
gaugeF9 = mkByte I I I I I O O I  -- 0xF9
gaugeFA :: Byte
gaugeFA = mkByte I I I I I O I O  -- 0xFA
gaugeFB :: Byte
gaugeFB = mkByte I I I I I O I I  -- 0xFB
gaugeFC :: Byte
gaugeFC = mkByte I I I I I I O O  -- 0xFC
gaugeFD :: Byte
gaugeFD = mkByte I I I I I I O I  -- 0xFD
gaugeFE :: Byte
gaugeFE = mkByte I I I I I I I O  -- 0xFE
gaugeFF :: Byte
gaugeFF = mkByte I I I I I I I I  -- 0xFF

--
-- Gauge pre-header: 8-byte constant
--

gaugeNul :: Byte
gaugeNul = mkByte O O O O O O O O  -- 0x00

gaugeFs :: Byte
gaugeFs = mkByte O O O I I I O O  -- 0x1C

gaugeGs :: Byte
gaugeGs = mkByte O O O I I I O I  -- 0x1D

gaugeRs :: Byte
gaugeRs = mkByte O O O I I I I O  -- 0x1E

gaugeUs :: Byte
gaugeUs = mkByte O O O I I I I I  -- 0x1F

gaugeSp :: Byte
gaugeSp = mkByte O O I O O O O O  -- 0x20

gaugePreHeader :: [Byte]
gaugePreHeader = [gaugeFF, gaugeNul, gaugeFs, gaugeGs, gaugeRs, gaugeUs, gaugeSp, gaugeFF]

--
-- Check if a byte is in the F* gauge family
--

isGaugeByte :: Byte -> Bool
isGaugeByte b = andB (geByte b gaugeF0) (leByte b gaugeFF)

gaugeLowNibble :: Byte -> Nibble
gaugeLowNibble (B _ n) = n

--
-- Match an 8-byte gauge pre-header
--

matchGaugePreHeader :: [Byte] -> Bool
matchGaugePreHeader (g:nul:fs:gs:rs:us:sp:g':_) =
  andB (andB (eqByte g g')
             (eqByte nul gaugeNul))
       (andB (andB (eqByte fs gaugeFs) (eqByte gs gaugeGs))
             (andB (andB (eqByte rs gaugeRs) (eqByte us gaugeUs))
                   (eqByte sp gaugeSp)))
matchGaugePreHeader _ = Fls

--
-- Wittgenstein operator: gauge low nibble → operator code (0x00–0x0F)
--

wittOperatorCode :: Byte -> Nibble
wittOperatorCode b = gaugeLowNibble b

wittIsContradiction :: Byte -> Bool
wittIsContradiction b = eqNibble (wittOperatorCode b) (N O O O O)

wittIsTautology :: Byte -> Bool
wittIsTautology b = eqNibble (wittOperatorCode b) (N I I I I)

--
-- The truth vector for a Wittgenstein operator is the binary representation
-- of the low nibble's bits, where 0 = false (F) and 1 = true (T).
-- This is a direct identity: the low nibble IS the truth vector.
--

wittTruthNibble :: Byte -> Nibble
wittTruthNibble b = gaugeLowNibble b

wittTruthBits :: Byte -> (Bit, Bit, Bit, Bit)
wittTruthBits b =
  let N a b' c d = gaugeLowNibble b
  in (a, b', c, d)

--
-- Convert a gauge byte to its canonical operator code byte (0x00–0x0F)
--

gaugeToCodeByte :: Byte -> Byte
gaugeToCodeByte b = let N a b' c d = gaugeLowNibble b
                    in mkByte O O O O a b' c d

--
-- Build a RuleSet from a decision table
--

ruleSetFromDT :: DecisionTable -> RuleSet
ruleSetFromDT dt =
  RuleSet (case dtName dt of SSym s -> s; _ -> [])
          []
          (case dtOperator dt of SSym (s:_) -> s; _ -> gaugeFF)
          []

gaugeFromCode :: Nibble -> Byte
gaugeFromCode (N a b c d) = B (N I I I I) (N a b c d)

--
-- Byte-list equality
--

eqByteList :: [Byte] -> [Byte] -> Bool
eqByteList [] [] = Tru
eqByteList (x:xs) (y:ys) = andB (eqByte x y) (eqByteList xs ys)
eqByteList _ _ = Fls

--
-- Token types
--

data Token = TokLParen | TokRParen | TokDot | TokSym [Byte] | TokStr [Byte] | TokEOF

--
-- Byte construction and constants
--

mkByte :: Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Bit -> Byte
mkByte a b c d e f g h = B (N a b c d) (N e f g h)

byteNull :: Byte
byteNull = mkByte O O O O O O O O

byteTab :: Byte
byteTab = mkByte O O O O I O O I

byteLF :: Byte
byteLF = mkByte O O O O I O I O

byteCR :: Byte
byteCR = mkByte O O O O I I O I

byteSpace :: Byte
byteSpace = mkByte O O I O O O O O

byteLP :: Byte
byteLP = mkByte O O I O I O O O

byteRP :: Byte
byteRP = mkByte O O I O I O O I

byteDot :: Byte
byteDot = mkByte O O I O I I I O

byteSemi :: Byte
byteSemi = mkByte O O I I I O I I

byteQuote :: Byte
byteQuote = mkByte O O I O O O I O

byte0 :: Byte
byte0 = mkByte O O I I O O O O

byte9 :: Byte
byte9 = mkByte O O I I I O O I

byteA :: Byte
byteA = mkByte O I O O O O O I

byteZ :: Byte
byteZ = mkByte O I O I I O I O

byteUsc :: Byte
byteUsc = mkByte O I O I I I I I

byteMinus :: Byte
byteMinus = mkByte O O I O I I O I

bytePlus :: Byte
bytePlus = mkByte O O I O I O I I

byteStar :: Byte
byteStar = mkByte O O I O I O I O

byteSlash :: Byte
byteSlash = mkByte O O I O I I I I

byte_a :: Byte
byte_a = mkByte O I I O O O O I

byte_z :: Byte
byte_z = mkByte O I I I I O I O

--
-- Byte comparison
--

eqBit :: Bit -> Bit -> Bool
eqBit O O = Tru
eqBit I I = Tru
eqBit _ _ = Fls

eqNibble :: Nibble -> Nibble -> Bool
eqNibble (N a b c d) (N e f g h) =
  andB (andB (eqBit a e) (eqBit b f)) (andB (eqBit c g) (eqBit d h))

eqByte :: Byte -> Byte -> Bool
eqByte (B a b) (B c d) = andB (eqNibble a c) (eqNibble b d)

leByte :: Byte -> Byte -> Bool
leByte (B a b) (B c d) =
  orB (ltNibble a c)
      (andB (eqNibble a c)
            (leNibble b d))

geByte :: Byte -> Byte -> Bool
geByte (B a b) (B c d) =
  orB (gtNibble a c)
      (andB (eqNibble a c)
            (geNibble b d))

gtNibble :: Nibble -> Nibble -> Bool
gtNibble (N a b c d) (N e f g h) =
  orB (gtBit a e)
      (andB (eqBit a e)
            (orB (gtBit b f)
                 (andB (eqBit b f)
                       (orB (gtBit c g)
                            (andB (eqBit c g) (gtBit d h))))))

ltNibble :: Nibble -> Nibble -> Bool
ltNibble (N a b c d) (N e f g h) =
  orB (ltBit a e)
      (andB (eqBit a e)
            (orB (ltBit b f)
                 (andB (eqBit b f)
                       (orB (ltBit c g)
                            (andB (eqBit c g) (ltBit d h))))))

leNibble :: Nibble -> Nibble -> Bool
leNibble x y = orB (ltNibble x y) (eqNibble x y)

geNibble :: Nibble -> Nibble -> Bool
geNibble x y = orB (gtNibble x y) (eqNibble x y)

gtBit :: Bit -> Bit -> Bool
gtBit I O = Tru
gtBit _ _ = Fls

ltBit :: Bit -> Bit -> Bool
ltBit O I = Tru
ltBit _ _ = Fls

--
-- Character classification
--

isWhitespace :: Byte -> Bool
isWhitespace b = orB (orB (eqByte b byteSpace) (eqByte b byteTab))
                     (orB (eqByte b byteLF) (eqByte b byteCR))

isDigit :: Byte -> Bool
isDigit b = andB (geByte b byte0) (leByte b byte9)

isUpper :: Byte -> Bool
isUpper b = andB (geByte b byteA) (leByte b byteZ)

isLower :: Byte -> Bool
isLower b = andB (geByte b byte_a) (leByte b byte_z)

isAlpha :: Byte -> Bool
isAlpha b = orB (isUpper b) (isLower b)

isAlnum :: Byte -> Bool
isAlnum b = orB (isAlpha b) (isDigit b)

isSymChar :: Byte -> Bool
isSymChar b = orB (isAlnum b)
              (orB (orB (eqByte b byteMinus) (eqByte b byteUsc))
                   (orB (eqByte b bytePlus) (eqByte b byteStar)))

--
-- Lexer
--

lexer :: [Byte] -> [Token]
lexer [] = [TokEOF]
lexer (b:bs) =
  ite (isWhitespace b) (lexer bs)
  (ite (eqByte b byteSemi) (skipLine bs)
  (ite (eqByte b byteLP) (TokLParen : lexer bs)
  (ite (eqByte b byteRP) (TokRParen : lexer bs)
  (ite (eqByte b byteDot) (TokDot : lexer bs)
  (ite (eqByte b byteQuote)
    (TokStr (takeStr bs []) : lexer (dropStr bs))
  (ite (isSymChar b)
    (TokSym (takeWhileSym (b:bs)) : lexer (dropWhileSym (b:bs)))
  (lexer bs)))))))

skipLine :: [Byte] -> [Token]
skipLine [] = [TokEOF]
skipLine (b:bs) =
  ite (orB (eqByte b byteLF) (eqByte b byteCR)) (lexer bs)
  (skipLine bs)

takeStr :: [Byte] -> [Byte] -> [Byte]
takeStr [] _ = []
takeStr (b:bs) acc =
  ite (eqByte b byteQuote) acc
  (takeStr bs (snoc acc b))

snoc :: [Byte] -> Byte -> [Byte]
snoc [] x = [x]
snoc (y:ys) x = y : snoc ys x

dropStr :: [Byte] -> [Byte]
dropStr [] = []
dropStr (b:bs) =
  ite (eqByte b byteQuote) bs
  (dropStr bs)

takeWhileSym :: [Byte] -> [Byte]
takeWhileSym [] = []
takeWhileSym (b:bs) =
  ite (isSymChar b) (b : takeWhileSym bs)
  []

dropWhileSym :: [Byte] -> [Byte]
dropWhileSym [] = []
dropWhileSym (b:bs) =
  ite (isSymChar b) (dropWhileSym bs)
  (b : bs)

--
-- Parser
--

parse :: [Token] -> [SExpr]
parse [] = []
parse (TokEOF:_) = []
parse ts =
  case parseExpr ts of
    (expr, TokEOF:_) -> [expr]
    (expr, rest) -> expr : parse rest

parseExpr :: [Token] -> (SExpr, [Token])
parseExpr [] = (SNil, [])
parseExpr (TokEOF:rest) = (SNil, TokEOF : rest)
parseExpr (TokSym s:rest) = (SSym s, rest)
parseExpr (TokStr s:rest) = (SStr s, rest)
parseExpr (TokLParen:rest) = parseList rest
parseExpr _ = (SNil, [])

parseList :: [Token] -> (SExpr, [Token])
parseList (TokRParen:rest) = (SNil, rest)
parseList (TokDot:rest) =
  case parseExpr rest of
    (expr, TokRParen:rest') -> (expr, rest')
    (expr, rest') -> (SCons expr SNil, rest')
parseList ts =
  case parseExpr ts of
    (expr, TokDot:rest) ->
      case parseExpr rest of
        (lastExpr, TokRParen:rest') -> (SCons expr lastExpr, rest')
        _ -> (SNil, rest)
    (expr, rest) ->
      case parseList rest of
        (list, rest') -> (SCons expr list, rest')

--
-- Convenience: parse byte sequence directly
--

parseBytes :: [Byte] -> [SExpr]
parseBytes bs = parse (lexer bs)

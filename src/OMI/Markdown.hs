module OMI.Markdown
  ( MarkdownBlockKind(..)
  , EvidenceChecksum
  , EvidenceSpan
  , MarkdownBlock
  , MarkdownExtraction
  , extractMarkdown
  , verifyEvidence
  , markdownBlocks
  , markdownFrontMatter
  , markdownDeclarations
  , markdownCitationCandidates
  , blockKind
  , blockScope
  , blockBody
  , blockEvidence
  , evidenceStartByte
  , evidenceEndByte
  , evidenceStartLine
  , evidenceEndLine
  , evidenceChecksum
  , evidenceIdentity
  , checksumBytes
  ) where

import Data.Char (ord, toLower)
import Data.List (isPrefixOf)
import Prelude

import OMI.Core
import OMI.Kernel
import qualified OMI.Lisp as L
import OMI.Lisp (SExpr, mkByte, parseBytes)
import OMI.Relation (relW16a, relW16b, relW16c, relW16d)
import OMI.Scope

data MarkdownBlockKind =
    FrontMatterBlock
  | OmiLispBlock
  | DecisionTableBlock
  | CanvasBlock
  | EvidenceBlock
  deriving (Eq, Show)

newtype EvidenceChecksum = EvidenceChecksum [Byte]

data EvidenceSpan = EvidenceSpan
  { evidenceStartByte :: Int
  , evidenceEndByte :: Int
  , evidenceStartLine :: Int
  , evidenceEndLine :: Int
  , evidenceChecksum :: EvidenceChecksum
  , evidenceIdentity :: Relation
  }

data MarkdownBlock = MarkdownBlock
  { blockKind :: MarkdownBlockKind
  , blockScope :: OmiScope
  , blockBody :: String
  , blockEvidence :: EvidenceSpan
  }

data MarkdownExtraction = MarkdownExtraction
  { markdownBlocks :: [MarkdownBlock]
  , markdownFrontMatter :: [MarkdownBlock]
  , markdownDeclarations :: [SExpr]
  , markdownCitationCandidates :: [Relation]
  }

extractMarkdown :: String -> MarkdownExtraction
extractMarkdown input =
  let blocks = extractBlocks input
      decls = concatMap declarationsFromBlock blocks
      candidates = map blockRelation blocks
  in MarkdownExtraction blocks (filter isFrontMatter blocks) decls candidates

verifyEvidence :: String -> EvidenceSpan -> Bool
verifyEvidence input ev =
  let body = take (evidenceEndByte ev - evidenceStartByte ev)
           (drop (evidenceStartByte ev) input)
  in sameBytes (checksumBytes (checksumBytesFromString body))
       (checksumBytes (evidenceChecksum ev))

checksumBytes :: EvidenceChecksum -> [Byte]
checksumBytes (EvidenceChecksum bs) = bs

extractBlocks :: String -> [MarkdownBlock]
extractBlocks input =
  let ls = annotateLines input
      front = frontMatterBlock input ls
      fences = fenceBlocks input ls
  in case front of
       Nothing -> fences
       Just block -> block : fences

frontMatterBlock :: String -> [LineInfo] -> Maybe MarkdownBlock
frontMatterBlock input (first:rest)
  | lineText first == "---" =
      case spanUntilFrontMatterClose rest of
        Nothing -> Nothing
        Just (bodyLines, closeLine) ->
          let start = lineStart first
              end = lineEnd closeLine
              body = take (end - start) (drop start input)
          in Just (mkBlock FrontMatterBlock emptyScope input body start end
                    (lineNumber first) (lineNumber closeLine))
frontMatterBlock _ _ = Nothing

spanUntilFrontMatterClose :: [LineInfo] -> Maybe ([LineInfo], LineInfo)
spanUntilFrontMatterClose [] = Nothing
spanUntilFrontMatterClose (x:xs)
  | lineText x == "---" = Just ([], x)
  | otherwise =
      case spanUntilFrontMatterClose xs of
        Nothing -> Nothing
        Just (ys, closeLine) -> Just (x:ys, closeLine)

fenceBlocks :: String -> [LineInfo] -> [MarkdownBlock]
fenceBlocks input = go
  where
    go [] = []
    go (line:rest)
      | "```" `isPrefixOf` lineText line =
          let info = drop 3 (lineText line)
          in case collectFence rest of
               Nothing -> go rest
               Just (bodyLines, closeLine, afterClose) ->
                 let kind = blockKindFromInfo info
                     scope = scopeFromInfo info
                     bodyStart = case bodyLines of
                                   [] -> lineEnd line
                                   (b:_) -> lineStart b
                     bodyEnd = case reverse bodyLines of
                                 [] -> lineEnd line
                                 (b:_) -> lineEnd b
                     body = take (bodyEnd - bodyStart) (drop bodyStart input)
                 in mkBlock kind scope input body bodyStart bodyEnd
                      (lineNumber line) (lineNumber closeLine)
                    : go afterClose
      | otherwise = go rest

collectFence :: [LineInfo] -> Maybe ([LineInfo], LineInfo, [LineInfo])
collectFence [] = Nothing
collectFence (line:rest)
  | "```" `isPrefixOf` lineText line = Just ([], line, rest)
  | otherwise =
      case collectFence rest of
        Nothing -> Nothing
        Just (body, closeLine, afterClose) -> Just (line:body, closeLine, afterClose)

blockKindFromInfo :: String -> MarkdownBlockKind
blockKindFromInfo info =
  case words (map toLower info) of
    ("omi":_) -> OmiLispBlock
    ("omi-lisp":_) -> OmiLispBlock
    ("decision-table":_) -> DecisionTableBlock
    ("canvas":_) -> CanvasBlock
    _ -> EvidenceBlock

scopeFromInfo :: String -> OmiScope
scopeFromInfo info =
  mkScope (field "fs" parts) (field "gs" parts) (field "rs" parts) (field "us" parts)
  where
    parts = words info

field :: String -> [String] -> [Byte]
field key [] = []
field key (part:parts) =
  case break (== '=') part of
    (name, '=':value)
      | map toLower name == key -> stringToBytes value
    _ -> field key parts

mkBlock :: MarkdownBlockKind -> OmiScope -> String -> String -> Int -> Int -> Int -> Int -> MarkdownBlock
mkBlock kind scope _ body start end startLine endLine =
  let ev = EvidenceSpan start end startLine endLine
             (checksumBytesFromString body)
             (blockIdentity kind scope start end startLine endLine)
  in MarkdownBlock kind scope body ev

blockIdentity :: MarkdownBlockKind -> OmiScope -> Int -> Int -> Int -> Int -> Relation
blockIdentity kind scope start end startLine endLine =
  let scopeRel = scopeRelation scope
  in Relation
       (intToWord16 (kindCode kind))
       (intToWord16 startLine)
       (intToWord16 endLine)
       (intToWord16 start)
       (intToWord16 end)
       (relW16a scopeRel)
       (relW16b scopeRel)
       (relW16c scopeRel)
       (W32 (relW16d scopeRel) null16)
       null32
       null32
       null32

blockRelation :: MarkdownBlock -> Relation
blockRelation = evidenceIdentity . blockEvidence

declarationsFromBlock :: MarkdownBlock -> [SExpr]
declarationsFromBlock block =
  case blockKind block of
    OmiLispBlock -> parseBytes (stringToBytes (blockBody block))
    DecisionTableBlock -> parseBytes (stringToBytes (blockBody block))
    _ -> []

isFrontMatter :: MarkdownBlock -> Bool
isFrontMatter block = blockKind block == FrontMatterBlock

checksumBytesFromString :: String -> EvidenceChecksum
checksumBytesFromString input = EvidenceChecksum [intToByte (sum (map ord input) `mod` 256)]

kindByte :: MarkdownBlockKind -> Byte
kindByte = intToByte . kindCode

kindCode :: MarkdownBlockKind -> Int
kindCode FrontMatterBlock = 1
kindCode OmiLispBlock = 2
kindCode DecisionTableBlock = 3
kindCode CanvasBlock = 4
kindCode EvidenceBlock = 5

stringToBytes :: String -> [Byte]
stringToBytes = map (intToByte . ord)

intToByte :: Int -> Byte
intToByte n =
  mkByte
    (bit 7) (bit 6) (bit 5) (bit 4)
    (bit 3) (bit 2) (bit 1) (bit 0)
  where
    bit shift = if ((n `div` pow2 shift) `mod` 2) == 0 then O else I

pow2 :: Int -> Int
pow2 0 = 1
pow2 n = 2 * pow2 (n - 1)

intToWord16 :: Int -> Word16
intToWord16 n = W16 (intToByte ((n `div` 256) `mod` 256)) (intToByte (n `mod` 256))

data LineInfo = LineInfo
  { lineNumber :: Int
  , lineStart :: Int
  , lineEnd :: Int
  , lineText :: String
  }

annotateLines :: String -> [LineInfo]
annotateLines input = go 1 0 (splitLinesKeepingEnd input)

go :: Int -> Int -> [String] -> [LineInfo]
go _ _ [] = []
go n offset (line:rest) =
  let len = length line
      text = trimLineEnd line
  in LineInfo n offset (offset + len) text : go (n + 1) (offset + len) rest

splitLinesKeepingEnd :: String -> [String]
splitLinesKeepingEnd [] = []
splitLinesKeepingEnd xs =
  let (line, rest) = break (== '\n') xs
  in case rest of
       [] -> [line]
       (_:after) -> (line ++ "\n") : splitLinesKeepingEnd after

trimLineEnd :: String -> String
trimLineEnd = reverse . dropWhile isLineEnd . reverse

isLineEnd :: Char -> Bool
isLineEnd '\n' = True
isLineEnd '\r' = True
isLineEnd _ = False

sameBytes :: [Byte] -> [Byte] -> Bool
sameBytes [] [] = True
sameBytes (x:xs) (y:ys) =
  case L.eqByte x y of
    L.Tru -> sameBytes xs ys
    L.Fls -> False
sameBytes _ _ = False

{-
Copyright (C) 2008, Niels Aan de Brugh <nielsadb@gmail.com>
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.
    * Neither the name of the Pandoc Project nor the
      names of its contributors may be used to endorse or promote products
      derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY The Pandoc Project ''AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL The Pandoc Project BE LIABLE FOR ANY
DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-}

{- |
   Module      : Text.Pandoc.Readers.MediaWiki
   Copyright   : Copyright (C) 2008 Niels Aan de Brugh
   License     : BSD-3

   Maintainer  : Niels Aan de Brugh <nielsadb@gmail.com>
   Stability   : pre-alpha
   Portability : portable

Conversion from MediaWiki to 'Pandoc' document.
-}
module Text.Pandoc.Readers.MediaWiki (readMediaWiki, test) where

import Data.List ( intersperse )

import Text.Pandoc.Definition
import Text.Pandoc.Shared 
import Text.ParserCombinators.Parsec

type MWP a = GenParser Char ParserState a

test :: String -> Pandoc
test s = readMediaWiki defaultParserState s

-- | Function to start the parsing process.
readMediaWiki :: ParserState -> String -> Pandoc
readMediaWiki state s = (readWith parseMediaWiki) state (stripTrailingNewLines s)
                      where
                        stripTrailingNewLines s' = reverse $ '\n' : dropWhile (=='\n') (reverse s')



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- * Top Level Parsing Functions

-- | Parse a document which is a list of blocks.
parseMediaWiki :: MWP Pandoc
parseMediaWiki = do
    let meta = Meta [] [] []
    blocks <- many parseBlock
    return $ Pandoc meta $ mergePlain blocks

-- | Parse a Pandoc block.
-- A block can either be a
--   * Header
--   * Horizontal line
--   * Bullet (unordered) list
--   * Numbered (ordered) list
--   * Definition list
--   * Simply a bunch of text
parseBlock :: MWP Block
parseBlock = do
    block <- parseHeader
         <|> parseHorizontalRule
         <|> parseBulletList
         <|> parseOrderedList
         <|> parseDefinitionList
         <|> parsePlain
         <|> parseEmptyBlock
    return block

-- | Parse a header
parseHeader :: MWP Block
parseHeader = do
    openHeader <- (many1 $ char '=') <?> "Start of heading"
    inlines <- parseInlines "="
    let stripped = stripSpaces inlines
    closeHeader <- many1 $ char '='
    let lengthDiff = abs $ length openHeader - length closeHeader
    let level = min (length openHeader) (length closeHeader)
    let text = case compare (length openHeader) (length closeHeader) of
                 LT -> stripped ++ [(Str $ replicate lengthDiff '=')]
                 GT -> [(Str $ replicate lengthDiff '=')] ++ stripped
                 EQ -> stripped
    return $ Header (level - 1) text

-- | Parse a horizontal line
parseHorizontalRule :: MWP Block
parseHorizontalRule = do
    count 4 (char '-') >> skipMany (char '-') <?> "Horizontal rule"
    return HorizontalRule

-- | Parsing an empty block (used as a fall-back to consume newlines)
parseEmptyBlock :: MWP Block
parseEmptyBlock = do
    endOfLine <?> "Line break"
    return $ Plain [LineBreak]

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- ** Parsing (Un)ordered Lists
    
-- | Parse a bullet list (uses parseList)
parseBulletList :: MWP Block
parseBulletList = parseList BulletList '*'

-- | Parse an ordered list (uses parseList)
-- The list attributes of the result are fixed as MediaWiki does not support
-- list continuations or other styles than Decimal.
parseOrderedList :: MWP Block
parseOrderedList = parseList (OrderedList defaultOrderedListAttributes) '#'
                 where
                   defaultOrderedListAttributes = (1, Decimal, Period)

-- | Generic list parsing 
-- The function first constructs a plain list of [Block] content chunks paired
-- with the indent level of each chunk. Next, a nesting function scopes sequences
-- of the same indent and generates a nested Pandoc list structure.
-- Note that this function currently only supports Plain list items.
parseList :: ([[Block]] -> Block) -- ^ Constructor to generate (sub)lists.
            -> Char               -- ^ The character to indicate list items.
            -> MWP Block
parseList ctor itemChar = do
    items <- many1 parseListItem
    return $ ctor $ nestListItems items 1
  where
    -- | Parse string into a series of chunks paired with the indent level.
    parseListItem :: MWP (Int, [Block])
    parseListItem = do
        listSymbols <- (many1 $ char itemChar) <?> "List item"
        content <- parsePlain <|> return Null -- TODO: expand to more things available in lists
        endOfLine -- Consume end of this list item
        return (length listSymbols, [content])

    nestListItems :: [(Int,[Block])] -- ^ Chunks with indent level.
                  -> Int             -- ^ Current (or starting) nesting level.
                  -> [[Block]]       -- ^ Contents for a list at that level of indention.
    nestListItems [] _ = []
    nestListItems entire@((lvl,bs):more) cur
        | lvl == cur = bs : nestListItems more cur
        | lvl < cur = []
        | lvl > cur = let (scope, rest) = span (\(x,_) -> x >= lvl) entire
                       in [ctor $ nestListItems scope (cur+1)] : nestListItems rest cur
    nestListItems ((_,_):_) _ = error "non-expected pattern match" -- Suppress spurious GHC warning.



-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- ** Parsing Definitions

-- | Type to store list of definition list items.
data DefinitionItem = DefIt Int [Inline] | DataIt Int Block
                    deriving (Eq, Show)

-- | Type to store nested definition lists (simpler than Pandoc structure).
data DefinitionTree = Definition [Inline] [DefinitionTree] | Data Block
                    deriving (Eq, Show)

-- | Parse a definition list.
-- This function takes a similar approach as parseList above, but is more
-- complex because there are two types of list items (definitions and mere
-- data elements) that have a semantically difference in nesting.
-- First the text is parsed into a list of definition list elements.
-- Then that list is nested into a tree-like structure. This extra intermediate
-- step is taken because the Pandoc types for definition lists do not allow
-- simple recursive processing of the item list.
-- Finally the tree is converted into a Pandoc definition list.
parseDefinitionList :: MWP Block
parseDefinitionList = do
    items <- many1 (parseNewDefinition <|> parseDefinitionData)
    return $ DefinitionList (map mergeTree $ listToTrees items 1)
  where
    -- | Parse a new definition.
    -- Note that a data element may start on the same line (indicated by a colon).
    parseNewDefinition :: MWP DefinitionItem
    parseNewDefinition = do
        defSymbols <- (many1 $ char ';') <?> "New definition"
        defName <- parseInlines ":\n"
        optional endOfLine
        return $ DefIt (length defSymbols) (stripWhiteSpace defName)

    -- | Parse a data definition element.
    -- Note that this function currently only support Plain content in a definition list.
    parseDefinitionData :: MWP DefinitionItem
    parseDefinitionData = do
        defSymbols <- (many1 $ char ':') <?> "Definition data"
        defContents <- parseInlines "\n"
        endOfLine -- Consume end of this list item
        return $ DataIt (length defSymbols) (Plain $ stripWhiteSpace defContents)

    -- | Pericate to select sub elements in a certain indention level.
    isSubElement :: Int -> DefinitionItem -> Bool
    isSubElement cur (DefIt lvl _)  = cur < lvl
    isSubElement cur (DataIt lvl _) = cur <= lvl

    -- | Transform a list of definition list items plus indention to a proper tree.
    listToTrees :: [DefinitionItem] -> Int -> [DefinitionTree]
    listToTrees [] _ = []
    listToTrees entire@((DefIt lvl name):more) cur
        | lvl == cur   = let (subs, rest) = span (isSubElement cur) more
                          in (Definition name $ listToTrees subs cur) : (listToTrees rest cur)
        | lvl == cur+1 = let (subs, rest) = span (isSubElement cur) entire
                          in (listToTrees subs (cur+1)) ++ (listToTrees rest cur)
        | lvl > cur    = let (subs, rest) = span (isSubElement cur) entire
                          in (Definition [] $ listToTrees subs (cur+1)) : (listToTrees rest cur)
        | lvl < cur     = []
    listToTrees entire@((DataIt lvl block):more) cur
        | lvl == cur = (Data block) : listToTrees more cur
        | lvl > cur  = let (subs, rest) = span (isSubElement (cur+1)) entire
                        in (Definition [] $ listToTrees subs (cur+1)) : (listToTrees rest cur)
        | lvl < cur  = []
    listToTrees ((DefIt _ _) : _) _  = error "Incomplete pattern match"
    listToTrees ((DataIt _ _) : _) _ = error "Incomplete pattern match"

    -- | Transform a tree into the data part of a DefinitionList.
    -- The Pandoc data type DefinitionList is a breadth-first data structure, whereas the
    -- intermediate structure is a depth-first structure.
    mergeTree :: DefinitionTree -> ([Inline], [[Block]])
    mergeTree (Data block)          = ([], [[block]])
    mergeTree (Definition name sub) = (name, [map mergeTree' sub])
         where
            mergeTree' :: DefinitionTree -> Block
            mergeTree' (Data block')           = block'
            mergeTree' (Definition name' sub') = DefinitionList [(name', [map mergeTree' sub'])]
       


-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- ** Parsing Text

-- | Parse a plain text elements, i.e. a series of Inline terminated by a newline.
parsePlain :: MWP Block
parsePlain = do
    inlines <- parseInlines "\n"
    return $ Plain $ stripSpaces inlines

-- | Parse a list of inlines.
-- Again, a multi-stage approach is taken.
-- This function uses a Parsec parser to consume the whole text at once. The
-- text is then processed through a series of functions:
--  1. First bold/italic text parsing is performed (see parseBoldItalic below).
--     This function also processes the list to coarse Inline chunks.
--  2. The coarse chunks are divided further via parseInlineStrings which
--     leaves the Bold/Italic constructors in place, but divides each string
--     via a collection of Parsec parsers.
--  3. A number of parsec parsers are defined to parse spaces, dashes, and other
--     special characters.
parseInlines :: [Char] -> MWP [Inline]
parseInlines delims = do
    rawText <- many1 $ noneOf delims
    let boldItalics = mergeBI $ parseBoldItalic rawText
    let fullyParsed = concatMap parseInlineStrings boldItalics
    return fullyParsed
  where
    -- Merge consecutive strings
    mergeBI [] = []
    mergeBI (x:xs)
     | str x = let (ss,ns) = span str (x:xs)
               in Str (concatMap fromStr ss) : mergeBI ns
     | otherwise = x : mergeBI xs
    fromStr (Str xs) = xs
    fromStr _ = error "fromStr"
    str (Str _) = True
    str _ = False

    -- Divide a coarse chunk up into smaller chunks.
    parseInlineStrings :: Inline -> [Inline]
    parseInlineStrings (Str x)    = case parse parseInlineString "" x of
                                         Left _    -> error $ "Parsing string: " ++ x
                                         Right val -> val
    parseInlineStrings (Emph x)   = [Emph $ concatMap parseInlineStrings x]
    parseInlineStrings (Strong x) = [Strong $ concatMap parseInlineStrings x]    
    parseInlineStrings x          = [x]

    -- Micro-level parsing of a string chunk. Disjunction of the parsers found below.
    parseInlineString :: Parser [Inline]
    parseInlineString = do
        inlines <- many $ choice [parseSpace,
                                  parseImage,
                                  parseLocalLink, parseRemoteLink,
                                  parseEmDash, parseEnDash, parseEllipses,
                                  parseLineBreak, parseApostrophe, parseString]
        return inlines

    specialChars    = " -.\n\'"
    parseSpace      = do { char ' '; return Space }
    parseEmDash     = do { try $ count 3 $ char '-'; return EmDash }
    parseEnDash     = do { try $ count 2 $ char '-'; return EnDash }
    parseEllipses   = do { try $ count 3 $ char '.'; return Ellipses }
    parseLineBreak  = do { char '\n'; return LineBreak }
    parseApostrophe = do { char '\''; return Apostrophe }
    parseString     = (Str . concat) `fmap` many1 parseStringChunk
    parseStringChunk = do { str <- many1 $ noneOf specialChars; return str }
                     <|> do { singleton `fmap` char '-' }
                     <|> do { singleton `fmap` char '.' }

singleton :: a -> [a]
singleton = (: [])

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
-- * Parsing Bold/Italic Text

-- | Type used by the bold/italic lexer (apostrophes are handled separately).
data BIToken = Apos Int | NTxt String

-- | Intermediate output list.
-- Note that this structure is not properly nested: something like the illegal
-- HTML code @<b> <i> </b> <i>@ may be expressed by a list of this type.
type FTxt = (BIMode, String)

-- | Type synonym to indicate bold/italic markup of a text.
-- 0 = None, 2 = Bold, 3 = Italic, 5 = Bold+Italic
-- Note that this corresponds to the number of apostrophes needed to open the mode.
type BIMode = Int

-- | Readable output of the tokens (for debugging).
instance Show BIToken where
    show (Apos n) = "<" ++ show n ++ ">"
    show (NTxt s) = s

-- | Tokenize a string by extracting sequences of at least two apostrophes.
-- The rest of the string is put in string (NTxt) chunks. A single apostrophe is 
-- also put in a string chunk. Two or more are put into an Apos chunk.
-- Note that NTxt and Apos elements don't necessarily strictly alternate, viz.
-- the output @[Apos _, NTxt _, NTxt _, Apos _]@ is possible.
biLexer :: String -> [BIToken]
biLexer "" = []
biLexer str = let (pre,post') = span (/= '\'') str
                  (ac,post)   = takeApos post'
                  aposPart    = listIf (ac > 1) (Apos ac)
                  strPart     = listIf (not $ null pre)
                                       (NTxt $ pre ++ listIf (ac == 1) '\'')
               in strPart ++ aposPart ++ biLexer post
           where
             takeApos :: String -> (Int, String)
             takeApos s = let (pre,post) = span (== '\'') s
                           in (length pre, post)

-- | Parse a list of B/I Tokens into a list of formatted text elements.
-- This function is based on emperical study using the SandBox on www.mediawiki.org.
-- It is not based on the original PHP code or any kind of formal grammar. Some
-- ground rules to make this more comprehensible:
--   * Apostrophes that have no role in the formatting go into the left-most
--     formatted chunk.
--   * Open first, ask question later. For example, the text @'''test@ prints @test@
--     bold, even while it's never properly closed.
--   * If a chunk is properly closed, choose that path. For example @'''text''@ prints
--     @'text@ with @text@ in italics, because italic is the formatting that is properly
--     closed whereas bold is never closed. For the extra apostrophe the first rule
--     applies.
--   * Opening is better than closing. For example @''aap'''noot''@ is parsed as in HTML
--     @<i>aap<b>noot</b></i>@. This may be a more specific case of "open first, ask
--     questions later". Note that adding an extra apostrophe at the beginning would
--     result in this being parsed as @<b>aap</b>noot<i></i>@.
-- I'm not 100% sure this function is always correct. It will require many test cases.
-- If you are in any way unsure about the output, please verify using the SandBox at
-- www.mediawiki.org.
biParser :: BIMode -> [BIToken] -> [FTxt]
biParser = biParse
  where
    biParse _ [] = []
    -- Parse an apostrophe token. This is the complex part.
    biParse current ((Apos n):rest) =
        let mx = maxToCome rest
         in case current of
            -- Fully formatted: we can only go downward.
            5 -> closeMode current n rest
            -- Bold: Two would open Italics, three or more will close Bold.
            3 -> if n >= 3
                    then closeMode current n rest
                    else openMode current 2 n rest
            -- Italic: Two will close Italics, three or more will open Bold if that
            -- scope is ever neatly closed.
            2 -> if n == 2 || mx < 3
                    then closeMode current n rest
                    else openMode current 3 n rest
            -- Unformatted: open all if we have to apostrophes to show for it,
            -- else open bold if it's closed or there are no closers whatsoever,
            -- else open italics.
            0 -> if (n == 3 || n == 4) && (mx == 0 || mx >= 3)
                    then openMode current 3 n rest
                    else if n == 5
                        then openMode current 5 n rest
                        else openMode current 2 n rest
            _ -> error $ "Illegal mode: " ++ show current
    -- Leave strings as is. Check for empty strings has debugging purpose.
    biParse current ((NTxt s):rest) 
        | null s = error "empty string in stream of tokens"
        | otherwise = mkTxt current s : (biParse current rest)

    -- Open a formatting mode, abundant apostrophes become text.
    openMode :: BIMode -> BIMode -> Int -> [BIToken] -> [FTxt]
    openMode current open n rest =
        let extra = n - open
            aposString = listIf (extra > 0) $ makeAposText current extra
         in aposString ++ biParse (current+open) rest
      
    -- Close a formatting mode, abundant apostrophes become text.
    closeMode :: BIMode -> Int -> [BIToken] -> [FTxt]
    closeMode current n rest =
        let close   = greedyTakeMode $ min current n
            aposTxt = listIf (n > close) $ makeAposText current (n-close)
         in aposTxt ++ biParse (current-close) rest

    -- Make a formatted text chunk containing just a number of apostrophes.
    makeAposText :: BIMode -> Int -> FTxt
    makeAposText current n
        | n == 0    = error "will not produce apos string of length 0"
        | otherwise = mkTxt current $ replicate n '\''

    -- Utility function to take as much apostrophes as semantically relavant.
    greedyTakeMode :: Int -> BIMode
    greedyTakeMode n | n >= 5 = 5
                     | n >= 3 = 3
                     | n == 2 = 2
                     | otherwise = error $ "Illegal argument: " ++ show n

    -- Utility: list of all lengths of apostrophe sequences.
    allApos :: [BIToken] -> [Int]
    allApos toks = [n | (Apos n) <- filter isApos toks]

    -- Utility: the maximum of all apostrophe sequences to come.
    maxToCome :: [BIToken] -> Int
    maxToCome = (foldl max 0) . allApos 

    -- Selector for apostrophe tokens.
    isApos :: BIToken -> Bool
    isApos (Apos _) = True
    isApos _        = False

    -- Constructor for formatted text elements.
    mkTxt :: BIMode -> String -> FTxt
    mkTxt _ "" = error "Text cannot be empty"
    mkTxt m s  = (m,s)

-- | Formatted text to Pandoc.
-- This function's prime function is to deal with the improper nesting of the
-- MediaWiki formatted text. It tries to reduce the number of new Pandoc formatting
-- block from being used (e.g. by squeezing a Bold or Italic block in with a
-- Bold+Italic block) at the expense of some readability.
fTxtToPandoc :: [FTxt] -> [Inline]
-- No text, no Pandoc, simple.
fTxtToPandoc [] = []
-- Text that is Bold+Italic. Its nesting level includes all B+I text plus one
-- block that is either Bold or Italic. We start the formatting of that block
-- first so we can include it in the same scope.
fTxtToPandoc total@((5,_):_) =
    let (full,notFull) = span ((==5).fst) total :: ([FTxt], [FTxt])
        firstSuggest = if null notFull
                          then 0 -- not particularly interesting
                          else fst $ head notFull
        (included,reallyDifferent) = if firstSuggest `elem` [2,3]
                                        then span ((==firstSuggest).fst) notFull
                                        else ([], notFull)
     in (case firstSuggest of -- Determine the order of opening Bold and Italic
             2 -> Emph $ [(Strong [(Str s)]) | (_,s) <- full] ++
                         [(Str s) | (_,s) <- included]
             3 -> Strong $ [(Emph [(Str s)]) | (_,s) <- full] ++
                         [(Str s) | (_,s) <- included]
             _ -> Emph $ [(Strong [(Str s)]) | (_,s) <- full])
        : fTxtToPandoc reallyDifferent
-- A block with no formatting, it's only compatible with other unformatted blocks.
fTxtToPandoc total@((0,_):_) =
    let (scope,other) = span ((==0).fst) total
     in [(Str s) | (_,s) <- scope] ++ fTxtToPandoc other
-- A formmated block that is either block or italic. It includes equally formatted
-- blocks as well as the promiscuous 5-blocks.
fTxtToPandoc total@((n,_):_) =
    let (scope,other) = span (\(n',_) -> n == n' || n' == 5) total
        fmtScope = fTxtToPandoc $ map (\(n',s') -> (n'-n,s')) scope
     in (case n of
             2 -> (Emph fmtScope)
             3 -> (Strong fmtScope)
             _ -> error $ "illegal formatting: " ++ show n)
        : fTxtToPandoc other 

-- | Convert a string into a couse list of Inlines, with only Bold/Italic parts parsed.
parseBoldItalic :: String -> [Inline]
parseBoldItalic = fTxtToPandoc . (biParser 0) . biLexer

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- * Links

parseLocalLink :: GenParser Char () Inline
parseLocalLink = try $ doubleBracketed $ do
  p <- unwords `fmap` sepEndBy1 (many1 $ noneOf "|] \t") spaceChar
  l <- option p $ do { char '|' ; skipSpaces ; many1 $ noneOf "]" }
  return $ Link [Str l] (p, "")

parseRemoteLink :: GenParser Char () Inline
parseRemoteLink = try $ singleBracketed $ do
  u <- uri
  skipSpaces
  l <- option u $ many1 (noneOf "]")
  return $ Link [Str l] (u, "")

parseImage :: GenParser Char () Inline
parseImage = try $ doubleBracketed $ do
  string "Image:"
  ps <- many1 (noneOf "|]") `sepBy` (char '|')
  case ps of
   []      -> error "empty image"
   [p]     -> return $ Image [] (p, "")
   (p:_:_) -> return $ Image [Str (last ps)] (p, "")

-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
-- * Utility funcions

mergePlain :: [Block] -> [Block]
mergePlain [] = []
mergePlain (Plain [LineBreak] : bs) = mergePlain bs
mergePlain (Plain i : bs) = para : mergePlain bs2
 where
   para = Para . concat . intersperse [ Space ] $ inlines
   (inlines,bs2) = takeLB [i] bs
mergePlain (b:bs)          = b : mergePlain bs

takeLB acc bs@(Plain [LineBreak] : Plain [LineBreak] : _) = (reverse acc, bs)
takeLB acc (Plain [LineBreak] : Plain p : bs) = takeLB (p : acc) bs
takeLB acc xs = (reverse acc, xs)

-- | A singleton list of a given element if a condition holds, empty otherwise.
listIf :: Bool      -- ^ If this holds, then...
            -> a    -- ^ put this element in the singleton list...
            -> [a]  -- ^ else the list will be empty.
listIf b x | b = [x]
           | otherwise = []

-- | Parses a single end-of-line character and discards it.
-- If it's ever needed, this is where to take into account other line endings (e.g. \r\n).
endOfLine :: MWP ()
endOfLine = do char '\n' <?> "Newline"
               return ()

-- | Strip all elements from a collection from the begin and end of a list.
stripAll :: Eq a => [a] -- ^ The elements to strip.
                 -> [a] -- ^ The list to strip.
                 -> [a] -- ^ The input list with its begin and end strippped.
stripAll toDrop = let dropSome = dropWhile (\x -> x `elem` toDrop)
                   in reverse . dropSome . reverse . dropSome

-- | Short-hand to strip spaces.
stripSpaces :: [Inline] -> [Inline]
stripSpaces = stripAll [Space]

-- | Short-hand to strip all white-spaces (spaces and newlines).
stripWhiteSpace :: [Inline] -> [Inline]
stripWhiteSpace = stripAll [Space, LineBreak]


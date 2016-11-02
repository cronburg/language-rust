{-# LANGUAGE RecordWildCards, DeriveFunctor #-}

module Language.Rust.Data.Position where

-- Taken and abbreviated from
-- | A position in a source file. The row and column information is kept only for its convenience
-- and human-readability.
-- https://hackage.haskell.org/package/language-c-0.5.0/docs/src/Language-C-Data-Position.html#Position
data Position = Position {
    absoluteOffset :: {-# UNPACK #-} !Int, -- ^ absolute offset the source file.
    row :: {-# UNPACK #-} !Int,            -- ^ row (line) in the source file.
    col :: {-# UNPACK #-} !Int             -- ^ column in the source file.
  }
  | NoPosition
  deriving (Eq, Ord)

-- | starting position in a file
initPos :: Position
initPos = Position 0 1 1

-- | advance column
incPos :: Position -> Int -> Position
incPos Position{..} offset = Position (absoluteOffset + offset) (row + offset) col

-- | advance to the next line
retPos :: Position -> Position
retPos Position{..} = Position (absoluteOffset + 1) (row + 1) 1

instance Show Position where
  show pos = show (row pos) ++ ":" ++ show (col pos)


type ExpnId = Int -- https://docs.serde.rs/syntex_pos/struct.ExpnId.html


-- | Spans represent a region of code, used for error reporting. Positions in spans are absolute positions from the
-- beginning of the codemap, not positions relative to FileMaps. Methods on the CodeMap can be used to relate spans
-- back to the original source. You must be careful if the span crosses more than one file - you will not be able to
-- use many of the functions on spans in codemap and you cannot assume that the length of the span = hi - lo; there may
-- be space in the BytePos range between files.
-- https://docs.serde.rs/syntex_syntax/ext/quote/rt/struct.Span.html
data Span
  = Span {
    lo :: Position,
    hi :: Position --,
    -- expnId :: ExpnId
  }

data Spanned a = Spanned { node :: a, span :: Span } deriving (Functor)


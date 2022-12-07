module Noble.Internal.Helpers (showBytes, compareIntArray, byteLength) where

import Prelude

import Data.ArrayBuffer.Types (Uint8Array)

compareIntArray :: Uint8Array -> Uint8Array -> Ordering
compareIntArray xs ys = compare 0 (_ordIntArray aux xs ys)
  where aux x y = case compare x y of
                    LT -> 1
                    EQ -> 0
                    GT -> -1

foreign import byteLength :: Uint8Array -> Int

foreign import showBytes :: Uint8Array -> String

foreign import _ordIntArray :: (Int -> Int -> Int) -> Uint8Array -> Uint8Array -> Int



-- editorconfig-checker-disable-file

{-# LANGUAGE BangPatterns  #-}
{-# LANGUAGE TypeOperators #-}

module Benchmarks.Bitwise (makeBenchmarks) where

import Common
import Generators

import PlutusCore
import PlutusCore.Evaluation.Machine.ExMemoryUsage

import Criterion.Main
import Data.ByteString qualified as BS
import Hedgehog qualified as H


---------------- ByteString builtins ----------------

largeSampleNum :: Int
largeSampleNum = 150

largeSampleSizes :: [Int]
largeSampleSizes = fmap (10 *) [1..largeSampleNum]

-- Smallish bytestring inputs: 150 entries.  Note that the length of a
-- bytestring is eight times the size.
largeSample :: H.Seed -> [BS.ByteString]
largeSample seed = makeSizedByteStrings seed largeSampleSizes

smallSampleNum :: Int
smallSampleNum = 50

smallSampleSizes :: [Int]
smallSampleSizes = [1..smallSampleNum]

-- Smallish bytestring inputs: 150 entries.  Note that the length of a
-- bytestring is eight times the size.
smallSample :: H.Seed -> [BS.ByteString]
smallSample seed = makeSizedByteStrings seed smallSampleSizes

-- Make an integer of size n which encodes to 0xFF...FF
repunit :: Int -> Integer
repunit n = 256^(8*n) - 1

------------------------- ByteStringToInteger -------------------------

{- Experiments show that the times for big-endian and little-endian conversions
   are very similar, with big-endian conversion perhaps taking a fraction
   longer.  We just generate a costing function for big-endian conversion and
   use that for the little-endian conversion as well.  A quadratic function
   fitted to inputs of size up to 150 gives a good fit and extrapolates well to
   larger inputs. -}
benchByteStringToInteger :: Benchmark
benchByteStringToInteger =  createTwoTermBuiltinBenchElementwise ByteStringToInteger []
                            (repeat True) (largeSample seedA)


------------------------- IntegerToByteString -------------------------

{- We have four possibilities for integer to bytestring conversions: they can be
 big- or little-endian, and they can also be of bounded or unbounded width.
 Experiments show that all of these take about the same time, with the bounded
 big-endian conversion taking a few percent longer than the other three
 possiblities.  We just benchmark that and use the model for all of the
 possibilities.  The bounded conversions can require some extra work to pad the
 result to the required width, for example if we ask to convert the integer 2 to
 a bytestring of width 1000.  We use a quadratic costing function which uses
 only the size of the integer, but this is safe because the implementation uses
 a single function call to generate the padding and experiments show that the
 time required for this is negligible in comparison to the conversion time.
 It's important to make sure that the memory cost does take account of the width
 though. -}

-- Make sure that the input integer really does require the full width so that
-- the conversion does the maximum amount of work.
benchIntegerToByteString :: Benchmark
benchIntegerToByteString =
    let b = IntegerToByteString
        widths = largeSampleSizes
        inputs = fmap repunit widths
        -- This is like createThreeTermBuiltinBenchElementwise, but we want to
        -- make sure that the width appears literally in the benchmark name.
        createBench l =
            let mkOneBM (e, width, n) =
                      -- Widths are in words: we need to convert those to widths in bytes for the implementation
                      let width' = 8 * fromIntegral width
                      in bgroup (showMemoryUsage e) [
                              bgroup (showMemoryUsage (LiteralByteSize width')) [mkBM e width' n]
                             ]
                          where mkBM x y z = benchDefault (showMemoryUsage z) $ mkApp3 b [] x y z
            in bgroup (show b) $ fmap mkOneBM l

    in createBench $ zip3 (repeat True) widths inputs


{- For AndByteString with different-sized inputs, calling it with extension
semantics (ie, first argument=True) takes up to about 5% longer than with
truncation semantics for small arguments and up to about 15% for larger inputs.
Fitting t~min(x,y) gives a reasonable prediction for small values of x and y but
this doesn't extend too well to larger values.  There are two factors in play:
with extension semantics there's less copying work to do but more alloction work
(which is a lot cheaper).  If we fit a model of the form t~pmin(x,y) then this
accounts for the copying but not the allocation.  if we add a factor for copying
as well (t ~ pmin(x,y) + abs(x-y)) then we get a model that extends well to
larger data.  Equivalently we can fit t~x+y to the data for y<=x, but then we'd
have to swap the inputs for y>x.

I(x+y) does a good job though: we get within +/-5% for the small data and -20%
to +5% for big data. We could try fitting t=a+bx along x=y for the small data
and then extrapolate that to a/2+ b/2(x+y) elsewhere.
-}

benchAndByteString :: Benchmark
benchAndByteString =
  let xs = smallSample seedA
      ys = smallSample seedB
  in createTwoTermBuiltinBenchWithFlag AndByteString [] True xs ys

{- Most of the initial benchmarks were run with a set of small input bytestrings
  (up to size 40 / 320 bytes) and then again with a set of large inputs (up to
  size 400 / 3200 bytes).  In the budgeting benchmarks we go up to size 50 (=
  400 bytes) for small inputs.
-}

{- For ComplementByteString, the time taken is linear in the length.  A model
 based on small input sizes extrapolates well to results for large inputs -}
benchComplementByteString :: Benchmark
benchComplementByteString =
  let xs = smallSample seedA
  in createOneTermBuiltinBench ComplementByteString [] xs


{- readBit is pretty much constant time regardless of input size and the position of
the bit to be read. -}
benchReadBit :: Benchmark
benchReadBit =
  let xs = largeSample seedA
      ys :: [Integer] = fmap (\n -> fromIntegral $ 8*n-1) largeSampleSizes
  in createTwoTermBuiltinBenchElementwise ReadBit [] xs ys


{- Benchmarks show that the time taken by `writeBits` depends mostly on the size
   of the list of updates, although it may take a little longer to write bits
   with larger indices.  We run all of the benchmarks on a 1000-byte bytestring
   and benchmark the time taken to write the highest-indexed bit to take account
   of this.
-}
benchWriteBits :: Benchmark
benchWriteBits =
  let fun = WriteBits
      len = 1000
      bs = BS.replicate len 0xFF
      topIndex :: Integer = fromIntegral $ 8*len - 1
      mkUpdates k = replicate k (topIndex, True) -- write the highest bit k times
      updates = fmap mkUpdates [1..smallSampleNum]
      mkBM :: [(Integer, Bool)] -> Benchmark
      mkBM l = benchDefault (showMemoryUsage (ListCostedByLength l)) $ mkApp2 fun [] bs l
  in bgroup (show fun) $ [bgroup (showMemoryUsage bs) $ fmap mkBM updates]

{- For small inputs `replicateByte` looks constant-time.  For larger inputs it's
   linear. A model based on large data overestimates the small results, but not
   too badly.
-}
benchReplicateByte :: Benchmark
benchReplicateByte =
  let ys = replicate largeSampleNum (0xFF :: Integer)
  in createTwoTermBuiltinBenchElementwiseLiteralInX ReplicateByte []
       (fmap (fromIntegral . (8*)) largeSampleSizes) ys

{- Benchmarks with varying sizes of bytestrings and varying amounts of shifting
   show that the execution time of `shiftByteString` depends linearly on the
   length of the bytestring and (to a much smaller degree) the size of the
   shift, except that shifts which involve shifting bits between bytes are
   significantly more expensive than shfts by a whole number of bytes.  For
   bytestrings of size 50 the ratio between the former and the latter is about
   1.5 and for size 400 it's about 4.  We could add a special case for costing
   whole-byte shifts, but for the time being we run benchmarks for a single-bit
   shift and fit a linear model to the time taken versus the length of the
   bytestring.  This gives a mmodel which is very accurate for small shifts and
   overstimates times for large shifts by maybe 4% or so, A model fitted to
   smaller data extrapolates very well to larger data.
-}
benchShiftByteString :: Benchmark
benchShiftByteString =
  let xs = smallSample seedA
      ns = fmap (const 1) xs
      in createTwoTermBuiltinBenchElementwiseLiteralInY ShiftByteString [] xs ns

{- The behaviour of `rotateByteString` is very similar to that of
   `shiftByteString` except that the time taken depends pretty much linearly on
   the length of the bytestring and the effect of the size of the rotation is
   negligible.  We could add a special case for costing whole-byte rotations,
   but for the time being we run benchmarks for a single-bit shift and fit a
   straight line to the time taken.  A model fitted to smaller data extrapolates
   well to larger data.
-}
benchRotateBytestring :: Benchmark
benchRotateBytestring =
  let xs = smallSample seedA
      ns = fmap (const 1) xs
  in createTwoTermBuiltinBenchElementwiseLiteralInY RotateByteString [] xs ns

{- For CountSetBits, the time taken is linear in the length.  A model based on
   small input sizes (up to 1280 bytes) extrapolates well to results for large
   inputs (up to 12800 bytes).  Counting the bits in an all-0xFF bytestring may
   take 1% or so longer than for an all-0x00 bytestring. -}
benchCountSetBits :: Benchmark
benchCountSetBits =
  let xs = fmap (\n -> BS.replicate (8*n) 0xFF) smallSampleSizes
  in createOneTermBuiltinBench CountSetBits [] xs

{- For FindFirstSetBits the time taken is pretty much linear in the length, with
   occasional bumps.  Unsurprisingly the function takes longest for an all-0x00
   bytestring because it has to examine every byte in that case.  For costing we
   use 0x8000...00 just to avoid the all-zeros case in case someone attempts to
   optimise for that case at some time in the future.  For small data the worst
   case takes up to 8% longer than the best case (0x00..01) and for large data
   it can take up 40% longer. A model based on small input sizes extrapolates
   well to results for large inputs. -}
benchFindFirstSetBit :: Benchmark
benchFindFirstSetBit =
  let xs = fmap (\n -> BS.cons 0x80 (BS.replicate (8*(n-1)) 0x00)) smallSampleSizes
  in createOneTermBuiltinBench FindFirstSetBit [] xs

makeBenchmarks :: [Benchmark]
makeBenchmarks =
  [ bgroup "bytestrings"
    [ benchIntegerToByteString
    , benchByteStringToInteger
    , benchAndByteString
    , benchComplementByteString
    , benchReadBit
    , benchWriteBits
    , benchReplicateByte
    , benchShiftByteString
    , benchRotateBytestring
    , benchCountSetBits
    , benchFindFirstSetBit
    ]
  ]

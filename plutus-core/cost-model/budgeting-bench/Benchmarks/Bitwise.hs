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
largeSampleNum = 160

largeSampleSizes :: [Int]
largeSampleSizes = fmap (10 *) [1..largeSampleNum]

-- Smallish bytestring inputs: 150 entries.  Note that the length of a
-- bytestring is eight times the size.
largeSample :: H.Seed -> [BS.ByteString]
largeSample seed = makeSizedByteStrings seed largeSampleSizes

smallSampleNum :: Int
smallSampleNum = 40

smallSampleSizes :: [Int]
smallSampleSizes = [1..smallSampleNum]

-- Smallish bytestring inputs: 150 entries.  Note that the length of a
-- bytestring is eight times the size.
smallSample :: H.Seed -> [BS.ByteString]
smallSample seed = makeSizedByteStrings seed smallSampleSizes

-- Make an integer of size n which encodes to 0xFF...FF
allFF :: Int -> Integer
allFF n = 256^(8*n) - 1

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
        inputs = fmap allFF widths
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

-- Benchmark with equal-sized inputs: it should be linear in the size.
-- Initially check what happens for different-sized inputs with padding and
-- truncation.  Presumably both of these will be bounded by the same-size case.
benchAndByteStringFalse :: Benchmark
benchAndByteStringFalse =
  let b = AndByteString
      xs = smallSample seedA
      ys = smallSample seedB
      in createTwoTermBuiltinBenchWithNameAndFlag "AndByteStringFalse" b [] False xs ys

benchAndByteStringTrue :: Benchmark
benchAndByteStringTrue =
  let b = AndByteString
      xs = smallSample seedA
      ys = smallSample seedB
      in createTwoTermBuiltinBenchWithNameAndFlag "AndByteStringTrue" b [] True xs ys

-- This should be a straightforward liear function of the size.
benchComplementByteString :: Benchmark
benchComplementByteString =
  let xs = largeSample seedA
  in createOneTermBuiltinBench ComplementByteString [] xs

-- Linear in length and/or position?  Maybe pretty much constant time.
benchReadBitFirst :: Benchmark
benchReadBitFirst =
  let xs = largeSample seedA
      ys :: [Integer] = replicate largeSampleNum 0
  in createTwoTermBuiltinBenchElementwiseWithName "ReadBitFirst" ReadBit [] xs ys

-- Linear in length and/or position?  Maybe pretty much constant time.
benchReadBitLast :: Benchmark
benchReadBitLast =
  let xs = largeSample seedA
      ys :: [Integer] = fmap (\n -> fromIntegral $ 8*n-1) largeSampleSizes
  in createTwoTermBuiltinBenchElementwiseWithName "ReadBitLast" ReadBit [] xs ys

-- The function uses pokeByteOff, which updates a byte in place, presumably in
-- constant time.  If readBit is constant time then this should be linear in the
-- size of the second argument.

benchWriteBitsFirst :: Benchmark
benchWriteBitsFirst =
  let xs = smallSample seedA
      ys = fmap (\n -> replicate n (0::Integer,True)) smallSampleSizes
  in createTwoTermBuiltinBenchWithName "WriteBitsFirst" WriteBits [] xs ys

-- For each n we create a bytestring of length n and then for each
-- of those we create a list of pairs which write 1 into the highest
-- bit.  Stricly the actual length of the list should be the size measure,
-- but the size will be a reasonable proxy.
benchWriteBitsLast :: Benchmark
benchWriteBitsLast =
  let xs0 = smallSample seedA
      xs = concat $ fmap (replicate smallSampleNum) xs0
      mkYs x =
        let n = fromIntegral $ BS.length x :: Integer
        in fmap (\s -> replicate s (8*n-1 :: Integer, True)) smallSampleSizes
      ys = concat $ fmap mkYs xs0
      !_ = if length xs /= length ys then error "mismatch" else ()
  in createTwoTermBuiltinBenchElementwiseWithName "WriteBitsLast" WriteBits [] xs ys

-- This will be linear in the first argument (the number of replications), but
-- may appear constant time.
benchReplicateByte :: Benchmark
benchReplicateByte =
  let ys = replicate largeSampleNum (0xFF :: Integer)
  in createTwoTermBuiltinBenchElementwiseLiteralInX ReplicateByte []
       (fmap (fromIntegral . (8*)) largeSampleSizes) ys

-- Second batch

shifts :: [Int]
shifts = [1..smallSampleNum]

-- Probably linear in x and literally in y; will be more expensive for y not divisible by 8
benchShiftByteStringPos1 :: Benchmark
benchShiftByteStringPos1 =
  let b = ShiftByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringPos1" b [] xs ns

benchShiftByteStringPos2 :: Benchmark
benchShiftByteStringPos2 =
  let b = ShiftByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n-1) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringPos2" b [] xs ns

benchShiftByteStringNeg1 :: Benchmark
benchShiftByteStringNeg1 =
  let b = ShiftByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ -8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringNeg1" b [] xs ns

benchShiftByteStringNeg2 :: Benchmark
benchShiftByteStringNeg2 =
  let b = ShiftByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 1-8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringNeg2" b [] xs ns

-- Probably linear in x and literally in y; will be more expensive for y not divisible by 8
benchRotateBytestringPos1 :: Benchmark
benchRotateBytestringPos1 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringPos" b [] xs ns

benchRotateBytestringPos2 :: Benchmark
benchRotateBytestringPos2 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n-1) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringPos" b [] xs ns

benchRotateBytestringNeg1 :: Benchmark
benchRotateBytestringNeg1 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ -8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringNeg" b [] xs ns

benchRotateBytestringNeg2 :: Benchmark
benchRotateBytestringNeg2 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 1-8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringNeg" b [] xs ns

benchCountSetBits00 :: Benchmark
benchCountSetBits00 =
  let xs = fmap (\n -> BS.replicate (8*n) 0x00) largeSampleSizes
  in createOneTermBuiltinBenchWithName "CountSetBits00" CountSetBits [] xs

-- Probably linear in x
benchCountSetBitsFF :: Benchmark
benchCountSetBitsFF =
  let xs = fmap (\n -> BS.replicate (8*n) 0xFF) largeSampleSizes
  in createOneTermBuiltinBenchWithName "CountSetBitsFF" CountSetBits [] xs

-- 0x0000...00
benchFindFirstSetBit1 :: Benchmark
benchFindFirstSetBit1 =
  let xs = fmap (\n -> BS.replicate (8*n) 0x00) largeSampleSizes
  in createOneTermBuiltinBenchWithName "FindFirstSetBit1" FindFirstSetBit [] xs

-- 0x8000...00
benchFindFirstSetBit2 :: Benchmark
benchFindFirstSetBit2 =
  let xs = fmap (\n -> BS.cons 0x80 (BS.replicate (8*(n-1)) 0x00)) largeSampleSizes
  in createOneTermBuiltinBenchWithName "FindFirstSetBit2" FindFirstSetBit [] xs

-- 0x0101..01
benchFindFirstSetBit3 :: Benchmark
benchFindFirstSetBit3 =
  let xs = fmap (\n -> BS.replicate (8*n) 0x01) largeSampleSizes
  in createOneTermBuiltinBenchWithName "FindFirstSetBit3" FindFirstSetBit [] xs

makeBenchmarks :: [Benchmark]
makeBenchmarks = [
   bgroup "experiment1"
    [ benchIntegerToByteString
    , benchByteStringToInteger
    , benchAndByteStringFalse
    , benchAndByteStringTrue
    , benchComplementByteString
    , benchReadBitFirst
    , benchReadBitLast
    , benchWriteBitsFirst
    , benchWriteBitsLast
    , benchReplicateByte
    ]
  , bgroup "experiment2"
    [
      benchShiftByteStringPos1
    , benchShiftByteStringPos2
    , benchShiftByteStringNeg1
    , benchShiftByteStringNeg2
    , benchRotateBytestringPos1
    , benchRotateBytestringPos2
    , benchRotateBytestringNeg1
    , benchRotateBytestringNeg2
    , benchCountSetBits00
    , benchCountSetBitsFF
    , benchFindFirstSetBit1
    , benchFindFirstSetBit2
    , benchFindFirstSetBit3
    ]
  ]

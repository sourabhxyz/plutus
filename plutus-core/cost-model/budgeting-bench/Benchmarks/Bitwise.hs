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



-- AndByteString shows very similar times for False and True flags, with True
-- (ie extension) taking a bit longer (~8%). It shows a good fit to
-- a+b·min(x,y). This is on inputs up to size 400; for inputs up to size 1600
-- the picture is similar, but it becomes a bit flattened for size>500.  We
-- should fit the moel to the smaller data and see how it predicts the larger
-- data.



{- For AddInteger with different-sized inputs, calling it with extension semantics
(ie, firsrt argument=True) takes up to about 5% longer than with truncation
semantics for small arguments and up to about 15% for larger inputs.  Fitting
t~min(x,y) gives a reasonable prediction for small values of x and y but this
doesn't extend too well to larger values.  There are two factors in play: with
extension semantics there's less copying work to do but more alloction work
(which is a lot cheaper).  If we fit a model of the form t~pmin(x,y) then this
accounts for the copying but not the allocation.  if we add a factor for copying
as well (t ~ pmin(x,y) + abs(x-y)) then we get a model that extends well to
larger data.  Equivalently we can fit t~x+y to the data for y<=x, but then we'd
have to swap the inputs for y>x.

I(x+y) does a perfectly good job though: we get within +/-5% for the small data
and -20% to +5% for big data. We could try fitting t=a+bx along x=y for the
small data and then extrapolate that to a/2+ b/2(x+y) elsewhere.
-}

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

-- For ComplementByteString, the time taken is linear in the length.  A model
-- based on small input sizes (up to 1280 bytes) extrapolates well to results
-- for large inputs (up to 12800 bytes).
benchComplementByteString :: Benchmark
benchComplementByteString =
  let xs = largeSample seedA
  in createOneTermBuiltinBench ComplementByteString [] xs


-- ReadBit seems pretty much constant time.  Let's benchmark up to size 500
-- reading the first (maybe fifth?) bit and take the mean.

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


-- The WriteBits function uses pokeByteOff, which updates a byte in place,
-- presumably in constant time.  If readBit is constant time then this should be
-- linear in the size of the second argument.

-- Benchmarks show that the time does indeed depend mostly on the size of the
-- list of updates, and it's linear over a large range of scales. Writing the
-- final bit rather than the first one takes a little longer, maybe by about
-- 5-10%.  The time taken to write the final bit doesn't seem to depend on the
-- length of the bytestring.  Why does it take longer than writing the first
-- bit?  Does it make any difference if we write a big list of different bits?

-- For the real benchmarks, take a moderately long bytestring and try varying
-- lengths of updates.  We don't need to vary the bytestring.

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


-- For small inputs (up to 160 bytes) this looks kind of constant-time.  For
-- larger inputs (up to 1600 bytes)it's linear.  A linear model based on small
-- data underestimates large results very badly. A model based on large data
-- overestimates the small results. A model based on figures up to 500 bytes
-- fits the smaller data much better.
benchReplicateByte :: Benchmark
benchReplicateByte =
  let ys = replicate largeSampleNum (0xFF :: Integer)
  in createTwoTermBuiltinBenchElementwiseLiteralInX ReplicateByte []
       (fmap (fromIntegral . (8*)) largeSampleSizes) ys

-- Second batch

shifts :: [Int]
shifts = [1..smallSampleNum]

{- Benchmarks with varying sizes of bytestrings and varying amounts of rotation
   show that the time depends linearly on the length of the bytestring and (to a
   much smaller degree) the size of the shift, except that shifts which involve
   shifting bits between bytes are significantly more expensive than shfts by a
   whole number of bytes.  For bytestrings of size 50 the ratio between the
   former and the latter is about 1.5 and for size 400 it's about 4.  For
   bytestrings of a given length the size of the rotation appears to have
   essentially no effect on the time taken. We could add a special case for
   costing whole-byte rotations, but for the time being we run benchmarks for a
   single-bit shift and fit a straight line to the time taken.  A model fitted
   to smaller data extrapolates very well to larger data.
-}
-- Probably linear in x and literally in y; will be more expensive for y not divisible by 8
benchShiftByteStringPos1 :: Benchmark
benchShiftByteStringPos1 =
  let b = ShiftByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringPos1" b [] xs ns

-- For a fixed input bytestring, the time taken decreases as the amount of shift
-- increases because the number of bits you actually have to copy decreases.
--
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



{- Benchmarks with varying sizes of bytestrings and varying amounts of rotation
   show that the time taken depends pretty much linearly on the length of the
   bytestring (and the effect of the size of the rotation is negligible), except
   that rotations which involve shifting bits between bytes are significantly
   more expensive than rotations by a whole number of bytes.  For bytestrings of
   size 50 the ratio between the former and the latter is about 1.5 and for size
   200 it's about 3.  For bytestrings of a given length the size of the rotation
   appears to have essentially no effect on the time taken. We could add a
   special case for costing whole-byte rotations, but for the time being we run
   benchmarks for a single-bit shift and fit a straight line to the time taken.
   A model fitted to smaller data extrapolates very well to larger data.
-}

-- Probably linear in x and literally in y; will be more expensive for y not divisible by 8
benchRotateBytestringPos1 :: Benchmark
benchRotateBytestringPos1 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringPos1" b [] xs ns

benchRotateBytestringPos2 :: Benchmark
benchRotateBytestringPos2 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 8*n-1) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringPos2" b [] xs ns

benchRotateBytestringNeg1 :: Benchmark
benchRotateBytestringNeg1 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ -8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringNeg1" b [] xs ns

benchRotateBytestringNeg2 :: Benchmark
benchRotateBytestringNeg2 =
  let b = RotateByteString
      xs = smallSample seedA
      ns = fmap (\n -> fromIntegral $ 1-8*n) shifts
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringNeg2" b [] xs ns



-- For CountSetBits, the time taken is linear in the length.  A model based on
-- small input sizes (up to 1280 bytes) extrapolates well to results for large
-- inputs (up to 12800 bytes).  Counting the bits in an all-0xFF bytestring may
-- take 1% or so longer than for an all-0x00 bytestring.

benchCountSetBits00 :: Benchmark
benchCountSetBits00 =
  let xs = fmap (\n -> BS.replicate (8*n) 0xFF) largeSampleSizes
  in createOneTermBuiltinBenchWithName "CountSetBits00" CountSetBits [] xs

-- Probably linear in x
benchCountSetBitsFF :: Benchmark
benchCountSetBitsFF =
  let xs = fmap (\n -> BS.replicate (8*n) 0xFF) largeSampleSizes
  in createOneTermBuiltinBenchWithName "CountSetBitsFF" CountSetBits [] xs

-- For FindFirstSetBits the time taken is pretty much linear in the length, with
-- occasional bumps.  Unsurprisingly the function takes longest for an all-0x00
-- bytestring because it has to examine every byte in that case so this is the
-- case we use for costing.  For small data (up to 1280 bytes = 160 ExMemory)
-- the worst case takes up to 8% longer than the best case (all 0x00) and for
-- large data (up to 12800 bytes) it can take up 40% longer. A model based on
-- small input sizes extrapolates well to results for large inputs.
--
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
  in createOneTermBuiltinBench FindFirstSetBit [] xs

bs1000 :: BS.ByteString
bs1000 = makeSizedByteString seedA 125

benchShiftByteStringPos1000 :: Benchmark
benchShiftByteStringPos1000 =
  let b = ShiftByteString
      xs = [bs1000]
      ns = [0..1100]
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringPos1000" b [] xs ns

benchShiftByteStringNeg1000 :: Benchmark
benchShiftByteStringNeg1000 =
  let b = ShiftByteString
      xs = [bs1000]
      ns = fmap negate [0..1100]
      in createTwoTermBuiltinBenchWithNameLiteralInY "ShiftByteStringNeg1000" b [] xs ns

benchRotateBytestringPos1000 :: Benchmark
benchRotateBytestringPos1000 =
  let b = RotateByteString
      xs = [bs1000]
      ns = [1..1100]
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringPos1000" b [] xs ns

benchRotateBytestringNeg1000 :: Benchmark
benchRotateBytestringNeg1000 =
  let b = RotateByteString
      xs = [bs1000]
      ns = map negate [1..1100]
      in createTwoTermBuiltinBenchWithNameLiteralInY "RotateByteStringNeg1000" b [] xs ns

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
  , bgroup "experiment3"
    [
      benchShiftByteStringPos1000
    , benchShiftByteStringNeg1000
    , benchRotateBytestringPos1000
    , benchRotateBytestringNeg1000
    ]
  ]

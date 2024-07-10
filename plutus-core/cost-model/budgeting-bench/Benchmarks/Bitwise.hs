-- editorconfig-checker-disable-file
module Benchmarks.Bitwise (makeBenchmarks) where

import Common
import Generators

import PlutusCore
import PlutusCore.Evaluation.Machine.ExMemoryUsage

import Criterion.Main
import Data.ByteString qualified as BS
import Hedgehog qualified as H


---------------- ByteString builtins ----------------

numSamples :: Int
numSamples = 150

sampleSizes :: [Int]
sampleSizes = [1..numSamples]

-- Smallish bytestring inputs: 150 entries.  Note that the length of a
-- bytestring is eight times the size.
makeSample :: H.Seed -> [BS.ByteString]
makeSample seed = makeSizedByteStrings seed sampleSizes

-- Make an integer of size n which encodes to 0xFF...FF
repunit :: Int -> Integer
repunit n = 256^(8*n) - 1

-- Calculate the index of the top (ie, righmost) bit in a bytestring.
topBitIndex :: BS.ByteString -> Integer
topBitIndex s = fromIntegral $ 8*(BS.length s)-1

------------------------- ByteStringToInteger -------------------------

{- Experiments show that the times for big-endian and little-endian conversions
   are very similar, with big-endian conversion perhaps taking a fraction
   longer.  We just generate a costing function for big-endian conversion and
   use that for the little-endian conversion as well.  A quadratic function
   fitted to inputs of size up to 150 gives a good fit and extrapolates well to
   larger inputs. -}
benchByteStringToInteger :: Benchmark
benchByteStringToInteger =  createTwoTermBuiltinBenchElementwise ByteStringToInteger []
                            (repeat True) (makeSample seedA)


------------------------- IntegerToByteString -------------------------

{- We have four possibilities for integer to bytestring conversions: they can be big- or
 little-endian, and they can also be of bounded or unbounded width.  Experiments show that
 all of these take about the same time, with the bounded big-endian conversion taking a
 few percent longer than the other three possiblities.  We just benchmark that and use the
 model for all of the possibilities.  The bounded conversions can require some extra work
 to pad the result to the required width, for example if we ask to convert the integer 2
 to a bytestring of width 1000.  We use a quadratic costing function which uses only the
 size of the integer, but this is safe because the implementation uses a single function
 call to generate the padding and experiments show that the time required for this is
 negligible in comparison to the conversion time.  It's important to make sure that the
 memory cost does take account of the width though.  The sample we use gives us
 bytestrings up to 8*150 = 1200 bytes long.  This is well -- within the 8192-byte limit. -}
benchIntegerToByteString :: Benchmark
benchIntegerToByteString =
    let b = IntegerToByteString
        widths = sampleSizes
        inputs = fmap repunit widths
        -- This is like createThreeTermBuiltinBenchElementwise, but we want to
        -- make sure that the width appears literally in the benchmark name.
        createBench l =
            let mkOneBM (e, width, n) =
                      -- Widths are in words: we need to convert those to widths in bytes for the implementation
                      let width' = 8 * fromIntegral width
                      in bgroup (showMemoryUsage e) [
                              bgroup (showMemoryUsage (IntegerCostedAsByteSize width')) [mkBM e width' n]
                             ]
                          where mkBM x y z = benchDefault (showMemoryUsage z) $ mkApp3 b [] x y z
            in bgroup (show b) $ fmap mkOneBM l

    in createBench $ zip3 (repeat True) widths inputs


{- Most of the initial exploratory benchmarks were run with a set of small input
  bytestrings (up to size 160 / 1280 bytes) and then again with a set of large
  inputs (up to size 1600 / 12800 bytes).  In the final budgeting benchmarks we
  mostly go up to size 150 (= 1200 bytes).
-}

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

A model for t~x+y does a good job though: we get within +/-5% for the small data
and -20% to +5% for big data. We could also try fitting t=a+bx along x=y for the
small data and then extrapolate that to a/2+ b/2(x+y) elsewhere.
-}

benchAndByteString :: Benchmark
benchAndByteString =
  let inputSizes = fmap (20*) [1..25]  -- 20..400: 625 cases, which should take an hour or so.
      xs = makeSizedByteStrings seedA inputSizes
      ys = makeSizedByteStrings seedB inputSizes
  in createTwoTermBuiltinBenchWithFlag AndByteString [] True xs ys

{- For ComplementByteString, the time taken is linear in the length.  A model
 based on small input sizes extrapolates well to results for large inputs -}
benchComplementByteString :: Benchmark
benchComplementByteString =
  let xs = makeSample seedA
  in createOneTermBuiltinBench ComplementByteString [] xs

{- readBit is pretty much constant time regardless of input size and the position of
the bit to be read. -}
benchReadBit :: Benchmark
benchReadBit =
  let xs = makeSample seedA
      ys :: [Integer] = fmap topBitIndex xs
  in createTwoTermBuiltinBenchElementwise ReadBit [] xs ys


{- Benchmarks show that the time taken by `writeBits` depends mostly on the size
   of the list of updates, although it may take a little longer to write bits
   with larger indices.  We run benchmarks involving increasing numbers of
   updates to 1024-byte bytestrings, always writing the highest-indexed bit to
   take account of this.  We use a fresh bytestring for each set of updates.
-}
benchWriteBits :: Benchmark
benchWriteBits =
  let fun = WriteBits
      size = 128  -- This is equal to length 1024.
      xs = makeSizedByteStrings seedA $ take numSamples $ repeat size
      l = zip xs [1..numSamples]
      -- Given a bytestring s and an integer k, return a pair (s,u) where u is a
      -- list of updates which write the highest bit in s 10*k times.  Here k
      -- will range from 1 to numSamples, which is 150.
      mkUpdatesFor (s,k) =
        let topIndex = topBitIndex s
            updates = take (10*k) $ cycle [(topIndex, False), (topIndex, True)]
        in (s, updates)
      inputs = fmap mkUpdatesFor l
      mkBM x y = benchDefault (showMemoryUsage (ListCostedByLength y)) $ mkApp2 fun [] x y
  in bgroup (show fun) $ fmap(\(s,u) -> bgroup (showMemoryUsage s) $ [mkBM s u]) inputs
  {- This is like createTwoTermBuiltinBenchElementwise except that the benchmark name contains
   the length of the list of updates, not the memory usage.  The denotation of WriteBits
   in Default.Builtins must wrap its second argument in ListCostedByLength to make sure
   that the correct ExMemoryUsage instance is called for costing. -}

{- For small inputs `replicateByte` looks constant-time.  For larger inputs it's
   linear.  We're limiting the output to 8192 bytes (size 1024), so we may as
   well test the whole legal range.  NB: if we change the value of
   integerToByteStringMaximumOutputLength then we probably need to change the
   limits here too.
-}
benchReplicateByte :: Benchmark
benchReplicateByte =
  let numCases = 128 :: Int
      xs = fmap (fromIntegral . (64*)) [1..numCases] :: [Integer]
      -- ^ This gives us replication counts up to 64*128 = 8192, the maximum allowed.
      ys = replicate numCases (0xFF :: Integer)
  in createTwoTermBuiltinBenchElementwiseWithXAsByteSize ReplicateByte [] xs ys

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
  let xs = makeSample seedA
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
  let xs = makeSample seedA
      ns = fmap (const 1) xs
  in createTwoTermBuiltinBenchElementwiseLiteralInY RotateByteString [] xs ns

{- For CountSetBits, the time taken is linear in the length.  A model based on small input
   sizes (up to 1280 bytes) extrapolates well to results for large inputs (up to 12800
   bytes).  Counting the bits in an all-0xFF bytestring may take 1% or so longer than for
   an all-0x00 bytestring. -}
benchCountSetBits :: Benchmark
benchCountSetBits =
  let xs = fmap (\n -> BS.replicate (8*n) 0xFF) sampleSizes  -- length 8, 16, ..., 1200
  in createOneTermBuiltinBench CountSetBits [] xs

{- For FindFirstSetBits the time taken is pretty much linear in the length, with occasional
   bumps.  Unsurprisingly the function takes longest for an all-0x00 bytestring because it
   has to examine every byte in that case.  For costing we use 0x8000...00 just to avoid
   the all-zeros case in case someone attempts to optimise for that case at some time in
   the future.  For small data the worst case takes up to 8% longer than the best case
   (0x00..01) and for large data it can take up to 40% longer. A model based on small
   input sizes extrapolates well to results for large inputs. -}
benchFindFirstSetBit :: Benchmark
benchFindFirstSetBit =
  let xs = fmap (\n -> BS.cons 0x80 (BS.replicate (8*n-1) 0x00)) sampleSizes
  in createOneTermBuiltinBench FindFirstSetBit [] xs

makeBenchmarks :: [Benchmark]
makeBenchmarks =
  [ bgroup "bytestrings"
    [ benchIntegerToByteString
    , benchByteStringToInteger
    , benchAndByteString
    , benchComplementByteString
    , benchReadBit
    , benchWriteBits100
    , benchWriteBits1024
    , benchReplicateByte
    , benchShiftByteString
    , benchRotateBytestring
    , benchCountSetBits
    , benchFindFirstSetBit
    ]
  ]

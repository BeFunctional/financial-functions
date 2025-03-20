module Main
  ( main,
  )
where

import Data.Maybe
  ( isJust,
    isNothing,
  )
import Numeric.Financial
import Test.Tasty
import Test.Tasty.QuickCheck as QC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Property tests" [irrTerminates]

irrTerminates :: TestTree
irrTerminates =
  QC.testProperty
    "That internal rate of return finds a solution "
    irrTerminates'

irrTerminates' :: [Double] -> Bool
irrTerminates' cfs = isJust r || isNothing r
  where
    r = internalRateOfReturn (fmap (fromRational . toRational) cfs)

paymentPartsEqualToComposit :: TestTree
paymentPartsEqualToComposit =
  QC.testProperty
    "That compenent parts are always equal to the sum "
    paymentPartsEqualToComposit'

paymentPartsEqualToComposit' ::
  Double -> Int -> Double -> Double -> Bool -> Int -> Bool
paymentPartsEqualToComposit' rate nper pv fv payAtBeginning per =
  pmt == (i + p)
  where
    (i, p) = paymentParts rate nper pv fv payAtBeginning per
    pmt = payment rate nper pv fv payAtBeginning

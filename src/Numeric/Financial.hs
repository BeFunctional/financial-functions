-- | This module defines cannoncial financial functions
-- ported from the numpy financial library.
module Numeric.Financial where

import Data.Default.Class
import Data.Foldable
  ( foldl',
    toList,
  )
import Numeric.AD.Rank1.Forward
  ( Forward,
    diff',
  )
import Numeric.RootFinding

-- | Financial functions for present value, future value, numper of periods, and periodic rate.
-- Following the numpy convention, cashflows out are negative.

--- Expressions are derived from the following equation:
-- `fv + pv*(1+rate)**nper + pmt*(1+rate*when)/rate*((1+rate)**nper-1) = 0`
-- See:  http://docs.oasis-open.org/office/v1.2/os/OpenDocument-v1.2-os-part2.html#PV
-- and: https://github.com/numpy/numpy/blob/v1.15.1/numpy/lib/financial.py#L287-L380

-- | The internal rate of return of a series of cashflows.
--
-- TODO: This is very inefficient as it iteratively attempts
-- to bracket the underlaying irr expression based on a crude guess.
-- >>> import Data.Maybe (fromJust)
-- >>> import Data.Fixed (Pico)
-- >>> realToFrac . fromJust $ internalRateOfReturn [negate 100, 39, 59, 55, 20] :: Pico
-- 0.280948421159
-- >>> realToFrac . fromJust $ internalRateOfReturn [negate 100, 0, 0, 74] :: Pico
-- -0.095495830349
-- >>> realToFrac . fromJust $ internalRateOfReturn [negate 100, 100, 0, negate 7] :: Pico
-- -0.083299666185
-- >>> realToFrac . fromJust $ internalRateOfReturn [negate 100, 100, 0, 7] :: Pico
-- 0.062058485629
-- >>> realToFrac . fromJust $ internalRateOfReturn [negate 5, 10.5, 1, negate 8, 1] :: Pico
-- 0.088598338527
internalRateOfReturn :: (Foldable t) => t (Forward Double) -> Maybe Double
internalRateOfReturn cashFlows
  | isFlat = Just 0.0
  | lc > 1 = go 0.3
  | otherwise = Nothing
  where
    lc = length (takeWhile (/= 0) cfList)
    cfList = toList cashFlows
    isFlat = all (head cfList ==) cfList
    go p
      | p >= 100 = error "internalRateOfReturn: Couldn't bracket underlaying function!"
      | otherwise = case r p of
          NotBracketed -> go (p + 0.1)
          SearchFailed -> Nothing -- error "SearchFailed"
          Root r' -> Just r'
    guess = (sum cashFlows / 100) * fromIntegral (length cashFlows)
    r p =
      newtonRaphson
        def {newtonTol = tol}
        ( negate . fromRational . toRational $ abs (guess * p),
          0,
          fromRational . toRational $ abs (guess * p)
        )
        funcD
    tol = RelTol 1e-8
    cashFlows' = zip (toList cashFlows) ([0 ..] :: [Int])
    funcD :: Double -> (Double, Double)
    funcD = diff' func
    func :: Forward Double -> Forward Double
    func r' = foldl' (func' r') 0 cashFlows'
    func' r' acc (cf, t)
      | t <= 0 = acc + cf
      | t == 1 = acc + (cf / (1 + r'))
      | otherwise = exp' acc cf r' t
    exp' acc' cf' r' t' = acc' + (cf' / ((1 + r') ^ t'))

-- | Calculate the net present value of a series of cashflows.
--
-- >>> netPresentValue 0.08 [negate 40000, 5000, 8000, 12000, 30000]
-- 3065.2226681795255
netPresentValue :: (Foldable t, Fractional b) => b -> t b -> b
netPresentValue rate cashFlows =
  foldl'
    (go rate)
    0
    (zip (toList cashFlows) [0 ..])
  where
    go :: (Fractional a) => a -> a -> (a, Int) -> a
    go r acc (cf, i) = acc + (cf / ((1 + r) ^ i))

-- | Calculate the present value
--
-- >>> presentValue (0.05 / 12) (10 *12) (negate 100) 15692.93 False
-- -100.00067131616947
presentValue ::
  (Eq a, Fractional a, Integral a1) => a -> a1 -> a -> a -> Bool -> a
presentValue rate nper pmt fv payAtBeginning
  | rate /= 0 = pv
  | otherwise = pv'
  where
    a = (1 + rate) ^ nper
    fact = (1 + rate * payType) * (a - 1) / rate
    pv = negate (fv + pmt * fact) / a
    payType = if payAtBeginning then 1.0 else 0
    pv' = negate (pmt * fromIntegral nper) - fv

-- | The Future value of a debt
--
-- >>> futureValue (0.05 / 12) (10*12) (negate 100) (negate 100) False
-- 15692.928894335892
futureValue ::
  (Eq a1, Integral a2, Fractional a1) => a1 -> a2 -> a1 -> a1 -> Bool -> a1
futureValue rate nper pmt pv payAtBeginning
  | rate /= 0 = fv
  | otherwise = fv'
  where
    a = (1 + rate) ^ nper
    fv = negate (pv * a) - ((pmt * (1 + rate * payType) * (a - 1)) / rate)
    payType = if payAtBeginning then 1.0 else 0
    fv' = negate pv - (pmt * fromIntegral nper)

-- | The interest portion of a payment.  Note that the periods
-- start at 1!
--
-- Calculate the interest portion of the first and second payment2:
-- >>> interestPayment (0.0824/12) 1 (1*12) 2500 0 False
-- -17.166666666666668
-- >>> interestPayment (0.0824/12) 2 (1*12) 2500 0 False
-- -15.78933745735078
interestPayment ::
  (Eq p1, Fractional p1, Integral p3, Integral p2) =>
  p1 ->
  p3 ->
  p2 ->
  p1 ->
  p1 ->
  Bool ->
  p1
interestPayment rate per nper pv fv payAtBeginning = ipmt
  where
    pmt = payment rate nper pv fv payAtBeginning
    ipmt = futureValue rate (per - 1) pmt pv payAtBeginning * rate

-- | The principal portion of a payment.  Note that the periods
-- start at 1!
--
-- Calculate the principle portion of the first and second payment2:
-- >>> principalPayment (0.0824/12) 1 (1*12) 2500 0 False
-- -200.5819236867822
-- >>> principalPayment (0.0824/12) 2 (1*12) 2500 0 False
-- -201.9592528960981
principalPayment ::
  (Integral p3, Integral p2, Fractional p1, Eq p1) =>
  p1 ->
  p3 ->
  p2 ->
  p1 ->
  p1 ->
  Bool ->
  p1
principalPayment rate per nper pv fv payAtBeginning =
  payment rate nper pv fv payAtBeginning
    - interestPayment rate per nper pv fv payAtBeginning

-- | The total payment (principal + interest)
--
-- >>> payment (0.075 / 12) (12 * 15) 200000 0 False
-- -1854.0247200054675
payment ::
  (Eq p1, Fractional p1, Integral p2) => p1 -> p2 -> p1 -> p1 -> Bool -> p1
payment rate nper pv fv payAtBeginning
  | rate /= 0 = pmt
  | otherwise = pmt'
  where
    a = (1 + rate) ^ nper
    pmt = (negate fv - (pv * a)) / ((1 + rate * payType) / rate * (a - 1))
    payType = if payAtBeginning then 1.0 else 0
    pmt' = (negate fv - pv) / fromIntegral nper

-- | The individual compoenents of a payment.
--
-- >>> paymentParts (0.0824/12) (1*12) 2500 0 False 1
-- (-200.5819236867822,-17.166666666666668)
paymentParts ::
  (Eq p1, Integral p3, Fractional p1, Integral p2) =>
  p1 ->
  p3 ->
  p1 ->
  p1 ->
  Bool ->
  p2 ->
  (p1, p1)
paymentParts rate nper pv fv payAtBeginning per
  | per >= 1 = (ppay, ipmt)
  | otherwise = error "paymentParts: period `per` must be >= 1"
  where
    pmt = payment rate nper pv fv payAtBeginning
    ipmt = interestPayment rate per nper pv fv payAtBeginning
    ppay = pmt - ipmt

-- | All payment componenets per period for all remaining periods.
--
-- >>> amortizedPayments (0.0824/12) (1*12) 2500 0 False 1
-- [(-200.5819236867822,-17.166666666666668),(-201.9592528960981,-15.78933745735078),(-203.34603976598459,-14.402550587464265),(-204.74234923904433,-13.006241114404535),(-206.1482467038191,-11.600343649629751),(-207.56379799785196,-10.184792355596889),(-208.98906941077053,-8.759520942678323),(-210.42412768739112,-7.324462666057752),(-211.8690400308446,-5.879550322604281),(-213.32387410572304,-4.424716247725815),(-214.78869804124898,-2.9598923121998846),(-216.26358043446552,-1.4850099189833357)]
amortizedPayments ::
  (Eq p1, Integral p2, Fractional p1) =>
  p1 ->
  p2 ->
  p1 ->
  p1 ->
  Bool ->
  p2 ->
  [(p1, p1)]
amortizedPayments rate nper pv fv payAtBeginning p
  | p >= 1 = pmts
  | otherwise = error "amortizedPayments: starting period `p` must be >= 1"
  where
    pmts = fmap (paymentParts rate nper pv fv payAtBeginning) [p .. nper]

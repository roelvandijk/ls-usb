{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad                  ( (>=>), filterM, liftM2 )
import PrettyDevList                  ( ppDeviceList )
import System.Console.CmdArgs
import System.USB
import System.USB.IDDB.LinuxUsbIdRepo ( staticDb )
import Text.PrettyPrint.ANSI.Leijen   ( putDoc )

-------------------------------------------------------------------------------
-- Main

data Options = Options { vid     :: [Int]
                       , pid     :: [Int]
                       , bus     :: [Int]
                       , address :: [Int]
                       } deriving (Show, Data, Typeable)

defaultOpts :: Mode Options
defaultOpts = mode Options
  { vid     = def &= typ "VID"  & text "List devices with this VID"
  , pid     = def &= typ "PID"  & text "List devices with this PID"
  , bus     = def &= typ "BUS"  & text "List devices on this BUS"
  , address = def &= typ "ADDR" & text "List devices with this ADDRESS"
  } &= prog "ls-usb"
    & text "Lists connected USB devices"
    & helpSuffix ["Please ensure you have sufficient rights before running with higher verbosity"]

main :: IO ()
main = do opts <- cmdArgs "ls-usb 0.1, (C) Roel van Dijk 2009" [defaultOpts]
          verbose <- isLoud
          db <- staticDb
          ctx <- newUSBCtx
          putDoc =<< ppDeviceList db verbose
                 =<< filterM (filterFromOpts opts)
                 =<< getDeviceList ctx
          putStrLn ""

filterFromOpts :: Options -> F IO USBDevice
filterFromOpts opts = andF $ map (filterNonEmpty . ($ opts))
                             [ map (descToDevFilter . matchVID . fromIntegral) . vid
                             , map (descToDevFilter . matchPID . fromIntegral) . pid
                             , map matchBus     . bus
                             , map matchDevAddr . address
                             ]

-------------------------------------------------------------------------------
-- Filters

type F m a = a -> m Bool

-- Construct a filter combinator from a binary boolean operator.
boolBinOpToFComb :: Monad m => (Bool -> Bool -> Bool) -> F m a -> F m a -> F m a
boolBinOpToFComb op f g = \x -> liftM2 op (f x) (g x)

(<||>) :: Monad m => F m a -> F m a -> F m a
(<||>) = boolBinOpToFComb (||)

(<&&>) :: Monad m => F m a -> F m a -> F m a
(<&&>) = boolBinOpToFComb (&&)

constF :: Monad m => Bool -> F m a
constF = const . return

andF :: Monad m => [F m a] -> F m a
andF = foldr (<&&>) (constF True)

orF :: Monad m => [F m a] -> F m a
orF = foldr (<||>) (constF False)

filterNonEmpty :: Monad m => [F m a] -> F m a
filterNonEmpty [] = constF True
filterNonEmpty xs = foldr (<||>) (constF False) xs

-------------------------------------------------------------------------------
-- Specific USBDevice filters

descToDevFilter :: F IO USBDeviceDescriptor -> F IO USBDevice
descToDevFilter = (getDeviceDescriptor >=>)

matchVID :: VendorID -> F IO USBDeviceDescriptor
matchVID vid' desc = return $ vid' == deviceIdVendor desc

matchPID :: ProductID -> F IO USBDeviceDescriptor
matchPID pid' desc = return $ pid' == deviceIdProduct desc

matchBus :: Int -> F IO USBDevice
matchBus bus' dev = return . (bus' ==) =<< getBusNumber dev

matchDevAddr :: Int -> F IO USBDevice
matchDevAddr address' dev = return . (address' ==) =<< getDeviceAddress dev

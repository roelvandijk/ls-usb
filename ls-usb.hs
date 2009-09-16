{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad                  ( filterM, fmap, foldM, mapM )
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
                 =<< filterDeviceList (filtersFromOpts opts)
                 =<< getDeviceList ctx
          putStrLn ""

filtersFromOpts :: Options -> [DeviceFilter]
filtersFromOpts opts = concat [ fmap (matchVID . fromIntegral) $ vid     opts
                              , fmap (matchPID . fromIntegral) $ pid     opts
                              , fmap matchBus                  $ bus     opts
                              , fmap matchDevAddr              $ address opts
                              ]
-------------------------------------------------------------------------------
-- Filters

type DeviceFilter = USBDevice -> USBDeviceDescriptor -> IO Bool

filterDeviceList :: [DeviceFilter] -> [USBDevice] -> IO [USBDevice]
filterDeviceList fs devs = foldM applyFilter devs fs

applyFilter :: [USBDevice] -> DeviceFilter -> IO [USBDevice]
applyFilter devs f = fmap (map fst) . filterM (uncurry f) . zip devs
                     =<< mapM getDeviceDescriptor devs

matchVID :: VendorID -> DeviceFilter
matchVID vid' _ desc = return $ vid' == deviceIdVendor desc

matchPID :: ProductID -> DeviceFilter
matchPID pid' _ desc = return $ pid' == deviceIdProduct desc

matchBus :: Int -> DeviceFilter
matchBus bus' dev _ = return . (bus' ==) =<< getBusNumber dev

matchDevAddr :: Int -> DeviceFilter
matchDevAddr address' dev _ = return . (address' ==) =<< getDeviceAddress dev

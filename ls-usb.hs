{-# LANGUAGE DeriveDataTypeable #-}

module Main where

import Control.Monad                  ( (>=>), filterM, liftM2 )
import PrettyDevList                  ( ppDevices
                                      , brightStyle, darkStyle
                                      )
import System.Console.CmdArgs
import System.IO                      ( stdout )
import System.USB
import System.USB.IDDB.LinuxUsbIdRepo ( staticDb )
import Text.PrettyPrint.ANSI.Leijen   ( Doc, SimpleDoc(..)
                                      , displayIO
                                      , putDoc, renderPretty
                                      )

-------------------------------------------------------------------------------
-- Main

data Options = Options { vid      :: [Int]
                       , pid      :: [Int]
                       , bus      :: [Int]
                       , address  :: [Int]
                       , nocolour :: Bool
                       , darker   :: Bool
                       } deriving (Show, Data, Typeable)

defaultOpts :: Mode Options
defaultOpts = mode Options
  { vid      = def &= explicit & flag "vid" & typ "VID"
             & text "List devices with this VID"
  , pid      = def &= explicit & flag "pid" & typ "PID"
             & text "List devices with this PID"
  , bus      = def &= explicit & flag "b" & flag "bus" & typ "BUS"
             & text "List devices on this BUS"
  , address  = def &= explicit & flag "a" & flag "address" & typ "ADDR"
             & text "List devices with this ADDRESS"
  , nocolour = def &= explicit & flag "nc" & flag "nocolour" & flag "nocolor"
             & text "Don't colour the output"
  , darker   = def &= explicit & flag "dark"
             & text "Use darker colours (for bright backgrounds)"
  } &= prog "ls-usb"
    & text "Lists connected USB devices"
    & helpSuffix ["Please ensure you have sufficient rights before running with higher verbosity"]

main :: IO ()
main = do opts <- cmdArgs "ls-usb 0.1, (C) Roel van Dijk 2009" [defaultOpts]
          verbose <- isLoud
          db <- staticDb
          ctx <- newCtx
          let style | darker opts = darkStyle
                    | otherwise   = brightStyle
          printDoc (not $ nocolour opts)
              =<< ppDevices style db verbose
              =<< filterM (filterFromOpts opts)
              =<< getDevices ctx
          putStrLn ""

filterFromOpts :: Options -> F IO Device
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
-- Specific Device filters

descToDevFilter :: F IO DeviceDesc -> F IO Device
descToDevFilter = (getDeviceDesc >=>)

matchVID :: VendorId -> F IO DeviceDesc
matchVID vid' desc = return $ vid' == deviceVendorId desc

matchPID :: ProductId -> F IO DeviceDesc
matchPID pid' desc = return $ pid' == deviceProductId desc

matchBus :: Int -> F IO Device
matchBus bus' dev = return
                  . (bus' ==)
                  . fromIntegral
                  =<< getBusNumber dev

matchDevAddr :: Int -> F IO Device
matchDevAddr address' dev = return
                          . (address' ==)
                          . fromIntegral
                          =<< getDeviceAddress dev

-------------------------------------------------------------------------------
-- Pretty printer utilities

printDoc :: Bool -> Doc -> IO ()
printDoc colour doc | colour    = putDoc doc
                    | otherwise = displayIO stdout
                                  . stripColours
                                  $ renderPretty 0.4 100 doc
    where
      stripColours :: SimpleDoc -> SimpleDoc
      stripColours e@(SEmpty)    = e
      stripColours (SChar   c d) = SChar c   $ stripColours d
      stripColours (SText l s d) = SText l s $ stripColours d
      stripColours (SLine l   d) = SLine l   $ stripColours d
      stripColours (SSGR _    d) = stripColours d

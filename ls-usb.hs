{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

module Main where

-- ansi-wl-pprint
import Text.PrettyPrint.ANSI.Leijen   ( putDoc, plain )

-- base
import Control.Monad                  ( (>>=), (>>), (=<<), fail )
import Data.Bool                      ( Bool(False, True), otherwise )
import Data.Data                      ( Data )
import Data.Function                  ( ($), const, id )
import Data.Int                       ( Int )
import Data.List                      ( filter, foldr, map )
import Data.Typeable                  ( Typeable )
import Data.Word                      ( Word8 )
import Prelude                        ( fromIntegral )
import System.IO                      ( IO, putStrLn )
import Text.Show                      ( Show )

-- base-unicode-symbols
import Data.Function.Unicode          ( (∘) )
import Data.Bool.Unicode              ( (∧), (∨) )
import Data.Eq.Unicode                ( (≡) )

-- CmdArgs
import System.Console.CmdArgs         ( Mode
                                      , (&), (&=)
                                      , mode, def, explicit, flag, typ, text
                                      , prog, helpSuffix, cmdArgs, isLoud
                                      )

-- ls-usb (this program)
import PrettyDevList                  ( ppDevices
                                      , brightStyle, darkStyle
                                      )

-- usb
import System.USB.Initialization      ( newCtx )
import System.USB.Enumeration         ( Device
                                      , getDevices, deviceDesc
                                      , busNumber, deviceAddress
                                      )
import System.USB.Descriptors         ( VendorId, ProductId
                                      , deviceVendorId, deviceProductId
                                      )

-- usb-id-database
import System.USB.IDDB.LinuxUsbIdRepo ( staticDb )


-------------------------------------------------------------------------------
-- Main

data Options = Options { vid      ∷ [Int]
                       , pid      ∷ [Int]
                       , bus      ∷ [Int]
                       , address  ∷ [Int]
                       , nocolour ∷ Bool
                       , darker   ∷ Bool
                       } deriving (Show, Data, Typeable)

defaultOpts ∷ Mode Options
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

main ∷ IO ()
main = do opts    ← cmdArgs "ls-usb 0.1.0.4, (C) Roel van Dijk 2009-2010"
                            [defaultOpts]
          verbose ← isLoud
          db      ← staticDb
          ctx     ← newCtx
          let style | darker opts = darkStyle
                    | otherwise   = brightStyle
          (putDoc ∘ if nocolour opts then plain else id)
              =<< ppDevices style db verbose
              ∘   filter (filterFromOpts opts)
              =<< getDevices ctx
          putStrLn ""

filterFromOpts ∷ Options → F Device
filterFromOpts opts = andF $ map (filterNonEmpty ∘ ($ opts))
                      [ map (matchVID     ∘ fromIntegral) ∘ vid
                      , map (matchPID     ∘ fromIntegral) ∘ pid
                      , map (matchBus     ∘ fromIntegral) ∘ bus
                      , map (matchDevAddr ∘ fromIntegral) ∘ address
                      ]


-------------------------------------------------------------------------------
-- Filters

type F α = α → Bool

-- Construct a filter combinator from a binary boolean operator.
binBoolOpToFComb ∷ (Bool → Bool → Bool) → F α → F α → F α
binBoolOpToFComb (⊗) f g = \x → f x ⊗ g x

(<∨>) ∷ F α → F α → F α
(<∨>) = binBoolOpToFComb (∨)

(<∧>) ∷ F α → F α → F α
(<∧>) = binBoolOpToFComb (∧)

andF ∷ [F α] → F α
andF = foldr (<∧>) (const True)

orF ∷ [F α] → F α
orF = foldr (<∨>) (const False)

filterNonEmpty ∷ [F α] → F α
filterNonEmpty [] = const True
filterNonEmpty xs = foldr (<∨>) (const False) xs

-------------------------------------------------------------------------------
-- Specific Device filters

matchVID ∷ VendorId → F Device
matchVID vid' = (vid' ≡) ∘ deviceVendorId ∘ deviceDesc

matchPID ∷ ProductId → F Device
matchPID pid' = (pid' ≡) ∘ deviceProductId ∘ deviceDesc

matchBus ∷ Word8 → F Device
matchBus bus' = (bus' ≡) ∘ busNumber

matchDevAddr ∷ Word8 → F Device
matchDevAddr addr = (addr ≡) ∘ deviceAddress

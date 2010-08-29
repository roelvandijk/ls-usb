{-# LANGUAGE DeriveDataTypeable, NoImplicitPrelude, UnicodeSyntax #-}

module Main where

-- from ansi-wl-pprint:
import Text.PrettyPrint.ANSI.Leijen    ( putDoc, plain )

-- from base:
import Control.Monad                   ( (>>=), (>>), (=<<), fail )
import Data.Bool                       ( Bool(False, True), otherwise )
import Data.Data                       ( Data )
import Data.Function                   ( ($), const, id )
import Data.Int                        ( Int )
import Data.List                       ( filter, foldr, map )
import Data.Typeable                   ( Typeable )
import Data.Word                       ( Word8 )
import Prelude                         ( fromIntegral )
import System.IO                       ( IO, putStrLn )
import Text.Show                       ( Show )

-- from base-unicode-symbols:
import Data.Function.Unicode           ( (∘) )
import Data.Bool.Unicode               ( (∧), (∨) )
import Data.Eq.Unicode                 ( (≡) )

-- from cmdargs:
import System.Console.CmdArgs.Implicit ( (&=)
                                       , cmdArgs, def, details, explicit, help
                                       , isLoud, name, summary, typ, verbosity
                                       )

-- from ls-usb (this package):
import PrettyDevList                   ( ppDevices
                                       , brightStyle, darkStyle
                                       )

-- from usb:
import System.USB.Initialization       ( newCtx )
import System.USB.Enumeration          ( Device
                                       , getDevices, deviceDesc
                                       , busNumber, deviceAddress
                                       )
import System.USB.Descriptors          ( VendorId, ProductId
                                       , deviceVendorId, deviceProductId
                                       )

-- from usb-id-database:
import System.USB.IDDB.LinuxUsbIdRepo  ( staticDb )


-------------------------------------------------------------------------------
-- Main

data Options = Options { vid      ∷ [Int]
                       , pid      ∷ [Int]
                       , bus      ∷ [Int]
                       , address  ∷ [Int]
                       , nocolour ∷ Bool
                       , darker   ∷ Bool
                       } deriving (Show, Data, Typeable)

defaultOpts ∷ Options
defaultOpts = Options
  { vid      = def &= explicit &= name "vid" &= typ "VID"
             &= help "List devices with this VID"
  , pid      = def &= explicit &= name "pid" &= typ "PID"
             &= help "List devices with this PID"
  , bus      = def &= explicit &= name "b" &= name "bus" &= typ "BUS"
             &= help "List devices on this BUS"
  , address  = def &= explicit &= name "a" &= name "address" &= typ "ADDR"
             &= help "List devices with this ADDRESS"
  , nocolour = def &= explicit &= name "nc" &= name "nocolour" &= name "nocolor"
             &= help "Don't colour the output"
  , darker   = def &= explicit &= name "dark"
             &= help "Use darker colours (for bright backgrounds)"
  } &= verbosity
    &= help "Lists connected USB devices"
    &= summary "ls-usb 0.1.0.5, (C) Roel van Dijk 2009-2010"
    &= details ["Please ensure you have sufficient rights before running with higher verbosity"]

main ∷ IO ()
main = do opts    ← cmdArgs defaultOpts
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

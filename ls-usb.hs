{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE NoImplicitPrelude  #-}
{-# LANGUAGE PackageImports     #-}
{-# LANGUAGE UnicodeSyntax      #-}

{-|
Module     : Main
Copyright  : 2009–2012 Roel van Dijk
License    : BSD3 (see the file LICENSE)
Maintainer : Roel van Dijk <vandijk.roel@gmail.com>
-}
module Main where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

import "ansi-wl-pprint" Text.PrettyPrint.ANSI.Leijen ( putDoc, plain )
import "base" Control.Monad ( (=<<) )
import "base" Data.Bool     ( Bool(False, True), otherwise )
import "base" Data.Data     ( Data )
import "base" Data.Function ( ($), const, id )
import "base" Data.Int      ( Int )
import "base" Data.List     ( (++), filter, foldr, map )
import "base" Data.Typeable ( Typeable )
import "base" Data.Word     ( Word8 )
import "base" Data.Version  ( showVersion )
import "base" Prelude       ( fromIntegral )
import "base" System.IO     ( IO, putStrLn )
import "base" Text.Show     ( Show )
#if __GLASGOW_HASKELL__ < 700
import "base" Control.Monad ( (>>=), (>>), fail )
#endif
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Bool.Unicode     ( (∧), (∨) )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "cmdargs" System.Console.CmdArgs.Implicit
    ( (&=)
    , cmdArgs, def, details, explicit, help
    , isLoud, name, summary, typ, verbosity
    )
import "this" PrettyDevList ( ppDevices, brightStyle, darkStyle )
import "this" Paths_ls_usb  ( version )
import "usb" System.USB.Initialization
    ( Verbosity(PrintNothing), newCtx, setDebug )
import "usb" System.USB.Enumeration
    ( Device, getDevices, deviceDesc, busNumber, deviceAddress )
import "usb" System.USB.Descriptors
    ( VendorId, ProductId, deviceVendorId, deviceProductId )
import "usb-id-database" System.USB.IDDB.LinuxUsbIdRepo  ( staticDb )


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

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
    &= summary ("ls-usb " ++ showVersion version ++ ", (C) Roel van Dijk 2009-2010")
    &= details ["Please ensure you have sufficient rights before running with higher verbosity"]

main ∷ IO ()
main = do opts    ← cmdArgs defaultOpts
          verbose ← isLoud
          db      ← staticDb
          ctx     ← newCtx
          setDebug ctx PrintNothing
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


--------------------------------------------------------------------------------
-- Filters
--------------------------------------------------------------------------------

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


--------------------------------------------------------------------------------
-- Specific Device filters
--------------------------------------------------------------------------------

matchVID ∷ VendorId → F Device
matchVID vid' = (vid' ≡) ∘ deviceVendorId ∘ deviceDesc

matchPID ∷ ProductId → F Device
matchPID pid' = (pid' ≡) ∘ deviceProductId ∘ deviceDesc

matchBus ∷ Word8 → F Device
matchBus bus' = (bus' ≡) ∘ busNumber

matchDevAddr ∷ Word8 → F Device
matchDevAddr addr = (addr ≡) ∘ deviceAddress

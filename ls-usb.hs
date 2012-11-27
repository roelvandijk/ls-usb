{-# LANGUAGE CPP                #-}
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
import "base" Control.Applicative ( (<$>), (<*>) )
import "base" Control.Monad ( (=<<) )
import "base" Data.Bool     ( Bool(False, True), otherwise )
import "base" Data.Function ( ($), const, id )
import "base" Data.Functor  ( fmap )
import "base" Data.Int      ( Int )
import "base" Data.List     ( foldr, map )
import "base" Data.Word     ( Word8 )
import "base" Data.Version  ( showVersion )
import "base" Prelude       ( fromIntegral )
import "base" System.IO     ( IO, putStrLn )
#if __GLASGOW_HASKELL__ < 700
import "base" Control.Monad ( (>>=), (>>), fail )
#endif
import "base-unicode-symbols" Data.Function.Unicode ( (∘) )
import "base-unicode-symbols" Data.Bool.Unicode     ( (∧), (∨) )
import "base-unicode-symbols" Data.Eq.Unicode       ( (≡) )
import "cmdtheline" System.Console.CmdTheLine
  ( Term, TermInfo
  , defTI, flag, opt, optAll, optDoc, optInfo
  , optName, run, termName, termDoc, value, version
  )
import "this" PrettyDevList ( DescribedDevice(..), ppDevices, brightStyle, darkStyle )
import qualified "this" Paths_ls_usb  as This ( version )
import "usb" System.USB.Initialization
    ( Verbosity(PrintNothing), newCtx, setDebug )
import "usb" System.USB.Enumeration
    ( getDevices, busNumber, deviceAddress )
import "usb" System.USB.Descriptors
    ( VendorId, ProductId, getDeviceDesc, deviceVendorId, deviceProductId )
import "usb-id-database" System.USB.IDDB.LinuxUsbIdRepo  ( staticDb )
import qualified "vector" Data.Vector as V


--------------------------------------------------------------------------------
-- Main
--------------------------------------------------------------------------------

termInfo ∷ TermInfo
termInfo = defTI { termName = "ls-usb"
                 , version  = showVersion This.version
                 , termDoc  = "Lists connected USB devices."
                 }

verboseT ∷ Term Bool
verboseT = value $ flag (optInfo ["verbose", "V"])
           { optDoc = "Be verbose." }

colourT ∷ Term Bool
colourT = value $ opt True (optInfo ["colour", "c"])
          { optDoc = "Colour the output." }

darkerT ∷ Term Bool
darkerT = value $ flag (optInfo ["darker"])
          { optDoc = "Use darker colours (for bright backgrounds)." }

vidT ∷ Term [Int]
vidT = value $ optAll [] (optInfo ["vid", "v"])
       { optName = "VID"
       , optDoc = "List devices with this VID."
       }

pidT ∷ Term [Int]
pidT = value $ optAll [] (optInfo ["pid", "p"])
       { optName = "PID"
       , optDoc = "List devices with this PID."
       }

busT ∷ Term [Int]
busT = value $ optAll [] (optInfo ["bus", "b"])
       { optName = "BUS"
       , optDoc = "List devices on this BUS."
       }

addressT ∷ Term [Int]
addressT = value $ optAll [] (optInfo ["address", "a"])
            { optName = "ADDRESS"
            , optDoc = "List devices with this ADDRESS."
            }

main ∷ IO ()
main = run (term, termInfo)

term ∷ Term (IO ())
term = listUSB <$> verboseT
               <*> colourT
               <*> darkerT
               <*> vidT
               <*> pidT
               <*> busT
               <*> addressT

listUSB ∷ Bool → Bool → Bool → [Int] → [Int] → [Int] → [Int] → IO ()
listUSB verbose colour darker vs ps bs as = do
    db  ← staticDb
    ctx ← newCtx
    setDebug ctx PrintNothing

    devs ← fmap (V.toList ∘ V.filter filter) ∘
             V.mapM (\dev -> DD dev <$> getDeviceDesc dev) =<<
               getDevices ctx

    let style | darker    = darkStyle
              | otherwise = brightStyle

    (putDoc ∘ if colour then id else plain) =<< ppDevices style db verbose devs

    putStrLn ""
  where
    filter ∷ F DescribedDevice
    filter = andF $ map filterNonEmpty
                        [ map (matchVID     ∘ fromIntegral) vs
                        , map (matchPID     ∘ fromIntegral) ps
                        , map (matchBus     ∘ fromIntegral) bs
                        , map (matchDevAddr ∘ fromIntegral) as
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

matchVID ∷ VendorId → F DescribedDevice
matchVID vid' = (vid' ≡) ∘ deviceVendorId ∘ deviceDesc

matchPID ∷ ProductId → F DescribedDevice
matchPID pid' = (pid' ≡) ∘ deviceProductId ∘ deviceDesc

matchBus ∷ Word8 → F DescribedDevice
matchBus bus' = (bus' ≡) ∘ busNumber ∘ device

matchDevAddr ∷ Word8 → F DescribedDevice
matchDevAddr addr = (addr ≡) ∘ deviceAddress ∘ device

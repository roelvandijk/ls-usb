{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Prelude hiding (catch)

import Control.Exception     (catch)
import Control.Monad         (liftM)
import Data.ByteString.Char8 (ByteString, unpack)
import Data.Char             (toLower)
import Data.List             (intersperse, transpose)
import Data.Word             (Word8, Word16)
import System.USB
import System.USB.IDDB       (VendorDB, vdbDefault, vendorName)
import Text.PrettyPrint

-------------------------------------------------------------------------------

class Document a where
    doc :: a -> Doc

instance Document Doc where
    doc = id

instance Document Bool where
    doc = text . show

instance Document String where
    doc = text

instance Document ByteString where
    doc = doc . unpack

instance Document Int where
    doc = int

instance Document Word8 where
    doc = int . fromIntegral

instance Document Word16 where
    doc = int . fromIntegral

instance Document BCD4 where
    doc (a, b, c, d) = hcat $ punctuate (char '.') $ map doc [a, b, c, d]

instance Document DeviceStatus where
    doc ds = columns 2 [ "Remote wakeup" .: remoteWakeup ds
                       , "Self powered"  .: selfPowered  ds
                       ]

instance Document USBEndpointDescriptor where
    doc ep = columns 2 [ "Address"         .: endpointAddress       ep
                       , "Attributes"      .: endpointAttributes    ep
                       , "Max packet size" .: endpointMaxPacketSize ep
                       , "Interval"        .: endpointInterval      ep
                       , "Refresh"         .: endpointRefresh       ep
                       , "Synch address"   .: endpointSynchAddress  ep
                       , "Extra"           .: endpointExtra         ep
                       ]

instance Document EndpointAddress where
    doc ea = int (endpointNumber ea)
             <+> char '-'
             <+> doc (endpointDirection ea)

instance Document TransferDirection where
    doc Out = text "host"   <+> text "->" <+> text "device"
    doc In  = text "device" <+> text "->" <+> text "host"

instance Document EndpointSynchronization where
    doc NoSynchronization = text "no synchronization"
    doc es                = text $ map toLower $ show es

instance Document EndpointUsage where
    doc = text . map toLower . show

instance Document EndpointMaxPacketSize where
    doc mps = int (maxPacketSize mps)
              <+> char '-'
              <+> doc (transactionOpportunities mps)

instance Document EndpointTransactionOpportunities where
    doc NoAdditionalTransactions         = text "no additional transactions"
    doc OneAdditionlTransaction          = text "one additional transaction"
    doc TwoAdditionalTransactions        = text "two additional transactions"
    doc ReservedTransactionOpportunities = text "reserved transactional opportunities"

instance Document EndpointTransferType where
    doc (Isochronous s u) = text "isochronous"
                            <+> char '-' <+> text "synch:" <+> doc s
                            <+> char '-' <+> text "usage:" <+> doc u
    doc tt                = text $ map toLower $ show tt

-------------------------------------------------------------------------------

(.:) :: (Document a, Document b) => a -> b -> [Doc]
x .: y = [doc x <> char ':', doc y]

infixr 0 .:

columns :: Int -> [[Doc]] -> Doc
columns s rows = vcat $ map ( hcat
                            . map (uncurry padr)
                            . zip (map (+s) $ columnSizes rows)
                            )
                            rows

columnSizes :: [[Doc]] -> [Int]
columnSizes = map (maximum . map docLen) . transpose

docLen :: Doc -> Int
docLen = length . render

padr :: Int -> Doc -> Doc
padr w d = d <> text (replicate (w - docLen d ) ' ')

padl :: Int -> Doc -> Doc
padl w d = text (replicate (w - docLen d ) ' ') <> d

-------------------------------------------------------------------------------

catchUSBError :: IO a -> (USBError -> IO a) -> IO a
catchUSBError = catch

getDescriptor :: USBDevice -> Ix -> IO Doc
getDescriptor dev ix = catchUSBError ( withUSBDeviceHandle dev $ \devH ->
                                       liftM doc $ getStringDescriptorAscii devH ix 512
                                     ) $ \e -> return $ text "Couldn't retrieve string descriptor:"
                                                        <+> text (show e)

-------------------------------------------------------------------------------

main :: IO ()
main = do db <- vdbDefault
          ctx <- newUSBCtx
          setDebug ctx PrintInfo
          putStrLn . render =<< ppDeviceList db =<< getDeviceList ctx

-------------------------------------------------------------------------------

ppDeviceList :: VendorDB -> [USBDevice] -> IO Doc
ppDeviceList db = liftM (vcat . intersperse (char ' '))
                  . mapM (ppDevice db)

ppDevice :: VendorDB -> USBDevice -> IO Doc
ppDevice db dev = do
    busNum  <- getBusNumber        dev
    devAddr <- getDeviceAddress    dev
    desc    <- getDeviceDescriptor dev
    descDoc <- ppDeviceDescriptor db dev desc

    return $ columns 2 [ "Bus number" .: busNum
                       , "Address"    .: devAddr
                       ]
             $+$ text "Device descriptor:"
             $$  nest 2 descDoc

ppDeviceDescriptor :: VendorDB -> USBDevice -> USBDeviceDescriptor -> IO Doc
ppDeviceDescriptor db dev desc = do
    let ppVendorID vid = doc vid <+> maybe empty
                                           (parens . doc)
                                           (vendorName db (fromIntegral vid))
        manufacturerIx = deviceManufacturerIx desc
        productIx      = deviceProductIx      desc
        serialIx       = deviceSerialNumberIx desc
        numConfigs     = deviceNumConfigs     desc

    configDescriptors <- mapM (getConfigDescriptor dev) [0 .. numConfigs - 1]

    manufacturerDoc <- getDescriptor dev manufacturerIx
    productDoc      <- getDescriptor dev productIx
    serialDoc       <- getDescriptor dev serialIx
    configDocs      <- mapM (ppConfigDescriptor dev) configDescriptors

    return $ columns 2
      [ "USB specification" .: deviceUSBSpecReleaseNumber desc
      , "Class"             .: deviceClass                desc
      , "Sub class"         .: deviceSubClass             desc
      , "Protocol"          .: deviceProtocol             desc
      , "Max packet size"   .: deviceMaxPacketSize0       desc
      , "Vendor ID"         .: ppVendorID (deviceIdVendor desc)
      , "Product ID"        .: deviceIdProduct            desc
      , "Release number"    .: deviceReleaseNumber        desc
      , "Manufacturer"      .: doc manufacturerIx <+> parens manufacturerDoc
      , "Product"           .: doc productIx      <+> parens productDoc
      , "Serial number"     .: doc serialIx       <+> parens serialDoc
      , "Num configs"       .: numConfigs
      ] $+$ vcat (map (\d -> text "Configuration descriptor:" $$ nest 2 d) configDocs)

ppConfigDescriptor :: USBDevice -> USBConfigDescriptor -> IO Doc
ppConfigDescriptor dev conf = do
  let stringIx = configIx         conf
      ifDescs  = configInterfaces conf

  stringDescriptor <- getDescriptor dev stringIx
  ifDescDocs       <- mapM (mapM $ ppInterfaceDescriptor dev) ifDescs

  let vcatAlternatives = ($$) (text "Interface:")
                       . nest 2
                       . vcat
                       . map (($$) (text "Alternative:") . nest 2)

  return $ columns 2
    [ "Value"          .: configValue conf
    , "Descriptor"     .: doc stringIx <+> parens stringDescriptor
    , "Attributes"     .: doc (configAttributes conf)
    , "Max power"      .: doc (2 * configMaxPower conf) <+> text "mA"
    , "Num interfaces" .: configNumInterfaces conf
    , "Extra"          .: configExtra conf
    ] $+$ vcat (map vcatAlternatives ifDescDocs)

ppInterfaceDescriptor :: USBDevice -> USBInterfaceDescriptor -> IO Doc
ppInterfaceDescriptor dev ifDesc = do
  let stringIx = interfaceIx ifDesc

  stringDescriptor <- getDescriptor dev stringIx

  return $ columns 2
    [ "Interface number"    .: interfaceNumber       ifDesc
    , "Alternative setting" .: interfaceAltSetting   ifDesc
    , "Class"               .: interfaceClass        ifDesc
    , "Sub class"           .: interfaceSubClass     ifDesc
    , "Protocol"            .: interfaceProtocol     ifDesc
    , "Descriptor"          .: doc stringIx <+> parens stringDescriptor
    , "Num endpoints"       .: interfaceNumEndpoints ifDesc
    , "Extra"               .: interfaceExtra        ifDesc
    ] $+$ vcat ( map (\e -> text "Endpoint:" $$ nest 2 (doc e))
                     $ interfaceEndpoints ifDesc
               )

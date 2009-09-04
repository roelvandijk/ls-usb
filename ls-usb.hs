{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Prelude hiding (catch)

import Control.Exception            (catch)
import Control.Monad                (liftM)
import Data.ByteString.Char8        (ByteString, unpack)
import Data.Char                    (toLower)
import Data.List                    (intersperse, transpose)
import Data.Word                    (Word8, Word16)
import System.USB
import System.USB.IDDB              (VendorDB, vdbDefault, vendorName)
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf                  (PrintfArg, printf)

-------------------------------------------------------------------------------
-- Program entry point

main :: IO ()
main = do db <- vdbDefault
          ctx <- newUSBCtx
          setDebug ctx PrintInfo
          putDoc =<< ppDeviceList db =<< getDeviceList ctx
          putStrLn ""

-------------------------------------------------------------------------------
-- Pretty printing styles

sectionStyle :: Pretty a => a -> Doc
sectionStyle = underline . bold . white . pretty

fieldStyle :: Pretty a => a -> Doc
fieldStyle = white . pretty

numberStyle :: Pretty a => a -> Doc
numberStyle = yellow . pretty

stringStyle :: Pretty a => a -> Doc
stringStyle = green . pretty

versionStyle :: Pretty a => a -> Doc
versionStyle = cyan . pretty

addressStyle :: Pretty a => a -> Doc
addressStyle = magenta . pretty

-------------------------------------------------------------------------------

field :: String -> Doc
field = fieldStyle . text

section :: String -> Doc
section = sectionStyle . text

-------------------------------------------------------------------------------
-- Some basic instances of the Pretty class

instance Pretty ByteString where
    pretty = pretty . unpack

instance Pretty Word8 where
    pretty = int . fromIntegral

instance Pretty Word16 where
    pretty = int . fromIntegral

-------------------------------------------------------------------------------
-- Miscellaneous pretty printing functions

(.:) :: (Pretty a) => String -> a -> [Doc]
x .: y = [field x <> char ':', pretty y]

infixr 0 .:

columns :: Int -> [[Doc]] -> Doc
columns s rows = vcat $ map ( hcat
                            . map (uncurry fill)
                            . zip (map (+s) $ columnSizes rows)
                            )
                            rows

columnSizes :: [[Doc]] -> [Int]
columnSizes = map (maximum . map docLen) . transpose

-- A bit of a hack to calculate the length of a document without
-- considering colour codes.
docLen :: Doc -> Int
docLen = sdocLen . renderPretty 0.4 100
    where
      sdocLen :: SimpleDoc -> Int
      sdocLen (SEmpty)      = 0
      sdocLen (SChar _ d)   = 1 + sdocLen d
      sdocLen (SText i _ d) = i + sdocLen d
      sdocLen (SLine i d)   = i + sdocLen d
      sdocLen (SSGR _ d)    = sdocLen d

-------------------------------------------------------------------------------
-- USB utility functions

stringBufferSize :: Int
stringBufferSize = 512

catchUSBError :: IO a -> (USBError -> IO a) -> IO a
catchUSBError = catch

getDescriptor :: USBDevice -> Ix -> IO Doc
getDescriptor dev ix = catchUSBError ( withUSBDeviceHandle dev $ \devH ->
                                       liftM (green . pretty)
                                       $ getStringDescriptorAscii devH ix stringBufferSize
                                     )
                                     $ \e -> return
                                           $ dullred
                                           $ text "Couldn't retrieve string descriptor:"
                                             <+> red (text $ show e)

-------------------------------------------------------------------------------
-- Pretty printers for USB types

ppId :: PrintfArg n => n -> Doc
ppId x = text (printf "%04x" x :: String)

ppVendorId :: VendorDB -> Word16 -> Doc
ppVendorId db vid = numberStyle (text "0x" <> ppId vid)
                    <+> ppVendorName db vid

ppVendorName:: VendorDB -> Word16 -> Doc
ppVendorName db vid = maybe empty
                            (parens . stringStyle)
                            (vendorName db (fromIntegral vid))

ppDeviceList :: VendorDB -> [USBDevice] -> IO Doc
ppDeviceList db = liftM (vcat . intersperse (char ' '))
                  . mapM (ppDevice db)

ppDevice :: VendorDB -> USBDevice -> IO Doc
ppDevice db dev = do
    busNum  <- getBusNumber        dev
    devAddr <- getDeviceAddress    dev
    desc    <- getDeviceDescriptor dev
    descDoc <- ppDeviceDescriptor db dev desc

    let vid = deviceIdVendor desc
        pid = deviceIdProduct desc

    return $ text "Bus"
             <+> addressStyle (printf "%03d" busNum :: String)
             <+> text "Device"
             <+> addressStyle (printf "%03d" devAddr :: String)
             <>  char ':'
             <+> text "ID"
             <+> numberStyle (ppId vid)
             <>  char ':'
             <>  numberStyle (ppId pid)
             <+> stringStyle (ppVendorName db vid)
             <$> section "Device descriptor"
             <$> indent 2 descDoc

ppDeviceDescriptor :: VendorDB -> USBDevice -> USBDeviceDescriptor -> IO Doc
ppDeviceDescriptor db dev desc = do
    let manufacturerIx = deviceManufacturerIx desc
        productIx      = deviceProductIx      desc
        serialIx       = deviceSerialNumberIx desc
        numConfigs     = deviceNumConfigs     desc

    configDescriptors <- mapM (getConfigDescriptor dev) [0 .. numConfigs - 1]

    manufacturerDoc <- getDescriptor dev manufacturerIx
    productDoc      <- getDescriptor dev productIx
    serialDoc       <- getDescriptor dev serialIx
    configDocs      <- mapM (ppConfigDescriptor dev) configDescriptors

    return $ columns 2
      [ "USB specification" .: versionStyle  (deviceUSBSpecReleaseNumber desc)
      , "Class"             .: numberStyle   (deviceClass          desc)
      , "Sub class"         .: numberStyle   (deviceSubClass       desc)
      , "Protocol"          .: numberStyle   (deviceProtocol       desc)
      , "Max packet size"   .: numberStyle   (deviceMaxPacketSize0 desc)
      , "Vendor ID"         .: ppVendorId db (deviceIdVendor       desc)
      , "Product ID"        .: numberStyle   (text "0x" <> ppId (deviceIdProduct desc))
      , "Release number"    .: versionStyle  (deviceReleaseNumber  desc)
      , "Manufacturer"      .: addressStyle manufacturerIx <+> parens manufacturerDoc
      , "Product"           .: addressStyle productIx      <+> parens productDoc
      , "Serial number"     .: addressStyle serialIx       <+> parens serialDoc
      , "Num configs"       .: numberStyle numConfigs
      ] <$> vcat ( map (\d -> section "Configuration descriptor"
                              <$> indent 2 d)
                   configDocs
                 )

ppConfigDescriptor :: USBDevice -> USBConfigDescriptor -> IO Doc
ppConfigDescriptor dev conf = do
  let stringIx = configIx         conf
      ifDescs  = configInterfaces conf

  stringDescriptor <- getDescriptor dev stringIx
  ifDescDocs       <- mapM (mapM $ ppInterfaceDescriptor dev) ifDescs

  let vcatAlternatives = (<$>) (section "Interface")
                       . indent 2
                       . vcat
                       . map ((<$>) (section "Alternative") . indent 2)

  return $ columns 2
    [ "Value"          .: numberStyle (configValue conf)
    , "Descriptor"     .: addressStyle stringIx <+> parens stringDescriptor
    , "Attributes"     .: pretty (configAttributes conf)
    , "Max power"      .: pretty (2 * configMaxPower conf) <+> text "mA"
    , "Num interfaces" .: numberStyle (configNumInterfaces conf)
    ] <$> vcat (map vcatAlternatives ifDescDocs)

ppInterfaceDescriptor :: USBDevice -> USBInterfaceDescriptor -> IO Doc
ppInterfaceDescriptor dev ifDesc = do
  let stringIx = interfaceIx ifDesc

  stringDescriptor <- getDescriptor dev stringIx

  return $ columns 2
    [ "Interface number"    .: numberStyle (interfaceNumber       ifDesc)
    , "Alternative setting" .: numberStyle (interfaceAltSetting   ifDesc)
    , "Class"               .: numberStyle (interfaceClass        ifDesc)
    , "Sub class"           .: numberStyle (interfaceSubClass     ifDesc)
    , "Protocol"            .: numberStyle (interfaceProtocol     ifDesc)
    , "Descriptor"          .: addressStyle stringIx <+> parens stringDescriptor
    , "Num endpoints"       .: numberStyle (interfaceNumEndpoints ifDesc)
    ] <$> vcat ( map (\e -> section "Endpoint" <$> indent 2 (pretty e))
                     $ interfaceEndpoints ifDesc
               )
-------------------------------------------------------------------------------
-- USB specific instances of Pretty

instance Pretty BCD4 where
    pretty (a, b, c, d) = hcat $ punctuate (char '.') $ map pretty [a, b, c, d]

instance Pretty DeviceStatus where
    pretty ds = align
           $ columns 2 [ "Remote wakeup" .: remoteWakeup ds
                       , "Self powered"  .: selfPowered  ds
                       ]

instance Pretty USBEndpointDescriptor where
    pretty ep = columns 2 [ "Address"         .: endpointAddress       ep
                       , "Attributes"      .: endpointAttributes    ep
                       , "Max packet size" .: endpointMaxPacketSize ep
                       , "Interval"        .: numberStyle  (endpointInterval      ep)
                       , "Refresh"         .: numberStyle  (endpointRefresh       ep)
                       , "Synch address"   .: addressStyle (endpointSynchAddress  ep)
                       ]

instance Pretty EndpointAddress where
    pretty ea = addressStyle (endpointNumber ea)
                <+> char '-'
                <+> pretty (endpointDirection ea)

instance Pretty TransferDirection where
    pretty Out = text "host"   <+> text "->" <+> text "device"
    pretty In  = text "device" <+> text "->" <+> text "host"

instance Pretty EndpointSynchronization where
    pretty NoSynchronization = text "no synchronization"
    pretty es                = text $ map toLower $ show es

instance Pretty EndpointUsage where
    pretty = text . map toLower . show

instance Pretty EndpointMaxPacketSize where
    pretty mps = numberStyle (maxPacketSize mps)
                 <+> char '-'
                 <+> pretty (transactionOpportunities mps)

instance Pretty EndpointTransactionOpportunities where
    pretty NoAdditionalTransactions         = text "no additional transactions"
    pretty OneAdditionlTransaction          = text "one additional transaction"
    pretty TwoAdditionalTransactions        = text "two additional transactions"
    pretty ReservedTransactionOpportunities = text "reserved transactional opportunities"

instance Pretty EndpointTransferType where
    pretty (Isochronous s u) = text "isochronous"
                               <+> char '-' <+> text "synch:" <+> pretty s
                               <+> char '-' <+> text "usage:" <+> pretty u
    pretty tt                = text $ map toLower $ show tt

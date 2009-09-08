{-# LANGUAGE TypeSynonymInstances #-}

module Main where

import Prelude hiding (catch)

import Control.Exception              (catch)
import Control.Monad                  (liftM)
import Data.ByteString.Char8          (ByteString, unpack)
import Data.Char                      (toLower)
import Data.List                      (intersperse, transpose)
import Data.Word                      (Word8, Word16)
import System.Console.GetOpt          ( OptDescr(..)
                                      , ArgDescr(..)
                                      , ArgOrder(..)
                                      , getOpt
                                      , usageInfo
                                      )
import System.Environment             (getArgs)
import System.USB
import System.USB.IDDB                (IDDB, vendorName, productName)
import System.USB.IDDB.LinuxUsbIdRepo (staticDb)
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf                    (PrintfArg, printf)

-------------------------------------------------------------------------------
-- Main

data Flag = Verbose | Help deriving Eq

options :: [OptDescr Flag]
options = [ Option ['v'] ["verbose"] (NoArg Verbose)
                   "Give verbose output"
          , Option ['h'] ["help"] (NoArg Help)
                   "Shows information regarding the usage of this program"
          ]

header :: String
header = "Usage: ls-usb [OPTION...]"

parseArgs :: IO (Either String [Flag])
parseArgs = do args <- getArgs
               return $ case getOpt RequireOrder options args of
                 (os, _, []) -> Right os
                 (_,  _, es) -> Left $ concat es ++ usageInfo header options

main :: IO ()
main = either putStrLn prog =<< parseArgs
    where
      prog flags = do let help    = Help    `elem` flags
                          verbose = Verbose `elem` flags
                      if help
                        then putStrLn $ usageInfo header options
                        else do db <- staticDb
                                ctx <- newUSBCtx
                                putDoc =<< ppDeviceList db verbose
                                       =<< getDeviceList ctx
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

-------------------------------------------------------------------------------
-- Pretty printers for USB types

ppStringDescr :: USBDevice -> Ix -> (Doc -> Doc) -> IO Doc
ppStringDescr _   0  _ = return empty
ppStringDescr dev ix f = catchUSBError
                           ( withUSBDeviceHandle dev $ \devH ->
                               liftM (green . pretty)
                                     $ getStringDescriptorAscii devH
                                                                ix
                                                                stringBufferSize
                           )
                           (return . ppErr)
    where ppErr e = f $ dullred $ text "Couldn't retrieve string descriptor:"
                                  <+> red (text $ show e)

ppId :: PrintfArg n => n -> Doc
ppId x = text (printf "%04x" x :: String)

ppVendorId :: IDDB -> Word16 -> Doc
ppVendorId db vid = numberStyle (text "0x" <> ppId vid)
                    <+> ppVendorName db vid parens

ppProductId :: IDDB -> Word16 -> Word16 -> Doc
ppProductId db vid pid = numberStyle (text "0x" <> ppId pid)
                         <+> ppProductName db vid pid parens

ppVendorName :: IDDB -> Word16 -> (Doc -> Doc) -> Doc
ppVendorName db vid f = maybe empty (f . stringStyle) $ vendorName db vid'
    where vid' = fromIntegral vid

ppProductName :: IDDB -> Word16 -> Word16 -> (Doc -> Doc) -> Doc
ppProductName db vid pid f = maybe empty (f . stringStyle) $ productName db vid' pid'
    where vid' = fromIntegral vid
          pid' = fromIntegral pid

ppDeviceList :: IDDB -> Bool -> [USBDevice] -> IO Doc
ppDeviceList db False = liftM vcat . mapM (ppDeviceShort db)
ppDeviceList db True  = liftM (vcat . intersperse (char ' '))
                      . mapM (ppDevice db)

ppDeviceShort :: IDDB -> USBDevice -> IO Doc
ppDeviceShort db dev = do
    busNum  <- getBusNumber        dev
    devAddr <- getDeviceAddress    dev
    desc    <- getDeviceDescriptor dev

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
             <+> ppVendorName db vid (<+> ppProductName db vid pid (char '-' <+>))

ppDevice :: IDDB -> USBDevice -> IO Doc
ppDevice db dev = do shortDesc <- ppDeviceShort db dev
                     desc      <- getDeviceDescriptor dev
                     descDoc   <- ppDeviceDescriptor db dev desc

                     return $ shortDesc
                              <$> section "Device descriptor"
                              <$> indent 2 descDoc

ppDeviceDescriptor :: IDDB -> USBDevice -> USBDeviceDescriptor -> IO Doc
ppDeviceDescriptor db dev desc = do
    let manufacturerIx = deviceManufacturerIx desc
        productIx      = deviceProductIx      desc
        serialIx       = deviceSerialNumberIx desc
        numConfigs     = deviceNumConfigs     desc

    configDescriptors <- mapM (getConfigDescriptor dev) [0 .. numConfigs - 1]

    manufacturerDoc <- ppStringDescr dev manufacturerIx parens
    productDoc      <- ppStringDescr dev productIx      parens
    serialDoc       <- ppStringDescr dev serialIx       parens
    configDocs      <- mapM (ppConfigDescriptor dev) configDescriptors

    return $ columns 2
      [ "USB specification" .: versionStyle  (deviceUSBSpecReleaseNumber desc)
      , "Class"             .: numberStyle   (deviceClass          desc)
      , "Sub class"         .: numberStyle   (deviceSubClass       desc)
      , "Protocol"          .: numberStyle   (deviceProtocol       desc)
      , "Max packet size"   .: numberStyle   (deviceMaxPacketSize0 desc)
      , "Vendor ID"         .: ppVendorId  db (deviceIdVendor desc)
      , "Product ID"        .: ppProductId db (deviceIdVendor desc) (deviceIdProduct desc)
      , "Release number"    .: versionStyle  (deviceReleaseNumber  desc)
      , "Manufacturer"      .: addressStyle manufacturerIx <+> manufacturerDoc
      , "Product"           .: addressStyle productIx      <+> productDoc
      , "Serial number"     .: addressStyle serialIx       <+> serialDoc
      , "Num configs"       .: numberStyle numConfigs
      ] <$> vcat ( map (\d -> section "Configuration descriptor"
                              <$> indent 2 d)
                   configDocs
                 )

ppConfigDescriptor :: USBDevice -> USBConfigDescriptor -> IO Doc
ppConfigDescriptor dev conf = do
  let stringIx = configIx         conf
      ifDescs  = configInterfaces conf

  stringDescriptor <- ppStringDescr dev stringIx parens
  ifDescDocs       <- mapM (mapM $ ppInterfaceDescriptor dev) ifDescs

  let vcatAlternatives = (<$>) (section "Interface")
                       . indent 2
                       . vcat
                       . map ((<$>) (section "Alternative") . indent 2)

  return $ columns 2
    [ "Value"          .: numberStyle (configValue conf)
    , "Descriptor"     .: addressStyle stringIx <+> stringDescriptor
    , "Attributes"     .: pretty (configAttributes conf)
    , "Max power"      .: pretty (2 * configMaxPower conf) <+> text "mA"
    , "Num interfaces" .: numberStyle (configNumInterfaces conf)
    ] <$> vcat (map vcatAlternatives ifDescDocs)

ppInterfaceDescriptor :: USBDevice -> USBInterfaceDescriptor -> IO Doc
ppInterfaceDescriptor dev ifDesc = do
  let stringIx = interfaceIx ifDesc

  stringDescriptor <- ppStringDescr dev stringIx parens

  return $ columns 2
    [ "Interface number"    .: numberStyle (interfaceNumber       ifDesc)
    , "Alternative setting" .: numberStyle (interfaceAltSetting   ifDesc)
    , "Class"               .: numberStyle (interfaceClass        ifDesc)
    , "Sub class"           .: numberStyle (interfaceSubClass     ifDesc)
    , "Protocol"            .: numberStyle (interfaceProtocol     ifDesc)
    , "Descriptor"          .: addressStyle stringIx <+> stringDescriptor
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

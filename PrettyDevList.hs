module PrettyDevList (ppDeviceList) where

import Prelude hiding ( catch )

import Control.Exception              ( catch )
import Control.Monad                  ( liftM, mapM )
import Data.ByteString.Char8          ( ByteString, unpack )
import Data.Char                      ( toLower )
import Data.List                      ( intersperse, transpose )
import Data.Word                      ( Word8, Word16 )
import System.USB
import System.USB.IDDB                ( IDDB
                                      , vendorName
                                      , productName
                                      , className
                                      , subClassName
                                      , protocolName
                                      )
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf                    ( PrintfArg, printf )

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

field :: String -> [Doc] -> [Doc]
field name xs = (fieldStyle (text name) <> char ':') : xs

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

columns :: Int -> [[Doc]] -> Doc
columns s rows = vcat $ map ( hcat
                            . zipWith fill (map (+s) $ columnSizes rows)
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

ppBCD4 :: BCD4 -> Doc
ppBCD4 (a, b, c, d) = hcat . punctuate (char '.') $ map pretty [a, b, c, d]

ppStringDescr :: USBDevice -> Ix -> (Doc -> Doc) -> IO Doc
ppStringDescr _   0  _ = return empty
ppStringDescr dev ix f = catchUSBError
                           ( withUSBDeviceHandle dev $ \devH ->
                               liftM (f . green . pretty)
                                     $ getStringDescriptorAscii devH
                                                                ix
                                                                stringBufferSize
                           )
                           (return . ppErr)
    where ppErr e = f . dullred $ text "Couldn't retrieve string descriptor:"
                                  <+> red (text $ show e)

ppId :: PrintfArg n => n -> Doc
ppId x = text (printf "%04x" x :: String)

ppVendorName :: IDDB -> Word16 -> (Doc -> Doc) -> Doc
ppVendorName db vid f = maybe empty (f . stringStyle) $ vendorName db vid'
    where vid' = fromIntegral vid

ppProductName :: IDDB -> Word16 -> Word16 -> (Doc -> Doc) -> Doc
ppProductName db vid pid f = maybe empty (f . stringStyle) $ productName db vid' pid'
    where vid' = fromIntegral vid
          pid' = fromIntegral pid

ppDevClass :: IDDB -> Word8 -> (Doc -> Doc) -> Doc
ppDevClass _  0   f = f $ text "defined at interface level"
ppDevClass db cid f = ppClass db cid f

ppDevSubClass :: IDDB -> Word8 -> Word8 -> (Doc -> Doc) -> Doc
ppDevSubClass _  0   0    f = f $ text "defined at interface level"
ppDevSubClass db cid scid f = ppSubClass db cid scid f

ppDevProtocol :: IDDB -> Word8 -> Word8 -> Word8 -> (Doc -> Doc) -> Doc
ppDevProtocol _  0   0    0      f = f $ text "defined at interface level"
ppDevProtocol db cid scid protId f = ppProtocol db cid scid protId f

ppClass :: IDDB -> Word8 -> (Doc -> Doc) -> Doc
ppClass db cid f = maybe empty (f . stringStyle) $ className db cid'
    where cid' = fromIntegral cid

ppSubClass :: IDDB -> Word8 -> Word8 -> (Doc -> Doc) -> Doc
ppSubClass db cid scid f = maybe empty (f . stringStyle) $ subClassName db cid' scid'
    where cid'  = fromIntegral cid
          scid' = fromIntegral scid

ppProtocol :: IDDB -> Word8 -> Word8 -> Word8 -> (Doc -> Doc) -> Doc
ppProtocol db cid scid protId f = maybe empty (f . stringStyle)
                                        $ protocolName db cid' scid' protId'
    where cid'    = fromIntegral cid
          scid'   = fromIntegral scid
          protId' = fromIntegral protId

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
        classId        = deviceClass          desc
        subClassId     = deviceSubClass       desc
        protocolId     = deviceProtocol       desc
        vendorId       = deviceIdVendor       desc
        productId      = deviceIdProduct      desc

    configDescriptors <- mapM (getConfigDescriptor dev) [0 .. numConfigs - 1]

    manufacturerDoc <- ppStringDescr dev manufacturerIx parens
    productDoc      <- ppStringDescr dev productIx      parens
    serialDoc       <- ppStringDescr dev serialIx       parens
    configDocs      <- mapM (ppConfigDescriptor db dev) configDescriptors

    let classDoc    = ppDevClass    db classId                       parens
        subClassDoc = ppDevSubClass db classId subClassId            parens
        protocolDoc = ppDevProtocol db classId subClassId protocolId parens

    return $ columns 2
      [ field "USB specification" [versionStyle (ppBCD4 $ deviceUSBSpecReleaseNumber desc)]
      , field "Class"             [numberStyle classId, classDoc]
      , field "Sub class"         [numberStyle subClassId, subClassDoc]
      , field "Protocol"          [numberStyle protocolId, protocolDoc]
      , field "Max packet size"   [numberStyle (deviceMaxPacketSize0 desc)]
      , field "Vendor ID"         [ numberStyle $ text "0x" <> ppId vendorId
                                  , ppVendorName db vendorId parens
                                  ]
      , field "Product ID"        [ numberStyle $ text "0x" <> ppId productId
                                  , ppProductName db vendorId productId parens
                                  ]
      , field "Release number"    [versionStyle (ppBCD4 $ deviceReleaseNumber  desc)]
      , field "Manufacturer"      [addressStyle manufacturerIx, manufacturerDoc]
      , field "Product"           [addressStyle productIx,      productDoc]
      , field "Serial number"     [addressStyle serialIx,       serialDoc]
      , field "Num configs"       [numberStyle numConfigs]
      ] <$> vcat ( map (\d -> section "Configuration descriptor"
                              <$> indent 2 d)
                   configDocs
                 )

ppConfigDescriptor :: IDDB -> USBDevice -> USBConfigDescriptor -> IO Doc
ppConfigDescriptor db dev conf = do
  let stringIx = configIx         conf
      ifDescs  = configInterfaces conf

  stringDescriptor <- ppStringDescr dev stringIx parens
  ifDescDocs       <- mapM (mapM $ ppInterfaceDescriptor db dev) ifDescs

  let vcatAlternatives = (<$>) (section "Interface")
                       . indent 2
                       . vcat
                       . map ((<$>) (section "Alternative") . indent 2)

  return $ columns 2
    [ field "Value"          [numberStyle (configValue conf)]
    , field "Descriptor"     [addressStyle stringIx, stringDescriptor]
    , field "Attributes"     [pretty (configAttributes conf)]
    , field "Max power"      [pretty (2 * configMaxPower conf) <+> text "mA"]
    , field "Num interfaces" [numberStyle (configNumInterfaces conf)]
    ] <$> vcat (map vcatAlternatives ifDescDocs)

ppInterfaceDescriptor :: IDDB -> USBDevice -> USBInterfaceDescriptor -> IO Doc
ppInterfaceDescriptor db dev ifDesc = do
  let stringIx    = interfaceIx       ifDesc
      classId     = interfaceClass    ifDesc
      subClassId  = interfaceSubClass ifDesc
      protocolId  = interfaceProtocol ifDesc
      classDoc    = ppClass    db classId                       parens
      subClassDoc = ppSubClass db classId subClassId            parens
      protocolDoc = ppProtocol db classId subClassId protocolId parens

  stringDescriptor <- ppStringDescr dev stringIx parens

  return $ columns 2
    [ field "Interface number"    [numberStyle (interfaceNumber       ifDesc)]
    , field "Alternative setting" [numberStyle (interfaceAltSetting   ifDesc)]
    , field "Class"               [numberStyle classId, classDoc]
    , field "Sub class"           [numberStyle subClassId, subClassDoc]
    , field "Protocol"            [numberStyle protocolId, protocolDoc]
    , field "Descriptor"          [addressStyle stringIx, stringDescriptor]
    , field "Num endpoints"       [numberStyle (interfaceNumEndpoints ifDesc)]
    ] <$> vcat ( map (\e -> section "Endpoint" <$> indent 2 (pretty e))
                     $ interfaceEndpoints ifDesc
               )
-------------------------------------------------------------------------------
-- USB specific instances of Pretty

instance Pretty DeviceStatus where
    pretty ds = align
           $ columns 2 [ field "Remote wakeup" [pretty $ remoteWakeup ds]
                       , field "Self powered"  [pretty $ selfPowered  ds]
                       ]

instance Pretty USBEndpointDescriptor where
    pretty ep = columns 2 [ field "Address"         [pretty $ endpointAddress       ep]
                          , field "Attributes"      [pretty $ endpointAttributes    ep]
                          , field "Max packet size" [pretty $ endpointMaxPacketSize ep]
                          , field "Interval"        [numberStyle  (endpointInterval      ep)]
                          , field "Refresh"         [numberStyle  (endpointRefresh       ep)]
                          , field "Synch address"   [addressStyle (endpointSynchAddress  ep)]
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
    pretty es                = text . map toLower $ show es

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
    pretty tt                = text . map toLower $ show tt

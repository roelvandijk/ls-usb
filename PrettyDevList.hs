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
sectionStyle = underline . bold . pretty

fieldStyle :: Pretty a => a -> Doc
fieldStyle = pretty

usbNumVal :: Pretty a => a -> Doc
usbNumVal = yellow . pretty

usbStrVal :: Pretty a => a -> Doc
usbStrVal = pretty

stringStyle :: Pretty a => a -> Doc
stringStyle = green . pretty

descriptorStyle :: Pretty a => a -> Doc
descriptorStyle = dquotes . cyan . pretty

descrAddrStyle :: Pretty a => a -> Doc
descrAddrStyle = bold . cyan . pretty

versionStyle :: Pretty a => a -> Doc
versionStyle = yellow . pretty

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

catchUSBException :: IO a -> (USBException -> IO a) -> IO a
catchUSBException = catch

-------------------------------------------------------------------------------
-- Pretty printers for USB types

unknown :: Doc
unknown = text "unknown"

ppBCD4 :: BCD4 -> Doc
ppBCD4 (a, b, c, d) = hcat . punctuate (char '.') $ map pretty [a, b, c, d]

ppStringDescr :: Device -> StrIx -> IO Doc
ppStringDescr _   0  = return empty
ppStringDescr dev ix = catchUSBException
                           ( withDeviceHandle dev $ \devH ->
                               liftM descriptorStyle
                                     $ getStringDescriptorAscii devH
                                                                ix
                                                                stringBufferSize
                           )
                           (return . ppErr)
    where ppErr e = dquotes . dullred $ text "Couldn't retrieve string descriptor:"
                                        <+> red (text $ show e)

ppId :: PrintfArg n => n -> Doc
ppId x = text (printf "%04x" x :: String)

ppVendorName :: IDDB -> Word16 -> (Doc -> Doc) -> Doc
ppVendorName db vid f = maybe unknown (f . stringStyle) $ vendorName db vid'
    where vid' = fromIntegral vid

ppProductName :: IDDB -> Word16 -> Word16 -> (Doc -> Doc) -> Doc
ppProductName db vid pid f = maybe unknown (f . stringStyle)
                           $ productName db vid' pid'
    where vid' = fromIntegral vid
          pid' = fromIntegral pid

ppDevClass :: IDDB -> Word8 -> Doc
ppDevClass _  0   = text "defined at interface level"
ppDevClass db cid = ppClass db cid

ppDevSubClass :: IDDB -> Word8 -> Word8 -> Doc
ppDevSubClass _  0   0    = text "defined at interface level"
ppDevSubClass db cid scid = ppSubClass db cid scid

ppDevProtocol :: IDDB -> Word8 -> Word8 -> Word8 -> Doc
ppDevProtocol _  0   0    0      = text "defined at interface level"
ppDevProtocol db cid scid protId = ppProtocol db cid scid protId

ppClass :: IDDB -> Word8 -> Doc
ppClass db cid = maybe unknown stringStyle $ className db cid'
    where cid' = fromIntegral cid

ppSubClass :: IDDB -> Word8 -> Word8 -> Doc
ppSubClass db cid scid = maybe unknown stringStyle
                       $ subClassName db cid' scid'
    where cid'  = fromIntegral cid
          scid' = fromIntegral scid

ppProtocol :: IDDB -> Word8 -> Word8 -> Word8 -> Doc
ppProtocol db cid scid protId = maybe unknown stringStyle
                              $ protocolName db cid' scid' protId'
    where cid'    = fromIntegral cid
          scid'   = fromIntegral scid
          protId' = fromIntegral protId

ppDeviceList :: IDDB -> Bool -> [Device] -> IO Doc
ppDeviceList db False = liftM vcat . mapM (ppDeviceShort db)
ppDeviceList db True  = liftM (vcat . intersperse (char ' '))
                      . mapM (ppDevice db)

ppDeviceShort :: IDDB -> Device -> IO Doc
ppDeviceShort db dev = do
    busNum  <- getBusNumber        dev
    devAddr <- getDeviceAddress    dev
    desc    <- getDeviceDescriptor dev

    let vid = deviceVendorId  desc
        pid = deviceProductId desc

    return $ text "Bus"
             <+> addressStyle (printf "%03d" busNum :: String)
             <+> text "Device"
             <+> addressStyle (printf "%03d" devAddr :: String)
             <>  char ':'
             <+> text "ID"
             <+> usbNumVal (ppId vid)
             <>  char ':'
             <>  usbNumVal (ppId pid)
             <+> ppVendorName db vid (<+> ppProductName db vid pid (char '-' <+>))

ppDevice :: IDDB -> Device -> IO Doc
ppDevice db dev = do shortDesc <- ppDeviceShort db dev
                     desc      <- getDeviceDescriptor dev
                     descDoc   <- ppDeviceDescriptor db dev desc

                     return $ shortDesc
                              <$> section "Device descriptor"
                              <$> indent 2 descDoc

ppDeviceDescriptor :: IDDB -> Device -> DeviceDescriptor -> IO Doc
ppDeviceDescriptor db dev desc = do
    let manufacturerIx = deviceManufacturerStrIx desc
        productIx      = deviceProductStrIx      desc
        serialIx       = deviceSerialNumberStrIx desc
        numConfigs     = deviceNumConfigs        desc
        classId        = deviceClass             desc
        subClassId     = deviceSubClass          desc
        protocolId     = deviceProtocol          desc
        vendorId       = deviceVendorId          desc
        productId      = deviceProductId         desc

    configDescriptors <- mapM (getConfigDescriptor dev)
                              [0 .. fromIntegral numConfigs - 1]

    manufacturerDoc <- ppStringDescr dev manufacturerIx
    productDoc      <- ppStringDescr dev productIx
    serialDoc       <- ppStringDescr dev serialIx
    configDocs      <- mapM (ppConfigDescriptor db dev) configDescriptors

    let classDoc    = ppDevClass    db classId
        subClassDoc = ppDevSubClass db classId subClassId
        protocolDoc = ppDevProtocol db classId subClassId protocolId

    return $ columns 2
      [ field "USB specification" [versionStyle (ppBCD4 $ deviceUSBSpecReleaseNumber desc)]
      , field "Class"             [usbNumVal classId, classDoc]
      , field "Sub class"         [usbNumVal subClassId, subClassDoc]
      , field "Protocol"          [usbNumVal protocolId, protocolDoc]
      , field "Max packet size"   [usbNumVal (deviceMaxPacketSize0 desc)]
      , field "Vendor ID"         [ usbNumVal $ text "0x" <> ppId vendorId
                                  , ppVendorName db vendorId id
                                  ]
      , field "Product ID"        [ usbNumVal $ text "0x" <> ppId productId
                                  , ppProductName db vendorId productId id
                                  ]
      , field "Release number"    [versionStyle (ppBCD4 $ deviceReleaseNumber  desc)]
      , field "Manufacturer"      [descrAddrStyle manufacturerIx, manufacturerDoc]
      , field "Product"           [descrAddrStyle productIx,      productDoc]
      , field "Serial number"     [descrAddrStyle serialIx,       serialDoc]
      , field "Num configs"       [usbNumVal numConfigs]
      ] <$> vcat ( map (\d -> section "Configuration descriptor"
                              <$> indent 2 d)
                   configDocs
                 )

ppConfigDescriptor :: IDDB -> Device -> ConfigDescriptor -> IO Doc
ppConfigDescriptor db dev conf = do
  let stringIx = configStrIx         conf
      ifDescs  = configInterfaces conf

  stringDescriptor <- ppStringDescr dev stringIx
  ifDescDocs       <- mapM (mapM $ ppInterfaceDescriptor db dev) ifDescs

  let vcatAlternatives = (<$>) (section "Interface")
                       . indent 2
                       . vcat
                       . map ((<$>) (section "Alternative") . indent 2)

  return $ columns 2
    [ field "Value"          [usbNumVal (configValue conf)]
    , field "Descriptor"     [descrAddrStyle stringIx, stringDescriptor]
    , field "Attributes"     [pretty (configAttributes conf)]
    , field "Max power"      [usbNumVal (2 * configMaxPower conf) <+> text "mA"]
    , field "Num interfaces" [usbNumVal (configNumInterfaces conf)]
    ] <$> vcat (map vcatAlternatives ifDescDocs)

ppInterfaceDescriptor :: IDDB -> Device -> InterfaceDescriptor -> IO Doc
ppInterfaceDescriptor db dev ifDesc = do
  let stringIx    = interfaceStrIx    ifDesc
      classId     = interfaceClass    ifDesc
      subClassId  = interfaceSubClass ifDesc
      protocolId  = interfaceProtocol ifDesc
      classDoc    = ppClass    db classId
      subClassDoc = ppSubClass db classId subClassId
      protocolDoc = ppProtocol db classId subClassId protocolId

  stringDescriptor <- ppStringDescr dev stringIx

  return $ columns 2
    [ field "Interface number"    [usbNumVal (interfaceNumber       ifDesc)]
    , field "Alternative setting" [usbNumVal (interfaceAltSetting   ifDesc)]
    , field "Class"               [usbNumVal classId, classDoc]
    , field "Sub class"           [usbNumVal subClassId, subClassDoc]
    , field "Protocol"            [usbNumVal protocolId, protocolDoc]
    , field "Descriptor"          [descrAddrStyle stringIx, stringDescriptor]
    , field "Num endpoints"       [usbNumVal (interfaceNumEndpoints ifDesc)]
    ] <$> vcat ( map (\e -> section "Endpoint" <$> indent 2 (pretty e))
                     $ interfaceEndpoints ifDesc
               )
-------------------------------------------------------------------------------
-- USB specific instances of Pretty

instance Pretty DeviceStatus where
    pretty ds = align
           $ columns 2 [ field "Remote wakeup" [usbNumVal $ remoteWakeup ds]
                       , field "Self powered"  [usbNumVal $ selfPowered  ds]
                       ]

instance Pretty EndpointDescriptor where
    pretty ep = columns 2 [ field "Address"         [pretty $ endpointAddress       ep]
                          , field "Attributes"      [pretty $ endpointAttributes    ep]
                          , field "Max packet size" [pretty $ endpointMaxPacketSize ep]
                          , field "Interval"        [usbNumVal (endpointInterval      ep)]
                          , field "Refresh"         [usbNumVal (endpointRefresh       ep)]
                          , field "Synch address"   [usbNumVal (endpointSynchAddress  ep)]
                          ]

instance Pretty EndpointAddress where
    pretty ea = usbNumVal (endpointNumber ea)
                <+> char '-'
                <+> pretty (endpointDirection ea)

instance Pretty TransferDirection where
    pretty Out = usbStrVal $ text "host"   <+> text "->" <+> text "device"
    pretty In  = usbStrVal $ text "device" <+> text "->" <+> text "host"

instance Pretty Synchronization where
    pretty NoSynchronization = usbStrVal "no synchronization"
    pretty es                = usbStrVal . map toLower $ show es

instance Pretty Usage where
    pretty = text . map toLower . show

instance Pretty MaxPacketSize where
    pretty mps = usbNumVal (maxPacketSize mps)
                 <+> char '-'
                 <+> pretty (transactionOpportunities mps)

instance Pretty TransactionOpportunities where
    pretty NoAdditionalTransactions  = usbStrVal "no additional transactions"
    pretty OneAdditionlTransaction   = usbStrVal "one additional transaction"
    pretty TwoAdditionalTransactions = usbStrVal "two additional transactions"

instance Pretty TransferType where
    pretty (Isochronous s u) = usbStrVal
                             $ text "isochronous"
                               <+> char '-' <+> text "synch:" <+> pretty s
                               <+> char '-' <+> text "usage:" <+> pretty u
    pretty tt                = usbStrVal . text . map toLower $ show tt

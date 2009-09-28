{-# LANGUAGE RankNTypes #-}

module PrettyDevList
    ( PPStyle(..)
    , brightStyle, darkStyle
    , ppDeviceList
    ) where

import Prelude hiding ( catch )

import Control.Arrow                  ( (>>>) )
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

data PPStyle = PPStyle
    { sectionStyle   :: forall a. Pretty a => a -> Doc
    , fieldStyle     :: forall a. Pretty a => a -> Doc
    , usbNumStyle    :: forall a. Pretty a => a -> Doc
    , usbStrStyle    :: forall a. Pretty a => a -> Doc
    , stringStyle    :: forall a. Pretty a => a -> Doc
    , descrStyle     :: forall a. Pretty a => a -> Doc
    , descrAddrStyle :: forall a. Pretty a => a -> Doc
    , versionStyle   :: forall a. Pretty a => a -> Doc
    , addrStyle      :: forall a. Pretty a => a -> Doc
    }

brightStyle :: PPStyle
brightStyle = PPStyle
    { sectionStyle   = pretty >>> white >>> bold >>> underline
    , fieldStyle     = pretty >>> white
    , usbNumStyle    = pretty >>> yellow
    , usbStrStyle    = pretty
    , stringStyle    = pretty >>> green
    , descrStyle     = pretty >>> cyan >>> dquotes
    , descrAddrStyle = pretty >>> cyan >>> bold
    , versionStyle   = pretty >>> yellow
    , addrStyle      = pretty >>> magenta
    }

darkStyle :: PPStyle
darkStyle = PPStyle
    { sectionStyle   = pretty >>> black >>> bold >>> underline
    , fieldStyle     = pretty >>> black
    , usbNumStyle    = pretty >>> dullyellow
    , usbStrStyle    = pretty
    , stringStyle    = pretty >>> dullgreen
    , descrStyle     = pretty >>> dullcyan >>> dquotes
    , descrAddrStyle = pretty >>> dullcyan
    , versionStyle   = pretty >>> dullyellow
    , addrStyle      = pretty >>> dullmagenta
    }

-------------------------------------------------------------------------------

field :: PPStyle -> String -> [Doc] -> [Doc]
field style name xs = (fieldStyle style $ (text name) <> char ':') : xs

section :: PPStyle -> String -> Doc
section style = sectionStyle style . text

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

ppStringDescr :: PPStyle -> Device -> StrIx -> IO Doc
ppStringDescr _     _   0  = return empty
ppStringDescr style dev ix = catchUSBException
    ( withDeviceHandle dev $ \devH ->
        liftM (descrStyle style)
              $ getStringDescriptorAscii devH
                                         ix
                                         stringBufferSize
    )
    (return . ppErr)
    where ppErr e = dquotes . dullred $ text "Couldn't retrieve string descriptor:"
                                        <+> red (text $ show e)

ppId :: PrintfArg n => n -> Doc
ppId x = text (printf "%04x" x :: String)

ppVendorName :: PPStyle -> IDDB -> Word16 -> (Doc -> Doc) -> Doc
ppVendorName style db vid f = maybe unknown (f . stringStyle style)
                              $ vendorName db vid'
    where vid' = fromIntegral vid

ppProductName :: PPStyle -> IDDB -> Word16 -> Word16 -> (Doc -> Doc) -> Doc
ppProductName style db vid pid f = maybe unknown (f . stringStyle style)
                                   $ productName db vid' pid'
    where vid' = fromIntegral vid
          pid' = fromIntegral pid

ppDevClass :: PPStyle -> IDDB -> Word8 -> Doc
ppDevClass _     _  0   = text "defined at interface level"
ppDevClass style db cid = ppClass style db cid

ppDevSubClass :: PPStyle -> IDDB -> Word8 -> Word8 -> Doc
ppDevSubClass _     _  0   0    = text "defined at interface level"
ppDevSubClass style db cid scid = ppSubClass style db cid scid

ppDevProtocol :: PPStyle -> IDDB -> Word8 -> Word8 -> Word8 -> Doc
ppDevProtocol _     _  0   0    0      = text "defined at interface level"
ppDevProtocol style db cid scid protId = ppProtocol style db cid scid protId

ppClass :: PPStyle -> IDDB -> Word8 -> Doc
ppClass style db cid = maybe unknown (stringStyle style)
                       $ className db cid'
    where cid' = fromIntegral cid

ppSubClass :: PPStyle -> IDDB -> Word8 -> Word8 -> Doc
ppSubClass style db cid scid = maybe unknown (stringStyle style)
                               $ subClassName db cid' scid'
    where cid'  = fromIntegral cid
          scid' = fromIntegral scid

ppProtocol :: PPStyle -> IDDB -> Word8 -> Word8 -> Word8 -> Doc
ppProtocol style db cid scid protId = maybe unknown (stringStyle style)
                                      $ protocolName db cid' scid' protId'
    where cid'    = fromIntegral cid
          scid'   = fromIntegral scid
          protId' = fromIntegral protId

ppDeviceList :: PPStyle -> IDDB -> Bool -> [Device] -> IO Doc
ppDeviceList style db False = liftM vcat . mapM (ppDeviceShort style db)
ppDeviceList style db True  = liftM (vcat . intersperse (char ' '))
                              . mapM (ppDevice style db)

ppDeviceShort :: PPStyle -> IDDB -> Device -> IO Doc
ppDeviceShort style db dev = do
    busNum  <- getBusNumber        dev
    devAddr <- getDeviceAddress    dev
    desc    <- getDeviceDescriptor dev

    let vid = deviceVendorId  desc
        pid = deviceProductId desc

    return $ text "Bus"
             <+> (addrStyle style) (printf "%03d" busNum :: String)
             <+> text "Device"
             <+> (addrStyle style) (printf "%03d" devAddr :: String)
             <>  char ':'
             <+> text "ID"
             <+> (usbNumStyle style) (ppId vid)
             <>  char ':'
             <>  (usbNumStyle style) (ppId pid)
             <+> ppVendorName style db vid
                 (<+> ppProductName style db vid pid (char '-' <+>))

ppDevice :: PPStyle -> IDDB -> Device -> IO Doc
ppDevice style db dev = do
  shortDesc <- ppDeviceShort style db dev
  desc      <- getDeviceDescriptor dev
  descDoc   <- ppDeviceDescriptor style db dev desc

  return $  shortDesc
        <$> section style "Device descriptor"
        <$> indent 2 descDoc

ppDeviceDescriptor :: PPStyle -> IDDB -> Device -> DeviceDescriptor -> IO Doc
ppDeviceDescriptor style db dev desc = do
    let field'          = field          style
        usbNumStyle' :: forall a. Pretty a => a -> Doc
        usbNumStyle'    = usbNumStyle    style
        versionStyle'   = versionStyle   style
        descrAddrStyle' = descrAddrStyle style
        manufacturerIx  = deviceManufacturerStrIx desc
        productIx       = deviceProductStrIx      desc
        serialIx        = deviceSerialNumberStrIx desc
        numConfigs      = deviceNumConfigs        desc
        classId         = deviceClass             desc
        subClassId      = deviceSubClass          desc
        protocolId      = deviceProtocol          desc
        vendorId        = deviceVendorId          desc
        productId       = deviceProductId         desc

    configDescriptors <- mapM (getConfigDescriptor dev)
                              [0 .. fromIntegral numConfigs - 1]

    manufacturerDoc <- ppStringDescr style dev manufacturerIx
    productDoc      <- ppStringDescr style dev productIx
    serialDoc       <- ppStringDescr style dev serialIx
    configDocs      <- mapM (ppConfigDescriptor style db dev)
                            configDescriptors

    let classDoc    = ppDevClass    style db classId
        subClassDoc = ppDevSubClass style db classId subClassId
        protocolDoc = ppDevProtocol style db classId subClassId protocolId

    return $ columns 2
      [ field' "USB specification" [ versionStyle' . ppBCD4
                                   $ deviceUSBSpecReleaseNumber desc
                                   ]
      , field' "Class"             [usbNumStyle' classId, classDoc]
      , field' "Sub class"         [usbNumStyle' subClassId, subClassDoc]
      , field' "Protocol"          [usbNumStyle' protocolId, protocolDoc]
      , field' "Max packet size"   [usbNumStyle' $ deviceMaxPacketSize0 desc]
      , field' "Vendor ID"         [ usbNumStyle' $ text "0x" <> ppId vendorId
                                   , ppVendorName style db vendorId id
                                   ]
      , field' "Product ID"        [ usbNumStyle' $ text "0x" <> ppId productId
                                   , ppProductName style db vendorId productId id
                                   ]
      , field' "Release number"    [ versionStyle' . ppBCD4
                                   $ deviceReleaseNumber desc
                                   ]
      , field' "Manufacturer"      [descrAddrStyle' manufacturerIx, manufacturerDoc]
      , field' "Product"           [descrAddrStyle' productIx, productDoc]
      , field' "Serial number"     [descrAddrStyle' serialIx, serialDoc]
      , field' "Num configs"       [usbNumStyle' numConfigs]
      ] <$> vcat ( map (\d -> section style "Configuration descriptor"
                              <$> indent 2 d)
                   configDocs
                 )

ppConfigDescriptor :: PPStyle -> IDDB -> Device -> ConfigDescriptor -> IO Doc
ppConfigDescriptor style db dev conf = do
  let field'       = field       style
      usbNumStyle' = usbNumStyle style
      stringIx     = configStrIx      conf
      ifDescs      = configInterfaces conf

  stringDescriptor <- ppStringDescr style dev stringIx
  ifDescDocs       <- mapM (mapM $ ppInterfaceDescriptor style db dev) ifDescs

  let vcatAlternatives = (<$>) (section style "Interface")
                       . indent 2
                       . vcat
                       . map ( (<$>) (section style "Alternative")
                             . indent 2
                             )

  return $ columns 2
    [ field' "Value"          [usbNumStyle' $ configValue conf]
    , field' "Descriptor"     [descrAddrStyle style stringIx, stringDescriptor]
    , field' "Attributes"     [pretty' style (configAttributes conf)]
    , field' "Max power"      [usbNumStyle' (2 * configMaxPower conf) <+> text "mA"]
    , field' "Num interfaces" [usbNumStyle' $ configNumInterfaces conf]
    ] <$> vcat (map vcatAlternatives ifDescDocs)

ppInterfaceDescriptor :: PPStyle -> IDDB -> Device -> InterfaceDescriptor -> IO Doc
ppInterfaceDescriptor style db dev ifDesc = do
  let field'          = field       style
      usbNumStyle'    = usbNumStyle style
      stringIx        = interfaceStrIx    ifDesc
      classId         = interfaceClass    ifDesc
      subClassId      = interfaceSubClass ifDesc
      protocolId      = interfaceProtocol ifDesc
      classDoc        = ppClass    style db classId
      subClassDoc     = ppSubClass style db classId subClassId
      protocolDoc     = ppProtocol style db classId subClassId protocolId

  stringDescriptor <- ppStringDescr style dev stringIx

  return $ columns 2
    [ field' "Interface number"    [usbNumStyle' $ interfaceNumber ifDesc]
    , field' "Alternative setting" [usbNumStyle' $ interfaceAltSetting ifDesc]
    , field' "Class"               [usbNumStyle' classId, classDoc]
    , field' "Sub class"           [usbNumStyle' subClassId, subClassDoc]
    , field' "Protocol"            [usbNumStyle' protocolId, protocolDoc]
    , field' "Descriptor"          [descrAddrStyle style stringIx, stringDescriptor]
    , field' "Num endpoints"       [usbNumStyle' $ interfaceNumEndpoints ifDesc]
    ] <$> vcat ( map (\e -> section style "Endpoint" <$> indent 2 (pretty' style e))
                     $ interfaceEndpoints ifDesc
               )

-------------------------------------------------------------------------------
-- USB specific instances of Pretty

class PrettyStyle a where
    pretty' :: PPStyle -> a -> Doc

instance PrettyStyle DeviceStatus where
    pretty' s ds = align $ columns 2 [ field s "Remote wakeup"
                                       [usbNumStyle s $ remoteWakeup ds]
                                     , field s "Self powered"
                                       [usbNumStyle s $ selfPowered  ds]
                                     ]

instance PrettyStyle EndpointDescriptor where
    pretty' s ep =
        columns 2 [ field' "Address"         [pretty' s $ endpointAddress ep]
                  , field' "Attributes"      [pretty' s $ endpointAttributes    ep]
                  , field' "Max packet size" [pretty' s $ endpointMaxPacketSize ep]
                  , field' "Interval"
                    [usbNumStyle s (endpointInterval ep)]
                  , field' "Refresh"
                    [usbNumStyle s (endpointRefresh ep)]
                  , field' "Synch address"
                    [usbNumStyle s (endpointSynchAddress  ep)]
                  ]
        where field' = field s

instance PrettyStyle EndpointAddress where
    pretty' s ea = usbNumStyle s (endpointNumber ea)
                   <+> char '-'
                   <+> pretty' s (endpointDirection ea)

instance PrettyStyle TransferDirection where
    pretty' s Out = usbStrStyle s $ text "host"   <+> text "->" <+> text "device"
    pretty' s In  = usbStrStyle s $ text "device" <+> text "->" <+> text "host"

instance PrettyStyle Synchronization where
    pretty' s NoSynchronization = usbStrStyle s "no synchronization"
    pretty' s es                = usbStrStyle s . map toLower $ show es

instance Pretty Usage where
    pretty = text . map toLower . show

instance PrettyStyle MaxPacketSize where
    pretty' s mps = usbNumStyle s (maxPacketSize mps)
                    <+> char '-'
                    <+> pretty' s (transactionOpportunities mps)

instance PrettyStyle TransactionOpportunities where
    pretty' s NoAdditionalTransactions  = usbStrStyle s "no additional transactions"
    pretty' s OneAdditionlTransaction   = usbStrStyle s "one additional transaction"
    pretty' s TwoAdditionalTransactions = usbStrStyle s "two additional transactions"

instance PrettyStyle  TransferType where
    pretty' s (Isochronous syn u) =
        usbStrStyle s $ text "isochronous"
                        <+> char '-' <+> text "synch:" <+> pretty' s syn
                        <+> char '-' <+> text "usage:" <+> pretty u
    pretty' s tt = usbStrStyle s . text . map toLower $ show tt

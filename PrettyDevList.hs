{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax #-}

module PrettyDevList
    ( PPStyle(..)
    , brightStyle, darkStyle
    , ppDevices
    ) where

import Prelude hiding ( catch )

import Control.Arrow         ( (>>>) )
import Control.Exception     ( catch )
import Control.Monad         ( liftM, mapM )
import Data.ByteString.Char8 ( ByteString, unpack )
import Data.Char             ( toLower )
import Data.List             ( intersperse, partition, transpose )
import Data.Word             ( Word8, Word16 )
import Prelude.Unicode       ( (∘), (⋅) )
import System.USB
import System.USB.IDDB       ( IDDB
                             , vendorName, productName
                             , className, subClassName, protocolName
                             , langName, subLangName
                             )
import Text.PrettyPrint.ANSI.Leijen
import Text.Printf           ( PrintfArg, printf )

-------------------------------------------------------------------------------
-- Pretty printing styles

data PPStyle = PPStyle
    { sectionStyle    ∷ ∀ α. Pretty α ⇒ α → Doc
    , fieldStyle      ∷ ∀ α. Pretty α ⇒ α → Doc
    , usbNumStyle     ∷ ∀ α. Pretty α ⇒ α → Doc
    , usbStrStyle     ∷ ∀ α. Pretty α ⇒ α → Doc
    , stringStyle     ∷ ∀ α. Pretty α ⇒ α → Doc
    , descrStyle      ∷ ∀ α. Pretty α ⇒ α → Doc
    , descrAddrStyle  ∷ ∀ α. Pretty α ⇒ α → Doc
    , versionStyle    ∷ ∀ α. Pretty α ⇒ α → Doc
    , addrStyle       ∷ ∀ α. Pretty α ⇒ α → Doc
    , errorStyle      ∷ ∀ α. Pretty α ⇒ α → Doc
    , errorDescrStyle ∷ ∀ α. Pretty α ⇒ α → Doc
    }

brightStyle ∷ PPStyle
brightStyle = PPStyle
    { sectionStyle    = pretty >>> white >>> bold >>> underline
    , fieldStyle      = pretty
    , usbNumStyle     = pretty >>> yellow
    , usbStrStyle     = pretty
    , stringStyle     = pretty >>> green
    , descrStyle      = pretty >>> cyan >>> dquotes
    , descrAddrStyle  = pretty >>> cyan >>> bold
    , versionStyle    = pretty >>> yellow
    , addrStyle       = pretty >>> magenta
    , errorStyle      = pretty >>> red
    , errorDescrStyle = pretty >>> dullred
    }

darkStyle ∷ PPStyle
darkStyle = PPStyle
    { sectionStyle    = pretty >>> bold >>> underline
    , fieldStyle      = pretty
    , usbNumStyle     = pretty >>> onyellow
    , usbStrStyle     = pretty
    , stringStyle     = pretty >>> ongreen
    , descrStyle      = pretty >>> oncyan >>> dquotes
    , descrAddrStyle  = pretty >>> oncyan >>> bold
    , versionStyle    = pretty >>> onyellow
    , addrStyle       = pretty >>> onmagenta
    , errorStyle      = pretty >>> red
    , errorDescrStyle = pretty >>> dullred
    }

-------------------------------------------------------------------------------

field ∷ PPStyle → String → [Doc] → [Doc]
field style name xs = (fieldStyle style $ (text name) <> char ':') : xs

section ∷ PPStyle → String → Doc
section style = sectionStyle style ∘ text

-------------------------------------------------------------------------------
-- Some basic instances of the Pretty class

instance Pretty ByteString where
    pretty = pretty ∘ unpack

instance Pretty Word8 where
    pretty = int ∘ fromIntegral

instance Pretty Word16 where
    pretty = int ∘ fromIntegral

-------------------------------------------------------------------------------
-- Miscellaneous pretty printing functions

columns ∷ Int → [[Doc]] → Doc
columns s rows = vcat $ map ( hcat
                            ∘ zipWith fill (map (+ s) $ columnSizes rows)
                            )
                            rows

columnSizes ∷ [[Doc]] → [Int]
columnSizes = map (maximum ∘ map docLen) ∘ transpose

-- A bit of a hack to calculate the length of a document without
-- considering colour codes.
docLen ∷ Doc → Int
docLen = sdocLen ∘ renderPretty 0.4 100
    where
      sdocLen ∷ SimpleDoc → Int
      sdocLen (SEmpty)      = 0
      sdocLen (SChar _ d)   = 1 + sdocLen d
      sdocLen (SText i _ d) = i + sdocLen d
      sdocLen (SLine i d)   = i + sdocLen d
      sdocLen (SSGR _ d)    = sdocLen d

-------------------------------------------------------------------------------
-- USB utility functions

stringBufferSize ∷ Int
stringBufferSize = 512

catchUSBException ∷ IO α → (USBException → IO α) → IO α
catchUSBException = catch

-------------------------------------------------------------------------------
-- Pretty printers for USB types

unknown ∷ Doc
unknown = text "unknown"

ppBCD4 ∷ BCD4 → Doc
ppBCD4 (a, b, c, d) = hcat ∘ punctuate (char '.') $ map pretty [a, b, c, d]

ppStringDesc ∷ PPStyle → Device → StrIx → IO Doc
ppStringDesc _     _   0  = return empty
ppStringDesc style dev ix = catchUSBException
    ( withDeviceHandle dev $ \devH →
        liftM (descrStyle style)
              $ getStrDescFirstLang devH
                                    ix
                                    stringBufferSize
    )
    (return ∘ ppError style "Couldn't retrieve string descriptor")

ppError ∷ Show e ⇒ PPStyle → String → e → Doc
ppError style descr err = errorDescrStyle style
                        $ text descr
                          <> char ':'
                          <+> errorStyle style (show err)

ppId ∷ PrintfArg n ⇒ n → Doc
ppId x = text (printf "%04x" x ∷ String)

ppVendorName ∷ PPStyle → IDDB → Word16 → (Doc → Doc) → Doc
ppVendorName style db vid f = maybe unknown (f ∘ stringStyle style)
                              $ vendorName db vid'
    where vid' = fromIntegral vid

ppProductName ∷ PPStyle → IDDB → Word16 → Word16 → (Doc → Doc) → Doc
ppProductName style db vid pid f = maybe unknown (f ∘ stringStyle style)
                                   $ productName db vid' pid'
    where vid' = fromIntegral vid
          pid' = fromIntegral pid

ppDevClass ∷ PPStyle → IDDB → Word8 → Doc
ppDevClass _     _  0   = text "defined at interface level"
ppDevClass style db cid = ppClass style db cid

ppDevSubClass ∷ PPStyle → IDDB → Word8 → Word8 → Doc
ppDevSubClass _     _  0   0    = text "defined at interface level"
ppDevSubClass style db cid scid = ppSubClass style db cid scid

ppDevProtocol ∷ PPStyle → IDDB → Word8 → Word8 → Word8 → Doc
ppDevProtocol _     _  0   0    0      = text "defined at interface level"
ppDevProtocol style db cid scid protId = ppProtocol style db cid scid protId

ppClass ∷ PPStyle → IDDB → Word8 → Doc
ppClass style db cid = maybe unknown (stringStyle style)
                       $ className db cid'
    where cid' = fromIntegral cid

ppSubClass ∷ PPStyle → IDDB → Word8 → Word8 → Doc
ppSubClass style db cid scid = maybe unknown (stringStyle style)
                               $ subClassName db cid' scid'
    where cid'  = fromIntegral cid
          scid' = fromIntegral scid

ppProtocol ∷ PPStyle → IDDB → Word8 → Word8 → Word8 → Doc
ppProtocol style db cid scid protId = maybe unknown (stringStyle style)
                                      $ protocolName db cid' scid' protId'
    where cid'    = fromIntegral cid
          scid'   = fromIntegral scid
          protId' = fromIntegral protId

ppLanguage ∷ PPStyle → IDDB → LangId → Doc
ppLanguage style db (lid, slid) =
    let lid'  = fromIntegral lid
        slid' = fromIntegral slid
        prettyLang = stringStyle style
        langDoc = maybe unknown prettyLang $ langName db lid'
        subDoc  = maybe unknown prettyLang $ subLangName db lid' slid'
    in langDoc <+> char '-' <+> subDoc

ppLanguageList ∷ PPStyle → IDDB → Device → IO Doc
ppLanguageList style db dev =
    catchUSBException ( withDeviceHandle dev
                      $ fmap (hsep ∘ punctuate (text ", ") ∘ map (ppLanguage style db))
                      ∘ getLanguages
                      )
                      ( return
                      ∘ ppError style "Couldn't retrieve language list"
                      )

ppDevices ∷ PPStyle → IDDB → Bool → [Device] → IO Doc
ppDevices style db False = liftM vcat ∘ mapM (ppDeviceShort style db)
ppDevices style db True  = liftM (vcat ∘ intersperse (char ' '))
                           ∘ mapM (ppDevice style db)

ppAddr ∷ PrintfArg n ⇒ PPStyle → n → Doc
ppAddr style a = let toDoc = addrStyle style ∷ String → Doc
                 in toDoc $ printf "%03d" a

ppDeviceShort ∷ PPStyle → IDDB → Device → IO Doc
ppDeviceShort style db dev = do
  let desc       = deviceDesc dev
      vid        = deviceVendorId  desc
      pid        = deviceProductId desc
      busDoc     = ppAddr style $ busNumber dev
      devAddrDoc = ppAddr style $ deviceAddress dev

  return $   text "Bus"    <+> busDoc
         <+> text "Device" <+> devAddrDoc <> char ':'
         <+> text "ID"
         <+> (usbNumStyle style) (ppId vid) <>  char ':'
         <>  (usbNumStyle style) (ppId pid)
         <+> ppVendorName style db vid
             (<+> ppProductName style db vid pid (char '-' <+>))

ppDevice ∷ PPStyle → IDDB → Device → IO Doc
ppDevice style db dev = do
  let desc = deviceDesc dev
  descDoc ← ppDeviceDesc style db dev desc
  langDoc ← ppLanguageList style db dev

  let field'     = field style
      busDoc     = ppAddr style $ busNumber dev
      devAddrDoc = ppAddr style $ deviceAddress dev

  return $ section style "Device"
        <$> indent 2
            (   columns 2 [ field' "Bus"                 [busDoc]
                          , field' "Address"             [devAddrDoc]
                          , field' "Supported languages" [langDoc]
                          ]
            <$> section style "Device Descriptor"
            <$> indent 2 descDoc
            )

ppDeviceDesc ∷ PPStyle → IDDB → Device → DeviceDesc → IO Doc
ppDeviceDesc style db dev desc = do
    let usbNumStyle' ∷ ∀ α. Pretty α ⇒ α → Doc
        usbNumStyle'    = usbNumStyle    style
        field'          = field          style
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

    manufacturerDoc ← ppStringDesc style dev manufacturerIx
    productDoc      ← ppStringDesc style dev productIx
    serialDoc       ← ppStringDesc style dev serialIx
    configDocs      ← mapM (ppConfigDesc style db dev)
                           $ deviceConfigs desc

    let classDoc    = ppDevClass    style db classId
        subClassDoc = ppDevSubClass style db classId subClassId
        protocolDoc = ppDevProtocol style db classId subClassId protocolId

    return $ columns 2
      [ field' "USB specification" [ versionStyle' ∘ ppBCD4
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
      , field' "Release number"    [ versionStyle' ∘ ppBCD4
                                   $ deviceReleaseNumber desc
                                   ]
      , field' "Manufacturer"      [descrAddrStyle' manufacturerIx, manufacturerDoc]
      , field' "Product"           [descrAddrStyle' productIx, productDoc]
      , field' "Serial number"     [descrAddrStyle' serialIx, serialDoc]
      , field' "Num configs"       [usbNumStyle' numConfigs]
      ] <$> vcat ( map (\d → section style "Configuration Descriptor"
                              <$> indent 2 d)
                   configDocs
                 )

ppConfigDesc ∷ PPStyle → IDDB → Device → ConfigDesc → IO Doc
ppConfigDesc style db dev conf = do
  let field'       = field       style
      usbNumStyle' = usbNumStyle style
      stringIx     = configStrIx      conf
      ifDescs      = configInterfaces conf

  strDesc    ← ppStringDesc style dev stringIx
  ifDescDocs ← mapM (mapM $ ppInterfaceDesc style db dev) ifDescs

  let vcatAlts = (<$>) (section style "Interface")
               ∘ indent 2
               ∘ vcat
               ∘ map ( (<$>) (section style "Alternative")
                     ∘ indent 2
                     )

  return $ columns 2
    [ field' "Value"          [usbNumStyle' $ configValue conf]
    , field' "Descriptor"     [descrAddrStyle style stringIx, strDesc]
    , field' "Attributes"     [pretty' style (configAttribs conf)]
    , field' "Max power"      [usbNumStyle' (2 ⋅ configMaxPower conf) <+> text "mA"]
    , field' "Num interfaces" [usbNumStyle' $ configNumInterfaces conf]
    ] <$> vcat (map vcatAlts ifDescDocs)

ppInterfaceDesc ∷ PPStyle → IDDB → Device → InterfaceDesc → IO Doc
ppInterfaceDesc style db dev ifDesc = do
  let field'          = field       style
      usbNumStyle' ∷ ∀ α. Pretty α ⇒ α → Doc
      usbNumStyle'    = usbNumStyle style
      stringIx        = interfaceStrIx    ifDesc
      classId         = interfaceClass    ifDesc
      subClassId      = interfaceSubClass ifDesc
      protocolId      = interfaceProtocol ifDesc
      classDoc        = ppClass    style db classId
      subClassDoc     = ppSubClass style db classId subClassId
      protocolDoc     = ppProtocol style db classId subClassId protocolId
      (inEndpts, outEndpts) = 
          partition ((==) In ∘ transferDirection ∘ endpointAddress) 
                    $ interfaceEndpoints ifDesc

  strDesc ← ppStringDesc style dev stringIx

  return $ columns 2
    [ field' "Interface number"    [usbNumStyle' $ interfaceNumber ifDesc]
    , field' "Alternative setting" [usbNumStyle' $ interfaceAltSetting ifDesc]
    , field' "Class"               [usbNumStyle' classId, classDoc]
    , field' "Sub class"           [usbNumStyle' subClassId, subClassDoc]
    , field' "Protocol"            [usbNumStyle' protocolId, protocolDoc]
    , field' "Descriptor"          [descrAddrStyle style stringIx, strDesc]
    , field' "Num in endpoints"    [usbNumStyle' $ length inEndpts]
    , field' "Num out endpoints"   [usbNumStyle' $ length outEndpts]
    ] <$> vcat ( map (\e → section style "In Endpoint" <$> indent 2 (pretty' style e))
                     $ inEndpts
               )
      <$> vcat ( map (\e → section style "Out Endpoint" <$> indent 2 (pretty' style e))
                     $ outEndpts
               )

-------------------------------------------------------------------------------
-- USB specific instances of Pretty

class PrettyStyle α where
    pretty' ∷ PPStyle → α → Doc

instance PrettyStyle EndpointDesc where
    pretty' s ep =
        columns 2
        [ field' "Address"         [pretty' s $ endpointAddress       ep]
        , field' "Attributes"      [pretty' s $ endpointAttribs       ep]
        , field' "Max packet size" [pretty' s $ endpointMaxPacketSize ep]
        , field' "Interval"        [usbNumStyle s $ endpointInterval     ep]
        , field' "Refresh"         [usbNumStyle s $ endpointRefresh      ep]
        , field' "Synch address"   [usbNumStyle s $ endpointSynchAddress ep]
        ]
        where field' = field s

instance PrettyStyle EndpointAddress where
    pretty' s ea = usbNumStyle s (endpointNumber ea)
                   <+> char '-' <+> pretty' s (transferDirection ea)

instance PrettyStyle TransferDirection where
    pretty' s Out = usbStrStyle s $ text "Out (host" <+> text "->" <+> text "device)"
    pretty' s In  = usbStrStyle s $ text "In (device" <+> text "->" <+> text "host)"

instance PrettyStyle DeviceStatus where
    pretty' s ds = align $ columns 2 [ field s "Remote wakeup"
                                       [usbNumStyle s $ remoteWakeup ds]
                                     , field s "Self powered"
                                       [usbNumStyle s $ selfPowered  ds]
                                     ]

instance PrettyStyle Synchronization where
    pretty' s NoSynchronization = usbStrStyle s "no synchronization"
    pretty' s es                = usbStrStyle s ∘ map toLower $ show es

instance Pretty Usage where
    pretty = text ∘ map toLower ∘ show

instance PrettyStyle MaxPacketSize where
    pretty' s mps = usbNumStyle s (maxPacketSize mps)
                    <+> char '-'
                    <+> pretty' s (transactionOpportunities mps)

instance PrettyStyle TransactionOpportunities where
    pretty' s Zero = usbStrStyle s "Zero additional transactions"
    pretty' s One  = usbStrStyle s "One additional transaction"
    pretty' s Two  = usbStrStyle s "Two additional transactions"

instance PrettyStyle  TransferType where
    pretty' s (Isochronous syn u) =
        usbStrStyle s $ text "isochronous"
                        <+> char '-' <+> text "synch:" <+> pretty' s syn
                        <+> char '-' <+> text "usage:" <+> pretty u
    pretty' s tt = usbStrStyle s ∘ text ∘ map toLower $ show tt

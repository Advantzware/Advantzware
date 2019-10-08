
/*------------------------------------------------------------------------
    File        : edi/sp856xml.p  SOPI ASN
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 28 09:07:49 EST 2017
    Notes       :
        
        SPS EDI 856 structures:
            
*Shipment
**Meta

**Header
***ShipmentHeader
***Dates
***References
 ****ReferenceIDs
***Notes
***Contacts
 ****AdditionalContactDetails
***Address
 ****References
  *****ReferenceIDs
 ****Contacts
  *****AdditionalContactDetails
 ****Dates
***CarrierInformation
 ****ServiceLevelCodes
 ****Address
  *****Dates
 ****SealNumbers
***QuantityAndWeight
***Commodity
***CarrierSpecialHandlingDetail
***Taxes
***ChargesAllowances
 ****Taxes
***FOBRelatedInstruction
***QuantityTotals
***RegulatoryCompliances

**OrderLevel
 ***OrderHeader
 ***QuantityAndWeight
 ***CarrierInformation
  ****ServiceLevelCodes
  ****Address
   *****Dates
  ****SealNumbers
 ***Dates
 ***References
  ****ReferenceIDs
 ***Notes
 ***Address
  ****References
   *****ReferenceIDs
  ****Contacts
   *****AdditionalContactDetails
  ****Dates
 ***Taxes
 ***ChargesAllowances
  ****Taxes
 ***Commodity
 ***RegulatoryCompliances
 ***PackLevel
  ****Pack
  ****PhysicalDetails 
  ****MarksAndNumbersCollection
  ****PalletInformation
  ****Dates
  ****References
   *****ReferenceIDs
  ****Notes
  ****Address
   *****References
    ******ReferenceIDs
   *****Contacts
    ******AdditionalContactDetails
   *****Dates
  ****Taxes
  ****ChargesAllowances
   *****Taxes
  ****CarrierInformation
   *****ServiceLevelCodes
   *****Address
    ******Dates
   *****SealNumbers
  ****Packaging
  ****RegulatoryCompliances
  ****ItemLevel
   *****ShipmentLine
    ******ProductID
    ******NRFStandardColorAndSize
   *****PhysicalDetails
   *****CarrierSpecialHandlingDetail
   *****CarrierInformation
    ******ServiceLevelCodes
    ******Address
     *******Dates
    ******SealNumbers
   *****Measurements
   *****PriceInformation
   *****ProductOrItemDescription
   *****MasterItemAttribute
    ******ItemAttribute
     *******Measurements
   *****Dates
   *****References
    ******ReferenceIDs
   *****Notes
   *****Commodity
   *****Address
    ******References
     *******ReferenceIDs
    ******Contacts
     *******AdditionalContactDetails
    ******Dates
   *****Subline
    ******SublineItemDetail
     *******ProductID
     *******NRFStandardColorAndSize
    ******PriceInformation
    ******ProductOrItemDescription
    ******Commodity
    ******RegulatoryCompliances
   *****Taxes
   *****ChargesAllowances
    ******Taxes
   *****ItemLoadInfo
    ******ItemLoad
    ******References
     *******ReferenceIDs
    ******Notes
   *****RegulatoryCompliances
**Summary

        
 ** Meta 
    Header - shipmentHeader
    OrderLevel - OrderHeader
                 Taxes
                 PackLevel - pack 
                             itemlevel - shipmentline
                             
                   
    Summary        
  ----------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipBOLNumber AS INTEGER.
DEFINE INPUT PARAMETER ipPartner AS CHARACTER.
DEFINE INPUT PARAMETER ipSeq AS INTEGER.

/* ***************************  Definitions  ************************** */
{custom/globdefs.i}
{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/asnsps.i}

DEF VAR v-ship-name  LIKE shipto.ship-name.
DEF VAR v-ship-addr  LIKE shipto.ship-addr.
DEF VAR v-ship-city  LIKE shipto.ship-city.
DEF VAR v-ship-state LIKE shipto.ship-state.
DEF VAR v-ship-zip   LIKE shipto.ship-zip.
DEF VAR v-ship-addr3 AS   CHAR FORMAT "x(30)".

DEFINE TEMP-TABLE Shipment
   FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
   .
   
DEFINE TEMP-TABLE Meta
    FIELD SenderUniqueID AS CHAR
    FIELD SenderCompanyName AS CHAR
    FIELD ReceiverUniqueId AS CHAR
    FIELD InterchangeControlNumber AS INTEGER
    FIELD GroupControIdentifier AS CHARACTER
    FIELD GroupControlNumber AS INTEGER
    FIELD InterchangeSenderID AS CHARACTER
    FIELD InterchangeReceiverID AS CHARACTER
    FIELD GroupSenderID AS CHARACTER 
    FIELD GourpReceiverID AS CHARACTER 
    FIELD BatchPart AS CHARACTER 
    FIELD BatchTotal AS CHARACTER 
    FIELD BatchID AS CHARACTER 
    FIELD Comments AS CHARACTER 
    FIELD Validation AS CHARACTER 
    FIELD OrderManagement AS CHARACTER 
    FIELD VERSION AS CHARACTER 
    .

DEFINE TEMP-TABLE tHeader XML-NODE-NAME "Header"
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
      
DEFINE TEMP-TABLE tOrderLevel XML-NODE-NAME "OrderLevel"
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
            
DEFINE TEMP-TABLE ShipmentHeader /*excel row 159 */
    FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHAR
    FIELD ShipDate AS DATE
    FIELD TsetPurposeCode AS CHAR
    FIELD ShipNoticeDate AS DATE
    FIELD ShipNoticeTime AS CHAR
    FIELD ASNStructureCode AS CHAR INIT "0001"  /*SOPI*/
    FIELD BillOfLadingNumber AS CHAR
    FIELD CarrierProNumber AS CHAR SERIALIZE-HIDDEN
    FIELD CurrentScheduledDeliveryDate AS DATE
    .

DEFINE TEMP-TABLE References    /* excel row 196 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD ReferenceQual AS CHAR INIT "MR"
    FIELD ReferenceID AS CHAR
    .
    
DEFINE TEMP-TABLE Address   /* excel row 231 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD AddressTypeCode AS CHAR
    FIELD LocationCodeQualifier AS CHAR
    FIELD AddressLocationNumber AS CHAR
    FIELD AddressName AS CHAR
    FIELD Address1 AS CHAR
    FIELD Address2 AS CHAR
    FIELD City AS CHAR
    FIELD State AS CHAR
    FIELD PostalCode AS CHAR
    FIELD Country AS CHAR
    .    

DEFINE TEMP-TABLE CarrierInformation  /* excel row 291 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD CarrierAlphaCode AS CHAR  /* SCAC code 2-4 digit */
    .    

DEFINE TEMP-TABLE QuantityAndWeight   /* excel row 348 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD PackingMedium AS CHAR
    FIELD PackingMaterial AS CHAR
    FIELD LadingQuantity AS CHAR
    FIELD WeightQualifier AS CHAR
    FIELD Weight AS CHAR
    FIELD WeightUOM AS CHAR
    .


DEFINE TEMP-TABLE FOBRelatedInstruction   /* excel row 429 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD FOBPayCode AS CHAR
    .

DEFINE TEMP-TABLE OrderHeader /* OrderLevel */   /* excel row 461 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD cust-po AS CHARACTER XML-NODE-NAME "PurchaseOrderNumber"
    FIELD cust-po-date AS DATE XML-NODE-NAME "PurchaseOrderDate"
    .

DEFINE TEMP-TABLE ChargesAllowances   /* excel row 654 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD AllowChrgIndicator AS CHAR
    FIELD AllowChrgCode AS CHAR
    FIELD AllowChrgAmt AS CHAR
    .
    
DEFINE TEMP-TABLE PackLevel XML-NODE-NAME "PackLevel"
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN .                    
                 
DEFINE TEMP-TABLE Pack    /* excel row 706 */
        /* children table - Pack, PhysicalDetails, MarksAndNumbersCollection */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN         
    FIELD PackLevelType AS CHAR INIT "P"
    FIELD CarrierPackageID AS CHAR SERIALIZE-HIDDEN
    .
  
DEFINE TEMP-TABLE PhysicalDetail    /* excel row 712 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN  
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN        
/*    field PackLevelType as char init "P"*/
    FIELD PackQualifier AS CHAR
    FIELD Description AS CHAR
    .
     
DEFINE TEMP-TABLE MarksAndNumbersCollection    /* excel row 733 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN        
/*    field PackLevelType as char init "P"*/
    FIELD MarksAndNumbersQualifier1 AS CHAR INIT "CA"
    FIELD MarksAndNumbers1 AS CHAR
    .
                  /* ItemLevel */
DEFINE TEMP-TABLE ItemLevel 
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN .  
                      
DEFINE TEMP-TABLE ShipmentLine    /* excel row 976 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN        
/*    field PackLevelType as char init "P"*/
    FIELD LineSequenceNumber AS CHAR
    FIELD BuyerPartNumber AS CHAR
    FIELD OrderQty AS INTEGER
    FIELD OrderQtyUOM AS CHAR
    FIELD ShipQty AS INTEGER
    FIELD ShipQtyUOM AS CHAR
    .
       
DEFINE TEMP-TABLE summary   /* excel row 1462 */
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
    FIELD ShipmentIdentification AS CHARACTER SERIALIZE-HIDDEN
    FIELD TotalLineItemNumber AS INTEGER  
    .
       
/* DEFINE QUERY qShipment FOR shipment. error 11877 */
DEFINE DATA-SOURCE dsShipment FOR EDCode /*QUERY qShipment */ . 
/*DEFINE DATA-SOURCE dsMeta FOR Meta.*/
DEFINE BUFFER bfEdCode FOR EDCode. 
DEFINE DATA-SOURCE dstHeader FOR bfEDCode .
DEFINE BUFFER bf2EdCode FOR EDCode. 
DEFINE DATA-SOURCE dstOrderLevel FOR bf2EDCode  .
DEFINE BUFFER bf3EdCode FOR EDCode.
DEFINE DATA-SOURCE dsPackLevel FOR bf3EDCode  .
DEFINE BUFFER bf4EdCode FOR EDCode.
DEFINE DATA-SOURCE dsItemLevel FOR bf4EDCode  .

DEFINE DATA-SOURCE dsAddress FOR EDShipto.

DEFINE DATA-SOURCE dsShipmentHeader FOR edshtran.
DEFINE DATA-SOURCE dsOrderHeader FOR edshord.
DEFINE DATA-SOURCE dsPack FOR EDSHPack.
DEFINE DATA-SOURCE dsItem FOR EDSHLine. 
       
DEFINE DATASET Shipments /*ds856xml*/
   FOR shipment,
       /*meta, */
       tHeader,
          shipmentHeader, References , Address, CarrierInformation, QuantityAndWeight, FOBRelatedInstruction,
       tOrderLevel, OrderHeader, ChargesAllowances,
                    PackLevel, Pack, 
                               ItemLevel,PhysicalDetail, MarksAndNumbersCollection, ShipmentLine, 
       Summary
/*       DATA-RELATION rShipMeta FOR shipment, Meta relation-fields(SenderUniqueID,SenderUniqueID)*/
       DATA-RELATION rShiptHeader FOR shipment, tHeader RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShiptOrderLevel FOR shipment, tOrderLevel RELATION-FIELDS(Partner,Partner) NESTED
       
       DATA-RELATION rShip1 FOR tHeader, ShipmentHeader RELATION-FIELDS(Partner,Partner) NESTED        
       DATA-RELATION rShipRef FOR theader, References RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShipAddr FOR tHeader, Address RELATION-FIELDS(Partner,Partner) NESTED  
       DATA-RELATION rShipCarrier FOR tHeader, CarrierInformation RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShipQtyWgt FOR tHeader, QuantityAndWeight RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShipFob FOR tHeader, FOBRelatedInstruction RELATION-FIELDS(Partner,Partner) NESTED
       
       DATA-RELATION rShipOrderH FOR tOrderLevel, OrderHeader RELATION-FIELDS(Partner,Partner) NESTED
              /*DATA-RELATION rOrderHChg FOR OrderHeader, ChargesAllowances RELATION-FIELDS(Partner,Partner,Seq,Seq,cust-po,cust-po) nested*/
       DATA-RELATION rOrderHChg FOR tOrderLevel, ChargesAllowances RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShipPackLevel FOR tOrderLevel, PackLevel RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShipPack FOR PackLevel, Pack RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rShipPackItem FOR PackLevel, ITEMLevel RELATION-FIELDS(Partner,Partner) NESTED
/*       DATA-RELATION rPackDet FOR Pack, PhysicalDetail RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested            */
/*       DATA-RELATION rPackMark FOR Pack, MarksAndNumbersCollection RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested*/
/*       DATA-RELATION rPackLine FOR Pack, ShipmentLine RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested             */
       DATA-RELATION rPackDet FOR ItemLevel, PhysicalDetail RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rPackMark FOR PackLevel, MarksAndNumbersCollection RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION rPackLine FOR ItemLevel, ShipmentLine RELATION-FIELDS(Partner,Partner) NESTED
       .
        
DEFINE VAR hds856xml AS HANDLE NO-UNDO.
DEFINE VARIABLE iTotalItem# AS INTEGER NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

hds856xml = DATASET Shipments:HANDLE.
 
RUN BuildData.
RUN GenerateXmlFiles.





/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  BUFFER Shipment:ATTACH-DATA-SOURCE(DATA-SOURCE dsShipment:handle, "Partner,Partner").
  BUFFER tHeader:ATTACH-DATA-SOURCE(DATA-SOURCE dstHeader:handle, "Partner,Partner").    
  BUFFER tOrderLevel:ATTACH-DATA-SOURCE(DATA-SOURCE dstOrderLevel:handle, "Partner,Partner").
  BUFFER PackLevel:ATTACH-DATA-SOURCE(DATA-SOURCE dsPackLevel:handle, "Partner,Partner").
  BUFFER ItemLevel:ATTACH-DATA-SOURCE(DATA-SOURCE dsItemLevel:handle, "Partner,Partner").
/*  BUFFER Shipment:attach-data-source(DATA-SOURCE dsShipment:handle, "SenderUniqueID,custom-proc").*/
/*  QUERY qShipment:QUERY-PREPARE("for each shipment no-lock where SenderUniqueID = '1000' ").      */
   
  /*BUFFER Meta:attach-data-source(DATA-SOURCE dsMeta:handle, "Partner,Partner").*/
  BUFFER ShipmentHeader:ATTACH-DATA-SOURCE(DATA-SOURCE dsShipmentHeader:handle, "Partner,Partner,seq,seq").
  BUFFER OrderHeader:ATTACH-DATA-SOURCE(DATA-SOURCE dsOrderHeader:handle, "Partner,Partner,seq,seq").
  BUFFER Pack:ATTACH-DATA-SOURCE(DATA-SOURCE dsPack:handle, "Partner,Partner,Seq,seq").
  BUFFER ShipmentLine:ATTACH-DATA-SOURCE(DATA-SOURCE dsItem:handle, "Partner,Partner,Seq,seq").
  
  hds856xml:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ds856xmlBeforeFill", THIS-PROCEDURE).
  BUFFER Shipment:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentBeforeFill", THIS-PROCEDURE ).
  BUFFER Shipment:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "ShipmentAfterRowFill", THIS-PROCEDURE ).
/*  BUFFER Meta:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "MetaBeforeFill", this-procedure ).*/
  BUFFER tHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "tHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER tOrderLevel:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "tOrderLevelBeforeFill", THIS-PROCEDURE ).
  BUFFER PackLevel:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackLevelBeforeFill", THIS-PROCEDURE ).
  BUFFER ItemLevel:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ItemLevelBeforeFill", THIS-PROCEDURE ).
  BUFFER ShipmentHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER ShipmentHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "ShipmentHeaderAfterRowFill", THIS-PROCEDURE ).
  BUFFER References:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER Address:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER CarrierInformation:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER QuantityAndWeight:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER FOBRelatedInstruction:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", THIS-PROCEDURE ).
  
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER ChargesAllowances:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderHeaderAfterRowFill", THIS-PROCEDURE ).
  
  BUFFER Pack:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", THIS-PROCEDURE ).
  BUFFER PhysicalDetail:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", THIS-PROCEDURE ).
  BUFFER MarksAndNumbersCollection:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", THIS-PROCEDURE ).
  BUFFER ShipmentLine:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", THIS-PROCEDURE ).
  BUFFER Pack:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "PackAfterRowFill", THIS-PROCEDURE ).
  BUFFER ShipmentLine:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "ShipmentLineAfterRowFill", THIS-PROCEDURE ).
  
  BUFFER Summary:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "SummaryBeforeFill", THIS-PROCEDURE ).
    
  DATA-SOURCE dsShipment:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner)  .
   
  DATA-SOURCE dsShipmentHeader:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner) + " and seq = " + string(ipSeq) .
  /* wfk - test */
    DATA-SOURCE dsOrderHeader:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner) + " and seq = " + string(ipSeq) .
/* wfk - est */
  /* Test DATA-SOURCE dsPackLevel:FILL-WHERE-STRING = " where Partner = '1EWALLGRAPHPACK'  " . */
  DATA-SOURCE dsPack:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner) + " and seq = " + string(ipSeq) .
  DATA-SOURCE dsItem:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner) + " and seq = " + string(ipSeq) .                                               
       
  hds856xml:FILL().
DEF VAR cnt AS INT.
cnt = 0.
   FOR EACH pack:
    
    cnt = cnt + 1.
    IF cnt GT 1 THEN 
     DELETE pack.
    END.
END PROCEDURE.

PROCEDURE ds856xmlBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE GenerateXmlFiles:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE VAR cXMLOutputfile AS CHAR NO-UNDO.
   
   cXMLOutputfile = (IF asnsps-cha <> "" THEN asnsps-cha ELSE "c:\temp:\") +
                    "SPS856_BOL#_" + string(ipBOLNumber) + ".xml".
                                        
   hds856xml:WRITE-XML("file",
       cXMLOutputfile,
       FALSE,
       ?,?,FALSE, FALSE, FALSE, FALSE )
       .
MESSAGE "Advance Ship Notice file created: " cXMLOutputfile
VIEW-AS ALERT-BOX.   

END PROCEDURE.

PROCEDURE ItemLevelBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE MetaBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE OrderheaderAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
 
  
END PROCEDURE.

PROCEDURE OrderHeaderBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

  
END PROCEDURE.

PROCEDURE PackAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE PackBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE ShipmentAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  
  
END PROCEDURE.

PROCEDURE ShipmentBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.


END PROCEDURE.

PROCEDURE ShipmentHeaderAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  DEFINE BUFFER bfSHeader FOR ShipmentHeader.
  IF CAN-FIND(FIRST bfSheader WHERE RECID(bfSheader) <> recid(shipmentheader)) THEN DO:
     DELETE shipmentheader.
     RETURN.
  END.
    
  /*shipmentHeader, References , Address, CarrierInformation, QuantityAndWeight, FOBRelatedInstruction */
  
    ASSIGN shipmentHeader.ShipmentIdentification = STRING(EDSHTran.seq, "999999999")
           shipmentHeader.ShipDate = EDSHTran.Ship-Date
           shipmentHeader.TsetPurposeCode = EDSHTran.Purpose-code 
           shipmentHeader.ShipNoticeDate = EDSHTran.ship-date
           shipmentHeader.ShipNoticeTime = STRING(EDSHTran.Ship-time, "HH:MM:SS") + "-05:00"
           shipmentHeader.ASNStructureCode = "0001"  /*SOPI*/
           shipmentHeader.BillOfLadingNumber = EDSHTran.BOL-No
           shipmentHeader.CarrierProNumber = IF EDSHTran.Pro-Number = "" THEN "TEST" ELSE EDSHTran.Pro-Number
           shipmentHeader.CurrentScheduledDeliveryDate = IF EDSHTran.Del-date = ? THEN TODAY + 30 ELSE EDSHTran.Del-date      
           .
    /* WFK - until data is corrected */
    IF shipmentHeader.partner = "" THEN shipmentHeader.partner = "Stock".
    
    CREATE References.
    ASSIGN References.Partner = shipmentHeader.partner
           References.shipmentIdentification = shipmentHeader.ShipmentIdentification
           References.ReferenceQual = "MR"
           References.ReferenceID = "Paper"
           .
    CREATE References.
    ASSIGN References.Partner = shipmentHeader.partner
           References.shipmentIdentification = shipmentHeader.ShipmentIdentification
           References.ReferenceQual = "19"
           References.ReferenceID = "Division"
           .
    FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.company EQ cocode AND oe-bolh.bol-no EQ integer(edshtran.bol-no) NO-ERROR.
    
    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    ASSIGN
     v-ship-name    = shipto.ship-name
     v-ship-addr[1] = shipto.ship-addr[1]
     v-ship-addr[2] = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip                  
     .
    CREATE Address.
    ASSIGN Address.Partner = shipmentHeader.partner
           Address.shipmentIdentification = shipmentHeader.ShipmentIdentification
           Address.AddressTypeCode = "ST"
           address.LocationCodeQualifier = "6"  /* 1: DUN#, 6: Plant Code */
           address.AddressLocationNumber = (IF AVAILABLE shipto THEN shipto.ship-id ELSE "Plant")
           address.AddressName = v-ship-name
           address.Address1 = v-ship-addr[1]
           address.Address2 = v-ship-addr[2]
           address.City = (IF AVAIL shipto THEN shipto.ship-city ELSE "")
           address.State = (IF AVAIL shipto THEN shipto.ship-state ELSE "")
           address.PostalCode = (IF AVAIL shipto THEN shipto.ship-zip ELSE "")
           address.Country = "USA"
           .
    
    CREATE CarrierInformation.
    ASSIGN CarrierInformation.Partner = shipmentHeader.partner
           CarrierInformation.shipmentIdentification = shipmentHeader.ShipmentIdentification
           CarrierInformation.CarrierAlphaCode = (IF AVAILABLE shipto THEN shipto.scac ELSE "SCAC")
           .               
    DEFINE VARIABLE iBolQty AS INTEGER NO-UNDO.
    DEFINE VARIABLE dBolWgt AS DECIMAL NO-UNDO.
    ASSIGN iBolQty = 0
           dBolWgt = 0
           .
    FOR EACH oe-boll NO-LOCK WHERE oe-boll.company EQ oe-bolh.company
                               AND oe-boll.bol-no  EQ oe-bolh.bol-no
                             :
        iBolQty = iBolQty + oe-boll.qty.
    END.
    dBolWgt = edshtran.tot-wght.
    
    CREATE QuantityAndWeight.
    ASSIGN QuantityAndWeight.Partner = shipmentHeader.partner
           QuantityAndWeight.shipmentIdentification = shipmentHeader.ShipmentIdentification
           QuantityAndWeight.PackingMedium = "PCS"  /* PCS, CTN PLT */
           QuantityAndWeight.PackingMaterial = "25"  /* 25: corrugated, 94: Wood */
           QuantityAndWeight.LadingQuantity = STRING(iBolQty, ">>>>>>>>>")
           QuantityAndWeight.WeightQualifier = "G"  /* Gross Weight */
           QuantityAndWeight.Weight = IF DECIMAL(dBolWgt) EQ DECIMAL(  INTEGER(dbolWgt) ) THEN STRING(dBolWgt, ">>>>>>") ELSE STRING(dBolWgt, ">>>>>>>.99")
           QuantityAndWeight.WeightUOM = "LB"  /* EA, FT, GA, LB, RL */
           .
           
    CREATE FOBRelatedInstruction.
    ASSIGN FOBRelatedInstruction.Partner = shipmentHeader.partner
           FOBRelatedInstruction.shipmentIdentification = shipmentHeader.ShipmentIdentification
           FOBRelatedInstruction.FOBPayCode = "CC"  /* CC, CF, PP */ 
           .       
                                
   IF NOT CAN-FIND(FIRST SUMMARY WHERE summary.partner = shipmentHeader.partner) THEN DO:
      CREATE SUMMARY.
      ASSIGN summary.partner = shipmentHeader.partner.
      
   END.  
END PROCEDURE.

PROCEDURE ShipmentHeaderBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
 
END PROCEDURE.

PROCEDURE ShipmentLineAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
 
 ASSIGN shipmentLine.BuyerPartNumber = EDSHLine.cust-Item-no
        shipmentLine.LineSequenceNumber = EDSHLine.cust-po-line
        shipmentLine.OrderQty = EDSHLine.Qty-orig-ord
        shipmentLine.OrderQtyUOM = EDSHLine.uom
        shipmentLine.ShipQty = EDSHLine.Qty-shipped
        shipmentLine.ShipQtyUOM = EDSHLine.uom-code
        .
 iTotalItem# = iTotalItem# + 1.
 FIND FIRST summary NO-ERROR.
 IF AVAILABLE summary THEN SUMMARY.TotalLineItemNumber = SUMMARY.TotalLineItemNumber + 1.
 
END PROCEDURE.

PROCEDURE SummaryBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE PackLevelBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
 

FIND FIRST MarksAndNumbersCollection WHERE MarksAndNumbersCollection.Partner EQ ipPartner
   AND MarksAndNumbersCollection.seq EQ ipSEq
    NO-ERROR.
IF NOT AVAIL MarksAndNumbersCollection THEN DO:
CREATE MarksAndNumbersCollection.
ASSIGN 
    MarksAndNumbersCollection.Partner = ipPartner
    MarksAndNumbersCollection.seq = ipSeq
/*    MarksAndNumbersCollection.ShipmentIdentification = shipmentHeader.ShipmentIdentification
 */
/*  PackLevelType as char init "P"*/
    MarksAndNumbersCollection.MarksAndNumbersQualifier1 = "CA"
    MarksAndNumbersCollection.MarksAndNumbers1 = STRING(ipSeq, "9999999999999999")
    .
END.
END PROCEDURE.

PROCEDURE tHeaderBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  
  
  
END PROCEDURE.

PROCEDURE tOrderLevelBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

  
  
END PROCEDURE.


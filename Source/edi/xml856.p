
/*------------------------------------------------------------------------
    File        : edi/xml856.p  SOPI ASN
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

/* ***************************  Definitions  ************************** */
DEFINE /*INPUT PARAMETER */ VARIABLE ipPartner AS CHARACTER.
DEFINE /*INPUT PARAMETER*/  VARIABLE ipSeq AS INTEGER.

DEFINE TEMP-TABLE Shipment
   FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
   .
   
DEFINE TEMP-TABLE Meta
    FIELD SenderUniqueID AS char
    FIELD SenderCompanyName AS char
    FIELD ReceiverUniqueId AS char
    FIELD InterchangeControlNumber AS integer
    FIELD GroupControIdentifier AS Character
    FIELD GroupControlNumber AS integer
    FIELD InterchangeSenderID AS character
    FIELD InterchangeReceiverID AS character
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
            
define temp-table ShipmentHeader /*excel row 159 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as char
    field ShipDate as date
    field TsetPurposeCode as char
    field ShipNoticeDate as date
    field ShipNoticeTime as char
    field ASNStructureCode as char init "0001"  /*SOPI*/
    field BillOfLadingNumber as char
    field CarrierProNumber as char
    field CurrentScheduledDeliveryDate as date
    .

define temp-table References    /* excel row 196 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field ReferenceQual as char init "MR"
    field ReferenceId as char
    .
    
define temp-table Address   /* excel row 231 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field AddressTypeCode as char
    field LocationCodeQualifier as char
    field AddressLocationNumber as char
    field AddressName as char
    field Address1 as char
    field Address2 as char
    field City as char
    field State as char
    field PostalCode as char
    field Country as char
    .    

define temp-table CarrierInformation  /* excel row 291 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field CarrierAlphaCode as char  /* SCAC code 2-4 digit */
    .    

define temp-table QuantityAndWeight   /* excel row 348 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field PackingMedium as char
    field PackingMaterial as char
    field LadingQuantity as char
    field WeightQualifier as char
    field Weight as char
    field WeightUOM as char
    .


define temp-table FOBRelatedInstruction   /* excel row 429 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field FOBPayCode as char
    .

define temp-table OrderHeader /* OrderLevel */   /* excel row 461 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field cust-po as CHARACTER XML-NODE-NAME "PrchaseOrderNumber"
    field cust-po-date as date XML-NODE-NAME "PurchaseOrderDate"
    .

Define temp-table ChargesAllowances   /* excel row 654 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field AllowChrgIndicator as char
    field AllowChrgCode as char
    field AllowChrgAmt as char
    .
    
DEFINE TEMP-TABLE PackLevel XML-NODE-NAME "PackLevel"
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN .                    
                 
define temp-table Pack    /* excel row 706 */
        /* children table - Pack, PhysicalDetails, MarksAndNumbersCollection */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN         
    field PackLevelType as char init "P"
    field CarrierPackageID as char
    .
  
define temp-table PhysicalDetail    /* excel row 712 */
    field Partner as CHARACTER SERIALIZE-HIDDEN  
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN        
/*    field PackLevelType as char init "P"*/
    field PackQualifier as char
    field Description as char
    .
     
define temp-table MarksAndNumbersCollection    /* excel row 733 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN        
/*    field PackLevelType as char init "P"*/
    field MarksAndNumbersQualifier1 as char init "CA"
    field MarksAndNumbers1 as char
    .
                  /* ItemLevel */
DEFINE TEMP-TABLE ItemLevel 
    FIELD Partner AS CHARACTER SERIALIZE-HIDDEN .  
                      
define temp-table ShipmentLine    /* excel row 976 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    FIELD seq AS INTEGER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN        
/*    field PackLevelType as char init "P"*/
    field LineSequenceNumber as char
    field BuyerPartNumber as char
    field OrderQty as integer
    field OrderQtyUOM as char
    field ShipQty as integer
    field ShipQtyUOM as char
    .
       
define temp-table summary   /* excel row 1462 */
    field Partner as CHARACTER SERIALIZE-HIDDEN
    field ShipmentIdentification as CHARACTER SERIALIZE-HIDDEN
    field TotalLineItemNumber as INTEGER  
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


DEFINE DATA-SOURCE dsShipmentHeader FOR edshtran.
DEFINE DATA-SOURCE dsOrderHeader FOR edshord.
DEFINE DATA-SOURCE dsPack FOR EDSHPack.
DEFINE DATA-SOURCE dsItem for EDSHLine. 
       
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
       DATA-RELATION rShiptHeader FOR shipment, tHeader relation-fields(Partner,Partner) nested
       DATA-RELATION rShiptOrderLevel FOR shipment, tOrderLevel relation-fields(Partner,Partner) nested
       
       DATA-RELATION rShip1 FOR tHeader, ShipmentHeader relation-fields(Partner,Partner) NESTED        
       DATA-RELATION rShipRef FOR theader, References relation-fields(Partner,Partner) nested
       DATA-RELATION rShipAddr FOR tHeader, Address relation-fields(Partner,Partner) nested  
       DATA-RELATION rShipCarrier FOR tHeader, CarrierInformation relation-fields(Partner,Partner) nested
       DATA-RELATION rShipQtyWgt FOR tHeader, QuantityAndWeight RELATION-FIELDS(Partner,Partner) nested
       DATA-RELATION rShipFob FOR tHeader, FOBRelatedInstruction RELATION-FIELDS(Partner,Partner) nested
       
       DATA-RELATION rShipOrderH FOR tOrderLevel, OrderHeader RELATION-FIELDS(Partner,Partner) nested
              /*DATA-RELATION rOrderHChg FOR OrderHeader, ChargesAllowances RELATION-FIELDS(Partner,Partner,Seq,Seq,cust-po,cust-po) nested*/
       DATA-RELATION rOrderHChg FOR tOrderLevel, ChargesAllowances RELATION-FIELDS(Partner,Partner) nested
       DATA-RELATION rShipPackLevel FOR tOrderLevel, PackLevel RELATION-FIELDS(Partner,Partner) nested
       DATA-RELATION rShipPack FOR PackLevel, Pack RELATION-FIELDS(Partner,Partner) nested
       DATA-RELATION rShipPackItem FOR PackLevel, ITEMLevel RELATION-FIELDS(Partner,Partner) nested
/*       DATA-RELATION rPackDet FOR Pack, PhysicalDetail RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested            */
/*       DATA-RELATION rPackMark FOR Pack, MarksAndNumbersCollection RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested*/
/*       DATA-RELATION rPackLine FOR Pack, ShipmentLine RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested             */
       DATA-RELATION rPackDet FOR ItemLevel, PhysicalDetail RELATION-FIELDS(Partner,Partner) nested
       DATA-RELATION rPackMark FOR ItemLevel, MarksAndNumbersCollection RELATION-FIELDS(Partner,Partner) nested
       DATA-RELATION rPackLine FOR ItemLevel, ShipmentLine RELATION-FIELDS(Partner,Partner) nested
       .
        
define var hds856xml as handle no-undo.
DEFINE VARIABLE iTotalItem# AS INTEGER NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

hds856xml = dataset Shipments:handle.

/*  CREATE shipment.                                           */
/*  ASSIGN shipment.SenderUniqueID = "123456".                 */
/*                                                             */
/*  CREATE Meta.                                               */
/*  ASSIGN Meta.SenderUniqueID = shipment.SenderUniqueID       */
/*         .                                                   */
/*                                                             */
/*  CREATE tHeader.                                            */
/*  ASSIGN tHeader.SenderUniqueID = shipment.SenderUniqueID    */
/*         .                                                   */
/*                                                             */
/*  CREATE tOrderLevel.                                        */
/*  ASSIGN tOrderLevel.SenderUniqueID = shipment.SenderUniqueID*/
/*         .                                                   */
/*                                                             */
/* MESSAGE "crt temp id" shipment.SenderUniqueID               */
/* VIEW-AS ALERT-BOX.                                          */
 
Run BuildData.
run GenerateXmlFiles.





/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

  BUFFER Shipment:attach-data-source(DATA-SOURCE dsShipment:handle, "Partner,Partner").
  BUFFER tHeader:attach-data-source(DATA-SOURCE dstHeader:handle, "Partner,Partner").    
  BUFFER tOrderLevel:attach-data-source(DATA-SOURCE dstOrderLevel:handle, "Partner,Partner").
  BUFFER PackLevel:attach-data-source(DATA-SOURCE dsPackLevel:handle, "Partner,Partner").
  BUFFER ItemLevel:attach-data-source(DATA-SOURCE dsItemLevel:handle, "Partner,Partner").
/*  BUFFER Shipment:attach-data-source(DATA-SOURCE dsShipment:handle, "SenderUniqueID,custom-proc").*/
/*  QUERY qShipment:QUERY-PREPARE("for each shipment no-lock where SenderUniqueID = '1000' ").      */
   
  /*BUFFER Meta:attach-data-source(DATA-SOURCE dsMeta:handle, "Partner,Partner").*/
  BUFFER ShipmentHeader:attach-data-source(DATA-SOURCE dsShipmentHeader:handle, "Partner,Partner,seq,seq").
  BUFFER OrderHeader:attach-data-source(DATA-SOURCE dsOrderHeader:handle, "Partner,Partner,seq,seq").
  BUFFER Pack:attach-data-source(DATA-SOURCE dsPack:handle, "Partner,Partner,Seq,seq").
  BUFFER ShipmentLine:attach-data-source(DATA-SOURCE dsItem:handle, "Partner,Partner,Seq,seq").
  
  hds856xml:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ds856xmlBeforeFill", this-procedure).
  BUFFER Shipment:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentBeforeFill", this-procedure ).
  BUFFER Shipment:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "ShipmentAfterRowFill", this-procedure ).
/*  BUFFER Meta:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "MetaBeforeFill", this-procedure ).*/
  BUFFER tHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "tHeaderBeforeFill", this-procedure ).
  BUFFER tOrderLevel:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "tOrderLevelBeforeFill", this-procedure ).
  BUFFER PackLevel:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackLevelBeforeFill", this-procedure ).
  BUFFER ItemLevel:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ItemLevelBeforeFill", this-procedure ).
  BUFFER ShipmentHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", this-procedure ).
  BUFFER ShipmentHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "ShipmentHeaderAfterRowFill", this-procedure ).
  BUFFER References:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", this-procedure ).
  BUFFER Address:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", this-procedure ).
  BUFFER CarrierInformation:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", this-procedure ).
  BUFFER QuantityAndWeight:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", this-procedure ).
  BUFFER FOBRelatedInstruction:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ShipmentHeaderBeforeFill", this-procedure ).
  
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER ChargesAllowances:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderHeaderAfterRowFill", this-procedure ).
  
  BUFFER Pack:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", this-procedure ).
  BUFFER PhysicalDetail:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", this-procedure ).
  BUFFER MarksAndNumbersCollection:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", this-procedure ).
  BUFFER ShipmentLine:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "PackBeforeFill", this-procedure ).
  BUFFER Pack:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "PackAfterRowFill", this-procedure ).
  BUFFER ShipmentLine:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "ShipmentLineAfterRowFill", this-procedure ).
  
  BUFFER Summary:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "SummaryBeforeFill", this-procedure ).
    
/*  DATA-SOURCE dsShipment:FILL-WHERE-STRING = " where Partner = 'Matt21' "  .*/
  
 
  DATA-SOURCE dsShipmentHeader:FILL-WHERE-STRING = " where Partner = 'Matt21' and seq = 2 "
                                                   /*" where Partner = '" + ipPartner + '" SEQ = " + ipSeq + ".  */
                                                   .
       
  hds856xml:fill().

   
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
  define var cXMLOutputfile as char no-undo.
   
   cXMLOutputfile = "c:\temp\856sps.xml".
   hds856xml:write-xml("file",
       cXMLOutputfile,
       false,
       ?,?,false, FALSE, false, FALSE )
       .
       
/*   ipdshandle:WRITE-XML("file"                                             */
/*        , getxmldir() + xmloutname                                         */
/*        ,FALSE /* formatted */                                             */
/*        ,? /* encoding */                                                  */
/*        ,? /* schema location */                                           */
/*        ,FALSE /* write-schema */                                          */
/*        ,FALSE /* min schema */                                            */
/*        ,TRUE /* before image !!! we want to see the errors */             */
/*        ,FALSE /* omit initial values */).                                 */
/*    FILE-INFO:FILE-NAME =  getxmldir() + xmloutname.                       */
/*    IF FILE-INFO:FULL-PATHNAME NE ? THEN ASSIGN opxmlFilename = xmloutname.*/
   

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

  /*shipmentHeader, References , Address, CarrierInformation, QuantityAndWeight, FOBRelatedInstruction */
  
    ASSIGN shipmentHeader.ShipmentIdentification = string(EDSHTran.seq)
           shipmentHeader.ShipDate = EDSHTran.Ship-Date
           shipmentHeader.TsetPurposeCode = EDSHTran.Purpose-code 
           shipmentHeader.ShipNoticeDate = EDSHTran.ship-date
           shipmentHeader.ShipNoticeTime = string(EDSHTran.Ship-time)
           shipmentHeader.ASNStructureCode = "0001"  /*SOPI*/
           shipmentHeader.BillOfLadingNumber = EDSHTran.BOL-No
           shipmentHeader.CarrierProNumber = IF EDSHTran.Pro-Number = "" THEN "TEST" ELSE EDSHTran.Pro-Number
           shipmentHeader.CurrentScheduledDeliveryDate = EDSHTran.Del-date      
           .
  

    CREATE References.
    ASSIGN References.Partner = shipmentHeader.partner
           References.shipmentIdentification = shipmentHeader.ShipmentIdentification
           .
           
    CREATE Address.
    ASSIGN Address.Partner = shipmentHeader.partner
           Address.shipmentIdentification = shipmentHeader.ShipmentIdentification
           .
    CREATE CarrierInformation.
    ASSIGN CarrierInformation.Partner = shipmentHeader.partner
           CarrierInformation.shipmentIdentification = shipmentHeader.ShipmentIdentification
           .               
    CREATE QuantityAndWeight.
    ASSIGN QuantityAndWeight.Partner = shipmentHeader.partner
           QuantityAndWeight.shipmentIdentification = shipmentHeader.ShipmentIdentification
           .
    CREATE FOBRelatedInstruction.
    ASSIGN FOBRelatedInstruction.Partner = shipmentHeader.partner
           FOBRelatedInstruction.shipmentIdentification = shipmentHeader.ShipmentIdentification
           .       
                                
   IF NOT can-find(FIRST SUMMARY WHERE summary.partner = shipmentHeader.partner) THEN DO:
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
 
 ASSIGN shipmentLine.BuyerPartNumber = EDSHLine.Item-no
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


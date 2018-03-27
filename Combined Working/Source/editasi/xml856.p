
/*------------------------------------------------------------------------
    File        : xml856.p  SOPI ASN
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


define temp-table ShipmentHeader /*excel row 159 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
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
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    field ShipmentIdentification as char
    field ReferenceQual as char init "MR"
    field ReferenceId as char
    .
    
define temp-table Address   /* excel row 231 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    field ShipmentIdentification as char
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
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    field ShipmentIdentification as char
    field CarrierAlphaCode as char  /* SCAC code 2-4 digit */
    .    

define temp-table QuantityAndWeight   /* excel row 348 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    field ShipmentIdentification as char
    field PackingMedium as char
    field PackingMaterial as char
    field LadingQuantity as char
    field WeightQualifier as char
    field Weight as char
    field WeightUOM as char
    .


define temp-table FOBRelatedInstruction   /* excel row 429 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    field ShipmentIdentification as char
    field FOBPayCode as char
    .

define temp-table OrderHeader /* OrderLevel */   /* excel row 461 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
    field ShipmentIdentification as char
    field cust-po as CHARACTER XML-NODE-NAME "PrchaseOrderNumber"
    field cust-po-date as date XML-NODE-NAME "PurchaseOrderDate"
    .

Define temp-table ChargesAllowances   /* excel row 654 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
    field ShipmentIdentification as char
    field cust-po as CHARACTER XML-NODE-NAME "PrchaseOrderNumber"
    field AllowChrgIndicator as char
    field AllowChrgCode as char
    field AllowChrgAmt as char
    .
    
                 /* PackLevel */
define temp-table Pack    /* excel row 706 */
        /* children table - Pack, PhysicalDetails, MarksAndNumbersCollection */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
    field ShipmentIdentification as char        
    field PackLevelType as char init "P"
    field CarrierPackageID as char
    .
    
define temp-table PhysicalDetail    /* excel row 712 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
    field ShipmentIdentification as char        
    field PackLevelType as char init "P"
    field PackQualifier as char
    field Description as char
    .
     
define temp-table MarksAndNumbersCollection    /* excel row 733 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
    field ShipmentIdentification as char        
    field PackLevelType as char init "P"
    field MarksAndNumbersQualifier1 as char init "CA"
    field MarksAndNumbers1 as char
    .
                  /* ItemLevel */
define temp-table ShipmentLine    /* excel row 976 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    FIELD seq AS INTEGER 
    field ShipmentIdentification as char        
    field PackLevelType as char init "P"
    field LineSequenceNumber as char
    field BuyerPartNumber as char
    field OrderQty as integer
    field OrderQtyUOM as char
    field ShipQty as integer
    field ShipQtyUOM as char
    .
       
define temp-table summary   /* excel row 1462 */
    field Partner as CHARACTER XML-NODE-NAME "TradingPartnerId"
    field ShipmentIdentification as char
    field TotalLineItemNumber as INTEGER  
    .
       

DEFINE DATA-SOURCE dsShipmentHeader FOR edshtran.
DEFINE DATA-SOURCE dsOrderHeader FOR edshord.
DEFINE DATA-SOURCE dsPack FOR EDSHPack.
DEFINE DATA-SOURCE dsItem for EDSHLine. 
       
DEFINE DATASET ds856xml
   FOR shipmentHeader, References , Address, CarrierInformation, QuantityAndWeight, FOBRelatedInstruction,
       OrderHeader, ChargesAllowances,
       Pack, PhysicalDetail, MarksAndNumbersCollection, ShipmentLine, 
       Summary
       DATA-RELATION rShipRef FOR shipmentHeader, References relation-fields(Partner,Partner,ShipmentIdentification,ShipmentIdentification) nested
       DATA-RELATION rShipAddr FOR shipmentHeader, Address relation-fields(Partner,Partner,ShipmentIdentification,ShipmentIdentification) nested  
       DATA-RELATION rShipCarrier FOR shipmentHeader, CarrierInformation relation-fields(Partner,Partner,ShipmentIdentification,ShipmentIdentification) nested
       DATA-RELATION rShipQtyWgt FOR shipmentHeader, QuantityAndWeight RELATION-FIELDS(Partner,Partner,ShipmentIdentification,ShipmentIdentification) nested
       DATA-RELATION rShipFob FOR shipmentHeader, FOBRelatedInstruction RELATION-FIELDS(Partner,Partner,ShipmentIdentification,ShipmentIdentification) nested
       DATA-RELATION rShipOrderH FOR shipmentHeader, OrderHeader RELATION-FIELDS(Partner,Partner,Seq,Seq) nested
       /*DATA-RELATION rOrderHChg FOR OrderHeader, ChargesAllowances RELATION-FIELDS(Partner,Partner,Seq,Seq,cust-po,cust-po) nested*/
       DATA-RELATION rOrderHChg FOR OrderHeader, ChargesAllowances RELATION-FIELDS(Partner,Partner,Seq,Seq) nested
       DATA-RELATION rShipPack FOR shipmentHeader, Pack RELATION-FIELDS(Partner,Partner,Seq,Seq) nested
/*       DATA-RELATION rPackDet FOR Pack, PhysicalDetail RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested            */
/*       DATA-RELATION rPackMark FOR Pack, MarksAndNumbersCollection RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested*/
/*       DATA-RELATION rPackLine FOR Pack, ShipmentLine RELATION-FIELDS(Partner,Partner,Seq,Seq,PackLevelType,PackLevelType) nested             */
       DATA-RELATION rPackDet FOR Pack, PhysicalDetail RELATION-FIELDS(Partner,Partner,Seq,Seq) nested
       DATA-RELATION rPackMark FOR Pack, MarksAndNumbersCollection RELATION-FIELDS(Partner,Partner,Seq,Seq) nested
       DATA-RELATION rPackLine FOR Pack, ShipmentLine RELATION-FIELDS(Partner,Partner,Seq,Seq) nested
       .
        
define var hds856xml as handle no-undo.
DEFINE VARIABLE iTotalItem# AS INTEGER NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

hds856xml = dataset ds856xml:handle.

Run BuildData.
run GenerateXmlFiles.

MESSAGE "Completed"
VIEW-AS ALERT-BOX.



/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  BUFFER ShipmentHeader:attach-data-source(DATA-SOURCE dsShipmentHeader:handle, "Partner,Partner,Seq,seq").
  BUFFER OrderHeader:attach-data-source(DATA-SOURCE dsOrderHeader:handle, "Partner,Partner,Seq,seq").
  BUFFER Pack:attach-data-source(DATA-SOURCE dsPack:handle, "Partner,Partner,Seq,seq").
  BUFFER ShipmentLine:attach-data-source(DATA-SOURCE dsItem:handle, "Partner,Partner,Seq,seq").
  
  DATASET ds856xml:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ds856xmlBeforeFill", this-procedure).
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
    
  DATA-SOURCE dsShipmentHeader:FILL-WHERE-STRING = " where Partner = 'Matt21' and seq = 2 "
                                                   /*" where Partner = '" + ipPartner + '" SEQ = " + ipSeq + ".  */
                                                   .
  
  DATASET ds856xml:fill().

   
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

PROCEDURE OrderheaderAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

  MESSAGE "oeheadafterrowfill: " orderheader.cust-po
  VIEW-AS ALERT-BOX.
  
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
           shipmentHeader.CarrierProNumber = EDSHTran.Pro-Number
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


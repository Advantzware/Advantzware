
/*------------------------------------------------------------------------
    File        : edi/xml855.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Feb 28 08:42:41 EST 2017
    Notes       :
       
       SPS edi 855 sturctures:
   /*       OrderAck
             Meta
             Header
                OrderHeader
                  AdditionalPOTypeCodes
                PaymentTerms
                Dates
                Contacts
                  AdditionalContactDetails
                Address
                  References
                    ReferencesIDs
                  Contacts
                    AdditionalContactDetails
                  Dates
                FOBRelatedInstruction
                CarrierInformation
                  ServiceLevelCodes
                  Address
                    Dates
                  SerialNumbers
                References
                  ReferenceIDs
                Notes
                Commodity
                Taxes
                ChargesAllowances
                  Taxes
                MonetaryAmounts
                QuantityAndWeight
                QuantyTotals
                RegulatoryCompliances
             LineItem
                OrderLine   
                  ProductID
                  NRFStandardColorAndSize
        ..............  */
        
OrderAck
*Meta
*Header
**OrderHeader
***AdditionalPOTypeCodes
**PaymentTerms
**Dates
**Contacts
***AdditionalContactDetails
**Address
***References
****ReferenceIDs
***Contacts
****AdditionalContactDetails
***Dates
**FOBRelatedInstruction
**CarrierInformation
***ServiceLevelCodes
***Address
****Dates
***SealNumbers
**References
***ReferenceIDs
**Notes
**Commodity
**Taxes
**ChargesAllowances
***Taxes
**MonetaryAmounts
**QuantityAndWeight
**QuantityTotals
**RegulatoryCompliances
*LineItem
**OrderLine
***ProductID
***NRFStandardColorAndSize
**LineItemAcknowledgement
***ProductID
**Dates
**Measurements
**PriceInformation
**ProductOrItemDescription
**MasterItemAttribute
***ItemAttribute
****Measurements
**PhysicalDetails
**References
***ReferenceIDs
**Notes
**Commodity
**Address
***References
****ReferenceIDs
***Contacts
****AdditionalContactDetails
***Dates
**Subline
***SublineItemDetail
****ProductID
****NRFStandardColorAndSize
***Dates
***PriceInformation
***ProductOrItemDescription
***PhysicalDetails
***References
****ReferenceIDs
***Notes
***Taxes
***ChargesAllowances
****Taxes
***Address
****References
*****ReferenceIDs
****Contacts
*****AdditionalContactDetails
****Dates
***Commodity
***RegulatoryCompliances
**QuantitiesSchedulesLocations
***LocationQuantity
***Dates
**Taxes
**ChargesAllowances
***Taxes
**FOBRelatedInstruction
**CarrierInformation
***ServiceLevelCodes
***Address
****Dates
***SealNumbers
**RegulatoryCompliances
*Summary
                                  
                  
                                 
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* define temp-table header */

DEFINE TEMP-TABLE OrderAck
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
DEFINE TEMP-TABLE tHeader XML-NODE-NAME "Header"
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
      
define temp-table OrderHeader  /* excel row 126 */
       field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"
       FIELD Seq AS int
       field cust-po AS CHARACTER XML-NODE-NAME "PurchaseOrderNumber"
       field TsetPurposeCode as CHARACTER INIT "00"
       FIELD PurchaseOrderDate AS CHARACTER /* format CCYYMMDD */
       field AcknowledgementType as char  /* AC,AD,AK,RJ   AC - with change, AD - without change */
       field AcknowledgementDate as CHARACTER  /* CCYYMMDD */
       .
       
define temp-table Address /* excel row 208 */
       FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
       FIELD seq AS INTEGER SERIALIZE-HIDDEN
       field AddressTypeCode as CHARACTER 
       field LocationCodeQualifier as CHARACTER 
       field AddressLocationNumber as CHARACTER 
       FIELD AddressName AS CHARACTER 
       .
               
define temp-table references  /* excel row 337 */
       FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
       FIELD seq AS INTEGER SERIALIZE-HIDDEN
       field ReferenceQual as char init "MR"
       field ReferenceId as CHARACTER INIT "R1"
       .

define temp-table QuantityTotals  /* excel row 436 */
       FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
       FIELD seq AS INTEGER SERIALIZE-HIDDEN
       field QuantityTotalQualifier as CHARACTER INIT "SQT"
       field Quantity as char
       field Description as CHARACTER INIT "Quantity Description"
       .
       
/*define temp-table lineItem  When AcknowledgementType = AK or RJ, then the detail record is not required. */
DEFINE TEMP-TABLE LineItem
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
      
define temp-table OrderLine   /* excel row 456 */
     field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"    SERIALIZE-HIDDEN
     field seq AS INTEGER XML-NODE-NAME "LineSequenceNumber"
     field cust-item-no AS CHARACTER XML-NODE-NAME "BuyerPartNumber"
     field item-no AS CHARACTER XML-NODE-NAME "VendorPartNumber"
     field qty-orig-ord AS int XML-NODE-NAME "OrderQty"
     field uom-code AS CHARACTER XML-NODE-NAME "OrderQtyUom"
     field unit-price AS DECIMAL XML-NODE-NAME "PurchasePrice"
     field PurchasePriceBasis as CHARACTER INIT "PE" /* Price per each */
     .
     
define temp-table LineItemAcknowledgement   /* excel row 514 */
     field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     field item-no AS CHARACTER SERIALIZE-HIDDEN
     field ItemStatusCode as char init "IA" /* DR,IA,IC,IP */
     field ItemScheduleQty as char
     field ItemScheduleUOM as char
     field ItemScheduleQualifier as char
     field ItemScheduleDate as char
     .
     
define temp-table LineItemReferences   /* excel row 652 */
     field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     field item-no AS CHARACTER SERIALIZE-HIDDEN
     field ReferenceQual as CHARACTER INIT "DR"
     field ReferenceId as char
     .
          
define temp-table Notes   /* excel row 666 */
     field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     field item-no AS CHARACTER SERIALIZE-HIDDEN
     field NoteCode as char
     field Note as char
     .
          
define temp-table Summary  /* excel row 1153 */ 
     field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     field item-no AS CHARACTER SERIALIZE-HIDDEN
     field TotalAmount as CHARACTER SERIALIZE-HIDDEN
     field TotalLineItemNumber as int
     .     

DEFINE DATA-SOURCE dsOrderAck FOR EDCode. 
DEFINE BUFFER bfEdCode FOR EDCode. 
DEFINE DATA-SOURCE dstHeader FOR bfEDCode .
DEFINE BUFFER bf2EdCode FOR EDCode. 
DEFINE DATA-SOURCE dsLineItem FOR bf2EDCode  .

DEFINE DATA-SOURCE dsOrderHeader FOR EDPOTran.
/*DEFINE DATA-SOURCE dsAddress FOR EDPOTran.*/
DEFINE DATA-SOURCE dsOrderLine FOR EDPOLine.

define dataset ds855xml XML-NODE-NAME "OrderAcks"
   for OrderAck,
       tHeader, OrderHeader, Address, References, QuantityTotals,
       lineitem, OrderLine, LineItemAcknowledgement, LineItemReferences, Notes, Summary       
       DATA-RELATION dr1 FOR OrderAck, tHeader relation-fields(Partner,Partner) nested
       DATA-RELATION drOrderHd FOR tHeader, OrderHeader relation-fields(Partner,Partner) nested 
       DATA-RELATION drOrderAddress FOR tHeader, Address relation-fields(Partner,Partner) nested
       DATA-RELATION drOrderReferences FOR tHeader, References relation-fields(Partner,Partner) nested  
       DATA-RELATION drOrderTotal FOR tHeader, QuantityTotals relation-fields(Partner,Partner) nested
       DATA-RELATION dr2 FOR OrderAck, LineItem relation-fields(Partner,Partner) nested
       DATA-RELATION drOrderLine FOR LineItem, OrderLine RELATION-FIELDS(Partner,Partner) nested 
       DATA-RELATION drLineLineItemAcknowledgement FOR LineItem, LineItemAcknowledgement relation-fields(Partner,Partner) NESTED /* item-no,item-no */ 
       DATA-RELATION drLineLineItemReferences FOR LineItem, LineItemReferences relation-fields(Partner,Partner) nested
       DATA-RELATION drLineLineNotes FOR LineItem, Notes relation-fields(Partner,Partner) nested
       DATA-RELATION drLineLineSummary FOR lineItem, Summary relation-fields(Partner,Partner) nested
       .

define var hds855xml as handle no-undo.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

hds855xml = dataset ds855xml:handle.

Run BuildData.
run GenerateXmlFiles.


/* **********************  Internal Procedures  *********************** */

PROCEDURE AddressBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE BuildData:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/

/*
  create Orderheader.
  assign OrderHeader.TradingPartnerId = "Lutron"
         OrderHeader.PurchaseOrderNumber = "100"
         .
         edpotran.cust-po = OrderHeader.PurchaseOrderNumber
*/
  BUFFER OrderAck:attach-data-source(DATA-SOURCE dsOrderAck:handle, "Partner,Partner").
  BUFFER tHeader:attach-data-source(DATA-SOURCE dstHeader:handle, "Partner,Partner").
  BUFFER LineItem:attach-data-source(DATA-SOURCE dsLineItem:handle, "Partner,Partner").
  BUFFER OrderHeader:attach-data-source(DATA-SOURCE dsOrderHeader:handle, "Partner,Partner,Seq,seq").
  BUFFER OrderLine:attach-data-source(DATA-SOURCE dsOrderLine:handle, "Partner,Partner,Seq,seq").
  
  DATASET ds855xml:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ds855xmlBeforeFill", this-procedure).
  BUFFER OrderAck:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderAckBeforeFill", this-procedure).
  BUFFER tHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "tHeaderBeforeFill", this-procedure).
  BUFFER LineItem:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "LineItemBeforeFill", this-procedure).
  
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderHeaderAfterRowFill", this-procedure ).
  BUFFER Address:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "AddressBeforeFill", this-procedure ).
  BUFFER References:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ReferencesBeforeFill", this-procedure ).
  BUFFER QuantityTotals:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "QuantityTotalsBeforeFill", this-procedure ).
  BUFFER OrderLine:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderLineAfterRowFill", this-procedure ).        
  BUFFER LineItemAcknowledgement:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "LineItemAcknowledgementBeforeFill", this-procedure ).
  BUFFER LineItemAcknowledgement:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "LineItemAcknowledgementAfterRowFill", this-procedure ).
  BUFFER LineItemReferences:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "LineItemReferencesBeforeFill", this-procedure ).
  BUFFER Notes:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "NotesBeforeFill", this-procedure ).
  BUFFER Summary:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "SummaryBeforeFill", this-procedure ).
  .
  
                                                                                                                                     
  DATA-SOURCE dsOrderHeader:FILL-WHERE-STRING = " where Partner = 'Matt21' and seq = 9 ".
  DATA-SOURCE dsOrderLine:FILL-WHERE-STRING = " where Partner = 'Matt21' and seq = 9 ".
  
  DATASET ds855xml:fill().

  
     
END PROCEDURE.

PROCEDURE ds855xmlBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE GenerateXMLfiles:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   define var cXMLOutputfile as char no-undo.
   
   cXMLOutputfile = "c:\temp\855sps.xml".
   hds855xml:write-xml("file",
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

PROCEDURE LineItemAcknowledgementAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  
END PROCEDURE.

PROCEDURE LineItemAcknowledgementBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE LineItemBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE LineItemReferencesBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE NotesBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE OrderAckBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE OrderHeaderAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  
  ASSIGN OrderHeader.AcknowledgementDate = string(year(today),"9999") + string(month(today),"99")
                                           + string(day(TODAY),"99") 
         OrderHeader.AcknowledgementType = "AK"  /* AC - Ack with detail and change, AD - ACK with detail without change */                                   
         OrderHeader.PurchaseOrderDate = string(year(EDPOTran.Order-date),"9999") + string(month(EDPOTran.order-date),"99") + string(day(EDPOTran.order-date),"99")                                   
         .
                                            
  /* Address, References, QuantityTotals */
  CREATE address.
  ASSIGN address.Partner = OrderHeader.Partner
         address.seq = OrderHeader.seq
         address.addressTypeCode = "VN"  /* VN:Vendor, ST:ship to */
         address.locationCodeQualifier = "6" /* plant code */
         address.addressLocationNumber = "1"  /* unique value assigned to identify a location */
         address.addressName = "Test Vendor"
         .
         
  CREATE References.
  ASSIGN References.Partner = OrderHeader.Partner
         References.seq = OrderHeader.seq
         .
  
  CREATE QuantityTotals.
  ASSIGN QuantityTotals.Partner = OrderHeader.Partner
         QuantityTotals.seq = OrderHeader.seq
         .              
         
   IF NOT can-find(FIRST SUMMARY WHERE summary.partner = OrderHeader.partner) THEN DO:
      CREATE SUMMARY.
      ASSIGN summary.partner = OrderHeader.partner.
      
   END.  
          
END PROCEDURE.

PROCEDURE OrderHeaderBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE OrderLineAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  
  /* LineItemAcknowledgement, LineItemReferences, Notes, Summary */
  
  CREATE LineItemAcknowledgement.
  ASSIGN LineItemAcknowledgement.Partner = phDataSet:get-buffer-handle("OrderAck"):buffer-field("Partner"):buffer-value
         LineItemAcknowledgement.item-no = OrderLine.item-no
         LineItemAcknowledgement.ItemScheduleQty = "1"
         LineItemAcknowledgement.ItemScheduleUOM = "EA"
         LineItemAcknowledgement.ItemScheduleQualifier = "017" /* 017: Estimated Delivery Date, 067: Current Scheduled Delivery Date */
         LineItemAcknowledgement.ItemScheduleDate = string(year(today),"9999") + string(month(today),"99") + string(day(today),"99")
         .
                 
  CREATE LineItemReferences.
  ASSIGN LineItemReferences.Partner =  phDataSet:get-buffer-handle("OrderAck"):buffer-field("Partner"):buffer-value
         LineItemReferences.item-no = OrderLine.item-no
         LineItemReferences.ReferenceId = "R-" + OrderLine.item-no 
         .
     
  CREATE Notes.
  ASSIGN Notes.Partner = phDataSet:get-buffer-handle("OrderAck"):buffer-field("Partner"):buffer-value
         Notes.item-no = OrderLine.item-no
         notes.NoteCode = "SHP"  /* shp: shipping note (ZZ) */
         notes.note = "Test Shipping Note"
         .
     
/*  CREATE Summary.                           */
/*  ASSIGN Summary.item-no = OrderLine.item-no*/
         .
                  
 FIND FIRST summary NO-ERROR.
 IF AVAILABLE summary THEN SUMMARY.TotalLineItemNumber = SUMMARY.TotalLineItemNumber + 1.


         
END PROCEDURE.

PROCEDURE QuantityTotalsBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE ReferencesBeforeFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.

END PROCEDURE.

PROCEDURE SummaryBeforeFill:
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


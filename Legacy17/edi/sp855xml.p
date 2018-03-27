
/*------------------------------------------------------------------------
    File        : edi/sp855xml.p
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

DEFINE INPUT PARAM ipOrderNumber AS INTEGER NO-UNDO.
DEFINE INPUT PARAM  ipPartner AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAM  ipSeq AS INTEGER  NO-UNDO.
DEFINE OUTPUT PARAMETER opReturn AS LOG NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i new shared}

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/acksps.i}
DEFINE VARIABLE iOrderHeaderCount AS INTEGER NO-UNDO.
DEFINE VARIABLE lOrderHeaderDeleted AS LOG NO-UNDO.
DEFINE VARIABLE iOrderLineCount AS INTEGER NO-UNDO.

/* define temp-table header */
DEFINE TEMP-TABLE OrderAck
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
DEFINE TEMP-TABLE tHeader XML-NODE-NAME "Header"
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
      
DEFINE TEMP-TABLE OrderHeader  /* excel row 126 */
       FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"
       FIELD Seq AS INT
       FIELD cust-po AS CHARACTER XML-NODE-NAME "PurchaseOrderNumber"
       FIELD TsetPurposeCode AS CHARACTER INIT "00"
       FIELD PurchaseOrderDate AS CHARACTER /* format CCYYMMDD */
       FIELD AcknowledgementType AS CHAR  /* AC,AD,AK,RJ   AC - with change, AD - without change */
       FIELD AcknowledgementDate AS CHARACTER  /* CCYYMMDD */
       .
       
DEFINE TEMP-TABLE Address /* excel row 208 */
       FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
       FIELD seq AS INTEGER SERIALIZE-HIDDEN
       FIELD AddressTypeCode AS CHARACTER 
       FIELD LocationCodeQualifier AS CHARACTER 
       FIELD AddressLocationNumber AS CHARACTER 
       FIELD AddressName AS CHARACTER 
       .
               
DEFINE TEMP-TABLE references  /* excel row 337 */
       FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
       FIELD seq AS INTEGER SERIALIZE-HIDDEN
       FIELD ReferenceQual AS CHAR INIT "MR"
       FIELD ReferenceID AS CHARACTER INIT "R1"
       .

DEFINE TEMP-TABLE QuantityTotals  /* excel row 436 */
       FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
       FIELD seq AS INTEGER SERIALIZE-HIDDEN
       FIELD QuantityTotalQualifier AS CHARACTER INIT "SQT"
       FIELD Quantity AS CHAR
       FIELD Description AS CHARACTER INIT "Quantity Description"
       .
       
/*define temp-table lineItem  When AcknowledgementType = AK or RJ, then the detail record is not required. */
DEFINE TEMP-TABLE LineItem
      FIELD Partner AS CHARACTER SERIALIZE-HIDDEN
      .
      
DEFINE TEMP-TABLE OrderLine   /* excel row 456 */
     FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"    SERIALIZE-HIDDEN
     FIELD seq AS INTEGER XML-NODE-NAME "LineSequenceNumber"
     FIELD cust-item-no AS CHARACTER XML-NODE-NAME "BuyerPartNumber"
     FIELD item-no AS CHARACTER XML-NODE-NAME "VendorPartNumber"
     FIELD qty-orig-ord AS INT XML-NODE-NAME "OrderQty"
     FIELD uom-code AS CHARACTER XML-NODE-NAME "OrderQtyUom"
     FIELD unit-price AS DECIMAL XML-NODE-NAME "PurchasePrice"
     FIELD PurchasePriceBasis AS CHARACTER INIT "PE" /* Price per each */
     .
     
DEFINE TEMP-TABLE LineItemAcknowledgement   /* excel row 514 */
     FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     FIELD seq AS INTEGER SERIALIZE-HIDDEN
     FIELD item-no AS CHARACTER SERIALIZE-HIDDEN
     FIELD ItemStatusCode AS CHAR INIT "IA" /* DR,IA,IC,IP */
     FIELD ItemScheduleQty AS CHAR
     FIELD ItemScheduleUOM AS CHAR
     FIELD ItemScheduleQualifier AS CHAR
     FIELD ItemScheduleDate AS CHAR
     .
     
DEFINE TEMP-TABLE LineItemReferences XML-NODE-NAME "References"  /* excel row 652 */
     FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     FIELD seq AS INTEGER SERIALIZE-HIDDEN
     FIELD item-no AS CHARACTER SERIALIZE-HIDDEN
     FIELD ReferenceQual AS CHARACTER INIT "DRRN"
     FIELD ReferenceID AS CHAR
     .
          
DEFINE TEMP-TABLE Notes   /* excel row 666 */
     FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     FIELD seq AS INTEGER SERIALIZE-HIDDEN
     FIELD item-no AS CHARACTER SERIALIZE-HIDDEN
     FIELD NoteCode AS CHAR
     FIELD Note AS CHAR
     .
          
DEFINE TEMP-TABLE Summary  /* excel row 1153 */ 
     FIELD Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"  SERIALIZE-HIDDEN
     FIELD item-no AS CHARACTER SERIALIZE-HIDDEN
     FIELD TotalAmount AS CHARACTER SERIALIZE-HIDDEN
     FIELD TotalLineItemNumber AS INT
     .     

DEFINE DATA-SOURCE dsOrderAck FOR EDCode. 
DEFINE BUFFER bfEdCode FOR EDCode. 
DEFINE DATA-SOURCE dstHeader FOR bfEDCode .
DEFINE BUFFER bf2EdCode FOR EDCode. 
DEFINE DATA-SOURCE dsLineItem FOR bf2EDCode  .

DEFINE DATA-SOURCE dsOrderHeader FOR EDPOTran.
/*DEFINE DATA-SOURCE dsAddress FOR EDPOTran.*/
DEFINE DATA-SOURCE dsOrderLine FOR EDPOLine.

DEFINE DATASET ds855xml XML-NODE-NAME "OrderAcks"
   FOR OrderAck,
       tHeader, OrderHeader, Address, References, QuantityTotals,
       lineitem, OrderLine, LineItemAcknowledgement, LineItemReferences, Notes, Summary       
       DATA-RELATION dr1 FOR OrderAck, tHeader RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION drOrderHd FOR tHeader, OrderHeader RELATION-FIELDS(Partner,Partner) NESTED 
       DATA-RELATION drOrderAddress FOR tHeader, Address RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION drOrderReferences FOR tHeader, References RELATION-FIELDS(Partner,Partner) NESTED  
       DATA-RELATION drOrderTotal FOR tHeader, QuantityTotals RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION dr2 FOR OrderAck, LineItem RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION drOrderLine FOR LineItem, OrderLine RELATION-FIELDS(Partner,Partner) NESTED 
       DATA-RELATION drLineLineItemAcknowledgement FOR LineItem, LineItemAcknowledgement RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION drLineLineItemReferences FOR LineItem, LineItemReferences RELATION-FIELDS(Partner,Partner) NESTED
       DATA-RELATION drLineLineNotes FOR LineItem, Notes RELATION-FIELDS(Partner,Partner) NESTED
       
       DATA-RELATION drLineLineSummary FOR lineItem, Summary RELATION-FIELDS(Partner,Partner) NESTED
       .

DEFINE VAR hds855xml AS HANDLE NO-UNDO.
    
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnDateToString RETURNS CHARACTER 
	(ipdDate AS DATE  ) FORWARD.


/* ***************************  Main Block  *************************** */

hds855xml = DATASET ds855xml:HANDLE.

RUN BuildData.
RUN GenerateXmlFiles.


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


  BUFFER OrderAck:ATTACH-DATA-SOURCE(DATA-SOURCE dsOrderAck:handle, "Partner,Partner").
  BUFFER tHeader:ATTACH-DATA-SOURCE(DATA-SOURCE dstHeader:handle, "Partner,Partner").
  BUFFER LineItem:ATTACH-DATA-SOURCE(DATA-SOURCE dsLineItem:handle, "Partner,Partner").
  BUFFER OrderHeader:ATTACH-DATA-SOURCE(DATA-SOURCE dsOrderHeader:handle, "Partner,Partner,Seq,seq").
  BUFFER OrderLine:ATTACH-DATA-SOURCE(DATA-SOURCE dsOrderLine:handle, "Partner,Partner,Seq,seq").
  
  DATASET ds855xml:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ds855xmlBeforeFill", THIS-PROCEDURE).
  BUFFER OrderAck:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderAckBeforeFill", THIS-PROCEDURE).
  BUFFER tHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "tHeaderBeforeFill", THIS-PROCEDURE).
  BUFFER tHeader:SET-CALLBACK-PROCEDURE ("AFTER-ROW-FILL", "tHeaderAfterFill", THIS-PROCEDURE).
  
  BUFFER LineItem:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "LineItemBeforeFill", THIS-PROCEDURE).
  
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", THIS-PROCEDURE ).
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderHeaderAfterRowFill", THIS-PROCEDURE ).
  BUFFER Address:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "AddressBeforeFill", THIS-PROCEDURE ).
  BUFFER References:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ReferencesBeforeFill", THIS-PROCEDURE ).
  BUFFER QuantityTotals:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "QuantityTotalsBeforeFill", THIS-PROCEDURE ).
  BUFFER OrderLine:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderLineAfterRowFill", THIS-PROCEDURE ).        
  BUFFER LineItemAcknowledgement:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "LineItemAcknowledgementBeforeFill", THIS-PROCEDURE ).
  BUFFER LineItemAcknowledgement:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "LineItemAcknowledgementAfterRowFill", THIS-PROCEDURE ).
  BUFFER LineItemReferences:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "LineItemReferencesBeforeFill", THIS-PROCEDURE ).
  BUFFER Notes:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "NotesBeforeFill", THIS-PROCEDURE ).
  BUFFER Summary:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "SummaryBeforeFill", THIS-PROCEDURE ).
  .
  
  DATA-SOURCE dsOrderAck:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner)  .

                                                                                                                                     
  DATA-SOURCE dsOrderHeader:FILL-WHERE-STRING = " where Partner = " + quoter(ipPartner) + " and seq = " + string(ipSeq) .
                                                
  DATA-SOURCE dsOrderLine:FILL-WHERE-STRING =  " where Partner = " + quoter(ipPartner) + " and seq = " + string(ipSeq).
  
  DATASET ds855xml:FILL().
  
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
   DEFINE VAR cXMLOutputfile AS CHAR NO-UNDO.
   
   cXMLOutputfile = (IF acksps-cha <> "" THEN acksps-cha ELSE "c:\temp\")  +
                 "SPS855_Order#_" + string(ipOrderNumber) + ".xml".
   
   hds855xml:WRITE-XML("file",
       cXMLOutputfile,
       FALSE,
       ?,?,FALSE, FALSE, FALSE, FALSE )
       .
       
   MESSAGE "xml file generated: " cXMLOutputfile SKIP
     VIEW-AS ALERT-BOX.    
   


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
  
  iOrderHeaderCount = iOrderHeaderCount + 1.
  lOrderHeaderDeleted = NO.
  
  ASSIGN OrderHeader.AcknowledgementDate = fnDateToString(TODAY) 
         OrderHeader.AcknowledgementType = "AD"  /* AC - Ack with detail and change, AD - ACK with detail without change */                                   
         OrderHeader.PurchaseOrderDate = fnDateToString(EDPOTran.Order-Date)                                   
         .

/*
  IF iOrderHeaderCount = 1 THEN DO:
     DELETE OrderHeader.
     lOrderHeaderDeleted = YES. 
     RETURN. 
  END.                                         
  */                                          
  /* Address, References, QuantityTotals */
  CREATE address.
  ASSIGN address.Partner = OrderHeader.Partner
         address.seq = OrderHeader.seq
         address.addressTypeCode = "VN"  /* VN:Vendor, ST:ship to */
         address.locationCodeQualifier = "6" /* plant code */
         address.addressLocationNumber = "Main Location"  /* unique value assigned to identify a location */
         address.addressName = "Test Vendor"
         .
         
  CREATE address.
  ASSIGN address.Partner = OrderHeader.Partner
         address.seq = OrderHeader.seq
         address.addressTypeCode = "ST"  /* VN:Vendor, ST:ship to */
         address.locationCodeQualifier = "6" /* plant code */
         address.addressLocationNumber = "Main Location"  /* unique value assigned to identify a location */
         address.addressName = "Test Vendor"
         .
                
  CREATE References.
  ASSIGN References.Partner = OrderHeader.Partner
         References.seq = OrderHeader.seq
         References.ReferenceQual = "MR"
         References.ReferenceID = "Merchandise Type Code"
         .
         
  CREATE References.
  ASSIGN References.Partner = OrderHeader.Partner
         References.seq = OrderHeader.seq
         References.ReferenceQual = "19"
         References.ReferenceID = "Division-19"       
         .
         
  CREATE References.
  ASSIGN References.Partner = OrderHeader.Partner
         References.seq = OrderHeader.seq
         References.ReferenceQual = "TPP"
         References.ReferenceID = "Third Party Payment"       
         .       
  
  CREATE QuantityTotals.
  ASSIGN QuantityTotals.Partner = OrderHeader.Partner
         QuantityTotals.seq = OrderHeader.seq
         .              
         
   IF NOT CAN-FIND(FIRST SUMMARY WHERE summary.partner = OrderHeader.partner) THEN DO:
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
  
/*  /* LineItemAcknowledgement, LineItemReferences, Notes, Summary */*/
/*  iOrderLineCount = iOrderLineCount + 1.                           */
/*                                                                   */
/*  IF lOrderHeaderDeleted THEN DO:                                  */
/*/*     lOrderHeaderDeleted = NO.*/                                 */
/*     RETURN.                                                       */
/*  END.                                                             */
/*                                                                   */
/*  MESSAGE "OL after: " orderline.item-no orderline.partner         */
/*  VIEW-AS ALERT-BOX.                                               */
  
  IF orderline.item-no <> "" AND
    NOT CAN-FIND(FIRST LineItemAcknowledgement WHERE LineItemAcknowledgement.item-no = orderline.item-no) THEN DO: 
   CREATE LineItemAcknowledgement.
   ASSIGN LineItemAcknowledgement.Partner = phDataSet:get-buffer-handle("OrderAck"):buffer-field("Partner"):buffer-value
         LineItemAcknowledgement.item-no = OrderLine.item-no
         LineItemAcknowledgement.ItemScheduleQty = "1"
         LineItemAcknowledgement.ItemScheduleUOM = "EA"
         LineItemAcknowledgement.ItemScheduleQualifier = "017" /* 017: Estimated Delivery Date, 067: Current Scheduled Delivery Date */
         LineItemAcknowledgement.ItemScheduleDate = fnDateToString(TODAY)
         .
      FIND FIRST summary NO-ERROR.
      IF AVAILABLE summary THEN SUMMARY.TotalLineItemNumber = SUMMARY.TotalLineItemNumber + 1.
         
  END.
  
  IF orderline.item-no <> "" AND
     NOT CAN-FIND(FIRST LineItemReferences WHERE LineItemReferences.item-no = orderline.item-no) THEN DO:               
   CREATE LineItemReferences.
   ASSIGN LineItemReferences.Partner =  phDataSet:get-buffer-handle("OrderAck"):buffer-field("Partner"):buffer-value
         LineItemReferences.item-no = OrderLine.item-no
         LineItemReferences.ReferenceID = "R-" + OrderLine.item-no 
         .
  END.
  IF orderline.item-no <> "" AND
     NOT CAN-FIND(FIRST LineItemAcknowledgement WHERE LineItemAcknowledgement.item-no = orderline.item-no) THEN DO:   
   CREATE Notes.
   ASSIGN Notes.Partner = phDataSet:get-buffer-handle("OrderAck"):buffer-field("Partner"):buffer-value
         Notes.item-no = OrderLine.item-no
         notes.NoteCode = "SHP"  /* shp: shipping note (ZZ) */
         notes.note = "Test Shipping Note"
         .
  END.
     
/*  CREATE Summary.                           */
/*  ASSIGN Summary.item-no = OrderLine.item-no*/
         .
                  

         
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

PROCEDURE tHeaderAfterFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION fnDateToString RETURNS CHARACTER 
	(ipdDate AS DATE):
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/	

		DEFINE VARIABLE opcDate AS CHARACTER NO-UNDO.
        opcDate = STRING(YEAR(ipdDate),"9999") 
                    + "-" + string(MONTH(ipdDate),"99")
                    + "-" + string(DAY(ipdDate),"99").        
		RETURN opcDate.
		
END FUNCTION.


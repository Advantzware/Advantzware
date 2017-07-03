
/*------------------------------------------------------------------------
    File        : xml855.p
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
define temp-table OrderHeader  /* excel row 126 */
       field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"
       FIELD Seq AS int
       field cust-po AS CHARACTER XML-NODE-NAME "PurchaseOrderNumber"
       field TsetPurposeCode as char
       field AcknowledgementType as char  /* AC,AD,AK,RJ */
       field AcknowledgementDate as CHARACTER  /* CCYYMMDD */
       .
       
define temp-table Address /* excel row 208 */
       FIELD Partner AS char
       FIELD seq AS int
       field AddressTypeCode as char
       field LocationCodeQualifier as char
       field AddressLocationNumber as char
       .
               
define temp-table references  /* excel row 337 */
       FIELD Partner AS char
       FIELD seq AS int
       field ReferenceQual as char init "MR"
       field ReferenceId as char
       .

define temp-table QuantityTotals  /* excel row 436 */
       FIELD Partner AS char
       FIELD seq AS int
       field QuantityTotalQualifier as char
       field Quantity as char
       field Description as char
       .
       
/*define temp-table lineItem  When AcknowledgementType = AK or RJ, then the detail record is not required. */
define temp-table OrderLine   /* excel row 456 */
     field Partner AS CHARACTER XML-NODE-NAME "TradingPartnerId"    
     field seq AS INTEGER XML-NODE-NAME "LineSequenceNumber"
     field cust-item-no AS CHARACTER XML-NODE-NAME "BuyerPartNumber"
     field item-no AS CHARACTER XML-NODE-NAME "VendorPartNumber"
     field qty-orig-ord AS decimal XML-NODE-NAME "OrderQty"
     field uom-code AS CHARACTER XML-NODE-NAME "OrderQtyUom"
     field unit-price AS DECIMAL XML-NODE-NAME "PurchasePrice"
     field PurchasePriceBasis as char
     .
     
define temp-table LineItemAcknowledgement   /* excel row 514 */
     field item-no AS CHARACTER 
     field ItemStatusCode as char init "IA" /* DR,IA,IC,IP */
     field ItemScheduleQty as char
     field ItemScheduleUOM as char
     field ItemScheduleQualifier as char
     field ItemScheduleDate as char
     .
     
define temp-table LineItemReferences   /* excel row 652 */
     field item-no AS CHARACTER 
     field ReferenceQual as char
     field ReferenceId as char
     .
          
define temp-table Notes   /* excel row 666 */
     field item-no AS CHARACTER 
     field NoteCode as char
     field Note as char
     .
          
define temp-table Summary  /* excel row 1153 */ 
     field item-no AS CHARACTER 
     field TotalAmount as char
     field TotalLineItemNumber as char
     .     

DEFINE DATA-SOURCE dsOrderHeader FOR EDPOTran.
/*DEFINE DATA-SOURCE dsAddress FOR EDPOTran.*/
DEFINE DATA-SOURCE dsOrderLine FOR EDPOLine.

define dataset ds855xml
   for OrderHeader
       , Address, References, QuantityTotals, OrderLine, LineItemAcknowledgement, LineItemReferences, Notes, Summary 
       DATA-RELATION dsOrderAddress FOR OrderHeader, Address relation-fields(Partner,Partner,Seq,Seq) nested
       DATA-RELATION dsOrderReferences FOR OrderHeader, References relation-fields(Partner,Partner,Seq,Seq) nested  
       DATA-RELATION dsOrderTotal FOR OrderHeader, QuantityTotals relation-fields(Partner,Partner,Seq,Seq) nested
       DATA-RELATION drOrderHeader FOR OrderHeader, OrderLine RELATION-FIELDS(Partner,Partner,Seq,Seq) 
       DATA-RELATION dsLineLineItemAcknowledgement FOR OrderLine, LineItemAcknowledgement relation-fields(item-no,item-no) nested 
       DATA-RELATION dsLineLineItemReferences FOR OrderLine, LineItemReferences relation-fields(item-no,item-no) nested
       DATA-RELATION dsLineLineNotes FOR OrderLine, Notes relation-fields(item-no,item-no) nested
       DATA-RELATION dsLineLineSummary FOR OrderLine, Summary relation-fields(item-no,item-no) nested
       .

define var hds855xml as handle no-undo.
    
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

hds855xml = dataset ds855xml:handle.

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

/*
  create Orderheader.
  assign OrderHeader.TradingPartnerId = "Lutron"
         OrderHeader.PurchaseOrderNumber = "100"
         .
         edpotran.cust-po = OrderHeader.PurchaseOrderNumber
*/

  BUFFER OrderHeader:attach-data-source(DATA-SOURCE dsOrderHeader:handle, "Partner,Partner,Seq,seq").
  BUFFER OrderLine:attach-data-source(DATA-SOURCE dsOrderLine:handle, "Partner,Partner,Seq,seq").
  
  DATASET ds855xml:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "ds855xmlBeforeFill", this-procedure).
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER OrderHeader:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderHeaderAfterRowFill", this-procedure ).
  BUFFER Address:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER References:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER QuantityTotals:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER OrderLine:SET-CALLBACK-PROCEDURE ("After-Row-FILL", "OrderLineAfterRowFill", this-procedure ).        
  BUFFER LineItemAcknowledgement:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER LineItemReferences:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER Notes:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  BUFFER Summary:SET-CALLBACK-PROCEDURE ("BEFORE-FILL", "OrderHeaderBeforeFill", this-procedure ).
  .
  
                                                                                                                                     
  DATA-SOURCE dsOrderHeader:FILL-WHERE-STRING = " where Partner = 'Matt21' and seq = 9 ".
  
  DATASET ds855xml:fill().
/*   hds855xml:fill().*/

  
     
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

PROCEDURE OrderHeaderAfterRowFill:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER DATASET-HANDLE phDataSet.
  
  ASSIGN OrderHeader.AcknowledgementDate = string(year(today),"9999") + string(month(today),"99")
                                           + string(day(TODAY),"99") 
         OrderHeader.AcknowledgementType = "AK"                                   
        .
                                            
  /* Address, References, QuantityTotals */
  CREATE address.
  ASSIGN address.Partner = OrderHeader.Partner
         address.seq = OrderHeader.seq
         .
         
  CREATE References.
  ASSIGN References.Partner = OrderHeader.Partner
         References.seq = OrderHeader.seq
         .
  
  CREATE QuantityTotals.
  ASSIGN QuantityTotals.Partner = OrderHeader.Partner
         QuantityTotals.seq = OrderHeader.seq
         .              
         
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
  ASSIGN LineItemAcknowledgement.item-no = OrderLine.item-no
         .
          
  CREATE LineItemReferences.
  ASSIGN LineItemReferences.item-no = OrderLine.item-no
         .
     
  CREATE Notes.
  ASSIGN Notes.item-no = OrderLine.item-no
         .
     
  CREATE Summary.
  ASSIGN Summary.item-no = OrderLine.item-no
         .
                       
END PROCEDURE.


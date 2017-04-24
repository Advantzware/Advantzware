/*------------------------------------------------------------------------
    File        : oe/oe856xml.p
    Purpose     : 

    Syntax      :

    Description : Generate Advanced Shipment Notice to send SPS Commerce in XML format

    Author(s)   : 
    Created     : Wed Nov 09 19:32:08 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Xml tables: Header: tOrderHeader, tContacts, tAddress, tReferences, tNotes
           LineItem: tOrderLine, tProductORItemDescription, tReferences, tQuantitiesSchedulesLocations,
           Sumary: tSummary          
*/

DEFINE TEMP-TABLE ShipmentHeader
    FIELD TradingPartnerId AS char
    FIELD ShipmentIdentification AS char
    FIELD ShipDate AS char
    FIELD ShipmentTime AS char
    FIELD ShipNoticeDate AS char
    FIELD ShipNoticeTime AS char
    FIELD ASNSturctureCode AS char
    FIELD BuyerCurrency AS char
    FIELD BillOfLadingNumber AS char
    FIELD CarrierProNumber AS char
    FIELD AppointmentNumber AS char
    FIELD PickupNumber AS char
    FIELD RequestedPickupDate AS char
    FIELD ScheduledShipDate AS char
    FIELD CurrentScheduledDeliverDate AS char
    FIELD CurrentScheduledShipDate AS char   
    FIELD DocumentVersion AS char
    FIELD DocumentRevision AS char
    .

DEFINE TEMP-TABLE PaymentTerms
    FIELD TermsType AS char
    FIELD TermsBasisDateCode AS char
    FIELD TermsTimeRelationCode AS char
    FIELD TermsDiscountPercentage AS char
    FIELD TermsDiscountDate AS char
    FIELD TermsDiscountDueDays AS char
    FIELD TermsNetDueDate AS char
    FIELD TermsNetDays AS char
    FIELD TermsDiscountAmount AS char
    FIELD TermsDeferedDueDate AS char
    FIELD TermsDeferedAmountDue AS char
    FIELD PercendOfInvoicePayable AS char
    FIELD TermsDescription AS char
    FIELD TermsDueDay AS char
    FIELD PaymentMethodCode AS char
    FIELD PaymentMethodId AS char
    FIELD LatePaymentChargePercent AS char
    FIELD TermsStartDate AS char
    FIELD TermsDueDateQual AS char
    FIELD AmountSubjectToDiscount AS char
    FIELD DiscountAmountDue AS char
    .
  
      
DEFINE TEMP-TABLE Contacts
    FIELD ContactTypeCode AS char
    FIELD ContactName AS char
    FIELD PrimaryPhone AS char
    FIELD PrimaryEmail AS char
    .
    
DEFINE TEMP-TABLE Address
    FIELD AddressTypeCode AS char
    FIELD LocationCodeQualifier AS char
    FIELD AddressLocationNumber AS char
    FIELD AddressName AS char
    FIELD Address1 AS char
    FIELD Address2 AS char
    FIELD City AS char
    FIELD State AS char
    FIELD PostalCode AS char
    FIELD Country AS char
    .
    
DEFINE TEMP-TABLE References
    FIELD ReferencesQual AS char
    FIELD ReferenceID AS char
    .
    
DEFINE TEMP-TABLE Notes
    FIELD NoteCode AS char
    FIELD Note AS char
    .            
    
DEFINE TEMP-TABLE Taxes
   FIELD TaxTypeCode AS char
   FIELD TaxAmount AS char
   FIELD TaxPercentQual AS char
   FIELD TaxPercent AS char
   FIELD JurisdictionQual AS char
   FIELD JurisdictionCode AS char
   FIELD TaxExemptCode AS char
   FIELD TaxId AS char
   FIELD AssignedID AS char
   FIELD DESCRIPTION AS char
   .    
   
DEFINE TEMP-TABLE QuantityTotals
   FIELD QuantityTotalQualifier AS char
   FIELD Quantity AS char
   FIELD QuantityUOM AS char
   FIELD WeightQualifier AS char
   FIELD WeightUOM AS char
   FIELD Volumn AS char
   FIELD VolumeUOM AS char
   FIELD DESCRIPTION AS char
   .
   
DEFINE TEMP-TABLE OrderHeader NO-UNDO    
    FIELD DepositOrderNumber AS CHARACTER
    FIELD InternalOrderNumber AS CHARACTER 
    FIELD InternalOrderDate AS CHARACTER 
    FIELD PurchaseOrderNumber AS CHARACTER 
    FIELD ReleaseNumber AS CHARACTER 
    FIELD PurchaseOrderDate AS DATE
    FIELD Department AS CHARACTER 
    FIELD Vendor AS CHARACTER  
    FIELD CustomerAccountNumber AS CHARACTER
    FIELD CustomerOrderNumber AS CHARACTER  
    FIELD DeliveryDate AS CHARACTER 
    FIELD DeliveryTime AS CHARACTER
    FIELD Header1_id AS RECID XML-NODE-TYPE "HIDDEN"
    .
   
DEFINE TEMP-TABLE OrderLine
   FIELD LineSequenceNumber AS char
   FIELD BuyerPartNumber AS char
   FIELD VendorPartNumber AS char
   FIELD OrderQty AS char
   FIELD OrderQtyUOM AS char
   FIELD PurchasePrice AS char
   FIELD PurchasePriceBasis AS char
   .
   
DEFINE TEMP-TABLE ProductiOrItemDescription
   FIELD ProductCharacteristicCode AS char
   FIELD ProductDescription AS char
   .

DEFINE TEMP-TABLE QuantitiesSchedulesLocation
   FIELD TotalQty AS char
   FIELD TotalQtyUOM AS char
   .
DEFINE TEMP-TABLE Dates
   FIELD DateTimeQualifier AS char
   FIELD DATE AS char
   .
   
DEFINE TEMP-TABLE Summary
   FIELD TotalAmount AS char
   FIELD TotalLineItemNumber AS char
   .
         
        
DEFINE DATASET dsOrder FOR OrderHeader, Contacts, Address, References, Notes, OrderLine, ProductiOrItemDescription, QuantitiesSchedulesLocation, Dates
  .
     
/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */

DEFINE VARIABLE /* input param */ ipOrderNumber AS INTEGER NO-UNDO.
def VARIABLE /*input param*/ ipXmlFile as cha no-undo.
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPDS AS HANDLE NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.

ipXmlFile = "C:\ASIWorks\SPSCommerceOrder7.7.1\asi_files\850Acknowledge.xml".
hPDS = DATASET dsOrder:HANDLE.

/* create temp-table records and dataset */
FOR EACH oe-ord WHERE ord-no = ipOrderNumber NO-LOCK:
    CREATE OrderHeader.
    ASSIGN OrderHeader.PurchaseOrderNumber = string(oe-ord.ord-no).
END.

ASSIGN cSourceType = "FILE"
            /*cFile = "C:\temp\ardenxml\P-ASN-000177_0_Rev.xml"  Single Est Xml*/
            /*cFile = "C:\temp\ardenxml\Example Tandem Job_0_Rev.xml" */
            /*cFile = "C:\temp\ardenxml\FC Combo Die_0_Rev.xml" */
            /*cFile = "C:\temp\ardenxml\Display Set_0_Rev.xml"  */
            /*=========*/
            cFile = ipXmlFile /*"C:\ASI\FOL TEST - DO NOT USE_2.xml" */
            /*cFile = "C:\temp\ardenxml\newxml\Example Tandem Job_0.xml" */
            /*cFile = "C:\temp\ardenxml\newxml\FC Combo Die_1.xml"*/
            /*cFile = "C:\temp\ardenxml\newxml\Display Set_0.xml"  */
              
            cReadMode = "EMPTY"
            cSchemaLocation = ?
            lOverrideDefaultMapping = NO.


    lReturn = hPDS:WRITE-XMLSCHEMA(cSourceType,cFile,TRUE,?,FALSE).

MESSAGE lReturn
VIEW-AS ALERT-BOX.
IF lReturn THEN DO:
END.

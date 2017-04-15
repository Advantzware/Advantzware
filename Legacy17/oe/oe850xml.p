
/*------------------------------------------------------------------------
    File        : oe850xml.p
    Purpose     : 

    Syntax      :

    Description : Import orders from edi 850 xml file and build edi table records

    Author(s)   : 
    Created     : Wed Nov 09 19:32:08 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/* Xml tables: Header: tOrderHeader, tContacts, tAddress, tReferences, tNotes
           LineItem: tOrderLine, tProductORItemDescription, tReferences, tQuantitiesSchedulesLocations,
           Sumary: tSummary          
*/

DEFINE TEMP-TABLE OrderHeader
    FIELD TradingPartnerId AS char
    FIELD PurchaseOrderNumber AS char
    FIELD TsetPurposeCode AS char
    FIELD PrimaryPOTypeCode AS char
    FIELD PurchaseOrderDate AS char
    FIELD BuyersCurrency AS char
    FIELD Vendor AS char
    .

DEFINE TEMP-TABLE Contacts
    FIELD ContactTypeCode AS char
    FIELD ContactName AS char
    FIELD PrimaryPhone AS char
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

/*=== sample codes ==== */

/*DEFINE VARIABLE lRetOK                  AS LOGICAL   NO-UNDO.             */
/*DEFINE VARIABLE cSourceType             AS CHARACTER NO-UNDO.             */
/*DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.             */
/*DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.             */
/*DEFINE VARIABLE cFieldTypeMapping       AS CHARACTER NO-UNDO.             */
/*DEFINE VARIABLE cVerifySchemaMode       AS CHARACTER NO-UNDO.             */
/*                                                                          */
/*DEFINE TEMP-TABLE ttEdPOTran LIKE EDPOTran.                               */
/*DEFINE TEMP-TABLE ttEdPOLine LIKE edPOLine.                               */
/*                                                                          */
/*DEFINE DATASET dsEdPO FOR ttEdPOTran, ttEdPOLine                          */
/*  DATA-RELATION EdPOTran FOR ttEdPOTran,                                  */
/*    ttEdPOLine RELATION-FIELDS(Partner,Partner,Seq,Seq) .                 */
/*                                                                          */
/*                                                                          */
/*ASSIGN                                                                    */
/*  cSourceType             = "file"                                        */
/*  cFile                   = "c:\asiworks\SPSCommerceOrder7.7.1\orders.xsd"*/
/*  lOverrideDefaultMapping = FALSE                                         */
/*  cFieldTypeMapping       = ?                                             */
/*  cVerifySchemaMode       = ? /*"strict"*/ .                              */
/*                                                                          */
/*lRetOK = DATASET dsEdPO:READ-XMLSCHEMA (cSourceType, cFile,               */
/*  lOverrideDefaultMapping, cFieldTypeMapping,cVerifySchemaMode).          */
/*MESSAGE lRetOk  DATASET dsEdPO:NUM-entries ":"  DATASET dsEdPO:NUM-results*/
/*VIEW-AS ALERT-BOX.                                                        */
/*FOR EACH ttEDPoTran:                         */
/*                                             */
/*   DISPLAY ttEdPOTran.partner ttEdPOTran.seq.*/
/*                                             */
/*   END.                                      */
/* === END OF sample codes ==== */

/* === sample using temp-table == */
/*DEFINE VARIABLE lRetOK                  AS LOGICAL   NO-UNDO.               */
/*DEFINE VARIABLE cSourceType             AS CHARACTER NO-UNDO.               */
/*DEFINE VARIABLE cFile                   AS CHARACTER NO-UNDO.               */
/*DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL   NO-UNDO.               */
/*DEFINE VARIABLE cFieldTypeMapping       AS CHARACTER NO-UNDO.               */
/*DEFINE VARIABLE cVerifySchemaMode       AS CHARACTER NO-UNDO.               */
/*DEFINE VARIABLE hTable                  AS HANDLE    NO-UNDO.               */
/*                                                                            */
/*CREATE TEMP-TABLE hTable.                                                   */
/*ASSIGN                                                                      */
/*  cSourceType             = "file"                                          */
/*  cFile                   = "c:\asiworks\SPSCommerceOrder7.7.1\orders.xsd"  */
/*  lOverrideDefaultMapping = FALSE                                           */
/*  cFieldTypeMapping       = "address2,CLOB"                                 */
/*  cVerifySchemaMode       = ?.                                              */
/*                                                                            */
/*lRetOK = hTable:READ-XMLSCHEMA (cSourceType, cFile, lOverrideDefaultMapping,*/
/*  cFieldTypeMapping,cVerifySchemaMode).                                     */
  
/* === end of sample temp-table === */
def VARIABLE /*input param*/ ipXmlFile as cha no-undo.
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPDS AS HANDLE NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.

ipXmlFile = "C:\ASIWorks\SPSCommerceOrder7.7.1\asi_files\POdc4-6f40e83b-8f4c-4704-b30a-cd9196414374.p.xml".
hPDS = DATASET dsOrder:HANDLE.

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


    lReturn = hPDS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
MESSAGE lReturn
VIEW-AS ALERT-BOX.
IF lReturn THEN DO:
END.

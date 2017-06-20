
/*------------------------------------------------------------------------
    File        : oe850imp.p
    Purpose     : 

    Syntax      :

    Description : Import edi orders (850)

    Author(s)   : 
    Created     : Fri Dec 16 01:44:33 EST 2016
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*
{C:\ASIWorks\SPSCommerceOrder7.7.1\orders.i} /* orig definition from SPS Commerce orders.xsd */
*/
{edi\ed850def.i}  /* simplied version of orders.i for ASI edi 850 import */


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
DEF /* variable */ INPUT PARAM ipXmlFile AS cha NO-UNDO.
DEFINE VARIABLE cSourceType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cTargetType AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile AS CHARACTER NO-UNDO.
DEFINE VARIABLE cReadMode AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE hPDS AS HANDLE NO-UNDO.
DEFINE VARIABLE lReturn AS LOGICAL NO-UNDO. 
DEFINE VARIABLE lOverrideDefaultMapping AS LOGICAL NO-UNDO.

DEFINE STREAM stEDI.
OUTPUT STREAM stEDI TO c:\temp\edirec.tmp.

/*ipXmlFile = "C:\ASIWorks\SPSCommerceOrder7.7.1\asi_files\POdc4-e43d6789-e152-425e-b45a-a4f63bdf4492.p.xml".*/
/*ipXmlFile = "C:\ASIWorks\SPSCommerceOrder7.7.1\asi_files\PO_test1.xml".                                    */
/*  ipxmlFile = "C:\ASIWorks\SPSCommerceOrder7.7.1\7.7.1_Orders.xml".*/

hPDS = DATASET Orders:HANDLE.


ASSIGN cSourceType = "FILE"
       cFile = ipXmlFile 
       cReadMode = "EMPTY"
       cSchemaLocation = ?
       lOverrideDefaultMapping = NO.


    lReturn = hPDS:READ-XML (cSourceType, cFile, cReadMode, cSchemaLocation, lOverrideDefaultMapping).
    IF lReturn THEN RUN BuildEdiTables. 
   
/*FOR EACH OrderHeader NO-LOCK :                                                                       */
/*   DISPLAY orderheader.header1_id orderHeader.TradingPartnerId PurchaseOrderNumber PurchaseOrderDate.*/
/*END.                                                                                                 */
/*FOR EACH orderline:                                                                                  */
/*   DISPLAY orderline.lineitem_id orderline.orderqty                                                  */
/*           orderline.buyerpartnumber WITH TITLE "orderline".                                         */
/*END.                                                                                                 */
/*                                                                                                     */
/*FOR EACH contacts:                                                                                   */
/*    DISPLAY contacttypecode contactname WITH TITLE "contacts".                                       */
/*END.                                                                                                 */
/*                                                                                                     */
/*FOR EACH address:                                                                                    */
/*    DISPLAY addresstypecode addressname addressLocationNumber WITH TITLE "address".                  */
/*                                                                                                     */
/*END.                                                                                                 */
    
OUTPUT STREAM stEDI CLOSE.
    
/* **********************  Internal Procedures  *********************** */

PROCEDURE BuildEdiTables:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEFINE VARIABLE li-seq AS INTEGER NO-UNDO.
 
 FOR EACH orderHeader:
 FIND LAST EDPOTran WHERE EDPOTran.Partner = OrderHeader.TradingPartnerId USE-INDEX seq
      NO-LOCK NO-ERROR.
 li-seq = IF AVAILABLE EDPOTran THEN EDPOTran.Seq + 1 ELSE 1.
 FIND FIRST EDMast WHERE EDMast.Partner = OrderHeader.TradingPartnerId NO-LOCK NO-ERROR.
 IF NOT AVAILABLE EDMast THEN DO:
    CREATE EDMast.
    ASSIGN EDMast.Partner = OrderHeader.TradingPartnerId
           EDMast.Cust = OrderHeader.TradingPartnerId
           . 
 END.   

  /*  CREATE EDPOTran, edPOLine */
   CREATE edpotran.
        ASSIGN
/*          eddoc.interpdate = TODAY                       */
/*          eddoc.interptime = TIME                        */
/*          eddoc.st-code =                                */
/*          IF AVAIL edshipto THEN edshipto.st-code ELSE ""*/
/*          eddoc.fgid = "PO"                              */
/*          eddoc.version = ws_version                     */
         
/*          edpotran.partner = eddoc.partner*/
/*          edpotran.seq = eddoc.seq        */
          edpotran.cust     = edmast.cust 
          EDPOTran.Partner = OrderHeader.TradingPartnerId
          EDPOTran.Seq = li-seq
          edpotran.cust-po = OrderHeader.PurchaseOrderNumber
          edpotran.order-date = OrderHeader.PurchaseOrderDate
          edpotran.request-date = TODAY
/*          edpotran.sf-code =                                                          */
/*                          if edmast.sf-code > "" then edmast.sf-code else edco.sf-code*/
/*          edpotran.by-code = ordering_store_number                                    */
/*          edpotran.vn-code = edmast.we-vend-no                                        */
/*          edpotran.st-code = IF AVAIL edshipto THEN edshipto.st-code ELSE ""          */
/*          edpotran.cust-div = IF AVAIL edshipto THEN edshipto.cust-region ELSE ""     */
          .

  FIND FIRST Address WHERE Address.AddressTypeCode = "ST" /* ship-to */
           NO-LOCK NO-ERROR.
  IF AVAILABLE address THEN 
     ASSIGN EDPOTran.Ship-name = address.addressName                           
            EDPOTran.Ship-address[1] = address.Address1                         
            EDPOTran.Ship-address[2] = address.address2                         
            EDPOTran.Ship-city      = address.city                           
            EDPOTran.Ship-st        = address.state                           
            EDPOTran.Ship-country   = address.country                           
            EDPOTran.Ship-zip = address.postalCode
            EDPOTran.St-code = address.AddressLocationNumber
            EDPOTran.Ship-loc-code = address.AddressLocationNumber 
            .
     
/*  edpotran.by-code                                   */
/*  edpotran.st-code                                   */
/*  EDPOTran.Cust                                      */
/*  EDPOTran.cust-div                                  */
/*  EDPOTran.cust-dept                                 */
/*  EDPOTran.ship-date-code                            */
/*  EDPOTran.Request-date                              */
/*  EDPOTran.cancel-date-code                          */
/*  EDPOTran.Cancel-date                               */
/*  EDPOTran.purpose-code                              */
/*  EDPOTran.Order-type                                */
/*  EDPOTran.Catalog                                   */
/*  EDPOTran.sf-code                                   */
/*  EDPOTran.bo-flag                                   */
/*  EDPOTran.ship-stat                                 */
/*  EDPOTran.contract                                  */
/*  EDPOTran.release-no                                */
/*  EDPOTran.promo-code                                */
/*  EDPOTran.package-code                              */
/*  EDPOTran.Ship-name                                 */
/*  EDPOTran.Ship-address[1]                           */
/*  EDPOTran.Ship-address[2]                           */
/*  EDPOTran.Ship-address[3]                           */
/*  EDPOTran.Ship-city                                 */
/*  EDPOTran.Ship-st                                   */
/*  EDPOTran.Ship-country                              */
/*  EDPOTran.Ship-zip                                  */
/*  EDPOTran.routing[1]                                */
/*  EDPOTran.routing[2]                                */
/*  EDPOTran.routing[3]                                */
/*  EDPOTran.routing[4]                                */
/*  EDPOTran.ship-method-code                          */
/*  EDPOTran.ship-pay-code                             */
/*  EDPOTran.ship-loc-code                             */
/*  EDPOTran.terms                                     */
/*  EDPOTran.terms-desc[1]                             */
/*  EDPOTran.terms-desc[2]                             */
/*  EDPOTran.curr-buyer                                */
/*  EDPOTran.curr-rate-buyer                           */
/*  EDPOTran.curr-seller                               */
/*  EDPOTran.curr-rate-seller                          */
/*  EDPOTran.misc-date1-code                           */
/*  EDPOTran.misc-date1                                */
/*  EDPOTran.scheduled-code1                           */
/*  EDPOTran.scheduled-code2                           */
/*  EDPOTran.scheduled-code3                           */
/*  EDPOTran.special-svc-code                          */
/*  EDPOTran.ref2-code                                 */
/*  EDPOTran.ref2                                      */
/*  EDPOTran.ref3-code                                 */
/*  EDPOTran.ref3                                      */
/*  EDPOTran.tin                                       */
/*  EDPOTran.tin-code                                  */
/*  EDPOTran.tin-loc                                   */
/*  EDPOTran.bt-code                                   */
/*  EDPOTran.re-code                                   */
/*  EDPOTran.sf-code                                   */
/*  EDPOTran.sn-code                                   */
/*  EDPOTran.vn-code                                   */
/*                                                     */

   CREATE EDDoc.
   ASSIGN EDDoc.Partner = EDPOTran.Partner
          EDDoc.Seq = EDPOTran.Seq.

   PUT STREAM stEDI RECID(edpotran) SKIP.
             
/*  /* edpoline */                                     */
   FOR /*EACH lineitem,*/
       EACH orderline NO-LOCK:
 
     CREATE edpoline.
     ASSIGN edpoline.partner = edpotran.partner
              edpoline.seq = edpotran.seq
              EDPOTran.Last-line = EDPOTran.Last-line + 1
              EDPOTran.Lines = EDPOTran.Lines + 1
              edpoline.line = edpotran.last-line
              edpoline.item-no = OrderLine.VendorPartNumber
            edpoline.cust-item-no = OrderLine.BuyerPartNumber
            edpoline.cust-po-line = STRING(edpoline.line,"99")
            edpoline.qty-orig-ord = OrderLine.OrderQty
            edpoline.by-code = edpotran.by-code            
            edpoline.st-code = edpotran.st-code
            edpoline.sf-code = edpotran.sf-code
            edpoline.uom-code = OrderLine.OrderQtyUOM
            edpoline.unit-price = OrderLine.PurchasePrice
            edpotran.order-amount = edpotran.order-amount 
                + round(edpoline.qty-orig-ord * edpoline.unit-price, 2)
     edpoline.uom-code = OrderLine.OrderQtyUOM 
            .
/*  EDPOLine.cust-po-line                              */
/*  EDPOLine.cust-item-no                              */
/*  EDPOLine.Item-no                                   */
/*  EDPOLine.Qty-orig-ord                              */
/*  EDPOLine.Uom-code                                  */
/*  EDPOLine.unit-price                                */
/*  EDPOLine.Description[1]                            */
/*  EDPOLine.Description[2]                            */
/*  EDPOLine.by-code                                   */
/*  EDPOLine.st-code                                   */
/*  EDPOLine.sf-code                                   */
/*  EDPOLine.UPC                                       */
/*  EDPOLine.price-basis                               */
/*  EDPOLine.pack-size                                 */
/*  EDPOLine.special-svc-code                          */
/*  EDPOLine.taxable                                   */
/*  EDPOLine.bo-flag                                   */
/*  EDPOLine.qty-change                                */
/*  EDPOLine.selling-price                             */
/*  /*                                                 */
/*  EDPOLine.product-type                              */
/*  EDPOLine.color-desc                                */
/*  EDPOLine.size-desc                                 */
/*  EDPOLine.size-qual[1]                              */
/*  EDPOLine.size-qual[2]                              */
/*  EDPOLine.size-qual[3]                              */
/*  EDPOLine.dimension[1]                              */
/*  EDPOLine.dimension[2]                              */
/*  EDPOLine.dimension[3]                              */
/*  */                                                 */
/*                                                     */
  
  END. /* orderLine */  

END.

END PROCEDURE.



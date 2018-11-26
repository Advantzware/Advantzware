{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}
{sys/inc/var.i "new shared"}

ASSIGN
    cocode = g_company
    locode = g_loc.
DEFINE VARIABLE iPayLoadNum AS INTEGER NO-UNDO.
DEFINE VARIABLE cFromIdentity AS CHARACTER NO-UNDO.
DEFINE VARIABLE cToIdentity   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAddressType  AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPoNumber     AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCurrentOrdLine AS INTEGER NO-UNDO.
DEFINE VARIABLE iNextOrderShipment AS INTEGER NO-UNDO
.
{cxml\cxmldefs.i}
define input parameter table for ttNodes.
define input-output parameter table for ttOrdHead.
define input-output parameter table for ttOrdLines.
define input-output parameter table for ttOrdSchedShipments.
/*
DEFINE TEMP-TABLE ttOrdHead NO-UNDO
    FIELD ttpayLoadID AS CHARACTER 
    FIELD ttfromIdentity AS CHARACTER 
    FIELD tttoIdentity AS CHARACTER 
    FIELD ttsenderIdentity AS CHARACTER 
    FIELD ttorderDate AS CHARACTER 
    FIELD ttorderID AS CHARACTER 
    FIELD tttotalMoney AS CHARACTER 
    FIELD ttpaymentPCard AS CHARACTER 
    FIELD ttpaymentExpiration AS CHARACTER 
    FIELD ttshipToID AS CHARACTER 
    FIELD ttshipToName AS CHARACTER 
    FIELD ttshipToAddress1 AS CHARACTER 
    FIELD ttshipToAddress2 AS CHARACTER 
    FIELD ttshipToCity AS CHARACTER 
    FIELD ttshipToState AS CHARACTER 
    FIELD ttshipToZip AS CHARACTER 
    FIELD ttshipToContact AS CHARACTER 
    FIELD ttshiptoCountry AS CHARACTER 
    FIELD ttshipToEmail AS CHARACTER 
    FIELD ttshipToPhone AS CHARACTER 
    FIELD ttshipToAreaCode AS CHARACTER 
    FIELD ttbillToID AS CHARACTER 
    FIELD ttbillToName AS CHARACTER 
    FIELD ttbillToAddress1 AS CHARACTER 
    FIELD ttbillToAddress2 AS CHARACTER 
    FIELD ttbillToCity AS CHARACTER 
    FIELD ttbillToState AS CHARACTER 
    FIELD ttbillToZip AS CHARACTER 
    FIELD ttcustNo AS CHARACTER 
 .
 DEFINE TEMP-TABLE ttOrdLines NO-UNDO
    FIELD ttpayLoadID AS CHARACTER
    FIELD ttitemLineNumber AS CHARACTER 
    FIELD ttitemQuantity AS CHARACTER 
    FIELD ttitemSupplierPartID AS CHARACTER 
    FIELD ttitemManufacturerPartID AS CHARACTER 
    FIELD ttitemSupplierPartAuxiliaryID AS CHARACTER 
    FIELD ttitemMoney AS CHARACTER 
    FIELD ttitemDescription AS CHARACTER 
    FIELD ttitemUnitOfMeasure AS CHARACTER 
    FIELD ttItemDueDate AS CHARACTER
.
DEFINE TEMP-TABLE ttOrdSchedShipments NO-UNDO
    FIELD ttpayLoadID AS CHARACTER
    FIELD ttitemLineNumber AS CHARACTER
    FIELD ttOrdShipmentNumber AS CHARACTER
    FIELD ttShipTo AS CHARACTER
    FIELD ttQty    AS CHARACTER.
    */
/* run xmloutput/xmlparser.p (input  "C:\Users\brad\Downloads\Sample850_000000082.xml"). */
for each ttNodes.
   /* disp ttNodes. */ 
  
  /* Values that apply to muliple data elements */
  IF NodeName EQ "ISA06" THEN 
    cFromIdentity = ttNodes.nodeValue.
  IF NodeName EQ "ISA08" THEN 
    cToIdentity = ttNodes.nodeValue.
  IF ttNodes.nodeName EQ "N101" THEN
    cAddressType = ttNodes.nodeValue.  
    
  IF NodeName EQ "BEG03" THEN DO:

    iPayLoadNum  = iPayLoadNum  + 1.
    cPoNumber = ttNodes.nodeValue.
    CREATE ttOrdHead.
    ASSIGN ttOrdHead.ttpayLoadID = STRING(iPayLoadNum)
           ttOrdHead.ttfromIdentity = cFromIdentity
           ttOrdHead.ttToIdentity = cToIdentity 
           .       
      ttOrdHead.ttCustNo = getCustNo(cFromIdentity).
      
  END.
    
  IF ttNodes.nodeName EQ "PO101" THEN DO:
    iCurrentOrdLine = INTEGER(ttNodes.nodeValue).
    CREATE ttOrdLines.
    ASSIGN ttOrdLines.ttpayLoadID      = STRING(iPayLoadNum)
           ttOrdLines.ttitemLineNumber = STRING(iCurrentOrdLine)
    .
  END.
    
  IF ttNodes.nodeName EQ "SCH01" THEN DO:
    iNextOrderShipment = iNextOrderShipment + 1.
    CREATE ttOrdSchedShipments.
    ASSIGN ttOrdSchedShipments.ttPayLoadID = STRING(iPayLoadNum)
           ttOrdSchedShipments.ttItemLineNumber = STRING(iCurrentOrdLine)
           ttOrdSchedShipments.ttOrdShipmentNumber = STRING(iNextOrderShipment)
           ttOrdSchedShipments.ttShipTo = ""
           ttOrdSchedShipments.ttQty = ""
           .
  END.
  
  IF ttNodes.nodeName BEGINS "BEG" THEN DO:
    FIND FIRST ttOrdHead WHERE ttOrdHead.ttPayLoadID EQ STRING(iPayLoadNum)
      NO-ERROR.
    IF NOT AVAIL ttOrdHead THEN do:
      next.
     end.
    
    CASE ttNodes.nodeName:
      WHEN "BEG03" THEN 
        ttOrdHead.ttorderID = ttNodes.nodeValue.      
    END CASE.
  END.
  
  IF ttNodes.nodeName BEGINS "N1" 
     OR ttNodes.nodeName BEGINS "N3"
     OR ttNodes.nodeName BEGINS "N4" THEN DO:
    FIND FIRST ttOrdHead WHERE ttOrdHead.ttPayLoadID EQ STRING(iPayLoadNum)
      NO-ERROR.
/*     message "caddr" caddresstype skip
        "avail" avail(ttOrdHead) skip
        "value" ttNodes.nodeValue skip
        ttNodes.nodeName
        view-as alert-box. */
    IF NOT AVAIL ttOrdHead THEN 
      NEXT.
    IF NOT cAddressType EQ "ST" THEN 
      NEXT.
    CASE ttNodes.nodeName:
      WHEN "N102" THEN 
        ttOrdHead.ttshipToID = ttNodes.nodeValue.      
      /*WHEN "N103" THEN 
        ttOrdHead.ttorderID = ttNodes.nodeValue.      
      WHEN "N104" THEN 
        ttOrdHead.ttorderID = ttNodes.nodeValue.       */
      WHEN "N301" THEN 
        ttOrdHead.ttShipToAddress1 = ttNodes.nodeValue.      
      WHEN "N302" THEN 
        ttOrdHead.ttShipToAddress1 = ttNodes.nodeValue.      
      WHEN "N401" THEN 
        ttOrdHead.ttshipToCity  = ttNodes.nodeValue.      
      WHEN "N402" THEN 
        ttOrdHead.ttshipToState = ttNodes.nodeValue.      
      WHEN "N403" THEN 
        ttOrdHead.ttshipToZip = ttNodes.nodeValue.      
      WHEN "N404" THEN 
        ttOrdHead.ttshiptoCountry = ttNodes.nodeValue.      

    END CASE.
  END. /* begins "N1" */
  
  IF ttNodes.nodeName BEGINS "PO1" 
     OR ttNodes.nodeName BEGINS "PID" 
     OR ttNodes.nodeName BEGINS "SCH" THEN DO:
         
    FIND FIRST ttOrdLines
      WHERE ttOrdLines.ttpayLoadID EQ STRING(iPayLoadNum) NO-ERROR.
    IF NOT AVAILABLE ttOrdLines THEN 
       NEXT.         
    CASE ttNodes.nodeName:
      WHEN "PO102" THEN 
        ttOrdLines.ttitemQuantity = ttNodes.nodeValue.      
      WHEN "PO103" THEN 
        ttOrdLines.ttitemUnitOfMeasure = ttNodes.nodeValue.      
      WHEN "PO104" THEN 
        ttOrdLines.ttitemMoney = ttNodes.nodeValue.      
      WHEN "PO109" THEN 
        ttOrdLines.ttitemSupplierPartID  = ttNodes.nodeValue.      
      WHEN "PO107" THEN 
        ttOrdLines.ttitemManufacturerPartID = ttNodes.nodeValue.      
      WHEN "PID05" THEN 
            ttOrdLines.ttitemDescription  = ttNodes.nodeValue.         
      WHEN "SCH06" THEN 
        ttOrdLines.ttItemDueDate = ttNodes.nodeValue.      
    END CASE.
  END. /* begins "N1" */  
  
END.
/*
FOR EACH ttOrdHead.
 disp ttOrdHead with 2 col.
 end.
 */
/*
payLoadID               = ttpayLoadID           
fromIdentity            = ttfromIdentity         
toIdentity              = tttoIdentity           
senderIdentity          = ttsenderIdentity       
orderDate               = ttorderDate            
orderID                 = ttorderID              
totalMoney              = tttotalMoney           
paymentPCard            = ttpaymentPCard         
paymentExpiration       = ttpaymentExpiration    
shipToID                = ttshipToID             
shipToName              = ttshipToName           
shipToAddress1          = ttshipToAddress1       
shipToAddress2          = ttshipToAddress2       
shipToCity              = ttshipToCity           
shipToState             = ttshipToState          
shipToZip               = ttshipToZip            
shipToContact           = ttshipToContact        
shiptoCountry           = ttshiptoCountry        
shipToEmail             = ttshipToEmail          
shipToPhone             = ttshipToPhone          
shipToAreaCode          = ttshipToAreaCode       
billToID                = ttbillToID             
billToName              = ttbillToName           
billToAddress1          = ttbillToAddress1       
billToAddress2          = ttbillToAddress2       
billToCity              = ttbillToCity           
billToState             = ttbillToState          
billToZip               = ttbillToZip            
custNo                  = ttcustNo               
 .
FOR EACH ttOrdLines :

  itemLineNumber                = ttitemLineNumber              
  itemQuantity                  = ttitemQuantity                 
  itemSupplierPartID            = ttitemSupplierPartID           
  itemManufacturerPartID        = ttitemManufacturerPartID       
  itemSupplierPartAuxiliaryID   = ttitemSupplierPartAuxiliaryID  
  itemMoney                     = ttitemMoney                    
  itemDescription               = ttitemDescription              
  itemUnitOfMeasure             = ttitemUnitOfMeasure            
  ItemDueDate                   = ttItemDueDate                 
*/
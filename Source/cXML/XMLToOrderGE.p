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
DEFINE VARIABLE iNextOrderShipment AS INTEGER NO-UNDO.
DEFINE VARIABLE cDocType      AS CHARACTER NO-UNDO.
{cxml\cxmldefs.i}
DEFINE INPUT PARAMETER table FOR ttNodes.
DEFINE INPUT-OUTPUT PARAMETER table FOR ttOrdHead.
DEFINE INPUT-OUTPUT PARAMETER table FOR ttOrdLines.
DEFINE INPUT-OUTPUT PARAMETER table FOR ttOrdSchedShipments.
FIND FIRST ttNodes NO-ERROR.

FOR EACH ttNodes:
  
  /* Values that apply to muliple data elements */
  IF NodeName EQ "ISA06" THEN 
    cFromIdentity = ttNodes.nodeValue.
  IF NodeName EQ "ISA08" THEN 
    cToIdentity = ttNodes.nodeValue.
  IF ttNodes.nodeName EQ "N101" THEN
    cAddressType = ttNodes.nodeValue.  

  /* 850 or 860 */
  IF ttNodes.nodeName EQ "ST01" AND ttNodes.nodeValue GT "" THEN 
  DO:
      iPayLoadNum  = iPayLoadNum  + 1.

      CREATE ttOrdHead.
      ASSIGN 
          ttOrdHead.ttpayLoadID    = STRING(iPayLoadNum)
          ttOrdHead.ttfromIdentity = cFromIdentity
          ttOrdHead.ttToIdentity   = cToIdentity 
          .   
   
      ASSIGN 
          cDocType            = ttNodes.nodeValue
          ttOrdHead.ttDocType = cDocType .
  END.    
    
  IF NodeName EQ "BEG03" OR NodeName EQ "BCH03" THEN DO:

      FIND FIRST ttOrdHead WHERE ttOrdHead.ttPayLoadID EQ STRING(iPayLoadNum)
          NO-ERROR.    
    cPoNumber = ttNodes.nodeValue.
    
      ttOrdHead.ttCustNo = getCustNo(cFromIdentity, "").
      ttOrdHead.ttorderID = cPoNumber.
 
  END.
  
  IF ttNodes.nodeName EQ "PO101" OR ttnodes.nodeName EQ "POC01" THEN DO:
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
  
  IF ttNodes.nodeName BEGINS "BEG" OR ttNodes.nodeName BEGINS "BCH" THEN DO:
    FIND FIRST ttOrdHead WHERE ttOrdHead.ttPayLoadID EQ STRING(iPayLoadNum)
      NO-ERROR.
    IF NOT AVAIL ttOrdHead THEN DO:
      NEXT.
     END.
    
    CASE ttNodes.nodeName:
      WHEN "BEG03" OR WHEN "BCH03" THEN 
        ttOrdHead.ttorderID = ttNodes.nodeValue.      
      WHEN "BEG04" OR WHEN "BCH04" THEN 
            ttOrdHead.ttRelease = ttNodes.nodeValue.          
      WHEN "BEG01" OR WHEN "BCH02" THEN 
            ttOrdHead.setPurpose = ttNodes.nodeValue.         
    END CASE.
  END.
  
  IF ttNodes.nodeName BEGINS "N1" 
     OR ttNodes.nodeName BEGINS "N3"
     OR ttNodes.nodeName BEGINS "N4" THEN DO:
         
    FIND FIRST ttOrdHead WHERE ttOrdHead.ttPayLoadID EQ STRING(iPayLoadNum)
      NO-ERROR.
    IF NOT AVAIL ttOrdHead THEN 
      NEXT.
    IF NOT cAddressType EQ "ST" OR ttNodes.nodeValue EQ "" THEN 
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
     OR ttNodes.nodeName BEGINS "SCH" 
     OR ttNodes.nodeName BEGINS "POC" THEN DO:
         
    FIND FIRST ttOrdLines
      WHERE ttOrdLines.ttpayLoadID EQ STRING(iPayLoadNum) 
        AND ttOrdLines.ttitemLineNumber EQ STRING(iCurrentOrdLine) NO-ERROR.
    IF NOT AVAILABLE ttOrdLines THEN 
       NEXT.         
    CASE ttNodes.nodeName:
      WHEN "PO102" OR WHEN "POC03" THEN 
        ttOrdLines.ttitemQuantity = ttNodes.nodeValue.      
      WHEN "PO103" OR WHEN "POC0501" THEN 
        ttOrdLines.ttitemUnitOfMeasure = ttNodes.nodeValue.      
      WHEN "PO104" OR WHEN "POC06" THEN 
        ttOrdLines.ttitemMoney = ttNodes.nodeValue.      
/*      WHEN "PO109" OR WHEN "POC09" THEN                      */
/*        ttOrdLines.ttitemSupplierPartID  = ttNodes.nodeValue.*/
      WHEN "PO107" OR WHEN "POC07" THEN DO:
        ttOrdLines.ttitemSupplierPartID = ttNodes.nodeValue.
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
            AND itemfg.part-no EQ ttOrdLines.ttitemSupplierPartID 
            NO-ERROR.
        IF AVAILABLE itemfg THEN 
          ttOrdLines.ttitemManufacturerPartID = itemfg.i-no.
      END.
      WHEN "PID05" THEN 
            ttOrdLines.ttitemDescription  = ttNodes.nodeValue.         
      WHEN "SCH06" THEN 
        ttOrdLines.ttItemDueDate = ttNodes.nodeValue.      
    END CASE.
  END. /* begins "N1" */  
  
END.
RUN cxml\ttToEdiTab.p (INPUT TABLE ttNodes, INPUT-OUTPUT TABLE ttOrdHead , INPUT-OUTPUT TABLE ttOrdLines, INPUT-OUTPUT TABLE ttOrdSchedShipments).
/*------------------------------------------------------------------------
    File        : cXML\gencXMLOrder.p
    Purpose     : To create cXML based purchase orders

    Syntax      :

    Description : To create cXML based purchase orders

    Author(s)   : Vishnu Vellanki
    Created     : Tue July 26 07:33:22 EDT 2019
    
    Notes       : *** This is modelled after internal procedure (gencXMLOrder)
                  from cXML\cXMLOrderProc.i ***
  ----------------------------------------------------------------------*/

&SCOPED-DEFINE NEW NEW
{methods/defines/globdefs.i}
{methods/defines/hndldefs.i}

DEFINE VARIABLE hSession AS HANDLE NO-UNDO.
DEFINE VARIABLE hTags    AS HANDLE NO-UNDO.

DEFINE BUFFER bf-shipto FOR shipto.

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).

{sys/inc/var.i "new shared"}

 {cXML/cXMLDefs.i}  
 {cXML/cXMLOrderProc.i}

  DEFINE INPUT  PARAMETER ipcXMLData        AS LONGCHAR  NO-UNDO.
  DEFINE INPUT  PARAMETER lpcTempTableOnly  AS LOGICAL   NO-UNDO.

  /* This program gets company code and location code which are passed 
     as input parameters from request handler. Currently both are being 
     passed as blank from the request handler. The request handler will
     pass company code and customer number once session manager related
     work is completed */
  DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcWarehouseID    AS CHARACTER NO-UNDO.
  
  DEFINE OUTPUT PARAMETER opcPayloadID         AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcOrderID           AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplSuccess           AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcReturnValue       AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcInternalException AS CHARACTER NO-UNDO.

  DEFINE VARIABLE payLoadID        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fromIdentity     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE orderDate        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE custNo           AS CHARACTER NO-UNDO.
  DEFINE VARIABLE shipToID         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
  DEFINE VARIABLE cShipToTaxCode   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE rOrdRec          AS ROWID     NO-UNDO.
  DEFINE VARIABLE cShipToID        AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cPartnerItem     AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReturn          AS CHARACTER NO-UNDO.  
  DEFINE VARIABLE cDueDate         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lIsEdiXML        AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE hOrderProcs      AS HANDLE    NO-UNDO.
  DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lOEAutoApproval  AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cResult          AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE orderID          AS CHARACTER NO-UNDO.
  
  DEFINE VARIABLE iNextOrderNumber LIKE oe-ord.ord-no  NO-UNDO.
  DEFINE VARIABLE iNextShipNo      LIKE shipto.ship-no NO-UNDO.
  DEFINE VARIABLE dItemQtyEach     LIKE oe-ordl.qty    NO-UNDO.
 
  oplSuccess = YES.
  
  RUN oe/OrderProcs.p PERSISTENT SET hOrderProcs.

  RUN XMLOutput/APIXMLParser.p (
      INPUT ipcXMLData
      ) NO-ERROR.
  
  IF ERROR-STATUS:ERROR OR NOT TEMP-TABLE ttNodes:HAS-RECORDS THEN DO:
      ASSIGN
          opcInternalException = RETURN-VALUE
          opcReturnValue       = "Requested XML is not in valid format"
          oplSuccess           = NO
          .
          
      RETURN.  
  END.
  
  FIND FIRST ttNodes NO-LOCK
        WHERE ttNodes.nodeName BEGINS "ISA" 
        NO-ERROR. 

  lIsEdiXml = (IF AVAILABLE ttNodes THEN YES ELSE NO).
  ASSIGN
      payLoadID    = getNodeValue('cXML','payloadID')
      opcPayloadID = payLoadID.

  IF NOT lIsEdiXML THEN DO:
 
      ASSIGN
          orderID    = getNodeValue('OrderRequestHeader','orderID')
          opcOrderID = orderID
          .            
      /* Validation of orderID */
      IF orderID EQ "" THEN DO:
          ASSIGN
              oplSuccess     = NO
              opcReturnValue = 'OrderID is empty'
              .
              
          RETURN.
      END.
      
      ASSIGN
          payLoadID    = getNodeValue('cXML','payloadID')
          opcPayloadID = payLoadID
          .
      /* Validation of PayloadID */
      IF payLoadID EQ "" THEN DO:
          ASSIGN
              oplSuccess     = NO
              opcReturnValue = 'PayloadID is empty : PO # ' + orderID 
              .
              
          RETURN.
      END. 
      
      fromIdentity = getNodeValue('From','Identity').
      /* Validation of identity present in from tag */
      IF fromIdentity EQ "" THEN DO:
          ASSIGN
              oplSuccess     = NO
              opcReturnValue = 'FromIdentity is empty : PO # ' + orderID
              .
              
          RETURN.
      END. 
      
      orderDate = getNodeValue('OrderRequestHeader','orderDate').
      /* Validation of orderDate */
      IF orderDate EQ "" THEN DO:
          ASSIGN
              oplSuccess     = NO
              opcReturnValue = 'orderDate is empty : PO # ' + orderID
              .
              
          RETURN.
      END. 
      
      shipToID = getNodeValue('shipTo','AddressID').
      /* Validation of AddressID */    
      IF shipToID EQ "" THEN DO:
          ASSIGN
              opcReturnValue = "AddressID is empty : PO # " + orderID
              oplSuccess     = NO
              .
              
          RETURN. 
      END.
      
      /* This procedure validates company code,shipToID and location code, 
         and returns valid company code,location code,shipToID and customer number.
         and additionally it returns the shipto table buffer to access any other data 
         from shipto table */ 
      RUN cXML/getCustDetails.p (
          INPUT        fromIdentity,
          INPUT-OUTPUT shipToID,
          INPUT-OUTPUT ipcCompany,
          INPUT-OUTPUT ipcWarehouseID,
          INPUT        OrderID,
          OUTPUT       custno,
          OUTPUT       oplSuccess,
          OUTPUT       opcReturnValue,
          BUFFER       bf-shipto
          ).

       IF NOT oplSuccess THEN
           RETURN.   
      /* This assignment is required to populate cocode and locode variables 
         with company code and location code since as these variables are being 
         used in cXMLOrderProc.i procedures(genOrderHeader,touchOrder,assignOrderHeader
         and genTempOrderHeader) */
      ASSIGN
          cocode = ipcCompany
          locode = ipcWarehouseID
          .
      
      FIND FIRST oe-ord NO-LOCK
           WHERE oe-ord.company     EQ ipcCompany
             AND oe-ord.cust-no      EQ custNo
             AND oe-ord.po-no        EQ orderID
             AND oe-ord.spare-char-3 EQ payLoadID
           NO-ERROR.
      IF AVAILABLE oe-ord AND orderID GT "" THEN DO:
          ASSIGN
              oplSuccess     = NO
              opcReturnValue = 'Order already exists with PO#: ' + orderID + ', Payload ID: ' + payloadID
              .
              
          RETURN.
      END.
     
      FIND FIRST ttNodes NO-LOCK 
             WHERE ttNodes.parentName EQ 'itemDetail' 
               AND ttNodes.nodeName EQ 'ManufacturerPartID'
             NO-ERROR.
      IF NOT AVAILABLE ttNodes THEN 
          FIND FIRST ttNodes NO-LOCK 
              WHERE (ttNodes.parentName EQ 'itemDetail'
                      OR ttNodes.parentName EQ 'itemID'
                    ) 
              AND ttNodes.nodeName EQ 'SupplierPartID'
              NO-ERROR.

      IF NOT AVAILABLE ttNodes THEN DO:
          ASSIGN
              oplSuccess = NO
              opcReturnValue = 'Part Number is missing from XML file  : PO # ' + orderID
              .
              
          RETURN.
      END. 
      
      RUN genTempOrderHeader (
          OUTPUT cReturn
          ).
      
      /* Procedure genTempOrderHeader updates the value of ttOrdHead.ttcustNo from the return
         value of function getCustNo, which is not useful for the case when "cXMLorder"
         sys control is not available, so the value of ttOrdHead.ttcustNo is reassigned with
         custno which is output from procedure getCustDetails.p */

      IF AVAILABLE ttOrdHead THEN
          ASSIGN  
              ttOrdHead.ttcustNo   = custno
              ttOrdHead.ttshipToID = shipToID
              .

      RUN genTempOrderLinesLocal (
          INPUT rOrdRec, 
          OUTPUT cReturn
          ).  

  END.  
  ELSE DO:
      ASSIGN 
          payLoadID = "1"
          opcPayloadID = payLoadID
          .
      FIND FIRST ttNodes WHERE  
         ttNodes.nodeName EQ "ISA06" NO-ERROR.
      IF AVAILABLE ttNodes THEN 
          fromIdentity = ttNodes.nodeValue.
      /* fromIdentity = getNodeValue('ISA','ISA06'). */
      orderDate    = getNodeValue('BEG','BEG05'). 

      ASSIGN 
          orderdate = SUBSTRING(orderDate, 1, 4) + "-" + substring(orderDate, 5, 2) + "-" + substring(orderDate, 7, 2) /* "2018 11 05" */
          NO-ERROR.
      
      /* This procedure validates company code,shipToID and location code, 
         and returns valid company code,location code,shipToID and customer number.
         and additionally it returns the shipto table buffer to access any other 
         data from shipto */    
      RUN cXML/getCustDetails.p (
          INPUT        fromIdentity,
          INPUT-OUTPUT shipToID,
          INPUT-OUTPUT ipcCompany,
          INPUT-OUTPUT ipcWarehouseID,
          INPUT        OrderID,
          OUTPUT       custno,
          OUTPUT       oplSuccess,
          OUTPUT       opcReturnValue,
          BUFFER       bf-shipto
          ).
      
      /* This assignment is required to populate cocode and locode variables 
         with company code and location code since as these variables are being 
         used in cXMLOrderProc.i procedures(genOrderHeader,touchOrder,assignOrderHeader
         and genTempOrderHeader) */
      ASSIGN
          cocode = ipcCompany
          locode = ipcWarehouseID
          .
      IF payLoadID EQ "" OR
         fromIdentity EQ "" OR
         orderDate EQ ? THEN DO:
          ASSIGN
              opcReturnValue = "Requested XML is not in valid format : PO # " + orderID
              oplSuccess     = NO
              .
          RETURN. 
      END.
                
      FOR EACH ttNodes NO-LOCK 
        WHERE ttNodes.nodeName EQ "PO107":
            cPartnerItem = ttNodes.nodeValue.
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ ipcCompany
                  AND itemfg.part-no EQ cPartnerItem
                NO-ERROR.

            IF NOT AVAILABLE itemfg THEN DO:
                ASSIGN
                    oplSuccess     = NO
                    opcReturnValue = 'Part Number is missing from XML file : PO # ' + orderID 
                    .
                    
                RETURN.                
            END.
      END.
      RUN cxml\xmltoOrderGE.p (
          INPUT TABLE ttNodes, 
          INPUT-OUTPUT TABLE ttOrdHead , 
          INPUT-OUTPUT TABLE ttOrdLines, 
          INPUT-OUTPUT TABLE ttOrdSchedShipments
          ).
  END.
  
  IF lpcTempTableOnly THEN 
      RETURN.
  
  /* Order creation does not happen in case of any failure */ 
  EACH-ORDER:    
  DO TRANSACTION ON ERROR UNDO EACH-ORDER, LEAVE: 
      FOR EACH ttOrdHead NO-LOCK  
          WHERE (ttOrdHead.ttDocType EQ "PO" OR ttOrdHead.ttDocType EQ "850") :
    
          ttOrdHead.ttSelectedOrder = TRUE.
        
          iNextOrderNumber = GetNextOrder#().
    
          RUN genOrderHeader (
              INPUT iNextOrderNumber, 
              INPUT orderDate, 
              OUTPUT rOrdRec
              ).
              
          IF NOT lIsEdiXML THEN DO:
    
          END.
          
          RUN assignOrderHeader (
              INPUT rOrdRec, 
              OUTPUT cShipToID,
              OUTPUT cReturn
              ).
              
          RUN genOrderLinesLocal (
              INPUT rOrdRec, 
              INPUT cShipToID, 
              OUTPUT cReturn,
              OUTPUT oplSuccess
              ).
              
          opcReturnValue = cReturn.
          
          RUN touchOrder (
              INPUT  rOrdRec, 
              OUTPUT cReturn
              ).
     
          ASSIGN
              ttOrdHead.ttSelectedOrder = FALSE 
              ttOrdHead.ttProcessed     = TRUE
              .
          
      /* Determine autoapproval for this customer/shipto */
      lOEAutoApproval = NO.
      RUN sys/ref/nk1look.p (
          INPUT  ipcCompany, 
          INPUT  "OEAutoApproval", 
          INPUT  "L", 
          INPUT  YES /* use shipto */,
          INPUT  YES /* use cust*/, 
          INPUT  custno, 
          INPUT  ShipToID, 
          OUTPUT cResult, 
          OUTPUT lFound
          ).
      
      IF lFound THEN
          lOEAutoApproval = LOGICAL(cResult) NO-ERROR.
      
      /* 52995 DSG Automated Ship To Creation */
      IF lOeAutoApproval THEN 
          RUN ProcessImportedOrder IN hOrderProcs (
              INPUT  rOrdRec, 
              OUTPUT lError, 
              OUTPUT cMessage
              ).
              
         IF NOT oplSuccess THEN
            UNDO EACH-ORDER, LEAVE.
          
      END.
 
  END.
        
  IF NOT oplSuccess THEN
     RETURN.
     
  RELEASE oe-ord.  
  RELEASE reftable.
  RELEASE oe-ord-whs-order.
  RELEASE oe-ordl-whs-item.
      
  ASSIGN
      oplSuccess     = TRUE
      opcReturnValue = 'Successfully Generated Order : PO # ' + orderID 
      .

/* Procedure genTempOrderLinesLocal is modelled after internal procedure (genTempOrderLines)
   defined in cXML\cXMLOrderProc.i. The logic in the existing procedure genTempOrderLines
   validates supplierid against itemfg table and empties temp tables ttOrdLines and ttOrdHead
   if the validation fails.
   procedure genTempOrderLinesLocal is exact copy of existing internal procedure (genTempOrderLines)
   without the validation on supplierid */


/* **********************  Internal Procedures  *********************** */


PROCEDURE genTempOrderLinesLocal:
    DEFINE INPUT  PARAMETER iprOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE itemLineNumber              AS CHARACTER NO-UNDO. 
    DEFINE VARIABLE itemSupplierPartID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRequestedDeliveryDate      AS CHARACTER NO-UNDO.

    DEFINE VARIABLE dRequestedDeliveryDate      AS DATE      NO-UNDO.    
    DEFINE VARIABLE iCurrentLineNum             AS INTEGER NO-UNDO.
    DEFINE VARIABLE cNodeParentName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lSuccess AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cFailedReason AS CHARACTER NO-UNDO.
    
    FIND oe-ord WHERE ROWID(oe-ord) EQ iprOeOrd NO-LOCK NO-ERROR.

    lSuccess = TRUE. 
    EMPTY TEMP-TABLE ttOrdLines.
    FOR EACH ttNodes:
        IF AVAILABLE oe-ord THEN 
          ASSIGN dRequestedDeliveryDate = oe-ord.due-date
                 cRequestedDeliveryDate = ""
                 .
                                  
        IF ttNodes.parentName EQ 'itemOut' AND ttNodes.nodeName EQ 'lineNumber' THEN DO:
            FIND FIRST ttOrdHead NO-ERROR.
            IF NOT AVAILABLE ttOrdHead THEN 
                RETURN.
            itemLineNumber = TRIM(ttNodes.nodeValue).
            FIND FIRST ttOrdLines
                WHERE ttOrdLines.ttpayLoadID                   EQ ttOrdHead.ttpayLoadID
                  AND ttOrdLines.ttItemLineNumber              EQ itemLineNumber
                NO-ERROR.
            IF NOT AVAILABLE ttOrdLines THEN DO:
                CREATE ttOrdLines.
                ASSIGN                           
                    ttOrdLines.ttpayLoadID      = ttOrdHead.ttpayLoadID
                    ttOrdLines.ttItemLineNumber = itemLineNumber 
                    .
            END.      
            NEXT.
        END.
        
        cNodeParentName = ttNodes.parentName + "|" + ttNodes.nodeName.
        CASE cNodeParentName:
            WHEN 'itemOut|requestedDeliveryDate' THEN DO:
                cRequestedDeliveryDate = TRIM(ttNodes.nodeValue).

                IF cRequestedDeliveryDate NE "" THEN
                    ASSIGN dRequestedDeliveryDate = DATE(INT(SUBSTR(cRequestedDeliveryDate,6,2))
                             ,INT(SUBSTR(cRequestedDeliveryDate,9,2))
                             ,INT(SUBSTR(cRequestedDeliveryDate,1,4)))
                           ttOrdLines.ttItemDueDate = cRequestedDeliveryDate
                           .
                                
            END.
            WHEN 'itemOut|quantity' THEN
                ttOrdLines.ttItemQuantity = TRIM(ttNodes.nodeValue).
/*            WHEN 'itemID|supplierPartID' THEN                             */
/*                ttOrdLines.ttItemSupplierPartID = TRIM(ttNodes.nodeValue).*/
            WHEN 'itemID|supplierPartAuxiliaryID'THEN
                ttOrdLines.ttItemSupplierPartAuxiliaryID = TRIM(ttNodes.nodeValue).
            WHEN 'unitPrice|money'THEN
                ttOrdLines.ttItemMoney = TRIM(ttNodes.nodeValue).
            WHEN 'itemDetail|description' THEN
                ASSIGN
                    ttOrdLines.ttItemDescription    = TRIM(ttNodes.nodeValue)
                    ttOrdLines.ttItemSupplierPartID = SUBSTRING(ttOrdLines.ttItemDescription,1,8)
                    .
            WHEN  'itemDetail|unitOfMeasure' THEN
                ttOrdLines.ttItemUnitOfMeasure = TRIM(ttNodes.nodeValue).
            WHEN  'itemDetail|ManufacturerPartID' THEN DO:
                ttOrdLines.ttItemManufacturerPartID = TRIM(ttNodes.nodeValue).
            END.
                
        END CASE.        

    END.  
     
END PROCEDURE.

/* Procedure genOrderLinesLocal is modelled after internal procedure (genOrderLines)
   defined in cXML\cXMLOrderProc.i. The logic in the existing procedure genOrderLines
   do not assign values to oe-ordl.part-no and oe-ordl.i-no based on the validation of 
   supplierid against cust-part and itemfg tables
   procedure genOrderLinesLocal is exact copy of existing internal procedure (genOrderLines)
   with the validation on supplierid */
PROCEDURE genOrderLinesLocal:
  DEFINE INPUT  PARAMETER iprOeOrd AS ROWID NO-UNDO.
  DEFINE INPUT  PARAMETER ipcShipToID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplSuccess AS LOGICAL NO-UNDO.

  DEFINE VARIABLE itemLineNumber AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemQuantity AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemSupplierPartID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemManufacturerPartID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemSupplierPartAuxiliaryID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemMoney AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemDescription AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemUnitOfMeasure AS CHARACTER NO-UNDO.
  DEFINE VARIABLE itemDueDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE iNextOrderNumber LIKE oe-ord.ord-no NO-UNDO.
  DEFINE VARIABLE iNextShipNo LIKE shipto.ship-no NO-UNDO.
  DEFINE VARIABLE dItemQtyEach LIKE oe-ordl.qty NO-UNDO.
  DEFINE VARIABLE cShipToTaxCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRequestedDeliveryDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dRequestedDeliveryDate AS DATE NO-UNDO.
  DEFINE VARIABLE dMultiplier AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cCaseUOMList AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMTotal AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMDL AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMFO AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMVO AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMDM AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hdCostProcs AS HANDLE.
  RUN system\CostProcs.p PERSISTENT SET hdCostProcs.  
  oplSuccess = YES.
  
  FIND oe-ord WHERE ROWID(oe-ord) EQ iprOeOrd NO-LOCK NO-ERROR.
  
  RUN sys/ref/nk1look.p (
      INPUT oe-ord.company, 
      INPUT "CaseUOMList", 
      INPUT "C" /* Logical */, 
      INPUT NO /* check by cust */, 
      INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
      OUTPUT cRtnChar, OUTPUT lRecFound
      ).
  cCaseUomList = cRtnChar.  
  
  FIND FIRST cust WHERE cust.cust-no EQ oe-ord.cust-no 
    AND cust.company EQ oe-ord.company NO-LOCK NO-ERROR.
  FIND FIRST ttordlines NO-LOCK NO-ERROR.

  IF NOT AVAILABLE ttOrdHead THEN 
    FIND FIRST ttOrdHead
      WHERE ttOrdHead.ttOrderID EQ oe-ord.po-no NO-ERROR.

  FOR EACH ttOrdLines WHERE 
      ttOrdLines.ttpayLoadID = ttOrdHead.ttpayLoadID
      BY ttItemLineNumber:
     
     IF ttOrdLines.ttitemSupplierPartID EQ "" THEN DO:
        ASSIGN
            oplSuccess     = NO
            opcReturnValue = "SupplierPartID is empty for line (" + ttItemLineNumber + ") : PO #" + ttOrdHead.ttOrderID
            .

        RETURN.
     END.
     
     ASSIGN cRequestedDeliveryDate = ttOrdLines.ttItemDueDate
            dRequestedDeliveryDate = DATE(INT(SUBSTR(cRequestedDeliveryDate,6,2))
                                     ,INT(SUBSTR(cRequestedDeliveryDate,9,2))
                                     ,INT(SUBSTR(cRequestedDeliveryDate,1,4)))        
            NO-ERROR.
            
    /* This procedure takes Company code,Customer number,SupplierPartID,
       WarehouseID and ManufacturerPartID values as inputs.Then it validates 
       SupplierPartID against cust-part and itemfg tables 
       And then returns valid SupplierPartID and ManufacturerPartID*/
     RUN GetItemAndPart (
         INPUT  ipcCompany,
         INPUT  ttOrdLines.ttItemSupplierPartID,
         INPUT  ttOrdLines.ttItemManufacturerPartID,
         INPUT  oe-ord.cust-no,
         INPUT  ipcWarehouseID,
         OUTPUT itemSupplierPartID,
         OUTPUT itemManufacturerPartID,
         OUTPUT oplSuccess
         ).
        
     IF NOT oplSuccess THEN DO:
        opcReturnValue = "SupplierPartID (" + ttOrdLines.ttItemSupplierPartID + ") does not exist : PO #" + ttOrdHead.ttOrderID.

        RETURN.
     END.

     ASSIGN 
         itemLineNumber              = ttOrdLines.ttItemLineNumber              
         itemQuantity                = ttOrdLines.ttItemQuantity                 
         itemSupplierPartAuxiliaryID = ttOrdLines.ttItemSupplierPartAuxiliaryID  
         itemMoney                   = ttOrdLines.ttItemMoney                    
         itemDescription             = ttOrdLines.ttItemDescription              
         itemUnitOfMeasure           = ttOrdLines.ttItemUnitOfMeasure            
         ItemDueDate                 = ttOrdLines.ttItemDueDate   
         .

      FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ ipcCompany
              AND itemfg.i-no    EQ TRIM(itemManufacturerPartID) NO-ERROR.
      CREATE oe-ordl.
      ASSIGN
        oe-ordl.company   = oe-ord.company
        oe-ordl.ord-no    = oe-ord.ord-no
        oe-ordl.type-code = oe-ord.type
        oe-ordl.cust-no   = oe-ord.cust-no
        oe-ordl.po-no     = oe-ord.po-no
        oe-ordl.req-code  = oe-ord.due-code
        oe-ordl.req-date  = dRequestedDeliveryDate
        oe-ordl.prom-code = oe-ord.due-code
        oe-ordl.prom-date = oe-ord.due-date
        oe-ordl.ship-id   = oe-ord.ship-id
        oe-ordl.disc      = cust.disc
        oe-ordl.tax       = cust.sort EQ 'Y' AND oe-ord.tax-gr NE ''
        oe-ordl.over-pct  = oe-ord.over-pct   
        oe-ordl.under-pct = oe-ord.under-pct
        oe-ordl.line      = INT(itemLineNumber)
        oe-ordl.i-no      = itemManufacturerPartID
        oe-ordl.part-no   = TRIM(itemSupplierPartID)
        oe-ordl.qty       = DEC(itemQuantity)
        oe-ordl.pr-uom    = TRIM(itemUnitOfMeasure)
        oe-ordl.price     = DEC(itemMoney)
        oe-ordl.est-no    = oe-ord.est-no
        oe-ordl.q-qty     = oe-ord.t-fuel
        oe-ordl.whsed     = oe-ordl.est-no NE ''
        oe-ordl.q-no      = oe-ord.q-no
        oe-ordl.prom-date = oe-ord.due-date
        .

      IF oe-ordl.price EQ 0 THEN DO:                      
        FIND FIRST xoe-ord OF oe-ord NO-LOCK.
        RUN getPrice (
            INPUT ROWID(oe-ordl)
            ).
      END.
      DO iCount = 1 TO 3:
        ASSIGN
          oe-ordl.s-man[iCount]  = oe-ord.sman[iCount]
          oe-ordl.s-pct[iCount]  = oe-ord.s-pct[iCount]
          oe-ordl.s-comm[iCount] = oe-ord.s-comm[iCount]
          .
      END. /* do icount */
      
      ASSIGN
        oe-ordl.i-name     = itemfg.i-name
        oe-ordl.cases-unit = itemfg.case-pall
        oe-ordl.part-dscr1 = itemfg.part-dscr1
        oe-ordl.part-dscr2 = itemfg.part-dscr2 
        .
        
      RUN GetCostForOrderLine IN hdCostProcs(rowid(oe-ordl), OUTPUT dCostPerUOMTotal, OUTPUT dCostPerUOMDL,OUTPUT dCostPerUOMFO,
                                             OUTPUT dCostPerUOMVO,OUTPUT dCostPerUOMDM, OUTPUT cCostUOM , OUTPUT lFound) .
       oe-ordl.cost = dCostPerUOMTotal .
       oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000 .
      
      IF oe-ordl.pr-uom NE "EA" THEN 
      DO:  /*This assumes the qty uom is the same as the price uom on imported orders*/
          ASSIGN 
              oe-ordl.spare-dec-1  = oe-ordl.qty
              oe-ordl.spare-char-2 = oe-ordl.pr-uom
              oe-ordl.t-price      = oe-ordl.spare-dec-1 * oe-ordl.price
              oe-ordl.pr-uom       = (IF LOOKUP(oe-ordl.pr-uom, cCaseUOMList) GT 0 THEN "CS" ELSE oe-ordl.pr-uom)
              .
          RUN Conv_QtyToEA(oe-ordl.company, oe-ordl.i-no, oe-ordl.qty, oe-ordl.pr-uom, itemfg.case-count, OUTPUT oe-ordl.qty).
      END. /*oe-ordl.pr-uom ne "EA"*/
      ELSE 
          oe-ordl.t-price = oe-ordl.qty * oe-ordl.price.
       
      oe-ordl.cas-cnt = IF oe-ordl.qty LT itemfg.case-count THEN oe-ordl.qty ELSE itemfg.case-count.
      /* {oe/defwhsed.i oe-ordl} */
      
      IF oe-ordl.req-date EQ ? THEN 
        oe-ordl.req-date = oe-ord.ord-date + 10.

      RUN CreateRelease (
          INPUT ipcShipToID,
          INPUT ""
          ).
      RELEASE oe-ordl.
      RELEASE oe-rel.      
      RELEASE reftable.
      RELEASE oe-ord-whs-order.
      RELEASE oe-ordl-whs-item.

  END. /* for each  */
  
  RELEASE reftable.
  RELEASE oe-ord-whs-order.
  RELEASE oe-ordl-whs-item.
  FIND FIRST oe-ordl WHERE oe-ordl.company   = oe-ord.company
        AND oe-ordl.ord-no    = oe-ord.ord-no
    NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl THEN
    opcReturnValue =  'Success'.
  ELSE
    opcReturnValue = 'No Items'.
    
  IF oe-ord.due-date NE dRequestedDeliveryDate THEN DO:
      FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
      oe-ord.due-date = dRequestedDeliveryDate.
  END.
  
  RELEASE oe-ord.  
  RELEASE oe-ordl.
  DELETE OBJECT hdCostProcs.
END PROCEDURE.

PROCEDURE GetItemAndPart:
/*------------------------------------------------------------------------------
 Purpose: Gets Part number and Item number for the order
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcSupplierPartID AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcManufactureID  AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcCustNo         AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcWarehouseID    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcPartID         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcItemID         AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess        AS LOGICAL   NO-UNDO.

    oplSuccess = YES.
    
    /* Checking itemfg part number */   
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.company EQ ipcCompany
           AND itemfg.def-loc EQ ipcWarehouseID
           AND itemfg.part-no EQ ipcSupplierPartID
           AND itemfg.stat    EQ "A"
         NO-ERROR.
    IF NOT AVAILABLE itemfg THEN DO:
            oplSuccess = NO.
 
        RETURN.
    END.
    
    ASSIGN
        opcPartID = itemfg.part-no
        opcItemID = itemfg.i-no
        .

/* This has been commented out because as per the new specs 
   present in ticket #60939 this logic is no longer in use */
/* 
/* Checking customer partID */
    FIND FIRST cust-part NO-LOCK
         WHERE cust-part.company EQ ipcCompany
           AND cust-part.cust-no EQ ipcCustNo
           AND cust-part.part-no EQ ipcSupplierPartID
           NO-ERROR.      
    IF AVAILABLE cust-part THEN DO:
        ASSIGN
            opcPartID = cust-part.part-no
            opcItemID = cust-part.i-no
            .
        
        RETURN.
    END. 

/* Checking itemfg part number */   
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.part-no EQ ipcSupplierPartID
           AND itemfg.company EQ ipcCompany
         NO-ERROR.
    IF AVAILABLE itemfg THEN DO:
         ASSIGN
             opcPartID = itemfg.part-no
             opcItemID = itemfg.i-no
             .
         
         RETURN.
    END.

/* Checking itemfg if part number is not available */  
    FIND FIRST itemfg NO-LOCK 
         WHERE itemfg.company EQ ipcCompany 
         NO-ERROR.
    IF AVAILABLE itemfg THEN DO:
         ASSIGN
             opcPartID = itemfg.part-no
             opcItemID = itemfg.i-no
             .
 
         RETURN.
    END.*/
END PROCEDURE.




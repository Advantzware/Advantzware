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

RUN nosweat/persist.p  PERSISTENT SET Persistent-Handle.
RUN lstlogic/persist.p PERSISTENT SET ListLogic-Handle.

RUN system/session.p  PERSISTENT SET hSession.
SESSION:ADD-SUPER-PROCEDURE (hSession).
RUN system/TagProcs.p PERSISTENT SET hTags.
SESSION:ADD-SUPER-PROCEDURE (hTags).

{sys/inc/var.i "new shared"}


/* Company and location codes has been hardcoded for now, 
   however this has to be fixed in future commits as per the finalized approach*/
ASSIGN
    cocode = "001"
    locode = "MAIN"
    .
 
 {cXML/cXMLDefs.i}  
 {cXML/cXMLOrderProc.i}

  DEFINE INPUT  PARAMETER ipcXMLData       AS LONGCHAR  NO-UNDO.
  DEFINE INPUT  PARAMETER lpcTempTableOnly AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcPayloadID     AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oplSuccess       AS LOGICAL   NO-UNDO.
  DEFINE OUTPUT PARAMETER opcReturnValue   AS CHARACTER NO-UNDO.

  DEFINE VARIABLE payLoadID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE orderID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fromIdentity AS CHARACTER NO-UNDO.
  DEFINE VARIABLE orderDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE custNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE shipToID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE iNextOrderNumber LIKE oe-ord.ord-no NO-UNDO.
  DEFINE VARIABLE iNextShipNo LIKE shipto.ship-no   NO-UNDO.
  DEFINE VARIABLE dItemQtyEach LIKE oe-ordl.qty NO-UNDO.
  DEFINE VARIABLE cShipToTaxCode AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE rOrdRec AS ROWID NO-UNDO.
  DEFINE VARIABLE cShipToID AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cPartnerItem AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDueDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lIsEdiXML AS LOGICAL NO-UNDO.
  
  DEFINE VARIABLE hOrderProcs AS HANDLE NO-UNDO.
  DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
  
FUNCTION getCustNo-local RETURNS CHARACTER (ipIdentity AS CHARACTER, ipcShipToID AS CHARACTER):
  DEFINE VARIABLE cReturnCust AS CHARACTER NO-UNDO.
  FIND FIRST sys-ctrl-shipto NO-LOCK
       WHERE sys-ctrl-shipto.company EQ cocode
         AND sys-ctrl-shipto.name EQ 'cXMLOrder'
         AND sys-ctrl-shipto.cust-vend EQ YES
         AND sys-ctrl-shipto.char-fld EQ ipIdentity
         AND sys-ctrl-shipto.log-fld EQ YES NO-ERROR.
  IF AVAILABLE sys-ctrl-shipto THEN
      cReturnCust =  sys-ctrl-shipto.cust-vend-no. 
  ELSE
      RETURN "".
  /* Option to find cust# by cust-no + ship-id */
  IF sys-ctrl-shipto.int-fld EQ 1 
      AND cReturnCust GT "" 
      AND ipcShipToID GT "" THEN DO:
      FOR EACH edMast NO-LOCK 
        WHERE edMast.cust EQ cReturnCust
        ,
        EACH edShipTo NO-LOCK 
            WHERE edShipTo.partner EQ edMast.partner
              AND edShipTo.siteID EQ ipIdentity
            ,
            FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                  AND shipto.cust-no EQ edShipto.cust
                  AND shipto.ship-id EQ ipcShipToID
              :
        cReturnCust = edShipTo.cust.
      END.
  END.
  RETURN cReturnCust.
END FUNCTION.
  
  RUN oe/OrderProcs.p PERSISTENT SET hOrderProcs.

  RUN XMLOutput/APIXMLParser.p (ipcXMLData) NO-ERROR.
  
  IF ERROR-STATUS:ERROR OR NOT TEMP-TABLE ttNodes:HAS-RECORDS THEN DO:
      ASSIGN
          opcReturnValue = "Requested XML is not in valid format" +  " " + ERROR-STATUS:get-message(1).
          oplSuccess     = NO
          .
      RETURN.  
  END.
  
  FIND FIRST ttNodes NO-LOCK
        WHERE ttNodes.nodeName BEGINS "ISA" 
        NO-ERROR. 

  lIsEdiXml = (IF AVAILABLE ttNodes THEN YES ELSE NO).
          payLoadID = getNodeValue('cXML','payloadID').
          opcPayloadID = payLoadID.

  IF NOT lIsEdiXML THEN DO:
      ASSIGN
        payLoadID = getNodeValue('cXML','payloadID')
        opcPayloadID = payLoadID
        fromIdentity = getNodeValue('From','Identity')
        orderDate = getNodeValue('OrderRequestHeader','orderDate')
        orderID = getNodeValue('OrderRequestHeader','orderID')
        shipToID = getNodeValue('shipTo','AddressID')
        custNo = getCustNo-local(fromIdentity, shipToID)
        NO-ERROR
        .
      
      IF payLoadID EQ "" OR
         fromIdentity EQ "" OR
         orderDate EQ ? OR
         orderID EQ "" OR
         shipToID EQ "" THEN DO:
          ASSIGN
              opcReturnValue = "Requested XML is not in valid format"
              oplSuccess     = NO
              .
          RETURN. 
      END.

      FIND FIRST oe-ord NO-LOCK
           WHERE oe-ord.company EQ cocode
             AND oe-ord.cust-no EQ custNo
             AND oe-ord.po-no   EQ orderID
             AND oe-ord.spare-char-3 EQ payLoadID
           NO-ERROR.
      IF AVAILABLE oe-ord AND orderID GT "" THEN DO:
        opcReturnValue = 'Order already exists with PO#: ' + orderID + ', Payload ID: ' + payloadID.
        RETURN.
      END.

      FIND FIRST cust NO-LOCK
           WHERE cust.company EQ cocode
             AND cust.cust-no EQ custNo
           NO-ERROR.
      IF NOT AVAILABLE cust THEN DO:
  
        opcReturnValue = 'Customer: ' + custNo + ' not found'.
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
          opcReturnValue = 'Part Number is missing from XML file' .
          RETURN.
      END. 

      RUN genTempOrderHeader (OUTPUT cReturn).

      RUN genTempOrderLines (INPUT rOrdRec, OUTPUT cReturn).  

      IF cReturn NE "Success" THEN DO:
          opcReturnValue = cReturn.
          RETURN.
      END. 
    
  END.  
  ELSE DO:
      ASSIGN 
        payLoadID = "1"
        opcPayloadID = payLoadID.
      FIND FIRST ttNodes WHERE  
         ttNodes.nodeName EQ "ISA06" NO-ERROR.
      IF AVAILABLE ttNodes THEN 
          fromIdentity = ttNodes.nodeValue.
      /* fromIdentity = getNodeValue('ISA','ISA06'). */
      orderDate    = getNodeValue('BEG','BEG05'). 

      ASSIGN 
          orderdate = SUBSTRING(orderDate, 1, 4) + "-" + substring(orderDate, 5, 2) + "-" + substring(orderDate, 7, 2) /* "2018 11 05" */
          custNo = getCustNo-local(fromIdentity, "" /* Custno by shipto */)
          NO-ERROR.

      IF payLoadID EQ "" OR
         fromIdentity EQ "" OR
         orderDate EQ ? THEN DO:
          ASSIGN
              opcReturnValue = "Requested XML is not in valid format"
              oplSuccess     = NO
              .
          RETURN. 
      END.
                
      FIND FIRST cust NO-LOCK
          WHERE cust.company EQ cocode
          AND cust.cust-no EQ custNo
          NO-ERROR.
      IF NOT AVAILABLE cust THEN 
      DO:
          opcReturnValue = 'Customer: ' + custNo + ' not found'.
          RETURN.
      END.
      FOR EACH ttNodes NO-LOCK 
        WHERE ttNodes.nodeName EQ "PO107":
            cPartnerItem = ttNodes.nodeValue.
            FIND FIRST itemfg NO-LOCK 
                WHERE itemfg.company EQ cocode
                  AND itemfg.part-no EQ cPartnerItem
                NO-ERROR.

            IF NOT AVAILABLE itemfg THEN DO:
                opcReturnValue = 'Part Number is missing from XML file' .
                RETURN.                
            END.
      END.

      RUN cxml\xmltoOrderGE.p (INPUT TABLE ttNodes, INPUT-OUTPUT TABLE ttOrdHead , INPUT-OUTPUT TABLE ttOrdLines, INPUT-OUTPUT TABLE ttOrdSchedShipments).
  END.
  
  IF lpcTempTableOnly THEN 
      RETURN.
  
  EACH-ORDER:
  FOR EACH  ttOrdHead NO-LOCK  
         WHERE (ttOrdHead.ttDocType EQ "PO" OR ttOrdHead.ttDocType EQ "850") :

      ttOrdHead.ttSelectedOrder = TRUE.
    
      iNextOrderNumber = GetNextOrder#().
      RUN genOrderHeader (INPUT iNextOrderNumber, INPUT orderDate, OUTPUT rOrdRec).
      IF NOT lIsEdiXML THEN DO:

      END.
      RUN assignOrderHeader (INPUT rOrdRec, OUTPUT cShipToID, OUTPUT cReturn).
      RUN genOrderLines (INPUT rOrdRec, INPUT cShipToID, OUTPUT cReturn).
      RUN touchOrder (INPUT rOrdRec, OUTPUT cReturn).
 
      ASSIGN ttOrdHead.ttSelectedOrder = FALSE ttOrdHead.ttProcessed = TRUE.
      
      RUN ProcessImportedOrder IN hOrderProcs (rOrdRec, OUTPUT lError, OUTPUT cMessage).
      
  END. 
  
  RELEASE oe-ord.  
  RELEASE reftable.
  RELEASE oe-ord-whs-order.
  RELEASE oe-ordl-whs-item.
  
  ASSIGN
      oplSuccess     = TRUE
      opcReturnValue = 'Successfully Generated Order'
      .

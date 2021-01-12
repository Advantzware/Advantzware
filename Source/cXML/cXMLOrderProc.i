/* cXMLOrderProc.i - shared by oe/impord.w & cXML/monitor.w */

{oe/createRelease.i}
{oe/getPrice.i}

DEFINE VARIABLE ccXMLCustomerPartSource AS CHARACTER NO-UNDO.
DEFINE VARIABLE icXMLCustomerPartLength AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnValue               AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound               AS LOGICAL   NO-UNDO.
DEFINE VARIABLE hOrderProcs             AS HANDLE    NO-UNDO.

RUN oe/OrderProcs.p PERSISTENT SET hOrderProcs.

PROCEDURE cXMLOrder:
  DEFINE VARIABLE XMLFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
  DEFINE VARIABLE returnValue AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cCompanyMask  AS CHARACTER NO-UNDO INITIAL "$company$". /* Mask character */
  DEFINE VARIABLE ccXMLOrderDir AS CHARACTER NO-UNDO. 
  
  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ cocode
         AND sys-ctrl.name EQ 'cXMLOrder'
       NO-ERROR.
  IF sys-ctrl.log-fld THEN DO:
    ccXMLOrderDir = sys-ctrl.char-fld.
    IF INDEX(ccXMLOrderDir,cCompanyMask) GT 0 THEN /* checks whether mask character present or not */
        ccXMLOrderDir = REPLACE(ccXMLOrderDir,cCompanyMask,cocode). /* Replaces mask with company code */
    INPUT FROM OS-DIR(ccXMLOrderDir) NO-ECHO.
    REPEAT:
      SET XMLFile ^ attrList.
      IF attrList NE 'f' OR XMLFile BEGINS '.' OR
         INDEX(XMLFile,'.xml') EQ 0 THEN NEXT.
      XMLFile = SEARCH(ccXMLOrderDir + '/' + XMLFile).
      IF XMLFile NE ? THEN DO:
        RUN gencXMLOrder (XMLFile,
                          NO, /* temptable only */
                          OUTPUT returnValue).
      END. /* cxmlfile ne ? */
    END. /* repeat */
  END. /* if log-fld */
END PROCEDURE.

PROCEDURE genOrderHeader:
  DEFINE INPUT  PARAMETER ipiNextOrderNumber AS INTEGER NO-UNDO.
  DEFINE INPUT  PARAMETER ipcOrderDate AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER oprOrdRec AS ROWID NO-UNDO.
    
  CREATE oe-ord.
  ASSIGN
    oe-ord.company = cocode
    oe-ord.loc = locode
    oe-ord.ord-date = TODAY
    oe-ord.ord-date = DATE(INT(SUBSTR(ipcOrderDate,6,2))
                          ,INT(SUBSTR(ipcOrderDate,9,2))
                          ,INT(SUBSTR(ipcOrderDate,1,4)))
    oe-ord.ord-no = ipiNextOrderNumber
    oe-ord.user-id = USERID('asi')
    oe-ord.type = 'O'
    oe-ord.stat = 'W'
    oe-ord.due-code = 'ON'
    .
  oprOrdRec = ROWID(oe-ord).
  FIND CURRENT oe-ord NO-LOCK NO-ERROR.
END PROCEDURE.

PROCEDURE assignOrderHeader:
    DEFINE INPUT  PARAMETER iprOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipToID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

    DEFINE VARIABLE payLoadID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE fromIdentity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE toIdentity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE senderIdentity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE orderDate AS CHARACTER NO-UNDO.
    DEFINE VARIABLE orderID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE totalMoney AS CHARACTER NO-UNDO.
    DEFINE VARIABLE paymentPCard AS CHARACTER NO-UNDO.
    DEFINE VARIABLE paymentExpiration AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToAddress1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToAddress2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToCity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToState AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToZip AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToContact AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shiptoCountry AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToEmail AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToPhone AS CHARACTER NO-UNDO.
    DEFINE VARIABLE shipToAreaCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToName AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToAddress1 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToAddress2 AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToCity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToState AS CHARACTER NO-UNDO.
    DEFINE VARIABLE billToZip AS CHARACTER NO-UNDO.
    DEFINE VARIABLE custNo AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemLineNumber AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemQuantity AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemSupplierPartID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemManufacturerPartID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemSupplierPartAuxiliaryID AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemMoney AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemDescription AS CHARACTER NO-UNDO.
    DEFINE VARIABLE itemUnitOfMeasure AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
    DEFINE VARIABLE iNextOrderNumber LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE iNextShipNo LIKE shipto.ship-no NO-UNDO.
    DEFINE VARIABLE dItemQtyEach LIKE oe-ordl.qty NO-UNDO.
    DEFINE VARIABLE cShipToTaxCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rOrdRec AS ROWID NO-UNDO.

    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-shipto-state FOR shipto.
    FIND FIRST ttOrdHead NO-ERROR.
    FIND FIRST ttordlines NO-ERROR.

    FOR EACH ttOrdHead 
       WHERE /*ttOrdHead.ttSelectedOrder
         AND */ ttOrdHead.ttDocType NE "860":
          ASSIGN 
            PayLoadID             =           ttOrdHead.ttPayLoadID        
            fromIdentity          =           ttOrdHead.ttfromIdentity     
            toIdentity            =           ttOrdHead.tttoIdentity       
            senderIdentity        =           ttOrdHead.ttsenderIdentity   
            orderDate             =           ttOrdHead.ttorderDate        
            orderID               =           ttOrdHead.ttorderID          
            totalMoney            =           ttOrdHead.tttotalMoney       
            paymentPCard          =           ttOrdHead.ttpaymentPCard     
            paymentExpiration     =           ttOrdHead.ttpaymentExpiration
            shipToID              =           ttOrdHead.ttshipToID         
            shipToName            =           ttOrdHead.ttshipToName       
            shipToContact         =           ttOrdHead.ttshipToContact    
            shipToAddress1        =           ttOrdHead.ttshipToAddress1   
            shipToAddress2        =           ttOrdHead.ttshipToAddress2   
            shipToCity            =           ttOrdHead.ttshipToCity       
            shipToState           =           ttOrdHead.ttshipToState      
            shipToZip             =           ttOrdHead.ttshipToZip        
            shipToCountry         =           ttOrdHead.ttshipToCountry    
            shipToEmail           =           ttOrdHead.ttshipToEmail      
            shipToAreaCode        =           ttOrdHead.ttshipToAreaCode   
            shipToPhone           =           ttOrdHead.ttshipToPhone      
            billToID              =           ttOrdHead.ttbillToID         
            billToName            =           ttOrdHead.ttbillToName       
            billToAddress1        =           ttOrdHead.ttbillToAddress1   
            billToAddress2        =           ttOrdHead.ttbillToAddress2   
            billToCity            =           ttOrdHead.ttbillToCity       
            billToState           =           ttOrdHead.ttbillToState      
            billToZip             =           ttOrdHead.ttbillToZip        
            custNo                =           ttOrdHead.ttcustNo           
         .          
        IF INDEX(billToID, ":") > 0 AND NOT INDEX(billToID, ":") EQ LENGTH(TRIM(billToID)) THEN 
          billToID = TRIM(SUBSTRING(billToID, INDEX(billToID, ":") + 1, LENGTH(billToID))).
    
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ cocode
               AND cust.cust-no EQ custNo NO-ERROR.
        IF NOT AVAILABLE cust THEN DO:
          opcReturnValue = 'Customer: ' + custNo + ' not found'.
          RETURN.
        END.
    
        FIND oe-ord WHERE ROWID(oe-ord) EQ iprOeOrd EXCLUSIVE-LOCK.
        ASSIGN
          oe-ord.cust-no = custNo
          oe-ord.sold-id = billToID
          oe-ord.po-no = orderID
          oe-ord.spare-char-3 = payLoadID
          oe-ord.ship-id       = shipToID
          oe-ord.cust-name     = cust.name
          oe-ord.addr[1]       = cust.addr[1]
          oe-ord.addr[2]       = cust.addr[2]
          oe-ord.city          = cust.city
          oe-ord.state         = cust.state
          oe-ord.zip           = cust.zip
          oe-ord.contact       = cust.contact
          oe-ord.last-date     = oe-ord.ord-date + cust.ship-days
          oe-ord.due-date      = oe-ord.last-date
          oe-ord.terms         = cust.terms
          oe-ord.over-pct      = cust.over-pct
          oe-ord.under-pct     = cust.under-pct
          oe-ord.fob-code      = cust.fob-code
          oe-ord.frt-pay       = cust.frt-pay
          oe-ord.tax-gr        = cust.tax-gr
          oe-ord.sman[1]       = cust.sman
          oe-ord.s-pct[1]      = 100.00
          oe-ord.carrier       = cust.carrier
          oe-ord.csrUser_id    = cust.csrUser_id
          oe-ord.cc-num        = paymentPCard
          oe-ord.cc-type       = IF paymentPCard BEGINS '3' THEN 'AMEX'
                            ELSE IF paymentPCard BEGINS '4' THEN 'VISA'
                            ELSE IF paymentPCard BEGINS '5' THEN 'MC'
                            ELSE IF paymentPCard BEGINS '6' THEN 'DISCOVER'
                            ELSE ''
          .
          IF paymentExpiration NE '' THEN
          oe-ord.cc-expiration = DATE(INT(SUBSTR(paymentExpiration,6,2))
                                     ,INT(SUBSTR(paymentExpiration,9,2))
                                     ,INT(SUBSTR(paymentExpiration,1,4)))
          .
    
        FIND FIRST sman NO-LOCK
             WHERE sman.company EQ oe-ord.company
               AND sman.sman EQ cust.sman NO-ERROR.
        IF AVAILABLE sman THEN
        ASSIGN
          oe-ord.sname[1] = sman.sname
          oe-ord.s-comm[1] = sman.scomm
          .
    
        FIND FIRST terms NO-LOCK
             WHERE terms.company EQ cust.company
               AND terms.t-code  EQ cust.terms NO-ERROR.
        IF AVAILABLE terms THEN oe-ord.terms-d = terms.dscr.
    
        FIND FIRST soldto NO-LOCK
             WHERE soldto.company EQ oe-ord.company
               AND soldto.cust-no EQ oe-ord.cust-no
               AND soldto.sold-id EQ oe-ord.sold-id NO-ERROR.
        IF NOT AVAIL soldto THEN
            FIND FIRST soldto NO-LOCK
              WHERE soldto.company EQ oe-ord.company
                AND soldto.cust-no EQ oe-ord.cust-no
                AND soldto.sold-id EQ oe-ord.cust-no NO-ERROR.
    
        IF AVAILABLE soldto THEN
        ASSIGN
          oe-ord.sold-id = soldto.sold-id
          oe-ord.sold-no = soldto.sold-no
          oe-ord.sold-name = soldto.sold-name
          oe-ord.sold-addr[1] = soldto.sold-addr[1]
          oe-ord.sold-addr[2] = soldto.sold-addr[2]
          oe-ord.sold-city = soldto.sold-city
          oe-ord.sold-state = soldto.sold-state
          oe-ord.sold-zip = soldto.sold-zip
          .
    
        IF oe-ord.frt-pay = 'B' THEN oe-ord.f-bill = YES.
        ELSE oe-ord.f-bill = NO.
    
        FIND FIRST sys-ctrl-shipto NO-LOCK
            WHERE sys-ctrl-shipto.company EQ cust.company
              AND sys-ctrl-shipto.NAME EQ 'cXMLShipToPrefix'
              AND sys-ctrl-shipto.cust-vend EQ YES
              AND sys-ctrl-shipto.cust-vend-no EQ cust.cust-no
            NO-ERROR.
        /* 52995 DSG Automated Ship To Creation */            
        IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.char-fld NE '' THEN DO:
            shipToID = REPLACE(shipToID, sys-ctrl-shipto.char-fld, "").
            oe-ord.ship-id   = shipToID.
        END.
        IF NOT CAN-FIND(FIRST shipto
                        WHERE shipto.company EQ oe-ord.company
                          AND shipto.cust-no EQ oe-ord.cust-no
                          AND shipto.ship-id EQ shipToID) THEN DO:
          FIND FIRST bf-shipto
              WHERE bf-shipto.company EQ oe-ord.company
                AND bf-shipto.cust-no EQ oe-ord.cust-no
                AND bf-shipto.ship-id EQ oe-ord.cust-no
              NO-LOCK NO-ERROR.
          iNextShipNo = nextShipNo(cocode,custNo).
          CREATE shipto.
          ASSIGN
            shipto.company = cocode
            shipto.cust-no = custNo
            shipto.ship-no = iNextShipNo
            shipto.ship-id = IF shipToID NE '' THEN shipToID ELSE STRING(shipto.ship-no)
            shipto.contact = shipToContact
            shipto.loc = IF AVAIL(bf-shipto) THEN bf-shipto.loc
                             ELSE locode
            shipto.area-code = shipToAreaCode
            shipto.phone = shipToPhone
            shipto.ship-name = shipToName
            shipto.ship-addr[1] = shipToAddress1
            shipto.ship-addr[2] = shipToAddress2
            shipto.ship-city = shipToCity
            shipto.ship-state = shipToState
            shipto.ship-zip = shipToZip
            shipto.country = shipToCountry
            shipto.carrier = IF AVAIL(bf-shipto) THEN bf-shipto.carrier 
                             ELSE IF AVAIL(cust) THEN cust.carrier 
                             ELSE ""
            shipto.dest-code = IF AVAIL(bf-shipto) THEN bf-shipto.dest-code 
                             ELSE ""
            .
    /* 10061401 */
    /*         FIND FIRST stax                          */
    /*             WHERE stax.company EQ oe-ord.company */
    /*               AND stax.tax-group EQ shipToState  */
    /*             NO-LOCK NO-ERROR.                    */
    /*         IF AVAIL stax THEN                       */
    /*             cShipToTaxCode = stax.tax-group.     */
    /*         ELSE                                     */
            FIND FIRST bf-shipto-state
                WHERE bf-shipto-state.company EQ shipto.company
                  AND bf-shipto-state.cust-no EQ shipto.cust-no
                  AND bf-shipto-state.ship-id NE shipto.cust-no
                  AND bf-shipto-state.ship-state EQ shipto.ship-state
                NO-LOCK NO-ERROR.
              cShipToTaxCode = IF AVAIL(bf-shipto-state) THEN bf-shipto-state.tax-code 
                                ELSE IF AVAIL(bf-shipto) THEN bf-shipto.tax-code
                                ELSE IF AVAIL(bf-shipto) THEN bf-shipto.tax-code 
                                ELSE IF AVAIL(cust) THEN cust.tax-gr 
                                ELSE "".
            shipto.tax-code = cShipToTaxCode.
            shipToID = shipto.ship-id.
            /*10061401*/
        END. /* can find shipto */
        opcShipToID = shipToID.
    
        RELEASE oe-ord.  
        RELEASE reftable.
        RELEASE oe-ord-whs-order.
    END.
    opcReturnValue =  'Success'.

END PROCEDURE.

PROCEDURE genTempOrderHeader:  
    DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

    EMPTY TEMP-TABLE ttOrdHead.

    
    CREATE ttOrdHead.
    ASSIGN
        ttOrdHead.ttPayLoadID         = getNodeValue('cXML','payloadID')
        ttOrdHead.ttfromIdentity      = getNodeValue('From','Identity')
        ttOrdHead.tttoIdentity        = getNodeValue('To','Identity')
        ttOrdHead.ttsenderIdentity    = getNodeValue('Sender','Identity')
        ttOrdHead.ttorderDate         = getNodeValue('OrderRequestHeader','orderDate')
        ttOrdHead.ttorderID           = getNodeValue('OrderRequestHeader','orderID')
        ttOrdHead.tttotalMoney        = getNodeValue('Total','Money')
        ttOrdHead.ttpaymentPCard      = getNodeValue('Payment','number')
        ttOrdHead.ttpaymentExpiration = getNodeValue('Payment','expiration')
        ttOrdHead.ttshipToID          = getNodeValue('shipTo','AddressID')
        ttOrdHead.ttshipToName        = getNodeValue('shipTo','Name')
        ttOrdHead.ttshipToContact     = getNodeValue('shipTo','DeliverTo')
        ttOrdHead.ttshipToAddress1    = getNodeValue('shipTo','Street|1')
        ttOrdHead.ttshipToAddress2    = getNodeValue('shipTo','Street|2')
        ttOrdHead.ttshipToCity        = getNodeValue('shipTo','City')
        ttOrdHead.ttshipToState       = getNodeValue('shipTo','State')
        ttOrdHead.ttshipToZip         = getNodeValue('shipTo','PostalCode')
        ttOrdHead.ttshipToCountry     = getNodeValue('Country','isoCountryCode')
        ttOrdHead.ttshipToEmail       = getNodeValue('Address','Email')
        ttOrdHead.ttshipToAreaCode    = getNodeValue('TelephoneNumber','AreaOrCityCode')
        ttOrdHead.ttshipToPhone       = getNodeValue('TelephoneNumber','Number')
        ttOrdHead.ttbillToID          = getNodeValue('billTo','AddressID')
        ttOrdHead.ttbillToName        = getNodeValue('billTo','Name')
        ttOrdHead.ttbillToAddress1    = getNodeValue('billTo','Street|1')
        ttOrdHead.ttbillToAddress2    = getNodeValue('billTo','Street|2')
        ttOrdHead.ttbillToCity        = getNodeValue('billTo','City')
        ttOrdHead.ttbillToState       = getNodeValue('billTo','State')
        ttOrdHead.ttbillToZip         = getNodeValue('billTo','PostalCode')
        ttOrdHead.ttDocType           = "PO"
        ttOrdHead.ttcustNo            = getCustNo(ttOrdHead.ttfromIdentity, ttOrdHead.ttshipToID)
        .             
        opcReturnValue =  'Success'.
END PROCEDURE.

PROCEDURE genTempOrderLines:
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
            WHEN 'itemID|supplierPartID' THEN
                ttOrdLines.ttItemSupplierPartID = TRIM(ttNodes.nodeValue).
            WHEN 'itemID|supplierPartAuxiliaryID'THEN
                ttOrdLines.ttItemSupplierPartAuxiliaryID = TRIM(ttNodes.nodeValue).
            WHEN 'unitPrice|money'THEN
                ttOrdLines.ttItemMoney = TRIM(ttNodes.nodeValue).
            WHEN 'itemDetail|description' THEN DO:
                ttOrdLines.ttItemDescription = TRIM(ttNodes.nodeValue).
                IF icXMLCustomerPartLength EQ 0 THEN DO:
                    IF ccXMLCustomerPartSource EQ "SupplierPartId" THEN
                        ttOrdLines.ttItemSupplierPartID = IF ttOrdLines.ttItemSupplierPartID EQ "" THEN
                                                             ttOrdLines.ttItemDescription
                                                          ELSE
                                                              ttOrdLines.ttItemSupplierPartID. 
                    ELSE IF ccXMLCustomerPartSource EQ "AuxiliaryPartId" THEN
                        ttOrdLines.ttItemSupplierPartID = ttOrdLines.ttItemSupplierPartAuxiliaryID.
                    ELSE                                                     
                        ttOrdLines.ttItemSupplierPartID = ttOrdLines.ttItemDescription.            
                END.
                ELSE DO:
                    IF ccXMLCustomerPartSource EQ "SupplierPartId" THEN
                        ttOrdLines.ttItemSupplierPartID = IF ttOrdLines.ttItemSupplierPartID EQ "" THEN
                                                             SUBSTRING(ttOrdLines.ttItemDescription,1,icXMLCustomerPartLength)
                                                          ELSE
                                                              SUBSTRING(ttOrdLines.ttItemSupplierPartID,1,icXMLCustomerPartLength). 
                    ELSE IF ccXMLCustomerPartSource EQ "AuxiliaryPartId" THEN
                        ttOrdLines.ttItemSupplierPartID = SUBSTRING(ttOrdLines.ttItemSupplierPartAuxiliaryID,1,icXMLCustomerPartLength).
                    ELSE
                        ttOrdLines.ttItemSupplierPartID = SUBSTRING(ttOrdLines.ttItemDescription,1,icXMLCustomerPartLength).
                END.                    
            END.
            WHEN  'itemDetail|unitOfMeasure' THEN
                ttOrdLines.ttItemUnitOfMeasure = TRIM(ttNodes.nodeValue).
            WHEN  'itemDetail|ManufacturerPartID' THEN DO:
                ttOrdLines.ttItemManufacturerPartID = TRIM(ttNodes.nodeValue).
            END.
                
        END CASE.        

    END.  
     
    FOR EACH ttOrdLines NO-LOCK 
        WHERE ttOrdLines.ttpayLoadID      EQ ttOrdHead.ttpayLoadID
        :
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ cocode
               AND itemfg.i-no    EQ TRIM(ttOrdLines.ttItemManufacturerPartID) 
             NO-ERROR.

        /* Manufacturer part is being assigned to oe-ordl.i-no */
        IF NOT AVAIL itemfg THEN DO:
             FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ cocode
                   AND itemfg.cust-no EQ ttOrdHead.ttcustNo
                   AND itemfg.part-no EQ TRIM(ttOrdLines.ttItemSupplierPartID) 
                 NO-ERROR.
             IF AVAIL itemfg THEN 
                 ttOrdLines.ttItemManufacturerPartID = itemfg.i-no.
        END.
        IF NOT AVAIL itemfg THEN DO:
            ASSIGN 
                lSuccess = FALSE
                cFailedReason = " Item not found - Order not loaded " + TRIM(ttOrdLines.ttItemManufacturerPartID) + ' / ' + ttOrdLines.ttItemSupplierPartID
                .
            &IF DEFINED(monitorActivity) NE 0 &THEN
            RUN monitorActivity ('Error: Item ' + TRIM(ttOrdLines.ttItemManufacturerPartID) + ' / ' + ttOrdLines.ttItemSupplierPartID + '/' + ttOrdHead.ttcustno + ' not found.',YES,'').
            &ENDIF      
        END.    
    END.
    IF lSuccess EQ FALSE THEN DO:
         EMPTY TEMP-TABLE ttOrdLines.
         EMPTY TEMP-TABLE ttOrdHead.
    END.
    opcReturnValue = IF lSuccess THEN  'Success' ELSE cFailedReason.
END PROCEDURE.

PROCEDURE genOrderLines:
  DEFINE INPUT  PARAMETER iprOeOrd AS ROWID NO-UNDO.
  DEFINE INPUT  PARAMETER ipcShipToID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

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
  
  DEFINE VARIABLE lOEImportConsol AS LOGICAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMTotal AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMDL AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMFO AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMVO AS DECIMAL NO-UNDO.
  DEFINE VARIABLE dCostPerUOMDM AS DECIMAL NO-UNDO.
  DEFINE VARIABLE cCostUOM AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE hdCostProcs AS HANDLE.
  RUN system\CostProcs.p PERSISTENT SET hdCostProcs.
  
  DEFINE BUFFER bf-ttOrdLines FOR ttOrdLines.
  
  FIND oe-ord WHERE ROWID(oe-ord) EQ iprOeOrd NO-LOCK NO-ERROR.
  
  RUN sys/ref/nk1look.p (INPUT oe-ord.company, "CaseUOMList", "C" /* Logical */, NO /* check by cust */, 
            INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
            OUTPUT cRtnChar, OUTPUT lRecFound).
  cCaseUomList = cRtnChar.  

  RUN sys/ref/nk1look.p (
      INPUT  oe-ord.company, 
      INPUT  "OEImportConsol", 
      INPUT  "L",  /* LOGICAL */
      INPUT  YES,  /* check by cust */
      INPUT  YES,  /* use cust not vendor */
      INPUT  oe-ord.cust-no, /* cust */
      INPUT  oe-ord.ship-id, /* ship-to*/
      OUTPUT cRtnChar,
      OUTPUT lRecFound
      ).
  
  IF lRecFound THEN
      lOEImportConsol = LOGICAL(cRtnChar).

  FIND FIRST cust WHERE cust.cust-no EQ oe-ord.cust-no 
    AND cust.company EQ oe-ord.company NO-LOCK NO-ERROR.
  FIND FIRST ttordlines NO-LOCK NO-ERROR.

  IF NOT AVAILABLE ttOrdHead THEN 
    FIND FIRST ttOrdHead
      WHERE ttOrdHead.ttOrderID EQ oe-ord.po-no NO-ERROR.
  
  /* Code to delete duplicates order line. Applicable only for the customers
     in NK1 OEImportConsol configuration */
  IF lOEImportConsol THEN DO:
      FOR EACH ttOrdLines 
          WHERE ttOrdLines.ttpayLoadID EQ ttOrdHead.ttpayLoadID
          BY ttOrdLines.ttItemLineNumber:
          FOR EACH bf-ttOrdLines
              WHERE bf-ttOrdLines.ttpayLoadID              EQ ttOrdHead.ttpayLoadID
                AND bf-ttOrdLines.ttItemManufacturerPartID EQ ttOrdLines.ttItemManufacturerPartID
                AND bf-ttOrdLines.ttItemLineNumber         NE ttOrdLines.ttItemLineNumber:
              ttOrdLines.ttItemQuantity = STRING(DECIMAL(ttOrdLines.ttItemQuantity) + DECIMAL(bf-ttOrdLines.ttItemQuantity)).
              DELETE bf-ttOrdLines.
          END.
      END.
  END.

  FOR EACH ttOrdLines WHERE 
      ttOrdLines.ttpayLoadID = ttOrdHead.ttpayLoadID
      BY ttItemLineNumber:
     ASSIGN cRequestedDeliveryDate = ttOrdLines.ttItemDueDate
            dRequestedDeliveryDate = DATE(INT(SUBSTR(cRequestedDeliveryDate,6,2))
                                     ,INT(SUBSTR(cRequestedDeliveryDate,9,2))
                                     ,INT(SUBSTR(cRequestedDeliveryDate,1,4)))        
            NO-ERROR.

      ASSIGN 
              itemLineNumber                = ttOrdLines.ttItemLineNumber              
              itemQuantity                  = ttOrdLines.ttItemQuantity                 
              itemSupplierPartID            = ttOrdLines.ttItemSupplierPartID           
              itemManufacturerPartID        = ttOrdLines.ttItemManufacturerPartID       
              itemSupplierPartAuxiliaryID   = ttOrdLines.ttItemSupplierPartAuxiliaryID  
              itemMoney                     = ttOrdLines.ttItemMoney                    
              itemDescription               = ttOrdLines.ttItemDescription              
              itemUnitOfMeasure             = ttOrdLines.ttItemUnitOfMeasure            
              ItemDueDate                   = ttOrdLines.ttItemDueDate   
              .

      FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
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
        oe-ordl.stat      = 'W'
        .
    
      IF oe-ordl.price EQ 0 THEN DO:                      
        FIND FIRST xoe-ord OF oe-ord NO-LOCK.
        RUN getPrice (ROWID(oe-ordl)).
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
      RUN GetCostForFGItem IN hdCostProcs(oe-ordl.company,oe-ordl.i-no, OUTPUT dCostPerUOMTotal, OUTPUT dCostPerUOMDL,OUTPUT dCostPerUOMFO,
                                             OUTPUT dCostPerUOMVO,OUTPUT dCostPerUOMDM, OUTPUT cCostUOM , OUTPUT lFound) .
       oe-ordl.cost = dCostPerUOMTotal .
       oe-ordl.t-cost = oe-ordl.cost * oe-ordl.qty / 1000 .  

      IF oe-ordl.pr-uom NE "EA" THEN DO:  /*This assumes the qty uom is the same as the price uom on imported orders*/
            ASSIGN 
                oe-ordl.spare-dec-1 = oe-ordl.qty
                oe-ordl.spare-char-2 = oe-ordl.pr-uom
                oe-ordl.t-price = oe-ordl.spare-dec-1 * oe-ordl.price
                oe-ordl.pr-uom = (IF LOOKUP(oe-ordl.pr-uom, cCaseUOMList) GT 0 THEN "CS" ELSE oe-ordl.pr-uom)
                .
            RUN Conv_QtyToEA(oe-ordl.company, oe-ordl.i-no, oe-ordl.qty, oe-ordl.pr-uom, itemfg.case-count, OUTPUT oe-ordl.qty).
      END. /*oe-ordl.pr-uom ne "EA"*/
      ELSE 
         oe-ordl.t-price = oe-ordl.qty * oe-ordl.price.
       
      oe-ordl.cas-cnt = IF oe-ordl.qty LT itemfg.case-count THEN oe-ordl.qty ELSE itemfg.case-count.
      /* {oe/defwhsed.i oe-ordl} */
      
      IF oe-ordl.req-date EQ ? THEN 
        oe-ordl.req-date = oe-ord.ord-date + 10.

      oe-ordl.promiseDate = oe-ordl.req-date.

      IF oe-ord.promiseDate EQ ? THEN DO:
          FIND CURRENT oe-ord EXCLUSIVE-LOCK NO-ERROR.
          IF AVAILABLE oe-ord THEN
              oe-ord.promiseDate = oe-ordl.promiseDate.
      END.
      RUN Tax_GetTaxableAR  (oe-ord.company, oe-ord.cust-no, oe-ord.ship-id, oe-ordl.i-no, OUTPUT oe-ordl.tax).
      
      RUN CreateRelease (INPUT ipcShipToID,
                         INPUT "").
      RELEASE oe-ordl.
      RELEASE oe-rel.      
      RELEASE reftable.
      RELEASE oe-ord-whs-order.
      RELEASE oe-ordl-whs-item.
  END. /* for each  */
  
  DELETE OBJECT hdCostProcs.
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
  RELEASE oe-ord.  
  RELEASE oe-ordl.
END PROCEDURE.

PROCEDURE gencXMLOrder:
  DEFINE INPUT  PARAMETER ipcXMLFile     AS CHARACTER NO-UNDO.
  DEFINE INPUT  PARAMETER lpcTempTableOnly AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

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
  DEFINE VARIABLE lOEAutoApproval AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.
  DEFINE VARIABLE lError AS LOGICAL NO-UNDO.
  DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cMissingLineMessage AS CHARACTER NO-UNDO.
  
  RUN XMLOutput/XMLParser.p (ipcXMLFile).
  FIND FIRST ttNodes NO-LOCK
        WHERE ttNodes.nodeName BEGINS "ISA" 
        NO-ERROR. 

  lIsEdiXml = (IF AVAILABLE ttNodes THEN YES ELSE NO).

  IF NOT lIsEdiXML THEN DO:
      ASSIGN
        payLoadID = getNodeValue('cXML','payloadID')
        fromIdentity = getNodeValue('From','Identity')
        orderDate = getNodeValue('OrderRequestHeader','orderDate')
        orderID = getNodeValue('OrderRequestHeader','orderID')
        shipToID = getNodeValue('shipTo','AddressID')
        custNo = getCustNo(fromIdentity, shipToID)
        
        .
      RUN sys/ref/nk1look.p(
          INPUT  cocode,
          INPUT  "cXMLCustomerPartSource",
          INPUT  "C",
          INPUT  YES,
          INPUT  YES,
          INPUT  custNo,
          INPUT  shipToID,
          OUTPUT ccXMLCustomerPartSource,
          OUTPUT lRecFound    
          ).
        
      RUN sys/ref/nk1look.p(
          INPUT  cocode,
          INPUT  "cXMLCustomerPartSource",
          INPUT  "I",
          INPUT  YES,
          INPUT  YES,
          INPUT  custNo,
          INPUT  shipToID,
          OUTPUT cRtnValue,
          OUTPUT lRecFound    
          ). 
         
      icXMLCustomerPartLength = IF INTEGER(cRtnValue) GT 15 THEN 15 ELSE INTEGER(cRtnValue).
      
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
        payLoadID = "1".
      FIND FIRST ttNodes WHERE  
         ttNodes.nodeName EQ "ISA06" NO-ERROR.
      IF AVAILABLE ttNodes THEN 
          fromIdentity = ttNodes.nodeValue.
      /* fromIdentity = getNodeValue('ISA','ISA06'). */
      orderDate    = getNodeValue('BEG','BEG05'). 

      ASSIGN 
          orderdate = SUBSTRING(orderDate, 1, 4) + "-" + substring(orderDate, 5, 2) + "-" + substring(orderDate, 7, 2) /* "2018 11 05" */
          custNo = getCustNo(fromIdentity, "" /* Custno by shipto */)
          .
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
      
  cMissingLineMessage = "".  
  EACH-ORDER:
  FOR EACH  ttOrdHead NO-LOCK  
         WHERE (ttOrdHead.ttDocType EQ "PO" OR ttOrdHead.ttDocType EQ "850") :
                 

      ttOrdHead.ttSelectedOrder = TRUE.
      
      FIND FIRST  ttOrdLines 
        WHERE ttOrdLines.ttpayLoadID = ttOrdHead.ttpayLoadID
        NO-ERROR.
      IF NOT AVAIL ttOrdLines THEN DO:
          /* No order line for the order header so can't create it */
          cMissingLineMessage = "No order line for PO ttOrdHead.po-no".
          NEXT.          
      END.
      
      iNextOrderNumber = GetNextOrder#().
      RUN genOrderHeader (INPUT iNextOrderNumber, INPUT orderDate, OUTPUT rOrdRec).
      IF NOT lIsEdiXML THEN DO:

      END.
      RUN assignOrderHeader (INPUT rOrdRec, OUTPUT cShipToID, OUTPUT cReturn).
      RUN genOrderLines (INPUT rOrdRec, INPUT cShipToID, OUTPUT cReturn).
      RUN touchOrder (INPUT rOrdRec, OUTPUT cReturn).
 
      ASSIGN ttOrdHead.ttSelectedOrder = FALSE ttOrdHead.ttProcessed = TRUE.
      
      /* Determine autoapproval for this customer/shipto */
      lOEAutoApproval = NO.
      RUN sys/ref/nk1look.p (cocode, "OEAutoApproval", "L", YES /* use shipto */, YES /* use cust*/, ttOrdHead.TTcustNO, cShipToID, 
                              OUTPUT cResult, OUTPUT lFound).
      IF lFound THEN
        lOEAutoApproval = LOGICAL(cResult) NO-ERROR.
      
      /* 52995 DSG Automated Ship To Creation */
      IF lOeAutoApproval THEN 
        RUN ProcessImportedOrder IN hOrderProcs (rOrdRec, OUTPUT lError, OUTPUT cMessage).
      
  END. 
  
  RELEASE oe-ord.  
  RELEASE reftable.
  RELEASE oe-ord-whs-order.
  RELEASE oe-ordl-whs-item.
  IF cMissingLineMessage EQ "" THEN 
    opcReturnValue = 'Successfully Generated Order'.
  ELSE 
    opcReturnValue = cMissingLineMessage.
  
END PROCEDURE.

PROCEDURE touchOrder:
  /* Allow the oe-ord write trigger to fire after order lines created */
  DEFINE INPUT  PARAMETER iprOeOrd       AS ROWID     NO-UNDO.
  DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

  FIND FIRST oe-ord EXCLUSIVE-LOCK
       WHERE ROWID(oe-ord) EQ iprOeOrd
       NO-ERROR.
  /* t-revenue is recalculated in trigger */
  oe-ord.t-revenue = 1.
  FIND CURRENT oe-ord NO-LOCK.

END PROCEDURE.


/* cXMLOrderProc.i - shared by oe/impord.w & cXML/monitor.w */

{oe/createRelease.i}
{oe/getPrice.i}

PROCEDURE cXMLOrder:
  DEFINE VARIABLE XMLFile AS CHARACTER FORMAT 'X(50)' NO-UNDO.
  DEFINE VARIABLE attrList AS CHARACTER FORMAT 'X(4)' NO-UNDO.
  DEFINE VARIABLE returnValue AS CHARACTER   NO-UNDO.
  
  FIND FIRST sys-ctrl NO-LOCK
       WHERE sys-ctrl.company EQ cocode
         AND sys-ctrl.name EQ 'cXMLOrder'
       NO-ERROR.
  IF sys-ctrl.log-fld THEN DO:
    INPUT FROM OS-DIR(sys-ctrl.char-fld) NO-ECHO.
    REPEAT:
      SET XMLFile ^ attrList.
      IF attrList NE 'f' OR XMLFile BEGINS '.' OR
         INDEX(XMLFile,'.xml') EQ 0 THEN NEXT.
      XMLFile = SEARCH(sys-ctrl.char-fld + '/' + XMLFile).
      IF XMLFile NE ? THEN DO:
        RUN gencXMLOrder (XMLFile,
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
    oe-ord.user-id = USERID('NoSweat')
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

    ASSIGN
      payLoadID = getNodeValue('cXML','payloadID')
      fromIdentity = getNodeValue('From','Identity')
      toIdentity = getNodeValue('To','Identity')
      senderIdentity = getNodeValue('Sender','Identity')
      orderDate = getNodeValue('OrderRequestHeader','orderDate')
      orderID = getNodeValue('OrderRequestHeader','orderID')
      totalMoney = getNodeValue('Total','Money')
      paymentPCard = getNodeValue('Payment','number')
      paymentExpiration = getNodeValue('Payment','expiration')
      shipToID = getNodeValue('shipTo','AddressID')
      shipToName = getNodeValue('shipTo','Name')
      shipToContact = getNodeValue('shipTo','DeliverTo')
      shipToAddress1 = getNodeValue('shipTo','Street|1')
      shipToAddress2 = getNodeValue('shipTo','Street|2')
      shipToCity = getNodeValue('shipTo','City')
      shipToState = getNodeValue('shipTo','State')
      shipToZip = getNodeValue('shipTo','PostalCode')
      shipToCountry = getNodeValue('Country','isoCountryCode')
      shipToEmail = getNodeValue('Address','Email')
      shipToAreaCode = getNodeValue('TelephoneNumber','AreaOrCityCode')
      shipToPhone = getNodeValue('TelephoneNumber','Number')
      billToID = getNodeValue('billTo','AddressID')
      billToName = getNodeValue('billTo','Name')
      billToAddress1 = getNodeValue('billTo','Street|1')
      billToAddress2 = getNodeValue('billTo','Street|2')
      billToCity = getNodeValue('billTo','City')
      billToState = getNodeValue('billTo','State')
      billToZip = getNodeValue('billTo','PostalCode')
      custNo = getCustNo(fromIdentity)
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

    {custom/rec_key.i "oe-ord"}
    FIND FIRST sys-ctrl-shipto NO-LOCK
        WHERE sys-ctrl-shipto.company EQ cust.company
          AND sys-ctrl-shipto.NAME EQ 'cXMLShipToPrefix'
          AND sys-ctrl-shipto.cust-vend EQ YES
          AND sys-ctrl-shipto.cust-vend-no EQ cust.cust-no
        NO-ERROR.
    IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.char-fld NE '' THEN DO:
       shipToID = TRIM(shipToID, sys-ctrl-shipto.char-fld).
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
        oe-ord.ship-id   = shipto.ship-id  /*31899 - apply oe-ord.ship-id after prefix is trimmed*/
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

    opcReturnValue =  'Success'.

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
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE iNextOrderNumber LIKE oe-ord.ord-no NO-UNDO.
  DEFINE VARIABLE iNextShipNo LIKE shipto.ship-no NO-UNDO.
  DEFINE VARIABLE dItemQtyEach LIKE oe-ordl.qty NO-UNDO.
  DEFINE VARIABLE cShipToTaxCode AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cRequestedDeliveryDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE dRequestedDeliveryDate AS DATE NO-UNDO.

  FIND oe-ord WHERE ROWID(oe-ord) EQ iprOeOrd NO-LOCK NO-ERROR.

  FOR EACH ttNodes:
    ASSIGN
        dRequestedDeliveryDate = oe-ord.due-date
        cRequestedDeliveryDate = ""
        .
    IF ttNodes.parentName EQ 'itemOut' AND ttNodes.nodeName EQ 'lineNumber' THEN
      itemLineNumber = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemOut' AND ttNodes.nodeName EQ 'requestedDeliveryDate' THEN
      cRequestedDeliveryDate = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemOut' AND ttNodes.nodeName EQ 'quantity' THEN
      itemQuantity = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemID' AND ttNodes.nodeName EQ 'supplierPartID' THEN
      itemSupplierPartID = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemID' AND ttNodes.nodeName EQ 'supplierPartAuxiliaryID' THEN
      itemSupplierPartAuxiliaryID = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'unitPrice' AND ttNodes.nodeName EQ 'money' THEN
      itemMoney = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemDetail' AND ttNodes.nodeName EQ 'description' THEN
      itemDescription = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemDetail' AND ttNodes.nodeName EQ 'unitOfMeasure' THEN 
      itemUnitOfMeasure = TRIM(ttNodes.nodeValue).
    ELSE IF ttNodes.parentName EQ 'itemDetail' AND ttNodes.nodeName EQ 'ManufacturerPartID' THEN DO:
      itemManufacturerPartID = TRIM(ttNodes.nodeValue).
    
      FIND FIRST itemfg NO-LOCK
           WHERE itemfg.company EQ cocode
             AND itemfg.i-no    EQ TRIM(itemSupplierPartID) NO-ERROR.
      IF NOT AVAILABLE itemfg THEN DO:
        &IF DEFINED(monitorActivity) NE 0 &THEN
        RUN monitorActivity ('ERROR: Item ' + TRIM(itemSupplierPartID) + ' not found.',YES,'').
        &ENDIF
        NEXT.
      END.
      
      IF cRequestedDeliveryDate NE "" THEN
      dRequestedDeliveryDate = DATE(INT(SUBSTR(cRequestedDeliveryDate,6,2))
                                   ,INT(SUBSTR(cRequestedDeliveryDate,9,2))
                                   ,INT(SUBSTR(cRequestedDeliveryDate,1,4))).
      
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
        oe-ordl.i-no      = TRIM(itemSupplierPartID)
        oe-ordl.part-no   = itemManufacturerPartID
        oe-ordl.qty       = DEC(itemQuantity)
        oe-ordl.pr-uom    = itemUnitOfMeasure
        oe-ordl.price     = DEC(itemMoney)
        oe-ordl.est-no    = oe-ord.est-no
        oe-ordl.q-qty     = oe-ord.t-fuel
        oe-ordl.whsed     = oe-ordl.est-no NE ''
        oe-ordl.q-no      = oe-ord.q-no
        oe-ordl.prom-date = oe-ord.due-date
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

      IF oe-ordl.pr-uom NE "EA" THEN DO:
        ASSIGN 
          oe-ordl.spare-dec-1 = oe-ordl.qty
          oe-ordl.spare-char-2 = oe-ordl.pr-uom
          oe-ordl.t-price = oe-ordl.spare-dec-1 * oe-ordl.price
          .
        IF oe-ordl.pr-uom EQ "CS" OR oe-ordl.pr-uom EQ "PLT" THEN
        oe-ordl.qty = oe-ordl.qty * itemfg.case-count.
        ELSE IF oe-ordl.pr-uom EQ "C" THEN oe-ordl.qty = oe-ordl.qty * 100.
        ELSE oe-ordl.qty = oe-ordl.qty * 1000.
      END.
      ELSE 
      oe-ordl.t-price = oe-ordl.qty * oe-ordl.price.
       
      oe-ordl.cas-cnt = IF oe-ordl.qty LT itemfg.case-count THEN oe-ordl.qty ELSE itemfg.case-count.
      /* {oe/defwhsed.i oe-ordl} */

      RUN CreateRelease (INPUT ipcShipToID,
                         INPUT "").
      RELEASE oe-ordl.
      RELEASE oe-rel.      
      RELEASE reftable.
      RELEASE oe-ord-whs-order.
      RELEASE oe-ordl-whs-item.
    END. /* ttNodes */
  END. /* each ttnodes */
  
  
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
  DEFINE OUTPUT PARAMETER opcReturnValue AS CHARACTER NO-UNDO.

  DEFINE VARIABLE payLoadID AS CHARACTER NO-UNDO.
  DEFINE VARIABLE fromIdentity AS CHARACTER NO-UNDO.
  DEFINE VARIABLE orderDate AS CHARACTER NO-UNDO.
  DEFINE VARIABLE custNo AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iCount AS INTEGER NO-UNDO.
  DEFINE VARIABLE iNextOrderNumber LIKE oe-ord.ord-no NO-UNDO.
  DEFINE VARIABLE iNextShipNo LIKE shipto.ship-no   NO-UNDO.
  DEFINE VARIABLE dItemQtyEach LIKE oe-ordl.qty NO-UNDO.
  DEFINE VARIABLE cShipToTaxCode AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE rOrdRec AS ROWID NO-UNDO.
  DEFINE VARIABLE cShipToID AS CHARACTER   NO-UNDO.
  DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cDueDate AS CHARACTER NO-UNDO.

  RUN XMLOutput/XMLParser.p (ipcXMLFile).
  ASSIGN
    payLoadID = getNodeValue('cXML','payloadID')
    fromIdentity = getNodeValue('From','Identity')
    orderDate = getNodeValue('OrderRequestHeader','orderDate')
    custNo = getCustNo(fromIdentity)
    .
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
  IF NOT AVAILABLE ttNodes THEN DO:
      opcReturnValue = 'Part Number is missing from XML file' .
      RETURN.
  END. 
    
    
  iNextOrderNumber = GetNextOrder#().
  RUN genOrderHeader (INPUT iNextOrderNumber, INPUT orderDate, OUTPUT rOrdRec).
  RUN assignOrderHeader (INPUT rOrdRec, OUTPUT cShipToID, OUTPUT cReturn).  
  RUN genOrderLines (INPUT rOrdRec, INPUT cShipToID, OUTPUT cReturn).
  RUN touchOrder (INPUT rOrdRec, OUTPUT cReturn).

  RELEASE oe-ord.  
  RELEASE reftable.
  RELEASE oe-ord-whs-order.
  RELEASE oe-ordl-whs-item.
  
  opcReturnValue = 'Successfully Generated Order'.
  
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


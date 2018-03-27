/* cXMLOrderFunc.i - shared by oe/impord.w & cXML/monitor.w */

DEFINE VARIABLE iNextOrder# AS INTEGER NO-UNDO.

FUNCTION getCustNo RETURNS CHARACTER (ipIdentity AS CHARACTER):
  FIND FIRST sys-ctrl-shipto NO-LOCK
       WHERE sys-ctrl-shipto.company EQ cocode
         AND sys-ctrl-shipto.name EQ 'cXMLOrder'
         AND sys-ctrl-shipto.cust-vend EQ YES
         AND sys-ctrl-shipto.char-fld EQ ipIdentity
         AND sys-ctrl-shipto.log-fld EQ YES NO-ERROR.
  RETURN IF AVAILABLE sys-ctrl-shipto THEN sys-ctrl-shipto.cust-vend-no ELSE ''.
END FUNCTION.

FUNCTION getNextOrder# RETURNS INTEGER ( /* parameter-definitions */ ):
  DEFINE BUFFER bf-oe-ord FOR oe-ord.

  RUN sys/ref/asiseq.p (cocode,'order_seq',OUTPUT iNextOrder#) NO-ERROR.

  /* Supposed to be a new order number, so cannot be found on an existing order */
  DO WHILE CAN-FIND(FIRST bf-oe-ord
                    WHERE bf-oe-ord.company EQ cocode
                      AND bf-oe-ord.ord-no  EQ iNextOrder#):
    RUN sys/ref/asiseq.p (cocode,'order_seq',OUTPUT iNextOrder#) NO-ERROR.
  END.
  RETURN iNextOrder#.
END FUNCTION.

FUNCTION getNodeValue RETURNS CHARACTER (ipParentName AS CHARACTER,ipNodeName AS CHARACTER):
  DEFINE VARIABLE nodeOrder AS INTEGER NO-UNDO.
  DEFINE VARIABLE subNode AS INTEGER NO-UNDO.

  IF NUM-ENTRIES(ipNodeName,'|') GT 1 THEN
  ASSIGN
    subNode = INT(ENTRY(2,ipNodeName,'|'))
    ipNodeName = ENTRY(1,ipNodeName,'|').
  FIND FIRST ttNodes
       WHERE ttNodes.parentName EQ ipParentName NO-ERROR.
  IF AVAILABLE ttNodes THEN DO:
    nodeOrder = ttNodes.order.
    FOR EACH ttNodes WHERE ttNodes.order GE nodeOrder
                       AND ttNodes.order LE nodeOrder + 25:
      IF ttNodes.nodeName EQ ipNodeName THEN DO:
        IF subNode NE 0 THEN
        ttNodes.nodeName = ttNodes.nodeName + STRING(subNode).
        RETURN TRIM(ttNodes.nodeValue).
      END. /* if found node */
    END. /* each ttnodes */
  END. /* avail ttnodes */
  RETURN ''.
END FUNCTION.

FUNCTION nextShipNo RETURNS INTEGER (ipCompany AS CHARACTER, ipCustNo AS CHARACTER):
  DEFINE BUFFER shipto FOR shipto.

  FIND LAST shipto NO-LOCK USE-INDEX ship-no
       WHERE shipto.company EQ ipCompany
         AND shipto.cust-no EQ ipCustNo NO-ERROR.
  RETURN IF AVAILABLE shipto THEN shipto.ship-no + 1 ELSE 1.
END FUNCTION.

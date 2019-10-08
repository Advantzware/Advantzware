/* cXMLCust.i - RStark - Task: 05291402 (cXML) */
/* syntax: {cXMLCust.i &cXMLSysCtrl=cXML??? &Company=??? &Customer=???} */

FIND FIRST sys-ctrl-shipto NO-LOCK
     WHERE sys-ctrl-shipto.company EQ {&Company}
       AND sys-ctrl-shipto.name EQ '{&cXMLSysCtrl}'
       AND sys-ctrl-shipto.cust-vend EQ YES
       AND sys-ctrl-shipto.cust-vend-no EQ {&Customer}
     NO-ERROR.
IF AVAILABLE sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN DO:
  ASSIGN
    cXMLIdentity = sys-ctrl-shipto.char-fld
    cXMLFile = '/{&cXMLSysCtrl}.' + {&Customer} + '.' + REPLACE(REPLACE(STRING(NOW),":",""),"/","") + '.xml' 
    cXMLTemp = 'XMLOutput' + cXMLFile
    clXMLOutput = YES
    .
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ {&Company}
       AND sys-ctrl.name EQ 'cXMLIdentity'
     NO-ERROR.
IF AVAIL sys-ctrl THEN
    ASSIGN
        cXMLIdentityCust = sys-ctrl.char-fld
       cXMLProduction = (IF sys-ctrl.log-fld THEN "production" ELSE "test")
    .
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ {&Company}
       AND sys-ctrl.name EQ 'cXMLSecret'
     NO-ERROR.
IF AVAIL sys-ctrl THEN
    ASSIGN
        cXMLSharedSecret = sys-ctrl.char-fld
       .

  OUTPUT STREAM cXMLOutput TO VALUE(cXMLTemp).
  RUN cXMLOutput (clXMLOutput,'','','Header').
END.
ELSE clXMLOutput = NO.

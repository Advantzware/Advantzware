&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME cust

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-inv-head FOR inv-head.


DISABLE TRIGGERS FOR LOAD OF oe-prmtx.
DISABLE TRIGGERS FOR LOAD OF itemfg.

IF {&TABLENAME}.cust-no      NE ""                       AND
   old-{&TABLENAME}.cust-no  NE ""                       AND
   (old-{&TABLENAME}.cust-no NE {&TABLENAME}.cust-no OR 
    old-{&TABLENAME}.type    NE {&TABLENAME}.type)       THEN
FOR EACH oe-prmtx
    WHERE oe-prmtx.company EQ old-{&TABLENAME}.company
      AND oe-prmtx.cust-no EQ old-{&TABLENAME}.cust-no:
  ASSIGN
   oe-prmtx.cust-no = {&TABLENAME}.cust-no
   oe-prmtx.custype = {&TABLENAME}.type.
END.

IF {&TABLENAME}.cust-no NE ""                 AND
   {&TABLENAME}.name NE old-{&TABLENAME}.name THEN
FOR EACH itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.cust-no EQ {&TABLENAME}.cust-no:
  itemfg.cust-name = {&TABLENAME}.name.
END.

IF {&TABLENAME}.cust-no      NE ""                        AND
   old-{&TABLENAME}.inv-meth EQ ?                         AND
   {&TABLENAME}.inv-meth     NE old-{&TABLENAME}.inv-meth THEN
FOR EACH inv-head
    WHERE inv-head.company       EQ {&TABLENAME}.company
      AND inv-head.cust-no       EQ {&TABLENAME}.cust-no
      AND inv-head.multi-invoice EQ YES
      AND (inv-head.inv-no       EQ 0 OR
           NOT CAN-FIND(FIRST b-inv-head
                        WHERE b-inv-head.company EQ inv-head.company
                          AND b-inv-head.cust-no EQ inv-head.cust-no
                          AND b-inv-head.inv-no  EQ inv-head.inv-no
                          AND NOT b-inv-head.multi-invoice)):

  FOR EACH b-inv-head
      WHERE b-inv-head.company EQ inv-head.company
        AND b-inv-head.cust-no EQ inv-head.cust-no
        AND b-inv-head.inv-no  EQ 0
        AND NOT b-inv-head.multi-invoice:
    b-inv-head.stat = "".
  END.

  DELETE inv-head.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.


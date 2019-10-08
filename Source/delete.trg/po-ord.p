&Scoped-define ACTION DELETE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME po-ord

TRIGGER PROCEDURE FOR DELETE OF {&TABLENAME}.

{methods/triggers/delete.i}

DEF VAR ll AS LOG NO-UNDO.


IF {&TABLENAME}.received THEN DO:
  MESSAGE "You must close PO when receipts have been entered against it, cannot delete..."
         VIEW-AS ALERT-BOX ERROR.
  RETURN ERROR.
END.

ll = NO.

FOR EACH po-ordl
    WHERE po-ordl.company EQ {&TABLENAME}.company
      AND po-ordl.po-no   EQ {&TABLENAME}.po-no
    NO-LOCK:

  IF po-ordl.t-rec-qty NE 0 OR po-ordl.t-inv-qty NE 0 THEN ll = YES.

  IF NOT ll THEN
  FOR EACH ap-inv
      WHERE ap-inv.company EQ po-ordl.company
        AND ap-inv.posted  EQ NO
      NO-LOCK,
      EACH ap-invl
      WHERE ap-invl.i-no EQ ap-inv.i-no
        AND ap-invl.line EQ ((po-ordl.po-no * 1000) + po-ordl.line)
      NO-LOCK:
    ll = YES.
    LEAVE.
  END.
  
  IF NOT ll THEN
    IF po-ordl.item-type THEN
      ll = CAN-FIND(FIRST rm-rctd
                    WHERE rm-rctd.company    EQ po-ordl.company
                      AND rm-rctd.rita-code  EQ "R"
                      AND rm-rctd.i-no       EQ po-ordl.i-no
                      AND INT(rm-rctd.po-no) EQ po-ordl.po-no
                    USE-INDEX rita-code) NO-ERROR.
    ELSE
      ll = CAN-FIND(FIRST fg-rctd
                    WHERE fg-rctd.company    EQ po-ordl.company
                      AND fg-rctd.rita-code  EQ "R"
                      AND fg-rctd.i-no       EQ po-ordl.i-no
                      AND INT(fg-rctd.po-no) EQ po-ordl.po-no
                    USE-INDEX rita-code) NO-ERROR.

  IF ll THEN DO:
    MESSAGE "Receipts and/or invoices have been entered against an item, cannot delete..."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
  END.
END.

FOR EACH po-ordl WHERE po-ordl.company = {&TABLENAME}.company
                   AND po-ordl.po-no = {&TABLENAME}.po-no:
  DELETE po-ordl.
END.

for each oe-ordl where oe-ordl.company  eq {&TABLENAME}.company
                   and oe-ordl.po-no-po eq {&TABLENAME}.po-no
        use-index po-no-po:
        
      assign
       oe-ordl.po-no-po = 0
       oe-ordl.vend-no  = "".
end.
      
find first po-ctrl WHERE po-ctrl.company = {&TABLENAME}.company exclusive-lock no-error.
if avail po-ctrl and {&TABLENAME}.po-no = po-ctrl.next-po-no - 1 then
  po-ctrl.next-po-no = po-ctrl.next-po-no - 1.

FIND FIRST vend OF {&TABLENAME} NO-LOCK NO-ERROR.

IF {&TABLENAME}.opened AND AVAIL vend THEN RUN ap/vendobal.p (ROWID(vend)).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.


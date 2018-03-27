
{sys/inc/var.i NEW SHARED}

DEF BUFFER b-ref1 FOR reftable.
DEF BUFFER b-ref2 FOR reftable.

DEF VAR li AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF po-ordl.

PAUSE 0 BEFORE-HIDE.

FOR EACH company,
    EACH po-ordl
    WHERE po-ordl.company EQ company.company
      AND po-ordl.opened  EQ YES
    USE-INDEX opened
    BY po-ordl.company BY po-ordl.po-no:

  cocode = po-ordl.company.

  DISPLAY "Processing Company/PO#: " +
          TRIM(po-ordl.company) + "/" +
          TRIM(STRING(po-ordl.po-no),">>>>>") FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  {po/po-ordls.i}
  
  {po/poordls2W.i}

  ld = 0.
  IF AVAIL b-ref1 THEN
  DO li = 1 TO 12:
    ld = ld + b-ref1.val[li].
  END.
  IF AVAIL b-ref2 THEN
  DO li = 1 TO 8:
    ld = ld + b-ref2.val[li].
  END. 

  IF ld EQ 0 THEN DO:
    IF AVAIL b-ref1 THEN DELETE b-ref1.
    IF AVAIL b-ref2 THEN DELETE b-ref2.

    RUN po/po-ordls.p (RECID(po-ordl)).
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

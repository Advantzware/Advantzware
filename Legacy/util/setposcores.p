
{sys/inc/var.i NEW SHARED}


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
  
  {po/poordls2W.i}

  ld = 0.
  DO li = 1 TO 20:
   IF po-ordl.scorePanels[li] NE 0 THEN   
    ld = ld + po-ordl.scorePanels[li].
  END.


  IF ld EQ 0 THEN DO:
    RUN po/po-ordls.p (RECID(po-ordl)).
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

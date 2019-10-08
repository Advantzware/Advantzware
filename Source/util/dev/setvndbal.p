
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR li-unit AS INT NO-UNDO.


FOR EACH company NO-LOCK:
  FOR EACH vend WHERE vend.company EQ company.company BY vend.vend-no:

    DISPLAY vend.company LABEL "Company"
            vend.vend-no LABEL "Vendor#"
        WITH FRAME f1 TITLE "  Vendor Order Balance  ".
        
    RUN ap/vendobal.p (ROWID(vend)).
  END.
END.

MESSAGE "Procedure Complete..."
    VIEW-AS ALERT-BOX INFO BUTTONS OK.

HIDE FRAME f1 NO-PAUSE.


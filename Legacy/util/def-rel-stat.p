DEF VAR a AS INT NO-UNDO.
IF NOT USERID("nosweat") = "ASI" THEN DO:
    MESSAGE "Must be logged in as ASI to run this utility!" VIEW-AS ALERT-BOX.
    RETURN.
END.

PAUSE 0 BEFORE-HIDE.    

FOR EACH company,
    EACH oe-rel WHERE oe-rel.company EQ company.company:

  DISPLAY "Processing Company/Order#: " +
          TRIM(oe-rel.company) + "/"    +
          TRIM(STRING(oe-rel.ord-no,">>>>>>>>>")) FORMAT "x(50)"
      WITH FRAME f1 1 DOWN.

  a = oe-rel.link-no.

  DO TRANSACTION:
    oe-rel.link-no = IF oe-rel.link-no EQ 0 THEN 999999999 ELSE 0.
  END.
  DO TRANSACTION:
    oe-rel.link-no = a.
  END.
END.

HIDE FRAME f1 NO-PAUSE.

MESSAGE "Process Complete" VIEW-AS ALERT-BOX.

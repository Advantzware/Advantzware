
FOR EACH itemfg NO-LOCK TRANSACTION:
  DISPLAY itemfg.company LABEL "Company"
          itemfg.i-no    LABEL "FG#"    FORMAT "x(20)" WITH DOWN.

  IF NOT CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ "FGSTATUS"
                    AND reftable.company  EQ itemfg.company
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ itemfg.i-no)
  THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "FGSTATUS"
     reftable.company  = itemfg.company
     reftable.loc      = ""
     reftable.code     = itemfg.i-no
     reftable.code2    = "A".
  END.
END.

FOR EACH company:
  IF NOT CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ "FGSTATUS"
                    AND reftable.company  EQ company.company
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ "")
  THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "FGSTATUS"
     reftable.company  = company.company
     reftable.loc      = ""
     reftable.code     = ""
     reftable.code2    = "A".
  END.
END.

MESSAGE "Process completed..." VIEW-AS ALERT-BOX.

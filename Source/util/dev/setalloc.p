
DEF VAR ll AS LOG INIT NO NO-UNDO.


IF CAN-FIND(FIRST reftable
            WHERE reftable.reftable EQ "util/setalloc.p eb"
              AND reftable.company  EQ ""
              AND reftable.loc      EQ ""
              AND reftable.code     EQ ""
              AND reftable.code2    EQ "DONE") OR
   CAN-FIND(FIRST reftable
            WHERE reftable.reftable EQ "util/setalloc.p itemfg"
              AND reftable.company  EQ ""
              AND reftable.loc      EQ ""
              AND reftable.code     EQ ""
              AND reftable.code2    EQ "DONE") THEN
  MESSAGE "Sorry, utility has already been run and cannot be re-run..."
      VIEW-AS ALERT-BOX.
ELSE
  MESSAGE "Are you ready to reverse set allocation flag for all estimates and FGs?"
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO UPDATE ll.

IF ll THEN DO:
  DISABLE TRIGGERS FOR LOAD OF eb.
  DISABLE TRIGGERS FOR LOAD OF itemfg.

  IF NOT CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ "util/setalloc.p eb"
                    AND reftable.company  EQ ""
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ ""
                    AND reftable.code2    EQ "DONE") THEN
  FOR EACH eb
      WHERE eb.form-no EQ 0
        AND NOT CAN-FIND(FIRST reftable
                         WHERE reftable.reftable EQ "util/setalloc.p eb"
                           AND reftable.company  EQ eb.company
                           AND reftable.loc      EQ ""
                           AND reftable.code     EQ eb.est-no)
      TRANSACTION
      WITH FRAME eb WITH DOWN:

    DISPLAY eb.company      LABEL "Company"
            TRIM(eb.est-no) LABEL "Est#"    FORMAT "x(10)".

    eb.set-is-assembled = IF eb.set-is-assembled THEN NO ELSE ?.

    CREATE reftable.
    ASSIGN
     reftable.reftable = "util/setalloc.p eb"
     reftable.company  = eb.company
     reftable.loc      = ""
     reftable.code     = eb.est-no.
  END.

  HIDE FRAME eb NO-PAUSE.

  DO TRANSACTION:
    FOR EACH reftable WHERE reftable.reftable EQ "util/setalloc.p eb":
      DELETE reftable.
    END.

    CREATE reftable.
    ASSIGN
     reftable.reftable = "util/setalloc.p eb"
     reftable.company  = ""
     reftable.loc      = ""
     reftable.code     = ""
     reftable.code2    = "DONE".
  END.

  IF NOT CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ "util/setalloc.p itemfg"
                    AND reftable.company  EQ ""
                    AND reftable.loc      EQ ""
                    AND reftable.code     EQ ""
                    AND reftable.code2    EQ "DONE") THEN
  FOR EACH itemfg
      WHERE itemfg.isaset
        AND NOT CAN-FIND(FIRST reftable
                         WHERE reftable.reftable EQ "util/setalloc.p itemfg"
                           AND reftable.company  EQ itemfg.company
                           AND reftable.loc      EQ ""
                           AND reftable.code     EQ itemfg.i-no)
      TRANSACTION
      WITH FRAME itemfg WITH DOWN:

    DISPLAY itemfg.company  LABEL "Company"
            itemfg.i-no     LABEL "FG Item#"    FORMAT "x(20)".

    itemfg.alloc = IF itemfg.alloc THEN ? ELSE YES.

    IF itemfg.i-no NE "" THEN
    FOR EACH eb
        WHERE eb.company  EQ itemfg.company
          AND eb.stock-no EQ itemfg.i-no
          AND eb.form-no  EQ 0:

      eb.set-is-assembled = itemfg.alloc.

      IF eb.set-is-assembled NE ? THEN
        eb.set-is-assembled = NOT eb.set-is-assembled.
    END.

    CREATE reftable.
    ASSIGN
     reftable.reftable = "util/setalloc.p itemfg"
     reftable.company  = itemfg.company
     reftable.loc      = ""
     reftable.code     = itemfg.i-no.
  END.

  HIDE FRAME itemfg NO-PAUSE.

  DO TRANSACTION:
    FOR EACH reftable WHERE reftable.reftable EQ "util/setalloc.p itemfg":
      DELETE reftable.
    END.

    CREATE reftable.
    ASSIGN
     reftable.reftable = "util/setalloc.p itemfg"
     reftable.company  = ""
     reftable.loc      = ""
     reftable.code     = ""
     reftable.code2    = "DONE".
  END.

  MESSAGE "Procedure has completed..." VIEW-AS ALERT-BOX.
END.

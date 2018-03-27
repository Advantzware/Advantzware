
{sys/inc/var.i NEW SHARED}

DEF VAR lv-i-no LIKE fg-rcpth.i-no NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF itemfg.

FOR EACH company NO-LOCK:
  lv-i-no = "".

  FIND FIRST fg-rcpth NO-LOCK
      WHERE fg-rcpth.company EQ company.company
      USE-INDEX i-no NO-ERROR.

  DO WHILE AVAIL fg-rcpth:
    lv-i-no = fg-rcpth.i-no.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ fg-rcpth.company
          AND itemfg.i-no    EQ fg-rcpth.i-no
        NO-ERROR.

    IF AVAIL itemfg THEN DO TRANSACTION:
      RELEASE fg-rcpth.

      FOR EACH fg-rcpth NO-LOCK
          WHERE fg-rcpth.company    EQ company.company
            AND fg-rcpth.i-no       EQ lv-i-no
            AND fg-rcpth.rita-code  EQ "S"
            AND fg-rcpth.trans-date LE 02/28/2006
          USE-INDEX i-no,
        
          EACH fg-rdtlh
          WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
            AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
            AND fg-rdtlh.qty-case  GT 0
            AND (fg-rdtlh.cases EQ 0 OR fg-rdtlh.partial EQ 0)
          USE-INDEX rm-rdtl
        
          BREAK BY fg-rcpth.i-no:

        DISPLAY fg-rcpth.company LABEL "Company"
                fg-rcpth.r-no    LABEL "Seq#"
                fg-rcpth.i-no    LABEL "FG Item#"
                                 FORMAT "X(23)".

        ASSIGN
         fg-rdtlh.cases   = TRUNC((fg-rdtlh.qty - fg-rdtlh.partial) /
                                  fg-rdtlh.qty-case,0)
         fg-rdtlh.partial = fg-rdtlh.qty - (fg-rdtlh.cases * fg-rdtlh.qty-case).

        IF LAST(fg-rcpth.i-no) THEN DO:
          cocode = itemfg.company.

          RUN fg/fg-mkbin.p (RECID(itemfg)).
    
          FOR EACH fg-bin
              WHERE fg-bin.company EQ itemfg.company
                AND fg-bin.i-no    EQ itemfg.i-no
                AND fg-bin.qty     EQ 0:
            DELETE fg-bin.
          END.

          RUN fg/fg-reset.p (RECID(itemfg)).
        END.
      END.
    END.

    RELEASE fg-rcpth.

    FIND FIRST fg-rcpth NO-LOCK
        WHERE fg-rcpth.company EQ company.company
          AND fg-rcpth.i-no    GT lv-i-no
        USE-INDEX i-no NO-ERROR.
  END.
END.

MESSAGE "Procedure complete..." VIEW-AS ALERT-BOX.

HIDE ALL NO-PAUSE.


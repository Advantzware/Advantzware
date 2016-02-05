/* unit/fx0fgtrn.p */ 
DISABLE TRIGGERS FOR LOAD OF loadtag.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-i-no LIKE fg-rcpth.i-no FORMAT "X(15)" NO-UNDO.

DEF BUFFER b-rcpth FOR fg-rcpth.
DEF BUFFER b-rdtlh FOR fg-rdtlh.

DEF TEMP-TABLE tt-fg-bin LIKE fg-bin.


MESSAGE "Enter FG Item (Leave blank to exit OR '*' for all:"
        UPDATE lv-i-no.

IF lv-i-no EQ "" THEN LEAVE.

SESSION:SET-WAIT-STATE ("general").

STATUS DEFAULT "Searching...".

IF lv-i-no EQ "*" THEN DO:
   FOR EACH b-rcpth
       WHERE company   EQ cocode
         AND rita-code EQ "T" NO-LOCK
       BY b-rcpth.i-no
       BY b-rcpth.trans-date
       BY b-rcpth.r-no:
       RUN check-trans.
   END.
   /* update loadtag */
   FOR EACH fg-bin WHERE company = cocode AND fg-bin.qty > 0 NO-LOCK:
       IF TRIM(fg-bin.tag) NE "" THEN
          RUN update-loadtag.
   END.
END.

ELSE DO:
  FOR EACH b-rcpth
    WHERE company   EQ cocode
      AND i-no      EQ lv-i-no
      AND rita-code EQ "T"
    NO-LOCK
    BY b-rcpth.i-no
    BY b-rcpth.trans-date
    BY b-rcpth.r-no:
    RUN check-trans.
  END.
  /* update loadtag */
  FOR EACH fg-bin NO-LOCK WHERE company = cocode 
                          AND fg-bin.i-no = lv-i-no 
                          AND fg-bin.qty > 0:  
       IF TRIM(fg-bin.tag) NE "" THEN
          RUN update-loadtag.
  END.
END.

STATUS DEFAULT "".

SESSION:SET-WAIT-STATE ("").

HIDE ALL NO-PAUSE.

MESSAGE "Process completed..." VIEW-AS ALERT-BOX.

RETURN.

PROCEDURE check-trans:
  FOR EACH tt-fg-bin:
    DELETE tt-fg-bin.
  END.

  FIND FIRST itemfg
      WHERE itemfg.company EQ b-rcpth.company
        AND itemfg.i-no    EQ b-rcpth.i-no
      NO-LOCK NO-ERROR.

  FOR EACH b-rdtlh
      WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
        AND b-rdtlh.rita-code EQ b-rcpth.rita-code
        AND b-rdtlh.qty       EQ 0
      NO-LOCK
      BY b-rdtlh.rec_key:

    DISPLAY b-rcpth.i-no          LABEL "FG#"
            b-rcpth.trans-date    LABEL "Trans Date"
            b-rcpth.r-no          LABEL "Sequence#"
        WITH DOWN.

    FOR each fg-rcpth
        where fg-rcpth.company      eq b-rcpth.company
          and fg-rcpth.i-no         eq b-rcpth.i-no
          AND fg-rcpth.job-no       EQ b-rcpth.job-no
          AND fg-rcpth.job-no2      EQ b-rcpth.job-no2
          AND (fg-rcpth.trans-date  LT b-rcpth.trans-date OR
               (fg-rcpth.trans-date EQ b-rcpth.trans-date AND
                fg-rcpth.r-no       LT b-rcpth.r-no))
          AND ROWID(fg-rcpth)       NE ROWID(b-rcpth)
        use-index tran no-lock,

        each fg-rdtlh
        where fg-rdtlh.r-no      eq fg-rcpth.r-no
          and fg-rdtlh.rita-code eq fg-rcpth.rita-code
          and fg-rdtlh.loc       eq b-rdtlh.loc
          and fg-rdtlh.loc-bin   eq b-rdtlh.loc-bin
          and fg-rdtlh.tag       eq b-rdtlh.tag
        use-index rm-rdtl no-lock

        by fg-rcpth.trans-date
        BY fg-rdtlh.trans-time
        by fg-rcpth.r-no:

      find first tt-fg-bin
          where tt-fg-bin.company eq fg-rcpth.company
            and tt-fg-bin.i-no    eq fg-rcpth.i-no
            and tt-fg-bin.loc     eq fg-rdtlh.loc
            and tt-fg-bin.loc-bin eq fg-rdtlh.loc-bin
            and tt-fg-bin.tag     eq fg-rdtlh.tag
            and tt-fg-bin.job-no  eq fg-rcpth.job-no
            and tt-fg-bin.job-no2 eq fg-rcpth.job-no2
          use-index co-ino no-error.
      if not avail tt-fg-bin then do:
        create tt-fg-bin.
        assign
         tt-fg-bin.company      = fg-rdtlh.company
         tt-fg-bin.job-no       = fg-rcpth.job-no
         tt-fg-bin.job-no2      = fg-rcpth.job-no2
         tt-fg-bin.loc          = fg-rdtlh.loc
         tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
         tt-fg-bin.tag          = fg-rdtlh.tag
         tt-fg-bin.i-no         = fg-rcpth.i-no
         tt-fg-bin.aging-date   = fg-rcpth.trans-date
         tt-fg-bin.pur-uom      = IF AVAIL itemfg THEN itemfg.prod-uom ELSE "M".
      end.

      IF fg-rcpth.rita-code EQ "R" THEN DO:
        IF tt-fg-bin.case-count   LE 0 AND fg-rdtlh.qty-case     GT 0 THEN
          tt-fg-bin.case-count   = fg-rdtlh.qty-case.
        IF tt-fg-bin.units-pallet LE 0 AND fg-rdtlh.units-pallet GT 0 THEN
          tt-fg-bin.units-pallet = fg-rdtlh.units-pallet.
        IF tt-fg-bin.cases-unit   LE 0 AND fg-rdtlh.stacks-unit  GT 0 THEN
          tt-fg-bin.cases-unit   = fg-rdtlh.stacks-unit.
      END.

      {fg/fg-mkbin.i tt-}
    END. /* each fg-rcpth */

    RELEASE fg-rdtlh.

    find first tt-fg-bin
        where tt-fg-bin.company eq b-rcpth.company
          and tt-fg-bin.i-no    eq b-rcpth.i-no
          and tt-fg-bin.loc     eq b-rdtlh.loc
          and tt-fg-bin.loc-bin eq b-rdtlh.loc-bin
          and tt-fg-bin.tag     eq b-rdtlh.tag
          and tt-fg-bin.job-no  eq b-rcpth.job-no
          and tt-fg-bin.job-no2 eq b-rcpth.job-no2
        use-index co-ino no-error.

    IF AVAIL tt-fg-bin THEN
    FOR EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ b-rcpth.r-no
          AND fg-rdtlh.rita-code EQ b-rcpth.rita-code
        BREAK BY fg-rdtlh.rec_key:
      fg-rdtlh.qty = tt-fg-bin.qty * (IF FIRST(fg-rdtlh.rec_key) THEN -1 ELSE 1).
    END.

    LEAVE.
  END.

END PROCEDURE.

PROCEDURE update-loadtag:
  IF AVAIL fg-bin THEN
  FOR EACH loadtag WHERE loadtag.company      EQ fg-bin.company
                AND loadtag.item-type    EQ NO
                AND loadtag.tag-no       EQ fg-bin.tag
                AND loadtag.i-no         EQ fg-bin.i-no
                AND loadtag.loc          EQ fg-bin.loc      /* task# 06130519 */
                AND loadtag.loc-bin      EQ fg-bin.loc-bin  /* Fg Transfer changes */
                AND loadtag.is-case-tag  EQ NO                    /* loadtags's values*/
               USE-INDEX tag:
           ASSIGN loadtag.qty       = FG-BIN.qty
                  loadtag.partial   = fg-bin.partial-count
                  loadtag.tot-cases = (loadtag.qty - loadtag.partial) / loadtag.qty-case.   

  END.
END PROCEDURE.


{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

ASSIGN
 cocode = g_company
 locode = g_loc.

DEF VAR lv-i-no LIKE fg-rcpth.i-no FORMAT "X(15)" NO-UNDO.

DEF BUFFER b-rcpth FOR fg-rcpth.
DEF BUFFER b-rdtlh FOR fg-rdtlh.

DEF TEMP-TABLE tt-fg-bin LIKE fg-bin.

OUTPUT TO c:\tmp\fxtaghst.txt.
MESSAGE "Enter FG Item (Leave blank to exit OR '*' for all:"
    UPDATE lv-i-no.

IF lv-i-no EQ "" THEN LEAVE.

SESSION:SET-WAIT-STATE ("general").

IF lv-i-no EQ "*" THEN
FOR EACH b-rcpth
    WHERE company   EQ cocode
      AND rita-code EQ "C"
    NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ b-rcpth.company
      AND itemfg.i-no    EQ b-rcpth.i-no
    NO-LOCK
    BY b-rcpth.trans-date
    BY b-rcpth.r-no:
  RUN check-hist.
END.

ELSE
FOR EACH b-rcpth
    WHERE company   EQ cocode
      AND i-no      EQ lv-i-no
      AND rita-code EQ "C"
    NO-LOCK,
    FIRST itemfg
    WHERE itemfg.company EQ b-rcpth.company
      AND itemfg.i-no    EQ b-rcpth.i-no
    NO-LOCK
    BY b-rcpth.trans-date
    BY b-rcpth.r-no:
  RUN check-hist.
END.

SESSION:SET-WAIT-STATE ("").

RETURN.

PROCEDURE check-hist:
  FOR EACH tt-fg-bin:
    DELETE tt-fg-bin.
  END.

  FIND FIRST b-rdtlh
      WHERE b-rdtlh.r-no      EQ b-rcpth.r-no
        AND b-rdtlh.rita-code EQ b-rcpth.rita-code
        AND b-rdtlh.tag       NE ""
        AND b-rdtlh.loc-bin   EQ "1 FLOOR"
        AND CAN-FIND(FIRST loadtag
                     WHERE loadtag.company     EQ b-rcpth.company
                       AND loadtag.item-type   EQ NO
                       AND loadtag.i-no        EQ b-rcpth.i-no
                       AND loadtag.tag-no      EQ b-rdtlh.tag
                       AND loadtag.is-case-tag EQ NO)
      NO-ERROR.

  IF AVAIL b-rdtlh THEN DO:
    FOR EACH fg-rcpth
        WHERE fg-rcpth.company      EQ b-rcpth.company
          AND fg-rcpth.i-no         EQ b-rcpth.i-no
          AND fg-rcpth.job-no       EQ b-rcpth.job-no
          AND fg-rcpth.job-no2      EQ b-rcpth.job-no2
          AND (fg-rcpth.trans-date  LT b-rcpth.trans-date OR
               (fg-rcpth.trans-date EQ b-rcpth.trans-date AND
                fg-rcpth.r-no       LT b-rcpth.r-no))
          AND ROWID(fg-rcpth)       NE ROWID(b-rcpth)
        USE-INDEX tran NO-LOCK,

        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          AND fg-rdtlh.loc       EQ b-rdtlh.loc
          AND fg-rdtlh.loc-bin   EQ b-rdtlh.loc-bin
          AND fg-rdtlh.tag       EQ b-rdtlh.tag
        USE-INDEX rm-rdtl NO-LOCK

        BY fg-rcpth.trans-date
        BY fg-rdtlh.trans-time
        BY fg-rcpth.r-no:

      FIND FIRST tt-fg-bin
          WHERE tt-fg-bin.company EQ fg-rcpth.company
            AND tt-fg-bin.i-no    EQ fg-rcpth.i-no
            AND tt-fg-bin.loc     EQ fg-rdtlh.loc
            AND tt-fg-bin.loc-bin EQ fg-rdtlh.loc-bin
            AND tt-fg-bin.tag     EQ fg-rdtlh.tag
            AND tt-fg-bin.job-no  EQ fg-rcpth.job-no
            AND tt-fg-bin.job-no2 EQ fg-rcpth.job-no2
          USE-INDEX co-ino NO-ERROR.
      IF NOT AVAIL tt-fg-bin THEN DO:
        CREATE tt-fg-bin.
        ASSIGN
         tt-fg-bin.company      = fg-rdtlh.company
         tt-fg-bin.job-no       = fg-rcpth.job-no
         tt-fg-bin.job-no2      = fg-rcpth.job-no2
         tt-fg-bin.loc          = fg-rdtlh.loc
         tt-fg-bin.loc-bin      = fg-rdtlh.loc-bin
         tt-fg-bin.tag          = fg-rdtlh.tag
         tt-fg-bin.i-no         = fg-rcpth.i-no
         tt-fg-bin.aging-date   = fg-rcpth.trans-date
         tt-fg-bin.pur-uom      = itemfg.prod-uom.
      END.

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

    FIND FIRST tt-fg-bin
        WHERE tt-fg-bin.company EQ b-rcpth.company
          AND tt-fg-bin.i-no    EQ b-rcpth.i-no
          AND tt-fg-bin.loc     EQ b-rdtlh.loc
          AND tt-fg-bin.loc-bin EQ b-rdtlh.loc-bin
          AND tt-fg-bin.tag     EQ b-rdtlh.tag
          AND tt-fg-bin.job-no  EQ b-rcpth.job-no
          AND tt-fg-bin.job-no2 EQ b-rcpth.job-no2
        USE-INDEX co-ino NO-ERROR.

    IF AVAIL tt-fg-bin AND tt-fg-bin.qty GT 0 THEN
    FOR EACH fg-rcpth
        WHERE fg-rcpth.company      EQ b-rcpth.company
          AND fg-rcpth.i-no         EQ b-rcpth.i-no
          AND fg-rcpth.job-no       EQ b-rcpth.job-no
          AND fg-rcpth.job-no2      EQ b-rcpth.job-no2
          AND (fg-rcpth.trans-date  LT b-rcpth.trans-date OR
               (fg-rcpth.trans-date EQ b-rcpth.trans-date AND
                fg-rcpth.r-no       LT b-rcpth.r-no))
          AND fg-rcpth.rita-code    EQ "T"
          AND ROWID(fg-rcpth)       NE ROWID(b-rcpth)
        USE-INDEX tran NO-LOCK,

        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
          AND fg-rdtlh.tag       EQ b-rdtlh.tag
          AND (fg-rdtlh.loc      NE b-rdtlh.loc OR
               fg-rdtlh.loc-bin  NE b-rdtlh.loc-bin)
          AND fg-rdtlh.qty       GT 0
        USE-INDEX rm-rdtl NO-LOCK

        BY fg-rcpth.trans-date DESC
        BY fg-rdtlh.trans-time DESC
        BY fg-rcpth.r-no       DESC:
       
       DISPLAY "FG" b-rcpth.i-no FORMAT "x(20)" SKIP
               "Job" b-rcpth.job-no b-rcpth.job-no2 SKIP
               "tag" b-rdtlh.tag FORMAT "x(30)" SKIP
               "old" b-rdtlh.loc b-rdtlh.loc-bin SKIP
               "new" fg-rdtlh.loc fg-rdtlh.loc-bin
               "qty" tt-fg-bin.qty WHEN AVAIL tt-fg-bin
                     0 WHEN NOT AVAIL tt-fg-bin @ tt-fg-bin.qty
               SKIP(1)
           WITH NO-LABELS DOWN.

       ASSIGN
        b-rdtlh.loc     = fg-rdtlh.loc
        b-rdtlh.loc-bin = fg-rdtlh.loc-bin.

      LEAVE.
    END.
  END.

END PROCEDURE.

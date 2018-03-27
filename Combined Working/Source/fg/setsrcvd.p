
DEF PARAM BUFFER io-job FOR job.
DEF PARAM BUFFER io-job-comp FOR reftable.
DEF PARAM BUFFER io-job-hdr FOR job-hdr.

DEF INPUT-OUTPUT PARAM io-sets AS INT NO-UNDO.

DEF BUFFER b-job-comp FOR reftable.

{fg/fullset.i NEW}

DEF TEMP-TABLE tt-set NO-UNDO FIELD per-set AS DEC FIELD set-qty AS INT.


IF AVAIL io-job AND AVAIL io-job-comp THEN
FIND io-job-hdr NO-LOCK
    WHERE io-job-hdr.company EQ io-job.company
      AND io-job-hdr.job     EQ io-job.job
      AND io-job-hdr.job-no  EQ io-job.job-no
      AND io-job-hdr.job-no2 EQ io-job.job-no2
   NO-ERROR.

IF AVAIL io-job-hdr THEN DO:
  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ io-job-hdr.company
        AND itemfg.i-no    EQ io-job-hdr.i-no
      NO-ERROR.

  IF AVAIL itemfg THEN RUN fg/fullset.p (ROWID(itemfg)).

  FOR EACH b-job-comp NO-LOCK
      WHERE b-job-comp.reftable EQ "jc/jc-calc.p"
        AND b-job-comp.company  EQ io-job.company
        AND b-job-comp.loc      EQ ""
        AND b-job-comp.code     EQ STRING(io-job.job,"999999999"):

    FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ b-job-comp.code2 NO-ERROR.

    CREATE tt-set.
    tt-set.per-set = IF AVAIL tt-fg-set THEN tt-fg-set.part-qty-dec ELSE 1.

    IF ROWID(b-job-comp) EQ ROWID(io-job-comp) THEN
      tt-set.set-qty = io-sets.

    FOR EACH fg-rcpth NO-LOCK
        WHERE fg-rcpth.company   EQ io-job.company
          AND fg-rcpth.job-no    EQ io-job.job-no
          AND fg-rcpth.job-no2   EQ io-job.job-no2
          AND fg-rcpth.i-no      EQ b-job-comp.code2
          AND fg-rcpth.rita-code EQ "R",
        EACH fg-rdtlh NO-LOCK
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
      tt-set.set-qty = tt-set.set-qty + fg-rdtlh.qty.
    END.

    tt-set.set-qty = TRUNC(tt-set.set-qty / tt-set.per-set,0).
  END.

  io-sets = 0.
  FOR EACH tt-set BY tt-set.set-qty:
    io-sets = tt-set.set-qty.
    LEAVE.
  END.

  FOR EACH fg-act NO-LOCK
      WHERE fg-act.company EQ io-job-hdr.company
        AND fg-act.job-no  EQ io-job-hdr.job-no
        AND fg-act.job-no2 EQ io-job-hdr.job-no2
        AND fg-act.i-no    EQ io-job-hdr.i-no:
    io-sets = io-sets - fg-act.qty.
  END.
END.


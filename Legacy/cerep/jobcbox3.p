
{sys/inc/var.i SHARED}

{jcrep/r-ticket.i "shared"}

DEF BUFFER oe-ordl-whs-item FOR reftable.

DEF TEMP-TABLE tt-job-hdr LIKE job-hdr
    USE-INDEX job-no.

DEF VAR ll-add-overrn AS LOG NO-UNDO.
DEF VAR lv-over-pct LIKE oe-ordl.over-pct NO-UNDO.
DEF VAR lv-undr-pct LIKE oe-ordl.under-pct NO-UNDO.
DEF VAR ld-over-qty AS DEC NO-UNDO.
DEF VAR ld-undr-qty AS DEC NO-UNDO.
DEF VAR ld-mach-qty AS DEC NO-UNDO.
def var v-reprint as LOG NO-UNDO.

{fg/fullset.i NEW}


FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "JOB QTY"
    NO-LOCK NO-ERROR.
IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
  CREATE sys-ctrl.
  ASSIGN
    sys-ctrl.company = cocode
    sys-ctrl.name    = "JOB QTY"
    sys-ctrl.descrip = "Create Job Quantity with overrun % from OE?"
    sys-ctrl.log-fld = NO.
  MESSAGE sys-ctrl.descrip
      VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
      UPDATE sys-ctrl.log-fld.
END.

ASSIGN
   ll-add-overrn = sys-ctrl.log-fld
   v-reprint   = reprint.

{cerep/jobcbox.i}:

  IF est.est-type EQ 2 THEN DO:

      FOR EACH reftable NO-LOCK
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job-hdr.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job-hdr.job,"999999999"):
    
        CREATE tt-job-hdr.
        BUFFER-COPY job-hdr TO tt-job-hdr.
        ASSIGN
         tt-job-hdr.qty      = 0
         tt-job-hdr.n-on     = reftable.val[11]
         tt-job-hdr.frm      = reftable.val[12]
         tt-job-hdr.blank-no = reftable.val[13].
    
        RUN fg/fullset.p (ROWID(itemfg)).
    
        FOR EACH tt-fg-set WHERE tt-fg-set.part-no EQ reftable.code2:
          tt-job-hdr.qty = tt-job-hdr.qty +
                           (job-hdr.qty * tt-fg-set.part-qty-dec).
        END.
      END. /* FOR EACH reftable */
  END. /*  IF est.est-type EQ 2 */

  ELSE DO:
    CREATE tt-job-hdr.
    BUFFER-COPY job-hdr TO tt-job-hdr.
  END.
END.

FOR EACH tt-job-hdr BY tt-job-hdr.job:
  FIND FIRST oe-ordl NO-LOCK
      WHERE oe-ordl.company  EQ tt-job-hdr.company
        AND ((oe-ordl.ord-no EQ tt-job-hdr.ord-no AND
              tt-job-hdr.ord-no NE 0)                   OR
             tt-job-hdr.ord-no  EQ 0)
        AND oe-ordl.job-no   EQ tt-job-hdr.job-no
        AND oe-ordl.job-no2  EQ tt-job-hdr.job-no2
        AND oe-ordl.i-no     EQ tt-job-hdr.i-no
        AND oe-ordl.est-no   EQ tt-job-hdr.est-no
      NO-ERROR.
  IF AVAIL oe-ordl THEN DO:
    ASSIGN
     lv-over-pct = oe-ordl.over-pct
     lv-undr-pct = oe-ordl.under-pct.

    IF ll-add-overrn                                      AND
       NOT CAN-FIND(FIRST oe-ordl-whs-item NO-LOCK
                    WHERE oe-ordl.managed = true) THEN
      tt-job-hdr.qty = tt-job-hdr.qty / (1 + (lv-over-pct * .01)).
  END.

  ELSE
    ASSIGN
     lv-over-pct = 0
     lv-undr-pct = 0.

  ASSIGN
   ld-undr-qty = tt-job-hdr.qty * (1 - ((lv-undr-pct) * .01))
   ld-over-qty = tt-job-hdr.qty * (1 + ((lv-over-pct + 1) * .01)).

  {sys/inc/roundup.i ld-undr-qty}
  {sys/inc/roundup.i ld-over-qty}

  FOR EACH job-mch NO-LOCK
      WHERE job-mch.company   EQ tt-job-hdr.company
        AND job-mch.job       EQ tt-job-hdr.job
        AND job-mch.job-no    EQ tt-job-hdr.job-no
        AND job-mch.job-no2   EQ tt-job-hdr.job-no2
        AND job-mch.frm       EQ tt-job-hdr.frm
        AND (job-mch.blank-no EQ tt-job-hdr.blank-no OR
             job-mch.blank-no EQ 0)
      USE-INDEX line-idx,
      
      FIRST mach NO-LOCK
      WHERE mach.company EQ job-mch.company
        AND mach.m-code  EQ job-mch.m-code
        AND NOT CAN-DO("A,P",mach.p-type)

      BY job-mch.line DESC:
    LEAVE.
  END.

  IF AVAIL job-mch THEN DO:
    ld-mach-qty = job-mch.run-qty - job-mch.mr-waste.

    IF job-mch.wst-prct NE 0 THEN
      ld-mach-qty = ld-mach-qty - (ld-mach-qty * job-mch.wst-prct / 100).

    {sys/inc/roundup.i ld-mach-qty}

    ld-mach-qty = ld-mach-qty *
                  (IF mach.p-type EQ "B" THEN 1 ELSE tt-job-hdr.n-on).

    IF ld-mach-qty LT ld-undr-qty OR
       ld-mach-qty GT ld-over-qty THEN
      MESSAGE "Job#/Form#/Blank# "                                            +
              TRIM(job-mch.job-no) + "-" + STRING(job-mch.job-no2,"99") + "/" +
              STRING(job-mch.frm,"99")                                  + "/" +
              STRING(job-mch.blank-no,"99")                                   +
              " requires between "                                            +
              TRIM(STRING(ld-undr-qty,">,>>>,>>>,>>9"))             + " and " +
              TRIM(STRING(ld-over-qty,">,>>>,>>>,>>9"))                       +
              " blanks but has "                                              +
              TRIM(STRING(ld-mach-qty,">,>>>,>>>,>>9"))                       +
              " on Machine "                                                  +
              TRIM(job-mch.m-code)                                            +
              "..."
          VIEW-AS ALERT-BOX WARNING.
  END.
END.

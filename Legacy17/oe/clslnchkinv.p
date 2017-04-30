/* --------------------------------------------------- oe/clslnchk.p          */
/*                                                                            */
/* Order Close - Check to see if invoice line should be closed                */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEF PARAMETER BUFFER oe-ordl FOR oe-ordl. /* line item to check */
DEFINE OUTPUT PARAMETER v-lin-closed AS LOGICAL NO-UNDO INITIAL TRUE. /* OK to close */

{sys/inc/var.i SHARED}
{sys/form/s-top.f}

DEF BUFFER b-oe-ordl FOR oe-ordl.

{oe/closchk.i}

DEF VAR v-ord-closed AS LOG NO-UNDO.
DEF VAR v-rel-closed AS LOG NO-UNDO.
DEF VAR v-job-qty LIKE job-hdr.qty NO-UNDO.
DEF VAR v-rec-qty LIKE fg-rdtlh.qty NO-UNDO.
DEF VAR v-bin-qty LIKE fg-bin.qty NO-UNDO.
DEF VAR v-add-overrn AS LOG NO-UNDO.
DEF VAR v-stat AS CHAR NO-UNDO.
DEF VAR v-rel-open AS LOG NO-UNDO.
DEF VAR v-all-transfers AS LOG NO-UNDO.

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
v-add-overrn = sys-ctrl.log-fld.

DO TRANSACTION:
  {sys/inc/oereleas.i}
  {sys/inc/oeclose.i}
END.

  FIND FIRST itemfg NO-LOCK
      WHERE itemfg.company EQ oe-ordl.company
        AND itemfg.i-no    EQ oe-ordl.i-no
      NO-ERROR.
  FIND FIRST oe-ord NO-LOCK OF oe-ordl NO-ERROR.
  IF NOT AVAILABLE oe-ord THEN RETURN ERROR "No such order".
  IF oe-ordl.stat = "C" THEN RETURN "Already Closed".

  /* wfk - 03181306 - oe-ord.type is not necessarily accurate  */
  /* So check if all the releases where transfers and consider */
  /* this a transfer order line                                */
  v-all-transfers = TRUE.
  FOR EACH oe-boll NO-LOCK
    WHERE oe-boll.company  EQ oe-ordl.company
      AND oe-boll.ord-no   EQ oe-ordl.ord-no
      AND oe-boll.line     EQ oe-ordl.line
      AND oe-boll.i-no     EQ oe-ordl.i-no
    USE-INDEX ord-no:
    /* This doesn't care whether posted or not, just that everything */
    /* involved was a tranfer release                                */

    IF oe-boll.s-code NE "T" THEN DO:
            v-all-transfers = FALSE.
            LEAVE.
    END.

  END.

  IF oe-ord.type EQ "T"  OR v-all-transfers THEN DO:
    v-lin-closed = NO.

    FOR EACH oe-boll NO-LOCK
        WHERE oe-boll.company  EQ oe-ordl.company
          AND oe-boll.ord-no   EQ oe-ordl.ord-no
          AND oe-boll.line     EQ oe-ordl.line
          AND oe-boll.i-no     EQ oe-ordl.i-no
          AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no
                                       AND oe-bolh.posted EQ YES)
        USE-INDEX ord-no:
      ACCUMULATE oe-boll.qty (TOTAL).
    END.

    IF (oe-ordl.qty GT 0 AND (ACCUM TOTAL oe-boll.qty) GE oe-ordl.qty) OR
       (oe-ordl.qty LT 0 AND (ACCUM TOTAL oe-boll.qty) LE oe-ordl.qty) THEN
      v-lin-closed = YES.
  END. /* Transfer order */

  ELSE IF oeclose-chr EQ "OnHand=0" AND oe-ordl.job-no NE "" 
      AND CAN-FIND(FIRST itemfg
                   WHERE itemfg.company EQ oe-ordl.company
                     AND itemfg.i-no    EQ oe-ordl.i-no
                     AND itemfg.pur-man EQ NO)               
  THEN DO:  /* close only if bin <= 0 */
    ASSIGN
     v-lin-closed = NO
     v-job-qty    = 0
     v-rec-qty    = 0
     v-bin-qty    = 0.

    FOR EACH job-hdr FIELDS(qty)
        WHERE job-hdr.company EQ oe-ordl.company
          AND job-hdr.job-no  EQ oe-ordl.job-no
          AND job-hdr.job-no2 EQ oe-ordl.job-no2
          AND job-hdr.i-no    EQ oe-ordl.i-no
        NO-LOCK:
      v-job-qty = v-job-qty + TRUNCATE((job-hdr.qty /
                               IF v-add-overrn THEN (1 + (oe-ordl.over-pct * .01))
                               ELSE 1),0).
    END.

    FOR EACH fg-rcpth FIELDS(r-no rita-code)
        WHERE fg-rcpth.company   EQ oe-ordl.company
          AND fg-rcpth.i-no      EQ oe-ordl.i-no
          AND fg-rcpth.job-no    EQ oe-ordl.job-no
          AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
          AND fg-rcpth.rita-code EQ "R"
        NO-LOCK,
        EACH fg-rdtlh FIELDS(qty)
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        NO-LOCK:
      v-rec-qty = v-rec-qty + fg-rdtlh.qty.
    END.

    IF v-rec-qty GE v-job-qty - (v-job-qty * oe-ordl.under-pct * .01) THEN DO:
      FOR EACH fg-bin FIELDS(qty)
          WHERE fg-bin.company EQ oe-ordl.company
            AND fg-bin.job-no  EQ oe-ordl.job-no
            AND fg-bin.job-no2 EQ oe-ordl.job-no2
            AND fg-bin.i-no    EQ oe-ordl.i-no
          USE-INDEX job NO-LOCK:
        v-bin-qty = v-bin-qty + fg-bin.qty.
      END.

      IF v-bin-qty LE 0 THEN v-lin-closed = YES.
    END.
  END.

  ELSE DO: /* no bin quantity requirement */

    v-lin-closed = NO.

    /*ASSIGN v-lin-closed = oereleas-log.

    IF v-lin-closed THEN
    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-ordl.company
          AND oe-rel.ord-no  EQ oe-ordl.ord-no
          AND oe-rel.i-no    EQ oe-ordl.i-no
          AND oe-rel.line    EQ oe-ordl.line
        NO-LOCK:
      v-rel-closed = NO.
    
      IF oe-rel.link-no NE 0 THEN DO:
        RELEASE oe-rell.
        RELEASE oe-boll.

        FIND FIRST oe-rell
            WHERE oe-rell.company EQ oe-rel.company
              AND oe-rell.r-no    EQ oe-rel.link-no
              and oe-rell.ord-no  eq oe-rel.ord-no
              AND oe-rell.i-no    EQ oe-rel.i-no
              AND oe-rell.line    EQ oe-rel.line
              AND oe-rell.s-code  EQ "B"
              AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
             NO-LOCK NO-ERROR.

        IF AVAIL oe-rell THEN
        FOR EACH oe-boll FIELDS(b-no p-c)
            WHERE oe-boll.company  EQ oe-rell.company
              AND oe-boll.ord-no   EQ oe-rell.ord-no
              AND oe-boll.line     EQ oe-rell.line
              AND oe-boll.i-no     EQ oe-rell.i-no
              AND oe-boll.r-no     EQ oe-rell.r-no
              AND oe-boll.rel-no   EQ oe-rell.rel-no
              AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
              AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no   EQ oe-boll.b-no
                                           AND oe-bolh.posted EQ YES)
            USE-INDEX ord-no NO-LOCK:

          IF oe-boll.p-c THEN DO:
            v-rel-closed = YES.
            LEAVE.
          END.
        END.
      END.

      IF NOT v-rel-closed THEN DO:
        v-lin-closed = NO.
        LEAVE.
      END.
    END.*/
  
     IF NOT v-lin-closed AND
       (oe-ordl.inv-qty  GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
          oe-ordl.is-a-component) AND
       (  oe-ordl.ship-qty GE oe-ordl.qty * (1 - (oe-ordl.under-pct / 100)) OR
          CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})  OR
          (AVAIL itemfg AND NOT itemfg.stocked) ) THEN
       DO:
          RUN oe/cleanrel.p (ROWID(oe-ordl)).

          /*task 10290904*/
          FOR EACH oe-rel WHERE 
              oe-rel.company EQ cocode AND
              oe-rel.ord-no  EQ oe-ordl.ord-no AND
              oe-rel.i-no    EQ oe-ordl.i-no AND
              oe-rel.line    EQ oe-ordl.line
              NO-LOCK:
          
              RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT v-stat).

              IF v-stat NE "C" THEN
              DO:
                 v-rel-open = YES.
                 LEAVE.
              END.
          END.

          IF v-rel-open EQ NO THEN
             v-lin-closed = YES.
       END.
  END.
     
  IF NOT v-lin-closed THEN DO:
     v-ord-closed = NO.
     LEAVE.
  END.

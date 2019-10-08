/* ---------------------------------------------------- oe/close.p 12/96 FWK  */
/* Close/Reopen orders - o/e module                                           */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-recid AS RECID NO-UNDO.
DEF INPUT PARAM ip-close AS LOG NO-UNDO.

{sys/inc/var.i SHARED}

DEF BUFFER b-oe-rell FOR oe-rell.

DEF VAR v-factor       AS   INT NO-UNDO.
DEF VAR v-fin-qty      AS   INT NO-UNDO.
DEF VAR v              AS   INT NO-UNDO.
DEF VAR v-tax-rate     AS   DEC FORMAT ">,>>9.99" NO-UNDO.
DEF VAR v-frt-tax-rate LIKE v-tax-rate NO-UNDO.
DEF VAR print-log      AS   LOG NO-UNDO.
DEF VAR close_date     AS DATE NO-UNDO.
DEF STREAM print-log.


/*print-log = SEARCH('logs/oeclose.log') NE ?.
IF print-log THEN
  OUTPUT STREAM print-log TO VALUE('logs/oeclose.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').*/

DO TRANSACTION:
  {sys/inc/closejob.i OrdClose}
END.

DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF oe-relh.
DISABLE TRIGGERS FOR LOAD OF oe-rell.
DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF oe-boll.
DISABLE TRIGGERS FOR LOAD OF itemfg.

FIND FIRST jc-ctrl WHERE jc-ctrl.company EQ cocode NO-LOCK NO-ERROR.

DO TRANSACTION:
FIND oe-ord WHERE RECID(oe-ord) EQ ip-recid NO-ERROR.
IF NOT AVAIL oe-ord OR oe-ord.stat EQ "D" THEN RETURN.

{oe/closeaud.i oe-ord}
reftable.val[3] = 2.
v-factor = IF ip-close THEN -1 ELSE 1.

FIND FIRST cust
    WHERE cust.company eq oe-ord.company
      AND cust.cust-no eq oe-ord.cust-no
    NO-ERROR.

close_date = TODAY.

FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ oe-ord.company
      AND oe-ordl.ord-no  EQ oe-ord.ord-no:

  IF (ip-close AND oe-ordl.stat NE "C")     OR
     (NOT ip-close AND oe-ordl.stat EQ "C") THEN
    RUN oe/closelin.p (ROWID(oe-ordl), ip-close).

  IF v-close-job GT 0 AND ip-close THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ oe-ordl.company
        AND job-hdr.ord-no  EQ oe-ordl.ord-no
        AND job-hdr.i-no    EQ oe-ordl.i-no
        AND job-hdr.opened  EQ YES
      USE-INDEX ord-no NO-LOCK,

      FIRST job
      WHERE job.company eq job-hdr.company
        AND job.job     eq job-hdr.job
        AND job.job-no  eq job-hdr.job-no
        AND job.job-no2 eq job-hdr.job-no2
      USE-INDEX job:

    {jc/job-clos.i}

    FIND CURRENT reftable NO-LOCK NO-ERROR.
  END.
END. /* for each oe-ordl */

RUN ar/cctaxrt.p (INPUT oe-ord.company, oe-ord.tax-gr,
                  OUTPUT v-tax-rate, OUTPUT v-frt-tax-rate).

/* add/remove misc cost from cust order balance */
FOR EACH oe-ordm OF oe-ord WHERE oe-ordm.bill EQ "Y" NO-LOCK:
  cust.ord-bal = cust.ord-bal + (oe-ordm.cost * v-factor).
  IF oe-ordm.tax THEN
    cust.ord-bal = cust.ord-bal + ((oe-ordm.cost * v-tax-rate / 100) * v-factor).
END.

/* delete any backorder releases */
IF ip-close THEN
FOR EACH oe-rell
    WHERE oe-rell.company  EQ oe-ord.company
      AND oe-rell.ord-no   EQ oe-ord.ord-no
      AND oe-rell.b-ord-no GT 0
      AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
    USE-INDEX ord-no:

  RELEASE oe-boll.

  FOR EACH oe-boll
      WHERE oe-boll.company  EQ cocode
        AND oe-boll.ord-no   EQ oe-rell.ord-no
        AND oe-boll.line     EQ oe-rell.line
        AND oe-boll.i-no     EQ oe-rell.i-no
        AND oe-boll.r-no     EQ oe-rell.r-no
        AND oe-boll.rel-no   EQ oe-rell.rel-no
        AND oe-boll.b-ord-no EQ oe-rell.b-ord-no
        AND oe-boll.po-no    EQ oe-rell.po-no
        AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no)
      USE-INDEX ord-no NO-LOCK:
    LEAVE.
  END.

  IF NOT AVAIL oe-boll THEN DO:

    IF CAN-FIND(FIRST itemfg WHERE
       itemfg.company EQ cocode AND
       itemfg.i-no    EQ oe-rell.i-no) THEN
       REPEAT:
          FIND FIRST itemfg WHERE
               itemfg.company EQ cocode AND
               itemfg.i-no    EQ oe-rell.i-no
               EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
          RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-rell.loc).
          IF AVAIL itemfg THEN
              RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-rell.loc).
              FIND FIRST itemfg-loc 
                WHERE itemfg-loc.company EQ cocode
                  AND itemfg-loc.i-no    EQ oe-rell.i-no
                  AND itemfg-loc.loc     EQ oe-rell.loc
                EXCLUSIVE-LOCK NO-ERROR.
         
          IF AVAIL itemfg THEN DO:
             itemfg.q-back = itemfg.q-back + (oe-rell.qty * v-factor).         

             IF AVAIL itemfg-loc THEN
                 itemfg-loc.q-back = itemfg-loc.q-back + (oe-rell.qty * v-factor).
             IF itemfg.q-back LT 0 THEN DO: 
                 itemfg.q-back = 0.
                 IF AVAIL itemfg-loc THEN
                     itemfg-loc.q-back = 0.
             END.
             FIND CURRENT itemfg NO-LOCK NO-ERROR.
             FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
             LEAVE.
          END.
       END.

    FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-ERROR.

    DELETE oe-rell.

    IF AVAIL oe-relh THEN DO:
      FIND FIRST b-oe-rell
          WHERE b-oe-rell.company EQ oe-relh.company
            AND b-oe-rell.r-no    EQ oe-relh.r-no
          USE-INDEX r-no NO-LOCK NO-ERROR.
      IF NOT AVAIL b-oe-rell THEN DELETE oe-relh.
    END.
  END.
END. /* for each oe-relh record */

oe-ord.stat = IF ip-close THEN "C" ELSE "U".

{oe/closeaud.i oe-ord}
reftable.val[3] = 3.

RUN oe/closkids.p (ROWID(oe-ord)).
END. /* transaction */

RETURN.

PROCEDURE print-log:
  DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

  PUT STREAM print-log UNFORMATTED STRING(TODAY,'99.99.9999') ' '
    STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.
END.

/* end ---------------------------------- copr. 1993  advanced software, inc. */

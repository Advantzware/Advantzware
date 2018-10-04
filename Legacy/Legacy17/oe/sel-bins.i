/* -------------------------------------------------- oe/sel-bins.i 06/99 JLF */
/* -------------------------------------------------------------------------- */

RELEASE {1}l.

IF v-qty LE 0 THEN LEAVE.

IF FIRST(w-bin.seq) THEN
  FIND {1}l WHERE ROWID({1}l) EQ ip-rowid NO-ERROR.

IF NOT AVAIL {1}l THEN DO:
  {oe/{1}l.a}
  BUFFER-COPY x{1}l EXCEPT rec_key TO {1}l.
END.

IF TRIM("{1}") EQ "oe-rel" THEN
DO:
  FIND FIRST b-reftable WHERE
       b-reftable.reftable EQ "oe-rell.lot-no" AND
       b-reftable.rec_key  EQ xoe-rell.rec_key
       USE-INDEX rec_key
       NO-LOCK NO-ERROR.

  IF AVAIL b-reftable THEN DO:

     FIND FIRST reftable WHERE
          reftable.reftable = "oe-rell.lot-no" AND
          reftable.rec_key  = oe-rell.rec_key
          USE-INDEX rec_key
          EXCLUSIVE-LOCK NO-ERROR.

     IF NOT AVAIL reftable THEN
     DO:
        CREATE reftable.
        ASSIGN
           reftable.reftable = "oe-rell.lot-no"
           reftable.rec_key  = oe-rell.rec_key.
     END.
     
     reftable.code = b-reftable.code.
     RELEASE reftable.
     RELEASE b-reftable.
  END.
END.
ELSE IF TRIM("{1}") EQ "oe-bol" THEN
DO:
  FIND FIRST b-reftable WHERE
       b-reftable.reftable EQ "oe-boll.lot-no" AND
       b-reftable.rec_key  EQ STRING(RECID(xoe-boll))
       USE-INDEX rec_key
       NO-LOCK NO-ERROR.

  IF AVAILABLE b-reftable THEN
  DO:
     FIND FIRST reftable WHERE
          reftable.reftable EQ "oe-boll.lot-no" AND
          reftable.rec_key  EQ STRING(RECID(oe-boll))
          USE-INDEX rec_key
          EXCLUSIVE-LOCK NO-ERROR.

     IF NOT AVAIL reftable THEN
     DO:
        CREATE reftable.
        ASSIGN
           reftable.reftable = "oe-boll.lot-no"
           reftable.rec_key = STRING(RECID(oe-boll)).
     END.

     reftable.CODE = b-reftable.CODE.
     RELEASE reftable.
     RELEASE b-reftable.
  END.
END.

ASSIGN
 {1}l.job-no   = fg-bin.job-no
 {1}l.job-no2  = fg-bin.job-no2
 {1}l.loc      = fg-bin.loc
 {1}l.loc-bin  = fg-bin.loc-bin
 {1}l.tag      = fg-bin.tag
 {1}l.cust-no  = fg-bin.cust-no
 {1}l.deleted  = NO
 {1}l.posted   = NO
 {1}l.printed  = NO
 {1}l.qty-case = IF fg-bin.case-count GT 0 THEN fg-bin.case-count
                 ELSE itemfg.case-count
 {1}l.qty      = MIN(v-qty,fg-bin.qty)
 {1}l.partial = fg-bin.partial-count  /*Bug 08291409 - partial being recalcuated*/
 {1}l.cases    = TRUNC(({1}l.qty - {1}l.partial) / {1}l.qty-case,0) 
 v-qty         = v-qty - {1}l.qty.

IF {1}l.cases GT TRUNC((fg-bin.qty - fg-bin.partial-count) / {1}l.qty-case,0) THEN
  {1}l.cases = TRUNC((fg-bin.qty - fg-bin.partial-count) / {1}l.qty-case,0).

{1}l.partial = {1}l.qty - ({1}l.cases * {1}l.qty-case).

IF {1}l.partial GE {1}l.qty-case AND fg-bin.partial-count EQ 0 THEN
  {1}l.cases = TRUNC({1}l.qty / {1}l.qty-case,0).

{1}l.partial = {1}l.qty - ({1}l.cases * {1}l.qty-case).

FIND FIRST reftable
    WHERE reftable.reftable EQ TRIM("{1}") + "l.selected"
      AND reftable.company  EQ fg-bin.company
      AND reftable.loc      EQ fg-bin.i-no
      AND reftable.code     EQ STRING(fg-bin.job-no,"x(10)") +
                               STRING(fg-bin.job-no2,"9999999999")
      AND reftable.code2    EQ STRING(fg-bin.loc,"x(10)")     +
                               STRING(fg-bin.loc-bin,"x(10)") +
                               STRING(fg-bin.tag,"x(40)")     +
                               STRING(fg-bin.cust-no,"x(10)")
      AND reftable.rec_key  EQ {1}l.rec_key 
    NO-ERROR.
IF NOT AVAIL reftable THEN DO:
  CREATE reftable.
  ASSIGN
   reftable.reftable = TRIM("{1}") + "l.selected"
   reftable.company  = fg-bin.company
   reftable.loc      = fg-bin.i-no
   reftable.code     = STRING(fg-bin.job-no,"x(10)") +
                       STRING(fg-bin.job-no2,"9999999999")
   reftable.code2    = STRING(fg-bin.loc,"x(10)")     +
                       STRING(fg-bin.loc-bin,"x(10)") +
                       STRING(fg-bin.tag,"x(40)")     +
                       STRING(fg-bin.cust-no,"x(10)")
   reftable.rec_key  = {1}l.rec_key.
END.
reftable.val[1] = 1.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

/* -------------------------------------------------- oe/sel-binsrel.i  */
/* -------------------------------------------------------------------- */

RELEASE {1}l.

IF v-qty LE 0 THEN LEAVE.

IF FIRST(w-bin.seq) THEN
  FIND {1}l WHERE ROWID({1}l) EQ ip-rowid NO-ERROR.

IF NOT AVAIL {1}l THEN DO:
  {oe/{1}l.ab}
  BUFFER-COPY x{1}l EXCEPT rec_key TO {1}l.
END.

IF TRIM("{1}") EQ "oe-rel" THEN
DO:
  ASSIGN oe-rell.lot-no = xoe-rell.lot-no.
END.
ELSE IF TRIM("{1}") EQ "oe-bol" THEN
DO:
  ASSIGN oe-boll.lot-no = xoe-boll.lot-no.
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
 {1}l.qty      = MIN(v-qty,fg-bin.qty)
 v-qty         = v-qty - {1}l.qty.

 IF ll-update-qty-no = NO THEN
    ASSIGN
       {1}l.qty-case = IF fg-bin.case-count GT 0 THEN fg-bin.case-count
                       ELSE itemfg.case-count
       {1}l.cases = TRUNC((fg-bin.qty - fg-bin.partial-count) / {1}l.qty-case,0)
       {1}l.partial = fg-bin.qty - ({1}l.cases * {1}l.qty-case).

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

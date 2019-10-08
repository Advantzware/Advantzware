&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-ordl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-inv-line FOR inv-line.
DEF BUFFER b-job-hdr FOR job-hdr.
DEF BUFFER oe-ordl-q-no FOR reftable.
DEF BUFFER oe-ordl-whs-item FOR reftable.
DEF BUFFER orderpo FOR reftable.
DEF BUFFER b-prep FOR prep.
DEF BUFFER b-oe-rel FOR oe-rel.

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-freight LIKE {&TABLENAME}.t-freight NO-UNDO.
DEF VAR xInvQty LIKE {&TABLENAME}.inv-qty NO-UNDO.
DEF VAR oeDateAuto-log  AS LOG  NO-UNDO.
DEF VAR oeDateAuto-char AS CHAR NO-UNDO.
DEF    VAR      cRtnChar  AS CHAR    NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
RUN sys/ref/nk1look.p (INPUT {&TABLENAME}.company, "OEDATEAUTO", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeDateAuto-log = LOGICAL(cRtnChar) NO-ERROR.

RUN sys/ref/nk1look.p (INPUT {&TABLENAME}.company, "OEDATEAUTO", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    oeDateAuto-char = cRtnChar NO-ERROR. 
    
{custom/globdefs.i}


DISABLE TRIGGERS FOR LOAD OF job-hdr.
DISABLE TRIGGERS FOR LOAD OF oe-ord.
DISABLE TRIGGERS FOR LOAD OF oe-ordm.
DISABLE TRIGGERS FOR LOAD OF oe-rel.
DISABLE TRIGGERS FOR LOAD OF oe-rell.
DISABLE TRIGGERS FOR LOAD OF oe-boll.
DISABLE TRIGGERS FOR LOAD OF inv-line.
DISABLE TRIGGERS FOR LOAD OF oe-ordl-q-no.
DISABLE TRIGGERS FOR LOAD OF itemfg.

{sys/inc/var.i NEW SHARED}


ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/notes.i "do not need transaction block"}
{sys/inc/oeuserid.i}
DEF BUFFER bf-shipto FOR shipto.
IF {&TABLENAME}.line LT 1 OR {&TABLENAME}.line GE 99999999 THEN {&TABLENAME}.i-no = "".

/*#PN 2 options to solve for 05281303*/
/*#PN 1 Modifying the oe/ordlsqty.p program*/
/*#PN Pro - centralized, better calculation for inv-qty*/
/*#PN Con - additional processing time for every oe-ordl write*/
/*#PN 2 Changing the ordlsqty.p call here so that it doesn't change inv-qty*/
/*#PN Pro - No additional processing*/
/*#PN Con - inv-qty can get overridden by other calls of ordsqty*/
/*#PN Con - inv-qty can get out of whack if invoice deleted*/
/* RUN oe/ordlsqty.p (ROWID({&TABLENAME}), OUTPUT xInvQty , OUTPUT {&TABLENAME}.ship-qty). */
RUN oe/ordlsqty.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.inv-qty, OUTPUT {&TABLENAME}.ship-qty).
{&TABLENAME}.t-ship-qty = {&TABLENAME}.ship-qty.

RUN oe/rell-qty.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.t-rel-qty).

IF {&TABLENAME}.po-no-po NE 0 THEN DO:
  FIND FIRST po-ord NO-LOCK
      WHERE po-ord.company EQ {&TABLENAME}.company
        AND po-ord.po-no   EQ {&TABLENAME}.po-no-po
      NO-ERROR.
  IF AVAIL po-ord THEN {&TABLENAME}.vend-no = po-ord.vend-no.
END.

FOR EACH oe-ord OF {&TABLENAME} NO-LOCK:
  ASSIGN
   {&TABLENAME}.opened  = oe-ord.opened
   {&TABLENAME}.cust-no = oe-ord.cust-no.
  LEAVE.
END.
IF NOT {&TABLENAME}.opened THEN {&TABLENAME}.stat = "C".

IF {&TABLENAME}.ord-no NE 0 THEN DO:
  FOR EACH b-job-hdr NO-LOCK
      WHERE b-job-hdr.company  EQ {&TABLENAME}.company
        AND b-job-hdr.ord-no   EQ {&TABLENAME}.ord-no
        AND b-job-hdr.i-no     EQ {&TABLENAME}.i-no:
    FIND job-hdr WHERE ROWID(job-hdr) EQ ROWID(b-job-hdr) EXCLUSIVE NO-ERROR NO-WAIT.
    IF AVAIL job-hdr THEN DO:
      job-hdr.due-date = {&TABLENAME}.req-date.
      RUN jc/duedates.p (ROWID(job-hdr)).
    END.
  END.

  ll = NOT CAN-FIND(FIRST b-{&TABLENAME}
                    WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                      AND b-{&TABLENAME}.ord-no  EQ {&TABLENAME}.ord-no
                      AND b-{&TABLENAME}.i-no    EQ {&TABLENAME}.i-no
                      AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})).

  IF {&TABLENAME}.line NE 0                             AND
     old-{&TABLENAME}.line NE 0                         AND
     ({&TABLENAME}.line NE old-{&TABLENAME}.line OR
      {&TABLENAME}.i-no NE old-{&TABLENAME}.i-no OR ll) THEN DO:

   RUN oe/lineUpdate.p (INPUT   ROWID({&TABLENAME}),
                     INPUT   old-{&TABLENAME}.i-no,
                     INPUT   old-{&TABLENAME}.LINE,
                     INPUT   ll).

  END.
END.

FIND FIRST itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.i-no    EQ {&TABLENAME}.i-no
    NO-LOCK NO-ERROR.

IF {&TABLENAME}.i-no NE ""                              AND
   {&TABLENAME}.i-no NE "0"                             AND
   {&TABLENAME}.qty NE 0                                AND
   {&TABLENAME}.pr-uom NE ""                            AND
   old-{&TABLENAME}.i-no NE ""                          AND
   AVAIL itemfg                                         AND
   ({&TABLENAME}.qty      NE old-{&TABLENAME}.qty      OR
    {&TABLENAME}.req-date NE old-{&TABLENAME}.req-date OR
    {&TABLENAME}.price    NE old-{&TABLENAME}.price    OR
    {&TABLENAME}.pr-uom   NE old-{&TABLENAME}.pr-uom)     THEN

  RUN fg/makenote.p (BUFFER {&TABLENAME},
                     BUFFER quoteqty,
                     BUFFER ar-invl,
                     NO,
                     itemfg.rec_key).

IF AVAIL itemfg                                         AND
   {&TABLENAME}.type-code NE "T"                        AND
   {&TABLENAME}.type-code NE old-{&TABLENAME}.type-code THEN DO:
  FIND CURRENT itemfg.
  itemfg.type-code = {&TABLENAME}.type-code.
END.

IF NOT {&TABLENAME}.is-a-component THEN {&TABLENAME}.set-hdr-line = {&TABLENAME}.line.

IF {&TABLENAME}.i-no NE "" OR {&TABLENAME}.ord-no NE 0 THEN
FOR EACH b-inv-line
    WHERE b-inv-line.company EQ {&TABLENAME}.company
      AND b-inv-line.ord-no  EQ {&TABLENAME}.ord-no
      AND b-inv-line.i-no    EQ {&TABLENAME}.i-no
      AND b-inv-line.line    EQ {&TABLENAME}.line
    NO-LOCK:

  DO lj = 1 TO 100:
    FIND inv-line WHERE ROWID(inv-line) EQ ROWID(b-inv-line) EXCLUSIVE-LOCK NO-WAIT NO-ERROR.

    IF AVAIL inv-line THEN DO:
      DO li = 1 TO EXTENT({&TABLENAME}.s-man):
        ASSIGN
         inv-line.sman[li]   = {&TABLENAME}.s-man[li]
         inv-line.s-comm[li] = {&TABLENAME}.s-comm[li]
         inv-line.s-pct[li]  = {&TABLENAME}.s-pct[li].
      END.

      LEAVE.
    END.
  END.
END.

IF {&TABLENAME}.req-date NE old-{&TABLENAME}.req-date THEN DO:
  FOR EACH oe-rel
      WHERE oe-rel.company EQ {&TABLENAME}.company
        AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no
        AND oe-rel.i-no    EQ {&TABLENAME}.i-no
        AND oe-rel.line    EQ {&TABLENAME}.line
      NO-LOCK
      BY oe-rel.rel-date:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
    IF INDEX("SLI",lv-stat) GT 0 THEN DO:

      FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ ROWID(oe-rel) EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

      IF AVAIL b-oe-rel THEN
      DO:
          FIND FIRST oe-ord WHERE oe-ord.company EQ {&TABLENAME}.company
           AND oe-ord.ord-no EQ {&TABLENAME}.ord-no
           NO-LOCK NO-ERROR.
          
          IF  AVAIL oe-ord AND NOT (oeDateAuto-log AND oeDateAuto-char = "Colonial") THEN DO:

             b-oe-rel.rel-date = {&TABLENAME}.req-date.
             RELEASE b-oe-rel.
             
         END.
      END.

      LEAVE.
    END.
  END.

  IF {&TABLENAME}.opened THEN
  FOR EACH job-hdr
      WHERE job-hdr.company EQ {&TABLENAME}.company
        AND job-hdr.ord-no  EQ {&TABLENAME}.ord-no:

    IF job-hdr.i-no EQ {&TABLENAME}.i-no                                   OR
       (job-hdr.job-no  EQ {&TABLENAME}.job-no  AND
        job-hdr.job-no2 EQ {&TABLENAME}.job-no2 AND
        NOT CAN-FIND(FIRST b-{&TABLENAME}
                     WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                       AND b-{&TABLENAME}.ord-no  EQ {&TABLENAME}.ord-no
                       AND b-{&TABLENAME}.job-no  EQ {&TABLENAME}.job-no
                       AND b-{&TABLENAME}.job-no2 EQ {&TABLENAME}.job-no2
                       AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME}))) THEN
      job-hdr.due-date = oe-ordl.req-date.
  END.
END.

IF TRIM({&TABLENAME}.est-no) NE "" THEN
FOR EACH eb NO-LOCK
    WHERE eb.company  EQ {&TABLENAME}.company 
      AND eb.est-no   EQ {&TABLENAME}.est-no
      AND eb.stock-no EQ {&TABLENAME}.i-no:

  IF TRIM(eb.die-no) NE "" THEN
  FOR EACH prep NO-LOCK
      WHERE prep.company    EQ eb.company
        AND prep.code       EQ eb.die-no
        AND prep.last-order LT {&TABLENAME}.ord-no:
    FIND b-prep WHERE ROWID(b-prep) EQ ROWID(prep) EXCLUSIVE NO-ERROR NO-WAIT.
    /* gdm - 06170905 */
    IF AVAIL b-prep THEN DO:      

      ASSIGN b-prep.last-order = {&TABLENAME}.ord-no.

      /*FIND FIRST reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable EQ "PREPLASTJOB"
          AND reftable.company  EQ b-prep.company 
          AND reftable.loc      EQ b-prep.loc     
          AND reftable.code     EQ b-prep.CODE NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN reftable.reftable = "PREPLASTJOB"
                 reftable.company  = b-prep.company
                 reftable.loc      = b-prep.loc
                 reftable.code     = b-prep.CODE 
                 reftable.code2    = {&TABLENAME}.job-no
                 reftable.val[1]   = {&TABLENAME}.job-no2.
      END.
      ELSE
      IF AVAIL reftable THEN DO:

          ASSIGN reftable.code2    = {&TABLENAME}.job-no
                 reftable.val[1]   = {&TABLENAME}.job-no2.
      END.
      RELEASE reftable.*/
      RELEASE b-prep.
    END. /* if avail b-prep */
    /* gdm - 06170905 end. */

  END. /* for each prep */

  IF TRIM(eb.plate-no) NE "" THEN
  FOR EACH prep NO-LOCK
      WHERE prep.company    EQ eb.company
        AND prep.code       EQ eb.plate-no
        AND prep.last-order LT {&TABLENAME}.ord-no:
    FIND b-prep WHERE ROWID(b-prep) EQ ROWID(prep) EXCLUSIVE NO-ERROR NO-WAIT.
    /* gdm - 06170905 */
    IF AVAIL b-prep THEN DO:      

      ASSIGN b-prep.last-order = {&TABLENAME}.ord-no.

      FIND FIRST reftable EXCLUSIVE-LOCK
        WHERE reftable.reftable EQ "PREPLASTJOB"
          AND reftable.company  EQ b-prep.company 
          AND reftable.loc      EQ b-prep.loc     
          AND reftable.code     EQ b-prep.CODE NO-ERROR.
      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN reftable.reftable = "PREPLASTJOB"
                 reftable.company  = b-prep.company
                 reftable.loc      = b-prep.loc
                 reftable.code     = b-prep.CODE 
                 reftable.code2    = {&TABLENAME}.job-no
                 reftable.val[1]   = {&TABLENAME}.job-no2.
      END.
      ELSE
      IF AVAIL reftable THEN DO:

          ASSIGN reftable.code2    = {&TABLENAME}.job-no
                 reftable.val[1]   = {&TABLENAME}.job-no2.
      END.
      RELEASE reftable.
      RELEASE b-prep.
    END. /* if avail b-prep */
    /* gdm - 06170905 end. */
  END.

  LEAVE.
END.

FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no:

  IF oeuserid-log                        AND
     oe-ord.user-id NE USERID("nosweat") THEN
    oe-ord.user-id = USERID("nosweat").

  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

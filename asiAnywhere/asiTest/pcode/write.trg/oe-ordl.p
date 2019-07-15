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

DEF VAR li AS INT NO-UNDO.
DEF VAR lj AS INT NO-UNDO.
DEF VAR ll AS LOG NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.
DEF VAR lv-stat AS CHAR NO-UNDO.
DEF VAR lv-freight LIKE {&TABLENAME}.t-freight NO-UNDO.
/*DEF VAR g_company AS CHAR NO-UNDO.
DEF VAR g_loc AS CHAR NO-UNDO.*/
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

IF {&TABLENAME}.line LT 1 OR {&TABLENAME}.line GE 99999999 THEN {&TABLENAME}.i-no = "".

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

    RELEASE oe-ordm.
    DO WHILE TRUE:
      FIND NEXT oe-ordm EXCLUSIVE
          WHERE oe-ordm.company    EQ {&TABLENAME}.company
            AND oe-ordm.ord-no     EQ {&TABLENAME}.ord-no
            AND oe-ordm.ord-i-no   EQ old-{&TABLENAME}.i-no
            AND (oe-ordm.ord-line  EQ old-{&TABLENAME}.line OR
                 (oe-ordm.ord-line NE {&TABLENAME}.line AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-ordm THEN
        ASSIGN
         oe-ordm.ord-i-no = {&TABLENAME}.i-no
         oe-ordm.ord-line = {&TABLENAME}.line.

      ELSE LEAVE.
    END.

    RELEASE oe-rel.
    DO WHILE TRUE:
      FIND NEXT oe-rel EXCLUSIVE
          WHERE oe-rel.company EQ {&TABLENAME}.company
            AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no
            AND oe-rel.i-no    EQ old-{&TABLENAME}.i-no
            AND (oe-rel.line   EQ old-{&TABLENAME}.line OR
                 (oe-rel.line  NE {&TABLENAME}.line AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-rel THEN
        ASSIGN
         oe-rel.i-no = {&TABLENAME}.i-no
         oe-rel.line = {&TABLENAME}.line.

      ELSE LEAVE.
    END.

    RELEASE oe-rell.
    DO WHILE TRUE:
      FIND NEXT oe-rell EXCLUSIVE
          WHERE oe-rell.company EQ {&TABLENAME}.company
            AND oe-rell.ord-no  EQ {&TABLENAME}.ord-no
            AND oe-rell.i-no    EQ old-{&TABLENAME}.i-no
            AND (oe-rell.line   EQ old-{&TABLENAME}.line OR
                 (oe-rell.line  NE {&TABLENAME}.line AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-rell THEN
        ASSIGN
         oe-rell.i-no = {&TABLENAME}.i-no
         oe-rell.line = {&TABLENAME}.line.

      ELSE LEAVE.
    END.

    RELEASE oe-boll.
    DO WHILE TRUE:
      FIND NEXT oe-boll EXCLUSIVE
          WHERE oe-boll.company EQ {&TABLENAME}.company
            AND oe-boll.ord-no  EQ {&TABLENAME}.ord-no
            AND oe-boll.i-no    EQ old-{&TABLENAME}.i-no
            AND (oe-boll.line   EQ old-{&TABLENAME}.line OR
                 (oe-boll.line  NE {&TABLENAME}.line AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-boll THEN
        ASSIGN
         oe-boll.i-no = {&TABLENAME}.i-no
         oe-boll.line = {&TABLENAME}.line.

      ELSE LEAVE.
    END.

    RELEASE inv-line.
    DO WHILE TRUE:
      FIND NEXT inv-line EXCLUSIVE
          WHERE inv-line.company EQ {&TABLENAME}.company
            AND inv-line.ord-no  EQ {&TABLENAME}.ord-no
            AND inv-line.i-no    EQ old-{&TABLENAME}.i-no
            AND (inv-line.line   EQ old-{&TABLENAME}.line OR
                 (inv-line.line  NE {&TABLENAME}.line AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL inv-line THEN
        ASSIGN
         inv-line.i-no = {&TABLENAME}.i-no
         inv-line.line = {&TABLENAME}.line.

      ELSE LEAVE.
    END.

    RELEASE oe-ordl-q-no.
    DO WHILE TRUE:
      FIND NEXT oe-ordl-q-no EXCLUSIVE
          WHERE oe-ordl-q-no.reftable EQ "oe-ordl.q-no"
            AND oe-ordl-q-no.company  EQ {&TABLENAME}.company
            AND oe-ordl-q-no.loc      EQ STRING({&TABLENAME}.ord-no,"9999999999")
            AND oe-ordl-q-no.code     EQ old-{&TABLENAME}.i-no
            AND (oe-ordl-q-no.code2   EQ STRING(old-{&TABLENAME}.line,"9999999999") OR
                 (oe-ordl-q-no.code2  NE STRING({&TABLENAME}.line,"9999999999") AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-ordl-q-no THEN
        ASSIGN
         oe-ordl-q-no.code  = {&TABLENAME}.i-no
         oe-ordl-q-no.code2 = STRING({&TABLENAME}.line,"9999999999").

      ELSE LEAVE.
    END.

    RELEASE oe-ordl-whs-item.
    DO WHILE TRUE:
      FIND NEXT oe-ordl-whs-item EXCLUSIVE
          WHERE oe-ordl-whs-item.reftable EQ "oe-ordl.whs-item"
            AND oe-ordl-whs-item.company  EQ {&TABLENAME}.company
            AND oe-ordl-whs-item.loc      EQ STRING({&TABLENAME}.ord-no,"9999999999")
            AND oe-ordl-whs-item.code     EQ old-{&TABLENAME}.i-no
            AND (oe-ordl-whs-item.code2   EQ STRING(old-{&TABLENAME}.line,"9999999999") OR
                 (oe-ordl-whs-item.code2  NE STRING({&TABLENAME}.line,"9999999999") AND ll))
          NO-ERROR NO-WAIT.

      IF AVAIL oe-ordl-whs-item THEN
        ASSIGN
         oe-ordl-whs-item.code  = {&TABLENAME}.i-no
         oe-ordl-whs-item.code2 = STRING({&TABLENAME}.line,"9999999999").

      ELSE LEAVE.
    END.
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
      BY oe-rel.rel-date:
    RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).
    IF INDEX("SLI",lv-stat) GT 0 THEN DO:
      oe-rel.rel-date = {&TABLENAME}.req-date.
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
    IF AVAIL b-prep THEN b-prep.last-order = {&TABLENAME}.ord-no.
  END.

  IF TRIM(eb.plate-no) NE "" THEN
  FOR EACH prep NO-LOCK
      WHERE prep.company    EQ eb.company
        AND prep.code       EQ eb.plate-no
        AND prep.last-order LT {&TABLENAME}.ord-no:
    FIND b-prep WHERE ROWID(b-prep) EQ ROWID(prep) EXCLUSIVE NO-ERROR NO-WAIT.
    IF AVAIL b-prep THEN b-prep.last-order = {&TABLENAME}.ord-no.
  END.

  LEAVE.
END.

FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no:

 /* IF oeuserid-log                        AND
     oe-ord.user-id NE USERID("nosweat") THEN
    oe-ord.user-id = USERID("nosweat").*/

  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

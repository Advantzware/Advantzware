&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME fg-rctd

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.
DEF VAR cocode AS CHAR NO-UNDO.
DEF VAR lv-r-no AS INT NO-UNDO.

{methods/triggers/write.i}

IF old-fg-rctd.r-no EQ 0 AND  fg-rctd.r-no NE 0 THEN DO: 
  /* If this is a new record, reftable should not exist for this */
  /* r-no, so delete any old ones that do exist                  */
  FIND FIRST reftable
    WHERE reftable.reftable EQ "fg-rctd.user-id"
      AND reftable.company  EQ fg-rctd.company
      AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
    EXCLUSIVE-LOCK NO-ERROR.
  IF AVAIL reftable THEN
    DELETE reftable.
END.

/* Obtain fgpofrt nk1 value for ext-cost calculation */
cocode = {&TABLENAME}.company.
{sys/inc/fgpofrt.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR ll AS LOG NO-UNDO.
DEF VAR li AS INT NO-UNDO.
DEF VAR li1 AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

{&TABLENAME}.po-no = TRIM(STRING(INT({&TABLENAME}.po-no),">>>>>>>>>>")).

IF ({&TABLENAME}.cases * {&TABLENAME}.qty-case) NE 0 OR
   {&TABLENAME}.partial NE 0                         THEN
 {&TABLENAME}.t-qty = ({&TABLENAME}.cases * {&TABLENAME}.qty-case) +
                      {&TABLENAME}.partial.

{&TABLENAME}.qty = {&TABLENAME}.t-qty.

IF {&TABLENAME}.job-no NE ""                   AND
   ({&TABLENAME}.rita-code EQ "R" OR
    CAN-FIND(FIRST fg-rcpth
             WHERE fg-rcpth.r-no      EQ {&TABLENAME}.r-no
               AND fg-rcpth.i-no      EQ {&TABLENAME}.i-no
               AND fg-rcpth.job-no    EQ {&TABLENAME}.job-no
               AND fg-rcpth.job-no2   EQ {&TABLENAME}.job-no2
               AND fg-rcpth.rita-code EQ "R")) THEN DO:
  FIND FIRST job-hdr NO-LOCK
      WHERE job-hdr.company EQ {&TABLENAME}.company
        AND job-hdr.i-no    EQ {&TABLENAME}.i-no
        AND job-hdr.job-no  EQ {&TABLENAME}.job-no
        AND job-hdr.job-no2 EQ {&TABLENAME}.job-no2
      NO-ERROR.

  IF NOT AVAIL job-hdr THEN DO:
    FIND FIRST job NO-LOCK
        WHERE job.company EQ {&TABLENAME}.company
          AND job.job-no  EQ {&TABLENAME}.job-no
          AND job.job-no2 EQ {&TABLENAME}.job-no2
        NO-ERROR.
    IF AVAIL job THEN
    FIND FIRST reftable NO-LOCK
        WHERE reftable.reftable EQ "jc/jc-calc.p"
          AND reftable.company  EQ job.company
          AND reftable.loc      EQ ""
          AND reftable.code     EQ STRING(job.job,"999999999")
          AND reftable.code2    EQ {&TABLENAME}.i-no
        NO-ERROR.
  END.
       
  IF AVAIL job-hdr AND job-hdr.std-tot-cost GT 0 THEN
    ASSIGN
     {&TABLENAME}.pur-uom  = "M"
     {&TABLENAME}.cost-uom = "M"
     {&TABLENAME}.std-cost = job-hdr.std-tot-cost.
  ELSE
  IF AVAIL reftable AND reftable.val[5] GT 0 THEN
    ASSIGN
     {&TABLENAME}.pur-uom  = "M"
     {&TABLENAME}.cost-uom = "M"
     {&TABLENAME}.std-cost = reftable.val[5].
END.

IF {&TABLENAME}.std-cost NE 0 THEN DO:
  {&TABLENAME}.ext-cost = {&TABLENAME}.std-cost *
                          ({&TABLENAME}.t-qty / IF {&TABLENAME}.cost-uom EQ "M" THEN 1000 ELSE 1).
  IF fgpofrt-log THEN 
  DO:

     {&TABLENAME}.ext-cost = {&TABLENAME}.ext-cost + {&TABLENAME}.frt-cost.


  END.
END.
ELSE
IF {&TABLENAME}.ext-cost NE 0 THEN
  {&TABLENAME}.std-cost = {&TABLENAME}.ext-cost /
                          ({&TABLENAME}.t-qty / IF {&TABLENAME}.cost-uom EQ "M" THEN 1000 ELSE 1).

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.r-no NE 0 THEN DO:
  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.r-no   EQ {&TABLENAME}.r-no
        AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME}):
    BUFFER-COMPARE b-{&TABLENAME} EXCEPT rec_key TO {&TABLENAME} SAVE RESULT IN ll.
    IF ll THEN DELETE b-{&TABLENAME}.
  END.

  li1 = 0.
  IF {&TABLENAME}.rita-code NE "P" THEN
  DO WHILE CAN-FIND(FIRST b-{&TABLENAME}
                    WHERE b-{&TABLENAME}.r-no   EQ {&TABLENAME}.r-no
                      AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME})) OR
           CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ {&TABLENAME}.r-no):
    ASSIGN
     li  = 0
     li1 = li1 + 1.

    IF li1 GE 1000 THEN LEAVE.

    FIND LAST b-{&TABLENAME} USE-INDEX {&TABLENAME} NO-LOCK NO-ERROR.
    IF AVAIL b-{&TABLENAME} AND b-{&TABLENAME}.r-no GT li THEN li = b-{&TABLENAME}.r-no.

    FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth AND fg-rcpth.r-no GT li THEN li = fg-rcpth.r-no.
    lv-r-no = {&TABLENAME}.r-no.
    {&TABLENAME}.r-no = li + 1.

    /* Change the r-no in related tables */
    FOR EACH fg-rcpts 
       WHERE fg-rcpts.company EQ {&TABLENAME}.company 
         AND fg-rcpts.linker EQ "fg-rctd: " + STRING(lv-r-no,"9999999999") 
       EXCLUSIVE-LOCK:
  
      fg-rcpts.linker = "fg-rctd: " + STRING({&TABLENAME}.r-no,"9999999999").
      FOR EACH reftable 
        WHERE reftable.reftable EQ "fg-rctd.user-id"
          AND reftable.company  EQ fg-rcpts.company
          AND reftable.loc      EQ STRING(fg-rcpts.r-no,"9999999999")        /* component */
          AND (reftable.dscr EQ "fg-rctd: " + STRING(lv-r-no, "9999999999")  /* set header r-no */
               AND reftable.dscr begins "fg-rctd: ")  
        USE-INDEX loc   EXCLUSIVE-LOCK:
        reftable.dscr = "fg-rctd: " + STRING({&TABLENAME}.r-no, "9999999999").
      END. /* each reftable */
    END. /* each fg-rcpts */
  END.

  FIND FIRST reftable
      WHERE reftable.reftable EQ "fg-rctd.user-id"
        AND reftable.company  EQ {&TABLENAME}.company
        AND reftable.loc      EQ STRING({&TABLENAME}.r-no,"9999999999")
      NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "fg-rctd.user-id"
     reftable.company  = {&TABLENAME}.company
     reftable.loc      = STRING({&TABLENAME}.r-no,"9999999999")
     reftable.code     = USERID("nosweat").
  END.

  {&TABLENAME}.updated-by = USERID("nosweat").

  ASSIGN
   reftable.code2        = USERID("nosweat")
   {&TABLENAME}.upd-date = TODAY
   {&TABLENAME}.upd-time = TIME.

  FIND FIRST fg-rcpts NO-LOCK
      WHERE fg-rcpts.r-no   EQ {&TABLENAME}.r-no
        AND fg-rcpts.linker BEGINS "fg-rctd: "
      NO-ERROR.
  IF AVAIL fg-rcpts THEN reftable.dscr = fg-rcpts.linker.
END.

IF {&TABLENAME}.created-by EQ "" THEN
  {&TABLENAME}.created-by = USERID("nosweat").

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

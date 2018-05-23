&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME fg-bin

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

DEF VAR li AS INT NO-UNDO.
    
{sys/inc/var.i NEW SHARED}

{methods/triggers/write.i}


DISABLE TRIGGERS FOR LOAD OF loadtag.    

cocode = {&TABLENAME}.company. 

IF {&TABLENAME}.case-count LT 0 THEN
  {&TABLENAME}.case-count = {&TABLENAME}.case-count * -1.

/*  Removing for 28873 - do not know the purpose of this code calculation          */
/*IF {&TABLENAME}.case-count NE old-{&TABLENAME}.case-count AND                    */
/*   old-{&TABLENAME}.case-count EQ old-{&TABLENAME}.qty    THEN                   */
/*  {&TABLENAME}.partial-count = {&TABLENAME}.partial-count + old-{&TABLENAME}.qty.*/

IF {&TABLENAME}.qty EQ 0 THEN
  ASSIGN
   {&TABLENAME}.partial-count = 0
   {&TABLENAME}.partial-total = 0.

ELSE DO:
  IF {&TABLENAME}.case-count GT 0 THEN DO:
    li = TRUNC(({&TABLENAME}.qty - {&TABLENAME}.partial-count) / {&TABLENAME}.case-count,0).
   
    IF {&TABLENAME}.partial-count LT 0                     AND
       {&TABLENAME}.qty GT {&TABLENAME}.partial-count * -1 THEN
      li = TRUNC({&TABLENAME}.qty / {&TABLENAME}.case-count,0).
   
    {&TABLENAME}.partial-count = {&TABLENAME}.qty - (li * {&TABLENAME}.case-count).
  END.

  IF {&TABLENAME}.case-count LE 0                       OR
     ({&TABLENAME}.qty LT {&TABLENAME}.case-count AND
      {&TABLENAME}.qty GT 0)                            THEN
    ASSIGN
    {&TABLENAME}.case-count    = {&TABLENAME}.qty
    {&TABLENAME}.partial-count = 0
    {&TABLENAME}.partial-total = 0.
END.

IF {&TABLENAME}.case-count LT 0 THEN
  {&TABLENAME}.case-count = {&TABLENAME}.case-count * -1.

/*IF {&TABLENAME}.i-no NE ""                                          AND
   ({&TABLENAME}.std-tot-cost NE old-{&TABLENAME}.std-tot-cost OR
    {&TABLENAME}.std-mat-cost NE old-{&TABLENAME}.std-mat-cost OR
    {&TABLENAME}.std-lab-cost NE old-{&TABLENAME}.std-lab-cost OR
    {&TABLENAME}.std-fix-cost NE old-{&TABLENAME}.std-fix-cost OR
    {&TABLENAME}.std-var-cost NE old-{&TABLENAME}.std-var-cost)     THEN
  RUN fg/updfgcst.p ({&TABLENAME}.i-no).*/

IF cocode NE ""                                                         AND
   ({&TABLENAME}.std-tot-cost EQ old-{&TABLENAME}.std-tot-cost * 1000 OR
    {&TABLENAME}.std-tot-cost EQ old-{&TABLENAME}.std-tot-cost / 1000)  THEN
  IF TRIM({&TABLENAME}.tag) NE "" THEN
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company     EQ {&TABLENAME}.company
        AND fg-rdtlh.loc         EQ {&TABLENAME}.loc
        AND fg-rdtlh.tag         EQ {&TABLENAME}.tag
        AND fg-rdtlh.loc-bin     EQ {&TABLENAME}.loc-bin
        AND fg-rdtlh.cust-no     EQ {&TABLENAME}.cust-no
        AND (fg-rdtlh.cost       EQ 0 OR
             fg-rdtlh.cost       EQ ?)
      USE-INDEX tag,
    
      FIRST fg-rcpth
      WHERE fg-rcpth.r-no    EQ fg-rdtlh.r-no
        AND fg-rcpth.i-no    EQ {&TABLENAME}.i-no
        AND fg-rcpth.job-no  EQ {&TABLENAME}.job-no
        AND fg-rcpth.job-no2 EQ {&TABLENAME}.job-no2
      USE-INDEX r-no NO-LOCK:

    /* WFK- Per Joe, cost should never be recalculated from bin cost */
    /* IF {&TABLENAME}.pur-uom EQ "M" THEN */
      fg-rdtlh.cost = {&TABLENAME}.std-tot-cost.
    /*
    ELSE
      RUN sys/ref/convcuom.p({&TABLENAME}.pur-uom, "M", 0, 0, 0, 0,
                             {&TABLENAME}.std-tot-cost,
                             OUTPUT fg-rdtlh.cost). */
  END.

  ELSE
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ {&TABLENAME}.company
        AND fg-rcpth.i-no      EQ {&TABLENAME}.i-no
        AND fg-rcpth.job-no    EQ {&TABLENAME}.job-no
        AND fg-rcpth.job-no2   EQ {&TABLENAME}.job-no2
      USE-INDEX i-no NO-LOCK,

      FIRST fg-rdtlh
      WHERE fg-rdtlh.r-no        EQ fg-rcpth.r-no
        AND fg-rdtlh.loc         EQ {&TABLENAME}.loc
        AND fg-rdtlh.loc-bin     EQ {&TABLENAME}.loc-bin
        AND fg-rdtlh.tag         EQ {&TABLENAME}.tag
        AND fg-rdtlh.cust-no     EQ {&TABLENAME}.cust-no
        AND (fg-rdtlh.cost       EQ 0 OR
             fg-rdtlh.cost       EQ ?)
      USE-INDEX rm-rdtl:

    /* WFK- Per Joe, cost should never be recalculated from bin cost */
    /* IF {&TABLENAME}.pur-uom EQ "M" THEN */
      fg-rdtlh.cost = {&TABLENAME}.std-tot-cost.
      /*
    ELSE
      RUN sys/ref/convcuom.p({&TABLENAME}.pur-uom, "M", 0, 0, 0, 0,
                             {&TABLENAME}.std-tot-cost,
                             OUTPUT fg-rdtlh.cost). */
  END.

/*
IF {&TABLENAME}.cases-unit EQ 0                   AND
   NOT CAN-FIND(FIRST loadtag
                WHERE loadtag.company     EQ {&TABLENAME}.company
                  AND loadtag.item-type   EQ NO
                  AND loadtag.i-no        EQ {&TABLENAME}.i-no
                  AND loadtag.tag-no      EQ {&TABLENAME}.tag
                  AND loadtag.is-case-tag EQ NO) THEN
  IF TRIM({&TABLENAME}.tag) NE "" THEN
  FOR EACH fg-rdtlh
      WHERE fg-rdtlh.company     EQ {&TABLENAME}.company
        AND fg-rdtlh.loc         EQ {&TABLENAME}.loc
        AND fg-rdtlh.tag         EQ {&TABLENAME}.tag
        AND fg-rdtlh.loc-bin     EQ {&TABLENAME}.loc-bin
        AND fg-rdtlh.stacks-unit GT 0
        AND fg-rdtlh.rita-code   EQ "R"
      USE-INDEX tag,
    
      FIRST fg-rcpth
      WHERE fg-rcpth.r-no    EQ fg-rdtlh.r-no
        AND fg-rcpth.i-no    EQ {&TABLENAME}.i-no
        AND fg-rcpth.job-no  EQ {&TABLENAME}.job-no
        AND fg-rcpth.job-no2 EQ {&TABLENAME}.job-no2
      USE-INDEX i-no NO-LOCK:
    {&TABLENAME}.cases-unit = fg-rdtlh.stacks-unit.
    LEAVE.
  END.

  ELSE
  FOR EACH fg-rcpth
      WHERE fg-rcpth.company   EQ {&TABLENAME}.company
        AND fg-rcpth.i-no      EQ {&TABLENAME}.i-no
        AND fg-rcpth.job-no    EQ {&TABLENAME}.job-no
        AND fg-rcpth.job-no2   EQ {&TABLENAME}.job-no2
        AND fg-rcpth.rita-code EQ "R"
      USE-INDEX i-no NO-LOCK,

      FIRST fg-rdtlh
      WHERE fg-rdtlh.r-no        EQ fg-rcpth.r-no
        AND fg-rdtlh.loc         EQ {&TABLENAME}.loc
        AND fg-rdtlh.loc-bin     EQ {&TABLENAME}.loc-bin
        AND fg-rdtlh.tag         EQ {&TABLENAME}.tag
        AND fg-rdtlh.stacks-unit GT 0
      USE-INDEX rm-rdtl NO-LOCK

      BY fg-rcpth.trans-date
      BY fg-rcpth.r-no
      BY fg-rdtlh.rec_key:
    {&TABLENAME}.cases-unit = fg-rdtlh.stacks-unit.
    LEAVE.
  END.
*/

IF {&TABLENAME}.partial-count EQ ? THEN {&TABLENAME}.partial-count = 0.

IF TRIM({&TABLENAME}.tag) NE "" THEN RUN fg/uploadtg.p (ROWID({&TABLENAME})).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

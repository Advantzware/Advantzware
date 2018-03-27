&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-rell

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-oe-ordl FOR oe-ordl.

DEF VAR cocode AS CHAR NO-UNDO.
DEFINE VARIABLE lSkipRecalcInventory AS LOGICAL NO-UNDO. 

DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF reftable.

cocode = {&TABLENAME}.company.
{sys/inc/oereordr.i}

/* Finding sys-ctrl directly for to improve speed for trigger */
FIND FIRST sys-ctrl NO-LOCK
    WHERE sys-ctrl.company EQ cocode
      AND sys-ctrl.name    EQ "RelSkipRecalc"
    NO-ERROR.
IF AVAILABLE sys-ctrl THEN 
  lSkipRecalcInventory = sys-ctrl.log-fld.

ASSIGN {&TABLENAME}.upd-date = TODAY
       {&TABLENAME}.upd-time = TIME
       .
IF TRIM({&TABLENAME}.s-code) EQ "" THEN {&TABLENAME}.s-code = "B".

IF {&TABLENAME}.qty-case LT 0 THEN
  {&TABLENAME}.qty-case = {&TABLENAME}.qty-case * -1.

IF {&TABLENAME}.qty LT 0 AND {&TABLENAME}.cases GT 0 THEN
  {&TABLENAME}.cases = {&TABLENAME}.cases * -1.

IF {&TABLENAME}.qty EQ 0 THEN
  ASSIGN
   {&TABLENAME}.cases   = 0
   {&TABLENAME}.partial = 0.

ELSE DO:
  IF ({&TABLENAME}.partial GT {&TABLENAME}.qty AND {&TABLENAME}.partial GT 0) OR
     ({&TABLENAME}.partial LT 0 AND {&TABLENAME}.partial LT {&TABLENAME}.qty) THEN
    {&TABLENAME}.partial = {&TABLENAME}.qty.

  IF {&TABLENAME}.qty-case GT 0 THEN DO:
    {&TABLENAME}.cases = TRUNC(({&TABLENAME}.qty - {&TABLENAME}.partial) / {&TABLENAME}.qty-case,0).
   
    IF {&TABLENAME}.partial LT 0                     AND
       {&TABLENAME}.qty GT {&TABLENAME}.partial * -1 THEN
      {&TABLENAME}.cases = TRUNC({&TABLENAME}.qty / {&TABLENAME}.qty-case,0).
   
    {&TABLENAME}.partial = {&TABLENAME}.qty - 
                           ({&TABLENAME}.cases * {&TABLENAME}.qty-case).
  END.

  IF {&TABLENAME}.qty-case LE 0                       OR
     ({&TABLENAME}.qty LT {&TABLENAME}.qty-case AND
      {&TABLENAME}.qty GT 0)                          THEN
    ASSIGN
    {&TABLENAME}.qty-case = {&TABLENAME}.qty
    {&TABLENAME}.partial  = 0.
END.

IF {&TABLENAME}.cases   EQ ? THEN {&TABLENAME}.cases   = 0.
IF {&TABLENAME}.partial EQ ? THEN {&TABLENAME}.partial = 0.

FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no
    NO-LOCK:

  FIND oe-ordl OF oe-ord NO-LOCK 
      WHERE oe-ordl.i-no EQ {&TABLENAME}.i-no
      NO-ERROR.
  IF AVAIL oe-ordl THEN {&TABLENAME}.line = oe-ordl.line.

  LEAVE.
END.

RELEASE oe-ordl.

FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ {&TABLENAME}.company
      AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
      AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
      AND oe-ordl.line    EQ {&TABLENAME}.line
    USE-INDEX item-ord:

  {&TABLENAME}.s-code = IF oe-ordl.is-a-component AND {&TABLENAME}.s-code NE "T"
                        THEN "S" ELSE
                        IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
                        THEN "I" ELSE {&TABLENAME}.s-code.

  IF {&TABLENAME}.qty NE old-{&TABLENAME}.qty THEN DO:
    FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl)
        EXCLUSIVE-LOCK NO-WAIT NO-ERROR.
    IF AVAIL b-oe-ordl THEN
      RUN oe/rell-qty.p (ROWID(b-oe-ordl), OUTPUT b-oe-ordl.t-rel-qty).
  END.

  LEAVE.
END.

IF lSkipRecalcInventory EQ NO AND {&TABLENAME}.b-ord-no GE 0               AND
   {&TABLENAME}.qty NE old-{&TABLENAME}.qty THEN
FOR EACH itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.i-no    EQ {&TABLENAME}.i-no:
  RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc,
                                    OUTPUT itemfg.q-back).
  itemfg.q-avail = itemfg.q-onh +
                   (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) -
                   itemfg.q-alloc.
  FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                        AND itemfg-loc.i-no    EQ itemfg.i-no
                      EXCLUSIVE-LOCK:
      RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, OUTPUT itemfg-loc.q-alloc,
                                        OUTPUT itemfg-loc.q-back).
      itemfg-loc.q-avail = itemfg-loc.q-onh +
                       (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg-loc.q-ono) -
                       itemfg-loc.q-alloc.
  END.
  LEAVE.
END.

RUN oe/rel-stat-upd.p (ROWID({&TABLENAME})).

IF old-{&TABLENAME}.rec_key NE {&TABLENAME}.rec_key AND
   old-{&TABLENAME}.rec_key NE ""                   THEN
FOR EACH reftable WHERE reftable.rec_key EQ old-{&TABLENAME}.rec_key:
  reftable.rec_key = {&TABLENAME}.rec_key.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

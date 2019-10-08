&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME ar-invl

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}



{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR li AS INT NO-UNDO.
DEF VAR ld AS DEC NO-UNDO.

DEF TEMP-TABLE w-sman FIELD w-man AS CHAR
                      FIELD w-pct AS DEC
                      FIELD w-com AS DEC
                      FIELD w-bas AS CHAR.


ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/notes.i}

FOR EACH ar-inv WHERE ar-inv.x-no EQ {&TABLENAME}.x-no NO-LOCK:
  {&TABLENAME}.cust-no = ar-inv.cust-no.
  LEAVE.
END.

IF {&TABLENAME}.amt         EQ ? THEN {&TABLENAME}.amt         = 0.
IF {&TABLENAME}.cost        EQ ? THEN {&TABLENAME}.cost        = 0.
IF {&TABLENAME}.t-cost      EQ ? THEN {&TABLENAME}.t-cost      = 0.
IF {&TABLENAME}.t-freight   EQ ? THEN {&TABLENAME}.t-freight   = 0.
IF {&TABLENAME}.t-freight-p EQ ? THEN {&TABLENAME}.t-freight-p = 0.

DO li = 1 TO EXTENT({&TABLENAME}.sman):
  FIND FIRST w-sman WHERE w-man EQ {&TABLENAME}.sman[li] NO-ERROR.

  IF NOT AVAIL w-sman THEN DO:
    CREATE w-sman.
    w-man = {&TABLENAME}.sman[li].
  END.

  w-pct = w-pct + {&TABLENAME}.s-pct[li].

  IF w-com EQ 0  THEN w-com = {&TABLENAME}.s-comm[li].
  IF w-bas EQ "" THEN w-bas = {&TABLENAME}.s-commbasis[li].
END.

/*FOR EACH w-sman:
  IF w-man EQ "" THEN DELETE w-sman.
  ELSE ld = ld + w-pct.
END.

IF ld GT 0 AND ld NE 100 THEN DO:
  FOR EACH w-sman:
    w-pct = ROUND(w-pct * 100 / ld,2).
  END.

  ld = 0.
  FOR EACH w-sman:
    ld = ld + w-pct.
  END.

  IF ld NE 100 THEN
  FOR EACH w-sman:
    w-pct = w-pct + (100 - ld).
    LEAVE.
  END.
END.*/

ASSIGN
 {&TABLENAME}.sman        = ""
 {&TABLENAME}.s-pct       = 0
 {&TABLENAME}.s-comm      = 0
 {&TABLENAME}.s-commbasis = "".

li = 0.
FOR EACH w-sman:
  li = li + 1.
  IF li GT EXTENT({&TABLENAME}.sman) THEN LEAVE.
  ASSIGN
   {&TABLENAME}.sman[li]        = w-man
   {&TABLENAME}.s-pct[li]       = w-pct
   {&TABLENAME}.s-comm[li]      = w-com
   {&TABLENAME}.s-commbasis[li] = w-bas.
END.

FIND FIRST itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.i-no    EQ {&TABLENAME}.i-no
    NO-LOCK NO-ERROR.

IF {&TABLENAME}.i-no NE ""                               AND
   {&TABLENAME}.i-no NE "0"                              AND
   {&TABLENAME}.inv-qty NE 0                             AND
   {&TABLENAME}.pr-uom NE ""                             AND
   old-{&TABLENAME}.i-no NE "0"                          AND
   AVAIL itemfg                                          AND
   ({&TABLENAME}.inv-qty NE old-{&TABLENAME}.inv-qty OR
    {&TABLENAME}.unit-pr NE old-{&TABLENAME}.unit-pr OR
    {&TABLENAME}.pr-uom  NE old-{&TABLENAME}.pr-uom)     THEN

  RUN fg/makenote.p (BUFFER oe-ordl,
                     BUFFER quoteqty,
                     BUFFER {&TABLENAME},
                     NO,
                     itemfg.rec_key).

RUN oe/rel-stat-upd.p (ROWID({&TABLENAME})).

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.




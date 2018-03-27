&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME inv-line

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF VAR v-invlotline-int AS INT NO-UNDO.

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.

DEF VAR ll-calc-disc-first AS LOG NO-UNDO.
DEF VAR v-cost AS DEC EXTENT 4.
DEF VAR v-basis LIKE sman.commbasis  INIT "" NO-UNDO.

DEF VAR fg-uom-list AS cha NO-UNDO.
RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).

ASSIGN
 cocode             = g_company
 locode             = g_loc
 ll-calc-disc-first = NO.

find first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "INVLOTLINE"
    no-lock no-error.

IF AVAIL sys-ctrl THEN
   v-invlotline-int = sys-ctrl.int-fld.

FOR EACH inv-head WHERE inv-head.r-no EQ {&TABLENAME}.r-no NO-LOCK:
  {&TABLENAME}.cust-no = inv-head.cust-no.
  LEAVE.
END.

FOR EACH oe-ord
    WHERE oe-ord.company EQ {&TABLENAME}.company
      AND oe-ord.ord-no  EQ {&TABLENAME}.ord-no
    NO-LOCK:

  FIND oe-ordl OF oe-ord
      WHERE oe-ordl.i-no EQ {&TABLENAME}.i-no
      NO-LOCK NO-ERROR.
  IF AVAIL oe-ordl THEN {&TABLENAME}.line = oe-ordl.line.

  LEAVE.
END.

IF TRIM({&TABLENAME}.stat) EQ "" THEN {&TABLENAME}.stat = "B".

IF {&TABLENAME}.ord-no NE 0  AND
   {&TABLENAME}.i-no   NE "" THEN
FOR EACH oe-rel
    WHERE oe-rel.company EQ {&TABLENAME}.company
      AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no
      AND oe-rel.i-no    EQ {&TABLENAME}.i-no
      AND oe-rel.line    EQ {&TABLENAME}.line
      AND oe-rel.link-no EQ 0
      AND oe-rel.rel-no  EQ 0
      AND oe-rel.r-no    GT 0
    USE-INDEX ord-item,
    FIRST oe-rell
    WHERE oe-rell.company EQ {&TABLENAME}.company
      AND oe-rell.ord-no  EQ {&TABLENAME}.ord-no
      AND oe-rell.i-no    EQ {&TABLENAME}.i-no
      AND oe-rell.line    EQ {&TABLENAME}.line
      AND oe-rell.link-no EQ oe-rel.r-no
      AND CAN-FIND(FIRST oe-relh
                   WHERE oe-relh.r-no EQ oe-rell.r-no
                     AND oe-relh.posted EQ YES)
    USE-INDEX ord-no NO-LOCK:

  ASSIGN
   oe-rel.link-no  = oe-rell.r-no
   oe-rel.rel-no   = oe-rell.rel-no
   oe-rel.b-ord-no = oe-rell.b-ord-no.
END.

/* Change Customer PO on BOLs & releases if changed on invoice */
IF {&TABLENAME}.po-no NE old-{&TABLENAME}.po-no THEN
  RUN oe/invlinpo.p (BUFFER {&TABLENAME}, old-{&TABLENAME}.po-no).

FOR EACH sys-ctrl
    WHERE sys-ctrl.company  EQ cocode
      AND sys-ctrl.name     EQ "INVPRINT"
      AND sys-ctrl.char-fld EQ "Dayton"
    NO-LOCK:
  ll-calc-disc-first = YES.
  LEAVE.
END.
    
FOR EACH inv-head OF {&TABLENAME}
    WHERE NOT inv-head.multi-invoice NO-LOCK,
    FIRST cust
    WHERE cust.company EQ inv-head.company
      AND cust.cust-no EQ inv-head.cust-no
    NO-LOCK,
    FIRST itemfg
    {sys/look/itemfgrlW.i}
      AND itemfg.i-no eq {&TABLENAME}.i-no
    NO-LOCK:

  {&TABLENAME}.t-price = {&TABLENAME}.inv-qty / 1000 * {&TABLENAME}.price.

  IF {&TABLENAME}.pr-uom BEGINS "L" AND {&TABLENAME}.pr-uom NE "LB" THEN
    {&TABLENAME}.t-price = {&TABLENAME}.price *
                           IF {&TABLENAME}.inv-qty LT 0 THEN -1 ELSE IF {&TABLENAME}.inv-qty EQ 0 THEN 0 ELSE 1.

  ELSE
  IF {&TABLENAME}.pr-uom EQ "CS" THEN
    {&TABLENAME}.t-price = {&TABLENAME}.inv-qty /
                           (IF {&TABLENAME}.cas-cnt NE 0 THEN
                             {&TABLENAME}.cas-cnt
                            ELSE
                            IF itemfg.case-count NE 0 THEN
                              itemfg.case-count ELSE 1) *
                           {&TABLENAME}.price.
  ELSE
  IF LOOKUP({&TABLENAME}.pr-uom,fg-uom-list) GT 0 THEN
    {&TABLENAME}.t-price = {&TABLENAME}.inv-qty * {&TABLENAME}.price.

  ELSE
  FOR EACH uom
      WHERE uom.uom  EQ {&TABLENAME}.pr-uom
        AND uom.mult NE 0
      NO-LOCK:
    {&TABLENAME}.t-price = {&TABLENAME}.inv-qty / uom.mult * {&TABLENAME}.price.
    LEAVE.
  END.

  {&TABLENAME}.t-price = ROUND({&TABLENAME}.t-price,2).


  IF {&TABLENAME}.t-price NE 0                                              AND
     {&TABLENAME}.pr-uom BEGINS "L"                                         AND
     {&TABLENAME}.pr-uom NE "LB"                                            AND
     v-invlotline-int = 0                                                   AND
     AVAIL oe-ordl                                                          AND
     ((oe-ordl.qty GT 0 AND oe-ordl.inv-qty - {&TABLENAME}.inv-qty GT 0) OR
      (oe-ordl.qty LT 0 AND oe-ordl.inv-qty - {&TABLENAME}.inv-qty LT 0))                          THEN
    {&TABLENAME}.t-price = 0.

  IF {&TABLENAME}.disc NE 0 THEN
    {&TABLENAME}.t-price = 
        IF ll-calc-disc-first THEN 
          ({&TABLENAME}.t-price - ROUND({&TABLENAME}.t-price * {&TABLENAME}.disc / 100,2))
        ELSE
          ROUND({&TABLENAME}.t-price * (1 - ({&TABLENAME}.disc / 100)),2).

  IF {&TABLENAME}.ord-no NE 0 THEN
    RUN oe/invlcost.p (ROWID({&TABLENAME}),
                       OUTPUT v-cost[1], OUTPUT v-cost[2],
                       OUTPUT v-cost[3], OUTPUT v-cost[4],
                       OUTPUT {&TABLENAME}.cost, OUTPUT {&TABLENAME}.t-cost).

  DO i = 1 TO EXTENT({&TABLENAME}.sman):    /** Calculate Commission Amount **/
    RUN custom/combasis.p (cocode, {&TABLENAME}.sman[i], cust.type, itemfg.procat, 0,
                           cust.cust-no,
                           OUTPUT v-basis).

    IF v-basis EQ "G" THEN
      {&TABLENAME}.comm-amt[i] = ROUND((({&TABLENAME}.t-price - {&TABLENAME}.t-cost)
                                       * {&TABLENAME}.s-comm[i]) / 100,2).

    ELSE
      {&TABLENAME}.comm-amt[i] = ROUND(((({&TABLENAME}.t-price
                                       * {&TABLENAME}.s-pct[i]) / 100)
                                       * {&TABLENAME}.s-comm[i]) / 100,2).
  END.

  RUN oe/oeinvup2.p (ROWID(inv-head), INPUT NO).

  LEAVE.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

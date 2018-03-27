&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-boll

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-rell FOR oe-rell.

DEF VAR li AS INT NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF oe-rel.
DISABLE TRIGGERS FOR LOAD OF oe-rell.
DISABLE TRIGGERS FOR LOAD OF oe-bolh.
DISABLE TRIGGERS FOR LOAD OF oe-ordl.
DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
DISABLE TRIGGERS FOR LOAD OF itemfg.
DISABLE TRIGGERS FOR LOAD OF reftable.
DISABLE TRIGGERS FOR LOAD OF b-{&TABLENAME}.

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

IF {&TABLENAME}.p-c NE old-{&TABLENAME}.p-c THEN
FOR EACH b-{&TABLENAME}
    WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
      AND b-{&TABLENAME}.ord-no   EQ {&TABLENAME}.ord-no
      AND b-{&TABLENAME}.i-no     EQ {&TABLENAME}.i-no 
      AND b-{&TABLENAME}.line     EQ {&TABLENAME}.line
      AND b-{&TABLENAME}.rel-no   EQ {&TABLENAME}.rel-no
      AND b-{&TABLENAME}.b-ord-no LE {&TABLENAME}.b-ord-no
      AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
    USE-INDEX ord-no:
  b-{&TABLENAME}.p-c = {&TABLENAME}.p-c.
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

RELEASE oe-ordl.

FOR EACH oe-ordl NO-LOCK
    WHERE oe-ordl.company EQ {&TABLENAME}.company
      AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
      AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
      AND oe-ordl.line    EQ {&TABLENAME}.line:

  {&TABLENAME}.s-code = IF oe-ordl.is-a-component
                        THEN "S"
                        ELSE
                        IF CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
                        THEN "I"
                        ELSE {&TABLENAME}.s-code.
                            
  FIND b-oe-ordl EXCLUSIVE WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl)
      NO-WAIT NO-ERROR.

  IF AVAIL b-oe-ordl THEN DO:
    RUN oe/ordlsqty.p (ROWID(b-oe-ordl),
                       OUTPUT b-oe-ordl.inv-qty,
                       OUTPUT b-oe-ordl.ship-qty).
        
    b-oe-ordl.t-ship-qty = b-oe-ordl.ship-qty.

    RUN oe/rell-qty.p (ROWID(b-oe-ordl), OUTPUT b-oe-ordl.t-rel-qty).

    FIND b-oe-ordl NO-LOCK WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) NO-ERROR.
  END.

  LEAVE.
END.

IF {&TABLENAME}.b-no   NE 0  AND
   {&TABLENAME}.ord-no NE 0  AND
   {&TABLENAME}.i-no   NE "" THEN DO:

  IF {&TABLENAME}.po-no NE old-{&TABLENAME}.po-no THEN
  FOR EACH oe-rell
      WHERE oe-rell.company  EQ {&TABLENAME}.company
        AND oe-rell.ord-no   EQ {&TABLENAME}.ord-no
        AND oe-rell.line     EQ {&TABLENAME}.line
        AND oe-rell.i-no     EQ {&TABLENAME}.i-no
        AND oe-rell.r-no     EQ {&TABLENAME}.r-no
        AND oe-rell.rel-no   EQ {&TABLENAME}.rel-no
        AND oe-rell.b-ord-no EQ {&TABLENAME}.b-ord-no
        AND oe-rell.po-no    EQ old-{&TABLENAME}.po-no
        AND CAN-FIND(FIRST oe-relh
                     WHERE oe-relh.r-no   EQ {&TABLENAME}.r-no
                       AND oe-relh.posted EQ YES)
      USE-INDEX ord-no:

    oe-rell.po-no = {&TABLENAME}.po-no.

    FOR EACH oe-rel
        WHERE oe-rel.company EQ oe-rell.company
          AND oe-rel.r-no    EQ oe-rell.link-no
          AND oe-rel.i-no    EQ oe-rell.i-no
          AND oe-rel.line    EQ oe-rell.line
          AND oe-rel.link-no EQ oe-rell.r-no
        USE-INDEX link:
      oe-rel.po-no = oe-rell.po-no.    
    END.
  END.

  

  FOR EACH oe-bolh WHERE oe-bolh.b-no EQ {&TABLENAME}.b-no NO-LOCK:
    ASSIGN
     {&TABLENAME}.bol-no   = oe-bolh.bol-no
     {&TABLENAME}.bol-date = oe-bolh.bol-date.


    FOR EACH oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ {&TABLENAME}.company
          AND oe-ordl.ord-no  EQ {&TABLENAME}.ord-no
          AND oe-ordl.i-no    EQ {&TABLENAME}.i-no
        BREAK BY oe-ordl.line DESC:
      IF LAST(oe-ordl.line) OR oe-ordl.line EQ {&TABLENAME}.line THEN DO:
        {&TABLENAME}.line = oe-ordl.line.
        LEAVE.
      END.
    END.

    FOR EACH oe-relh 
        WHERE oe-relh.company  EQ oe-bolh.company
          AND oe-relh.release# EQ oe-bolh.release#
        NO-LOCK:
       /* Task 04271204 - Was causing extra oe-rell's to be created */
      /* {&TABLENAME}.r-no = oe-relh.r-no. */
      /* This block was causing invoices to be short
    FOR EACH oe-rell
        WHERE oe-rell.company      EQ {&TABLENAME}.company
          AND oe-rell.ord-no       EQ {&TABLENAME}.ord-no
          AND oe-rell.line         EQ {&TABLENAME}.line
          AND oe-rell.i-no         EQ {&TABLENAME}.i-no
          /* AND oe-rell.r-no         EQ {&TABLENAME}.r-no */
          AND ({&TABLENAME}.rel-no EQ 0 OR
               (oe-rell.rel-no     EQ {&TABLENAME}.rel-no AND
                oe-rell.b-ord-no   EQ {&TABLENAME}.b-ord-no))
          AND oe-rell.po-no        EQ {&TABLENAME}.po-no
        USE-INDEX ord-no:
        IF oe-rell.r-no NE oe-relh.r-no THEN
            oe-rell.r-no = oe-relh.r-no.
    END.
    */
      LEAVE.
    END.
    
    FOR EACH oe-rell
        WHERE oe-rell.company      EQ {&TABLENAME}.company
          AND oe-rell.ord-no       EQ {&TABLENAME}.ord-no
          AND oe-rell.line         EQ {&TABLENAME}.line
          AND oe-rell.i-no         EQ {&TABLENAME}.i-no
          AND oe-rell.r-no         EQ {&TABLENAME}.r-no
          AND ({&TABLENAME}.rel-no EQ 0 OR
               (oe-rell.rel-no     EQ {&TABLENAME}.rel-no AND
                oe-rell.b-ord-no   EQ {&TABLENAME}.b-ord-no))
          AND oe-rell.po-no        EQ {&TABLENAME}.po-no
        USE-INDEX ord-no:
      LEAVE.
    END.
    
    IF AVAIL oe-rell THEN
      ASSIGN
       {&TABLENAME}.rel-no   = oe-rell.rel-no
       {&TABLENAME}.b-ord-no = oe-rell.b-ord-no.

    ELSE DO:
      CREATE oe-rell.
      BUFFER-COPY {&TABLENAME} EXCEPT rec_key TO oe-rell
      ASSIGN
       oe-rell.rel-no = 0
       oe-rell.posted = YES.
      /* needed because triggers are disabled */
      {custom/rec_key.i oe-rell}
    END.

    IF oe-rell.rel-no EQ 0 THEN DO:
      li = 0.

      FOR EACH b-oe-rell
          WHERE b-oe-rell.company EQ {&TABLENAME}.company
            AND b-oe-rell.ord-no  EQ {&TABLENAME}.ord-no
            AND ROWID(b-oe-rell)  NE ROWID(oe-rell)
          USE-INDEX ord-no NO-LOCK 
          BY b-oe-rell.rel-no DESC:
        li = b-oe-rell.rel-no.
        LEAVE.  
      END.

      ASSIGN
       oe-rell.rel-no      = li + 1
       {&TABLENAME}.rel-no = oe-rell.rel-no.
      RUN oe/upschrel.p (ROWID(oe-rell)).
    END.

    LEAVE.
  END.

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
        AND oe-rell.posted  EQ YES
      USE-INDEX ord-no NO-LOCK:

    ASSIGN
     oe-rel.link-no  = oe-rell.r-no
     oe-rel.rel-no   = oe-rell.rel-no
     oe-rel.b-ord-no = oe-rell.b-ord-no.
  END.
END.
    
IF {&TABLENAME}.i-no    NE ""                    AND
   ({&TABLENAME}.weight EQ 0 OR
    {&TABLENAME}.qty    NE old-{&TABLENAME}.qty) THEN
FOR EACH itemfg
    WHERE itemfg.company EQ {&TABLENAME}.company
      AND itemfg.i-no    EQ {&TABLENAME}.i-no:
  IF itemfg.weight-100 EQ 0 THEN itemfg.weight-100 = 1.
  {&TABLENAME}.weight = (((({&TABLENAME}.cases * {&TABLENAME}.qty-case) +
                            {&TABLENAME}.partial) / 100) * itemfg.weight-100).
  LEAVE.
END.

FOR EACH oe-bolh WHERE oe-bolh.b-no EQ {&TABLENAME}.b-no:

  IF PROGRAM-NAME(2) NE "oe/invpost3.p" THEN
     ASSIGN
       oe-bolh.upd-date = TODAY
       oe-bolh.upd-time = TIME
       oe-bolh.user-id  = USERID("nosweat").

  RUN oe/bolhtots.p (ROWID(oe-bolh)).

  LEAVE.
END.

IF {&TABLENAME}.weight LT 0 THEN {&TABLENAME}.weight = {&TABLENAME}.weight * -1.

/* wfk - This can't run here since it runs from write.trg/oe-rell.p 
  RUN oe/rel-stat-upd.p (ROWID({&TABLENAME})). */

IF old-{&TABLENAME}.rec_key NE {&TABLENAME}.rec_key AND
   old-{&TABLENAME}.rec_key NE ""                   THEN
FOR EACH reftable WHERE reftable.rec_key EQ old-{&TABLENAME}.rec_key:
  reftable.rec_key = {&TABLENAME}.rec_key.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

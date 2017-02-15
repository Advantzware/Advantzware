&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-bolh

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR ll AS LOG NO-UNDO.
DEF VAR lv-bol-no LIKE {&TABLENAME}.bol-no NO-UNDO.
DEF VAR v-nxt-r-no AS INT NO-UNDO.

DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel  FOR oe-rel.
DEF BUFFER b-oe-relh FOR oe-relh.
DEF BUFFER b-oe-rell FOR oe-rell.
DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-boll FOR oe-boll.

DEF VAR li AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF b-oe-boll.
DISABLE TRIGGERS FOR LOAD OF b-oe-relh.

IF {&TABLENAME}.company NE "" AND {&TABLENAME}.b-no NE 0 AND NOT {&TABLENAME}.posted THEN
DO WHILE CAN-FIND(FIRST b-{&TABLENAME}
                  WHERE b-{&TABLENAME}.b-no   EQ {&TABLENAME}.b-no
                    AND ROWID(b-{&TABLENAME}) NE ROWID({&TABLENAME})):
  li = 0.

  FIND LAST b-{&TABLENAME} USE-INDEX b-no NO-LOCK NO-ERROR.
  IF AVAIL b-{&TABLENAME} AND b-{&TABLENAME}.b-no GT li THEN li = b-{&TABLENAME}.b-no.

  {&TABLENAME}.b-no = li + 1.
END.
/*
IF {&TABLENAME}.release# NE 0                                    AND
   NOT CAN-FIND(FIRST oe-relh
                WHERE oe-relh.company  EQ {&TABLENAME}.company
                  AND oe-relh.release# EQ {&TABLENAME}.release#) THEN DO:
  IF oe-bolh.r-no = 0 THEN DO:
     find last oe-relh use-index r-no no-lock no-error.
     v-nxt-r-no = if avail oe-relh then oe-relh.r-no else 0.
     oe-bolh.r-no = v-nxt-r-no + 1.
  END.
  CREATE oe-relh.
  BUFFER-COPY oe-bolh EXCEPT rec_key TO oe-relh
     ASSIGN oe-relh.posted = YES.
END.
  */
IF {&TABLENAME}.bol-no NE old-{&TABLENAME}.bol-no AND
   {&TABLENAME}.bol-no NE 0                       AND
   old-{&TABLENAME}.bol-no NE 0                   THEN DO:

  FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ {&TABLENAME}.company EXCLUSIVE NO-ERROR.
  IF AVAIL oe-ctrl AND oe-ctrl.n-bol EQ old-{&TABLENAME}.bol-no + 1 THEN
    oe-ctrl.n-bol = old-{&TABLENAME}.bol-no.

  FIND FIRST oe-boll NO-LOCK
      WHERE oe-boll.company EQ {&TABLENAME}.company
        AND oe-boll.b-no    EQ {&TABLENAME}.b-no
        AND oe-boll.s-code  NE ""
      NO-ERROR.

  ll = NO.
  IF oe-boll.s-code EQ "T" THEN
  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
        AND b-{&TABLENAME}.bol-no   EQ {&TABLENAME}.bol-no
        AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
        AND (b-{&TABLENAME}.cust-no NE {&TABLENAME}.cust-no OR
             b-{&TABLENAME}.ship-id NE {&TABLENAME}.ship-id OR
             b-{&TABLENAME}.posted  EQ YES                  OR
             b-{&TABLENAME}.deleted EQ YES                  OR
             (AVAIL oe-boll AND
              CAN-FIND(FIRST b-oe-boll
                       WHERE b-oe-boll.company EQ b-{&TABLENAME}.company
                         AND b-oe-boll.b-no    EQ b-{&TABLENAME}.b-no
                         AND b-oe-boll.s-code  NE oe-boll.s-code)))
      NO-LOCK:
    ll = YES.
    LEAVE.
  END.

  IF ll                                                                OR
     NOT CAN-FIND(FIRST b-{&TABLENAME}
                  WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                    AND b-{&TABLENAME}.bol-no  EQ {&TABLENAME}.bol-no
                    AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})) THEN DO:

    IF old-{&TABLENAME}.bol-no NE {&TABLENAME}.bol-no AND
       old-{&TABLENAME}.bol-no NE 0                   THEN
      lv-bol-no = old-{&TABLENAME}.bol-no.

    ELSE RUN oe/oe-bolno.p ({&TABLENAME}.company, OUTPUT lv-bol-no).

    {&TABLENAME}.bol-no = lv-bol-no.
  END.

  ELSE
  FOR EACH b-{&TABLENAME}
      WHERE b-{&TABLENAME}.company  EQ {&TABLENAME}.company
        AND b-{&TABLENAME}.bol-no   EQ {&TABLENAME}.bol-no
        AND b-{&TABLENAME}.cust-no  EQ {&TABLENAME}.cust-no
        AND b-{&TABLENAME}.ship-id  EQ {&TABLENAME}.ship-id
        AND b-{&TABLENAME}.posted   EQ NO
        AND b-{&TABLENAME}.deleted  EQ NO
        AND ROWID(b-{&TABLENAME})   NE ROWID({&TABLENAME})
      USE-INDEX bol-no,

      FIRST b-oe-relh
      WHERE b-oe-relh.company  EQ b-{&TABLENAME}.company
        AND b-oe-relh.release# EQ b-{&TABLENAME}.release#,
      
      FIRST oe-relh
      WHERE oe-relh.company  EQ {&TABLENAME}.company
        AND oe-relh.release# EQ {&TABLENAME}.release#
      NO-LOCK:
    /*
    FOR EACH b-oe-rell
        WHERE b-oe-rell.company EQ b-oe-relh.company
          AND b-oe-rell.r-no    EQ b-oe-relh.r-no
        USE-INDEX r-no:
      FOR EACH b-oe-rel
          WHERE b-oe-rel.r-no    EQ b-oe-rell.link-no
            AND b-oe-rel.link-no NE 0:
        b-oe-rel.link-no = oe-relh.r-no.
      END.
      b-oe-rell.r-no = oe-relh.r-no.
    END.

    DELETE b-oe-relh.
    */
    FOR EACH b-oe-boll
        WHERE b-oe-boll.company EQ b-{&TABLENAME}.company
          AND b-oe-boll.b-no    EQ b-{&TABLENAME}.b-no:

      ASSIGN
       b-oe-boll.b-no     = {&TABLENAME}.b-no
       b-oe-boll.bol-no   = {&TABLENAME}.bol-no
       b-oe-boll.bol-date = {&TABLENAME}.bol-date
       /*b-oe-boll.r-no     = oe-relh.r-no */.
    END.

    FOR EACH b-oe-boll
        WHERE b-oe-boll.company EQ b-{&TABLENAME}.company
          AND b-oe-boll.bol-no  EQ old-{&TABLENAME}.bol-no:

        b-oe-boll.bol-no = b-{&TABLENAME}.bol-no.
    END.

    ASSIGN
     {&TABLENAME}.printed     = NO
     {&TABLENAME}.freight     = {&TABLENAME}.freight     + b-{&TABLENAME}.freight
     {&TABLENAME}.tot-wt      = {&TABLENAME}.tot-wt      + b-{&TABLENAME}.tot-wt 
     {&TABLENAME}.tot-pallets = {&TABLENAME}.tot-pallets + b-{&TABLENAME}.tot-pallets.

    FOR EACH oe-boll
        WHERE oe-boll.company EQ {&TABLENAME}.company
          AND oe-boll.b-no    EQ {&TABLENAME}.b-no:
      oe-boll.printed = YES.
    END.

    DELETE b-{&TABLENAME}.
  END.
END.

IF {&TABLENAME}.posted NE old-{&TABLENAME}.posted THEN
FOR EACH oe-boll OF {&TABLENAME} EXCLUSIVE:
  oe-boll.posted = {&TABLENAME}.posted.
      
  RUN oe/rel-stat-upd.p (ROWID(oe-boll)).

  IF NOT {&TABLENAME}.posted THEN
  FOR EACH oe-ordl
      WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.i-no    EQ oe-boll.i-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.is-a-component EQ NO
        AND CAN-FIND(FIRST b-oe-ordl {sys/inc/ordlcomp.i b-oe-ordl oe-ordl})
      NO-LOCK:
  
    FOR EACH oe-rell WHERE oe-rell.r-no EQ oe-boll.r-no
      AND oe-rell.i-no EQ oe-boll.i-no 
      AND oe-rell.loc  EQ oe-boll.loc EXCLUSIVE-LOCK:

      FOR EACH oe-rel WHERE oe-rel.company EQ oe-boll.company
          AND oe-rel.r-no EQ oe-rell.link-no 
          AND oe-rel.i-no    EQ oe-rell.i-no 
          EXCLUSIVE-LOCK USE-INDEX seq-no:
        DELETE oe-rel.
      END.
      DELETE oe-rell.

    END.
    DELETE oe-boll.
    LEAVE.
  END.
END.

IF {&TABLENAME}.posted EQ NO THEN DO:
  ASSIGN
   {&TABLENAME}.upd-date = TODAY
   {&TABLENAME}.upd-time = TIME
   {&TABLENAME}.user-id  = USERID("nosweat").

  IF {&TABLENAME}.printed     EQ YES AND
     old-{&TABLENAME}.printed EQ NO  THEN
    ASSIGN
     {&TABLENAME}.prt-date  = TODAY
     {&TABLENAME}.prt-time  = TIME.
END.

IF {&TABLENAME}.prt-time NE 0 THEN
  {&TABLENAME}.prt-timex = STRING({&TABLENAME}.prt-time,"HH:MM:SS").

RUN oe/bolhtots.p (ROWID({&TABLENAME})).

IF {&TABLENAME}.tot-wt LT 0 THEN {&TABLENAME}.tot-wt = {&TABLENAME}.tot-wt * -1.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.


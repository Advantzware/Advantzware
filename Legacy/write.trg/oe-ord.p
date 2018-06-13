&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME oe-ord

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER b-oe-rel FOR oe-rel.

DEF VAR li AS INT NO-UNDO.
DEF VAR li-next-ordno AS INT NO-UNDO.

DISABLE TRIGGERS FOR LOAD OF oe-rel.
DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

ASSIGN
 cocode = g_company
 locode = g_loc.

{sys/inc/oeuserid.i}
IF {&TABLENAME}.ord-no EQ 0 AND USERID("ASI") EQ "ASI" THEN DO:
    MESSAGE "Internal Error - Please Call ASI" SKIP
        "An order with order number 0 was created." SKIP
        "Press OK to continue." SKIP
        VIEW-AS ALERT-BOX INFO BUTTONS OK.
RUN sys/ref/asiseq.p (INPUT g_company, 
                      INPUT "order_seq", 
                      OUTPUT li-next-ordno) NO-ERROR.
  ASSIGN {&TABLENAME}.ord-no  = li-next-ordno
         {&TABLENAME}.company = cocode.

END.
/* WFK - Now using a sequence, this check not required */
/* IF {&TABLENAME}.company NE ""   AND                                     */
/*    {&TABLENAME}.ord-no NE 0     AND                                     */
/*    old-{&TABLENAME}.ord-no EQ 0 THEN                                    */
/* DO WHILE CAN-FIND(FIRST b-{&TABLENAME}                                  */
/*                   WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company  */
/*                     AND b-{&TABLENAME}.ord-no  EQ {&TABLENAME}.ord-no   */
/*                     AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})): */
/*                                                                         */
/*   {&TABLENAME}.ord-no = {&TABLENAME}.ord-no + 1.                        */
/* END.                                                                    */

FOR EACH cust NO-LOCK
    WHERE cust.company EQ {&TABLENAME}.company
      AND cust.cust-no EQ {&TABLENAME}.cust-no:

  ASSIGN
   {&TABLENAME}.cust-name = cust.name
   {&TABLENAME}.addr[1]   = cust.addr[1]
   {&TABLENAME}.addr[2]   = cust.addr[2]
   {&TABLENAME}.city      = cust.city
   {&TABLENAME}.state     = cust.state
   {&TABLENAME}.zip       = cust.zip.

  FOR EACH soldto NO-LOCK
      WHERE soldto.company EQ cust.company
        AND soldto.cust-no EQ cust.cust-no
        AND soldto.sold-id EQ {&TABLENAME}.sold-id
      USE-INDEX sold-id:

    ASSIGN
     {&TABLENAME}.sold-name    = soldto.sold-name
     {&TABLENAME}.sold-addr[1] = soldto.sold-addr[1]
     {&TABLENAME}.sold-addr[2] = soldto.sold-addr[2]
     {&TABLENAME}.sold-city    = soldto.sold-city
     {&TABLENAME}.sold-state   = soldto.sold-state
     {&TABLENAME}.sold-zip     = soldto.sold-zip.

    LEAVE.
  END.

  LEAVE.
END.

RUN oe/closkids.p (ROWID({&TABLENAME})).



DO li = 1 TO EXTENT({&TABLENAME}.sman):
  IF {&TABLENAME}.sman[li] EQ "" THEN
  DO:
    ASSIGN
     {&TABLENAME}.sname[li]  = ""
     {&TABLENAME}.s-pct[li]  = 0
     {&TABLENAME}.s-comm[li] = 0.

    IF li EQ 1 THEN
       {&TABLENAME}.t-fuel = 0.
  END.
END.

FOR EACH b-oe-ordl NO-LOCK
    WHERE b-oe-ordl.company EQ {&TABLENAME}.company
      AND b-oe-ordl.ord-no  EQ {&TABLENAME}.ord-no:

  FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.

  IF AVAIL oe-ordl THEN DO:
    ASSIGN 
        oe-ordl.cust-no = {&TABLENAME}.cust-no
        oe-ordl.ship-id = {&TABLENAME}.ship-id.

    IF {&TABLENAME}.t-freight NE old-{&TABLENAME}.t-freight AND
       old-{&TABLENAME}.t-freight NE 0                      THEN
      oe-ordl.t-freight = oe-ordl.t-freight *
                          ({&TABLENAME}.t-freight / old-{&TABLENAME}.t-freight).

    IF oe-ordl.t-freight EQ ? THEN oe-ordl.t-freight = 0.
  END.
END.

IF {&TABLENAME}.t-freight NE old-{&TABLENAME}.t-freight THEN
  RUN oe/ordfrate.p (ROWID({&TABLENAME})).

RUN oe/calcordt.p (ROWID({&TABLENAME})).

IF oeuserid-log THEN 
    ASSIGN 
        {&TABLENAME}.user-id = USERID("ASI")
        {&TABLENAME}.updated-id = USERID("ASI")
        .

FOR EACH oe-rel
    WHERE oe-rel.company EQ {&TABLENAME}.company
      AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no
    NO-LOCK:

  FIND b-oe-rel WHERE ROWID(b-oe-rel) EQ rowid(oe-rel) EXCLUSIVE NO-ERROR NO-WAIT.

  IF AVAIL b-oe-rel THEN
  DO:
     b-oe-rel.cust-no = {&TABLENAME}.cust-no.
     RELEASE b-oe-rel.
  END.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

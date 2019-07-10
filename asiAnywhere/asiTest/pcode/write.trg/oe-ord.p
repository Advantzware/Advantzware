&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-ord

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.
{custom/globdefs.i NEW}
{methods/triggers/write.i}



{sys/inc/var.i NEW SHARED}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER b-oe-ordl FOR oe-ordl.
DEF BUFFER oe-ord-close-checked FOR reftable.

DEF VAR li AS INT NO-UNDO.


DISABLE TRIGGERS FOR LOAD OF oe-rel.
DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.
DISABLE TRIGGERS FOR LOAD OF oe-ord-close-checked.
ASSIGN
 cocode = g_company
 locode = g_loc.
 


{sys/inc/oeuserid.i}

IF {&TABLENAME}.company NE ""   AND
   {&TABLENAME}.ord-no NE 0     AND
   old-{&TABLENAME}.ord-no EQ 0 THEN
DO WHILE CAN-FIND(FIRST b-{&TABLENAME}
                  WHERE b-{&TABLENAME}.company EQ {&TABLENAME}.company
                    AND b-{&TABLENAME}.ord-no  EQ {&TABLENAME}.ord-no
                    AND ROWID(b-{&TABLENAME})  NE ROWID({&TABLENAME})):

  {&TABLENAME}.ord-no = {&TABLENAME}.ord-no + 1.
END.

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

IF {&TABLENAME}.stat NE "C" AND old-{&TABLENAME}.stat EQ "C" THEN DO:
  CREATE oe-ord-close-checked.
  ASSIGN
   oe-ord-close-checked.reftable = "oe-ord.close-checked"
   oe-ord-close-checked.company  = STRING({&TABLENAME}.company,"x(10)") +
                                   STRING({&TABLENAME}.ord-no,"9999999999")
   oe-ord-close-checked.loc      = USERID("nosweat")
   oe-ord-close-checked.code     = STRING(TODAY,"99/99/9999")
   oe-ord-close-checked.code2    = STRING(TIME,"99999")
   oe-ord-close-checked.val[3]   = 1.
END.

DO li = 1 TO EXTENT({&TABLENAME}.sman):
  IF {&TABLENAME}.sman[li] EQ "" THEN
    ASSIGN
     {&TABLENAME}.sname[li]  = ""
     {&TABLENAME}.s-pct[li]  = 0
     {&TABLENAME}.s-comm[li] = 0.
END.

FOR EACH b-oe-ordl NO-LOCK
    WHERE b-oe-ordl.company EQ {&TABLENAME}.company
      AND b-oe-ordl.ord-no  EQ {&TABLENAME}.ord-no:

  FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(b-oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.

  IF AVAIL oe-ordl THEN DO:
    oe-ordl.cust-no = {&TABLENAME}.cust-no.

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

/*IF oeuserid-log THEN {&TABLENAME}.user-id = USERID("nosweat").*/

FOR EACH oe-rel
    WHERE oe-rel.company EQ {&TABLENAME}.company
      AND oe-rel.ord-no  EQ {&TABLENAME}.ord-no:
  oe-rel.cust-no = {&TABLENAME}.cust-no.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

&Scoped-define ACTION UPDATE
&Scoped-define DBNAME ASI
&Scoped-define TABLENAME quotehd

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

ASSIGN
   {&TABLENAME}.upd-date = TODAY 
   {&TABLENAME}.upd-time = TIME
   {&TABLENAME}.upd-user = USERID("nosweat").

IF {&TABLENAME}.company NE ""     AND
   {&TABLENAME}.cust-no NE "TEMP" THEN DO:

  IF {&TABLENAME}.cust-no NE old-{&TABLENAME}.cust-no THEN
  FIND FIRST cust NO-LOCK
      WHERE cust.company EQ {&TABLENAME}.company
        AND cust.cust-no EQ {&TABLENAME}.cust-no
      NO-ERROR.

  IF AVAIL cust THEN
    ASSIGN
     {&TABLENAME}.billto[1] = cust.name
     {&TABLENAME}.billto[2] = cust.addr[1]
     {&TABLENAME}.billto[3] = cust.addr[2]
     {&TABLENAME}.billto[4] = cust.city + ", " + cust.state + "  " + cust.zip.

  IF {&TABLENAME}.cust-no NE old-{&TABLENAME}.cust-no OR
     {&TABLENAME}.ship-id NE old-{&TABLENAME}.ship-id THEN
  FIND FIRST shipto NO-LOCK
      WHERE shipto.company EQ {&TABLENAME}.company
        AND shipto.cust-no EQ {&TABLENAME}.cust-no
        AND shipto.ship-id EQ {&TABLENAME}.ship-id
      NO-ERROR.

  IF AVAIL shipto THEN
    ASSIGN
     {&TABLENAME}.shipto[1] = shipto.ship-name
     {&TABLENAME}.shipto[2] = shipto.ship-addr[1]
     {&TABLENAME}.shipto[3] = shipto.ship-addr[2]
     {&TABLENAME}.shipto[4] = shipto.ship-city + ", " + shipto.ship-state +
                              "  " + shipto.ship-zip.

  IF {&TABLENAME}.cust-no NE old-{&TABLENAME}.cust-no OR
     {&TABLENAME}.sold-id NE old-{&TABLENAME}.sold-id THEN
  FIND FIRST soldto NO-LOCK
      WHERE soldto.company EQ {&TABLENAME}.company
        AND soldto.cust-no EQ {&TABLENAME}.cust-no
        AND soldto.sold-id EQ {&TABLENAME}.sold-id
      NO-ERROR.

  IF AVAIL soldto THEN
    ASSIGN
     {&TABLENAME}.soldto[1] = soldto.sold-name
     {&TABLENAME}.soldto[2] = soldto.sold-addr[1]
     {&TABLENAME}.soldto[3] = soldto.sold-addr[2]
     {&TABLENAME}.soldto[4] = soldto.sold-city + ", " + soldto.sold-state +
                              "  " + soldto.sold-zip.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

RETURN.

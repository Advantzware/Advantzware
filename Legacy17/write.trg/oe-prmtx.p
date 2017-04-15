&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME oe-prmtx

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

DEF VAR li AS INT NO-UNDO.
DEF VAR ldt AS DATE NO-UNDO.

DEF TEMP-TABLE w-{&TABLENAME} FIELD qty      AS DEC
                              FIELD uom      AS CHAR
                              FIELD price    AS DEC
                              FIELD discount AS DEC.

IF {&TABLENAME}.company NE "" THEN DO:
  IF {&TABLENAME}.rec_key NE "" THEN DO:
/*     ldt = DATE(INT(SUBSTR({&TABLENAME}.i-no,105,2)),           */
/*                INT(SUBSTR({&TABLENAME}.i-no,107,2)),           */
/*                INT(SUBSTR({&TABLENAME}.i-no,101,4))) NO-ERROR. */
/*     IF ERROR-STATUS:ERROR OR ldt EQ ? THEN ldt = 01/01/0001.   */
/*                                                                */
/*     FIND FIRST reftable                                        */
/*         WHERE reftable.rec_key  EQ {&TABLENAME}.rec_key        */
/*           AND reftable.company  EQ "{&TABLENAME}"              */
/*         USE-INDEX rec_key EXCLUSIVE-LOCK NO-ERROR.             */
/*                                                                */
/*     IF NOT AVAIL reftable THEN DO:                             */
/*       CREATE reftable.                                         */
/*       ASSIGN                                                   */
/*        reftable.rec_key  = {&TABLENAME}.rec_key                */
/*        reftable.company  = "{&TABLENAME}".                     */
/*     END.                                                       */
/*     reftable.code = STRING(ldt,"99/99/9999").                  */
/*                                                                */
/*     {&TABLENAME}.i-no = STRING({&TABLENAME}.i-no,"x(100)"). */
    IF {&TABLENAME}.i-no NE "" THEN DO:
      FIND FIRST itemfg WHERE
           itemfg.company EQ {&TABLENAME}.company AND
           itemfg.i-no EQ STRING({&TABLENAME}.i-no,"x(15)")
           NO-LOCK NO-ERROR.

      IF AVAIL itemfg THEN {&TABLENAME}.procat = itemfg.procat.
    END.

/*     {&TABLENAME}.i-no = STRING({&TABLENAME}.i-no,"x(100)") + */
/*                         SUBSTR(reftable.code,7,4)          + */
/*                         SUBSTR(reftable.code,1,2)          + */
/*                         SUBSTR(reftable.code,4,2).           */
/*                                                              */
/*                                                              */
  END.

  IF {&TABLENAME}.cust-no NE "" THEN
     FOR EACH cust OF {&TABLENAME} NO-LOCK:
         {&TABLENAME}.custype = cust.type.
     END.
END.

DO li = 1 TO EXTENT({&TABLENAME}.qty):
  IF {&TABLENAME}.qty[li] NE 0 THEN DO:
    CREATE w-{&TABLENAME}.
    ASSIGN
     w-{&TABLENAME}.qty      = {&TABLENAME}.qty[li]
     w-{&TABLENAME}.uom      = {&TABLENAME}.uom[li]
     w-{&TABLENAME}.price    = {&TABLENAME}.price[li]
     w-{&TABLENAME}.discount = {&TABLENAME}.discount[li].
  END.
END.

ASSIGN
 {&TABLENAME}.qty      = 0
 {&TABLENAME}.uom      = ""
 {&TABLENAME}.price    = 0
 {&TABLENAME}.discount = 0
 li                    = 0.

FOR EACH w-{&TABLENAME}
    WHERE w-{&TABLENAME}.qty GT 0
    BREAK BY w-{&TABLENAME}.qty: 
  li = li + 1.

  IF li LE EXTENT({&TABLENAME}.qty) THEN
    ASSIGN
     {&TABLENAME}.qty[li]      = IF LAST(w-{&TABLENAME}.qty) THEN 99999999
                                 ELSE w-{&TABLENAME}.qty
     {&TABLENAME}.uom[li]      = w-{&TABLENAME}.uom
     {&TABLENAME}.price[li]    = w-{&TABLENAME}.price
     {&TABLENAME}.discount[li] = w-{&TABLENAME}.discount.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

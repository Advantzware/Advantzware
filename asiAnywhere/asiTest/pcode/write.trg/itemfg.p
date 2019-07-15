&Scoped-define ACTION UPDATE
&Scoped-define DBNAME PDBNAME('ASI')
&Scoped-define TABLENAME itemfg

TRIGGER PROCEDURE FOR WRITE OF {&TABLENAME} OLD BUFFER old-{&TABLENAME}.

{methods/triggers/write.i}

/*{custom/globdefs.i}*/

{sys/inc/VAR.i NEW SHARED}

DEF VAR K_FRAC AS DEC INIT 6.25 NO-UNDO.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DISABLE TRIGGERS FOR LOAD OF eb.

ASSIGN
 cocode = g_company
 locode = g_loc.

{ce/msfcalc.i}

{sys/inc/oereordr.i}
    
{sys/inc/f16to32.i}

IF TRIM({&TABLENAME}.type-code) EQ "" THEN {&TABLENAME}.type-code = "O".

IF {&TABLENAME}.i-code EQ "C" THEN {&TABLENAME}.prod-uom = "M".

{&TABLENAME}.cust-name = "".
IF {&TABLENAME}.cust-no NE "" THEN
FOR EACH cust
    WHERE cust.company EQ {&TABLENAME}.company
      AND cust.cust-no EQ {&TABLENAME}.cust-no
    NO-LOCK:
  {&TABLENAME}.cust-name = cust.name.
  LEAVE.
END.

IF TRIM({&TABLENAME}.pur-uom) EQ "" THEN
  IF {&TABLENAME}.pur-man THEN DO:
    FOR EACH po-ordl
        WHERE po-ordl.company   EQ {&TABLENAME}.company
          AND po-ordl.i-no      EQ {&TABLENAME}.i-no
          AND po-ordl.item-type EQ NO
        NO-LOCK
        BY po-ordl.po-no DESC:
      LEAVE.
    END.
  
    {&TABLENAME}.pur-uom = IF AVAIL po-ordl THEN po-ordl.pr-qty-uom ELSE "EA".
  END.

  ELSE {&TABLENAME}.pur-uom = {&TABLENAME}.prod-uom.

RUN fg/calcqono.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.q-ono).


RUN fg/calcqa&b.p (ROWID({&TABLENAME}), OUTPUT {&TABLENAME}.q-alloc,
                                        OUTPUT {&TABLENAME}.q-back).

IF {&TABLENAME}.weight-100 EQ 0 THEN {&TABLENAME}.weight-100 = 1.

ASSIGN
 {&TABLENAME}.q-avail = {&TABLENAME}.q-onh +
                        (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE {&TABLENAME}.q-ono) -
                        {&TABLENAME}.q-alloc.
 {&TABLENAME}.est-no  = FILL(" ",8 - LENGTH(TRIM({&TABLENAME}.est-no))) +
                        TRIM({&TABLENAME}.est-no).

IF {&TABLENAME}.cust-no NE old-{&TABLENAME}.cust-no THEN
FOR EACH cust
    WHERE cust.company EQ {&TABLENAME}.company
      AND cust.cust-no EQ {&TABLENAME}.cust-no
    NO-LOCK:
  {&TABLENAME}.taxable = AVAIL cust AND cust.sort EQ "Y" AND cust.tax-gr NE "".
  LEAVE.
END.

IF {&TABLENAME}.i-no        NE ""                   AND
   old-{&TABLENAME}.i-no    NE ""                   AND
   (old-{&TABLENAME}.i-no   NE {&TABLENAME}.i-no OR 
    old-{&TABLENAME}.procat NE {&TABLENAME}.procat) THEN
FOR EACH oe-prmtx
    WHERE oe-prmtx.company            EQ old-{&TABLENAME}.company
      AND oe-prmtx.i-no               BEGINS old-{&TABLENAME}.i-no
      AND SUBSTR(oe-prmtx.i-no,1,100) EQ old-{&TABLENAME}.i-no
    EXCLUSIVE-LOCK:

  ASSIGN
     SUBSTR(oe-prmtx.i-no,1,100) = STRING({&TABLENAME}.i-no,"X(100)")
     oe-prmtx.procat             = {&TABLENAME}.procat.
  
END.

IF {&TABLENAME}.isaset AND {&TABLENAME}.alloc THEN
FOR EACH fg-set
    WHERE fg-set.company EQ {&TABLENAME}.company
      AND fg-set.set-no  EQ {&TABLENAME}.i-no
    NO-LOCK
    BREAK BY fg-set.set-no:
  
  IF FIRST(fg-set.set-no) AND LAST(fg-set.set-no) AND
     fg-set.part-no EQ {&TABLENAME}.i-no          THEN {&TABLENAME}.alloc = NO.
END.

/* itemfg-ink update
IF {&TABLENAME}.est-no NE ""                      AND
   {&TABLENAME}.est-no NE old-{&TABLENAME}.est-no THEN
FOR EACH est
    WHERE est.company EQ cocode
       AND est.est-no  EQ {&TABLENAME}.est-no
    NO-LOCK:

END.
*/

IF {&TABLENAME}.est-no NE ""                            AND
   ({&TABLENAME}.est-no NE old-{&TABLENAME}.est-no OR
    {&TABLENAME}.t-wid  EQ 0                       OR
    {&TABLENAME}.t-len  EQ 0                       OR
    {&TABLENAME}.t-sqin EQ 0                       OR
    {&TABLENAME}.t-sqft EQ 0)                           THEN DO:

  IF {&TABLENAME}.isaset AND
     CAN-FIND(FIRST fg-set WHERE fg-set.company EQ cocode
                             AND fg-set.set-no  EQ {&TABLENAME}.i-no
                             AND fg-set.part-no NE fg-set.set-no)
  THEN RUN fg/updsetdm.p (RECID({&TABLENAME})).

  ELSE DO:
    FIND FIRST est
        WHERE est.company EQ cocode
          AND est.est-no  EQ {&TABLENAME}.est-no
        NO-LOCK NO-ERROR.
    IF AVAIL est THEN
    FOR EACH eb NO-LOCK
        WHERE eb.company  EQ est.company
          AND eb.est-no   EQ est.est-no
          AND eb.stock-no EQ {&TABLENAME}.i-no
        BY eb.form-no:
      LEAVE.
    END.
    IF NOT AVAIL eb                             AND
       AVAIL est                                AND
       (est.est-type EQ 1 OR est.est-type EQ 5) THEN
    FIND FIRST eb
        WHERE eb.est-no  EQ est.est-no
          AND eb.form-no NE 0
        NO-LOCK NO-ERROR.
    IF AVAIL eb THEN
    IF {&TABLENAME}.isaset THEN RUN fg/updsetdm.p (RECID(eb)).
    ELSE DO:
      {sys/inc/updfgdim.i eb}
    END.
  END.
END.

IF CAN-FIND(FIRST asi._file WHERE asi._file._file-name EQ "cust-part") THEN
  RUN custom/updcpart.p ({&TABLENAME}.company,
                         {&TABLENAME}.i-no,
                         {&TABLENAME}.cust-no,
                         {&TABLENAME}.part-no).

RUN fg/prodcode.p (ROWID({&TABLENAME})).

IF {&TABLENAME}.company NE "" AND
   {&TABLENAME}.i-no    NE "" THEN DO:

  FIND FIRST reftable
      WHERE reftable.reftable EQ "FGSTATUS"
        AND reftable.company  EQ (IF old-{&TABLENAME}.company NE "" THEN
                                    old-{&TABLENAME}.company
                                  ELSE {&TABLENAME}.company)
        AND reftable.loc      EQ ""
        AND reftable.code     EQ (IF old-{&TABLENAME}.i-no NE "" THEN
                                    old-{&TABLENAME}.i-no
                                  ELSE {&TABLENAME}.i-no)
      NO-ERROR.

  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "FGSTATUS"
     reftable.loc      = ""
     reftable.code2    = "A".
  END.

  ASSIGN
   reftable.company = {&TABLENAME}.company
   reftable.code    = {&TABLENAME}.i-no.
END.

IF {&TABLENAME}.isaset                          AND
   {&TABLENAME}.i-no NE ""                      AND
   {&TABLENAME}.alloc NE old-{&TABLENAME}.alloc THEN
FOR EACH eb
    WHERE eb.company  EQ {&TABLENAME}.company
      AND eb.stock-no EQ {&TABLENAME}.i-no:
  eb.set-is-assembled = NOT {&TABLENAME}.alloc.
END.

/* Clear out any error-status from find with no-error that is false */
DEF VAR ll-error AS LOG NO-UNDO.
ll-error = YES NO-ERROR.

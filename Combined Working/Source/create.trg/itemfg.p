&Scoped-define TABLENAME itemfg

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

DEF BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEF BUFFER fg-status FOR reftable.
DEF BUFFER b-fg-status FOR reftable.

DEF VAR cPgmStack AS CHAR NO-UNDO.
{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

cPgmStack = PROGRAM-NAME(1) 
        + (IF PROGRAM-NAME(2) NE ? THEN "," + PROGRAM-NAME(2) ELSE "")
        + (IF PROGRAM-NAME(3) NE ? THEN "," + PROGRAM-NAME(3) ELSE "")
        + (IF PROGRAM-NAME(4) NE ? THEN "," + PROGRAM-NAME(4) ELSE "")
        + (IF PROGRAM-NAME(5) NE ? THEN "," + PROGRAM-NAME(5) ELSE "")
        + (IF PROGRAM-NAME(6) NE ? THEN "," + PROGRAM-NAME(6) ELSE "")
        + (IF PROGRAM-NAME(7) NE ? THEN "," + PROGRAM-NAME(7) ELSE "").
cocode = g_company.

FIND FIRST company
    WHERE company.company EQ cocode
    NO-LOCK NO-ERROR.

FIND FIRST fg-bin
    WHERE fg-bin.company EQ cocode
      AND fg-bin.i-no    EQ ""
      AND fg-bin.loc     EQ g_loc
    USE-INDEX co-ino NO-LOCK NO-ERROR.

FIND FIRST fgcat
    WHERE fgcat.company EQ g_company
    USE-INDEX fgcat NO-LOCK NO-ERROR.

ASSIGN
 {&TABLENAME}.def-loc      = g_loc
 {&TABLENAME}.def-loc-bin  = IF AVAIL fg-bin THEN fg-bin.loc-bin ELSE ""
 {&TABLENAME}.procat       = IF AVAIL fgcat THEN fgcat.procat ELSE ""
 {&TABLENAME}.pur-man      = NO
 {&TABLENAME}.i-code       = "C"
 {&TABLENAME}.curr-code[1] = IF AVAIL company THEN company.curr-code ELSE "USD"
 {&TABLENAME}.i-no = "               " + {&TABLENAME}.rec_key
 {&TABLENAME}.prod-uom     = "M"
 {&TABLENAME}.stat         = "A"
 {&tablename}.spare-char-5 = USERID("Nosweat") + " " + STRING(TODAY) 
                             + " " + STRING(TIME, "hh:mm") 
                             + " " + cPgmStack.

{sys/inc/fgmaster.i}

IF fgmaster-cha NE "FGITEM" THEN
FIND FIRST b-{&TABLENAME} NO-LOCK
    WHERE b-{&TABLENAME}.company EQ cocode
      AND b-{&TABLENAME}.i-no    EQ fgmaster-cha
    NO-ERROR.

IF AVAIL b-{&TABLENAME} THEN DO:
  ASSIGN
   {&TABLENAME}.procat         = b-{&TABLENAME}.procat
   {&TABLENAME}.class          = b-{&TABLENAME}.class
   {&TABLENAME}.sell-uom       = b-{&TABLENAME}.sell-uom
   {&TABLENAME}.prod-uom       = b-{&TABLENAME}.prod-uom
   {&TABLENAME}.curr-code[1]   = b-{&TABLENAME}.curr-code[1]
   {&TABLENAME}.def-loc        = b-{&TABLENAME}.def-loc
   {&TABLENAME}.def-loc-bin    = b-{&TABLENAME}.def-loc-bin
   {&TABLENAME}.pur-man        = b-{&TABLENAME}.pur-man
   {&TABLENAME}.i-code         = b-{&TABLENAME}.i-code
   {&TABLENAME}.stocked        = b-{&TABLENAME}.stocked
   {&TABLENAME}.cc-code        = b-{&TABLENAME}.cc-code
   {&TABLENAME}.prod-code      = b-{&TABLENAME}.prod-code
   {&TABLENAME}.taxable        = b-{&TABLENAME}.taxable
   {&TABLENAME}.frt-class      = b-{&TABLENAME}.frt-class
   {&TABLENAME}.frt-class-dscr = b-{&TABLENAME}.frt-class
   {&TABLENAME}.vend-no        = b-{&TABLENAME}.vend-no
   {&TABLENAME}.vend2-no       = b-{&TABLENAME}.vend2-no
   {&TABLENAME}.ord-policy     = b-{&TABLENAME}.ord-policy
   {&TABLENAME}.alloc          = b-{&TABLENAME}.alloc
   {&TABLENAME}.stat           = b-{&TABLENAME}.stat.

 
END.

IF {&TABLENAME}.def-loc EQ ""     AND
   {&TABLENAME}.def-loc-bin EQ "" THEN DO:
  FIND FIRST cust
      WHERE cust.company EQ cocode
        AND cust.active  EQ "X"
      NO-LOCK NO-ERROR.
  IF AVAIL cust THEN
  FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
  IF AVAIL shipto THEN
    ASSIGN
     {&TABLENAME}.def-loc     = CAPS(shipto.loc)
     {&TABLENAME}.def-loc-bin = CAPS(shipto.loc-bin).
END.

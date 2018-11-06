&Scoped-define TABLENAME itemfg

TRIGGER PROCEDURE FOR CREATE OF {&TABLENAME}.

{methods/triggers/create.i}

DEFINE BUFFER b-{&TABLENAME} FOR {&TABLENAME}.
DEFINE BUFFER fg-status      FOR reftable.
DEFINE BUFFER b-fg-status    FOR reftable.

{custom/globdefs.i}

{sys/inc/var.i NEW SHARED}

cocode = g_company.

FIND FIRST company NO-LOCK
     WHERE company.company EQ cocode
     NO-ERROR.

FIND FIRST fg-bin NO-LOCK
     WHERE fg-bin.company EQ cocode
       AND fg-bin.i-no    EQ ""
       AND fg-bin.loc     EQ g_loc
     USE-INDEX co-ino
     NO-ERROR.

FIND FIRST fgcat
    WHERE fgcat.company EQ g_company
    USE-INDEX fgcat NO-LOCK
    NO-ERROR.

FIND FIRST cust NO-LOCK
    WHERE cust.company EQ g_company
      AND cust.active  EQ "X"
      NO-ERROR.
IF AVAILABLE cust THEN 
{&TABLENAME}.cust-no = cust.cust-no.

ASSIGN
    {&TABLENAME}.company      = g_company
    {&TABLENAME}.loc          = g_loc
    {&TABLENAME}.def-loc      = g_loc
    {&TABLENAME}.def-loc-bin  = IF AVAILABLE fg-bin THEN fg-bin.loc-bin ELSE ""
    {&TABLENAME}.procat       = IF AVAILABLE fgcat THEN fgcat.procat ELSE ""
    {&TABLENAME}.pur-man      = NO
    {&TABLENAME}.i-code       = "C"
    {&TABLENAME}.curr-code[1] = IF AVAILABLE company THEN company.curr-code ELSE "USD"
    {&TABLENAME}.i-no         = "               " + {&TABLENAME}.rec_key
    {&TABLENAME}.prod-uom     = "M"
    {&TABLENAME}.stat         = "A"
    {&TABLENAME}.exempt-disc  = NO
    {&TABLENAME}.setupDate    = TODAY
    .

IF {&TABLENAME}.def-loc     EQ "" AND
   {&TABLENAME}.def-loc-bin EQ "" THEN 
DO:
    FIND FIRST cust NO-LOCK
         WHERE cust.company EQ cocode
           AND cust.active  EQ "X"
         NO-ERROR.
    IF AVAILABLE cust THEN
    FIND FIRST shipto OF cust NO-LOCK NO-ERROR.
    IF AVAILABLE shipto THEN
    ASSIGN
        {&TABLENAME}.def-loc     = CAPS(shipto.loc)
        {&TABLENAME}.def-loc-bin = CAPS(shipto.loc-bin)
        .
END.

{sys/inc/fgmaster.i}

IF fgmaster-cha NE "FGITEM" THEN
    FIND FIRST b-{&TABLENAME} NO-LOCK
         WHERE b-{&TABLENAME}.company EQ cocode
           AND b-{&TABLENAME}.i-no    EQ fgmaster-cha
         NO-ERROR.
IF AVAILABLE b-{&TABLENAME} THEN DO:
    ASSIGN
        {&TABLENAME}.procat         = b-{&TABLENAME}.procat
        {&TABLENAME}.class          = b-{&TABLENAME}.class
        {&TABLENAME}.sell-uom       = b-{&TABLENAME}.sell-uom
        {&TABLENAME}.stocked        = b-{&TABLENAME}.stocked
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
        {&TABLENAME}.stat           = b-{&TABLENAME}.stat
        {&TABLENAME}.ship-meth      = b-{&TABLENAME}.ship-meth
        .
    IF b-{&TABLENAME}.cust-no NE "" THEN
    {&TABLENAME}.cust-no = b-{&TABLENAME}.cust-no. 
END.

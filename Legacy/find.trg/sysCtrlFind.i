/* sysCtrlFind.i */

DEFINE VARIABLE cProgram AS CHARACTER NO-UNDO.
DEFINE VARIABLE cUserID  AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx      AS INTEGER   NO-UNDO INITIAL 1.

DO WHILE TRUE:
    idx = idx + 1.
    /* all done with program stack */
    IF PROGRAM-NAME(idx) EQ ? THEN LEAVE.
    /* do nothing if inside sys-ctrl window */
    IF INDEX(PROGRAM-NAME(idx),"sys-ctrl.")  NE 0 THEN RETURN.
    /* skip if a sub routine call */
    IF NUM-ENTRIES(PROGRAM-NAME(idx)," ")    GT 1 THEN NEXT.
    /* any of the main launch programs */
    IF INDEX(PROGRAM-NAME(idx),"asiLogin.")  NE 0 OR
       INDEX(PROGRAM-NAME(idx),"mainMenu2.") NE 0 OR
       INDEX(PROGRAM-NAME(idx),"nosweat.")   NE 0 OR
       INDEX(PROGRAM-NAME(idx),"nk1look.")   NE 0 THEN DO:
        IF cProgram EQ "" THEN
        cProgram = PROGRAM-NAME(idx).
        LEAVE.
    END. /* if ... leave */
    /* grab program making sys-ctrl find */
    cProgram = PROGRAM-NAME(idx).
END. /* while true */

cUserID = USERID("ASI").
IF cUserID EQ "" THEN cUserID = "NoSweat".
/* create .dat for use in system/sysCtrlUsage.w */
OUTPUT TO VALUE("users\" + cUserID + "\sysCtrlFind.dat") APPEND.
EXPORT
    cProgram
    {&tableName}.company
    {&tableName}.module
    {&tableName}.name
    {&tableName}.char-fld
    {&tableName}.date-fld
    {&tableName}.dec-fld
    {&tableName}.int-fld
    {&tableName}.log-fld
    {&tableName}.descrip
    USERID(LDBNAME(1))
    NOW
&IF "{&tableName}" EQ "sys-ctrl-shipto" &THEN
    {&tableName}.category
    {&tableName}.cust-vend
    {&tableName}.cust-vend-no
    {&tableName}.seqNo
    {&tableName}.ship-id
    {&tableName}.subCategory
    {&tableName}.sysCtrlID
    {&tableName}.typeCode
&ENDIF
    .
OUTPUT CLOSE.

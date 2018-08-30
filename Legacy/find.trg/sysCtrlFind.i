/* sysCtrlFind.i */

DEFINE VARIABLE hSysCtrlUsage AS HANDLE    NO-UNDO.
DEFINE VARIABLE cStackTrace   AS CHARACTER NO-UNDO.
DEFINE VARIABLE idx           AS INTEGER   NO-UNDO INITIAL 1.

/* build stack trace */
DO WHILE TRUE:
    idx = idx + 1.
    /* all done with program stack */
    IF PROGRAM-NAME(idx) EQ ? THEN LEAVE.
    /* do nothing if inside sys-ctrl window */
    IF INDEX(PROGRAM-NAME(idx),"sys-ctrl.") NE 0 THEN RETURN.
    cStackTrace = cStackTrace + PROGRAM-NAME(idx) + ",".
END. /* while true */
cStackTrace = TRIM(cStackTrace,",").

/* add record to session temp-table ttSysCtrlUsage */
RUN spCreateSysCtrlUsage (
    {&tableName}.company,
    {&tableName}.module,
    {&tableName}.name,
    {&tableName}.char-fld,
    {&tableName}.date-fld,
    {&tableName}.dec-fld,
    {&tableName}.int-fld,
    {&tableName}.log-fld,
    {&tableName}.descrip,
    NOW,
&IF "{&tableName}" EQ "sys-ctrl-shipto" &THEN
    {&tableName}.category,
    {&tableName}.cust-vend,
    {&tableName}.cust-vend-no,
    {&tableName}.seqNo,
    {&tableName}.ship-id,
    {&tableName}.subCategory,
    {&tableName}.sysCtrlID,
    {&tableName}.typeCode,
&ELSE
    "",?,"",0,"","",0,"",
&ENDIF
    cStackTrace
    ).

/* if SysCtrlUsage viewer open, auto refresh */
hSysCtrlUsage = DYNAMIC-FUNCTION("sfGetSysCtrlUsageHandle").
IF VALID-HANDLE(hSysCtrlUsage) THEN
RUN pGetSysCtrlUsage IN hSysCtrlUsage.

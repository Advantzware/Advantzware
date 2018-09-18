/* sysCtrlFind.i */

DEFINE VARIABLE hSysCtrlUsage      AS HANDLE    NO-UNDO.
DEFINE VARIABLE cStackTrace        AS CHARACTER NO-UNDO.
DEFINE VARIABLE iSecurityLevelUser AS INTEGER   NO-UNDO.
DEFINE VARIABLE idx                AS INTEGER   NO-UNDO INITIAL 1.
DEFINE VARIABLE cSuperProcedures   AS CHARACTER NO-UNDO.

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

&IF "{&tableName}" EQ "sys-ctrl-shipto" &THEN
/* prevent find trigger, already occured and recorded */
DISABLE TRIGGERS FOR DUMP OF sys-ctrl.
/* get parent of sys-ctrl-shipto */
FIND FIRST sys-ctrl NO-LOCK
     WHERE sys-ctrl.company EQ {&tableName}.company
       AND sys-ctrl.name    EQ {&tableName}.name
     NO-ERROR.
/* should never fail, but never say never */
IF NOT AVAILABLE sys-ctrl THEN RETURN.
iSecurityLevelUser = sys-ctrl.securityLevelUser.
&ELSE
iSecurityLevelUser = {&tableName}.securityLevelUser.      
&ENDIF

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
    cStackTrace,
    iSecurityLevelUser
    ).

/* if SysCtrlUsage viewer open, auto refresh */
hSysCtrlUsage = DYNAMIC-FUNCTION("sfGetSysCtrlUsageHandle").
IF VALID-HANDLE(hSysCtrlUsage) THEN
RUN pGetSysCtrlUsage IN hSysCtrlUsage.

/*------------------------------------------------------------------------
  File:                 asiInstaller.w
  Description:          utility to create files for patch distribution
  Input Parameters:     (none)
  Output Parameters:    (none)
  Author:               MYT
  Created:              04/05/19
  Change History:
------------------------------------------------------------------------*/
&SCOPED-DEFINE cDir N:\Repositories\Advantzware
&SCOPED-DEFINE cTarget N:\Repositories\Advantzware\Deployment\Patch\Admin\EnvAdmin

DEF VAR cConnectDb AS CHAR NO-UNDO.
DEF VAR cConnectAud AS CHAR NO-UNDO.
DEF VAR cDbName AS CHAR NO-UNDO FORMAT "x(11)" LABEL "  Enter the DB name for compile".
DEF VAR cAudName AS CHAR NO-UNDO.
DEF VAR cRaw AS CHAR NO-UNDO.
DEF VAR cDbList AS CHAR NO-UNDO FORMAT "x(100)" LABEL "  Choices".
DEF VAR cAudDbList AS CHAR NO-UNDO.
DEF VAR iIndex AS INT NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR cDir AS CHAR NO-UNDO.
DEF VAR cTarget AS CHAR NO-UNDO.
DEF VAR cNewVer AS CHAR NO-UNDO.

INPUT FROM c:\asigui\build\newVer.txt.
IMPORT cNewVer.
INPUT CLOSE.

ASSIGN 
    FILE-INFO:FILE-NAME = "N:\Repositories\Advantzware".
IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
    ASSIGN 
        FILE-INFO:FILE-NAME = "N:\Repository".
    IF FILE-INFO:FULL-PATHNAME EQ ? THEN DO:
        MESSAGE 
            "Unable to locate a source or target directory.  Quitting."
            VIEW-AS ALERT-BOX.
        QUIT.
    END.
END.

ASSIGN 
    cDir = "C:\Asigui\Repositories\Advantzware"
    cTarget = "C:\asigui\Upgrades\PATCH" + cNewVer + "\Admin\EnvAdmin".


INPUT FROM os-dir ("c:\asigui\databases\comp").
REPEAT:
    IMPORT cRaw.
    IF (cRaw BEGINS "asi" OR INDEX(cRaw,"d.lg") NE 0)
    AND INDEX(cRaw,".lg") NE 0 THEN ASSIGN 
        cDbList = cDbList + REPLACE(cRaw,".lg","") + ",".
    ELSE IF (cRaw BEGINS "aud" OR INDEX(cRaw,"a.lg") NE 0)
    AND INDEX(cRaw,".lg") NE 0 THEN ASSIGN 
        cAudDbList = cAudDbList + REPLACE(cRaw,".lg","") + ",".
END.
INPUT CLOSE.
ASSIGN 
    cDbList = TRIM(cDbList,",")
    cAudDbList = TRIM(cAudDbList,",").

ASSIGN 
    cDBname = "COMP" + SUBSTRING(cNewVer,1,2) +
              SUBSTRING(cNewVer,4,2) +
              SUBSTRING(cNewVer,7,2) + "d"
    cConnectDb = "-db c:\asigui\databases\comp\" + cDbName + " -1 -ld ASI"
    cConnectAud = "-db c:\asigui\databases\comp\" + replace(cDbName,"d","a") + " -1 -ld Audit"
    propath = cDir + "\Deployment\BuildScript\ToCompile," + cDir + "\Source," + cDir + "\Resources," + propath
    .

COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\asiDbMaint.w") SAVE INTO value(cTarget).
COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\asiLogin.w") SAVE INTO value(cTarget).
COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\asiUpdate.w") SAVE INTO value(cTarget).
COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\asiUpdateDb.w") SAVE INTO value(cTarget).

CONNECT VALUE(cConnectDb).
COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\prerun.p") SAVE INTO value(cTarget).
COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\asiAuditTest.p") SAVE INTO value(cTarget).

CONNECT VALUE(cConnectAud).
COMPILE VALUE(cDir + "\Deployment\BuildScript\ToCompile\asiUpdateEnv.w") SAVE INTO value(cTarget).

QUIT.

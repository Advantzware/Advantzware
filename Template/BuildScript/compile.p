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
&SCOPED-DEFINE cTarget N:\Repositories\Advantzware\Template\Patch\Admin\EnvAdmin

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
    cDir = FILE-INFO:FULL-PATHNAME
    cTarget = cDir + "\Template\Patch\Admin\EnvAdmin".


INPUT FROM os-dir ("c:\asigui\databases\compile").
REPEAT:
    IMPORT cRaw.
    IF cRaw BEGINS "asi"
    AND INDEX(cRaw,".lg") NE 0 THEN ASSIGN 
        cDbList = cDbList + REPLACE(cRaw,".lg","") + ",".
    ELSE IF cRaw BEGINS "aud"
    AND INDEX(cRaw,".lg") NE 0 THEN ASSIGN 
        cAudDbList = cAudDbList + REPLACE(cRaw,".lg","") + ",".
END.
INPUT CLOSE.
ASSIGN 
    cDbList = TRIM(cDbList,",")
    cAudDbList = TRIM(cAudDbList,",").

DISP 
    cDBname SKIP 
    cDbList 
    WITH FRAME a TITLE "DB Selection"
    VIEW-AS DIALOG-BOX THREE-D SIDE-LABELS WIDTH 120.

UPDATE 
    cDBname 
    WITH FRAME a.
    
DO iCtr = 1 TO NUM-ENTRIES(cDbList):
    IF cDbName EQ ENTRY(iCtr,cDbList) THEN ASSIGN 
        iIndex = iCtr.    
END.

IF iIndex = 0 THEN DO:
    MESSAGE 
        "You entered an invalid DB Name.  Quitting"
        VIEW-AS ALERT-BOX.
    QUIT.
END. 
ELSE ASSIGN 
    cConnectDb = "-db c:\asigui\databases\compile\" + cDbName + " -1 -ld ASI"
    cConnectAud = "-db c:\asigui\databases\compile\aud" + SUBSTRING(cDbName,4) + " -1 -ld Audit"
    propath = cDir + "\Template\BuildScript\ToCompile," + cDir + "\Legacy," + cDir + "\Resources," + propath
    .

COMPILE VALUE(cDir + "\Template\BuildScript\ToCompile\asiLogin.w") SAVE INTO value(cTarget).
COMPILE VALUE(cDir + "\Template\BuildScript\ToCompile\asiUpdate.w") SAVE INTO value(cTarget).
COMPILE VALUE(cDir + "\Template\BuildScript\ToCompile\asiUpdateDb.w") SAVE INTO value(cTarget).

CONNECT VALUE(cConnectDb).
COMPILE VALUE(cDir + "\Template\BuildScript\ToCompile\prerun.p") SAVE INTO value(cTarget).

CONNECT VALUE(cConnectAud).
COMPILE VALUE(cDir + "\Template\BuildScript\ToCompile\asiUpdateEnv.w") SAVE INTO value(cTarget).



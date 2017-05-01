/*---------------------------------------------------------------------------*/
/*  File:           getDbConnections.p                                       */
/*  Copyright:      (c)2017 Advanced Software Services, Inc.All rights rsrvd */
/*  Description:    creates multiple db conn .pf files from input            */
/*                                                                           */
/*  Included files:                                                          */
/*  External RUN/CALL:                                                       */
/*  External files:                                                          */
/*                                                                           */
/*  Revision history:   MM/DD/YY    INIT    TKT    Description               */
/*                      04/30/17    MYT            cleanup                   */
/*---------------------------------------------------------------------------*/

DEF VAR cProExe AS CHAR INIT "c:\progress\openedge\bin\prowin32.exe".
DEF VAR cModelDbConnection AS CHAR INIT "-db ASI -1 -ld slave" .
DEF VAR cPatchFolder AS CHAR INIT "P:\asi16ship\patch16.1.4".
DEF VAR cRcodeFolder AS CHAR INIT "P:\asi10test\rco1010".
DEF VAR cPfFile AS CHAR INIT "ASI.pf".
DEF VAR cDbName AS CHAR INIT "ASI".

DEF VAR rename-file AS CHAR NO-UNDO.
DEF VAR df-file-name AS CHAR NO-UNDO.
DEF VAR code-page AS CHAR NO-UNDO.
DEF VAR index-mode AS CHAR NO-UNDO.
DEF VAR debug-mode AS INT NO-UNDO.
DEF VAR cDfFile AS CHAR.
DEF VAR cCodePage AS CHAR.
DEF VAR cIndexMode AS CHAR.
DEF VAR cRenFile AS CHAR.
DEF VAR cIncDebug AS CHAR.
DEF VAR cOsCmd AS CHAR.

ASSIGN
    cDbName = "nosweat"
    cPfFile = "nosweat.pf".
RUN ipBuildCommand.

ASSIGN 
    cDbName = "emptrack"
    cPfFile = "addon\emptrack.pf".
RUN ipBuildCommand.

ASSIGN 
    cDbName = "asi"
    cPfFile = "asi".
RUN ipBuildCommand.

ASSIGN 
    cDbName = ""
    cPfFile = "addon\emptrack.pf".
RUN ipBuildCommand.

PROCEDURE ipBuildCommand:
    ASSIGN 
        debug-mode   = 1
        rename-file  = "wrk\renamefile.rf"
        df-file-name = "delta" + cDbName + ".df"
        code-page    = "8859"
        index-mode   = "active"
        cDfFile = "set DUMP_INC_DFFILE= ":U + df-file-name
        cCodePage = "set DUMP_INC_CODEPAGE=":U + code-page
        cIndexMode = "set DUMP_INC_INDEXMODE=":U + index-mode
        cRenFile = "set DUMP_INC_RENAMEFILE=":U + rename-file
        cIncDebug = "set DUMP_INC_DEBUG=":U + string(debug-mode, ">")
        cOsCmd =  cDfFile + " & " + 
                  cCodePage + " & " + 
                  cIndexMode + " & " + 
                  cRenFile + "&" + 
                  cIncDebug + " & " + 
                  cProExe + 
                  " -b " + " -pf " + cRcodeFolder + "\" + cPfFile + 
                  " -db " + cPatchFolder + "\" + cDbName + 
                  " -1 -ld slave " + 
                  " -p " + cPatchFolder + "\prodict\dump_inc.r" .
    OS-COMMAND VALUE(cOsCmd).
END.

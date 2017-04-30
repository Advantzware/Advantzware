DEF VAR cProExe AS CHAR INIT "c:\progress\openedge\bin\prowin32.exe".


DEFINE VARIABLE rename-file  AS CHARACTER NO-UNDO.
DEFINE VARIABLE df-file-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE code-page    AS CHARACTER NO-UNDO.
DEFINE VARIABLE index-mode   AS CHARACTER NO-UNDO.
DEFINE VARIABLE debug-mode   AS INTEGER   NO-UNDO.

DEF VAR cRcodeFolder AS CHAR INIT "P:\asi10test\rco1010".
DEF VAR cPfFile AS CHAR INIT "ASI.pf".
DEF VAR cModelDbConnection AS CHAR INIT "-db ASI -1 -ld slave" .
DEF VAR cPatchFolder AS CHAR INIT "P:\asi16ship\patch16.1.4".
DEF VAR cDbName AS CHAR INIT "ASI".
DEF VAR cDfFile AS CHAR.
DEF VAR cCodePage AS CHAR.
DEF VAR cIndexMode AS CHAR.
DEF VAR cRenFile AS CHAR.
DEF VAR cIncDebug AS CHAR.
DEF VAR cOsCmd AS CHAR .


/*   ASSIGN debug-mode   = 1                                                    */
/*        rename-file  = "wrk\renamefile.rf"                                    */
/*        df-file-name = "deltaAsi.df"                                          */
/*        code-page    = "8859"                                                 */
/*        index-mode   = "active"                                               */
/*     .                                                                        */
/*                                                                              */
/*                                                                              */
/*                                                                              */
/* ASSIGN                                                                       */
/* cDfFile = "set DUMP_INC_DFFILE= ":U + df-file-name                           */
/* cCodePage = "set DUMP_INC_CODEPAGE=":U + code-page                           */
/* cIndexMode = "set DUMP_INC_INDEXMODE=":U + index-mode                        */
/* cRenFile = "set DUMP_INC_RENAMEFILE=":U + rename-file                        */
/* cIncDebug = "set DUMP_INC_DEBUG=":U + string(debug-mode, ">")                */
/*   .                                                                          */
/*                                                                              */
/* cOsCmd =         cDfFile                                                     */
/*                  + " & " + cCodePage                                         */
/*                  + " & " + cIndexMode                                        */
/*                  + " & " + cRenFile                                          */
/*                  + "&" + cIncDebug                                           */
/*                  + " & " + cProExe + " -b "                                  */
/*                  + " -pf " + cRcodeFolder + "\" + cPfFile                    */
/*                  + " -db " + cPatchFolder + "\" + cDbName + " -1 -ld slave " */
/*                  + " -p " + cPatchFolder + "\prodict\dump_inc.r" .           */
/*                                                                              */
/*                                                                              */
/* MESSAGE cOsCmd                                                               */
/*   VIEW-AS ALERT-BOX INFO BUTTONS OK.                                         */
cDbName = "nosweat".
cPfFile = "nosweat.pf".
RUN buildCommand.


OS-COMMAND VALUE(cOsCmd).


cDbName = "emptrack".
cPfFile = "addon\emptrack.pf".
RUN buildCommand.

OS-COMMAND VALUE(cOsCmd).

cDbName = "asi".
cPfFile = "asi".
RUN buildCommand.

OS-COMMAND VALUE(cOsCmd).

cDbName = "".
cPfFile = "addon\emptrack.pf".
RUN buildCommand.

OS-COMMAND VALUE(cOsCmd).


PROCEDURE buildCommand:
    ASSIGN debug-mode   = 1
       rename-file  = "wrk\renamefile.rf"
       df-file-name = "delta" + cDbName + ".df"
       code-page    = "8859"
       index-mode   = "active"
    .



ASSIGN 
cDfFile = "set DUMP_INC_DFFILE= ":U + df-file-name
cCodePage = "set DUMP_INC_CODEPAGE=":U + code-page
cIndexMode = "set DUMP_INC_INDEXMODE=":U + index-mode
cRenFile = "set DUMP_INC_RENAMEFILE=":U + rename-file
cIncDebug = "set DUMP_INC_DEBUG=":U + string(debug-mode, ">")
  .

cOsCmd =         cDfFile
                 + " & " + cCodePage
                 + " & " + cIndexMode
                 + " & " + cRenFile
                 + "&" + cIncDebug                 
                 + " & " + cProExe + " -b "
                 + " -pf " + cRcodeFolder + "\" + cPfFile
                 + " -db " + cPatchFolder + "\" + cDbName + " -1 -ld slave " 
                 + " -p " + cPatchFolder + "\prodict\dump_inc.r" .

END.

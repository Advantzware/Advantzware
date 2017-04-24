{protools/abhack/ABHackResourcesTT.i &SHARED="SHARED"}
DEFINE INPUT  PARAMETER gphCaller AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER gphMon    AS HANDLE      NO-UNDO. /* monitoring editor handle */

/* 21-SEP-2007 sla: trick to suggest PROCEDUREs in gphCaller 
RUN protools/abhack/ABHackWin.w PERSISTENT SET gphCaller.
*/


DEFINE VARIABLE ghAbhackDbLib AS HANDLE      NO-UNDO.


DEFINE STREAM edtImport.

DEFINE VARIABLE cDummyTestParseBeforeForwardFunc AS CHARACTER   NO-UNDO.


DEFINE TEMP-TABLE ttosFile NO-UNDO LABEL "ttosFile (to load ABHack Global Definition Description files)"
 FIELD cFileName     AS CHARACTER
 FIELD cFullPathName AS CHARACTER
 INDEX cFullPathName IS PRIMARY UNIQUE cFullPathName.

FUNCTION removeLineComments RETURNS CHARACTER
 (pcLine   AS CHARACTER 
 ,piCIndex AS INTEGER ) IN gphCaller.


FUNCTION ProgressErrors RETURNS CHARACTER( ) FORWARD.    
FUNCTION errReturnValue RETURNS CHARACTER( ) FORWARD.    


FUNCTION createTTorDBBuffer RETURNS HANDLE
/* Create a buffer against TT or DB, based on the buffer name and all the resource we have in memory
   for the particular editor */  
  (phEditor     AS HANDLE
  ,pcBufferName AS CHARACTER
  ,pcOptn       AS CHARACTER ) IN gphCaller.


DEFINE VARIABLE cDummyTestParseAfterForwardFunc AS CHARACTER   NO-UNDO.

ON "CLOSE" OF THIS-PROCEDURE DO:
    RUN exitAbhackDbLib.
    DELETE PROCEDURE THIS-PROCEDURE.    
END.

SUBSCRIBE TO "abhackExit" ANYWHERE.
SUBSCRIBE TO "launchAbhackDbLib" IN SOURCE-PROCEDURE.
SUBSCRIBE TO "exitAbhackDbLib" IN SOURCE-PROCEDURE.
IF CONNECTED("abhack") AND NOT VALID-HANDLE(ghAbhackDbLib) THEN RUN launchAbhackDbLib.

PROCEDURE launchAbhackDbLib:    
    RUN exitAbhackDbLib.
    RUN protools/abhack/abhackDbLib.p PERSISTENT SET ghAbhackDbLib.
END PROCEDURE.

PROCEDURE exitAbhackDbLib:    
    IF VALID-HANDLE(ghAbhackDbLib) THEN DELETE PROCEDURE ghAbhackDbLib.    
END PROCEDURE.


PROCEDURE abhackExit:
    APPLY "CLOSE" TO THIS-PROCEDURE.
END PROCEDURE.


PROCEDURE addLikeFields :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor         AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcTableLike      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pittid           AS INTEGER    NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopifldSeq AS INTEGER    NO-UNDO.
/* DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttfld. /* 22-MAY-2007 sla: why input-output table ? */  */

DEFINE VARIABLE hBuffer      AS HANDLE   NO-UNDO.
DEFINE VARIABLE hBufferField AS HANDLE   NO-UNDO.
DEFINE VARIABLE iField       AS INTEGER  NO-UNDO.
DEFINE VARIABLE iNumFields   AS INTEGER  NO-UNDO.

DEFINE BUFFER ttfldFrom FOR ttfld.
DEFINE BUFFER ttfld     FOR ttfld.
DEFINE BUFFER tttt      FOR tttt.

&SCOPED-DEFINE RETURN DO: DELETE WIDGET-POOL.  RETURN.  ERROR-STATUS:ERROR = NO. END.

/* give priority to temp-table defined like another TT */
FIND FIRST tttt NO-LOCK WHERE tttt.hEditor = phEditor AND tttt.cttname = pcTableLike NO-ERROR.
IF AVAILABLE tttt THEN DO:
    FOR EACH ttfldFrom WHERE ttfldFrom.ittid = tttt.ittid:
        CREATE ttfld.
        BUFFER-COPY ttfldFrom TO ttfld ASSIGN
         iopifldSeq    = iopifldSeq + 1
         ttfld.ittid   = pittid
         ttfld.ifldSeq = iopifldSeq.
    END.
    RETURN.
END.

CREATE WIDGET-POOL.


CREATE BUFFER hBuffer FOR TABLE pcTableLike NO-ERROR.
IF ERROR-STATUS:ERROR THEN {&RETURN}

iNumFields = hBuffer:NUM-FIELDS.
DO iField = 1 TO iNumFields:
    hBufferField = hBuffer:BUFFER-FIELD(iField).
    CREATE ttfld.
    ASSIGN
     iopifldSeq      = iopifldSeq + 1
     ttfld.ittid     = pittid
     ttfld.ifldSeq   = iopifldSeq
     ttfld.cfldname  = hBufferField:NAME
     ttfld.cDataType = hBufferField:DATA-TYPE.
END.
{&RETURN}

&UNDEFINE RETURN

END PROCEDURE.

PROCEDURE addLikeIndices :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor         AS HANDLE     NO-UNDO.
DEFINE INPUT  PARAMETER pcTableLike      AS CHARACTER  NO-UNDO.
DEFINE INPUT  PARAMETER pittid           AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopidxSeq  AS INTEGER    NO-UNDO.
/* DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttIdx.  /* 22-MAY-2007 sla: why input-output table ? */ */

DEFINE VARIABLE cIdxInfo      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hBuffer       AS HANDLE      NO-UNDO.
DEFINE VARIABLE hBufferIdx    AS HANDLE      NO-UNDO.
DEFINE VARIABLE iFieldInfo    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iFieldInfoMax AS INTEGER     NO-UNDO.
DEFINE VARIABLE iNumIdx       AS INTEGER     NO-UNDO.

DEFINE BUFFER ttIdxFrom FOR ttIdx.
DEFINE BUFFER ttIdx     FOR ttIdx.
DEFINE BUFFER tttt      FOR tttt.
                                                  
&SCOPED-DEFINE RETURN DO: DELETE WIDGET-POOL.  RETURN.  ERROR-STATUS:ERROR = NO. END.

/* give priority to temp-table defined like another TT */
FIND FIRST tttt NO-LOCK WHERE tttt.hEditor = phEditor AND tttt.cttname = pcTableLike NO-ERROR.
IF AVAILABLE tttt THEN DO:
    FOR EACH ttIdxFrom WHERE ttIdxFrom.ittid = tttt.ittid:
        CREATE ttidx.
        BUFFER-COPY ttIdxFrom TO ttidx ASSIGN
         iopidxSeq    = iopidxSeq + 1
         ttidx.ittid  = pittid
         ttidx.idxSeq = iopidxSeq.
    END.
    RETURN.
END.

CREATE WIDGET-POOL.

CREATE BUFFER hBuffer FOR TABLE pcTableLike NO-ERROR.
IF ERROR-STATUS:ERROR THEN {&RETURN}

DO WHILE TRUE:
    ASSIGN
     iNumIdx  = iNumIdx + 1
     cIdxInfo = hBuffer:INDEX-INFORMATION(iNumIdx).
    
    IF cIdxInfo = ? THEN LEAVE.
    
    CREATE ttidx.
    ASSIGN
     iopidxSeq      = iopidxSeq + 1
     ttidx.ittid    = pittid
     ttidx.idxSeq   = iopidxSeq
     ttidx.cidxname = ENTRY(1, cIdxInfo)
     iFieldInfoMax  = NUM-ENTRIES(cIdxInfo).
    
    IF ENTRY(2, cIdxInfo) = "1" THEN ttIdx.coptn = ttIdx.coptn + ",UNIQUE".
    IF ENTRY(3, cIdxInfo) = "1" THEN ttIdx.coptn = ttIdx.coptn + ",PRIMARY".
    IF ENTRY(4, cIdxInfo) = "1" THEN ttIdx.coptn = ttIdx.coptn + ",WORD-INDEX".
    ttIdx.coptn = SUBSTRING(ttIdx.coptn, 2).
    
    
    IF iFieldInfoMax > 5 THEN /* 09-MAY-2007 sla: adding this condition to avoid error with TT that use a default index */
     DO iFieldInfo = 5 TO iFieldInfoMax BY 2:
        ttIdx.cFieldsInfo = ttIdx.cFieldsInfo + ","
         + ENTRY(iFieldInfo, cIdxInfo)
         + IF ENTRY(iFieldInfo + 1, cIdxInfo) = "0" THEN ",asc" ELSE ",desc".
    END.
    ttIdx.cFieldsInfo = SUBSTRING(ttIdx.cFieldsInfo, 2).
END.
{&RETURN}

&UNDEFINE RETURN

END PROCEDURE.



PROCEDURE addSourceFilesFromCatDb :    
    DEFINE INPUT        PARAMETER pcDir           AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcMatches       AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcSort          AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiFile        AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER piMaxFile       AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER piMaxLoadTime   AS INTEGER     NO-UNDO.

    RUN addSourceFilesFromCatDb IN ghAbhackDbLib (pcDir
                                                 ,pcMatches
                                                 ,pcSort
                                                 ,INPUT-OUTPUT iopiFile
                                                 ,piMaxFile
                                                 ,piMaxLoadTime).
END PROCEDURE.


PROCEDURE cleanUpDeletedFiles :    
    DEFINE INPUT PARAMETER pcRootPath  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttosFile.
    DEFINE INPUT PARAMETER phMonEditor AS HANDLE      NO-UNDO.

    IF gcGlobalResCat = "XML" THEN RETURN ERROR "21-SEP-2007 for now, the clean up is done only in abhack DB, not in XML dump directory structure".
    IF gcGlobalResCat <> "DB" THEN RETURN ERROR "Unexpected run of cleanUpDeletedFiles with gcGlobalResCat = " + QUOTER(gcGlobalResCat).
    
    RUN cleanUpDeletedFiles IN ghAbhackDbLib (pcRootPath
                                             ,TABLE ttosFile
                                             ,phMonEditor).
END PROCEDURE.


PROCEDURE clearGlobalResources :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE BUFFER ttEdt           FOR ttEdt.  /* 03-SEP-2007 sla: very important stuff to clean here... */
DEFINE BUFFER tttt            FOR tttt.
DEFINE BUFFER ttfld           FOR ttfld.
DEFINE BUFFER ttIdx           FOR ttIdx.
DEFINE BUFFER ttgVar          FOR ttgVar.
DEFINE BUFFER ttgbuffer       FOR ttgbuffer.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.
DEFINE BUFFER ttSection       FOR ttSection.
DEFINE BUFFER ttUsing         FOR ttUsing.
DEFINE BUFFER ttgLibHandle    FOR ttgLibHandle.
DEFINE BUFFER ttPreproc       FOR ttPreproc.  

FIND FIRST ttEdt WHERE ttEdt.hEditor = phEditor.


FOR EACH ttPreproc WHERE ttPreproc.hEditor = phEditor:
    DELETE ttPreproc. 
END.

ASSIGN
 ttEdt.cInherits   = ""
 /* ttEdt.iNumLines   = 0  21-SEP-2007 sla: don't touche that, so a next loadLocal does not mess up ttmark records */
 ttEdt.cParameters = "".

FOR EACH tttt WHERE tttt.hEditor = phEditor:
    FOR EACH ttfld WHERE ttfld.ittid = tttt.ittid:
        DELETE ttfld.
    END.
    FOR EACH ttIdx WHERE ttIdx.ittid = tttt.ittid:
        DELETE ttIdx.
    END.
    DELETE tttt.
END.

FOR EACH ttgVar    WHERE ttgVar.hEditor    = phEditor:
    DELETE ttgVar.
END.

FOR EACH ttgbuffer WHERE ttgbuffer.hEditor = phEditor:
    DELETE ttgbuffer.
END.

FOR EACH ttProc    WHERE ttProc.hEditor    = phEditor:
    DELETE ttProc.
END.

FOR EACH ttFunc    WHERE ttFunc.hEditor    = phEditor:
    DELETE ttFunc.
END.

FOR EACH ttMethod  WHERE ttMethod.hEditor  = phEditor:
    DELETE ttMethod.
END.

FOR EACH ttMark    WHERE ttMark.hEditor    = phEditor:
    DELETE ttMark.
END.

FOR EACH ttReferedBuffer WHERE ttReferedBuffer.hEditor = phEditor:
    DELETE ttReferedBuffer.
END.

FOR EACH ttUsing   WHERE ttUsing.hEditor   = phEditor:
    DELETE ttUsing.
END.

FOR EACH ttgLibHandle WHERE ttgLibHandle.hEditor = phEditor:
    DELETE ttgLibHandle. 
END.

END PROCEDURE.


PROCEDURE createDirStructure :
/*------------------------------------------------------------------------------
  Purpose:     Create a top down directory structure with all directories on the way
  
  Parameters:  pcDirToCreate  => Use '/' (backslashes... *NIX rules)
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER pcDirToCreate AS CHARACTER   NO-UNDO.

DEFINE VARIABLE cCreateThis  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iDirLevel    AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxDirLevel AS INTEGER     NO-UNDO.

                
iMaxDirLevel = NUM-ENTRIES(pcDirToCreate, "/").

cCreateThis = ENTRY(1, pcDirToCreate, "/").

DO iDirLevel = 2 TO iMaxDirLevel.
    cCreateThis = cCreateThis + "/" + ENTRY(iDirLevel, pcDirToCreate, "/").
    FILE-INFO:FILE-NAME = cCreateThis.
    IF FILE-INFO:FULL-PATHNAME <> ? THEN NEXT.
    OS-CREATE-DIR VALUE(cCreateThis).
    IF OS-ERROR <> 0 THEN DO:
        MESSAGE "Got error while trying to create the Directory" QUOTER(cCreateThis) SKIP
         "OS-ERROR = " OS-ERROR SKIP
         "About to return an error to abort the process"
            VIEW-AS ALERT-BOX INFO BUTTONS OK.
        RETURN ERROR "Failed to create directory" + STRING(OS-ERROR).
    END.
END.


END PROCEDURE.


PROCEDURE dumpGlobalCat :
    DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

    ERROR-STATUS:ERROR = NO.
        
    CASE gcGlobalResCat:
        WHEN "disabled" THEN RETURN. /* we should not have called it, but never mind */
        WHEN "XML" THEN RUN dumpGlobalXml (phEditor).
        WHEN "DB" THEN RUN dumpGlobalDb (phEditor) NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable gcGlobalResCat" SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN gphMon:SCREEN-VALUE = "xml dumped error " + QUOTER(RETURN-VALUE).
END PROCEDURE.


PROCEDURE dumpGlobalDb:
    DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
    
    DEFINE VARIABLE dStartEtime AS DECIMAL     NO-UNDO.
    
    dStartEtime = DECIMAL(ETIME).
    
    RUN dumpGlobalDb IN ghAbhackDbLib (phEditor) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
    
    gphMon:SCREEN-VALUE = "DB dump in " + STRING(ETIME - dStartEtime)
     + "ms ~n" + gphMon:SCREEN-VALUE.
END PROCEDURE.


PROCEDURE dumpGlobalXml :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cDumpFile    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDumpFileDir AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMaxDirLevel AS INTEGER     NO-UNDO.
DEFINE VARIABLE dStartEtime  AS DECIMAL     NO-UNDO.

/* limit their scope */
DEFINE BUFFER ttEdt           FOR ttEdt          .

IF PROVERSION BEGINS "9." OR PROVERSION BEGINS "10.0" THEN DO:
    gphMon:SCREEN-VALUE = "For now (10-AUG-2007) the ABHack XML Dump & Load Files are limited to OE 10.0+ (simpler with dataset methods)"
     + "~nI will perhaps try to support it with Version 9 Constructs later".
    RETURN.
END.

FIND ttEdt WHERE ttEdt.hEditor = phEditor.

cDumpFileDir = gcDumpedResourceFileRoot + "/" + REPLACE(ttEdt.cFullPathName, ":", "Drive").
iMaxDirLevel = NUM-ENTRIES(cDumpFileDir, "/").
ENTRY(iMaxDirLevel, cDumpFileDir, "/") = "".
cDumpFileDir = RIGHT-TRIM(cDumpFileDir, "/").

FILE-INFO:FILE-NAME = cDumpFileDir.
IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
    RUN createDirStructure (cDumpFileDir) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
END.

cDumpFile = cDumpFileDir + "/" + ttEdt.cFileName + ".abhack".
dStartEtime = DECIMAL(ETIME).

&IF NOT PROVERSION BEGINS "9." AND PROVERSION >= "10.0" &THEN
    RUN protools/abhack/abhackDumpXml.p (cDumpFile
                                        ,phEditor).

&ENDIF

gphMon:SCREEN-VALUE = "xml dumped in " + STRING(ETIME - dStartEtime)
 + "ms ~n" + gphMon:SCREEN-VALUE.
END PROCEDURE.

PROCEDURE GetExtProcParam :
    DEFINE INPUT  PARAMETER pcExtProcName AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcParams     AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE cLoadFile AS CHARACTER   NO-UNDO.

    CASE gcGlobalResCat:
        WHEN "disabled" THEN RETURN. /* we should not have called it, but never mind */
        WHEN "XML" THEN DO:
            IF PROVERSION BEGINS "9." THEN DO:
                IF VALID-HANDLE(gphMon) THEN gphMon:SCREEN-VALUE = "For now (10-AUG-2007) the ABHack XML Dump & Load Files are limited to OE 10"
                 + "~nWith Version 9, please use the abhack.db, which is more powerful anyway".
                RETURN.
            END.
            cLoadFile = REPLACE(SEARCH(pcExtProcName), "\","/").
            IF cLoadFile = ? THEN RETURN ERROR "Can't locate source file  " + QUOTER(pcExtProcName) + " in propath".
            cLoadFile = gcDumpedResourceFileRoot + "/" + REPLACE(cLoadFile, ":", "Drive") + ".abhack".
            FILE-INFO:FILE-NAME = cLoadFile.
            IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN ERROR "No xml file  " + QUOTER(cLoadFile).
            
            RUN protools/abhack/abhackLoadLibGlobalParam.p (cLoadFile
                                                           ,gcDumpedResourceFileRoot
                                                           ,""
                                                           ,OUTPUT opcParams).

        END.
        WHEN "DB" THEN RUN GetExtProcParam IN ghAbhackDbLib (pcExtProcName, OUTPUT opcParams) NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable" gcGlobalResCat SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
END PROCEDURE.




PROCEDURE loadGlobalFromCat:
    DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
    
    DEFINE VARIABLE cLoadError AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE lLoadError AS LOGICAL     NO-UNDO.

    RUN clearGlobalResources (phEditor).
    
    CASE gcGlobalResCat:
        WHEN "disabled" THEN RETURN. /* we should not have called it, but never mind */
        WHEN "XML" THEN RUN loadGlobalFromXml (phEditor) NO-ERROR.
        WHEN "DB" THEN RUN loadGlobalFromDB (phEditor) NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable" gcGlobalResCat SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN ASSIGN
     lLoadError = YES
     cLoadError = RETURN-VALUE.

    /* 25-OCT-2007 sla: alwyas RUN that, even when we have hit an error erlier */
    RUN addBeforeTable (phEditor). /* this one does NOT raise an error */
    
    IF lLoadError THEN RETURN ERROR gcGlobalResCat + cLoadError.

END PROCEDURE.


PROCEDURE loadGlobalFromDb :
    DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

    DEFINE VARIABLE dStartEtime AS DECIMAL     NO-UNDO.
    
    DEFINE BUFFER ttEdt FOR ttEdt.
    
    dStartEtime = DECIMAL(ETIME).
    
    FIND FIRST ttEdt WHERE ttEdt.hEditor = phEditor.
    
    RUN loadGlobalFromDb IN ghAbhackDbLib (phEditor, "All") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
    
    gphMon:SCREEN-VALUE = "CatLoadDB in " + STRING(ETIME - dStartEtime)
     + "ms for " + ttEdt.cFileName + "~n" + gphMon:SCREEN-VALUE.
END PROCEDURE.


PROCEDURE loadGlobalFromXml :
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

DEFINE VARIABLE cLoadFile    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMaxDirLevel AS INTEGER     NO-UNDO.
DEFINE VARIABLE dStartEtime  AS DECIMAL     NO-UNDO.
DEFINE VARIABLE hbttedt      AS HANDLE      NO-UNDO.

/* limit their scope */
DEFINE BUFFER ttEdt           FOR ttEdt          .

IF PROVERSION BEGINS "9." THEN DO:
    IF VALID-HANDLE(gphMon) THEN gphMon:SCREEN-VALUE = "For now (10-AUG-2007) the ABHack XML Dump & Load Files are limited to OE 10"
     + "~nWith Version 9, please use the abhack.db, which is more powerful anyway".
    RETURN.
END.


FIND ttEdt WHERE ttEdt.hEditor = phEditor.
cLoadFile = gcDumpedResourceFileRoot + "/" + REPLACE(ttEdt.cFullPathName, ":", "Drive") + ".abhack".
FILE-INFO:FILE-NAME = cLoadFile.

IF FILE-INFO:FULL-PATHNAME = ? THEN  DO:
    gphMon:SCREEN-VALUE = "No xml file  " + QUOTER(cLoadFile).
    RETURN ERROR "no file".
END.
                     

dStartEtime = DECIMAL(ETIME).
&IF NOT PROVERSION BEGINS "9." &THEN
    hbttedt =  BUFFER ttEdt:HANDLE.
    RUN protools/abhack/abhackLoadXml.p ("All"
                                        ,cLoadFile
                                        ,hbttedt
                                        ,INPUT gcDumpedResourceFileRoot
                                        ,INPUT-OUTPUT giLastttid)
                                        NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
&ENDIF

gphMon:SCREEN-VALUE  = "CatLoadXML in " + STRING(ETIME - dStartEtime) + "ms ".

END PROCEDURE.



PROCEDURE loadGlobalResources :
/*------------------------------------------------------------------------------
  Purpose:     Load global definitions of vars, tt's and their fields 
30-AUG-2007 sla: moved into abhackParser to leverage the code of abhackwin.w

18-DEC-2006 sla: The main risk we take wehn we try to skip over procedure and
function block is to miss a block to skip.  This would result in considering a
few resources as global although they are not.  So we could end up with unexpected
additional items in the popup list, but it is better than missing items

A little problem with functions that are prototyped in a given global handle
(like a Dynamics manager): FUNCTION adABLCallStack RETURNS CHARACTER
                           ( INPUT piMaxLevel AS INTEGER)
                            IN gshAladinManager.
  => I need to guess I am exiting from a function block. I will rely on the following:
    1) no empty line since the definition of the FUNCTION
    2) a trimmed line begins with 'IN'
  Note that prototyped Procedures does not need that because this kind of definition
  ends with the "END PROCEDURE" syntax anyway.
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor     AS HANDLE    NO-UNDO.
DEFINE INPUT  PARAMETER plBulkLoad   AS LOGICAL   NO-UNDO.
DEFINE OUTPUT PARAMETER oplCompileOK AS LOGICAL   NO-UNDO.

DEFINE VARIABLE cFileName                 AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFullPathName             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListingFile              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cListingTruncatedFileName AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cPreprocessedFile         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cCompilationErrors        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSaveFileName             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hbttedt                   AS HANDLE      NO-UNDO.
DEFINE VARIABLE iCompileEtime             AS DECIMAL     NO-UNDO.
DEFINE VARIABLE iLineCount                AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLineOK                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLoadEtime                AS DECIMAL     NO-UNDO.
DEFINE VARIABLE lModifiedBeforeSave       AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lSaveOK                   AS LOGICAL     NO-UNDO. /* the last part of a compiled listing refers truncated file name with the last 17 characters. Don't ask me why 17 */.

DEFINE BUFFER ttEdt            FOR ttEdt.
DEFINE BUFFER tttt             FOR tttt.
DEFINE BUFFER ttfld            FOR ttfld.
DEFINE BUFFER ttIdx            FOR ttIdx.
DEFINE BUFFER ttMark           FOR ttMark.
DEFINE BUFFER ttProc           FOR ttProc.
DEFINE BUFFER ttFunc           FOR ttFunc.
DEFINE BUFFER ttMethod         FOR ttMethod.
DEFINE BUFFER ttReferedBuffer  FOR ttReferedBuffer.
DEFINE BUFFER ttUsing          FOR ttUsing.
DEFINE BUFFER ttgVar           FOR ttgVar.
DEFINE BUFFER ttgbuffer        FOR ttgbuffer.

FIND ttEdt WHERE ttEdt.hEditor = phEditor. /* cannot fail at this point */

/*================== Reset resource for this editor =====================*/
IF plBulkLoad = NO THEN DO:
    RUN manageMonitorEdtColor IN gphCaller (BUFFER ttEdt).
    /* 17-AUG-2007 sla: this UI event flush seems to cause problems now that I have introduced a
    new feature: show editors being modified in bold in the bttEdtBrowse
    PROCESS EVENTS. /* flush the GUI  to see the color change right now */
    */
END.

RUN clearGlobalResources (phEditor).

IF ttEdt.cFileName MATCHES "*~~~.cls" THEN ttEdt.isClass = YES.

/* 13-AUG-2007 sla: keep ttEdt.cFullPathName if called from the XML producer utility */
IF plBulkLoad THEN cFullPathName = ttEdt.cFullPathName.
ELSE DO:
    RUN getEditorFileName IN gphCaller (phEditor, OUTPUT cFullPathName) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN DO:
        IF ttEdt.lManageableSections = NO THEN cFullPathName = "Untitled.p".
        ELSE DO:
            IF VALID-HANDLE(gphMon) THEN gphMon:SCREEN-VALUE = gphMon:SCREEN-VALUE + "~n cannot find this file on disk, aborting!".
            RETURN "invalid file".
        END.
    END.
END.

ASSIGN
 cFileName                 = ENTRY(NUM-ENTRIES(cFullPathName, "/"), cFullPathName, "/")
 cPreprocessedFile         = ENTRY(1, cFileName , ".")
 cPreprocessedFile         = gcImportDirName + cPreprocessedFile + ".pre.p" /* .p extentions make the slick edit use colors */
 cSaveFileName             = gcImportDirName + cFileName
 ttEdt.dLastLoadGlobalTime = TIME + (TODAY - 06/01/2007) * 3600 * 24. /* kind of dateTime for Progress 9 */


IF ttEdt.lManageableSections = NO
 AND (   phEditor:MODIFIED 
      OR cFullPathName = "Untitled.p")
 THEN DO:  /* 23-MAY-2007 sla: new if in a large editor (not in section editor), THEN the developper does not need to save the file first */
    /* The advantages of using a file we just saved from the editor are;
     1) no need to save before firing GLOBAL load
     2) gcImportDirName is a local directory, so it is faster to parse */
    lModifiedBeforeSave = phEditor:MODIFIED.
    phEditor:SAVE-FILE(cSaveFileName).
    IF cSaveFileName MATCHES "*~~~.cls" THEN DO: /* it seems a SAVE-FILE makes the editor loos its color for a class file ! */
        /* solution is to re-save with a .p extension ;)  As the import dir is local, the extra cost is negligeable */
        phEditor:SAVE-FILE(cSaveFileName + ".p").
        OS-DELETE VALUE(cSaveFileName + ".p").
    END.
    phEditor:MODIFIED = lModifiedBeforeSave.  /* it seems the save-file switches the modified attribute to NO... */
    cFullPathName = cSaveFileName.
END.

gphMon:SCREEN-VALUE = "  " +  STRING(TIME, "hh:mm:ss") +  " GlobalLoadFired ".

iCompileEtime = DECIMAL(ETIME).
COMPILE VALUE(cFullPathName) PREPROCESS VALUE(cPreprocessedFile) NO-ERROR.
iCompileEtime = DECIMAL(ETIME) - iCompileEtime.
/* 04-OCT-2007 sla: improvement to build the ERROR message */
oplCompileOK = COMPILER:ERROR-ROW = ? OR COMPILER:ERROR-ROW = 0 OR ERROR-STATUS:GET-NUMBER(1) = 6430.  /* 6430 =  SAVE not specified, and r-code file <name> exists.  List/xref/preprocess/debug-list listing will proceed, but no r-code will be generated. (6430) */
IF oplCompileOK = NO THEN DO:
    cCompilationErrors = ProgressErrors().
    IF cCompilationErrors = "" THEN cCompilationErrors = "COMPILER:ERROR-ROW = " + QUOTER(COMPILER:ERROR-ROW) + "  ERROR-STATUS:GET-NUMBER(1): " + STRING(ERROR-STATUS:GET-NUMBER(1)).
END.
gphMon:SCREEN-VALUE = gphMon:SCREEN-VALUE + "~n" + STRING(iCompileEtime) + " ms to comp-preprocess"
 + (IF oplCompileOK THEN "" ELSE " => failed with this errors " + cCompilationErrors + " (about to parse partialy generated file anyway)").

/* 19-JAN-2007 sla: implementing a global load in two phases:
  1) preprocessed file to catch resources defined ininclude files
    => but the file is partially generated in case of syntax errors, so the load can be incomplete
    => plus a procedure defined without proper END PROCEDURE at the end, or a UDF without END FUNCTION 
       can mess up abhack to find out it is back in the main block.
  2) load the source file itself to load as much as possible in case of compilation error or
   if ABhack could not find out when the code was going back to a main block */

iLoadEtime = DECIMAL(ETIME).
INPUT STREAM edtImport FROM VALUE(cPreprocessedFile).
RUN loadGlobalResourcesInFile (phEditor, INPUT-OUTPUT iLineOK, INPUT-OUTPUT iLineCount, NO /*record marks*/ ).
INPUT STREAM edtImport CLOSE.

/* 22-APR-2007 sla: now create temp-table record for the before-table we caught so we can handle
 their completion like any other table */
/* 04-SEP-2007 sla: use new IP for that, so I can reuse it in loadGlobalFromCat */
RUN addBeforeTable (phEditor).


OS-DELETE VALUE(REPLACE(cPreprocessedFile, "/", "\")).

iLoadEtime = DECIMAL(ETIME) - iLoadEtime.
gphMon:SCREEN-VALUE = gphMon:SCREEN-VALUE + "~n" + STRING(iLoadEtime) + " ms to PreprocParse "
 + STRING(iLineCount) + " lines.  Found " + STRING(iLineOK) + " with glob resources".

gphMon:CURSOR-LINE  = gphMon:NUM-LINES.

ASSIGN
 iLoadEtime = DECIMAL(ETIME)
 iLineCount = 0
 iLineOK    = 0.

/* 29-MAY-2007 sla: try to parse te end of a listing file to catch the line numbers of
 trigger, procedure function or method block.  The next part will update the file-offset info */
cListingFile = gcImportDirName + "listingFile.p".
COMPILE VALUE(cFullPathName) LISTING VALUE(cListingFile) PAGE-SIZE 127 PAGE-WIDTH 255 NO-ERROR.
IF oplCompileOK
 AND SEARCH(cListingFile) <> ? THEN DO:
    iLoadEtime = DECIMAL(ETIME) - iLoadEtime.
    gphMon:SCREEN-VALUE = gphMon:SCREEN-VALUE + "~n" + STRING(iLoadEtime) + " ms to Compile-Listing".
    iLoadEtime = DECIMAL(ETIME).
    
    /* 15-AUG-2007 sla: fix ERROR 82 when cFullPathName does not need to be truncated to analyze COMPILE LISTING */
    IF LENGTH(cFullPathName) > 17 THEN
     cListingTruncatedFileName = SUBSTRING(cFullPathName, LENGTH(cFullPathName) - 16). /* "- 17 + 1" = " -16" */
    ELSE cListingTruncatedFileName = cFullPathName.
    
    INPUT STREAM edtImport FROM VALUE(cListingFile).
    FILE-INFO:FILE-NAME = SEARCH(cListingFile).
    RUN loadGlobParseListing (INPUT phEditor
                                  ,INPUT FILE-INFO:FILE-SIZE  /* we will seek-jump to 80% of it to reach the block listing part at the end of the listing file */
                                  ,INPUT cListingTruncatedFileName
                                  ,OUTPUT iLineOK).
    INPUT STREAM edtImport CLOSE.
    
    OS-DELETE VALUE(cListingFile).
    iLoadEtime = DECIMAL(ETIME) - iLoadEtime.
    gphMon:SCREEN-VALUE = gphMon:SCREEN-VALUE + "~n" + STRING(iLoadEtime) + " ms to catch "
                        + STRING(iLineOK) + " preprocessors or block marks".
    gphMon:CURSOR-LINE  = gphMon:NUM-LINES.
END.

ASSIGN
 iLoadEtime = DECIMAL(ETIME)
 iLineCount = 0
 iLineOK    = 0.
 
INPUT STREAM edtImport FROM VALUE(cFullPathName).
RUN loadGlobalResourcesInFile IN TARGET-PROCEDURE
 (phEditor
 ,INPUT-OUTPUT iLineOK
 ,INPUT-OUTPUT iLineCount
 ,YES). /*record the marks*/
INPUT STREAM edtImport CLOSE.
ttEdt.iNumLines = iLineCount.  /* 21-SEP-2007 sla: to solve a problem that seem to occur with messed up ttmark records when a first loadLocal fires after a LoadGlobal */

IF ttEdt.lManageableSections = NO
 AND cFullPathName BEGINS gcImportDirName /* this extra condition is redondant, but I keep here for security */
 AND cFullPathName <> ttEdt.cFullPathName /* 31-AUG-2007 sla: extra protection to not delete source file... */
 THEN OS-DELETE VALUE(cFullPathName).

iLoadEtime = DECIMAL(ETIME) - iLoadEtime.
gphMon:SCREEN-VALUE = gphMon:SCREEN-VALUE + "~n" + STRING(iLoadEtime) + " ms to SourceParse "
 + STRING(iLineCount) + " lines and "
 + (IF oplCompileOK THEN " update line-mark offsets." ELSE " attempt to catch line-marks.")
 + "  Found " + STRING(iLineOK) + " remaining lines with new glob resources".



IF  gcGlobalResCat <> "disabled"
 OR plBulkLoad THEN DO: /* 31-AUG-2007 sla: sure, the point of a bulk dump is to dump */
    RUN dumpGlobalCat (phEditor).
    
    /* 14-AUG-2007 sla: now that I have dumped  add SUPER guys to my temp-tables */
    IF plBulkLoad = NO
     AND ttEdt.cInherits > "" THEN DO:
        DEFINE VARIABLE cUsingList  AS CHARACTER   NO-UNDO.
        DEFINE VARIABLE iSuperLevel AS INTEGER     NO-UNDO.
        
        hbttedt =  BUFFER ttEdt:HANDLE.
        FOR EACH ttUsing WHERE ttUsing.hEditor = ttEdt.hEditor:
            cUsingList = cUsingList + ";" + ttUsing.cUsing.
        END.
        cUsingList = SUBSTRING(cUsingList, 2).
        IF cUsingList = ? THEN cUsingList = "".
        
        CASE gcGlobalResCat:
            WHEN "XML" THEN
             RUN protools/abhack/abhackLoadXml.p ("onlySuperResources,UsingList," + cUsingList
                                                 ,""  /* cLoadFile   no necessary to just load SUPER guys */
                                                 ,hbttedt
                                                 ,INPUT gcDumpedResourceFileRoot
                                                 ,INPUT-OUTPUT giLastttid)
                                                 NO-ERROR.
            WHEN "DB" THEN
             RUN loadSuperResources IN ghAbhackDbLib (BUFFER ttEdt
                                                     ,ttEdt.cInherits
                                                     ,cUsingList
                                                     ,INPUT-OUTPUT ISuperLevel)
                                                     NO-ERROR.
        END CASE.
        IF ERROR-STATUS:ERROR THEN gphMon:INSERT-STRING("~n** Problem to load super resources..." + errReturnValue() ).
    END.
END. /* IF glUseGlobalResourcesXmlDumps */
 
END PROCEDURE.

DEFINE VARIABLE gcGlobalTestVar AS CHARACTER  NO-UNDO LABEL "The point of this dummy var gcGlobalTestVar is to test the loadGloabalVars process ;)".



PROCEDURE loadGlobalResourcesInFile :
/* 22-APR-2007 sla: started to restructure this large procedure by splitting it in such
a way it will not affect performance but allow each part to grow.
All subProcedure of this guy will be prefixed with "loadParse"

07-MAY-2007 sla: finished to restructure this main procedure with many sub-procedures*/
DEFINE INPUT        PARAM phEditor      AS HANDLE  NO-UNDO.
DEFINE INPUT-OUTPUT PARAM piLineOK      AS INTEGER NO-UNDO.
DEFINE INPUT-OUTPUT PARAM piLineCount   AS INTEGER NO-UNDO.
DEFINE INPUT        PARAM plOrigSrcFile AS LOGICAL NO-UNDO. /* record marks only when scanning the original source file (not its preprocessed version) */

DEFINE VARIABLE cAccesMode                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBlockName                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDefEntry1                AS CHARACTER   NO-UNDO. /* to set once and for all for performance reasons */
DEFINE VARIABLE cDefEntry2                AS CHARACTER   NO-UNDO. /* to set once and for all for performance reasons */
DEFINE VARIABLE cFor                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cFuncProcName             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cName                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOldString                AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cReturnType               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSectionType              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cVar                      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cViewAsWord               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWord                     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ifldseq                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iidxseq                   AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMainBlockMark            AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMarkUniqueCount          AS INTEGER     NO-UNDO.
DEFINE VARIABLE iMaxWord                  AS INTEGER     NO-UNDO.
DEFINE VARIABLE iSubstractToOffset        AS INTEGER     NO-UNDO.
DEFINE VARIABLE isStatic                  AS LOGICAL     NO-UNDO.
DEFINE VARIABLE ittid                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE iViewAsWord               AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord                     AS INTEGER     NO-UNDO.
DEFINE VARIABLE lCatchAFirstFuncMethParam AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lDefBlockMarkDone         AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lInFuncOrMethDef          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lMainBlockMarkDone        AS LOGICAL     NO-UNDO.
DEFINE VARIABLE rCatchViewAs              AS ROWID       NO-UNDO.

DEFINE BUFFER tttt            FOR tttt.
DEFINE BUFFER ttgVar          FOR ttgVar.
DEFINE BUFFER ttgbuffer       FOR ttgbuffer.
DEFINE BUFFER ttProc          FOR ttProc.
DEFINE BUFFER ttFunc          FOR ttFunc.
DEFINE BUFFER ttEdt           FOR ttEdt.
DEFINE BUFFER ttMark          FOR ttMark.
DEFINE BUFFER otherMark       FOR ttMark. /* beware about unique index violation that could occur (very very rarely) when multiple trigger block could have a same name (dlc/src files that use many include files) */
DEFINE BUFFER ttMethod        FOR ttMethod.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.
DEFINE BUFFER ttUsing         FOR ttUsing.
DEFINE BUFFER ttgLibHandle    FOR ttgLibHandle.
DEFINE BUFFER ttPreproc       FOR ttPreproc.

/* pseudo FUNCTION to speed up things */
&SCOPED-DEFINE innerTrim DO WHILE cOldString <> cLine :   cOldString = cLine.   cLine = REPLACE(cOldString, "  ", " ").  END.

FIND ttEdt WHERE ttEdt.hEditor = phEditor. /* cannot raise any error */
cSectionType = "Main".

lineIteration:
REPEAT ON ERROR UNDO, LEAVE:

/* 03-JAN-2007 sla: trying to catch view-as phrase.  By doing that before the import, I might be able to
catch a view-as that was located in the same line as a variable definition  */
IF   rCatchViewAs <> ? 
 AND cLine MATCHES "*VIEW-AS *" THEN DO:
    ASSIGN
     cViewAsWord = "" /* reset because the next assignment will have no-error option */
     iViewAsWord = LOOKUP("VIEW-AS", cLine, " ").
     
    cViewAsWord = ENTRY(iViewAsWord + 1, cLine, " ") NO-ERROR.
    IF cViewAsWord <> "ALERT-BOX" THEN DO:
        FIND FIRST ttgvar WHERE ROWID(ttgvar) = rCatchViewAs.
        ttgVar.cViewAs = cViewAsWord.
        rCatchViewAs = ?.
    END.
END.

/* 28-MAY-2007 sla: little improvment to catch a function/METHOD PARAMETER located in a first line */
IF lCatchAFirstFuncMethParam = NO THEN DO:
    IMPORT STREAM edtImport UNFORMATTED cLine.
    piLineCount = piLineCount + 1.
    
    FOR FIRST ttMark WHERE  /* faster than a FIND NO-ERROR  when we know we will not find the record most of the times, will perhaps change in 10.1B03 */
         ttMark.hEditor = phEditor
     AND ttMark.iLine   = piLineCount:
    END.
    IF AVAILABLE ttMark THEN DO:
        ttMark.iOffset = SEEK(edtImport) - LENGTH(cLine) - 2.
        /* if trigger mark, then try to catch trigger info */
        IF ttMark.cBlockType = "TRIGGER" THEN DO:
            cBlockName = TRIM(cLine).
            
            IF cBlockName BEGINS "ON " THEN DO:
                cBlockName = SUBSTRING(cBlockName, 4).
                /* 01-NOV-2008 sla: improve label for runtime triggers */
                IF cBlockName MATCHES "* RUN *" THEN cBlockName = SUBSTRING(cBlockName, 1, R-INDEX(cBlockName, " RUN ")).
            END.
            
            IF INDEX(cBlockName, "/~*") > 0 THEN cBlockName = TRIM(removeLineComments(cLine, INDEX(cBlockName, "/~*"))).
            
            /* 13-AUG-2007 sla: problem with multiple widget of same that exist in multiple frames ... */
            /* IF INDEX(cBlockName, " IN FRAME ") > 0 THEN cBlockName = SUBSTRING(cBlockName, 1, INDEX(cBlockName, " IN FRAME ") - 1). */
            IF INDEX(cBlockName, " IN FRAME ") > 0 THEN cBlockName = REPLACE(cBlockName, " IN FRAME ", " in ").
            /* end 13-AUG-2007 sla: problem with multiple widget of same name that exist in multiple frames ... */
            
            FIND FIRST otherMark WHERE otherMark.hEditor = ttMark.hEditor
             AND otherMark.cBlockType = ttMark.cBlockType
             AND otherMark.cBlockName = cBlockName
             AND ROWID(otherMark) <> ROWID(ttMark)
             NO-ERROR.
            IF AVAILABLE otherMark THEN ASSIGN
             iMarkUniqueCount = iMarkUniqueCount + 1
             cBlockName = cBlockName + "#" + STRING(iMarkUniqueCount).
            
            /* 03-AUG-2007 sla: beware the join with ttReferedBuffer */
            FOR EACH ttReferedBuffer OF ttMark:
                ttReferedBuffer.cBlockName = cBlockName.
            END.
            ttMark.cBlockName = SUBSTRING(cBlockName, 1, 175). /* 05-FEB-2008 sla: avoid possible error 129 because of a very large trigger event name in the index key */
        END.
    END.
    
    /* catch main block */
    IF plOrigSrcFile AND lMainBlockMarkDone = NO AND lDefBlockMarkDone
     AND (   cLine BEGINS "&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK"
          OR cLine BEGINS "MAIN-BLOCK:")
      THEN DO:
        RUN loadGlobRecordMark (phEditor, "Main", "MainBlock", piLineCount, SEEK(edtImport) - LENGTH(cLine)).
        lMainBlockMarkDone = YES.     
    END.    
END.
ELSE lCatchAFirstFuncMethParam = NO. /* do this trick only once, next time we will import a new line */ 
 
/* 21-MAY-2007 sla: slightly speed up things by ignoring EMPTY lines asap, and using the TRIM function only when needed  */
IF cLine = "" THEN DO:
    IF lInFuncOrMethDef THEN lInFuncOrMethDef = NO. /* 21-MAY-2007 sla: did the test, it costs a it less to first test if the flag is set rather than forcing it to NO directly */
    IF rCatchViewAs <> ? THEN rCatchViewAs = ?.
    NEXT lineIteration.
END.

/* 21-MAY-2007 sla: TRIM it now (we did avoid the TRIM with the above code) */
cLine = TRIM(cLine).

IF   rCatchViewAs <> ?
 AND cLine MATCHES "*~~~."
 AND NOT cLine MATCHES "* VIEW-AS *"
 THEN rCatchViewAs = ?.

IF cLine BEGINS "PROCEDURE "
 AND (   cLine MATCHES "*:*" /* beware, a comment my well begin with the word procedure... */
       /* 24-MAY-2007 sla: added last star for cases with comment at the end of the line */
      /* 17-JAN-2007 sla: The following line also supports procedure prototyped in SUPER */
      OR cLine MATCHES "* ~~~.") /* OCX trigger procedures generatedby the AppBuilder end up with " ." */
 THEN DO:
    cLine = TRIM(RIGHT-TRIM(cLine, ":")).
    {&innerTrim}
    IF NUM-ENTRIES(cLine, " ") = 1 THEN NEXT lineIteration. /* the line was just "PROCEDURE :" */
    /* start keeping track of this procedure here, we will take care of its signature later */
    ASSIGN
     cSectionType  = "procedure"
     cFuncProcName = TRIM(ENTRY(2, cLine, " "), ":"). /* 17-OCT-2007 sla: added TRIM( ,":") so we do not parse a procedure name with a trailing ':' when there is no space in front of it */
    
    IF   ttEdt.lManageableSections = NO
     AND plOrigSrcFile
     AND (NOT AVAILABLE ttMark /* if AVAILABLE ttMark  then the mark already recorded with an updated offset */
           OR ttMark.iLine <> piLineCount) /* perhaps we manage to catch a mark that has not been recorded yet */
     THEN RUN loadGlobRecordMark (phEditor, cFuncProcName, "procedure", piLineCount, SEEK(edtImport)).
    
    /* record only if not already in (can happen with forward declarations) */
    FOR FIRST ttProc WHERE ttProc.hEditor = phEditor AND ttProc.cName = cFuncProcName: END. /* 21-SEP-2007 sla: faster than a FIND NO-ERROR that fails most of the times.  May change in 10.1B03 */
    IF NOT AVAILABLE ttProc THEN CREATE ttProc.
    ASSIGN
     ttProc.hEditor     = phEditor
     ttProc.cFileName   = ttEdt.cFileName
     ttProc.cName       = cFuncProcName
     ttProc.cAccessMode = IF cLine MATCHES "PROCEDURE * PRIVATE*" THEN "PRIVATE" ELSE "".  /* 22-APR-2008 sla: catch the PRIVATE option */

    NEXT lineIteration.
END.
ELSE IF cLine BEGINS "FUNCTION "
 AND (   cLine MATCHES "* RETURNS *" /*beware, a comment my well begin with the word function... */
      OR cLine MATCHES "* RETURN *") /* 24-MAY-2007 sla: Progress allows the usage of RETURN without trailing 'S' */
 THEN DO:
    /* start keeping track of this function here, we will take care of its signature later */
    iSubstractToOffset = LENGTH(cLine) + 2.
    {&innerTrim}
    IF NUM-ENTRIES(cLine, " ") = 2 THEN NEXT lineIteration. /* the line was just "FUNCTION RETURNS" */
    /* start keeping track of this procedure here, we will take care of its signature later */
    ASSIGN
     cSectionType      = "function"
     lInFuncOrMethDef  = YES
     cFuncProcName     = ENTRY(2, cLine, " ").

    /* Modify cline so we can parse a first PARAMETER located in the first function/method definition line */
    IF INDEX(cLine, "(") > 0 THEN ASSIGN
     lCatchAFirstFuncMethParam = YES
     cLine = SUBSTRING(cLine, INDEX(cLine, "(") + 1).
        
    IF   ttEdt.lManageableSections = NO 
     AND plOrigSrcFile
     AND (NOT AVAILABLE ttMark /* if AVAILABLE ttMark  then the mark already recorded with an updated offset */
           OR ttMark.iLine <> piLineCount) /* perhaps we manage to catch a mark that has not been recorded yet */
     THEN RUN loadGlobRecordMark (phEditor, cFuncProcName, "function", piLineCount, SEEK(edtImport) - iSubstractToOffset).
    /* record only if not already in (can happen with forward declarations) */
    FOR FIRST ttFunc WHERE ttFunc.hEditor = phEditor AND ttFunc.cName = cFuncProcName:  END.  /* 21-SEP-2007 sla: faster than a FIND NO-ERROR that fails most of the times.  May change in 10.1B03 */
    IF AVAILABLE ttFunc THEN cFuncProcName = "". /* we already caught it */
    ELSE DO:
        CREATE ttFunc.
        ASSIGN
         ttFunc.hEditor     = phEditor
         ttFunc.cFileName   = ttEdt.cFileName
         ttFunc.cName       = cFuncProcName.
         ttFunc.cAccessMode = IF cLine MATCHES "FUNCTION * PRIVATE*" THEN "PRIVATE" ELSE "".  /* 22-APR-2008 sla: catch the PRIVATE option */
    END.
    NEXT lineIteration.
END.

ELSE IF cLine BEGINS "METHOD "
 OR cLine BEGINS "CONSTRUCTOR "
 OR cLine BEGINS "DESTRUCTOR "
 THEN DO: /* 28-MAY-2007 sla: support of class methods */
    /* start keeping track of this function here, we will take care of its signature later */
    iSubstractToOffset = LENGTH(cLine) + 2.
    {&innerTrim}
    IF NUM-ENTRIES(cLine, " ") < 3 THEN NEXT lineIteration. /* we expect at least the type and method name in the line */
    /* start keeping track of this procedure here, we will take care of its signature later */
    ASSIGN
     cSectionType      = "method"
     lInFuncOrMethDef  = YES
     iword             = 2
     cAccesMode        = "PUBLIC" /* default */
     isStatic          = NO /* default */
     cWord             = ENTRY(iWord, cLine, " ").
    
    IF CAN-DO("PRIVATE,PROTECTED,PUBLIC", cWord)
     THEN ASSIGN
      cAccesMode = cWord
      iWord      = iWord + 1
      cWord      = ENTRY(iWord, cLine, " ").

    /* 13-OCT-2009 sla: now we keep track of Static guys */
    IF cWord = "STATIC" THEN ASSIGN
     isStatic = YES
     iWord    = iWord + 1
     cWord    = ENTRY(iWord, cLine, " ").
    
    
    /* 13-OCT-2008 sla: added STATIC 
       13-OCT-2009 sla: removed it one year later, because we now keep track of it */
    IF CAN-DO("OVERRIDE,FINAL", cWord) /* we don't keep track of that for now' */
     THEN ASSIGN
      iWord = iWord + 1
      cWord = ENTRY(iWord, cLine, " ").


    /* 21-JAN-2009 sla: had to duplicate this code because we can either start with the 
    access mode or keywords like OVERRIDE,STATIC,FINAL */
    IF CAN-DO("PRIVATE,PROTECTED,PUBLIC", cWord)
     THEN ASSIGN
      cAccesMode = cWord
      iWord      = iWord + 1
      cWord      = ENTRY(iWord, cLine, " ").

    /* 13-OCT-2008 sla: added STATIC 
      13-OCT-2009 sla:  same as above: removed it because we now keep track of it */
    IF CAN-DO("OVERRIDE,FINAL", cWord) /* we don't keep track of that for now' */
     THEN ASSIGN
      iWord = iWord + 1
      cWord = ENTRY(iWord, cLine, " ").
    
    /* 13-OCT-2009 sla: now we keep track of Static guys */
    IF cWord = "STATIC" THEN ASSIGN
     isStatic = YES
     iWord    = iWord + 1
     cWord    = ENTRY(iWord, cLine, " ").
    /* end 21-JAN-2009 sla: had to duplicate this code because ...*/ 


    /* 29-JUL-2007 sla: keep CONSTRUCTOR or DESTRUCTOR as name */
    IF cLine BEGINS "CONSTRUCTOR " THEN ASSIGN
     cFuncProcName = "CONSTRUCTOR"
     cReturnType   = "VOID".

    ELSE IF cLine BEGINS "DESTRUCTOR " THEN ASSIGN
     cFuncProcName = "DESTRUCTOR"
     cReturnType   = "VOID".
    
    ELSE ASSIGN
     cReturnType   = cWord
     iWord         = iWord + 1
     cFuncProcName = ENTRY(iWord, cLine, " ").
     
    /* Modify cline so we can parse a first PARAMETER located in the first function/method definition line */
    IF INDEX(cLine, "(") > 0 THEN ASSIGN
     cFuncProcName             = ENTRY(1, cFuncProcName, "(") /* just in case */
     lCatchAFirstFuncMethParam = YES
     cLine                     = SUBSTRING(cLine, INDEX(cLine, "(") + 1).
        
    IF   ttEdt.lManageableSections = NO /* should never be yes for methods */
     AND plOrigSrcFile
     AND (NOT AVAILABLE ttMark /* if AVAILABLE ttMark  then the mark already recorded with an updated offset */
           OR ttMark.iLine <> piLineCount) /* perhaps we manage to catch a mark that has not been recorded yet */
     THEN RUN loadGlobRecordMark (phEditor, cFuncProcName, "method", piLineCount, SEEK(edtImport) - iSubstractToOffset).

    /* record only if not already in (can happen with forward declarations) */
    FOR FIRST ttMethod WHERE ttMethod.hEditor = phEditor AND ttMethod.cName = cFuncProcName: END.  /* 21-SEP-2007 sla: faster than a FIND NO-ERROR that fails most of the times.  May change in 10.1B03 */
    IF AVAILABLE ttMethod THEN cFuncProcName = "". /* we already caught it */
    ELSE DO:
        CREATE ttMethod.
        ASSIGN
         ttMethod.hEditor     = phEditor
         ttMethod.cFileName   = ttEdt.cFileName
         ttMethod.cName       = cFuncProcName
         ttMethod.cAccessMode = cAccesMode
         ttMethod.cReturnType = cReturnType
         ttMethod.isStatic    = isStatic.  /* 13-OCT-2009 sla: just added */
    END.
    NEXT lineIteration.
END.


/* Catch procedure params */
IF   cSectionType = "procedure"
 AND cFuncProcName > ""
 AND cLine MATCHES "DEF* PARAM*"
 THEN DO:
    {&innerTrim}
    IF NUM-ENTRIES(cLine, " ") < 5 THEN NEXT lineIteration.
    
    RUN loadGlobParseProcParam (BUFFER ttEdt
                               ,cLine
                               ,cFuncProcName
                               ,INPUT-OUTPUT piLineOK).
    NEXT lineIteration.
END.   /* End Catch procedure params */


/* Catch function params */
IF   cSectionType = "function"
 AND lInFuncOrMethDef
 AND cFuncProcName > ""
 THEN DO:
    IF NUM-ENTRIES(cLine, " ") < 2 THEN NEXT lineIteration.
    cLine = LEFT-TRIM(LEFT-TRIM(cLine, ",")).
    cLine = LEFT-TRIM(LEFT-TRIM(cLine, "(")).
    {&innerTrim}
    
    RUN loadGlobParseFuncParam (phEditor, cLine, cFuncProcName, INPUT-OUTPUT piLineOK).

    /* end the function param def block now ? */
    IF cLine MATCHES "*)*" THEN ASSIGN
     lInFuncOrMethDef = NO.  /* I have no choice but to rely on this, so we'd better not put any ')' in a comment withing the function param def block */
     /* 05-SEP-2007 sla: bad bug...  there was no reason to set this var back to "main" 
     cFuncProcName    = ""
     cSectionType     = "main". */
    
    
    /* 06-SEP-2007 sla: but it was need to consider we are back to main block after a FORWARD declare */
    /* note I cannot catch the FORWARD option is it is located after the last line that closes
    the signature definition with ")".
    Apparently, the AppBuilder always put this option in the same line, AND we usally do the same manually
    
    If a FUNCTION has no input parameter, then I can catch it as well  */
    IF  cLine MATCHES "* FORWARD *"
     OR cLine MATCHES "* FORWARD~~.*"
     OR cLine MATCHES "* IN *"  /* mapping in SUPER or explicit handle */
     THEN ASSIGN
     cSectionType = "main"
     /* set these two guys as well for the case wher ethe FUNCTION has no input PARAMETER [so no ')' ] */
     lInFuncOrMethDef = NO  /* I have no choice but to rely on this, so we'd better not put any ')' in a comment withing the function param def block */
     cFuncProcName    = "".
    
    NEXT lineIteration.
END.   /* End Catch function params */


/* Catch method params */
IF   cSectionType = "method"
 AND lInFuncOrMethDef
 AND cFuncProcName > ""
 THEN DO:
    cLine = TRIM(cLine).
    
    /* 10-JUN-2007 sla: occurs when METHOD has no param */
    IF cLine BEGINS ")" OR NUM-ENTRIES(cLine, " ") < 2 THEN DO:
        lInFuncOrMethDef = NO.
        NEXT lineIteration.
    END.
    cLine = LEFT-TRIM(LEFT-TRIM(cLine, ",")).
    cLine = LEFT-TRIM(LEFT-TRIM(cLine, "(")).
    {&innerTrim}
    
    RUN loadGlobParseMethParam (phEditor, cLine, cFuncProcName, INPUT-OUTPUT piLineOK).

    /* end the function param def block now ? */
    IF cLine MATCHES "*)*" THEN ASSIGN
     lInFuncOrMethDef = NO.  /* I have no choice but to rely on this, so we'd better not put any ')' in a comment withing the function param def block */
     /* 05-SEP-2007 sla: bad bug...  there was no reason to set this var back to "main" 
     cFuncProcName    = ""
     cSectionType     = "main". */

    NEXT lineIteration.
END.   /* End Catch function params */


/* end procedure or function definition block */
IF  cLine BEGINS "END PROCEDURE"
 OR cLine BEGINS "END FUNCTION"
 OR cLine BEGINS "END METHOD"
 OR cLine BEGINS "END CONSTRUCTOR"
 OR cLine BEGINS "END DESTRUCTOR"
 OR cLine BEGINS "~/~* _UIB-CODE-BLOCK-END ~*~/" /* so we can even skip trigger blocks */
 OR cLine BEGINS "&ANALYZE-RESUME"
 THEN DO:
    ASSIGN
     cSectionType = "main"
     lInFuncOrMethDef = NO. /* security */
    IF plOrigSrcFile THEN DO:
        iMainBlockMark = iMainBlockMark + 1.
        RUN loadGlobRecordMark (phEditor, "main" + STRING(iMainBlockMark) , "main", piLineCount, SEEK(edtImport) - LENGTH(cLine)).
    END.
END.


/* 30-JUL-2007 sla: catch super class name and packages declared with the USING Statement */
/* 26-OCT-2007 sla: Actually, .p's can now use the using statement for the uage of classes */
IF cLine BEGINS "USING " THEN DO:
    {&innerTrim}
    cWord = TRIM(ENTRY(2, cLine, " "), ".").
    IF NOT CAN-FIND(ttUsing WHERE ttUsing.hEditor = ttEdt.hEditor AND ttUsing.cUsing = cWord) THEN DO:
        CREATE ttUsing.
        ASSIGN
         ttUsing.hEditor = ttEdt.hEditor
         ttUsing.cUsing  = cWord.
    END.
    NEXT lineIteration.
END.
IF ttEdt.isClass = YES /* don't loose time for non class sources */ THEN DO:
    IF cLine BEGINS "CLASS " AND cLine MATCHES "* INHERITS *" THEN DO:
        {&innerTrim}
        iWord = LOOKUP("INHERITS", cLine, " ").
        ttEdt.cInherits = TRIM(ENTRY(iWord + 1, cLine, " "), ":").
        NEXT lineIteration.
    END.
END.


/* 22-AUG-2007 sla: now track usage of RUN PERSISTENT SET someGLOBALVars
  => this PROCESS will cost a lot => a MATCHES for *each* line of preprocessed file */
IF cLine MATCHES "*RUN * PERSIST* SET *" THEN DO:
    /* I also need to Inner trim here */
    {&innerTrim}
    ASSIGN
     iWord = LOOKUP("RUN", cLine, " ")
     cName = ENTRY(iWord + 1, cLine, " ")
     iWord = LOOKUP("SET", cLine, " ")
     cVar = RIGHT-TRIM(ENTRY(iWord + 1, cLine, " "), ".")
     NO-ERROR.
    IF ERROR-STATUS:ERROR THEN NEXT lineIteration.
    
    cVar = ENTRY(1, cVar, "(").
    
    IF NOT CAN-FIND(FIRST ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = cVar)
     THEN NEXT lineIteration.
    
    CREATE ttgLibHandle.
    ASSIGN
     ttgLibHandle.hEditor   = phEditor
     ttgLibHandle.cFileName = ttEdt.cFileName
     ttgLibHandle.cLibFile  = cName
     ttgLibHandle.cVar      = cVar
     ttgLibHandle.iLine     = piLineCount.
END.


/* Procedure and function and method parameters are handled above.  At this point, we are interested in main block things only */
IF cSectionType <> "Main" THEN NEXT lineIteration.


/* Ignore non definition lines */
IF   NOT cLine BEGINS "DEF"
 AND NOT cLine BEGINS "FIELD "
 AND NOT cLine BEGINS "INDEX "
 AND NOT cLine BEGINS "BEFORE-TABLE "
 THEN NEXT lineIteration.


IF cLine BEGINS "BEFORE-TABLE " THEN DO: /* 22-APR-2007 sla: let's catch before-table's' */
    FIND FIRST tttt WHERE tttt.ittid = ittid NO-ERROR.
    IF AVAILABLE tttt THEN tttt.cBeforeTable = RIGHT-TRIM(ENTRY(2, cLine, " "), ".") NO-ERROR.
    NEXT lineIteration.
END.  /* BEFORE-TABLE case */

/* 08-FEB-2007 sla: the guys to replace shall begin with space " NEW " to allow a variable or field to be called 'myGlobal' */
IF INDEX(cLine, " SHARED " ) > 0 THEN DO: /* 27-MAY-2007 sla: little performance gain, do it only if 'shared' is present in the line
                                            (+ consistency) ... and if these words are at the appropriate place */
    {&innerTrim}
    IF ENTRY(2, cLine, " ") = "NEW"    THEN SUBSTRING(cLine, INDEX(cLine, " NEW ")   , 4) = "".
    IF ENTRY(2, cLine, " ") = "GLOBAL" THEN SUBSTRING(cLine, INDEX(cLine, " GLOBAL "), 7) = "".
    IF ENTRY(2, cLine, " ") = "SHARED" THEN SUBSTRING(cLine, INDEX(cLine, " SHARED "), 7) = "".
END.


IF NUM-ENTRIES(cLine, " ") < 3 THEN NEXT lineIteration.  /* we need at least 3 words, this will also skip empty lines */

/* 17-FEB-2007 sla: remove comments from a line */
DEFINE VARIABLE iCommentStart AS INTEGER     NO-UNDO.
iCommentStart = INDEX(cLine, "/~*").
IF iCommentStart > 0 THEN cLine = RIGHT-TRIM(removeLineComments(cLine, iCommentStart)).

{&innerTrim}

IF NUM-ENTRIES(cLine, " ") < 3 THEN NEXT lineIteration.  /* perhaps we have less than 3 words after innerTrim */

IF plOrigSrcFile
 AND lDefBlockMarkDone = NO
 AND ttEdt.lManageableSections = YES /* 20-OCT-2008 sla: improvement for non structured sources, only consider one main block, so we can define a 'little' global variable anywhere in the main block without splitting it into two sections definition and main, which can disturbs a Local load  */
 THEN  DO:
    RUN loadGlobRecordMark (phEditor, "Definitions", "Definitions", piLineCount, SEEK(edtImport) - LENGTH(cLine)).
    lDefBlockMarkDone = YES.     
END.

/* 27-MAY-2007 sla: the easiest was to support the PRIVATE,PROTECTED and PUBLIC access mode option is to keep track of
 it and remove it from cLine to use the same parsing code afterwards */
cAccesMode = ENTRY(2, cLine, " ").
/* 13-OCT-2008 sla: added STATIC  */
IF CAN-DO("PRIVATE,PROTECTED,PUBLIC", cAccesMode)
 THEN SUBSTRING(cLine, INDEX(cLine, " " + cAccesMode), LENGTH(cAccesMode) + 1) = "". /* remove it from the line as explained above */
ELSE cAccesMode = "".

/* 15-OCT-2009 sla: same thing for static */
IF ENTRY(2, cLine, " ") = "STATIC" THEN ASSIGN
 isStatic   = YES /* 13-OCT-2009 sla: we really keep track of it now */
 SUBSTRING(cLine, INDEX(cLine, "STATIC "), 7) = "". /* remove it from the line as explained above */


cDefEntry1 = ENTRY(1, cLine, " ").
cDefEntry2 = ENTRY(2, cLine, " ").
                                     
IF   cDefEntry1 BEGINS "DEF"  /* more reliable than cLine MATCHES "DEF* VAR*"  (think of comments with 'VAR') */
 AND (   cDefEntry2 BEGINS "VAR"
      OR cDefEntry2 = "PROPERTY"  /* support of propert data members */
      OR cDefEntry2 = "DATASET"
      OR cDefEntry2 = "DATA-SOURCE")
 THEN DO:
    cvar  = ENTRY(3, cLine, " ").
    IF CAN-FIND(ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = cVar) THEN NEXT lineIteration.
    CREATE ttgVar.
    ASSIGN
     piLineOK           = piLineOK + 1
     ttgVar.hEditor     = phEditor
     ttgVar.cFileName   = ttEdt.cFileName
     ttgVar.cVar        = cVar
     ttgVar.cAccessMode = cAccesMode
     ttgVar.isStatic    = isStatic /* 13-OCT-2009 sla: now we keep track of it */
     rCatchViewAs       = ROWID(ttgvar)  WHEN cDefEntry2 BEGINS "VAR"  /* 25-FEB-2007 sla: improvement, set this flag only for variables */
     ttgVar.cDataType   = "ABHackDidNotCatch" /* default value */
     iWord              = LOOKUP("AS", cLine, " ").  /* 15-AUG-2007 sla: will try to catch LIKE later  */
    IF iWord > 0 THEN ttgVar.cDataType = ENTRY(iWord + 1, cLine, " ") NO-ERROR.
    CASE ttgVar.cDataType:
        WHEN "CLASS" THEN ttgVar.cDataType = ENTRY(iWord + 2, cLine, " ") NO-ERROR.
        /* aaabreviaaaaaation */
        WHEN "CHAR"  THEN ttgVar.cDataType = "CHARACTER".
        WHEN "INT"   THEN ttgVar.cDataType = "INTEGER".
        WHEN "DEC"   THEN ttgVar.cDataType = "DECIMAL".
        WHEN "LOG"   THEN ttgVar.cDataType = "LOGICAL".
        WHEN "DAT"   THEN ttgVar.cDataType = "DATE".
    END CASE.

    /* 12-DEC-2007 sla: let's try to catch LIKE now */
    IF iWord = 0 THEN DO:
        iWord = LOOKUP("LIKE", cLine, " ").
        IF iWord > 0 THEN DO:
            DEFINE VARIABLE cLikeField AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE cLikeTable AS CHARACTER   NO-UNDO.
            DEFINE VARIABLE hLikeTable AS HANDLE      NO-UNDO.
            DEFINE VARIABLE hLikeField AS HANDLE      NO-UNDO.
            DEFINE BUFFER likettgVar FOR ttgVar.
            cLikeField = ENTRY(iWord + 1, cLine, " ") NO-ERROR.
            
            IF ERROR-STATUS:ERROR = NO THEN DO:
                 cLikeField = RIGHT-TRIM(cLikeField, ".").
                 /* variable defined like another */
                 IF NUM-ENTRIES(cLikeField, ".") = 1 THEN DO:
                     FIND FIRST likettgVar WHERE likettgVar.hEditor = phEditor AND likettgVar.cVar = cLikeField NO-ERROR.
                     IF AVAIL likettgVar THEN ttgVar.cDataType = likettgVar.cDataType.
                 END.
                 ELSE IF NUM-ENTRIES(cLikeField, ".") > 1 THEN DO:
                     cLikeTable = SUBSTRING(cLikeField, 1, R-INDEX(cLikeField, ".") - 1).
                     cLikeField = SUBSTRING(cLikeField, R-INDEX(cLikeField, ".") + 1).
                     hLikeTable = ?.
                     hLikeTable = DYNAMIC-FUNCTION('createTTorDBBuffer' IN gphCaller, phEditor, cLikeTable, "").
                     IF VALID-HANDLE(hLikeTable) THEN DO:
                         hLikeField = hLikeTable:BUFFER-FIELD(cLikeField) NO-ERROR.
                         IF VALID-HANDLE(hLikeField) THEN ttgVar.cDataType = CAPS(hLikeField:DATA-TYPE).
                         IF   VALID-HANDLE(hLikeTable) /* 01-FEB-2008 sla: was missing, could potentially lead to error 3140 */
                          AND VALID-HANDLE(hLikeTable:TABLE-HANDLE)
                          THEN DELETE OBJECT hLikeTable:TABLE-HANDLE NO-ERROR.
                         ELSE DELETE OBJECT hLikeTable NO-ERROR.
                     END.
                 END.
            END. /* ERROR-STATUS:ERROR = no */
        END. /* LOOKUP("LIKE", cLine, " ") > 0 */
    END. /* LOOKUP("AS", cLine, " ") = 0 */
     
    NEXT lineIteration.
END. /* variable case */

ELSE IF   cDefEntry1 BEGINS "DEF"  /* more reliable than cLine MATCHES "DEF* VAR*"  (think of comments with 'VAR') */
      AND ENTRY(3, cLine, " ") BEGINS "PARAM"
      AND NUM-ENTRIES(cLine, " ") > 3 /* 04-SEP-2007 sla: don't raise an error with bad line */
 THEN DO:
    IF CAN-DO("TABLE-HANDLE,TABLE,DATASET,DATASET-HANDLE", ENTRY(4, cLine, " "))
      AND NUM-ENTRIES(cLine, " ") > 4  /* 04-SEP-2007 sla: don't raise an error with bad line */
      THEN cVar = ENTRY(5, cLine, " ").
    ELSE cvar = ENTRY(4, cLine, " "). /* 03-MAY-2007 sla: replaced 3 by 4 to catch GLOBAL parameters  */
    
    /* 19-JAN-2007 sla: already found it in preprocessed version */
    IF CAN-FIND(ttgVar WHERE ttgVar.hEditor = phEditor AND ttgVar.cVar = cVar) THEN NEXT lineIteration.
    
    CREATE ttgVar.
    ASSIGN
     piLineOK           = piLineOK + 1
     ttgVar.hEditor     = phEditor
     ttgVar.cFileName   = ttEdt.cFileName
     ttgVar.cVar        = cVar
     ttgVar.cAccessMode = cAccesMode
     ttgVar.isStatic    = isStatic
     rCatchViewAs       = ROWID(ttgvar)
     
    /* 05-JUL-2007 sla: starting to parse data type of those resources */
     ttgVar.cDataType = "ABHackDidNotCatch" /* default value */
     iWord            = LOOKUP("AS", cLine, " ").  /* 15-AUG-2007 sla: will try to catch LIKE later  */
     
    IF iWord > 0 THEN ttgVar.cDataType = ENTRY(iWord + 1, cLine, " ") NO-ERROR.
    CASE ttgVar.cDataType:
        WHEN "CLASS" THEN ttgVar.cDataType = ENTRY(iWord + 2, cLine, " ") NO-ERROR.
        /* aaabreviaaaaaation */
        WHEN "CHAR"  THEN ttgVar.cDataType = "CHARACTER".
        WHEN "INT"   THEN ttgVar.cDataType = "INTEGER".
        WHEN "DEC"   THEN ttgVar.cDataType = "DECIMAL".
        WHEN "LOG"   THEN ttgVar.cDataType = "LOGICAL".
        WHEN "DAT"   THEN ttgVar.cDataType = "DATE".
    END CASE.
    
    /* 12-DEC-2007 sla: let's try to catch LIKE now */
    IF iWord = 0 THEN DO:
        iWord = LOOKUP("LIKE", cLine, " ").
        IF iWord > 0 THEN DO:
            cLikeField = ENTRY(iWord + 1, cLine, " ") NO-ERROR.
            
            IF ERROR-STATUS:ERROR = NO THEN DO:
                 cLikeField = RIGHT-TRIM(cLikeField, ".").
                 /* variable defined like another */
                 IF NUM-ENTRIES(cLikeField, ".") = 1 THEN DO:
                     FIND FIRST likettgVar WHERE likettgVar.hEditor = phEditor AND likettgVar.cVar = cLikeField NO-ERROR.
                     IF AVAIL likettgVar THEN ttgVar.cDataType = likettgVar.cDataType.
                 END.
                 ELSE IF NUM-ENTRIES(cLikeField, ".") > 1 THEN DO:
                     cLikeTable = SUBSTRING(cLikeField, 1, R-INDEX(cLikeField, ".") - 1).
                     cLikeField = SUBSTRING(cLikeField, R-INDEX(cLikeField, ".") + 1).
                     hLikeTable = ?.
                     hLikeTable = DYNAMIC-FUNCTION('createTTorDBBuffer' IN gphCaller, phEditor, cLikeTable, "").
                     IF VALID-HANDLE(hLikeTable) THEN DO:
                         hLikeField = hLikeTable:BUFFER-FIELD(cLikeField) NO-ERROR.
                         IF VALID-HANDLE(hLikeField) THEN ttgVar.cDataType = hLikeField:DATA-TYPE.
                         /* DELETE OBJECT hLikeTable.   01-FEB-2008 sla: removed that since we do much better afterwards  */
                         IF   VALID-HANDLE(hLikeTable) /* 01-FEB-2008 sla: was missing, could potentially lead to error 3140 */
                          AND VALID-HANDLE(hLikeTable:TABLE-HANDLE)
                          THEN DELETE OBJECT hLikeTable:TABLE-HANDLE NO-ERROR.
                         ELSE DELETE OBJECT hLikeTable NO-ERROR.
                     END.
                 END.
            END. /* ERROR-STATUS:ERROR = no */
        END. /* LOOKUP("LIKE", cLine, " ") > 0 */
    END. /* LOOKUP("AS", cLine, " ") = 0 */
    

    RUN loadGlobParseProcParam (BUFFER ttEdt
                               ,cLine
                               ,"" /* empty means mainblock (external proc params)  */
                               ,INPUT-OUTPUT piLineOK).
     
    NEXT lineIteration.
END. /* parameter case */

ELSE IF cLine BEGINS "FIELD " AND giLastttid > 0 THEN DO:
    RUN loadGlobParseTTField (phEditor, cLine, ittid, INPUT-OUTPUT piLineOK, INPUT-OUTPUT ifldSeq).
    NEXT lineIteration.
END.

ELSE IF cLine BEGINS "INDEX " /* 03-FEB-2007 sla: added tracking of TT indices */ THEN DO:
    IF  giLastttid = 0
     OR NUM-ENTRIES(cLine, " ") < 3
     THEN NEXT lineIteration.
    
    RUN loadGlobParseTTIndex (phEditor, cLine, ittid, INPUT-OUTPUT piLineOK, INPUT-OUTPUT iidxseq).
    NEXT lineIteration.
END.

ELSE IF   cDefEntry1 BEGINS "DEF"
      AND cDefEntry2 = "BUFFER" THEN DO:
    ASSIGN
     cName = ENTRY(3, cLine, " ")
     cFor  = TRIM(ENTRY(5, cLine, " "), '.')
     NO-ERROR.
    IF ERROR-STATUS:ERROR 
     /* 08-JAN-2009 sla:  I want to let the ability to define a global buffer explicitly so ABHack does not report suspicious global buffers when the developper knows what he is doing
         OR cName = cFor  /* redefinition of GLOBAL buffer, ignore it */ */
     THEN NEXT lineIteration.
    IF CAN-FIND(ttgbuffer WHERE ttgbuffer.hEditor = phEditor AND ttgbuffer.cName = cName) THEN NEXT lineIteration.
    CREATE ttgbuffer.
    ASSIGN
     ttgbuffer.hEditor     = phEditor
     ttgbuffer.cFileName   = ttEdt.cFileName
     ttgbuffer.cName       = cName
     ttgbuffer.cfor        = cFor
     ttgbuffer.cAccessMode = cAccesMode
     ttgbuffer.isStatic    = isStatic.
    NEXT lineIteration.
END.  /* buffer case */

ELSE IF   cDefEntry1 BEGINS "DEF"  /* more reliable than cLine MATCHES "DEF* TEMP-TABLE*"  (think of comments with 'temp-table') */
      AND cDefEntry2 = "TEMP-TABLE"
 THEN RUN loadGlobParseTempTable
     (phEditor
     ,cLine
     ,cAccesMode
     ,isStatic
     ,INPUT-OUTPUT iidxseq
     ,INPUT-OUTPUT ifldseq
     ,INPUT-OUTPUT piLineOK
     ,INPUT-OUTPUT ittid).

END. /* main loop */

&UNDEFINE innerTrim
END PROCEDURE.



PROCEDURE loadGlobParseFuncParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER phEditor   AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER pcLine     AS CHARACTER   NO-UNDO. /* already inner trimmed */
DEFINE INPUT        PARAMETER pcFuncName AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER piLineOK   AS INTEGER     NO-UNDO.

DEFINE VARIABLE ciotype AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cName   AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttFunc FOR ttFunc.

FIND FIRST ttFunc WHERE ttFunc.hEditor = phEditor AND ttFunc.cName = pcFuncName. /* MUST be AVAILABLE  */

/* first care of the input/output case */
ciotype = "INPUT". /* default => note this keyword is optional for functions */
IF pcLine BEGINS "INPUT " THEN
 ASSIGN
  pcLine = SUBSTRING(pcLine, 7). /* remove the 'input ' */
ELSE IF pcLine BEGINS "INPUT-OUTPUT " THEN
 ASSIGN
  ciotype = "INPUT-OUTPUT"
  pcLine = SUBSTRING(pcLine, 14). /* remove the 'input-output ' */
ELSE IF pcLine BEGINS "OUTPUT " THEN
 ASSIGN
  ciotype = "OUTPUT"
  pcLine = SUBSTRING(pcLine, 8). /* remove the 'output ' */.

cName = "". /* reset cName, this is safer for an empty param with the usual '( /* parameter-definitions */ )' genarated by the AppBuilder */

IF   ENTRY(1, pcLine, " ") = "BUFFER"
 AND ENTRY(3, pcLine, " ") = "FOR"
 THEN cName = "BUFFER " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "TABLE"
 THEN cName = ciotype + " TABLE " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "DATASET"
 THEN cName = ciotype + " DATASET " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "TABLE-HANDLE"
 THEN cName = ciotype + " TABLE-HANDLE " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "DATASET-HANDLE"
 THEN cName = ciotype + " DATASET-HANDLE " + ENTRY(2, pcLine, " ").

/* main case with scalar parameters */
ELSE IF    NUM-ENTRIES(pcLine, " ") >= 3
       AND pcLine MATCHES "* AS *"
      THEN cName = ciotype + " " + ENTRY(1, pcLine, " ").

IF cName > ""
 /* 29-JAN-2007 sla: avoid puting a param more than once (due to load in 2 phases) */
 AND LOOKUP(cName, ttFunc.cParameters) = 0 THEN DO:
    IF ttFunc.cParameters <> "" THEN ttFunc.cParameters = ttFunc.cParameters + ",".
    ttFunc.cParameters = ttFunc.cParameters + cName.
    piLineOK = piLineOK + 1.
END.
END PROCEDURE.




PROCEDURE loadGlobParseListing :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       see comment at the bottom
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor                  AS HANDLE      NO-UNDO.
DEFINE INPUT  PARAMETER piFileSize                AS INTEGER     NO-UNDO.
DEFINE INPUT  PARAMETER pcTruncatedSourceFileName AS CHARACTER   NO-UNDO.
DEFINE OUTPUT PARAMETER opiResourceFound           AS INTEGER     NO-UNDO.

DEFINE VARIABLE cBlockName              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBlockType              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cBufferName             AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDataBase               AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLine                   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cOldString              AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cSourceFileName         AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iIncludeLevel           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iJumpOverHeader         AS INTEGER     NO-UNDO.
DEFINE VARIABLE iJumpToOffset           AS INTEGER     NO-UNDO.
DEFINE VARIABLE iLineNumber             AS INTEGER     NO-UNDO.
DEFINE VARIABLE iTriggerNumber          AS INTEGER     NO-UNDO. /* I need to identify triggers in a unique way thince the last part of a listing does not provide any identifier*/.
DEFINE VARIABLE iZeroIncLevelLineNumber AS INTEGER     NO-UNDO.
DEFINE VARIABLE lBufferLine             AS LOGICAL     NO-UNDO.
DEFINE VARIABLE lOneHeaderSeen          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE rttmark                 AS ROWID       NO-UNDO.

DEFINE BUFFER ttMark          FOR ttMark.                                                              
DEFINE BUFFER ttEdt           FOR ttEdt.
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer.

&SCOPED-DEFINE innerTrim DO WHILE cOldString <> cLine :   cOldString = cLine.   cLine = REPLACE(cOldString, "  ", " ").  END.

FIND ttEdt WHERE ttEdt.hEditor = phEditor.

/* quick guess to jumb over 66 % of the file */ /* changed from 80% to 66% */
iJumpToOffset = piFileSize * 0.5.

IF glPreprocessors  THEN DO:
    IMPORT STREAM edtImport UNFORM cLine.  /* skip the */
    IMPORT STREAM edtImport UNFORM cLine. /* very */
    IMPORT STREAM edtImport UNFORM cLine. /* first */
    IMPORT STREAM edtImport UNFORM cLine. /* header */
END.
ELSE DO:
    SEEK STREAM edtImport TO iJumpToOffset.
    IMPORT STREAM edtImport UNFORM cLine. /* so the next line will be a 'normal line' */
END.

REPEAT:
    IMPORT STREAM edtImport UNFORMATTED cLine.
    
    IF cLine BEGINS "" THEN iJumpOverHeader = 4.
    IF iJumpOverHeader > 0 THEN DO:
        ASSIGN
         lOneHeaderSeen = YES
         iJumpOverHeader = iJumpOverHeader - 1.
        NEXT.
    END.
    
    IF glPreprocessors THEN DO:
        ASSIGN
         iLineNumber   = 0
         iIncludeLevel = -1. /* will remain with this value if the conversion fails later */
        
        /* 13-AUG-2007 sla: terrible bug, should have been INTEGER(SUBSTRING(cLine,3, 6)) 
         iLineNumber = INTEGER(SUBSTRING(cLine,1, 6)) NO-ERROR. */
        ASSIGN 
         iIncludeLevel           = INTEGER(SUBSTRING(cLine, 1, 2))
         iLineNumber             = INTEGER(SUBSTRING(cLine,3, 6))
         cLine                   = LEFT-TRIM(SUBSTRING(cLine, 13))
         NO-ERROR.
    
        IF iIncludeLevel = 0 AND iLineNumber > 0 THEN iZeroIncLevelLineNumber = iLineNumber.
        
        /* 27-DEC-2007 sla: starting to track definition of preprocessors */
        IF     (   cLine BEGINS "&SCOPED" AND iIncludeLevel = 0
                OR cLine BEGINS "&GLOBAL"
                OR cLine BEGINS "&UNDEFINE") /* 12-JAN-2008 sla: note it is possible to undefine a global preprocessor that was define in a higher include level...  TO make it paerfect, I should just take care this preproc is not just a scoped preproc defined in a low include level of same name as a global preproc defined at a higer level (in the hierarchy)... */
            AND INDEX(cLine, " ") > 0 /* 19-JAN-2008 sla: added this condition to avoid error with ENTRY(2, cLine, " ") if we face with a dummy line that just contains "&SCOPED"  */
            AND ENTRY(2, cLine, " ") > ""
         THEN DO:
            CREATE ttPreproc.
            ASSIGN
             opiResourceFound        = opiResourceFound + 1
             ttPreproc.hEditor       = ttEdt.hEditor
             ttPreproc.cFileName     = ttEdt.cFileName
             ttPreproc.iLineNum      = iZeroIncLevelLineNumber /* this is the line nulber we need to know in the source file being edited */
             ttPreproc.iIncludeLevel = iIncludeLevel
             ttPreproc.cName         = ENTRY(2, cLine, " ")
             ttPreproc.cValue        = SUBSTRING(cLine, INDEX(cLine, " ") + 1)
             ttPreproc.cValue        = SUBSTRING(ttPreproc.cValue, INDEX(ttPreproc.cValue, " ") + 1)
             ttPreproc.cType         = IF cLine BEGINS "&SCOPED" THEN "SCOPED"
                                        ELSE IF cLine BEGINS "&GLOBAL" THEN "GLOBAL"
                                              ELSE "UNDEFINE".
            NEXT.
        END. /* caught a preprocessor definition */
    END. /*  IF glPreprocessors THEN DO: */
    ELSE DO:
         iLineNumber = 0.  /* reset */
         iLineNumber = INTEGER(SUBSTRING(cLine,3, 6)) NO-ERROR.
    END.        
    
    IF iLineNumber <> 0 THEN NEXT.
    
    /* a way to ensure I did not take a first EMPTY line (next to the SEEK jump) like the end of the main part of the listing */
    IF lOneHeaderSeen THEN LEAVE.
    lOneHeaderSeen = YES.
END.


/* at this point we should normaly be at the beginning of the block list part */
REPEAT:
    IF cLine BEGINS CHR(12) AND cline MATCHES "* PROGRESS(R) Page *" /* beware about page header, sadly, we cannot getid off them  */
     THEN DO:
        IMPORT STREAM edtImport UNFORMATTED cLine NO-ERROR. /* take next line then */
        
        /* 13-AUG-2007 sla: Can happen if we reached the end of the stream with the previous import */
        IF ERROR-STATUS:ERROR THEN RETURN.
     END.
    
    /* 15-AUG-2007 sla: another problem with short file name that don't get truncated in a compile listing */
    /* cSourceFileName = REPLACE(SUBSTRING(cLine, 4, 17), "\", "/"). /* note this does not cause any ERROR when cLine is empty */ */
    IF cLine BEGINS "..." THEN cSourceFileName = REPLACE(SUBSTRING(cLine, 4, 17), "\", "/"). /* note this does not cause any ERROR when cLine is empty */
    ELSE cSourceFileName = REPLACE(SUBSTRING(cLine, 1, 20), "\", "/"). /* note this does not cause any ERROR when cLine is empty */

    
    /* ignore page hearder lines, FRAME report lines and those line about include files */
    IF cSourceFileName = pcTruncatedSourceFileName THEN DO:
        /* {&innerTrim} 01-AUG-2007 sla:   this inner trimm should take place later... */
        
        /* reset */
        ASSIGN
         lBufferLine = NO /* 01-AUG-2007 sla: track buffer usage report */
         cBlockType  = ""
         cBlockName  = ""
         iLineNumber = -1 /* 01-AUG-2007 sla: was setting to 0 before.  Now I need to know if I could catch a 0 */
         cLine       = TRIM(SUBSTRING(cLine, 22)).
        
        {&innerTrim}
        ASSIGN
         iLineNumber = INTEGER(ENTRY(1, cLine, " "))
         cBlockType  = ENTRY(2, cLine, " ")
         NO-ERROR.
        
        IF CAN-DO("PROCEDURE,FUNCTION,METHOD", cBlockType)
         THEN cBlockName  = ENTRY(5, cLine, " ") NO-ERROR.
        ELSE cBlockName = "".

        IF CAN-DO("CONSTRUCTOR,DESTRUCTOR", cBlockType) THEN ASSIGN
         cBlockName = cBlockType /* let's handle contructors and destructors like methods... */
         cBlockType = "Method".
/*         IF iLineNumber = 0 THEN DO:                                                                                  */
/*             MESSAGE 'debug'                                                                                          */
/*              VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                      */
/*             OUTPUT TO "CLIPBOARD".                                                                                   */
/*             FOR EACH ttMark WHERE ttMark.hEditor = phEditor:                                                         */
/*                 PUT UNFORMATTED ttMark.cBlockName "~t" ttMark.cBlockType "~t" ttMark.iLine "~t" ttMark.iOffset SKIP. */
/*             END.                                                                                                     */
/*             OUTPUT CLOSE.                                                                                            */
/*         END.                                                                                                         */
        IF iLineNumber > -1 AND CAN-DO("PROCEDURE,FUNCTION,METHOD,TRIGGER", cBlockType) THEN DO:
            IF cBlockType = "TRIGGER" THEN ASSIGN
             iTriggerNumber = iTriggerNumber + 1
             cBlockName     = "Sometrigger" + STRING(iTriggerNumber). /* we will rename that when parsing the source file in the 2nd RUN loadGlobalResourcesInFile  */
        
            /* 02-AUG-2007 sla: special case for the main block that is reported by the very end of the report with a block type of "procedure" */
            IF iLineNumber = 0  THEN ASSIGN
             cBlockType = "MainBlock"
             cBlockName = "main".
        
            /* a ttMark record might already exist if a PROCEDURE or FUNCTION is prototyped */
            FIND FIRST ttMark WHERE
                 ttMark.hEditor    = phEditor
             AND ttMark.cBlockType = cBlockType
             AND ttMark.cBlockName = cBlockName
             NO-ERROR.
            IF NOT AVAILABLE ttMark THEN DO:
                CREATE ttMark.
                ASSIGN
                 ttMark.hEditor    = phEditor
                 ttMark.cBlockType = cBlockType
                 ttMark.cBlockName = cBlockName.
            END.
            ASSIGN
             ttMark.iLine    = iLineNumber
             opiResourceFound = opiResourceFound + 1.
            rttmark = ROWID(ttMark).
        END. /* IF iLineNumber > 0 AND CAN-DO("PROCEDURE,FUNCTION,METHOD,TRIGGER", cBlockType) THEN */
    END. /*     IF cSourceFileName = pcTruncatedSourceFileName THEN  */
    
    /* 01-AUG-2007 sla: now keep track of BUFFER usage.  I am especially interested in the GLOBAL buffers */
    ELSE IF glKeepTrackListingBuffer
     AND rttmark <> ? THEN DO: /* 03-AUG-2007 sla: consider a do or for or repeat block is attached to the last recorded ttMark */
     /* AND NOT CAN-DO(",do,For", cBlockType) THEN DO: */
        cLine = TRIM(cLine).
        IF  cLine = ""
         OR cLine BEGINS "Frames:"
         THEN lBufferLine = NO.
        ELSE IF cLine BEGINS "Buffers:" THEN ASSIGN
         lBufferLine = YES
         cLine       = SUBSTRING(cLine, 10). /* remove 'Buffers: ' */
        IF lBufferLine THEN DO: /* at this point cLine should just contain a buffer name  */
            FIND ttMark WHERE ROWID(ttMark) = rttmark NO-ERROR.
            IF NOT AVAILABLE ttMark THEN DO:
                MESSAGE "no ttmark  looking for main ?" SKIP cOldString
                    VIEW-AS ALERT-BOX INFO BUTTONS OK.
/*                 OUTPUT TO "CLIPBOARD".                                                                                                                                         */
/*                 FOR EACH ttMark:                                                                                                                                               */
/*                     PUT UNFORMATTED STRING(ttMark.hEditor) "~t" ttMark.cBlockName "~t" ttMark.cBlockType "~t" ttMark.iEditorLength "~t" ttMark.iLine "~t" ttMark.iOffset SKIP. */
/*                 END.                                                                                                                                                           */
/*                 OUTPUT CLOSE.                                                                                                                                                  */
                RETURN.
            END.
            
            ASSIGN
             cBufferName = cLine
             cDataBase   = "".
            IF INDEX(cBufferName, ".") > 0 THEN ASSIGN
             cDataBase   = ENTRY(1, cBufferName, ".")
             cBufferName = ENTRY(2, cBufferName, ".").
            FIND FIRST ttReferedBuffer OF ttMark WHERE
                 ttReferedBuffer.cBufferName = cBufferName
             AND ttReferedBuffer.cDataBase   = cDataBase
             NO-ERROR.
            
            /* 03-AUG-2007 sla: beware, perhaps we have already registered a buffer because it was appearing in multiple do or for block (or even repeat)) */
            IF NOT AVAILABLE ttReferedBuffer THEN DO:
                CREATE ttReferedBuffer.
                BUFFER-COPY ttMark TO ttReferedBuffer ASSIGN
                 ttReferedBuffer.cBufferName = cBufferName
                 ttReferedBuffer.cDataBase   = cDataBase.
            END.
        END.
    END. /* keep track of BUFFER usage */
    
    /* 15-AUG-2007 sla: avoid ERROR    ** Attempt to read from closed stream edtImport. (1386)  */
    /*   this ERROR could occur when the file was partially generated because the source file to parse failed to compile */
    IMPORT STREAM edtImport UNFORMATTED cLine NO-ERROR. 
    IF ERROR-STATUS:ERROR THEN LEAVE.
END.

&UNDEFINE innerTrim


/* we are looking for the last part of the listing that mentions block information as follows:
 12254   12179     &ANALYZE-RESUME
 12255   12180     
[...] 
       ...bhack\ABHackWin.w 1896 Do          No                                    
       ...bhack\ABHackWin.w 1912 Trigger     No                                    
           Frames:  fComplSuggestUsedBuffers
       
       ...bhack\ABHackWin.w 1924 Trigger     No                                    
           Frames:  fComplSuggestUsedBuffers
       
       ...bhack\ABHackWin.w 1938 Procedure   No   Procedure CtrlDelay.PSTimerApplyEvent.Tick
           Frames:  fMain
       
       ...bhack\ABHackWin.w 1956 Do          No                                    
       ...bhack\ABHackWin.w 1969 Do          No                                    
       ...bhack\ABHackWin.w 1991 Procedure   No   Procedure CtrlEdtValueChanged.PSTimerValueChanged.Tick
       ...bhack\ABHackWin.w 2013 Procedure   No   Procedure CtrlSpy.PSTimerSpy.Tick
           Buffers: ttAPI
                    ttsection
                    ttEdt
           Frames:  fMain
       
       ...bhack\ABHackWin.w 2044 Do          No                                    
 [...]
       ...bhack\ABHackWin.w 11629 Procedure   No   Procedure waitForEditorsKeyStroke
           Frames:  fMain
       
       ...bhack\ABHackWin.w 11641 For         No                                    
       ...bhack\ABHackWin.w 11653 Function    No   Function abReturnValue           
 [...]
 => characteristics: no line number between position 1 and 6
 
 now make sure we pass the */

END PROCEDURE.




PROCEDURE loadGlobParseMethParam :
/*------------------------------------------------------------------------------
  Purpose:     Parse method parameters
  Parameters:  <none>
  Notes:       This code was started with the code of loadGlobParseFuncParam as base line
               since the sysntax is pretty similar
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER phEditor   AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER pcLine     AS CHARACTER   NO-UNDO. /* already inner trimmed */
DEFINE INPUT        PARAMETER pcFuncName AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER piLineOK   AS INTEGER     NO-UNDO.

DEFINE VARIABLE ciotype AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cName   AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttMethod FOR ttMethod.

FIND FIRST ttMethod WHERE ttMethod.hEditor = phEditor AND ttMethod.cName = pcFuncName. /* MUST be AVAILABLE  */

/* first care of the input/output case */
ciotype = "INPUT". /* default => note this keyword is optional for functions */
IF pcLine BEGINS "INPUT " THEN
 ASSIGN
  pcLine = SUBSTRING(pcLine, 7). /* remove the 'input ' */
ELSE IF pcLine BEGINS "INPUT-OUTPUT " THEN
 ASSIGN
  ciotype = "INPUT-OUTPUT"
  pcLine = SUBSTRING(pcLine, 14). /* remove the 'input-output ' */
ELSE IF pcLine BEGINS "OUTPUT " THEN
 ASSIGN
  ciotype = "OUTPUT"
  pcLine = SUBSTRING(pcLine, 8). /* remove the 'output ' */.

cName = "". /* reset cName, this is safer for an empty param with the usual '( /* parameter-definitions */ )' genarated by the AppBuilder */

IF   ENTRY(1, pcLine, " ") = "BUFFER"
 AND ENTRY(3, pcLine, " ") = "FOR"
 THEN cName = "BUFFER " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "TABLE"
 THEN cName = ciotype + " TABLE " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "DATASET"
 THEN cName = ciotype + " DATASET " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "TABLE-HANDLE"
 THEN cName = ciotype + " TABLE-HANDLE " + ENTRY(2, pcLine, " ").

ELSE IF ENTRY(1, pcLine, " ") = "DATASET-HANDLE"
 THEN cName = ciotype + " DATASET-HANDLE " + ENTRY(2, pcLine, " ").

/* main case with scalar parameters */
ELSE IF    NUM-ENTRIES(pcLine, " ") >= 3
       AND pcLine MATCHES "* AS *"
      THEN cName = ciotype + " " + ENTRY(1, pcLine, " ").

/* 30-OCT-2007 sla: fix ')' and ':' being part of the name */
cName = RIGHT-TRIM(cName, ":").
cName = RIGHT-TRIM(cName, ")").

IF cName > ""
 /* 29-JAN-2007 sla: avoid puting a param more than once (due to load in 2 phases) */
 AND LOOKUP(cName, ttMethod.cParameters) = 0 THEN DO:
    IF ttMethod.cParameters <> "" THEN ttMethod.cParameters = ttMethod.cParameters + ",".
    ttMethod.cParameters = ttMethod.cParameters + cName.
    piLineOK = piLineOK + 1.
END.
END PROCEDURE.




PROCEDURE loadGlobParseProcParam :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.
DEFINE INPUT        PARAMETER pcLine     AS CHARACTER   NO-UNDO. /* already inner trimmed */
DEFINE INPUT        PARAMETER pcProcName AS CHARACTER   NO-UNDO. /* 14-SEP-2007 sla: if EMPTY then main block (ext proc PARAMETER ) */
DEFINE INPUT-OUTPUT PARAMETER piLineOK   AS INTEGER     NO-UNDO.

DEFINE VARIABLE ciotype AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cName   AS CHARACTER   NO-UNDO.

DEFINE BUFFER ttProc FOR ttProc.
    
ciotype = ENTRY(2, pcLine, " "). /* INPUT/OUTPUT */
IF ciotype = "RETURN" THEN ciotype = "OUTPUT".

IF   ENTRY(3, pcLine, " ") = "BUFFER"
 AND ENTRY(5, pcLine, " ") = "FOR"
 THEN cName = "BUFFER " + ENTRY(4, pcLine, " ").

ELSE IF   ENTRY(4, pcLine, " ") = "TABLE"
      AND ENTRY(5, pcLine, " ") = "FOR"
      AND NUM-ENTRIES(pcLine, " ") >= 6
 THEN cName = ciotype + " TABLE " + RIGHT-TRIM(ENTRY(6, pcLine, " "), ".").

ELSE IF   ENTRY(4, pcLine, " ") = "DATASET"
      AND ENTRY(5, pcLine, " ") = "FOR"
      AND NUM-ENTRIES(pcLine, " ") >= 6
 THEN cName = ciotype + " DATASET " + RIGHT-TRIM(ENTRY(6, pcLine, " "), ".").

ELSE IF   ENTRY(4, pcLine, " ") = "TABLE-HANDLE"
 THEN cName = ciotype + " TABLE-HANDLE " + RIGHT-TRIM(ENTRY(5, pcLine, " "), ".").

ELSE IF   ENTRY(4, pcLine, " ") = "DATASET-HANDLE"
 THEN cName = ciotype + " DATASET-HANDLE " + RIGHT-TRIM(ENTRY(5, pcLine, " "), ".").

ELSE /* main case with scalar parameters */
 cName = ciotype + " " + ENTRY(4, pcLine, " ").
 
IF pcProcName = "" THEN DO: /* 14-SEP-2007 sla: now keep track of external parameters when pcProcName = "" */
    IF LOOKUP(cName, ttEdt.cParameters) = 0 THEN DO:
        IF ttEdt.cParameters > "" THEN ttEdt.cParameters = ttEdt.cParameters + ",".
        ttEdt.cParameters = ttEdt.cParameters + cName.
        /* do not count this line twice 
          piLineOK = piLineOK + 1. */
    END.
END.

ELSE DO: /* internal (most frequent case) */
    FIND FIRST ttProc WHERE ttProc.hEditor = ttEdt.hEditor AND ttProc.cName = pcProcName. /* This record *must* exist */
    /* 29-JAN-2007 sla: avoid puting a param more than once (due to load in 2 phases) */
    IF LOOKUP(cName, ttProc.cParameters) = 0 THEN DO:
        IF ttProc.cParameters > "" THEN ttProc.cParameters = ttProc.cParameters + ",".
        ttProc.cParameters = ttProc.cParameters + cName.
        piLineOK = piLineOK + 1.
    END.
END. 

END PROCEDURE.




PROCEDURE loadGlobParseTempTable :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
  
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER phEditor     AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER cLine        AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER pcAccessMode AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER pIsStatic    AS LOGICAL     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iidxseq      AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER ifldseq      AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER piLineOK     AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopittid        AS INTEGER     NO-UNDO.

DEFINE VARIABLE cBeforeTable      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cEntryToRemove    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cTableLike        AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cttname           AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iBeforeTableEntry AS INTEGER     NO-UNDO.
DEFINE VARIABLE iEntryToRemove    AS INTEGER     NO-UNDO.

DEFINE BUFFER tttt  FOR tttt.
DEFINE BUFFER ttIdx FOR ttIdx.
DEFINE BUFFER ttfld FOR ttfld.
DEFINE BUFFER ttEdt FOR ttEdt.

FIND ttEdt WHERE ttEdt.hEditor = phEditor. /* 09-AUG-2007 sla: now necessary to keep track of cFileName */

cLine = REPLACE(cLine, " NO-UNDO", "").

/* 29-MAY-2007 sla: new options to remove to keep the same code with same entries afterwards */
cLine = REPLACE(cLine, " REFERENCE-ONLY", "").
IF INDEX(cLine, " NAMESPACE-URI ") > 0 THEN DO:
    iEntryToRemove = LOOKUP("NAMESPACE-URI", cLine, " ") + 1.
    ENTRY(iEntryToRemove, cLine, " ") = "ABhackRemoveThisOptionPlease" NO-ERROR.
    IF ERROR-STATUS:ERROR
     THEN cLine = REPLACE(cLine, " NAMESPACE-URI", "").
    ELSE  cLine = REPLACE(cLine, " NAMESPACE-URI ABhackRemoveThisOptionPlease", "").
END.
IF INDEX(cLine, " NAMESPACE-PREFIX ") > 0 THEN DO:
    iEntryToRemove = LOOKUP("NAMESPACE-PREFIX", cLine, " ") + 1.
    ENTRY(iEntryToRemove, cLine, " ") = "ABhackRemoveThisOptionPlease" NO-ERROR.
    IF ERROR-STATUS:ERROR
     THEN cLine = REPLACE(cLine, " NAMESPACE-PREFIX", "").
    ELSE  cLine = REPLACE(cLine, " NAMESPACE-PREFIX ABhackRemoveThisOptionPlease", "").
END.

IF INDEX(cLine, " BEFORE-TABLE ") > 0 THEN DO:
    iBeforeTableEntry = LOOKUP("BEFORE-TABLE", cLine, " ") + 1.
    ASSIGN
     cBeforeTable = RIGHT-TRIM(ENTRY(iBeforeTableEntry, cLine, " "), ".") /* 14-OCT-2008 sla: removed possible trialing full stop when the BEFORE-TABLE option is used at the end of the DEFINITION statement */
     ENTRY(iEntryToRemove, cLine, " ") = "ABhackRemoveThisOptionPlease"
     NO-ERROR.
    IF ERROR-STATUS:ERROR
     THEN cLine = REPLACE(cLine, " BEFORE-TABLE", "").
    ELSE  cLine = REPLACE(cLine, " BEFORE-TABLE ABhackRemoveThisOptionPlease", "").
END.


cttname = ENTRY(3, cLine, " ") NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN.

FIND FIRST tttt WHERE tttt.hEditor = phEditor AND tttt.cttname = cttname NO-ERROR.
/* 19-JAN-2007 sla: already found it in preprocessed version, keep iopittid so we can skip the 2nd definition of fields */
IF AVAILABLE tttt THEN DO:
    iopittid = tttt.ittid.
    /* 06-FEB-2007 sla: solve unique index violation issues... */
    FIND LAST ttIdx WHERE ttIdx.ittid = tttt.ittid USE-INDEX ittidseq NO-ERROR.
    iidxseq = IF AVAILABLE ttIdx THEN ttIdx.idxSeq ELSE 0.
    FIND LAST ttfld WHERE ttfld.ittid = tttt.ittid USE-INDEX ittidSeq NO-ERROR.
    ifldseq = IF AVAILABLE ttfld THEN ttfld.ifldSeq ELSE 0.
    
    RETURN.
END.
    
CREATE tttt.
ASSIGN
 piLineOK          = piLineOK + 1
 giLastttid        = giLastttid + 1
 iopittid          = giLastttid
 ifldSeq           = 0
 iidxseq           = 0
 tttt.hEditor      = phEditor
 tttt.cFileName    = ttEdt.cFileName
 tttt.ittid        = giLastttid
 tttt.cAccessMode  = pcAccessMode
 tttt.isStatic     = pIsStatic
 tttt.cttname      = cttname
 tttt.cBeforeTable = cBeforeTable.
/* TT defined with LIKE option => add fields of source table */
IF cLine MATCHES "* LIKE *" THEN DO:
    cTableLike = RIGHT-TRIM(ENTRY(5, cLine, " "), ".") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.
    RUN addLikeFields  (phEditor
                        ,cTableLike
                        ,tttt.ittid
                        ,INPUT-OUTPUT ifldSeq).
                        /*  ,INPUT-OUTPUT TABLE ttfld).  /* 22-MAY-2007 sla: why input-output table ? */ */
    RUN addLikeIndices (phEditor
                        ,cTableLike
                        ,tttt.ittid
                        ,INPUT-OUTPUT iidxseq).
                        /*  ,INPUT-OUTPUT TABLE ttIdx). /* 22-MAY-2007 sla: why input-output table ? */  */
END.

END PROCEDURE.




PROCEDURE loadGlobParseTTField :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER phEditor   AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER pcLine     AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER pittid     AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiLineOK AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopifldSeq AS INTEGER     NO-UNDO.

DEFINE VARIABLE cAsLike     AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cDataType   AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cfldname    AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cLikeBuffer AS CHARACTER   NO-UNDO.
DEFINE VARIABLE hLikeBuffer AS HANDLE      NO-UNDO.

DEFINE BUFFER ttfld FOR ttfld.
DEFINE BUFFER tttt FOR tttt.

/* 13-AUG-2007 sla: protection against unique index violation for an unexpected fields */
FIND FIRST tttt WHERE tttt.hEditor = phEditor
                  AND tttt.ittid = pittid NO-ERROR.
IF NOT AVAILABLE tttt
 OR tttt.hEditor <> phEditor
 THEN RETURN. /* there was probably a syntax error since the TT has not been recorded yet */
                           
                           
/* 18-DEC-2006 sla: The adm2 affords to define mutliple fields in one single line... */
DO WHILE TRUE:
    /* 02-JUN-2007 sla: this code could catch a bad comment with ' as ' or ' like '
        => It is more reliable to check entry(3,  ," ") */
    /* 13-DEC-2006 sla: replaced MATCHES " *LIKE* " by correct MATCHES "* LIKE *"  */
    /* IF NOT pcLine MATCHES "* AS *" AND NOT pcLine MATCHES "* LIKE *" THEN RETURN. */
    
    pcLine = SUBSTRING(pcLine, INDEX( pcLine, "FIELD")).
    cAsLike = ENTRY(3, pcLine, " ").
    IF NOT CAN-DO("AS,LIKE", cAsLike) THEN RETURN. /* comment line... */
    
    cfldname = ENTRY(2, pcLine, " ") NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN.
    
    /* 19-JAN-2007 sla: already found it in preprocessed version */
    IF CAN-FIND(ttfld WHERE ttfld.ittid = pittid AND ttfld.cfldname = cfldname) THEN RETURN.

    cDatatype = TRIM(ENTRY(4, pcLine, " "), ".") NO-ERROR. /* will do fine if 3rd entry is "AS" */
    IF ERROR-STATUS:ERROR THEN cDatatype = "CHARACTER". /* default when invalid (no 4th entry)*/
    
    IF cAsLike = "LIKE" THEN DO: /* LIKE */ 
        IF NUM-ENTRIES(cDataType, ".") = 1 THEN DO:
            /* 02-JUN-2007 sla: quick and dirty guess...  I will be able to do better when
             I parse the datatype of global variables */
            IF cDataType BEGINS "i" OR cDataType BEGINS "gi" THEN cDataType = "INTEGER".
            ELSE cDataType = "CHARACTER".
        END. 
        ELSE DO:
            ASSIGN
             cLikeBuffer = ENTRY(1, cDatatype, ".")
             hLikeBuffer = createTTorDBBuffer(phEditor, cLikeBuffer, "ignoreIndices").
             cDatatype = hLikeBuffer:BUFFER-FIELD(ENTRY(2, cDatatype, ".")):DATA-TYPE NO-ERROR.
            IF ERROR-STATUS:ERROR THEN cDatatype = "CHARACTER".
            ELSE DO:
                IF hLikeBuffer:DBNAME = "PROGRESST" THEN DO:
                    DEFINE VARIABLE httToDelete AS HANDLE      NO-UNDO.
                    httToDelete = hLikeBuffer:TABLE-HANDLE.
                    DELETE OBJECT httToDelete.
                END.
                ELSE DELETE OBJECT hLikeBuffer. /* 27-AUG-2007 sla: solve possible memory leak */
            END.
        END.
    END.
    
    CREATE ttfld.
    ASSIGN
     iopiLineOK      = iopiLineOK + 1
     iopifldSeq      = iopifldSeq + 1
     ttfld.ittid     = pittid
     ttfld.ifldSeq   = iopifldSeq
     ttfld.cfldname  = cfldname
     ttfld.cDataType = cDatatype.
    VALIDATE ttfld. /* flush indices */
    IF INDEX(pcLine, " FIELD ", INDEX(pcLine, "FIELD ") + 6) = 0 THEN  RETURN. /* no more fields in this line, next line interation */
    /* there are other fields left in this line => parse them */
    pcLine = SUBSTRING(pcLine, INDEX(pcLine, " FIELD") + 1). /* the space before field is very important: it makes the parser consider the enxt field */
END. /* DO WHILE TRUE */

END PROCEDURE.




PROCEDURE loadGlobParseTTIndex :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
DEFINE INPUT        PARAMETER phEditor    AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER pcLine      AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER pittid      AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiLineOK  AS INTEGER     NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopiiidxseq AS INTEGER     NO-UNDO.

DEFINE VARIABLE cIdxFieldsInfo AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cIdxname       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cIdxOptn       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE cWord          AS CHARACTER   NO-UNDO.
DEFINE VARIABLE iMaxWord       AS INTEGER     NO-UNDO.
DEFINE VARIABLE iWord          AS INTEGER     NO-UNDO.

DEFINE BUFFER ttIdx FOR ttIdx.
DEFINE BUFFER tttt FOR tttt.

/* 13-AUG-2007 sla: protection against unique index violation for an unexpected fields */
FIND FIRST tttt WHERE tttt.hEditor = phEditor
                  AND tttt.ittid = pittid NO-ERROR.
IF NOT AVAILABLE tttt
 OR tttt.hEditor <> phEditor
 THEN RETURN. /* there was probably a syntax error since the TT has not been recorded yet */

pcLine = RIGHT-TRIM(pcLine, ".").
cIdxname = ENTRY(2, pcLine, " ").

/* already found it in preprocessed version */
IF CAN-FIND(FIRST ttIdx WHERE ttIdx.ittid = pittid AND ttIdx.cidxname = cIdxname) THEN RETURN.
ASSIGN           
 cIdxOptn       = "<none>" /* trick to make sure we pass the 'IS' keyword */
 iMaxWord       = NUM-ENTRIES(pcLine, " ")
 cIdxFieldsInfo = "".
DO iWord = 3 TO iMaxWord:
    cWord = ENTRY(iWord, pcLine, " ").
    cWord = RIGHT-TRIM(cWord, "."). 
    IF cWord BEGINS "/~*" THEN LEAVE. /* I hope we will not have comments in the middle of the definition of a index " */
    /* 23-FEB-2007 sla: It seems that 'AS' is allowed, seen in adm2 include files... */
    IF (cWord = "IS" OR cWord = "AS") AND cIdxOptn = "<none>" THEN DO:
        cIdxOptn = "".
        NEXT.
    END.
    IF CAN-DO("PRIMARY,UNIQUE,WORD-INDEX", cWord) AND cIdxOptn <> "<none>" THEN DO:
        cIdxOptn = cIdxOptn + "," + cWord.
        NEXT.
    END.
    
    IF cWord BEGINS "ASC" OR cWord BEGINS "DESC" THEN NEXT. /* already taken into account bellow */
    
    cIdxFieldsInfo = cIdxFieldsInfo + "," + cWord.
    IF iWord = iMaxWord THEN DO:  /* Avoid error when looking at next word */
        cIdxFieldsInfo = cIdxFieldsInfo + ",asc".
        LEAVE.
    END.
    cWord = ENTRY(iWord + 1, pcLine, " ").
    IF cWord BEGINS "DESC" THEN cIdxFieldsInfo = cIdxFieldsInfo + ",desc" .
    ELSE cIdxFieldsInfo = cIdxFieldsInfo + ",asc". 
END.
cIdxFieldsInfo = SUBSTRING(cIdxFieldsInfo, 2).
cIdxOptn = IF cIdxOptn = "<none>" THEN "" ELSE SUBSTRING(cIdxOptn, 2).

CREATE ttIdx.
ASSIGN
 iopiLineOK        = iopiLineOK + 1
 iopiiidxseq       = iopiiidxseq + 1
 ttIdx.ittid       = pittid
 ttIdx.idxSeq      = iopiiidxseq
 ttIdx.cidxname    = cIdxName
 ttIdx.cFieldsInfo = cIdxFieldsInfo
 ttIdx.coptn       = cIdxOptn.

END PROCEDURE.




PROCEDURE loadGlobRecordMark :
/*------------------------------------------------------------------------------
  Purpose:     register line number and file offset for a given block type
               21-MAY-2007 sla: for now I keep track only of procedures and functions 
               but will later manage triggers, and methods...
  Notes:       
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER phEditor     AS HANDLE      NO-UNDO.
    DEFINE INPUT PARAMETER pcBlockName  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcBlockType  AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER piLineNumber AS INTEGER     NO-UNDO.
    DEFINE INPUT PARAMETER piOffset     AS INTEGER     NO-UNDO.

    DEFINE BUFFER ttMark FOR ttMark.

    /* if we were about to add a 2nd main mark after another, then update the current one */
    IF pcBlockType = "main" THEN DO:
        FIND LAST ttMark WHERE
             ttMark.hEditor = phEditor
         AND ttMark.iLine < piLineNumber
         NO-ERROR.
        IF AVAILABLE ttMark AND ttMark.cBlockType <> "main" THEN RELEASE ttMark.
    END.
    
    ELSE FIND ttMark WHERE
         ttMark.hEditor = phEditor
     AND ttMark.cBlockName = pcBlockName
     AND ttMark.cBlockType = pcBlockType
     NO-ERROR.
    
    IF NOT AVAILABLE ttMark THEN DO:
        CREATE ttMark.
        ASSIGN
         ttMark.cBlockName = pcBlockName
         ttMark.cBlockType = pcBlockType
         ttMark.hEditor    = phEditor.
    END.
    
    ASSIGN 
     ttMark.iLine   = piLineNumber
     ttMark.iOffset = piOffset.
END PROCEDURE.


PROCEDURE LoadLibFuncsFromCat:
    DEFINE INPUT  PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcOption   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.
    
    DEFINE VARIABLE cLibFileName AS CHARACTER   NO-UNDO.

    ERROR-STATUS:ERROR = NO.
        
    CASE gcGlobalResCat:
        WHEN "disabled" THEN RETURN. /* we should not have called it, but never mind */
        WHEN "XML" THEN DO:
            
            cLibFileName = gcDumpedResourceFileRoot + "/" + REPLACE(pcLoadFile, ":", "Drive") + ".abhack".
            RUN protools/abhack/abhackLoadLibFuncsXml.p
                 (cLibFileName
                 ,gcDumpedResourceFileRoot
                 ,pcOption
                 ,OUTPUT TABLE ttFreeList) NO-ERROR.
        END.

        WHEN "DB" THEN RUN loadLibFuncFromDb IN ghAbhackDbLib
                     (pcLoadFile
                     ,pcOption
                     ,OUTPUT TABLE ttFreeList)
                     NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable gcGlobalResCat" SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
END PROCEDURE.


PROCEDURE LoadLibProcsFromCat:
    DEFINE INPUT  PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcOption   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.
    
    DEFINE VARIABLE cLibFileName AS CHARACTER   NO-UNDO.

    ERROR-STATUS:ERROR = NO.
        
    CASE gcGlobalResCat:
        WHEN "disabled" THEN RETURN. /* we should not have called it, but never mind */
        WHEN "XML" THEN DO:
            
            cLibFileName = gcDumpedResourceFileRoot + "/" + REPLACE(pcLoadFile, ":", "Drive") + ".abhack".
            RUN protools/abhack/abhackLoadLibProcsXml.p
                 (cLibFileName
                 ,gcDumpedResourceFileRoot
                 ,pcOption
                 ,OUTPUT TABLE ttFreeList) NO-ERROR.
        END.

        WHEN "DB" THEN RUN loadLibProcFromDb IN ghAbhackDbLib
                     (pcLoadFile
                     ,pcOption
                     ,OUTPUT TABLE ttFreeList)
                     NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable gcGlobalResCat" SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
END PROCEDURE.


PROCEDURE loadPropMethFromCat:
    DEFINE INPUT  PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcOption   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.
    
    DEFINE VARIABLE cClassFileName AS CHARACTER   NO-UNDO.

    ERROR-STATUS:ERROR = NO.
        
    CASE gcGlobalResCat:
        WHEN "disabled" THEN RETURN. /* we should not have called it, but never mind */
        WHEN "XML" THEN DO:
            cClassFileName = gcDumpedResourceFileRoot + "/" + REPLACE(pcLoadFile, ":", "Drive") + ".abhack".
            RUN protools/abhack/abhackLoadLibProcsXml.p
                 (cClassFileName
                 ,gcDumpedResourceFileRoot
                 ,pcOption
                 ,OUTPUT TABLE ttFreeList)
                 NO-ERROR.
        END.
        WHEN "DB" THEN RUN loadPropMethFromDb IN ghAbhackDbLib
                     (REPLACE(pcLoadFile, "\", "/")
                     ,pcOption
                     ,INPUT-OUTPUT TABLE ttFreeList)
                     NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable gcGlobalResCat" SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.


END PROCEDURE.


PROCEDURE addBeforeTable:    
    DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.
    
    DEFINE BUFFER ttbeforefld      FOR ttfld.
    DEFINE BUFFER ttbeforeIdx      FOR ttIdx.
    DEFINE BUFFER ttHasBeforeTable FOR tttt.
    
    DEFINE BUFFER tttt  FOR tttt.
    DEFINE BUFFER ttfld FOR ttfld.
    DEFINE BUFFER ttIdx FOR ttIdx.
    
    FOR EACH ttHasBeforeTable WHERE ttHasBeforeTable.hEditor = phEditor
                                AND ttHasBeforeTable.cBeforeTable > "": /* 22-MAY-2007 sla: fixed bug: was AND tttt.cBeforeTable > "" */
        giLastttid = giLastttid + 1.
        CREATE tttt.
        BUFFER-COPY ttHasBeforeTable TO tttt
         ASSIGN
          tttt.cBeforeTable = ""
          tttt.cttname      = ttHasBeforeTable.cBeforeTable
          tttt.ittid        = giLastttid.
        
        FOR EACH ttbeforefld WHERE ttbeforefld.ittid = ttHasBeforeTable.ittid:
            CREATE ttfld.
            BUFFER-COPY ttbeforefld TO ttfld 
             ASSIGN ttfld.ittid = tttt.ittid.
        END.
        /* 24-OCT-2007 sla: why? RELEASE ttfld NO-ERROR. */
    
        FOR EACH ttbeforeIdx WHERE ttbeforeIdx.ittid = ttHasBeforeTable.ittid:
            CREATE ttIdx.
            BUFFER-COPY ttbeforeIdx TO ttIdx
             ASSIGN ttIdx.ittid = tttt.ittid.
        END.
        /* 24-OCT-2007 sla: why? RELEASE ttIdx NO-ERROR.*/
        
        /* 24-OCT-2007 sla: why?  RELEASE tttt.*/
    END. /* for each tttt having a before-table: add it to tttt and its two friends */
END PROCEDURE.


FUNCTION errReturnValue RETURNS CHARACTER( ):    
    IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.
    RETURN ProgressErrors().
END FUNCTION.

FUNCTION ProgressErrors RETURNS CHARACTER( ):    
    DEFINE VARIABLE cMessages AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMessage  AS INTEGER     NO-UNDO.
    
    DO iMessage = 1 TO ERROR-STATUS:NUM-MESSAGES:
        cMessages = cMessages + "   |   " + ERROR-STATUS:GET-MESSAGE(iMessage).
    END.
    cMessages = SUBSTRING(cMessages, 8).
    RETURN cMessages.
END FUNCTION.


PROCEDURE resolveSubtypeChain:    
    DEFINE INPUT  PARAMETER pcObjChain AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType    AS CHARACTER   NO-UNDO.

    CASE gcGlobalResCat:
        WHEN "XML" THEN RETURN ERROR "resolveSubtypeChain operation supported only with abhackDB (08-AUG-2008)".
        WHEN "DB" THEN RUN resolveSubtypeChain IN ghAbhackDbLib (pcObjChain, OUTPUT opcType) NO-ERROR.
        OTHERWISE DO:
            MESSAGE "Unexpected value of " QUOTER(gcGlobalResCat) "in shared variable" gcGlobalResCat SKIP
             "About to return, but please fix that..."
                VIEW-AS ALERT-BOX INFO BUTTONS OK.
            RETURN.
        END.
    END CASE.
    
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
END PROCEDURE.


FUNCTION protectWrappedContent RETURNS CHARACTER
 (INPUT pcExp  AS CHARACTER
 ,INPUT pcOptn AS CHARACTER 
 ,INPUT piWord AS INTEGER):    
/*------------------------------------------------------------------------------
  Purpose: change '(' and ')' to respectivally chr(1) and chr(2) for pairs that
  can be considered as part of a word expression
  
  Mainly used in abhackwin.w/extractWordN to keep paremeter in chained method expressions
  A simple REPLACE() transform will put the parentheses back at the end of the
  extractWordN process
  
  Author slacroix   09-SEP-2008

  first implemented with pcOptn = "parentheses"  
  
  Examples with piWord <= 0  (protecting from right to left) :
  
  searching backward  (piWord is negative)
  substring(myMethod("someParams"):getMember 
           ^kept    ^chr(1)      ^chr(2)


  substring(myMethod("someParams")):getMember 
           ^chr(1)  ^chr(1)      ^^chr(2)

  meParams"):getMember 
           ^chr(2)


  INTEGER(cSomeVar)    +   (someObj:getMember
         ^chr(1)  ^chr(2)  ^kept
  
  
  When piWord > 0 (protecting from left to right), then my intuition tells me we should just apply the opposite ;)                                           
------------------------------------------------------------------------------*/
    DEFINE VARIABLE cChar           AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cConvertedArray AS CHARACTER   EXTENT 5 NO-UNDO.
    DEFINE VARIABLE cProtectedList  AS CHARACTER   INITIAL " ,:'~"" NO-UNDO.
    DEFINE VARIABLE iChar           AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iExpLength      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNumClosingPar  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iNumOpeningPar  AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iProtectedChar  AS INTEGER     NO-UNDO.
    
    ASSIGN
     iExpLength         = LENGTH(pcExp)
     cConvertedArray[1] = CHR(3)
     cConvertedArray[2] = CHR(4)
     cConvertedArray[3] = CHR(5)
     cConvertedArray[4] = CHR(6)
     cConvertedArray[5] = CHR(7).
    
    IF piWord <= 0 THEN DO:  /* search backward */
        iChar = iExpLength.
        DO WHILE iChar > 0:
            cChar = SUBSTRING(pcExp, iChar, 1).
            IF cChar = ")" THEN DO:
                iNumClosingPar = iNumClosingPar + 1.
                SUBSTRING(pcExp, iChar, 1) = CHR(2).
            END.
            ELSE IF cChar = "(" THEN DO:
                iNumClosingPar = iNumClosingPar - 1.
                IF iNumClosingPar >= 0 THEN SUBSTRING(pcExp, iChar, 1) = CHR(1).
            END.
            ELSE IF iNumClosingPar > 0 THEN DO:
                iProtectedChar = INDEX(cProtectedList, cChar).
                IF iProtectedChar > 0 THEN SUBSTRING(pcExp, iChar, 1) = cConvertedArray[ iProtectedChar].
            END.
            iChar = iChar - 1.
        END.
    END.

    
    IF piWord > 0 THEN DO:  /* search forward */
        iChar = 1.
        DO WHILE iChar < iExpLength:
            cChar = SUBSTRING(pcExp, iChar, 1).
            IF cChar = "(" THEN DO:
                iNumOpeningPar = iNumOpeningPar + 1.
                SUBSTRING(pcExp, iChar, 1) = CHR(1).
            END.
            ELSE IF cChar = ")" THEN DO:
                iNumOpeningPar = iNumOpeningPar - 1.
                IF iNumOpeningPar >= 0 THEN SUBSTRING(pcExp, iChar, 1) = CHR(2).
            END.
            /* 20-OCT-2008 sla: this part was forgottent in the first implementation */
            ELSE IF iNumOpeningPar > 0 THEN DO:
                iProtectedChar = INDEX(cProtectedList, cChar).
                IF iProtectedChar > 0 THEN SUBSTRING(pcExp, iChar, 1) = cConvertedArray[ iProtectedChar].
            END.
            iChar = iChar + 1.
        END.
    END.

    RETURN pcExp.
END FUNCTION.

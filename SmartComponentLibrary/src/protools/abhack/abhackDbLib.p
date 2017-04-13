/*------------------------------------------------------------------------------
         abhackDbLib.p   02-SEP-2007  by slacroix
  Purpose: parsing library connected to abhack.db
  Parameters:  <none>
  Notes:       Started, stopped, and used only by abhackParser.p
------------------------------------------------------------------------------*/
{protools/abhack/ABHackResourcesTT.i &SHARED="SHARED"}

&IF DBTYPE("abhack") <> ? &THEN


&SCOPED-DEFINE redefineChildrenBufferScope DEFINE BUFFER ttfld           FOR ttfld          . ~
DEFINE BUFFER ttFunc          FOR ttFunc         . ~
DEFINE BUFFER ttgbuffer       FOR ttgbuffer      . ~
DEFINE BUFFER ttgVar          FOR ttgVar         . ~
DEFINE BUFFER ttIdx           FOR ttIdx          . ~
DEFINE BUFFER ttMark          FOR ttMark         . ~
DEFINE BUFFER ttMethod        FOR ttMethod       . ~
DEFINE BUFFER ttProc          FOR ttProc         . ~
DEFINE BUFFER ttReferedBuffer FOR ttReferedBuffer. ~
DEFINE BUFFER tttt            FOR tttt           . ~
DEFINE BUFFER ttUsing         FOR ttUsing        . ~
DEFINE BUFFER ttgLibHandle    FOR ttgLibHandle   . ~
DEFINE BUFFER ttPreproc       FOR ttPreproc      . ~
DEFINE BUFFER ahfld           FOR ahfld          . ~
DEFINE BUFFER ahFunc          FOR ahFunc         . ~
DEFINE BUFFER ahgbuffer       FOR ahgbuffer      . ~
DEFINE BUFFER ahgVar          FOR ahgVar         . ~
DEFINE BUFFER ahIdx           FOR ahIdx          . ~
DEFINE BUFFER ahMark          FOR ahMark         . ~
DEFINE BUFFER ahMethod        FOR ahMethod       . ~
DEFINE BUFFER ahProc          FOR ahProc         . ~
DEFINE BUFFER ahReferedBuffer FOR ahReferedBuffer. ~
DEFINE BUFFER ahtt            FOR ahtt           . ~
DEFINE BUFFER ahUsing         FOR ahUsing        . ~
DEFINE BUFFER ahgLibHandle    FOR ahgLibHandle   . ~
DEFINE BUFFER ahpreproc       FOR ahpreproc      . /* 01-FEB-2008 sla: that one was missing, leading to lock leaks */

/* 18-OCT-2008 sla: better support of Progress.Lang.* */
DEFINE TEMP-TABLE ttProgressLangClass NO-UNDO
 FIELD cType     AS CHARACTER
 FIELD cInherits AS CHARACTER
 INDEX cType cType.

DEFINE TEMP-TABLE ttProgressLangMember NO-UNDO
 FIELD cType       AS CHARACTER
 FIELD cMember     AS CHARACTER
 FIELD cReturnType AS CHARACTER
 FIELD cParamsType AS CHARACTER
 INDEX TypeMember cType cMember.

&IF NOT PROVERSION BEGINS "9." AND PROVERSION >= "10.0" &THEN
DEFINE DATASET dsProgressLangMembers FOR ttProgressLangClass, ttProgressLangMember
 DATA-RELATION drType FOR ttProgressLangClass, ttProgressLangMember
   RELATION-FIELDS (ctype, ctype) NESTED.

DEFINE VARIABLE cProgressLangMembersXmlFile AS CHARACTER   NO-UNDO.
cProgressLangMembersXmlFile = SEARCH("protools/abhack/ProgressLangDefs.xml").
DATASET dsProgressLangMembers:READ-XML("FILE"
                                       ,cProgressLangMembersXmlFile
                                       ,"EMPTY"
                                       ,?  /* schema location */ /* 22-OCT-2008 sla: the "" seems to cause problem in old version like 10.1B03, so now using ? instread  */
                                       ,NO /* override defult mapping */).
&ENDIF


FUNCTION ProgressErrors RETURNS CHARACTER( ) FORWARD.
FUNCTION errReturnValue RETURNS CHARACTER( ) FORWARD.
FUNCTION referredClassPath RETURNS CHARACTER
  (pcInClass      AS CHARACTER
  ,pcReferedClass AS CHARACTER) FORWARD.

DEFINE TEMP-TABLE ttosFile NO-UNDO LABEL "ttosFile (to load ABHack Global Definition Description files)"
 FIELD cFileName     AS CHARACTER
 FIELD cFullPathName AS CHARACTER
 INDEX cFullPathName IS PRIMARY UNIQUE cFullPathName.



main-block:
DO:

END.


PROCEDURE addSourceFilesFromCatDb:
    DEFINE INPUT        PARAMETER pcDir             AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcMatches         AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcSort            AS CHARACTER   NO-UNDO. /* similar to pcSort of abhackwin.w/prepareFileList */
    DEFINE INPUT-OUTPUT PARAMETER iopiFile          AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER piMaxFile         AS INTEGER     NO-UNDO.
    DEFINE INPUT        PARAMETER piMaxLoadTime     AS INTEGER     NO-UNDO.

    DEFINE VARIABLE cDiretory          AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFileBegins        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFileExtention     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRelativePath      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iRelativePathStart AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSlashIndex        AS INTEGER     NO-UNDO.
    IF R-INDEX(pcMatches, ".") = 0 THEN RETURN. /* this should not happen */


    DEFINE BUFFER ahsrc FOR ahsrc.

    IF INDEX(pcMatches, "*") = 0 THEN pcMatches = pcMatches + "*".
    ASSIGN
     pcDir              = REPLACE(pcDir, "\", "/")
     cFileBegins        = pcDir + "/" + SUBSTRING(pcMatches, 1, INDEX(pcMatches, "*") - 1)
     cFileBegins        = LEFT-TRIM(cFileBegins, "/")
     cFileExtention     = SUBSTRING(pcMatches, R-INDEX(pcMatches, ".") + 1)
     iRelativePathStart = LENGTH(pcDir) + 2 /* = length(pcDir) + 1 + length('/')  */
     pcMatches          = pcDir + "/" + pcMatches.

    FIND FIRST ahsrc NO-LOCK WHERE
         ahsrc.fileExtentions = cFileExtention
     AND ahsrc.cFullPathName BEGINS cFileBegins
     AND ahsrc.cFullPathName MATCHES pcMatches
     NO-ERROR.



    DO WHILE TRUE:
        IF NOT AVAILABLE ahsrc THEN LEAVE.

        cRelativePath = SUBSTRING(ahsrc.cFullPathName, iRelativePathStart).

        IF iopiFile > piMaxFile
         OR ETIME > piMaxLoadTime
         THEN RETURN.

        iSlashIndex = INDEX(cRelativePath, "/").
        IF iSlashIndex > 0 THEN DO:
            cDiretory = SUBSTRING(cRelativePath, 1, iSlashIndex - 1).

            /* 26-MAY-2008 sla: fix bug with subdirectories appearing more than once in the list */
            FIND FIRST ttFreeList WHERE ttFreeList.cItem = cDiretory + "/" NO-ERROR.
            IF NOT AVAILABLE ttFreeList THEN DO:
                CREATE ttFreeList.
                ASSIGN
                 ttFreeList.cItem       = cDiretory + "/"
                 ttFreeList.cSortOption = pcSort
                 iopiFile               = iopiFile + 1.
            END.

            FIND NEXT ahsrc NO-LOCK WHERE
                 ahsrc.fileExtentions = cFileExtention
             AND ahsrc.cFullPathName > pcDir + "/" + cDiretory + "/____"  /* _ > Z */
             AND ahsrc.cFullPathName MATCHES pcMatches
             NO-ERROR.
            NEXT.
        END.

        /* 26-MAY-2008 sla: fix bug with subdirectories appearing more than once in the list */
        FIND FIRST ttFreeList WHERE ttFreeList.cItem = ahsrc.cFileName NO-ERROR.
        IF NOT AVAILABLE ttFreeList THEN DO:
            CREATE ttFreeList.
            ASSIGN
             ttFreeList.cItem       = ahsrc.cFileName
             ttFreeList.cSortOption = pcSort
             iopiFile               = iopiFile + 1.
            VALIDATE ttFreeList.
        END.

        FIND NEXT ahsrc NO-LOCK WHERE
             ahsrc.fileExtentions = cFileExtention
         AND ahsrc.cFullPathName BEGINS cFileBegins
         AND ahsrc.cFullPathName MATCHES pcMatches
         NO-ERROR.
    END.

END PROCEDURE.

PROCEDURE dumpGlobalDb:
/*------------------------------------------------------------------------------
  Purpose: dump all GLOBAL resource of a source file in one single "pseudo" transaction
   "pseudo?" => Sometimes, I have ot dump a few thousands of records.  I do not want to
   hit the defult value of -L, plus this impacts performance.
   The trick I am going to use to improve performance will be to fetch the master
   record in SHARE-LOCK, so the children record will be updated in seperates small
   and fast transaction, but another user will not be able to fetch the record in
   EXCLUSIVE-LOCK mode.

  Author slacroix   02-SEP-2007
  Parameters:  <none>
  Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER phEditor AS HANDLE      NO-UNDO.

/* limit the scope of these buffers */
DEFINE BUFFER ttEdt           FOR ttEdt          .
DEFINE BUFFER ahsrc           FOR ahsrc          .
{&redefineChildrenBufferScope}

DEFINE BUFFER ttHasBeforeTable FOR tttt.

FIND ttEdt WHERE ttEdt.hEditor = phEditor.

DO FOR ahsrc TRANSACTION:
    FIND ahsrc EXCLUSIVE-LOCK WHERE ahsrc.cFullPathName = ttEdt.cFullPathName NO-ERROR NO-WAIT.
    IF LOCKED ahsrc THEN RETURN "ahsrc master record locked in DB by another user, dump aborted".

    /* create if does not exist */
    IF NOT AVAILABLE ahsrc THEN DO:
        CREATE ahsrc.
        ASSIGN
         ahsrc.srcId         = NEXT-VALUE(srcId)
         ahsrc.cFullPathName = ttEdt.cFullPathName.
    END.
    BUFFER-COPY ttEdt EXCEPT ttEdt.cFullPathName TO ahsrc.
    IF R-INDEX(ttEdt.cFileName, ".") > 0 THEN ahsrc.fileExtentions = SUBSTRING(ttEdt.cFileName, R-INDEX(ttEdt.cFileName, ".") + 1).
END. /* do transaction  */


/* as explained above, the point of this explicit SHARE-LOCK is to lock the record without
starting a transaction
I need to make a DO FOR block otherwise PROGRESS does not let me compile it due to the previous block
 but a transaction should definitely not start here */
DO FOR ahsrc /* no transaction because share-lock */ :
    FIND ahsrc SHARE-LOCK WHERE ahsrc.cFullPathName = ttEdt.cFullPathName. /* at this point, it cannot fail */

    RUN clearSourceMasterRec (BUFFER ahsrc).

    /* 13-JAN-2008 sla: new management of preprocessors */
    FOR EACH ttPreproc WHERE ttPreproc.hEditor = ttEdt.hEditor AND ttPreproc.cFileName = ttEdt.cFileName:
        CREATE ahpreproc.
        BUFFER-COPY ttpreproc TO ahpreproc ASSIGN ahpreproc.srcId = ahsrc.srcId.
    END.


    FOR EACH tttt WHERE tttt.hEditor = ttEdt.hEditor AND tttt.cFileName = ttEdt.cFileName:
        /* 04-SEP-2007 sla: don't dump before tables.  We will recreate them at reload */
        FIND FIRST ttHasBeforeTable WHERE
             ttHasBeforeTable.hEditor = tttt.hEditor
         AND ttHasBeforeTable.cBeforeTable = tttt.cttname
         NO-ERROR.
        IF AVAILABLE ttHasBeforeTable THEN NEXT.

        CREATE ahtt.
        BUFFER-COPY tttt TO ahtt ASSIGN ahtt.srcId = ahsrc.srcId.
        FOR EACH ttfld WHERE ttfld.ittid = tttt.ittid:
            CREATE ahfld.
            BUFFER-COPY ttfld TO ahfld ASSIGN
             ahfld.srcId   = ahsrc.srcId
             ahfld.cttname = ahtt.cttname.
        END.
        FOR EACH ttIdx WHERE ttidx.ittid = tttt.ittid:
            CREATE ahidx.
            BUFFER-COPY ttidx TO ahidx ASSIGN
             ahidx.srcId   = ahsrc.srcId
             ahidx.cttname = ahtt.cttname.
        END.
    END.

    FOR EACH ttFunc WHERE ttFunc.hEditor = ttEdt.hEditor AND ttFunc.cFileName = ttEdt.cFileName:
        CREATE ahFunc.
        BUFFER-COPY ttFunc TO ahFunc ASSIGN ahFunc.srcId = ahsrc.srcId.
    END.

    FOR EACH ttgbuffer WHERE ttgbuffer.hEditor = ttEdt.hEditor AND ttgbuffer.cFileName = ttEdt.cFileName:
        CREATE ahgbuffer.
        BUFFER-COPY ttgbuffer TO ahgbuffer ASSIGN ahgbuffer.srcId = ahsrc.srcId.
    END.
    FOR EACH ttgVar WHERE ttgVar.hEditor = ttEdt.hEditor AND ttgVar.cFileName = ttEdt.cFileName:
        CREATE ahgVar.
        BUFFER-COPY ttgVar TO ahgVar ASSIGN ahgVar.srcId = ahsrc.srcId.
    END.
    FOR EACH ttMark WHERE ttMark.hEditor = ttEdt.hEditor:
        CREATE ahMark.
        BUFFER-COPY ttMark TO ahMark ASSIGN ahMark.srcId = ahsrc.srcId.
    END.
    FOR EACH ttMethod WHERE ttMethod.hEditor = ttEdt.hEditor AND ttMethod.cFileName = ttEdt.cFileName:
        CREATE ahMethod.
        BUFFER-COPY ttMethod TO ahMethod ASSIGN ahMethod.srcId = ahsrc.srcId.
    END.
    FOR EACH ttProc WHERE ttProc.hEditor = ttEdt.hEditor AND ttProc.cFileName = ttEdt.cFileName:
        CREATE ahProc.
        BUFFER-COPY ttProc TO ahProc ASSIGN ahProc.srcId = ahsrc.srcId.
    END.
    FOR EACH ttReferedBuffer WHERE ttReferedBuffer.hEditor = ttEdt.hEditor:
        CREATE ahReferedBuffer.
        BUFFER-COPY ttReferedBuffer TO ahReferedBuffer ASSIGN ahReferedBuffer.srcId = ahsrc.srcId.
    END.
    FOR EACH ttUsing WHERE ttUsing.hEditor = ttEdt.hEditor:
        CREATE ahUsing.
        BUFFER-COPY ttUsing TO ahUsing ASSIGN ahUsing.srcId = ahsrc.srcId.
    END.
    FOR EACH ttgLibHandle WHERE ttgLibHandle.hEditor = ttEdt.hEditor AND ttgLibHandle.cFileName = ttEdt.cFileName:
        CREATE ahgLibHandle.
        BUFFER-COPY ttgLibHandle TO ahgLibHandle ASSIGN ahgLibHandle.srcId = ahsrc.srcId.
    END.

END. /* DO FOR ahsrc:   (no transaction) */
END PROCEDURE.



PROCEDURE cleanUpDeletedFiles:
    DEFINE INPUT  PARAMETER pcRootPath AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER TABLE FOR ttosFile.
    DEFINE INPUT PARAMETER phMonEditor AS HANDLE      NO-UNDO.

    DEFINE BUFFER ttosFile  FOR ttosFile.
    DEFINE BUFFER ahsrc     FOR ahsrc.
    DEFINE BUFFER Lockahsrc FOR ahsrc.

    DEFINE QUERY qOsFile FOR ahsrc, ttosFile.


    /* cannot set FORWARD-ONLY because it was implemented in 9.1D05 and some are still using 9.1D*/

    IF NOT pcRootPath MATCHES "*/" THEN pcRootPath = pcRootPath + "/".

    OPEN QUERY qOsFile FOR EACH ahsrc NO-LOCK WHERE ahsrc.cFullPathName BEGINS pcRootPath
      ,FIRST ttosFile WHERE ttosFile.cFullPathName = ahsrc.cFullPathName OUTER-JOIN.

    REPEAT FOR Lockahsrc:
        GET NEXT qOsFile.
        IF QUERY-OFF-END("qOsFile") THEN LEAVE.
        IF AVAILABLE ttosFile THEN NEXT.

        FIND Lockahsrc SHARE-LOCK WHERE ROWID(Lockahsrc) = ROWID(ahsrc) NO-ERROR NO-WAIT.
        IF LOCKED(Lockahsrc) THEN NEXT. /* ignore it if Locked, this step is not so vital after all */

        IF VALID-HANDLE(phMonEditor)
         AND phMonEditor:TYPE = "editor"
         THEN phMonEditor:INSERT-STRING("~n" + STRING(TIME, "hh:mm:ss") + " Removing this file from abhack.db: " + ahsrc.cFullPathName).
        RUN clearSourceMasterRec (BUFFER ahsrc).
        DELETE Lockahsrc.
    END.

END PROCEDURE.



PROCEDURE clearSourceMasterRec:
/*------------------------------------------------------------------------------
  Purpose: delete all GLOBAL resource connected to a given source file master record
  Author slacroix   03-SEP-2007
  Parameters:  <none>
  Notes:       Perhaps I will find a way later to merge changes without deleting
               and recreating all
------------------------------------------------------------------------------*/
DEFINE PARAMETER BUFFER ahsrc FOR ahsrc.

{&redefineChildrenBufferScope}
FOR EACH ahtt EXCLUSIVE-LOCK  OF ahsrc:
    FOR EACH ahfld EXCLUSIVE-LOCK OF ahtt:
        DELETE ahfld.
    END.
    FOR EACH ahIdx EXCLUSIVE-LOCK OF ahtt:
        DELETE ahIdx.
    END.
    DELETE ahtt.
END.

FOR EACH ahFunc EXCLUSIVE-LOCK OF ahsrc:           DELETE ahFunc.           END.
FOR EACH ahgbuffer EXCLUSIVE-LOCK OF ahsrc:        DELETE ahgbuffer.        END.
FOR EACH ahgVar EXCLUSIVE-LOCK OF ahsrc:           DELETE ahgVar.           END.
FOR EACH ahMark EXCLUSIVE-LOCK OF ahsrc:           DELETE ahMark.           END.
FOR EACH ahMethod EXCLUSIVE-LOCK OF ahsrc:         DELETE ahMethod.         END.
FOR EACH ahProc EXCLUSIVE-LOCK OF ahsrc:           DELETE ahProc.           END.
FOR EACH ahReferedBuffer EXCLUSIVE-LOCK OF ahsrc:  DELETE ahReferedBuffer.  END.
FOR EACH ahUsing EXCLUSIVE-LOCK OF ahsrc:          DELETE ahUsing.          END.
FOR EACH ahgLibHandle EXCLUSIVE-LOCK OF ahsrc:     DELETE ahgLibHandle.     END.
FOR EACH ahpreproc EXCLUSIVE-LOCK OF ahsrc:        DELETE ahpreproc.        END.


END PROCEDURE.



PROCEDURE GetExtProcParam :
    DEFINE INPUT  PARAMETER pcExtProcName AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcParams     AS CHARACTER   NO-UNDO.

    DEFINE BUFFER ahsrc FOR ahsrc.

    DEFINE VARIABLE cFullPathName AS CHARACTER   NO-UNDO.
    /* 16-OCT-2007 sla: search keeps './' in the path instead of solving it with a fullpath
     so I should use the file-info system handle again
    cFullPathName = REPLACE(SEARCH(pcExtProcName), "\", "/"). /* *nix rules */  */
    FILE-INFO:FILE-NAME = pcExtProcName.
    cFullPathName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/"). /* *nix rules */
    IF cFullPathName = ? THEN RETURN ERROR "Can't locate " + QUOTER(pcExtProcName) + " in PROPATH".

    FIND ahsrc NO-LOCK WHERE ahsrc.cFullPathName = cFullPathName NO-ERROR.
    IF NOT AVAILABLE ahsrc THEN RETURN ERROR "The source " + cFullPathName + " is not registered in abhack.db".

    opcParams = ahsrc.cParameters.
END PROCEDURE.



PROCEDURE loadGlobalFromDb:
    DEFINE INPUT        PARAMETER phEditor    AS HANDLE      NO-UNDO.
    DEFINE INPUT        PARAMETER pcOption    AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cUsingList      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iUsingListEntry AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iSuperLevel     AS INTEGER     NO-UNDO.

    /* limit the scope of these buffers */
    DEFINE BUFFER ttEdt           FOR ttEdt          .

    FIND ttEdt WHERE ttEdt.hEditor = phEditor.

    iUsingListEntry = LOOKUP("UsingList", pcOption).
    IF iUsingListEntry > 0 THEN cUsingList = ENTRY(iUsingListEntry + 1, pcOption).

    IF LOOKUP("onlySuperResources", pcOption) = 0 THEN DO:
        RUN loadTargetObject (BUFFER ttEdt, OUTPUT cUsingList) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR errReturnValue().
    END.

    IF ttEdt.cInherits > "" THEN DO:
        RUN loadSuperResources (BUFFER ttEdt
                               ,ttEdt.cInherits
                               ,cUsingList
                               ,INPUT-OUTPUT iSuperLevel
                               )  NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR errReturnValue().
    END.
END PROCEDURE.


PROCEDURE loadTargetObject:
    DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.
    DEFINE OUTPUT PARAMETER opcUsingList AS CHARACTER   NO-UNDO.

    DEFINE BUFFER ahsrc           FOR ahsrc          .
    {&redefineChildrenBufferScope}

    FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = ttEdt.cFullPathName NO-ERROR.
    IF NOT AVAILABLE ahsrc THEN RETURN ERROR "ABHackDB has no source for " + QUOTER(ttEdt.cFullPathName).

    BUFFER-COPY ahsrc EXCEPT cFileName TO ttEdt. /* 14-SEP-2007 sla: do not copy cFileName now
                                                   Note this fields has been adeed to the database on 14-SEP-2007 */
    FOR EACH ahtt NO-LOCK OF ahsrc:
        CREATE tttt.
        BUFFER-COPY ahtt TO tttt ASSIGN
         giLastttid     = giLastttid + 1
         tttt.hEditor   = ttEdt.hEditor
         tttt.cFileName = ttEdt.cFileName
         tttt.ittid     = giLastttid.

        FOR EACH ahfld NO-LOCK OF ahtt:
            CREATE ttfld.
            BUFFER-COPY ahfld TO ttfld ASSIGN ttfld.ittid = tttt.ittid.
        END.
        FOR EACH ahidx NO-LOCK OF ahtt:
            CREATE ttIdx.
            BUFFER-COPY ahidx TO ttIdx ASSIGN ttIdx.ittid = tttt.ittid.
        END.
    END.

    FOR EACH ahpreproc NO-LOCK OF ahsrc:
        CREATE ttPreproc.
        BUFFER-COPY ahpreproc TO ttPreproc ASSIGN
         ttPreproc.hEditor   = ttEdt.hEditor
         ttPreproc.cFileName = ttEdt.cFileName.
    END.

    FOR EACH ahFunc NO-LOCK OF ahsrc:
        CREATE ttFunc.
        BUFFER-COPY ahFunc TO ttFunc ASSIGN
         ttFunc.hEditor   = ttEdt.hEditor
         ttFunc.cFileName = ttEdt.cFileName.
    END.

    FOR EACH ahgbuffer NO-LOCK OF ahsrc:
        CREATE ttgbuffer.
        BUFFER-COPY ahgbuffer TO ttgbuffer ASSIGN
         ttgbuffer.hEditor   = ttEdt.hEditor
         ttgbuffer.cFileName = ttEdt.cFileName.
    END.
    FOR EACH ahgVar NO-LOCK OF ahsrc:
        CREATE ttgVar.
        BUFFER-COPY ahgVar TO ttgVar ASSIGN
         ttgVar.hEditor   = ttEdt.hEditor
         ttgVar.cFileName = ttEdt.cFileName.
    END.
    FOR EACH ahMark NO-LOCK OF ahsrc:
        CREATE ttMark.
        BUFFER-COPY ahMark TO ttMark ASSIGN ttMark.hEditor = ttEdt.hEditor.
    END.
    FOR EACH ahMethod NO-LOCK OF ahsrc:
        CREATE ttMethod.
        BUFFER-COPY ahMethod TO ttMethod ASSIGN
         ttMethod.hEditor   = ttEdt.hEditor
         ttMethod.cFileName = ttEdt.cFileName.
    END.
    FOR EACH ahProc NO-LOCK OF ahsrc:
        CREATE ttProc.
        BUFFER-COPY ahProc TO ttProc ASSIGN
         ttProc.hEditor   = ttEdt.hEditor
         ttProc.cFileName = ttEdt.cFileName.
    END.
    FOR EACH ahReferedBuffer NO-LOCK OF ahsrc:
        CREATE ttReferedBuffer.
        BUFFER-COPY ahReferedBuffer TO ttReferedBuffer ASSIGN
         ttReferedBuffer.hEditor   = ttEdt.hEditor.
    END.
    FOR EACH ahUsing NO-LOCK OF ahsrc:
        CREATE ttUsing.
        BUFFER-COPY ahUsing TO ttUsing ASSIGN ttUsing.hEditor = ttEdt.hEditor.
        opcUsingList = opcUsingList + ";" + ahUsing.cUsing.
    END.
    opcUsingList = SUBSTRING(opcUsingList, 2).

    FOR EACH ahgLibHandle NO-LOCK OF ahsrc:
        CREATE ttgLibHandle.
        BUFFER-COPY ahgLibHandle TO ttgLibHandle ASSIGN
         ttgLibHandle.hEditor   = ttEdt.hEditor
         ttgLibHandle.cFileName = ttEdt.cFileName.
    END.
END PROCEDURE.


PROCEDURE loadSuperResources:
    DEFINE PARAMETER BUFFER ttEdt FOR ttEdt.
    DEFINE INPUT        PARAMETER pcInherits  AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcUsing     AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiLevel   AS INTEGER     NO-UNDO.

    DEFINE VARIABLE cFullFileToLoad   AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cFullPathName     AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPathPart         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRelativeFileName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsing            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMaxUsing         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iUsing            AS INTEGER     NO-UNDO.

    DEFINE BUFFER ahsrc           FOR ahsrc          .
    {&redefineChildrenBufferScope}

    iopiLevel = iopiLevel + 1.
    IF iopiLevel > 100 THEN RETURN ERROR "Reaching level 100 of inheritance... This looks like an infinite loop".

    pcInherits = REPLACE(pcInherits, ".", "/") + ".cls".
    cRelativeFileName = pcInherits. /* as a a start */

    FILE-INFO:FILE-NAME = pcInherits.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
        iMaxUsing = NUM-ENTRIES(pcUsing, ";").
        DO iUsing = 1 TO iMaxUsing:
            ASSIGN
             cUsing              = ENTRY(iUsing, pcUsing, ";")
             cPathPart           = REPLACE(cUsing, ".", "/")
             cPathPart           = TRIM(cPathPart, "/~*")
             cRelativeFileName   = cPathPart + "/" + pcInherits.
             FILE-INFO:FILE-NAME = cRelativeFileName.
            IF FILE-INFO:FULL-PATHNAME <> ? THEN LEAVE.
        END.
    END.

    IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN ERROR "Cannot find file for super class " + QUOTER(pcInherits). /* can't find that super guy... */
    cFullPathName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").

    FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = cFullPathName NO-ERROR.
    IF NOT AVAILABLE ahsrc THEN RETURN ERROR "ABHackDB has no source for super class " + QUOTER(cFullPathName).

    /* prepare using list for the next super class */
    pcUsing = "".
    FOR EACH ahUsing NO-LOCK OF ahsrc:
        pcUsing = pcUsing + ";" + ahUsing.cUsing.
    END.
    pcUsing = SUBSTRING(pcUsing, 2).

    FOR EACH ahgbuffer NO-LOCK OF ahsrc WHERE ahgbuffer.cAccessMode = "PROTECTED":
        CREATE ttgbuffer.
        BUFFER-COPY ahgbuffer TO ttgbuffer ASSIGN
         ttgbuffer.hEditor   = ttEdt.hEditor
         ttgbuffer.cFileName = cRelativeFileName.
    END.
    FOR EACH ahgVar NO-LOCK OF ahsrc WHERE ahgVar.cAccessMode <> "PRIVATE":
        CREATE ttgVar.
        BUFFER-COPY ahgVar TO ttgVar ASSIGN
         ttgVar.hEditor   = ttEdt.hEditor
         ttgVar.cFileName = cRelativeFileName.
    END.
    FOR EACH ahMethod NO-LOCK OF ahsrc WHERE
         ahMethod.cAccessMode <> "PRIVATE"
     AND ahMethod.cName <> "CONSTRUCTOR"
     AND ahMethod.cName <> "DESTRUCTOR":
        CREATE ttMethod.
        BUFFER-COPY ahMethod TO ttMethod ASSIGN
         ttMethod.hEditor   = ttEdt.hEditor
         ttMethod.cFileName = cRelativeFileName.
    END.

    FOR EACH ahtt NO-LOCK OF ahsrc WHERE ahtt.cAccessMode = "PROTECTED":
        CREATE tttt.
        BUFFER-COPY ahtt TO tttt ASSIGN
         giLastttid     = giLastttid + 1
         tttt.ittid     = giLastttid
         tttt.hEditor   = ttEdt.hEditor
         tttt.cFileName = cRelativeFileName.
        FOR EACH ahfld NO-LOCK OF ahtt:
            CREATE ttfld.
            BUFFER-COPY ahfld TO ttfld
             ASSIGN ttfld.ittid = giLastttid.  /* 06-SEP-2007 sla: fixed error 132 and fields sent to the moon instead of to the TARGET temp-table */
        END.
        FOR EACH ahidx NO-LOCK OF ahtt:
            CREATE ttIdx.
            BUFFER-COPY ahidx TO ttIdx
             ASSIGN ttIdx.ittid = giLastttid.  /* 06-SEP-2007 sla: fixed error 132 and index sent to the moon instead of to the TARGET temp-table */
        END.
    END.

    IF ahsrc.cInherits > "" THEN RUN loadSuperResources (BUFFER ttEdt
                                                        ,ahsrc.cInherits
                                                        ,pcUsing
                                                        ,INPUT-OUTPUT iopiLevel).
END PROCEDURE.


PROCEDURE loadLibFuncFromDb:
    DEFINE INPUT PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcOption   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

    DEFINE BUFFER ahsrc  FOR ahsrc.
    DEFINE BUFFER ahFunc FOR ahFunc.

    FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = pcLoadFile NO-ERROR.
    IF NOT AVAILABLE ahsrc THEN RETURN ERROR "Nothing in catalog for " + QUOTER(pcLoadFile).

    FOR EACH ahFunc NO-LOCK OF ahsrc WHERE ahFunc.cAccessMode <> "PRIVATE": /* 04-SEP-2007 sla: did not find a way to use the idAccess index */
        CREATE ttFreeList.
        ASSIGN
         ttFreeList.cItem     = ahFunc.cName
         ttFreeList.cObjTypes = ahFunc.cParameters.
    END.
END PROCEDURE.


PROCEDURE loadLibProcFromDb:
    DEFINE INPUT PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcOption   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

    DEFINE BUFFER ahsrc  FOR ahsrc.
    DEFINE BUFFER ahProc FOR ahProc.

    FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = pcLoadFile NO-ERROR.
    IF NOT AVAILABLE ahsrc THEN RETURN ERROR "Nothing in catalog for " + QUOTER(pcLoadFile).

    FOR EACH ahProc NO-LOCK OF ahsrc WHERE ahProc.cAccessMode <> "PRIVATE": /* 04-SEP-2007 sla: did not find a way to use the idAccess index */
        CREATE ttFreeList.
        ASSIGN
         ttFreeList.cItem     = ahProc.cName
         ttFreeList.cObjTypes = ahProc.cParameters.
    END.
END PROCEDURE.



PROCEDURE loadProgressLangClass :
    /*------------------------------------------------------------------------------
      Purpose: support completion for Progress.Lang.* objects
      Author slacroix   15-OCT-2008
      Parameters:  <none>
      Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT        PARAMETER pcLoadFile  AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcOption    AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT       PARAMETER opcInherits AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList. /* this entry is recursive, but no need to put the APPEND keyword since the temp-table is shared => use INPUT-OUTPUT instead */

    DEFINE VARIABLE cMembers AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE eMember  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMember  AS INTEGER     NO-UNDO.

    DEFINE BUFFER ttFreeList FOR ttFreeList.
    DEFINE BUFFER ttProgressLangClass FOR ttProgressLangClass.
    DEFINE BUFFER ttProgressLangMember FOR ttProgressLangMember.

    FIND FIRST ttProgressLangClass WHERE ttProgressLangClass.cType = pcLoadFile NO-ERROR.
    IF NOT AVAILABLE ttProgressLangClass THEN RETURN.
    opcInherits = ttProgressLangClass.cInherits.

    FOR EACH ttProgressLangMember WHERE ttProgressLangMember.cType = ttProgressLangClass.cType:
        FIND FIRST ttFreeList WHERE ttFreeList.cItem = ttProgressLangMember.cMember NO-ERROR.
        IF AVAILABLE ttFreeList THEN NEXT.
        CREATE ttFreeList.
        ttFreeList.cItem = ttProgressLangMember.cMember.
    END.
END PROCEDURE.


PROCEDURE loadPropMethFromDb:
    DEFINE INPUT PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT PARAMETER pcOption   AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ttFreeList.  /* this entry is recursive, but no need to put the APPEND keyword since the temp-table is shared => use INPUT-OUTPUT instead */

    DEFINE VARIABLE cInherits         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cInheritsFullName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPathPart         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsing            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsingList        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMaxUsing         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iUsing            AS INTEGER     NO-UNDO.
    DEFINE VARIABLE lStaticMembersOnly AS LOGICAL    NO-UNDO.  /* 14-OCT-2009 sla */

    DEFINE BUFFER ahsrc    FOR ahsrc.
    DEFINE BUFFER ahMethod FOR ahMethod.
    DEFINE BUFFER ahUsing  FOR ahUsing.
    DEFINE BUFFER ahgvar   FOR ahgvar.

    lStaticMembersOnly = CAN-DO(pcOption, "StaticMembersOnly").

    /* 15-OCT-2008 sla: support of Progress.Lang.* types */
    /* IF pcLoadFile BEGINS "Progress.Lang." THEN DO: */

    /* 03-NOV-2008 sla: give ability to handle other embedded classes in ProgressLangDefs.xml */
    IF CAN-FIND(FIRST ttProgressLangClass WHERE ttProgressLangClass.cType = pcLoadFile)
     AND NOT lStaticMembersOnly /* 14-OCT-2009 sla: do not add these guys for classes referred statically */
     THEN DO:
        RUN loadProgressLangClass (pcLoadFile
                                  ,pcOption
                                  ,OUTPUT cInheritsFullName
                                  ,INPUT-OUTPUT TABLE ttFreeList).
        IF cInheritsFullName = "" THEN RETURN.
    END.
    ELSE DO:
        FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = pcLoadFile NO-ERROR.
        IF NOT AVAILABLE ahsrc THEN RETURN ERROR "Nothing in catalog for " + QUOTER(pcLoadFile).

        FOR EACH ahgvar NO-LOCK OF ahsrc WHERE ahgvar.cAccessMode = "PUBLIC":
            IF lStaticMembersOnly AND ahgvar.isStatic = NO THEN NEXT. /* 14-OCT-2009 sla */

            FIND FIRST ttFreeList WHERE ttFreeList.cItem = ahgvar.cVar NO-ERROR.
            IF AVAILABLE ttFreeList THEN NEXT.
            CREATE ttFreeList.
            ttFreeList.cItem = ahgvar.cVar.
        END.

        FOR EACH ahMethod NO-LOCK OF ahsrc WHERE ahMethod.cAccessMode = "PUBLIC":
            IF CAN-DO("CONSTRUCTOR,DESTRUCTOR", ahMethod.cName) THEN NEXT.

            IF lStaticMembersOnly AND ahMethod.isStatic = NO THEN NEXT. /* 14-OCT-2009 sla */

            FIND FIRST ttFreeList WHERE ttFreeList.cItem = ahMethod.cName + "()" NO-ERROR.
            IF AVAILABLE ttFreeList THEN NEXT.
            CREATE ttFreeList.
            ASSIGN
             ttFreeList.cItem     = ahMethod.cName + "()"
             ttFreeList.cObjTypes = ahMethod.cParameters.
        END.

        /* 20-JAN-2009 sla: make all class inherit from Progress.Lang.Object  */
        /* IF ahsrc.cInherits = "" THEN RETURN. */
        IF ahsrc.cInherits = "" THEN DO:
            RUN loadPropMethFromDb ("Progress.Lang.Object"
                                   ,pcOption
                                   ,INPUT-OUTPUT TABLE ttFreeList).
            RETURN.
        END.


        cInherits = ahsrc.cInherits.
        FOR EACH ahUsing NO-LOCK OF ahsrc:
            cUsingList = cUsingList + ";" + ahUsing.cUsing. /* 23-OCT-2007 sla: serious issue, I was using ',' as separator here, and ';' bellow, resulting in uresolved class names with multiple usages of USING in a class. */
        END.
        cUsingList = SUBSTRING(cUsingList, 2).

        IF cInherits BEGINS "Progress.Lang." THEN cInheritsFullName = cInherits.
        ELSE DO:
            cInherits = REPLACE(cInherits, ".", "/") + ".cls".
            FILE-INFO:FILE-NAME = cInherits.
            IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
                iMaxUsing = NUM-ENTRIES(cUsingList, ";").
                DO iUsing = 1 TO iMaxUsing:
                    ASSIGN
                     cUsing              = ENTRY(iUsing, cUsingList, ";")
                     cPathPart           = REPLACE(cUsing, ".", "/")
                     cPathPart           = TRIM(cPathPart, "/~*")
                     FILE-INFO:FILE-NAME = cPathPart + "/" + cInherits.
                    IF FILE-INFO:FULL-PATHNAME <> ? THEN LEAVE.
                END.
            END.

            IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN ERROR "Cannot find file for super class " + QUOTER(cInherits). /* can't find that super guy... */
            cInheritsFullName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").

            FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = cInheritsFullName NO-ERROR.
            IF NOT AVAILABLE ahsrc THEN RETURN ERROR "ABHackDB has no source for super class " + QUOTER(cInheritsFullName).
        END.  /* NOT Progress.Lang.*    */
    END.
    RUN loadPropMethFromDb (cInheritsFullName
                           ,pcOption
                           ,INPUT-OUTPUT TABLE ttFreeList).
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
    /*------------------------------------------------------------------------------
      Purpose: recursive PROCEDURE to resolve obj chaining type: return the obj type
                of the last object in the chain
      Author slacroix   08-SEP-2008
      Parameters:  <none>
      Notes: this procedure works with getClassMemberTypeFromDb in Order to support the inheritence of objects
      It could probably perform faster if I was doing the queries here against ahgvar and ahMethod,
      but I would have to manage a rather hard double recursivity to implement the object inheritence
      (with another intermediate API similar to loadPropMethFromDb probably)
      I can very probably improve the performance by passing cChildType in the pcoption to
      loadPropMethFromDb with appropriate filtering
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER pcObjChain AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType    AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cMember       AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cParentClass  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cRemaingChain AS CHARACTER   NO-UNDO.

    /* beware about having winblowze drive:path */
    IF SUBSTRING(pcObjChain, 2, 1) = ":" THEN ASSIGN
     cParentClass  = ENTRY(1, pcObjChain, ":") + ":" + ENTRY(2, pcObjChain, ":")
     cMember       = ENTRY(3, pcObjChain, ":")
     cRemaingChain = SUBSTRING(pcObjChain   , INDEX(pcObjChain, ":") + 1)
     cRemaingChain = SUBSTRING(cRemaingChain, INDEX(cRemaingChain, ":") + 1)
     NO-ERROR.
    ELSE ASSIGN
     cParentClass  = ENTRY(1, pcObjChain, ":")
     cMember       = ENTRY(2, pcObjChain, ":")
     cRemaingChain = SUBSTRING(pcObjChain, INDEX(pcObjChain, ":") + 1)
     NO-ERROR.

    IF ERROR-STATUS:ERROR THEN RETURN ERROR "ERROR in resolveSubtypeChain: " + errReturnValue().

    IF cMember MATCHES "*(*)" THEN cMember = ENTRY(1, cMember, "(") + "()". /* 15-SEP-2008 sla: remove wrapped parameters between parentheses */

    RUN getClassMemberTypeFromDb (cParentClass
                                 ,cMember
                                 ,OUTPUT opcType).

    IF     SUBSTRING(cRemaingChain, 2, 1) = ":"
       AND INDEX(SUBSTRING(cRemaingChain, 3), ":") = 0
     OR INDEX(cRemaingChain, ":") = 0
    THEN RETURN.

    IF SUBSTRING(cRemaingChain, 2, 1) = ":" THEN ENTRY(2, cRemaingChain, ":") = ENTRY(2, opcType, ":").
    ELSE ENTRY(1, cRemaingChain, ":") = opcType.
    RUN resolveSubtypeChain (cRemaingChain, OUTPUT opcType).
END PROCEDURE.




PROCEDURE getClassMemberTypeFromDb:
    DEFINE INPUT  PARAMETER pcLoadFile AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcMember   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcType    AS CHARACTER   NO-UNDO.

    DEFINE VARIABLE cInherits         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cInheritsFullName AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPathPart         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsing            AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsingList        AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMaxUsing         AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iUsing            AS INTEGER     NO-UNDO.

    DEFINE BUFFER ahsrc                FOR ahsrc.
    DEFINE BUFFER ahMethod             FOR ahMethod.
    DEFINE BUFFER ahUsing              FOR ahUsing.
    DEFINE BUFFER ahgvar               FOR ahgvar.
    DEFINE BUFFER ttProgressLangClass  FOR ttProgressLangClass.
    DEFINE BUFFER ttProgressLangMember FOR ttProgressLangMember.

    /* 19-OCT-2008 sla: support for Progress.Lang.* */
    /* IF pcLoadFile BEGINS "Progress.Lang." THEN DO: */

    /* 03-NOV-2008 sla: give ability to handle other embedded classes in ProgressLangDefs.xml */
    IF CAN-FIND(FIRST ttProgressLangClass WHERE ttProgressLangClass.cType = pcLoadFile) THEN DO:
        FIND FIRST ttProgressLangClass WHERE ttProgressLangClass.cType = pcLoadFile.
/*         FIND FIRST ttProgressLangClass WHERE ttProgressLangClass.cType = pcLoadFile.  NO-ERROR.                            */
/*         IF NOT AVAILABLE ttProgressLangClass THEN RETURN ERROR "Nothing in ttProgressLangClass for " + QUOTER(pcLoadFile). */

        FIND FIRST ttProgressLangMember WHERE ttProgressLangMember.cType   = ttProgressLangClass.cType
                                          AND ttProgressLangMember.cMember = pcMember NO-ERROR.
        IF AVAILABLE ttProgressLangMember THEN DO:
            opcType = ttProgressLangMember.cReturnType.
            RETURN.
        END.

        IF ttProgressLangClass.cInherits = "" THEN RETURN. /* found nothing, stop here */
        cInheritsFullName = ttProgressLangClass.cInherits.
    END.  /* IF pcLoadFile BEGINS "Progress.Lang." */

    /* not Progress.Lang.* */
    ELSE DO:
        FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = pcLoadFile NO-ERROR.
        IF NOT AVAILABLE ahsrc THEN RETURN ERROR "Nothing in catalog for " + QUOTER(pcLoadFile).

        IF INDEX(pcMember, "(") = 0 THEN DO:
            FIND FIRST ahgvar NO-LOCK OF ahsrc WHERE
                 ahgvar.cAccessMode = "PUBLIC"
             AND ahgvar.cVar = pcMember NO-ERROR.
            IF AVAIL ahgvar THEN DO:
                opcType = referredClassPath(ahsrc.cFullPathName, ahgvar.cDataType).
                RETURN.
            END.
        END.

        ELSE DO:
            FIND FIRST ahMethod NO-LOCK OF ahsrc WHERE
                 ahMethod.cAccessMode = "PUBLIC"
             AND ahMethod.cName = ENTRY(1, pcMember, "(") NO-ERROR.
            IF AVAILABLE ahMethod THEN DO:
                opcType = referredClassPath(ahsrc.cFullPathName, ahMethod.cReturnType).
                RETURN.
            END.
        END.

        /* 20-JAN-2009 sla: now make all classes inherit from Progress.Lang.Object */
        /* IF ahsrc.cInherits = "" THEN RETURN. /* found nothing, stop here */ */

        /*  search in super classes */
        cInherits = ahsrc.cInherits.

        /* 20-JAN-2009 sla: now make all classes inherit from Progress.Lang.Object */
        IF cInherits = "" THEN DO:
            opcType = "Progress.Lang.Object".
            RETURN.
        END.

        FOR EACH ahUsing NO-LOCK OF ahsrc:
            cUsingList = cUsingList + ";" + ahUsing.cUsing. /* 23-OCT-2007 sla: serious issue, I was using ',' as separator here, and ';' bellow, resulting in uresolved class names with multiple usages of USING in a class. */
        END.
        cUsingList = SUBSTRING(cUsingList, 2).

        cInherits = REPLACE(cInherits, ".", "/") + ".cls".
        FILE-INFO:FILE-NAME = cInherits.
        IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
            iMaxUsing = NUM-ENTRIES(cUsingList, ";").
            DO iUsing = 1 TO iMaxUsing:
                ASSIGN
                 cUsing              = ENTRY(iUsing, cUsingList, ";")
                 cPathPart           = REPLACE(cUsing, ".", "/")
                 cPathPart           = TRIM(cPathPart, "/~*")
                 FILE-INFO:FILE-NAME = cPathPart + "/" + cInherits.
                IF FILE-INFO:FULL-PATHNAME <> ? THEN LEAVE.
            END.
        END.

        IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN ERROR "Cannot find file for super class " + QUOTER(cInherits). /* can't find that super guy... */
        cInheritsFullName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").

        FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = cInheritsFullName NO-ERROR.
        IF NOT AVAILABLE ahsrc THEN RETURN ERROR "ABHackDB has no source for super class " + QUOTER(cInheritsFullName).
    END.  /* not Progress.Lang.* */

    RUN getClassMemberTypeFromDb (cInheritsFullName
                                 ,pcMember
                                 ,OUTPUT opcType).
END PROCEDURE.


FUNCTION referredClassPath RETURNS CHARACTER
  (pcInClass      AS CHARACTER
  ,pcReferedClass AS CHARACTER):

    DEFINE BUFFER ahsrc    FOR ahsrc.
    DEFINE BUFFER ahUsing  FOR ahUsing.

    FIND FIRST ahsrc NO-LOCK WHERE ahsrc.cFullPathName = pcInClass NO-ERROR.
    IF NOT AVAILABLE ahsrc THEN RETURN "".

    pcReferedClass = pcReferedClass + ".cls".
    FILE-INFO:FILE-NAME = pcReferedClass.

    IF FILE-INFO:FULL-PATHNAME <> ? THEN RETURN REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").

    FOR EACH ahUsing NO-LOCK OF ahsrc:
        FILE-INFO:FILE-NAME = ahUsing.cUsing + "/" + pcReferedClass.
        IF FILE-INFO:FULL-PATHNAME <> ? THEN RETURN REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").
    END.
END FUNCTION.

&ENDIF

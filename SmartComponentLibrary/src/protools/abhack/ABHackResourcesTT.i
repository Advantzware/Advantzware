/* 21-MAY-2007 sla: global Resource temp-tables are now defined in a seprate include file
 so we can reuse them in other procedures that will play with a few datasets...  */


&GLOBAL-DEFINE abhackXmlSchemaFile GlobResourceDescSchema.xml
&GLOBAL-DEFINE abhackXmlSchemaPath protools/abhack
&SCOPED-DEFINE dummyScopedPreproc to test the parsing of scoped preproc in include files
&GLOBAL-DEFINE BasicProgressTypes CHARACTER,INTEGER,DECIMAL,LOGICAL,DATE,DATETIME,DATETIME-TZ,HANDLE,WIDGET-HANDLE,RAW,MEMPTR,CLOB,BLOB,LONGCHAR,COM-HANDLE,INT64,ROWID,RECID

PROCEDURE dummyProcInInclude:
    /* the point of this dummy is to see how it appears in a compile-listing  file */
END PROCEDURE.


DEFINE {&SHARED} TEMP-TABLE ttEdt NO-UNDO LABEL "ttEdt (to keep track of source-code editor widgets" RCODE-INFORMATION 
 FIELD hEditor             AS HANDLE     
 
 /* these field are subject to be dumped in abhack.db */
 FIELD cFullPathName       AS CHARACTER
 FIELD isClass             AS LOGICAL
 FIELD cInherits           AS CHARACTER
 FIELD dLastLoadGlobalTime AS DECIMAL
 FIELD iCursorLine         AS INTEGER    
 FIELD iNumLines           AS INTEGER   FORMAT ">>>>>9" 
 FIELD cSrcNarative        AS CHARACTER
 
 /* these fields are not subect ot be dumped in abhack.db  */
 FIELD cABHackDumpVersion  AS CHARACTER FORMAT "X(11)" INITIAL "13-OCT-2009" /* update this initial value when a change in the resource structure files makes old xml files obsolet */
 FIELD iLength             AS DECIMAL /* 31-MAY-2007 sla: use dec instead of int to avoid error in 10.1B if the editor is empty */    
 FIELD hWin                AS HANDLE     
 FIELD cIgnoreValueChanged AS CHARACTER   /* 13-FEB-2007 sla: set by dotPressed (then possibly other guys) so the ValueChanged event can be ignored (and reset this flag) */
 FIELD lManageableSections AS LOGICAL     /* YES => source file is AB structured file, the editor manages one section at a time: definition block, main block, trigger, procedure block etc... */
 FIELD hSectionCombo       AS HANDLE     
 FIELD hWidgetName         AS HANDLE      /* widget name for trigger events */
 FIELD hEventCombo         AS HANDLE     
 FIELD currentSection      AS CHARACTER  
 FIELD currentEvent        AS CHARACTER  
 FIELD currentWidget       AS CHARACTER
 FIELD iMarkOffset         AS INTEGER  
 FIELD iEventLine          AS INTEGER    /* = line number within a internal PROCEDURE or function etc...*/
 FIELD cFileName           AS CHARACTER FORMAT "X(35)" LABEL "Source file"
 FIELD dLastSaveOpenTime   AS DECIMAL
 FIELD cLastSaveOpenTime   AS CHARACTER LABEL "Open/saved"
 FIELD cWinTitle           AS CHARACTER /* to compare with hwin:TITLE in order to detect that another source file has been loaded in the current editor window  */
 FIELD cParameters         AS CHARACTER /* 14-SEP-2007 sla: added this field to be able to insert parameters when doing a RUN external.p */
 
 /* indices */
 INDEX hEditor IS PRIMARY hEditor
 INDEX dLastSaveOpenTime dLastSaveOpenTime
 INDEX cFullPathName cFullPathName
 INDEX hWin hWin.

           
           
DEFINE {&SHARED} TEMP-TABLE tttt NO-UNDO LABEL "tttt (temp-table to manage temp-table definitions used in a procedure editor)"
 FIELD hEditor      AS HANDLE     
 FIELD ittid        AS INTEGER    
 FIELD cFileName    AS CHARACTER  
 FIELD cttname      AS CHARACTER  
 FIELD cBeforeTable AS CHARACTER  
 FIELD cAccessMode  AS CHARACTER   HELP "PRIVATE | PROTECTED | PUBLIC"
 FIELD cNarative    AS CHARACTER
 FIELD isStatic     AS LOGICAL
 INDEX hEditorcttname hEditor cttname
 INDEX hEditorcttBefore hEditor cBeforeTable /* 04-SEP-2007 sla: new index to avoid the dump to cat of before tables */
 INDEX ittid ittid
 INDEX cAssessMode cAccessMode
 INDEX isStatic isStatic
 INDEX hEditorcFileName hEditor cFileName.


DEFINE {&SHARED} TEMP-TABLE ttfld NO-UNDO LABEL "ttfld (temp-table to manage fields of temp-tables defined in a procedure editor)"
 FIELD ittid     AS INTEGER
 FIELD cfldname  AS CHARACTER
 FIELD ifldSeq   AS INTEGER
 FIELD cDataType AS CHARACTER  /* 05-FEB-2007 sla: using it now... */
 FIELD cNarative AS CHARACTER
 INDEX ittidName ittid cfldname /* up to me to make sure it is unique */
 INDEX ittidSeq  IS UNIQUE ittid ifldSeq.

DEFINE {&SHARED} TEMP-TABLE ttIdx NO-UNDO LABEL "ttIdx (temp-table to manage indices of temp-tables defined in a procedure editor)"
 FIELD ittid       AS INTEGER
 FIELD cidxname    AS CHARACTER
 FIELD idxSeq      AS INTEGER
 FIELD cFieldsInfo AS CHARACTER  /* as given by hBufer:INDEX-INFORMATION(i) but with '0' replced by 'asc' and '1' by 'desc' : Country,0,PostalCode,0  */
 FIELD coptn       AS CHARACTER   /* PRIMARY, UNIQUE ... */
 FIELD cNarative   AS CHARACTER
 INDEX ittidName ittid cidxname /* up to me to make sure it is unique */
 INDEX ittidSeq  IS UNIQUE ittid idxSeq.


DEFINE {&SHARED} TEMP-TABLE ttMark NO-UNDO LABEL "ttMark (to keep track of line number and file offset for a given )"
 FIELD hEditor       AS HANDLE     
 FIELD iLine         AS INTEGER    
 FIELD iOffset       AS INTEGER     /* file offset */
 FIELD cBlockType    AS CHARACTER  
 FIELD cBlockName    AS CHARACTER  
 FIELD iEditorLength AS INTEGER     /* to track that a local load has been fired */
 /* don't make these indices unique, because we may update the iline and ioffset quite often in such a way that it could lead to temporary conflicts */
 INDEX editorBlockName IS PRIMARY UNIQUE heditor cBlockType cBlockName
 INDEX editorLine hEditor iLine
 INDEX editorOffset hEditor ioffset.
 
DEFINE {&SHARED} TEMP-TABLE ttgVar NO-UNDO LABEL "ttgVar (To keep track of global variable definitions in a procedure editor)"
 FIELD hEditor     AS HANDLE     
 FIELD cFileName   AS CHARACTER  
 FIELD cVar        AS CHARACTER  
 FIELD cDataType   AS CHARACTER  
 FIELD cViewAs     AS CHARACTER
 FIELD cAccessMode AS CHARACTER  HELP "PRIVATE | PROTECTED | PUBLIC"
 FIELD cClassFile  AS CHARACTER HELP "Class file for non basic data types (resolved with USING's when needed)"
 FIELD cNarative   AS CHARACTER
 FIELD isStatic    AS LOGICAL
 INDEX hEditorcVar hEditor cVar /* up to me to avoid duplicates when loading these guys */
 INDEX cAssessMode cAccessMode
 INDEX isStatic isStatic
 INDEX hEditorcFileName hEditor cFileName.

DEFINE {&SHARED} TEMP-TABLE ttgbuffer NO-UNDO LABEL "ttgbuffer (to keep track of defined global buffers)"
 FIELD hEditor      AS HANDLE     
 FIELD cFileName    AS CHARACTER  
 FIELD cName        AS CHARACTER  
 FIELD cfor         AS CHARACTER  
 FIELD lExplicitDef AS LOGICAL     /* 31-JUL-2007 sla: and if the buffer is just refered or defined explicitly */
 FIELD cAccessMode  AS CHARACTER   HELP "PRIVATE | PROTECTED | PUBLIC"
 FIELD isStatic     AS LOGICAL
 FIELD cNarative    AS CHARACTER
 INDEX cName cName
 INDEX hEditorCname hEditor cName
 INDEX cAssessMode cAccessMode
 INDEX isStatic isStatic
 INDEX hEditorcFileName hEditor cFileName.

DEFINE {&SHARED} TEMP-TABLE ttProc NO-UNDO LABEL "ttProc (to keep track of internal PROCEDURE with their signature)"
 FIELD hEditor     AS HANDLE    
 FIELD cFileName   AS CHARACTER  
 FIELD cName       AS CHARACTER
 FIELD cParameters AS CHARACTER
 FIELD cAccessMode AS CHARACTER   HELP "PRIVATE | PROTECTED | PUBLIC"
 FIELD cNarative   AS CHARACTER
 INDEX hEditorCName hEditor cName
 INDEX cAssessMode cAccessMode.

DEFINE {&SHARED} TEMP-TABLE ttFunc NO-UNDO LIKE ttProc LABEL "ttFunc (to keep track of internal functions with their signature)"
 FIELD cReturnType AS CHARACTER.
 
DEFINE {&SHARED} TEMP-TABLE ttMethod NO-UNDO LIKE ttProc LABEL "ttMethod (to keep track of methods with their signature)"
 FIELD isStatic    AS LOGICAL
 FIELD cReturnType AS CHARACTER
 INDEX isStatic isStatic.

/* 20-DEC-2007 sla: slowly starting to keep track of preprocessors */ 
DEFINE {&SHARED} TEMP-TABLE ttPreproc NO-UNDO LABEL "ttPreproc (to keep track of scoped and global preprocessors"
 FIELD hEditor       AS HANDLE     
 FIELD cFileName     AS CHARACTER  
 FIELD iLineNum      AS INTEGER     /* zeroIncludeLevel line number caught in compiled-listing */
 FIELD cName         AS CHARACTER  
 FIELD cType         AS CHARACTER   /* SCOPED GLOBAL UNDEFINE */
 FIELD cValue        AS CHARACTER  
 FIELD iIncludeLevel AS INTEGER    /* 13-JAN-2008 sla: don't know yet if we gonna use it, but prefer to store this info now */
 INDEX heditorLinenum hEditor iLineNum cName /* not unique */
 INDEX hEditorcFileName hEditor cFileName
 INDEX hEditorCNameTypeLineNum hEditor cName cType iLineNum. 

DEFINE {&SHARED} TEMP-TABLE ttReferedBuffer NO-UNDO LABEL "ttReferedBuffer (Buffer reported at the end of a COMPILE LISTING, connect to ttMark)"
 FIELD hEditor     AS HANDLE     
 FIELD cBlockName  AS CHARACTER  
 FIELD cBlockType  AS CHARACTER  
 FIELD cDataBase   AS CHARACTER  
 FIELD cBufferName AS CHARACTER  
 INDEX editorBlockTypeNameDBBuffer IS PRIMARY UNIQUE heditor cBlockType cBlockName cDataBase cBufferName.
     
DEFINE {&SHARED} TEMP-TABLE ttUsing NO-UNDO LABEL "ttUsing (to keep track of USING package patterns)"
 FIELD hEditor    AS HANDLE
 FIELD cUsing     AS CHARACTER
 FIELD cNarative  AS CHARACTER
 INDEX editorUsing hEditor cUsing.

DEFINE {&SHARED} TEMP-TABLE ttgLibHandle NO-UNDO LABEL "ttgLibHandle (to track RUN<someLib> PERSISTENT SET <someGlobalVar>)"
 FIELD hEditor          AS HANDLE     
 FIELD cFileName        AS CHARACTER  
 FIELD iLine            AS INTEGER    
 FIELD cLibFile         AS CHARACTER  
 FIELD cVar             AS CHARACTER  
 FIELD lSuperOfThisProc AS LOGICAL    
 FIELD cNarative        AS CHARACTER
 INDEX hEditorcVar hEditor cVar
 INDEX editorLine hEditor iLine
 INDEX hEditorcFileName hEditor cFileName.



/* 15-AUG-2007 sla: these tables are sometimes need to feed from an external .p, but ar enot dump in catalog */
DEFINE {&SHARED} TEMP-TABLE ttFreeList NO-UNDO LABEL "ttFreeList (to popup a free list of items in procedure editor)"
 FIELD cItem       AS CHARACTER  
 FIELD cObjTypes   AS CHARACTER   /* this field is necessary to make ttFreeList and ttAttr have an identical signature*/
 FIELD cSortOption AS CHARACTER  
 INDEX cItem cItem
 INDEX cSortOptionItem IS PRIMARY cSortOption cItem
 INDEX cObjTypes IS WORD-INDEX cObjTypes.


DEFINE {&SHARED} TEMP-TABLE ttSection NO-UNDO LABEL "ttSection (to keep track of various sections for a given editor widget (ttEdt record)"
 FIELD hEditor      AS HANDLE     
 FIELD cSection     AS CHARACTER   /* these construct with 2 different fields will allow me do handle a procedure or event called 'definition' oàr 'mainblock', mad but possible */
 FIELD cEvent       AS CHARACTER  
 FIELD cWidget      AS CHARACTER   /* widget name for trigger events */
 FIELD cursorLine   AS INTEGER    
 FIELD cursorChar   AS INTEGER    
 FIELD iMruSequence AS INTEGER    
 INDEX hEditorSectionEventWidget IS UNIQUE hEditor cSection cEvent cWidget
 INDEX heditorMruSeq heditor iMruSequence.




DEFINE {&SHARED} VARIABLE gcImportDirName       AS CHARACTER   NO-UNDO. /* GLOBAL var because I do not bother with deleting the file until CLOSE of THIS-PROCEDURE */.
DEFINE {&SHARED} VARIABLE giLastttid            AS INTEGER     NO-UNDO.
DEFINE {&SHARED} VARIABLE gcPID                 AS CHARACTER   NO-UNDO. /* I am going to use this guy as CHARACTER so many times that I prefer to store it once and for all */.


/* variables with view as phrase */
&IF "{&excludeGuiVars}" <> "yes" &THEN
DEFINE {&SHARED} VARIABLE gcDumpedResourceFileRoot AS CHARACTER FORMAT "X(256)":U INITIAL "Type a valid root full path here" 
     LABEL "Dumped resource files root dir" 
     VIEW-AS FILL-IN 
     SIZE 29 BY 1 TOOLTIP "Root directory to dump/load xml abhack global resource files" NO-UNDO.

DEFINE {&SHARED} VARIABLE gcGlobalResCat AS CHARACTER FORMAT "X(256)":U 
     LABEL " Global Resource Catalog" 
     VIEW-AS COMBO-BOX INNER-LINES 5
     LIST-ITEM-PAIRS "disabled","disabled",
                     "XML Files","XML",
                     "Database","DB"
     DROP-DOWN-LIST
     SIZE 30 BY 1 TOOLTIP "THE REAL POWER OF ABHACK... YOU SHOULD DEFINITELY USE IT"
     BGCOLOR 13  NO-UNDO.

DEFINE {&SHARED} VARIABLE glKeepTrackListingBuffer AS LOGICAL INITIAL yes 
     LABEL "Always keep track compile-listing buffer usage && scope" 
     VIEW-AS TOGGLE-BOX
     SIZE 57 BY .81 TOOLTIP "Improves performances for remote databases - Uncheck and Recheck to refresh" NO-UNDO.

DEFINE {&SHARED} VARIABLE glPreprocessors AS LOGICAL INITIAL yes 
     LABEL "Completion for preprocessor names" 
     VIEW-AS TOGGLE-BOX
     SIZE 38 BY .81 TOOLTIP "Beware, the parsing of compile listing will be slightly slower" NO-UNDO.
&ENDIF

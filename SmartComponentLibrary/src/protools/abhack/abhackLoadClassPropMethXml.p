{protools/abhack/ABHackResourcesTT.i  &REFERENCE-ONLY=REFERENCE-ONLY}

DEFINE INPUT PARAMETER pcLoadFile               AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pcDumpedResourceFileRoot AS CHARACTER   NO-UNDO.
DEFINE INPUT PARAMETER pcOption                 AS CHARACTER   NO-UNDO.

DEFINE TEMP-TABLE tdEdt            NO-UNDO LIKE ttEdt          .
DEFINE TEMP-TABLE tdfld            NO-UNDO LIKE ttfld          .
DEFINE TEMP-TABLE tdFunc           NO-UNDO LIKE ttFunc         .
DEFINE TEMP-TABLE tdgbuffer        NO-UNDO LIKE ttgbuffer      .
DEFINE TEMP-TABLE tdgVar           NO-UNDO LIKE ttgVar         .
DEFINE TEMP-TABLE tdIdx            NO-UNDO LIKE ttIdx          .
DEFINE TEMP-TABLE tdMark           NO-UNDO LIKE ttMark         .
DEFINE TEMP-TABLE tdMethod         NO-UNDO LIKE ttMethod       .
DEFINE TEMP-TABLE tdProc           NO-UNDO LIKE ttProc         .
DEFINE TEMP-TABLE tdReferedBuffer  NO-UNDO LIKE ttReferedBuffer.
DEFINE TEMP-TABLE tdtt             NO-UNDO LIKE tttt           .
DEFINE TEMP-TABLE tdUsing          NO-UNDO LIKE ttUsing        .

/* will store property and METHOD in the cItem field, and methods parameters in cObjTypes (usually not used for the ttFreeList temp-table) */
DEFINE OUTPUT PARAMETER TABLE FOR ttFreeList.

DEFINE VARIABLE gcInherits       AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcUsingList      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ghEditor         AS HANDLE      NO-UNDO. /* will be set temporary to a valid handle that won't a handle (this-procedure is a good candidate ;) */
DEFINE VARIABLE giUsingListEntry AS INTEGER     NO-UNDO.
DEFINE VARIABLE lReadOK          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE gcXmlSchemaFile AS CHARACTER    NO-UNDO.

DEFINE DATASET dsGlobRsc
 FOR tdEdt, tdtt, tdfld, tdIdx, tdFunc, tdgbuffer, tdgVar, tdMark, tdMethod, tdProc, tdReferedBuffer, tdUsing
 DATA-RELATION reltdtt            FOR tdEdt, tdtt            RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdfld           FOR tdtt, tdfld            RELATION-FIELDS (ittid, ittid)
 DATA-RELATION reltdIdx           FOR tdtt, tdIdx            RELATION-FIELDS (ittid, ittid)
 DATA-RELATION reltdFunc          FOR tdEdt, tdFunc          RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdgbuffer       FOR tdEdt, tdgbuffer       RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdgVar          FOR tdEdt, tdgVar          RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdMark          FOR tdEdt, tdMark          RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdMethod        FOR tdEdt, tdMethod        RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdProc          FOR tdEdt, tdProc          RELATION-FIELDS (hEditor, hEditor, cFileName, cFileName)
 DATA-RELATION reltdReferedBuffer FOR tdEdt, tdReferedBuffer RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdUsing         FOR tdEdt, tdUsing         RELATION-FIELDS (hEditor, hEditor).

ghEditor = THIS-PROCEDURE.

gcXmlSchemaFile = SEARCH("{&abhackXmlSchemaPath}/{&abhackXmlSchemaFile}").
IF gcXmlSchemaFile = ? THEN RETURN ERROR "No valid xml schema file in {&abhackXmlSchemaPath}/{&abhackXmlSchemaFile}".

RUN loadXmlDef (pcLoadFile, OUTPUT gcInherits, OUTPUT gcUsingList) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.

IF gcInherits > "" THEN DO:
    RUN loadSuperResources (gcInherits, gcUsingList) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
END.
                                                                     
PROCEDURE loadSuperResources:
    DEFINE INPUT  PARAMETER pcInherits AS CHARACTER   NO-UNDO.
    DEFINE INPUT  PARAMETER pcUsing AS CHARACTER   NO-UNDO.
    
    DEFINE VARIABLE cFullPathName  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPathPart      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsing         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cXmlFileToLoad AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMaxUsing      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iUsing         AS INTEGER     NO-UNDO.
    
    pcInherits = REPLACE(pcInherits, ".", "/") + ".cls".
    
    FILE-INFO:FILE-NAME = pcInherits.
    IF FILE-INFO:FULL-PATHNAME = ? THEN DO:
        iMaxUsing = NUM-ENTRIES(pcUsing, ";").
        DO iUsing = 1 TO iMaxUsing:
            ASSIGN
             cUsing              = ENTRY(iUsing, pcUsing, ";")
             cPathPart           = REPLACE(cUsing, ".", "/")
             cPathPart           = TRIM(cPathPart, "/*")
             FILE-INFO:FILE-NAME = cPathPart + "/" + pcInherits.
            IF FILE-INFO:FULL-PATHNAME <> ? THEN LEAVE.
        END.
    END.
    
    IF FILE-INFO:FULL-PATHNAME = ? THEN RETURN ERROR "Cannot find xml file for super class: " + QUOTER(pcInherits). /* can't find that super guy... */
    
    cFullPathName = REPLACE(FILE-INFO:FULL-PATHNAME, "\", "/").
    cXmlFileToLoad = pcDumpedResourceFileRoot + "/" + REPLACE(cFullPathName, ":", "Drive") + ".abhack".


    RUN loadXmlDef (cXmlFileToLoad, OUTPUT pcInherits, OUTPUT pcUsing) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
    
    IF pcInherits > "" THEN DO:
        RUN loadSuperResources (pcInherits, pcUsing) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.
    END.
END PROCEDURE.

    
PROCEDURE loadXmlDef:    
    DEFINE INPUT  PARAMETER pcLoadFile   AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInherits  AS CHARACTER   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUsingList AS CHARACTER   NO-UNDO.
    
    DEFINE BUFFER tdEdt    FOR tdEdt.
    DEFINE BUFFER tdMethod FOR tdMethod.
    DEFINE BUFFER tdgVar   FOR tdgVar.
    
    lReadOK = DATASET dsGlobRsc:READ-XML("file"
                                        ,pcLoadFile
                                        ,"EMPTY"  /* readMode */
                                        ,gcXmlSchemaFile   /*schema-location*/
                                        ,NO) /* override-default-mapping*/
                                       NO-ERROR.
    IF lReadOK = NO THEN RETURN ERROR ERROR-STATUS:GET-MESSAGE(1).
    
    /* sophisticated buffer-copy FOR this one.  In short, keep some system fields with values set by abhackwin 
     I may rearchitect this later ... */
    FIND tdEdt NO-ERROR.
    IF NOT AVAILABLE tdEdt THEN RETURN ERROR "No valid record in super .abhack xml".
    IF tdEdt.isClass = NO THEN RETURN ERROR "This object is not a class".

    FOR EACH tdgVar WHERE tdgVar.cAccessMode = "PUBLIC":
        CREATE ttFreeList.
        ttFreeList.cItem = tdgVar.cVar.
    END.
    
    FOR EACH tdMethod WHERE tdMethod.cAccessMode = "PUBLIC":
        IF CAN-DO("CONSTRUCTOR,DESTRUCTOR", tdMethod.cName) THEN NEXT.
        CREATE ttFreeList.
        ASSIGN
         ttFreeList.cItem     = tdMethod.cName + "()"
         ttFreeList.cObjTypes = tdMethod.cParameters.
    END.

    FOR EACH tdUsing:
        opcUsingList = opcUsingList + ";" + tdUsing.cUsing. 
    END.
    opcUsingList = SUBSTRING(opcUsingList, 2).
    
    opcInherits = tdEdt.cInherits.
    
    
    /* perhaps I will mantain another smaller xml file for classes so I will not loose time with unecessary info */
    DATASET dsGlobRsc:EMPTY-DATASET().
END PROCEDURE.
                                            


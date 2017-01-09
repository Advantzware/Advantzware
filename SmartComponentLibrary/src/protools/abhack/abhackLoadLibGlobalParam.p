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

DEFINE OUTPUT PARAMETER opcParams AS CHARACTER   NO-UNDO.

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

RUN loadXmlDef (pcLoadFile) NO-ERROR.
IF ERROR-STATUS:ERROR THEN RETURN ERROR RETURN-VALUE.



PROCEDURE loadXmlDef:    
    DEFINE INPUT  PARAMETER pcLoadFile   AS CHARACTER   NO-UNDO.
    
    DEFINE BUFFER tdEdt    FOR tdEdt.
    
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

    opcParams = tdEdt.cParameters.

    /* perhaps I will mantain another smaller xml file for classes so I will not loose time with unecessary info */
    DATASET dsGlobRsc:EMPTY-DATASET().
END PROCEDURE.
                                            


/*------------------------------------------------------------------------
    File        : loadApiData.p
    Purpose     : Used to load API data from development into a connected DB
    Syntax      :
    Description : User must specify a source directory for .d and blb files
                  Can optionally specify whether to "clean" files prior to load
                  If not "cleaned" program will just add new records
    Author(s)   : MYT 
    Created     : Mon Feb 24 16:11:06 EST 2020
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF VAR cInputDir AS CHAR NO-UNDO.
DEF VAR cFileList AS CHAR NO-UNDO.
DEF VAR cDumpFileList AS CHAR NO-UNDO.
DEF VAR cFile AS CHAR NO-UNDO.
DEF VAR cDumpFile AS CHAR NO-UNDO.
DEF VAR cType AS CHAR NO-UNDO.
DEF VAR cLongFile AS CHAR NO-UNDO.
DEF VAR lEmptyFirst AS LOG NO-UNDO.

DEF TEMP-TABLE ttFiles
    FIELD cFileName AS CHAR 
    FIELD cDumpFile AS CHAR 
    FIELD cType AS CHAR 
    FIELD cLongFile AS CHAR.
    
DEF TEMP-TABLE ttApiInbound NO-UNDO LIKE apiInbound.
DEF TEMP-TABLE ttApiInboundDetail NO-UNDO LIKE apiInboundDetail.
DEF TEMP-TABLE ttApiOutbound NO-UNDO LIKE apiOutbound.
DEF TEMP-TABLE ttApiOutboundDetail NO-UNDO LIKE apiOutboundDetail.
DEF TEMP-TABLE ttApiOutboundTrigger NO-UNDO LIKE apiOutboundTrigger.
    

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
UPDATE 
    cInputDir FORMAT "x(65)" LABEL "  Input Directory" SKIP 
    lEmptyFirst LABEL "  Delete existing records before load?" FORMAT "Y/N"
    WITH FRAME a SIDE-LABELS WIDTH 90 VIEW-AS DIALOG-BOX THREE-D TITLE "API Load Parameters".
    
IF cInputDir EQ "" THEN RETURN.

FOR EACH _file NO-LOCK WHERE
    _file._file-name BEGINS "API":
    ASSIGN 
        cFileList = cFileList + _file._file-name + ","
        cDumpFileList = cDumpFileList + _file._dump-name + ",".
END.
ASSIGN 
    cFileList = TRIM(cFileList,",")
    cDumpFileList = TRIM(cDumpFileList,",").

INPUT FROM OS-DIR (cInputDir).
REPEAT:
    IMPORT 
        cDumpFile
        cLongFile
        cTypea.
    IF LENGTH(cDumpFile) LT 3 THEN NEXT.
    IF SUBSTRING(cDumpFile, LENGTH(cDumpFile) - 1, 2) EQ ".d" THEN DO: 
        CREATE ttFiles.
        ASSIGN 
            ttFiles.cDumpFile = cDumpFile
            ttFiles.cType = cType
            ttFiles.cLongFile = cLongFile
            ttFiles.cFileName = ENTRY(LOOKUP(SUBSTRING(cDumpFile, 1, LENGTH(cDumpFile) - 2),cDumpFileList),cFileList).
        DISP ttfiles.
    END.
END.

FOR EACH ttFiles:
    CASE ttFiles.cFileName:
        WHEN "APIInbound" THEN 
            DO:
            &Scoped-define ccFile APIInbound
                IF lEmptyFirst THEN 
                DO:
                    FOR EACH {&ccFile} EXCLUSIVE:
                        DELETE {&ccFile}.
                    END.
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE {&ccFile}.
                        IMPORT {&ccFile}.
                    END.
                    INPUT CLOSE.
                END.
                ELSE DO:
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE tt{&ccFile}.
                        IMPORT tt{&ccFile}.
                    END.
                    INPUT CLOSE.
                    FOR EACH tt{&ccFile}:
                        FIND {&ccFile} NO-LOCK WHERE
                            {&ccFile}.{&ccFile}ID EQ tt{&ccFile}.{&ccFile}ID
                            NO-ERROR.
                        IF NOT AVAIL {&ccFile} THEN DO:
                            CREATE {&ccFile}.
                            BUFFER-COPY tt{&ccFile} TO {&ccFile}.
                        END.  
                    END.                            
                END.
                FIND LAST {&ccFile} NO-LOCK NO-ERROR.
                IF AVAIL {&ccFile} THEN ASSIGN 
                    CURRENT-VALUE ({&ccFile}ID_seq) = {&ccFile}.{&ccFile}ID.   
            END.
        WHEN "APIInboundDetail" THEN 
            DO:
            &Scoped-define ccFile APIInboundDetail
                IF lEmptyFirst THEN 
                DO:
                    FOR EACH {&ccFile} EXCLUSIVE:
                        DELETE {&ccFile}.
                    END.
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE {&ccFile}.
                        IMPORT {&ccFile}.
                    END.
                    INPUT CLOSE.
                END.
                ELSE 
                DO:
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE tt{&ccFile}.
                        IMPORT tt{&ccFile}.
                    END.
                    INPUT CLOSE.
                    FOR EACH tt{&ccFile}:
                        FIND {&ccFile} NO-LOCK WHERE
                            {&ccFile}.{&ccFile}ID EQ tt{&ccFile}.{&ccFile}ID
                            NO-ERROR.
                        IF NOT AVAIL {&ccFile} THEN 
                        DO:
                            CREATE {&ccFile}.
                            BUFFER-COPY tt{&ccFile} TO {&ccFile}.
                        END.  
                    END.                            
                END.
                FIND LAST {&ccFile} NO-LOCK NO-ERROR.
                IF AVAIL {&ccFile} THEN ASSIGN 
                        CURRENT-VALUE ({&ccFile}ID_seq) = {&ccFile}.{&ccFile}ID.   
            END.
        WHEN "APIOutbound" THEN 
            DO:
            &Scoped-define ccFile APIOutbound
                IF lEmptyFirst THEN 
                DO:
                    FOR EACH {&ccFile} EXCLUSIVE:
                        DELETE {&ccFile}.
                    END.
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE {&ccFile}.
                        IMPORT {&ccFile}.
                    END.
                    INPUT CLOSE.
                END.
                ELSE 
                DO:
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE tt{&ccFile}.
                        IMPORT tt{&ccFile}.
                    END.
                    INPUT CLOSE.
                    FOR EACH tt{&ccFile}:
                        FIND {&ccFile} NO-LOCK WHERE
                            {&ccFile}.{&ccFile}ID EQ tt{&ccFile}.{&ccFile}ID
                            NO-ERROR.
                        IF NOT AVAIL {&ccFile} THEN 
                        DO:
                            CREATE {&ccFile}.
                            BUFFER-COPY tt{&ccFile} TO {&ccFile}.
                        END.  
                    END.                            
                END.
                FIND LAST {&ccFile} NO-LOCK NO-ERROR.
                IF AVAIL {&ccFile} THEN ASSIGN 
                        CURRENT-VALUE ({&ccFile}ID_seq) = {&ccFile}.{&ccFile}ID.   
            END.
        WHEN "APIOutboundDetail" THEN 
            DO:
            &Scoped-define ccFile APIOutboundDetail
                IF lEmptyFirst THEN 
                DO:
                    FOR EACH {&ccFile} EXCLUSIVE:
                        DELETE {&ccFile}.
                    END.
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE {&ccFile}.
                        IMPORT {&ccFile}.
                    END.
                    INPUT CLOSE.
                END.
                ELSE 
                DO:
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE tt{&ccFile}.
                        IMPORT tt{&ccFile}.
                    END.
                    INPUT CLOSE.
                    FOR EACH tt{&ccFile}:
                        FIND {&ccFile} NO-LOCK WHERE
                            {&ccFile}.{&ccFile}ID EQ tt{&ccFile}.{&ccFile}ID
                            NO-ERROR.
                        IF NOT AVAIL {&ccFile} THEN 
                        DO:
                            CREATE {&ccFile}.
                            BUFFER-COPY tt{&ccFile} TO {&ccFile}.
                        END.  
                    END.                            
                END.
                FIND LAST {&ccFile} NO-LOCK NO-ERROR.
                IF AVAIL {&ccFile} THEN ASSIGN 
                        CURRENT-VALUE ({&ccFile}ID_seq) = {&ccFile}.{&ccFile}ID.   
            END.
        WHEN "APIOutboundTrigger" THEN 
            DO:
            &Scoped-define ccFile APIOutboundTrigger
                IF lEmptyFirst THEN 
                DO:
                    FOR EACH {&ccFile} EXCLUSIVE:
                        DELETE {&ccFile}.
                    END.
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE {&ccFile}.
                        IMPORT {&ccFile}.
                    END.
                    INPUT CLOSE.
                END.
                ELSE 
                DO:
                    INPUT FROM VALUE(ttFiles.cLongFile).
                    REPEAT:
                        CREATE tt{&ccFile}.
                        IMPORT tt{&ccFile}.
                    END.
                    INPUT CLOSE.
                    FOR EACH tt{&ccFile}:
                        FIND {&ccFile} NO-LOCK WHERE
                            {&ccFile}.{&ccFile}ID EQ tt{&ccFile}.{&ccFile}ID
                            NO-ERROR.
                        IF NOT AVAIL {&ccFile} THEN 
                        DO:
                            CREATE {&ccFile}.
                            BUFFER-COPY tt{&ccFile} TO {&ccFile}.
                        END.  
                    END.                            
                END.
                FIND LAST {&ccFile} NO-LOCK NO-ERROR.
                IF AVAIL {&ccFile} THEN ASSIGN 
                        CURRENT-VALUE ({&ccFile}ID_seq) = {&ccFile}.{&ccFile}ID.   
            END.
    END.
END.



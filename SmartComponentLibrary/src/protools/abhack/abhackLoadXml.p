{protools/abhack/ABHackResourcesTT.i  &SHARED=SHARED}

DEFINE INPUT        PARAMETER pcOption                 AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER pcLoadFile               AS CHARACTER   NO-UNDO.
DEFINE INPUT        PARAMETER phbttedt                 AS HANDLE      NO-UNDO.
DEFINE INPUT        PARAMETER pcDumpedResourceFileRoot AS CHARACTER   NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER iopittidmax              AS INTEGER     NO-UNDO.

/* temp-table to be managed by a dataset.  They will often be passed as OUTPUT APPEND to the temp-tables
 of abhackwin.w.
 I prefer to not use the original temp-tables of ABHackResourcesTT.i because those are sometimes
 passed as reference to manipulate them directly, plus the td* TT allow we to refined the loaded info before
 adding them to the target temp-tables of abhackwin.w */
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
DEFINE TEMP-TABLE tdgLibHandle     NO-UNDO LIKE ttgLibHandle   .
DEFINE TEMP-TABLE tdPreproc        NO-UNDO LIKE ttPreproc      .

DEFINE VARIABLE gcUsingList      AS CHARACTER   NO-UNDO.
DEFINE VARIABLE gcXmlSchemaFile  AS CHARACTER   NO-UNDO.
DEFINE VARIABLE ghEditor         AS HANDLE      NO-UNDO.
DEFINE VARIABLE giUsingListEntry AS INTEGER     NO-UNDO.
DEFINE VARIABLE lReadOK          AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iSuperLevel      AS INTEGER     NO-UNDO.

DEFINE DATASET dsGlobRsc
 FOR tdEdt, tdtt, tdfld, tdIdx, tdFunc, tdgbuffer, tdgVar, tdMark, tdMethod, tdProc, tdReferedBuffer, tdUsing, tdgLibHandle, tdPreproc
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
 DATA-RELATION reltdUsing         FOR tdEdt, tdUsing         RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdglibhandle    FOR tdEdt, tdgLibHandle    RELATION-FIELDS (hEditor, hEditor)
 DATA-RELATION reltdtdPreproc     FOR tdEdt, tdPreproc       RELATION-FIELDS (hEditor, hEditor).

FUNCTION ProgressErrors RETURNS CHARACTER( ) FORWARD.    
FUNCTION errReturnValue RETURNS CHARACTER( ) FORWARD.    

ghEditor = phbttedt::hEditor.

giUsingListEntry = LOOKUP("UsingList", pcOption).
IF giUsingListEntry > 0 THEN gcUsingList = ENTRY(giUsingListEntry + 1, pcOption).

gcXmlSchemaFile = SEARCH("{&abhackXmlSchemaPath}/{&abhackXmlSchemaFile}").
IF gcXmlSchemaFile = ? THEN RETURN ERROR "No valid xml schema file in {&abhackXmlSchemaPath}/{&abhackXmlSchemaFile}".


IF LOOKUP("onlySuperResources", pcOption) = 0 THEN DO:
    RUN loadTargetObject NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR errReturnValue().
    
    RUN refineTargetObjectRecords NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR errReturnValue().
    
    gcUsingList = "".
    FOR EACH tdUsing:
        gcUsingList = gcUsingList + ";" + tdUsing.cUsing. 
    END.
    gcUsingList = SUBSTR(gcUsingList, 2).
END.



IF phbttedt::cInherits > "" THEN DO:
    RUN loadSuperResources (phbttedt::cInherits, gcUsingList, INPUT-OUTPUT iSuperLevel) NO-ERROR.
    IF ERROR-STATUS:ERROR THEN RETURN ERROR errReturnValue().
END.

RUN addLoadedDataToRealTT.
                                   
PROCEDURE addLoadedDataToRealTT:
    /* don't do this one
     FOR EACH tdEdt           :    CREATE ttEdt          . BUFFER-COPY tdEdt            TO ttEdt          . END. */
     
    FOR EACH tdfld           :    CREATE ttfld          . BUFFER-COPY tdfld            TO ttfld          . END.
    FOR EACH tdFunc          :    CREATE ttFunc         . BUFFER-COPY tdFunc           TO ttFunc         . END.
    FOR EACH tdgbuffer       :    CREATE ttgbuffer      . BUFFER-COPY tdgbuffer        TO ttgbuffer      . END.
    FOR EACH tdgVar          :    CREATE ttgVar         . BUFFER-COPY tdgVar           TO ttgVar         . END.
    FOR EACH tdIdx           :    CREATE ttIdx          . BUFFER-COPY tdIdx            TO ttIdx          . END.
    FOR EACH tdMark          :    CREATE ttMark         . BUFFER-COPY tdMark           TO ttMark         . END.
    FOR EACH tdMethod        :    CREATE ttMethod       . BUFFER-COPY tdMethod         TO ttMethod       . END.
    FOR EACH tdProc          :    CREATE ttProc         . BUFFER-COPY tdProc           TO ttProc         . END.
    FOR EACH tdReferedBuffer :    CREATE ttReferedBuffer. BUFFER-COPY tdReferedBuffer  TO ttReferedBuffer. END.
    FOR EACH tdtt            :    CREATE tttt           . BUFFER-COPY tdtt             TO tttt           . END.
    FOR EACH tdUsing         :    CREATE ttUsing        . BUFFER-COPY tdUsing          TO ttUsing        . END.
    FOR EACH tdgLibHandle    :    CREATE ttgLibHandle   . BUFFER-COPY tdgLibHandle     TO ttgLibHandle   . END.
    FOR EACH tdPreproc       :    CREATE ttPreproc      . BUFFER-COPY tdPreproc        TO ttPreproc      . END.
END PROCEDURE.

                                                                     
                                                                     
PROCEDURE loadSuperResources:
    DEFINE INPUT        PARAMETER pcInherits     AS CHARACTER   NO-UNDO.
    DEFINE INPUT        PARAMETER pcUsing        AS CHARACTER   NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiSuperLEvel AS INTEGER     NO-UNDO.
    
    DEFINE VARIABLE cFullPathName  AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cPathPart      AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cUsing         AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE cXmlFileToLoad AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMaxUsing      AS INTEGER     NO-UNDO.
    DEFINE VARIABLE iUsing         AS INTEGER     NO-UNDO.
    
                                                         
    iopiSuperLEvel = iopiSuperLEvel + 1.
    IF iopiSuperLEvel > 100 THEN RETURN ERROR "Reaching level 100 of inheritance... This looks like an infinite loop".
   
                                                         
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

    /* at this point, do not keep any record in tdEdt */
    EMPTY TEMP-TABLE tdEdt. /* I want only one record for the last loaded object in this TT after the Dataset load*/
    
    lReadOK = DATASET dsGlobRsc:READ-XML("file"
                                        ,cXmlFileToLoad
                                        ,"APPEND"  /* readMode */
                                        ,gcXmlSchemaFile   /*schema-location*/
                                        ,NO) /* override-default-mapping*/
                                        NO-ERROR.
    IF lReadOK = NO THEN RETURN ERROR ProgressErrors().
    
    FIND tdEdt NO-ERROR.
    IF NOT AVAILABLE tdEdt THEN RETURN ERROR "No valid object record in super .abhack xml".

    /* remove info that have nothing to do in the TARGET' definitions */
    FOR EACH tdMark WHERE tdMark.hEditor = ?:
        DELETE tdMark.
    END.
    
    FOR EACH tdReferedBuffer WHERE tdReferedBuffer.hEditor = ?:
        DELETE tdReferedBuffer.
    END.
    
    /* prepare the using list for the next SUPER class */
    gcUsingList = "".
    FOR EACH tdUsing WHERE tdUsing.hEditor = ?:
        gcUsingList = gcUsingList + ";" + tdUsing.cUsing. 
        DELETE tdUsing. /* and do not keep the tdUsing of SUPER classes here */
    END.
    gcUsingList = SUBSTR(gcUsingList, 2).
    
    /* Now REPLACE the dumped handle by the one passed as input 
      and remove super PRIVATE resources */
    FOR EACH tdgbuffer WHERE tdgbuffer.hEditor = ?:   
        IF tdgbuffer.cAccessMode = "PRIVATE" THEN DELETE tdgbuffer.
        ELSE tdgbuffer.hEditor = ghEditor.
    END.
    FOR EACH tdgVar WHERE tdgVar.hEditor = ?:
        IF tdgVar.cAccessMode = "PRIVATE" THEN DELETE tdgVar.
        ELSE tdgVar.hEditor = ghEditor.
    END.
    FOR EACH tdMethod WHERE tdMethod.hEditor = ?:
        IF tdMethod.cAccessMode = "PRIVATE"
         OR CAN-DO("CONSTRUCTOR,DESTRUCTOR", tdMethod.cName)
         THEN DELETE tdMethod.
        ELSE tdMethod.hEditor = ghEditor.
    END.

    /* I keep these two for the day I manage SUPER procedure resources, but there should be empty for classes
      => perhaps I will add something in the loadGloabal PROCESS to remove suspicious guys for .cls files  */
    FOR EACH tdFunc WHERE tdFunc.hEditor = ?:
        tdFunc.hEditor = ghEditor.
    END.
    FOR EACH tdProc WHERE tdProc.hEditor = ?:
        tdProc.hEditor = ghEditor.
    END.
    
    
    /* maintain the temp-table sequence  and remove super private resources */
    /* now we are sure there won't be any conflict with the sequences restored from the xml */
    FOR EACH tdtt WHERE tdtt.hEditor = ?:
        IF tdtt.cAccessMode <> "PRIVATE" THEN ASSIGN
         tdtt.hEditor = ghEditor
         iopittidmax  = iopittidmax + 1.
        FOR EACH tdfld WHERE tdfld.ittid = tdtt.ittid:
            IF tdtt.cAccessMode = "PRIVATE" THEN DELETE tdfld.
            ELSE tdfld.ittid = iopittidmax.
        END.
        FOR EACH tdIdx WHERE tdIdx.ittid = tdtt.ittid:
            IF tdtt.cAccessMode = "PRIVATE" THEN DELETE tdIdx.
            ELSE tdIdx.ittid = iopittidmax.
        END.
        IF tdtt.cAccessMode = "PRIVATE" THEN DELETE tdtt.
        ELSE tdtt.ittid = iopittidmax.
    END.
    
    
    /* and now if this super guy has also SUPER resources, then load them too */
    IF tdEdt.cInherits > "" THEN DO:
        RUN loadSuperResources (tdEdt.cInherits, gcUsingList, INPUT-OUTPUT iopiSuperLEvel) NO-ERROR.
        IF ERROR-STATUS:ERROR THEN RETURN ERROR errReturnValue().
    END.
        
END PROCEDURE.

    
PROCEDURE loadTargetObject:    
    lReadOK = DATASET dsGlobRsc:READ-XML("file"
                                        ,pcLoadFile
                                        ,"EMPTY"  /* readMode */
                                        ,gcXmlSchemaFile   /*schema-location*/
                                        ,NO) /* override-default-mapping*/
                                        NO-ERROR.
    IF lReadOK = NO THEN RETURN ERROR ProgressErrors().
    
    
    /* sophisticated buffer-copy FOR this one.  In short, keep some system fields with values set by abhackwin 
     I may rearchitect this later ... */
    FIND tdEdt.
    ASSIGN
     phbttedt::cABHackDumpVersion  = tdEdt.cABHackDumpVersion
     phbttedt::cInherits           = tdEdt.cInherits
     phbttedt::dLastLoadGlobalTime = tdEdt.dLastLoadGlobalTime
     phbttedt::iCursorLine         = tdEdt.iCursorLine
     phbttedt::isClass             = tdEdt.isClass.
END PROCEDURE.
                                            

PROCEDURE refineTargetObjectRecords:
    /* now REPLACE the dumped handle by the one passed as input */
    FOR EACH tdFunc         : tdFunc.hEditor = ghEditor.            END.
    FOR EACH tdgbuffer      : tdgbuffer.hEditor = ghEditor.         END.
    FOR EACH tdgVar         : tdgVar.hEditor = ghEditor.            END.
    FOR EACH tdMark         : tdMark.hEditor = ghEditor.            END.
    FOR EACH tdMethod       : tdMethod.hEditor = ghEditor.          END.
    FOR EACH tdProc         : tdProc.hEditor = ghEditor.            END.
    FOR EACH tdReferedBuffer: tdReferedBuffer.hEditor = ghEditor.   END.
    FOR EACH tdUsing        : tdUsing.hEditor = ghEditor.           END.
    FOR EACH tdgLibHandle   : tdgLibHandle.hEditor = ghEditor.      END.
    FOR EACH tdPreproc      : tdPreproc.hEditor    = ghEditor.      END.
    
    
    /* maintain the temp-table sequence */
    /* now we are sure there won't be any conflict with the sequences restored from the xml */
    FOR EACH tdtt:
        ASSIGN
         tdtt.hEditor = ghEditor
         iopittidmax  = iopittidmax + 1.
        FOR EACH tdfld WHERE tdfld.ittid = tdtt.ittid:
            tdfld.ittid = iopittidmax.
        END.
        FOR EACH tdIdx WHERE tdIdx.ittid = tdtt.ittid:
            tdIdx.ittid = iopittidmax.
        END.
        tdtt.ittid = iopittidmax.
    END.
END PROCEDURE.                                            



FUNCTION ProgressErrors RETURNS CHARACTER( ):    
    DEFINE VARIABLE cMessages AS CHARACTER   NO-UNDO.
    DEFINE VARIABLE iMessage  AS INTEGER     NO-UNDO.
    
    DO iMessage = 1 TO ERROR-STATUS:NUM-MESSAGES:
        cMessages = cMessages + "   |   " + ERROR-STATUS:GET-MESSAGE(iMessage).
    END.
    cMessages = SUBSTRING(cMessages, 8).
    RETURN cMessages.
END FUNCTION.


FUNCTION errReturnValue RETURNS CHARACTER( ):    
    IF RETURN-VALUE > "" THEN RETURN RETURN-VALUE.
    RETURN ProgressErrors().
END FUNCTION.

/* setCellColumns.i */
DEF VAR brwColHandle AS HANDLE NO-UNDO EXTENT 200.
DEF VAR brwColList AS CHAR NO-UNDO.
DEF VAR brwColName AS CHAR NO-UNDO EXTENT 200.
DEF VAR brwColWidth AS DEC NO-UNDO EXTENT 200.
DEF VAR cellColumnDat AS CHAR NO-UNDO.  /* Dat File Name */
DEF VAR cTestName AS CHAR NO-UNDO.
DEF VAR datColList AS CHAR NO-UNDO.
DEF VAR datColName AS CHAR NO-UNDO EXTENT 200.
DEF VAR datColWidth AS DEC NO-UNDO EXTENT 200.
DEF VAR iBrwCols AS INT NO-UNDO.
DEF VAR iDatCols AS INT NO-UNDO.
DEF VAR lAutoSave AS LOG NO-UNDO.
DEF VAR lNoMatch AS LOG NO-UNDO.
DEF VAR lSaveChanges AS LOG NO-UNDO.
DEF VAR iCtr AS INT NO-UNDO.
DEF VAR jCtr AS INT NO-UNDO.

/* create a &SCOPED-DEFINE cellColumnDat value prior to this include
   if another file name is desired to store user cell column order */
&IF DEFINED(cellColumnDat) EQ 0 &THEN
&SCOPED-DEFINE cellColumnDat {&FIRST-TABLE-IN-QUERY-{&BROWSE-NAME}}
&ENDIF

cellColumnDat = './users/' + USERID('ASI') + '/{&cellColumnDat}.dat'.

PROCEDURE setCellColumns:
  
    IF SEARCH(cellColumnDat) NE ? THEN DO:
        lAutoSave = NO.
     
        /* get user cell column order from .dat file */
        INPUT FROM VALUE(cellColumnDat) NO-ECHO.
        REPEAT:
            ASSIGN 
                iDatCols = iDatCols + 1.
            IMPORT datColName[iDatCols] datColWidth[iDatCols].
            ASSIGN 
                datColList = datColList + datColName[iDatCols] + ",".
        END. /* repeat */
        ASSIGN 
            datColList = TRIM(datColList,",")
            iDatCols = NUM-ENTRIES(datColList).
        INPUT CLOSE.
     
        /* read column handles from default browse */
        ASSIGN 
            iBrwCols = {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}.
        DO iCtr = 1 TO iBrwCols:
            ASSIGN 
                brwColHandle[iCtr] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iCtr)
                brwColName[iCtr] = brwColHandle[iCtr]:NAME
                brwColWidth[iCtr] = brwColHandle[iCtr]:WIDTH-PIXELS
                brwColList = brwColList + brwColName[iCtr] + "," .
        END.
        ASSIGN 
            brwColList = TRIM(brwColList,",")
            iBrwCols = NUM-ENTRIES(brwColList) .
    
        /* If the columns don't have the same number of list items (browse had a field added/delete in code), delete the dat file */
        IF iDatCols NE iBrwCols THEN DO:
            OS-DELETE VALUE(cellColumnDat).
            RETURN.
        END.        
        
        /* If the column names don't match (browse had a field replaced in code), delete the dat file */
        DO iCtr = 1 TO iBrwCols:
            IF NOT CAN-DO(datColList, ENTRY(iCtr,brwColList)) THEN DO:
                ASSIGN 
                    lNoMatch = TRUE.
                LEAVE.
            END. 
        END.
        IF lNoMatch THEN DO:
            OS-DELETE VALUE(cellColumnDat).
            RETURN.
        END.

        /* If there are two columns with the same name, discard the dat file */
        DO iCtr = 1 TO iDatCols:
            ASSIGN 
                cTestName = ENTRY(iCtr,datColList).
            DO jCtr = 1 TO iDatCols:
                IF ENTRY(jCtr,datColList) EQ cTestName 
                AND jCtr NE iCtr THEN DO:
                    ASSIGN 
                        lNoMatch = TRUE.
                    LEAVE.
                END.
            END.
        END.            
        IF lNoMatch THEN DO:
            OS-DELETE VALUE(cellColumnDat).
            RETURN.
        END.
            
        /* Now move the browse columns to match the order of the dat file */
        /* Have to do this in reverse order (R-L) to preserve browse col indexing */
        DO iCtr = iDatCols TO 1 BY -1:
            DO jCtr = 1 TO iBrwCols:
                IF brwColName[jCtr] EQ datColName[iCtr] THEN DO:
                    ASSIGN 
                        brwColHandle[jCtr]:WIDTH-PIXELS = datColWidth[iCtr].
                    {&BROWSE-NAME}:MOVE-COLUMN(jCtr,iCtr).
                END.
            END.
        END.    
    END. /* search */
  
    /* read new order to check for changes when exiting */
    DO iCtr = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        ASSIGN
            brwColHandle[iCtr] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iCtr)
            brwColWidth[iCtr] = {&BROWSE-NAME}:GET-BROWSE-COLUMN(iCtr):WIDTH-PIXELS.
    END. /* do iCtr */
    
END PROCEDURE.

PROCEDURE local-destroy:
/*------------------------------------------------------------------------------
  Purpose:     Override standard ADM method
  Notes:       
------------------------------------------------------------------------------*/
    /* check for any columns changes */
    DO iCtr = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
        IF brwColHandle[iCtr]:NAME EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(iCtr):NAME 
        AND brwColWidth[iCtr] EQ {&BROWSE-NAME}:GET-BROWSE-COLUMN(iCtr):WIDTH-PIXELS THEN 
        NEXT.
        ELSE ASSIGN 
            lNoMatch = TRUE.
    END.
  
    IF lNoMatch THEN DO:
        /* If there's no dat file, create one no matter what */
        IF SEARCH(cellColumnDat) EQ ? THEN ASSIGN 
            lAutoSave = TRUE. 
        IF NOT lAutoSave THEN MESSAGE 
            'Save Column Changes?' 
            VIEW-AS ALERT-BOX
            QUESTION BUTTONS YES-NO UPDATE lSaveChanges.
        IF lSaveChanges OR lAutoSave THEN DO:
            OS-CREATE-DIR VALUE("./users/" + USERID("ASI")). 
            OUTPUT TO VALUE(cellColumnDat).
            DO jCtr = 1 TO {&BROWSE-NAME}:NUM-COLUMNS IN FRAME {&FRAME-NAME}:
                EXPORT 
                    {&BROWSE-NAME}:GET-BROWSE-COLUMN(jCtr):NAME 
                    {&BROWSE-NAME}:GET-BROWSE-COLUMN(jCtr):WIDTH-PIXELS.
            END. /* do jCtr */
            OUTPUT CLOSE.
        END. /* if savechanges */
        LEAVE.
    END. /* lFail */

&IF DEFINED(xlocal-destroy) &THEN
    RUN xlocal-destroy.
&ENDIF
  
    RUN dispatch IN THIS-PROCEDURE ( INPUT 'destroy':U ) .

END PROCEDURE.

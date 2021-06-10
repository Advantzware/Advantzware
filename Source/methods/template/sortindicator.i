&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : sortindicator.i
    Purpose     : Indicate the column sortiong

    Syntax      : {methods/template/sortindicator.i}

    Description : 


    Author(s)   : Anjly
    Created     : 10/28/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
IF NOT VALID-HANDLE({&BROWSE-NAME}:CURRENT-COLUMN) THEN
DO:
   IF '{&programName}' NE ''  THEN
   DO:
FIND FIRST userColumn NO-LOCK 
    WHERE userColumn.usrId      EQ USERID('ASI') 
    AND userColumn.programName  EQ '{&programName}' 
    AND userColumn.ColName      EQ "Sorting-Column" NO-ERROR.        
IF AVAILABLE userColumn THEN 
DO:
    GET-HANDLE:
    DO iCnt = 1 TO NUM-ENTRIES(cColHandList, ","):
        hColumnRowColor = HANDLE(ENTRY(iCnt,cColHandList,",")).
        IF  VALID-HANDLE(hColumnRowColor) AND hColumnRowColor:NAME = userColumn.sortByColumnName THEN
        DO:
            hCurrentColumn = hColumnRowColor.
            LEAVE GET-HANDLE.
        END.
                          
    END.
    ASSIGN    
        lv-sort-by     = userColumn.sortByColumnName
        lv-sort-by-lab = userColumn.sortByColumnLabel
        ll-sort-asc    = userColumn.sortAsc.
    hCurrentColumn:LABEL-BGCOLOR        = 30.
    hCurrentColumn:SORT-ASCENDING = ll-sort-asc.
END.
END.
END.
ELSE
DO:
{&BROWSE-NAME}:CLEAR-SORT-ARROWS( ) NO-ERROR.
hCurrentColumn = {&BROWSE-NAME}:CURRENT-COLUMN.
IF VALID-HANDLE(hPrevColumn) AND hPrevColumn:LABEL-BGCOLOR = 30 THEN
hPrevColumn:LABEL-BGCOLOR                    = 14.
IF hCurrentColumn:LABEL-BGCOLOR = 30 THEN
hCurrentColumn:LABEL-BGCOLOR  = 14.
IF hCurrentColumn EQ hPrevColumn THEN
    ASSIGN
        lsortBy                        = NOT lsortBy. 
hCurrentColumn:SORT-ASCENDING = lsortBy.

hPrevColumn = {&BROWSE-NAME}:CURRENT-COLUMN.
END.

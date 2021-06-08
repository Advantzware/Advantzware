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

    
FIND FIRST userColumn NO-LOCK WHERE userColumn.usrId     EQ USERID('ASI') 
    AND userColumn.programName = '{&programName}' 
    AND userColumn.sorted = TRUE NO-ERROR.        
IF  {&BROWSE-NAME}:CURRENT-COLUMN = ?  AND AVAILABLE userColumn THEN 
DO:
    GET-HANDLE:
    DO iCnt = 1 TO NUM-ENTRIES(cColHandList, ","):
        hColumnRowColor = HANDLE(ENTRY(iCnt,cColHandList,",")).
        IF  hColumnRowColor:NAME = userColumn.ColName THEN
        DO:

            hCurrentColumn = hColumnRowColor.
            LEAVE GET-HANDLE.
        END.
                          
    END.
    hCurrentColumn:SORT-ASCENDING = lsortBy.
    ASSIGN    
        lv-sort-by     = userColumn.ColName
        lv-sort-by-lab = userColumn.sortByColumnLabel
        ll-sort-asc    = userColumn.sortAsc
        lh-column      = hCurrentColumn.
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

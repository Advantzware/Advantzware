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

{&BROWSE-NAME}:CLEAR-SORT-ARROWS( ) NO-ERROR.
hCurrentColumn = {&BROWSE-NAME}:CURRENT-COLUMN.

IF VALID-HANDLE(hPrevColumn) AND hPrevColumn:LABEL-BGCOLOR EQ 30 THEN
    hPrevColumn:LABEL-BGCOLOR   = 14.
    
IF VALID-HANDLE(hCurrentColumn) AND hCurrentColumn:LABEL-BGCOLOR EQ 30 THEN
    hCurrentColumn:LABEL-BGCOLOR  = 14.

IF VALID-HANDLE(hCurrentColumn) AND 
    VALID-HANDLE(hPrevColumn) AND 
    hCurrentColumn EQ hPrevColumn THEN
ASSIGN
    lsortBy = NOT lsortBy. 
        
hCurrentColumn:SORT-ASCENDING = lsortBy NO-ERROR.

hPrevColumn = {&BROWSE-NAME}:CURRENT-COLUMN NO-ERROR.


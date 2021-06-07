&ANALYZE-SUSPEND _VERSION-NUMBER UIB_v8r12
&ANALYZE-RESUME
&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _DEFINITIONS Include 
/*--------------------------------------------------------------------------
    File        : sortindicatorend.i
    Purpose     : Indicate the column sorting

    Syntax      : {methods/template/sortindicatorend.i}

    Description : 


    Author(s)   : Anjly
    Created     : 10/28/20
    Notes       :
  ------------------------------------------------------------------------*/
/*          This .W file was created with the Progress UIB.             */
/*----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

hCurrentColumn:LABEL-BGCOLOR = 30. 

FOR EACH userColumn EXCLUSIVE-LOCK WHERE userColumn.usrId     EQ USERID('ASI') 
AND userColumn.programName = {1}
AND userColumn.ColName     = hCurrentColumn:NAME BY userColumn.colPosition:
    userColumn.sorted      = FALSE .
    iLastColPos = userColumn.colPosition.
    END.

FIND FIRST userColumn EXCLUSIVE-LOCK WHERE userColumn.usrId     EQ USERID('ASI') 
    AND userColumn.programName = {1}
    AND userColumn.ColName    = hCurrentColumn:NAME NO-ERROR.
        
IF NOT AVAILABLE userColumn THEN 
DO:
    CREATE userColumn.
    ASSIGN
        userColumn.programName       = {1}
        userColumn.usrId             = USERID('ASI') 
        userColumn.ColName           = hCurrentColumn:NAME
        userColumn.sortByColumnLabel = hCurrentColumn:LABEL
        userColumn.sortAsc           = lsortBy
        userColumn.sorted            = TRUE
    userColumn.colPosition = iLastColPos + 1.
END.
ELSE
DO:
    ASSIGN
        userColumn.sortByColumnLabel = hCurrentColumn:LABEL
        userColumn.sortAsc           = lsortBy
        userColumn.sorted            = TRUE.
        END.

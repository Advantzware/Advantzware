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

IF '{&programName}' NE '' THEN
DO:
FIND FIRST userColumn EXCLUSIVE-LOCK WHERE userColumn.usrId     EQ USERID('ASI') 
    AND userColumn.programName = '{&programName}'
    AND userColumn.ColName = "Sorting-Column" NO-ERROR.
        
IF NOT AVAILABLE userColumn THEN 
DO:
    CREATE userColumn.
    ASSIGN
        userColumn.programName       = '{&programName}'
        userColumn.usrId             = USERID('ASI') 
        userColumn.ColName           = "Sorting-Column"
        userColumn.sortByColumnName  = hCurrentColumn:NAME
        userColumn.sortByColumnLabel = hCurrentColumn:LABEL
        userColumn.sortAsc           = lsortBy
        userColumn.sorted            = TRUE
        userColumn.colPosition       = 10000
        NO-ERROR.
END.
ELSE
DO:
    ASSIGN
        userColumn.sortByColumnName  = hCurrentColumn:NAME
        userColumn.sortByColumnLabel = hCurrentColumn:LABEL
        userColumn.sortAsc           = lsortBy
        userColumn.sorted            = TRUE
        NO-ERROR.
        END.
 END.      
        

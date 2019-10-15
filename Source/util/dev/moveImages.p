
/*------------------------------------------------------------------------
    File        : moveImages.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : MYT
MYT
    Created     : Fri Oct 11 07:39:26 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF STREAM moved.
DEF STREAM notMoved.
DEF VAR cShortName AS CHAR NO-UNDO.
DEF VAR cLongName AS CHAR NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
OUTPUT STREAM moved TO c:\tmp\moved.txt.
OUTPUT STREAM notMoved TO c:\tmp\notMoved.txt.

INPUT FROM c:\tmp\Images_to_delete.txt.

REPEAT:
    IMPORT cShortName.
    ASSIGN 
        cLongName = "c:\asigui\repository\resources\" + cShortName.
    IF SEARCH(cLongName) NE ? THEN DO:
        OS-COPY VALUE (cLongName) VALUE ("c:\asigui\repository\resources\ImageBackup\" + cShortName.
        OS-DELETE VALUE (cLongName).
    END.
    ELSE DO:
        PUT STREAM notMoved cLongName + CHR(10).
    END. 
END.
        
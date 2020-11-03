DEF VAR c{1}FullPathName AS CHAR INITIAL "{1}.exe" NO-UNDO.
DEF VAR c{1}CommandString AS CHAR NO-UNDO.
RUN OS_GetFullFileName (INPUT-OUTPUT c{1}FullPathName).
IF c{1}FullPathName = "" THEN 
DO:
    RUN displayMessage ("48").
    RETURN.
END.

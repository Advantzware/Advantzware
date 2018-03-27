     /* custom/out2file.i */
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
     DEF VAR ldummy AS LOG NO-UNDO.
     DEF VAR lv-output AS cha NO-UNDO.

     lv-output = list-name .
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE lv-output 
         TITLE      "Save Output File As"
         FILTERS    "Text Documents (*.txt)" "*.txt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         DEFAULT-EXTENSION ".txt"
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
     
     IF NOT OKpressed THEN  RETURN NO-APPLY.
     IF INDEX(lv-output,".") = 0 THEN 
             lv-output = lv-output + ".txt".
     OS-COPY VALUE(list-name) VALUE(lv-output).
     IF OS-ERROR = 0 AND list-name NE lv-output THEN OS-DELETE VALUE(list-name).


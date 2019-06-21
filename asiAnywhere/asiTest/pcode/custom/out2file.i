     /* custom/out2file.i */
     
     DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
     DEF VAR ldummy AS LOG NO-UNDO.
     DEF VAR lv-output AS cha NO-UNDO.

     lv-output = list-name.
     if init-dir = "" then init-dir = "c:\temp" .
     SYSTEM-DIALOG GET-FILE lv-output
         TITLE      "Enter Listing Name to SAVE AS ..."
         FILTERS    "Listing Files (*.rpt)" "*.rpt",
                    "All Files (*.*)" "*.*"
         INITIAL-DIR init-dir
         ASK-OVERWRITE
    /*     CREATE-TEST-FILE*/
         SAVE-AS
         USE-FILENAME
   
         UPDATE OKpressed.
         
     IF NOT OKpressed THEN  RETURN NO-APPLY.
     OS-COPY VALUE(list-name) VALUE(lv-output).
     IF OS-ERROR = 0 THEN OS-DELETE VALUE(list-name).


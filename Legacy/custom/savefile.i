/* custom/savefile.i  Save report to file */
DEFINE VARIABLE OKpressed AS LOGICAL NO-UNDO.
DEF VAR lv-save-to AS cha NO-UNDO.

if init-dir = "" then init-dir = "c:\temp" .
lv-save-to = list-name.

SYSTEM-DIALOG GET-FILE lv-save-to
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

OS-COPY VALUE(list-name) VALUE(lv-save-to).

/* help.p */

DEFINE INPUT PARAMETER callprgm AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER help-page AS INTEGER NO-UNDO.

DEFINE VARIABLE helpfile AS CHARACTER NO-UNDO.
DEFINE VARIABLE ldummy AS LOGICAL NO-UNDO.

ASSIGN
  ldummy = SESSION:SET-WAIT-STATE('')
  callprgm = REPLACE(callprgm,'.','').
IF help-page GT 0 THEN
callprgm = SUBSTRING(callprgm,1,7) + STRING(help-page).
helpfile = "help\" + SUBSTRING(callprgm,1) + ".htm".

IF SEARCH(helpfile) NE ? THEN
OS-COMMAND NO-WAIT VALUE(helpfile).
ELSE
MESSAGE 'Help file "' + helpfile + '" does not exist'
    VIEW-AS ALERT-BOX WARNING.

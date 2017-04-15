/* util/chk-mod.p  Check module license */

DEFINE INPUT PARAMETER ipcDBName AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcModule AS CHARACTER NO-UNDO. /* button label*/

IF INDEX(ipcModule,"&") GT 0 THEN
ipcModule = REPLACE(ipcModule,"&","").

FIND FIRST asi.module NO-LOCK
     WHERE asi.module.db-name EQ ipcDBName
       AND asi.module.module  EQ ipcModule
     NO-ERROR.
IF AVAILABLE asi.module AND
  (NOT asi.module.is-used OR
   asi.module.expire-date LT TODAY) THEN DO:
    MESSAGE "System License Error for" "~"" + asi.module.dscr + "~"!" SKIP(1)
            "Contact Advantzware (215.369.7800) to Obtain a License for this Module."
        VIEW-AS ALERT-BOX ERROR TITLE "System License".
    RETURN ERROR.
END.

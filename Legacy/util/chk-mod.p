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
   asi.module.expire-date LT TODAY)
THEN DO:
    MESSAGE "System License Error!" SKIP
            "Contact Advance Software Inc. to purchase this module."
        VIEW-AS ALERT-BOX ERROR.
    RETURN ERROR.
END.

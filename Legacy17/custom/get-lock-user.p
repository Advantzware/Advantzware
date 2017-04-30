/* custom/get-lock-user.p */
/* Determine who is lccking the record */
DEF INPUT PARAMETER ipcTable AS CHAR NO-UNDO.
DEF INPUT PARAMETER iprTableRow AS ROWID NO-UNDO.
DEF OUTPUT PARAMETER opcUpdUsr AS CHAR NO-UNDO.
ipcTable = TRIM(ipcTable).


DEFINE VARIABLE cCurrentDatabase AS CHARACTER  NO-UNDO. 
DEFINE VARIABLE iNumConnectedDBs AS INTEGER    NO-UNDO.

DO iNumConnectedDBs = 1 TO NUM-DBS: 
    cCurrentDatabase = LDBNAME(iNumConnectedDBs).
    IF cCurrentDatabase = "ASI" THEN DO:    
      CREATE ALIAS "DICTDB"  FOR DATABASE VALUE(cCurrentDatabase).
      RUN custom/chk-usr-lock.p (INPUT ipcTable, INPUT iprTableRow,
                               OUTPUT opcUpdUsr).
      DELETE ALIAS dictdb. 
    END.
END.



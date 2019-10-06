DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
DEFINE INPUT  PARAMETER iprRow AS ROWID       NO-UNDO.
DEF OUTPUT PARAMETER opiCount AS INT NO-UNDO.

DEF VAR hQryHndl AS HANDLE NO-UNDO.
DEF VAR hQryUpd AS HANDLE NO-UNDO.
DEF VAR lStatus AS LOG NO-UNDO.
DEF VAR cQry AS CHAR NO-UNDO. 
DEF VAR bfTable AS HANDLE NO-UNDO.


DEF VAR hBufField AS HANDLE NO-UNDO.
DEF VAR cValue AS CHAR NO-UNDO.
DEF VAR cNewRecKey AS CHAR NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.

DO TRANSACTION:

  CREATE BUFFER bfTable FOR TABLE  ipcTableName.
  
  CREATE QUERY hQryHndl.
  
  hQryHndl:ADD-BUFFER(bfTable).

  cQry = "For each " + ipcTableName  + " where " + "rowid(" + ipcTableName + ") " 
          + " = " 
          + "to-rowid(" + '"' + STRING(iprRow) + '"' + ")".
  
  ASSIGN lStatus = hQryHndl:QUERY-PREPARE(cQry) NO-ERROR.
  

  lStatus = hQryHndl:QUERY-OPEN() NO-ERROR.  
  lStatus = hQryHndl:GET-FIRST(EXCLUSIVE-LOCK) NO-ERROR.
  
  
  DO WHILE bfTable:ROWID NE ? :
  
      hBufField = bfTable:BUFFER-FIELD("rec_key").
      cValue = hBufField:STRING-VALUE.
  
      cNewRecKey = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999").
  
  
      /* Assign new value */  
      hBufField:BUFFER-VALUE = cNewRecKey NO-ERROR.
  
      iCount = iCount + 1.
  
  
      CREATE rec_key.
      ASSIGN
       rec_key.rec_key    = cNewRecKey
       rec_key.table_name = ipcTableName.
  
      lStatus = hQryHndl:GET-NEXT(EXCLUSIVE-LOCK).
    
  END.

  opiCount = iCount.

  DELETE OBJECT hQryHndl.
  DELETE OBJECT bfTable.

END. /* transaction */




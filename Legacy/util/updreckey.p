
DEF INPUT PARAMETER ipcTableName AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opiCount AS INT NO-UNDO.
DEF VAR hQryHndl AS HANDLE NO-UNDO.
DEF VAR hQryUpd AS HANDLE NO-UNDO.
DEF VAR lStatus AS LOG NO-UNDO.
DEF VAR cQry AS CHAR NO-UNDO. 
DEF VAR bfTable AS HANDLE NO-UNDO.
DEF VAR bfTabUpd AS HANDLE NO-UNDO.

DEF VAR hBufField AS HANDLE NO-UNDO.
DEF VAR cValue AS CHAR NO-UNDO.
DEF VAR cNewRecKey AS CHAR NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
DEF VAR rSaveUpdRowid AS ROWID NO-UNDO.
DEF VAR opCnt AS INT NO-UNDO.

CREATE BUFFER bfTable FOR TABLE  ipcTableName.


CREATE QUERY hQryHndl.


OUTPUT TO c:\tmp\newRecKey.LOG APPEND.

hQryHndl:ADD-BUFFER(bfTable).

cQry = "For each " + ipcTableName  + " where " + ipcTableName + ".rec_key = '' ".

ASSIGN lStatus = hQryHndl:QUERY-PREPARE(cQry) NO-ERROR.

lStatus = hQryHndl:QUERY-OPEN().



lStatus = hQryHndl:GET-FIRST(NO-LOCK).


DO WHILE bfTable:ROWID NE ? :
    rSaveUpdRowid = bfTable:ROWID.

    hBufField = bfTable:BUFFER-FIELD("rec_key").
    cValue = hBufField:STRING-VALUE.

    RUN util/updRecKey2.p (INPUT ipcTableName, INPUT rSaveUpdRowid, OUTPUT opCnt).

    iCount = iCount + 1.
    lStatus = hQryHndl:GET-NEXT(NO-LOCK).
  
END.

opiCount = iCount.

DELETE OBJECT hQryHndl.
DELETE OBJECT bfTable.

OUTPUT CLOSE.

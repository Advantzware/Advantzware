DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR iLineNo AS INT.
DEF VAR iMaxLines AS INT INITIAL 1.
DEF VAR iCtr AS INT.
def var lUnique AS LOG.

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:SheetsInNewWorkbook = 3.
/* Set pages and label rows */                    
ASSIGN 
    iLineNo = 1
    chWorkbook = chExcelApplication:Workbooks:Add()
    chWorksheet = chExcelApplication:Sheets:Item(1)
    chWorksheet:range("A1"):VALUE = "Table"
    chWorksheet:range("B1"):VALUE = "Unique"
    chWorksheet:range("C1"):VALUE = "DumpName"
    chWorksheet:range("D1"):VALUE = "Label"
    chWorksheet:range("E1"):VALUE = "Description"
    chWorksheet:range("A1" + ":E1"):Font:Bold = TRUE
    chWorksheet:NAME = "Tables"
    
    chWorksheet = chExcelApplication:Sheets:Item(2)
    chWorksheet:range("A1"):VALUE = "Table"
    chWorksheet:range("B1"):VALUE = "Order"
    chWorksheet:range("C1"):VALUE = "Field"
    chWorksheet:range("D1"):VALUE = "Label"
    chWorksheet:range("E1"):VALUE = "Datatype"
    chWorksheet:range("F1"):VALUE = "Mandatory"
    chWorksheet:range("G1"):VALUE = "Format"
    chWorksheet:range("H1"):VALUE = "Init Value"
    chWorksheet:range("I1"):VALUE = "Decimals"
    chWorksheet:range("J1"):VALUE = "Extents"
    chWorksheet:range("K1"):VALUE = "ValExp"
    chWorksheet:range("L1"):VALUE = "Description"
    chWorksheet:range("A1" + ":L1"):Font:Bold = TRUE
    chWorksheet:NAME = "Fields"

    chWorksheet = chExcelApplication:Sheets:Item(3)
    chWorksheet:range("A1"):VALUE = "Table"
    chWorksheet:range("B1"):VALUE = "Index Name"
    chWorksheet:range("C1"):VALUE = "Primary"
    chWorksheet:range("D1"):VALUE = "Unique"
    chWorksheet:range("E1"):VALUE = "Seq"
    chWorksheet:range("F1"):VALUE = "Field Name"
    chWorksheet:range("G1"):VALUE = "Ascending"
    chWorksheet:range("A1" + ":G1"):Font:Bold = TRUE
    chWorksheet:NAME = "Indexes".

ASSIGN 
    iLineNo = 1
    chWorksheet = chExcelApplication:Sheets:Item(1).
    chWorksheet:select().
for each _file where 
    substring(_file._file-name,1,1) <> "_" and 
    substring(_file._file-name,1,3) <> "SYS":
    if can-find(first _index of _file where
        _index._unique) then assign lUnique = true.
    else assign
        lUnique = false.
    assign
        iLineNo = iLineNo + 1    
        chWorksheet:range("A" + string(iLineNo)):VALUE = _file._file-name
        chWorksheet:range("B" + string(iLineNo)):VALUE = IF not lUnique then "NO" else ""
        chWorksheet:range("C" + string(iLineNo)):VALUE = _file._dump-name
        chWorksheet:range("D" + string(iLineNo)):VALUE = _file._file-label
        chWorksheet:range("E" + string(iLineNo)):VALUE = _file._desc
        no-error.
end.
chWorksheet:columns("A:E"):select.
chExcelApplication:selection:columns:autofit.
chWorksheet:range("A1:A1"):select.
pause 1.

ASSIGN 
    iLineNo = 1
    chWorksheet = chExcelApplication:Sheets:Item(2).
    chWorksheet:select().
for each _file where 
    substring(_file._file-name,1,1) <> "_" and 
    substring(_file._file-name,1,3) <> "SYS":
    for each _field of _file by _field._order:
        assign iLineNo = iLineNo + 1.    
        assign chWorksheet:range("A" + string(iLineNo)):VALUE = _file._file-name no-error.
        assign chWorksheet:range("B" + string(iLineNo)):VALUE = _field._order no-error.
        assign chWorksheet:range("C" + string(iLineNo)):VALUE = _field._field-name no-error.
        assign chWorksheet:range("D" + string(iLineNo)):VALUE = _field._label no-error.
        assign chWorksheet:range("E" + string(iLineNo)):VALUE = _field._data-type no-error.
        assign chWorksheet:range("F" + string(iLineNo)):VALUE = _field._mandatory no-error.
        assign chWorksheet:range("G" + string(iLineNo)):VALUE = _field._format no-error.
        assign chWorksheet:range("H" + string(iLineNo)):VALUE = _field._initial no-error.
        assign chWorksheet:range("I" + string(iLineNo)):VALUE = _field._decimals no-error.
        assign chWorksheet:range("J" + string(iLineNo)):VALUE = _field._extent no-error.
        assign chWorksheet:range("K" + string(iLineNo)):VALUE = _field._valexp no-error.
        assign chWorksheet:range("L" + string(iLineNo)):VALUE = _field._desc no-error.
    end.
end.
chWorksheet:columns("A:L"):select.
chExcelApplication:selection:columns:autofit.
chWorksheet:range("A1:A1"):select.
pause 1.

ASSIGN 
    iLineNo = 1
    chWorksheet = chExcelApplication:Sheets:Item(3).
    chWorksheet:select().
for each _file where 
    substring(_file._file-name,1,1) <> "_" and 
    substring(_file._file-name,1,3) <> "SYS":
    for each _index of _file break by _index._index-name:
        for each _index-field of _index break by _index-field._index-seq:
        find _field of _index-field no-lock no-error.
        assign
            iLineNo = iLineNo + 1    
            chWorksheet:range("A" + string(iLineNo)):VALUE = _file._file-name
            chWorksheet:range("B" + string(iLineNo)):VALUE = _index._index-name
            chWorksheet:range("C" + string(iLineNo)):VALUE = if _index-field._index-seq = 1 and recid(_index) = _file._prime-index then "PRIMARY" else ""
            chWorksheet:range("D" + string(iLineNo)):VALUE = if _index-field._index-seq = 1 and _index._unique THEN "UNIQUE" else ""
            chWorksheet:range("E" + string(iLineNo)):VALUE = _index-field._index-seq
            chWorksheet:range("F" + string(iLineNo)):VALUE = _field._field-name
            chWorksheet:range("G" + string(iLineNo)):VALUE = if NOT _index-field._ascending THEN "DESC" ELSE ""
            no-error.
        end.
    end.
end.
chWorksheet:columns("A:G"):select.
chExcelApplication:selection:columns:autofit.
chWorksheet:range("A1:A1"):select.
chWorksheet = chExcelApplication:Sheets:Item(1).
chWorksheet:select().

assign
    chExcelApplication:VISIBLE = TRUE.

    RELEASE OBJECT chWorksheet        NO-ERROR.
    RELEASE OBJECT chWorkBook         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

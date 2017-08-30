DEF VAR chExcelApplication  AS COM-HANDLE   NO-UNDO.
DEF VAR chWorkbook          AS COM-HANDLE   NO-UNDO.
DEF VAR chWorksheet         AS COM-HANDLE   NO-UNDO.
DEF VAR iLineNo AS INT.
DEF VAR iMaxLines AS INT INITIAL 1.
DEF VAR iCtr AS INT.

CREATE "Excel.Application" chExcelApplication.
chExcelApplication:SheetsInNewWorkbook = 3.
/* Set pages and label rows */                    
ASSIGN 
    iLineNo = 1
    chWorkbook = chExcelApplication:Workbooks:Add()
    chWorksheet = chExcelApplication:Sheets:Item(1)
    chWorksheet:range("A1"):VALUE = "Table"
    chWorksheet:range("B1"):VALUE = "DumpName"
    chWorksheet:range("C1"):VALUE = "Label"
    chWorksheet:range("D1"):VALUE = "Description"
    chWorksheet:range("A1" + ":D1"):Font:Bold = TRUE
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
for each _file where 
    substring(_file._file-name,1,1) <> "_" and 
    substring(_file._file-name,1,3) <> "SYS":
    assign
        iLineNo = iLineNo + 1    
        chWorksheet:range("A" + string(iLineNo)):VALUE = _file._file-name
        chWorksheet:range("B" + string(iLineNo)):VALUE = _file._dump-name
        chWorksheet:range("C" + string(iLineNo)):VALUE = _file._file-label
        chWorksheet:range("D" + string(iLineNo)):VALUE = _file._desc
        no-error.
end.
chExcelApplication:columns("A:D"):select.
chExcelApplication:selection:columns:autofit.
RELEASE OBJECT chWorksheet        NO-ERROR.
pause 2.

ASSIGN 
    iLineNo = 1
    chWorksheet = chExcelApplication:Sheets:Item(2).
for each _file where 
    substring(_file._file-name,1,1) <> "_" and 
    substring(_file._file-name,1,3) <> "SYS":
    for each _field of _file by _field._order:
        assign
            iLineNo = iLineNo + 1    
            chWorksheet:range("A" + string(iLineNo)):VALUE = _file._file-name
            chWorksheet:range("B" + string(iLineNo)):VALUE = _field._order
            chWorksheet:range("C" + string(iLineNo)):VALUE = _field._field-name
            chWorksheet:range("D" + string(iLineNo)):VALUE = _field._label
            chWorksheet:range("E" + string(iLineNo)):VALUE = _field._data-type
            chWorksheet:range("F" + string(iLineNo)):VALUE = _field._mandatory
            chWorksheet:range("G" + string(iLineNo)):VALUE = _field._format
            chWorksheet:range("H" + string(iLineNo)):VALUE = _field._initial
            chWorksheet:range("I" + string(iLineNo)):VALUE = _field._decimals
            chWorksheet:range("J" + string(iLineNo)):VALUE = _field._extent
            chWorksheet:range("K" + string(iLineNo)):VALUE = _field._valexp
            chWorksheet:range("L" + string(iLineNo)):VALUE = _field._desc
            no-error.
    end.
end.
chExcelApplication:columns("A:L"):select.
chExcelApplication:selection:columns:autofit.
RELEASE OBJECT chWorksheet        NO-ERROR.
pause 2.

ASSIGN 
    iLineNo = 1
    chWorksheet = chExcelApplication:Sheets:Item(3).
for each _file where 
    substring(_file._file-name,1,1) <> "_" and 
    substring(_file._file-name,1,3) <> "SYS":
    for each _index of _file:
        for each _index-field of _index by _index-field._index-seq:
        find _field of _index-field no-lock no-error.
        assign
            iLineNo = iLineNo + 1    
            chWorksheet:range("A" + string(iLineNo)):VALUE = _file._file-name
            chWorksheet:range("B" + string(iLineNo)):VALUE = _index._index-name
            chWorksheet:range("C" + string(iLineNo)):VALUE = if recid(_index) = _file._prime-index then "PRIMARY" else ""
            chWorksheet:range("D" + string(iLineNo)):VALUE = _index._unique
            chWorksheet:range("E" + string(iLineNo)):VALUE = _index-field._index-seq
            chWorksheet:range("F" + string(iLineNo)):VALUE = _field._field-name
            chWorksheet:range("G" + string(iLineNo)):VALUE = _index-field._ascending
            no-error.
        end.
    end.
end.
chExcelApplication:columns("A:L"):select.
chExcelApplication:selection:columns:autofit.

assign
    chExcelApplication:VISIBLE = TRUE.

    RELEASE OBJECT chWorksheet        NO-ERROR.
    RELEASE OBJECT chWorkBook         NO-ERROR.
    RELEASE OBJECT chExcelApplication NO-ERROR.

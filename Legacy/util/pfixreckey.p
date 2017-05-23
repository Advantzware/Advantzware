def var cfileList as char no-undo.
def var hBuf as handle no-undo.
def var hQuery as handle no-undo.
def var hField as handle no-undo.
def var hBufField as handle no-undo.
def var i as int no-undo.
def var j as int no-undo.

for each _field where
  _field._field-name = "rec_key":
  find _file of _field no-lock.
  assign
    cFileList = cFileLIst + _file._file-name + ",".
end.
assign
 cFileList = trim(cFileList,",").
 
message 
    "Fix (Yes) or List (No)?"
    view-as alert-box question
    buttons yes-no update lFix as log.
     
 
output to c:\temp\badreckey.txt.

do i = 1 to num-entries(cFileList):
    put unformatted "File: " + entry(i,cFileList) + chr(10).
    if valid-handle(hBuf) then delete widget hBuf.
    if valid-handle(hQuery) then delete widget hQuery.
    if valid-handle(hField) then delete widget hField.
    
    create buffer hBuf for table entry(i,cFileList).
    create query hQuery.
    hQuery:add-buffer(hBuf).
    hQuery:query-prepare("for each " + entry(i,cFileList) + " where " + 
                         entry(i,cFileList) + ".rec_key = ''").
    
    hQuery:query-open().
    hQuery:get-first().
    if hBuf:rowid <> ? then do while hBuf:rowid <> ?:
        hField = hBuf:buffer-field("rec_key").
        put unformatted
            entry(i,cFileList) + "," +
            string(hBuf:rowid) + chr(10).
        if lFix then assign
            hField:buffer-value = STRING(TODAY,"99999999") + STRING(NEXT-VALUE(rec_key_seq),"99999999").
        hQuery:get-next().
    end.    
end. 
 

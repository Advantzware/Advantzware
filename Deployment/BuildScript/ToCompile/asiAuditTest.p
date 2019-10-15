DEF OUTPUT PARAMETER oplAuditLicensed AS LOG NO-UNDO.
DEF OUTPUT PARAMETER oplHasTables AS LOG NO-UNDO.
def var cFileList as char no-undo.
def var hBuffer as handle no-undo.
def var hQuery as handle no-undo.
def var iCtr as int no-undo.
def var ipcTableName as char no-undo.
assign 
    cFileList = "apiinbound,apiinbounddetail,apiInboundEvent,apiOutbound,apiOutboundDetail,apiOutboundEvent,apiOutboundTrigger".

do ictr = 1 to num-entries(cFileList):
    assign ipcTableName = entry(iCtr,cFileList).
    if can-find(_file where _file._file-name eq ipcTableName) then do transaction:
        CREATE BUFFER hBuffer FOR TABLE entry(iCtr,cFileList).
        CREATE QUERY hQuery.
        hBuffer:DISABLE-LOAD-TRIGGERS(TRUE).
        hQuery:ADD-BUFFER(hBuffer).
        hQuery:QUERY-PREPARE("FOR EACH " + entry(iCtr,cFileList) + " EXCLUSIVE-LOCK BY ROWID(" + ipcTableName + ")").
        hQuery:QUERY-OPEN ().           
        if hQuery:num-results GT 0 then
        do while not hQuery:query-off-end:
            hquery:get-next.
            if hQuery:query-off-end then leave.
            hbuffer:buffer-delete().
        end.
        hQuery:QUERY-CLOSE().
        hBuffer:BUFFER-RELEASE().
        DELETE OBJECT hBuffer.
        DELETE OBJECT hQuery.
    end.
end.

FIND FIRST module NO-LOCK WHERE 
    module.module = "audit." OR
    module.module = "audit"
    NO-ERROR.
IF AVAIL module THEN ASSIGN 
    oplAuditLicensed = module.is-Used. 

IF CAN-FIND(FIRST _file WHERE _file._file-name EQ "dep-table") THEN ASSIGN 
    oplHasTables = TRUE.



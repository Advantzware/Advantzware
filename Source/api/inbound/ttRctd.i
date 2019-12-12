DEFINE TEMP-TABLE ttRctd NO-UNDO LIKE rm-rctd 
    FIELD ttRctdRowID  AS ROWID
    FIELD rmrctdRowID  AS ROWID
    FIELD ttRctdHasRec AS LOGICAL INIT NO
    FIELD SeqNo        AS INTEGER
    FIELD DBSeqNo      AS INTEGER 
    FIELD vend-tag     AS CHARACTER 
    INDEX SeqNo SeqNo i-no
    .

DEFINE TEMP-TABLE ttInvoice NO-UNDO
    FIELD company     AS CHARACTER
    FIELD invoiceID   AS INTEGER
    FIELD sourceRowID AS ROWID
    INDEX sourceRowID sourceRowID
    .
DEFINE TEMP-TABLE ttARCashLine NO-UNDO
    FIELD sequenceID       AS INTEGER
    FIELD invoiceID        AS CHARACTER
    FIELD checkNetAmount   AS DECIMAL
    FIELD checkGrossAmount AS DECIMAL
    FIELD invoiceDate      AS DATE
    FIELD discountAmount   AS DECIMAL
    FIELD accountNumber    AS CHARACTER
    INDEX sequenceID sequenceID
    .
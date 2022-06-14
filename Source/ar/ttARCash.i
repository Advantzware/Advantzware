DEFINE TEMP-TABLE ttARCash NO-UNDO
    FIELD sequenceID    AS INTEGER
    FIELD company       AS CHARACTER
    FIELD checkNumber   AS CHARACTER
    FIELD customerID    AS CHARACTER
    FIELD checkDate     AS DATE
    FIELD checkAmount   AS DECIMAL
    FIELD currency      AS CHARACTER
    FIELD payorID       AS CHARACTER
    FIELD paymentMethod AS CHARACTER
    FIELD creditDebit   AS CHARACTER
    FIELD bankCode      AS CHARACTER
    FIELD importType    AS CHARACTER
    INDEX checkNumber company checkNumber customerID
    .
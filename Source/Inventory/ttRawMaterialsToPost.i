DEFINE TEMP-TABLE ttRawMaterialsToPost NO-UNDO 
    FIELD parentRowID AS ROWID
    FIELD rmRctdRowID AS ROWID
    FIELD ritaCode    AS CHARACTER
    FIELD itemID      AS CHARACTER
    FIELD poID        AS CHARACTER
    FIELD formNo      AS INTEGER
    FIELD quantity    AS DECIMAL 
    FIELD vendorTag   AS CHARACTER
    FIELD sequenceID  AS INTEGER
    FIELD processed   AS LOGICAL
    .
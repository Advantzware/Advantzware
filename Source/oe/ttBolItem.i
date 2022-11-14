DEFINE TEMP-TABLE ttBolItem NO-UNDO
    FIELD riBolitm      AS ROWID
    FIELD company       AS CHARACTER
    FIELD locationID    AS CHARACTER
    FIELD bolID         AS INTEGER 
    FIELD lineID        AS INTEGER
    FIELD partID        AS CHARACTER
    FIELD custPO        AS CHARACTER
    FIELD itemID        AS CHARACTER
    FIELD itemPartDesc  AS CHARACTER
    FIELD itemName      AS CHARACTER
    FIELD ItemUnit      AS INTEGER
    FIELD ItemQtyUnit   AS INTEGER
    FIELD ItemPC        AS CHARACTER
    FIELD ItemWeight    AS DECIMAL 
    FIELD deptNotes     AS CHARACTER EXTENT 100 
    FIELD order         AS CHARACTER
    FIELD bolQty        AS INTEGER
    FIELD bolSummaryQty AS INTEGER
    FIELD ordSummaryQty AS INTEGER
    FIELD bolSummPart   AS CHARACTER
    FIELD totalPkgs     AS INTEGER
    FIELD firstLineItem AS LOGICAL
    FIELD lastLineItem  AS LOGICAL 
    FIELD cLotNo        AS CHARACTER
    FIELD cRefnum       AS CHARACTER
    FIELD cRecKey       AS CHARACTER
    .

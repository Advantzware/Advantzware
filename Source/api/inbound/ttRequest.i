DEFINE TEMP-TABLE ttRequest
    FIELD fieldOrder  AS INTEGER
    FIELD fieldName   AS CHARACTER
    FIELD fieldValue  AS CHARACTER
    FIELD fieldParent AS INTEGER
    INDEX fieldOrder IS PRIMARY UNIQUE fieldOrder
    INDEX fieldName fieldName
    .

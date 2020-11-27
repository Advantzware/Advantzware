DEFINE TEMP-TABLE ttInboundRequest NO-UNDO
    FIELD apiRoute        AS CHARACTER
    FIELD requestVerb     AS CHARACTER
    FIELD requestData     AS CLOB
    FIELD responseData    AS CLOB
    FIELD requestDataType AS CHARACTER
    FIELD requestTime     AS CHARACTER 
    FIELD exception       AS CHARACTER 
    FIELD eventID         AS INTEGER
    FIELD processed       AS LOGICAL
    FIELD success         AS LOGICAL
    .

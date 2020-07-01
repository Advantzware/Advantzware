DEFINE TEMP-TABLE ttScopes NO-UNDO
    FIELD company         AS CHARACTER  
    FIELD apiID           AS CHARACTER 
    FIELD clientID        AS CHARACTER 
    FIELD triggerID       AS CHARACTER
    FIELD scopeID         AS CHARACTER 
    FIELD scopeType       AS CHARACTER
    FIELD vendorID        AS CHARACTER
    FIELD customerID      AS CHARACTER
    FIELD shipToID        AS CHARACTER 
    FIELD inactive        AS LOGICAL    
    FIELD riApiClientXref AS ROWID
    . 
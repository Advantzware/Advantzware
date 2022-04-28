DEFINE TEMP-TABLE ttQuoteHeader NO-UNDO
    FIELD riQuotehd  AS ROWID
    FIELD company    AS CHARACTER
    FIELD locationID AS CHARACTER
    FIELD quoteID    AS INTEGER 
    FIELD estimateID AS CHARACTER
    FIELD customerID AS CHARACTER
    FIELD quoteDate  AS DATE
    FIELD expireDate AS DATE
    FIELD salesMan   AS CHARACTER
    FIELD terms      AS CHARACTER
    FIELD carrier    AS CHARACTER
    .
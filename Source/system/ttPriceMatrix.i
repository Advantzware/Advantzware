DEFINE TEMP-TABLE ttPriceMatrix NO-UNDO 
    FIELD company      AS CHARACTER
    FIELD itemID       AS CHARACTER 
    FIELD custID       AS CHARACTER
    FIELD shiptoID     AS CHARACTER
    FIELD level        AS INTEGER
    FIELD quantity     AS DECIMAL 
    FIELD quantityUOM  AS CHARACTER 
    FIELD targetUOM    AS CHARACTER
    FIELD price        AS DECIMAL
    FIELD priceUOM     AS CHARACTER
    .

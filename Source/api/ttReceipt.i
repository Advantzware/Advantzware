
/*------------------------------------------------------------------------
    File        : api/ttReceipt.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Thu Sep 02 14:16:59 IST 2021
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttReceipt NO-UNDO
    FIELD lineID       AS INTEGER 
    FIELD company      AS CHARACTER
    FIELD location     AS CHARACTER
    FIELD poID         AS INTEGER
    FIELD poLine       AS INTEGER
    FIELD jobID        AS CHARACTER
    FIELD jobID2       AS INTEGER
    FIELD itemID       AS CHARACTER
    FIELD itemName     AS CHARACTER
    FIELD quantity     AS DECIMAL
    FIELD quantityUOM  AS CHARACTER    
    FIELD rcpthRowID   AS ROWID
    INDEX poID IS PRIMARY company poID poLine
    .


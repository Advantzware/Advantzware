
/*------------------------------------------------------------------------
    File        : ttReleaseTag.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : DEVA$!
    Created     : Mon Feb 15 02:51:00 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttReleaseTag NO-UNDO
    FIELD company                  AS CHARACTER   LABEL "Company"             FORMAT "X(3)"
    FIELD releaseID                AS INTEGER     LABEL "Release #"           FORMAT ">>>>>>>>9"
    FIELD tag                      AS CHARACTER   LABEL "Tag #"               FORMAT "X(21)"
    FIELD itemID                   AS CHARACTER   LABEL "Item #"              FORMAT "x(15)"
    FIELD itemName                 AS CHARACTER   LABEL "Item Name"           FORMAT "x(30)"
    FIELD orderID                  AS INTEGER     LABEL "Order #"             FORMAT ">>>>>>>>9"
    FIELD jobID                    AS CHARACTER   LABEL "Job #"               FORMAT "X(6)"
    FIELD jobID2                   AS INTEGER     LABEL "Run"                 FORMAT ">9"
    FIELD location                 AS CHARACTER   LABEL "Location"            FORMAT "X(5)"
    FIELD bin                      AS CHARACTER   LABEL "Bin"                 FORMAT "X(20)"
    FIELD customerID               AS CHARACTER   LABEL "Customer #"          FORMAT "X(30)"
    FIELD quantityOfSubUnits       AS INTEGER     LABEL "Cases"               FORMAT ">>>>>>>>9"
    FIELD quantityInSubUnit        AS INTEGER     LABEL "Case Quantity"       FORMAT ">>>>>>>>9"
    FIELD quantityOfSubUnitsInUnit AS INTEGER     LABEL "Cases Per Pallet"    FORMAT ">>>>>>>>9"
    FIELD quantityPartial          AS INTEGER     LABEL "Partial"             FORMAT ">>>>>>>>9"  
    FIELD quantity                 AS INTEGER     LABEL "Quantity"            FORMAT ">>>>>>>>9"
    FIELD quantityTotal            AS INTEGER     LABEL "Total Quantity"      FORMAT ">>>>>>>>9"
    FIELD lineID                   AS INTEGER     LABEL "Line #"              FORMAT ">>9"     
    FIELD sequenceID               AS INTEGER     LABEL "Sequence"            FORMAT ">>>>>>>>9"
    FIELD custPoNo                 AS CHARACTER   LABEL "Customer PO #"       FORMAT "X(15)"
    FIELD trailerID                AS CHARACTER   LABEL "Trailer #"           FORMAT "X(20)"
    FIELD sourceRowID              AS ROWID
    INDEX releaseID company releaseID orderID itemID custPoNo
    .

/*------------------------------------------------------------------------
    File        : ttReleaseItem.i
    Purpose     : 

    Syntax      :

    Description : Stores the release items quantities

    Author(s)   : DEVA$!
    Created     : Sun Feb 14 06:31:41 EST 2021
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttReleaseItem NO-UNDO
    FIELD company                   AS CHARACTER   LABEL "Company"                    FORMAT "X(3)"
    FIELD releaseID                 AS INTEGER     LABEL "Release #"                  FORMAT ">>>>>>>>9"
    FIELD orderID                   AS INTEGER     LABEL "Order #"                    FORMAT ">>>>>>>>9"
    FIELD itemID                    AS CHARACTER   LABEL "Item #"                     FORMAT "X(40)"
    FIELD itemName                  AS CHARACTER   LABEL "Item Name"                  FORMAT "X(200)"
    FIELD quantityRelease           AS DECIMAL     LABEL "Release Quantity"           FORMAT "->>>>>>>>9.99<<<<"
    FIELD quantityScanned           AS DECIMAL     LABEL "Scanned Quantity"           FORMAT "->>>>>>>>9.99<<<<"
    FIELD quantityOnHand            AS DECIMAL     LABEL "On Hand Quantity"           FORMAT "->>>>>>>>9.99<<<<"
    FIELD quantityOfUnitsRelease    AS INTEGER     LABEL "Total Release Units"        FORMAT ">>>>>>>>9"
    FIELD quantityOfUnitsScanned    AS INTEGER     LABEL "Total Scanned Units"        FORMAT ">>>>>>>>9"
    FIELD jobID                     AS CHARACTER   LABEL "Job #"                      FORMAT "X(6)"
    FIELD jobID2                    AS INTEGER     LABEL "Run"                        FORMAT ">>9"
    FIELD customerPO                AS CHARACTER   LABEL "Customer PO #"              FORMAT "X(15)"
    FIELD quantityReceivedJob       AS DECIMAL     LABEL "Job Quantity Received"      FORMAT "->>>>>>>>9.99<<<<"
    FIELD quantityReceivedPO        AS DECIMAL     LABEL "PO Quantity Received"       FORMAT "->>>>>>>>9.99<<<<"
    FIELD underRunPercent           AS DECIMAL     LABEL "Under Run %"                FORMAT ">>9.99"
    FIELD overRunPercent            AS DECIMAL     LABEL "Over Run %"                 FORMAT ">>9.99"
    INDEX idxRelease company releaseID 
    INDEX idxItem company releaseID itemID
    .

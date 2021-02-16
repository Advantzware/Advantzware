
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
    FIELD quantityRelease           AS DECIMAL     LABEL "Release Quantity"           FORMAT ">>>>>>>>9.99<<<<"
    FIELD quantityScanned           AS DECIMAL     LABEL "Scanned Quantity"           FORMAT ">>>>>>>>9.99<<<<"
    FIELD quantityOnHand            AS DECIMAL     LABEL "On Hand Quantity"           FORMAT ">>>>>>>>9.99<<<<"
    FIELD quantityOfUnitsRelease    AS INTEGER     LABEL "Total Release Units"        FORMAT ">>>>>>>>9"
    FIELD quantityOfUnitsScanned    AS INTEGER     LABEL "Total Scanned Units"        FORMAT ">>>>>>>>9"
    INDEX idxRelease company releaseID 
    INDEX idxItem company releaseID itemID
    .

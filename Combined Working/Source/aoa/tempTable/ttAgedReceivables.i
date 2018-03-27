/* ttAgedReceivables.i */

/* Aged Receivables.rpa */
DEFINE TEMP-TABLE ttAgedReceivables NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD custNo        AS CHARACTER LABEL "Customer"        FORMAT "x(8)" 
    FIELD custName      AS CHARACTER LABEL "Customer Name"   FORMAT "x(30)"
    FIELD contact       AS CHARACTER LABEL "Contact"         FORMAT "x(15)"
    FIELD salesRep      AS CHARACTER LABEL "Sales Rep"       FORMAT "x(25)"
    FIELD terms         AS CHARACTER LABEL "Terms"           FORMAT "x(8)"
    FIELD address1      AS CHARACTER LABEL "Address 1"       FORMAT "x(30)"
    FIELD address2      AS CHARACTER LABEL "Address 2"       FORMAT "x(30)"
    FIELD city          AS CHARACTER LABEL "City"            FORMAT "x(15)"
    FIELD state         AS CHARACTER LABEL "State"           FORMAT "x(5)"
    FIELD zip           AS CHARACTER LABEL "Zip"             FORMAT "x(8)"
    FIELD creditLimit   AS DECIMAL   LABEL "Credit Lim"      FORMAT "->>,>>>,>>9.99"
    FIELD phone         AS CHARACTER LABEL "Phone"           FORMAT "x(13)"
    FIELD fax           AS CHARACTER LABEL "Fax"             FORMAT "x(12)"
    FIELD checkMemo     AS CHARACTER LABEL "Check/Memo"      FORMAT "x(10)"
    FIELD daysOld       AS INTEGER   LABEL "Days Old"        FORMAT "->>>>>>>9"
    FIELD vType         AS CHARACTER LABEL "Type"            FORMAT "x(4)"
    FIELD invoiceNo     AS INTEGER   LABEL "Invoice"         FORMAT ">>>>>>>>9"
    FIELD invoiceDate   AS DATE      LABEL "Inv Date"        FORMAT 99/99/9999
    FIELD amount        AS DECIMAL   LABEL "Amount"          FORMAT "->,>>>,>>>,>>9.99"
    FIELD vCurrent      AS DECIMAL   LABEL "Current"         FORMAT "->,>>>,>>>,>>9.99"
    FIELD adtp          AS INTEGER   LABEL "Adtp"            FORMAT "->,>>9"
    FIELD td            AS INTEGER   LABEL "Td"              FORMAT "->,>>9"
    FIELD periodDay1    AS DECIMAL   LABEL "Period Days 1"   FORMAT "->,>>>,>>>,>>9.99"
    FIELD periodDay2    AS DECIMAL   LABEL "Period Days 2"   FORMAT "->,>>>,>>>,>>9.99"
    FIELD periodDay3    AS DECIMAL   LABEL "Period Days 3"   FORMAT "->,>>>,>>>,>>9.99"
    FIELD custPoNo      AS CHARACTER LABEL "Customer PoNo"   FORMAT "x(15)"
    FIELD jobNo         AS CHARACTER LABEL "Job No "         FORMAT "x(9)"
    FIELD invoiceNote   AS CHARACTER LABEL "Invoice Note"    FORMAT "x(500)"
    FIELD collNote      AS CHARACTER LABEL "Collection Note" FORMAT "x(500)"
    FIELD xxSort1       AS CHARACTER LABEL "Sort 1"          FORMAT "x(100)"
    FIELD xxSort2       AS INTEGER   LABEL "Sort 2"          FORMAT ">>>>>>>>>9"
        INDEX ttAgedReceivables IS PRIMARY rowType xxSort1 xxSort2
    .

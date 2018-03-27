/* ttCustomerItemByMonth.i */

/* Customer Item By Month.rpa */
DEFINE TEMP-TABLE ttCustomerItemByMonth NO-UNDO 
    {aoa/tempTable/ttFields.i}
    FIELD xxOrder       AS INTEGER   LABEL "Order"            FORMAT ">>>,>>9"
    FIELD custNo        AS CHARACTER LABEL "Customer"         FORMAT "X(8)"
    FIELD custName      AS CHARACTER LABEL "Customer Name"    FORMAT "X(30)"
    FIELD itemNo        AS CHARACTER LABEL "FG Item"          FORMAT "X(15)"
    FIELD itemDscr      AS CHARACTER LABEL "FG Description"   FORMAT "X(30)"
    FIELD custPart      AS CHARACTER LABEL "Customer Part"    FORMAT "X(15)"
    FIELD invTotal1     AS CHARACTER LABEL "Invoice Total 1"  FORMAT "X(15)"
    FIELD qtyInvoiced1  AS CHARACTER LABEL "Qty Invoiced 1"   FORMAT "X(15)"
    FIELD qtyShipped1   AS CHARACTER LABEL "Qty Shipped 1"    FORMAT "X(15)"
    FIELD invTotal2     AS CHARACTER LABEL "Invoice Total 2"  FORMAT "X(15)"
    FIELD qtyInvoiced2  AS CHARACTER LABEL "Qty Invoiced 2"   FORMAT "X(15)"
    FIELD qtyShipped2   AS CHARACTER LABEL "Qty Shipped 2"    FORMAT "X(15)"
    FIELD invTotal3     AS CHARACTER LABEL "Invoice Total 3"  FORMAT "X(15)"
    FIELD qtyInvoiced3  AS CHARACTER LABEL "Qty Invoiced 3"   FORMAT "X(15)"
    FIELD qtyShipped3   AS CHARACTER LABEL "Qty Shipped 3"    FORMAT "X(15)"
    FIELD invTotal4     AS CHARACTER LABEL "Invoice Total 4"  FORMAT "X(15)"
    FIELD qtyInvoiced4  AS CHARACTER LABEL "Qty Invoiced 4"   FORMAT "X(15)"
    FIELD qtyShipped4   AS CHARACTER LABEL "Qty Shipped 4"    FORMAT "X(15)"
    FIELD invTotal5     AS CHARACTER LABEL "Invoice Total 5"  FORMAT "X(15)"
    FIELD qtyInvoiced5  AS CHARACTER LABEL "Qty Invoiced 5"   FORMAT "X(15)"
    FIELD qtyShipped5   AS CHARACTER LABEL "Qty Shipped 5"    FORMAT "X(15)"
    FIELD invTotal6     AS CHARACTER LABEL "Invoice Total 6"  FORMAT "X(15)"
    FIELD qtyInvoiced6  AS CHARACTER LABEL "Qty Invoiced 6"   FORMAT "X(15)"
    FIELD qtyShipped6   AS CHARACTER LABEL "Qty Shipped 6"    FORMAT "X(15)"
    FIELD invTotal7     AS CHARACTER LABEL "Invoice Total 7"  FORMAT "X(15)"
    FIELD qtyInvoiced7  AS CHARACTER LABEL "Qty Invoiced 7"   FORMAT "X(15)"
    FIELD qtyShipped7   AS CHARACTER LABEL "Qty Shipped 7"    FORMAT "X(15)"
    FIELD invTotal8     AS CHARACTER LABEL "Invoice Total 8"  FORMAT "X(15)"
    FIELD qtyInvoiced8  AS CHARACTER LABEL "Qty Invoiced 8"   FORMAT "X(15)"
    FIELD qtyShipped8   AS CHARACTER LABEL "Qty Shipped 8"    FORMAT "X(15)"
    FIELD invTotal9     AS CHARACTER LABEL "Invoice Total 9"  FORMAT "X(15)"
    FIELD qtyInvoiced9  AS CHARACTER LABEL "Qty Invoiced 9"   FORMAT "X(15)"
    FIELD qtyShipped9   AS CHARACTER LABEL "Qty Shipped 9"    FORMAT "X(15)"
    FIELD invTotal10    AS CHARACTER LABEL "Invoice Total 10" FORMAT "X(15)"
    FIELD qtyInvoiced10 AS CHARACTER LABEL "Qty Invoiced 10"  FORMAT "X(15)"
    FIELD qtyShipped10  AS CHARACTER LABEL "Qty Shipped 10"   FORMAT "X(15)"
    FIELD invTotal11    AS CHARACTER LABEL "Invoice Total 11" FORMAT "X(15)"
    FIELD qtyInvoiced11 AS CHARACTER LABEL "Qty Invoiced 11"  FORMAT "X(15)"
    FIELD qtyShipped11  AS CHARACTER LABEL "Qty Shipped 11"   FORMAT "X(15)"
    FIELD invTotal12    AS CHARACTER LABEL "Invoice Total 12" FORMAT "X(15)"
    FIELD qtyInvoiced12 AS CHARACTER LABEL "Qty Invoiced 12"  FORMAT "X(15)"
    FIELD qtyShipped12  AS CHARACTER LABEL "Qty Shipped 12"   FORMAT "X(15)"
    FIELD invTotal13    AS CHARACTER LABEL "Invoice Total 13" FORMAT "X(15)"
    FIELD qtyInvoiced13 AS CHARACTER LABEL "Qty Invoiced 13"  FORMAT "X(15)"
    FIELD qtyShipped13  AS CHARACTER LABEL "Qty Shipped 13"   FORMAT "X(15)"
    FIELD invTotal14    AS CHARACTER LABEL "Invoice Total 14" FORMAT "X(15)"
    FIELD qtyInvoiced14 AS CHARACTER LABEL "Qty Invoiced 14"  FORMAT "X(15)"
    FIELD qtyShipped14  AS CHARACTER LABEL "Qty Shipped 14"   FORMAT "X(15)"
    FIELD invTotal15    AS CHARACTER LABEL "Invoice Total 15" FORMAT "X(15)"
    FIELD qtyInvoiced15 AS CHARACTER LABEL "Qty Invoiced 15"  FORMAT "X(15)"
    FIELD qtyShipped15  AS CHARACTER LABEL "Qty Shipped 15"   FORMAT "X(15)"
    FIELD invTotal16    AS CHARACTER LABEL "Invoice Total 16" FORMAT "X(15)"
    FIELD qtyInvoiced16 AS CHARACTER LABEL "Qty Invoiced 16"  FORMAT "X(15)"
    FIELD qtyShipped16  AS CHARACTER LABEL "Qty Shipped 16"   FORMAT "X(15)"
    FIELD invTotal17    AS CHARACTER LABEL "Invoice Total 17" FORMAT "X(15)"
    FIELD qtyInvoiced17 AS CHARACTER LABEL "Qty Invoiced 17"  FORMAT "X(15)"
    FIELD qtyShipped17  AS CHARACTER LABEL "Qty Shipped 17"   FORMAT "X(15)"
    FIELD invTotal18    AS CHARACTER LABEL "Invoice Total 18" FORMAT "X(15)"
    FIELD qtyInvoiced18 AS CHARACTER LABEL "Qty Invoiced 18"  FORMAT "X(15)"
    FIELD qtyShipped18  AS CHARACTER LABEL "Qty Shipped 18"   FORMAT "X(15)"
    FIELD invTotal19    AS CHARACTER LABEL "Invoice Total 19" FORMAT "X(15)"
    FIELD qtyInvoiced19 AS CHARACTER LABEL "Qty Invoiced 19"  FORMAT "X(15)"
    FIELD qtyShipped19  AS CHARACTER LABEL "Qty Shipped 19"   FORMAT "X(15)"
    FIELD invTotal20    AS CHARACTER LABEL "Invoice Total 20" FORMAT "X(15)"
    FIELD qtyInvoiced20 AS CHARACTER LABEL "Qty Invoiced 20"  FORMAT "X(15)"
    FIELD qtyShipped20  AS CHARACTER LABEL "Qty Shipped 20"   FORMAT "X(15)"
    FIELD invTotal21    AS CHARACTER LABEL "Invoice Total 21" FORMAT "X(15)"
    FIELD qtyInvoiced21 AS CHARACTER LABEL "Qty Invoiced 21"  FORMAT "X(15)"
    FIELD qtyShipped21  AS CHARACTER LABEL "Qty Shipped 21"   FORMAT "X(15)"
    FIELD invTotal22    AS CHARACTER LABEL "Invoice Total 22" FORMAT "X(15)"
    FIELD qtyInvoiced22 AS CHARACTER LABEL "Qty Invoiced 22"  FORMAT "X(15)"
    FIELD qtyShipped22  AS CHARACTER LABEL "Qty Shipped 22"   FORMAT "X(15)"
    FIELD invTotal23    AS CHARACTER LABEL "Invoice Total 23" FORMAT "X(15)"
    FIELD qtyInvoiced23 AS CHARACTER LABEL "Qty Invoiced 23"  FORMAT "X(15)"
    FIELD qtyShipped23  AS CHARACTER LABEL "Qty Shipped 23"   FORMAT "X(15)"
    FIELD invTotal24    AS CHARACTER LABEL "Invoice Total 24" FORMAT "X(15)"
    FIELD qtyInvoiced24 AS CHARACTER LABEL "Qty Invoiced 24"  FORMAT "X(15)"
    FIELD qtyShipped24  AS CHARACTER LABEL "Qty Shipped 24"   FORMAT "X(15)"
    FIELD invTotal25    AS CHARACTER LABEL "Invoice Total 25" FORMAT "X(15)"
    FIELD qtyInvoiced25 AS CHARACTER LABEL "Qty Invoiced 25"  FORMAT "X(15)"
    FIELD qtyShipped25  AS CHARACTER LABEL "Qty Shipped 25"   FORMAT "X(15)"
    FIELD invTotal26    AS CHARACTER LABEL "Invoice Total 26" FORMAT "X(15)"
    FIELD qtyInvoiced26 AS CHARACTER LABEL "Qty Invoiced 26"  FORMAT "X(15)"
    FIELD qtyShipped26  AS CHARACTER LABEL "Qty Shipped 26"   FORMAT "X(15)"
    FIELD invTotal27    AS CHARACTER LABEL "Invoice Total 27" FORMAT "X(15)"
    FIELD qtyInvoiced27 AS CHARACTER LABEL "Qty Invoiced 27"  FORMAT "X(15)"
    FIELD qtyShipped27  AS CHARACTER LABEL "Qty Shipped 27"   FORMAT "X(15)"
        INDEX ttShipmentReport IS PRIMARY xxOrder
        .

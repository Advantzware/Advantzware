/* ttEDI810Exception.i */

/* EDI 810 Exception.rpa */
DEFINE TEMP-TABLE ttEDI810Exception NO-UNDO
{aoa/tempTable/ttFields.i}
    FIELD custCode          AS CHARACTER LABEL "Customer" FORMAT "x(8)"
    FIELD custName          AS CHARACTER LABEL "Name"     FORMAT "x(30)"
    FIELD invoiceNo         AS INTEGER   LABEL "Invoice"  FORMAT ">>>>>>>9"
    FIELD invoiceDate       AS DATE      LABEL "Inv Date" FORMAT "99/99/9999"
    FIELD statusDescription AS CHARACTER LABEL "Status"   FORMAT "x(30)"
    FIELD xxID              AS INTEGER   LABEL "ID"       FORMAT ">>>>>9" INITIAL 999999
        INDEX ID IS PRIMARY xxID
        .

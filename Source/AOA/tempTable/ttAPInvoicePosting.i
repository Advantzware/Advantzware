/* ttAPInvoicePosting.i */

/* AP Invoice Posting.rpa */
DEFINE TEMP-TABLE ttAPInvoicePosting NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD vendNo   AS CHARACTER LABEL "Vendor No"   FORMAT "x(6)"
    FIELD vendName AS CHARACTER LABEL "Vendor Name" FORMAT "x(30)"
    FIELD invNo    AS CHARACTER LABEL "Invoice"     FORMAT "x(8)"
    FIELD invDate  AS DATE      LABEL "Inv Date"    FORMAT "99/99/9999"
    FIELD dueDate  AS DATE      LABEL "Due Date"    FORMAT "99/99/9999"
    FIELD amount   AS DECIMAL   LABEL "Amount"      FORMAT "->,>>>,>>9.99"
    FIELD xxID     AS INTEGER   LABEL "ID"          FORMAT ">>>>>9"
        INDEX ID IS PRIMARY
              xxID
    .

/* ttAPInvoicePostingGL.i */

/* AP Invoice Posting.rpa */
DEFINE TEMP-TABLE ttAPInvoicePostingGL NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD glAcct   AS CHARACTER LABEL "GL Account"     FORMAT "x(25)"
    FIELD glDscr   AS CHARACTER LABEL "GL Description" FORMAT "x(45)"
    FIELD glAmount AS DECIMAL   LABEL "GL Amount"      FORMAT "->,>>>,>>9.99"
    FIELD xxID     AS INTEGER   LABEL "ID"             FORMAT ">>>>>9" INITIAL 999999
        INDEX ID IS PRIMARY
              xxID
    .

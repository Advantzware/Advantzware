/* ttAPInvoicePostingMsg.i */

/* AP Invoice Posting.rpa */
DEFINE TEMP-TABLE ttAPInvoicePostingMsg NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD Msg  AS CHARACTER LABEL "Message" FORMAT "x(70)" 
    FIELD xxID AS INTEGER   LABEL "ID"      FORMAT ">>>>>9" INITIAL 999999
        INDEX ID IS PRIMARY
              xxID
    .

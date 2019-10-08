/* ttInvoiceHighlights.i */

/* Invoice Hightlights.edp */
DEFINE TEMP-TABLE ttInvoiceHighlights NO-UNDO
    {aoa/tempTable/ttFields.i}
    FIELD company      AS CHARACTER LABEL "Company"
    FIELD salesYear    AS INTEGER   LABEL "Year"
    FIELD dateIdx      AS DATE      LABEL "Date"
    FIELD lyrSales     AS DECIMAL   LABEL "LYR Sales"
    FIELD lyrCost      AS DECIMAL   LABEL "LYR Cost"
    FIELD lyrQty       AS DECIMAL   LABEL "LYR Qty"
    FIELD lyrMSF       AS DECIMAL   LABEL "LYR MSF"
    FIELD lyrTons      AS DECIMAL   LABEL "LYR Tons"
    FIELD lyrNetProfit AS DECIMAL   LABEL "LYR Net Profit"
    FIELD cyrSales     AS DECIMAL   LABEL "CYR Sales"
    FIELD cyrCost      AS DECIMAL   LABEL "CYR Cost"
    FIELD cyrQty       AS DECIMAL   LABEL "CYR Qty"
    FIELD cyrMSF       AS DECIMAL   LABEL "CYR MSF"
    FIELD cyrTons      AS DECIMAL   LABEL "CYR Tons"
    FIELD cyrNetProfit AS DECIMAL   LABEL "CYR Net Profit"
        INDEX idx company dateIdx ASCENDING
    .

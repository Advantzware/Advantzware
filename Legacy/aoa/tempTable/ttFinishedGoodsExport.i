/* ttFinishedGoodsExport.i */

/* Finished Goods Export.rpa */
DEFINE TEMP-TABLE ttFinishedGoodsExport NO-UNDO
    {aoa/tempTable/ttFields.i}
    /* add additional fields here */
    FIELD itemNo AS CHARACTER LABEL "Item No" FORMAT "x(30)"
    .

/* ttFormTempTables.i */

{ar/ttInvoice.i}

DEFINE TEMP-TABLE ttFormHeader NO-UNDO LIKE ttInv
    {AOA/tempTable/ttFields.i}
    .
DEFINE TEMP-TABLE ttFormDetail NO-UNDO LIKE ttInvLIne
    {AOA/tempTable/ttFields.i}
    .
DEFINE TEMP-TABLE ttFormSummary NO-UNDO LIKE ttTaxDetail
    {AOA/tempTable/ttFields.i}
    .

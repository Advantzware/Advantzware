/* ---------------------------------------------- oe/rep/relprogn.p  */
/* Print oe Release/Picking tickets for Protagon PremierX Xprint              */
/* ------------------------------------------------------------------*/
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relprogn.i "oe-relh.r-no"}

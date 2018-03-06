/* ---------------------------------------------- oe/rep/relcsc.p  */
/* Print oe Release/Picking tickets for PremierX Xprint              */
/* ------------------------------------------------------------------*/
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relcsc.i "oe-relh.r-no"}

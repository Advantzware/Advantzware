/* ---------------------------------------------- oe/rep/relfibx.p  3/05 YSK */
/* Print oe Release/Picking tickets     for Fibre Xprint                     */
/* ------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relfibx.i "oe-relh.r-no"}

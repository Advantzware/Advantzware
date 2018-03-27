/* ---------------------------------------------- oe/rep/relsonoc.p  1/05 YSK */
/* Print oe Release/Picking tickets     for Sonoco                                      */
/* -------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relsonoc.i "oe-relh.r-no" }

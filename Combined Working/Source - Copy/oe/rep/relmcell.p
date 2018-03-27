/* ---------------------------------------------- oe/rep/relmcell.p   */
/* Print oe Release/Picking tickets     for Multicell Xprint                    */
/* -------------------------------------------------------------------------- */


DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relmcell.i "oe-relh.r-no"}

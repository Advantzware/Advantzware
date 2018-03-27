/* ---------------------------------------------- oe/rep/relcardx.p   */
/* Print oe Release/Picking tickets     for CardedX Xprint                    */
/* -------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .


{oe/rep/relcardx.i "oe-relh.r-no"}

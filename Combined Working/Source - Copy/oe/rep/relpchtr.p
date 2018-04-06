/* ---------------------------------------------- oe/rep/relpchtr.p   */
/* Print oe Release/Picking tickets     for Peachtree Xprint                    */
/* -------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .


{oe/rep/relpchtr.i "oe-relh.r-no"}

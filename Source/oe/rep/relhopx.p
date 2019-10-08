/* ---------------------------------------------- oe/rep/relhopx.p  3/05 YSK */
/* Print oe Release/Picking tickets     for HOP Xprint                        */
/* -------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .


{oe/rep/relhopx.i "oe-relh.r-no"}

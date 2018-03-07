/* ---------------------------------------------- oe/rep/relpacif.p  10/02 YSK */
/* Print oe Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .
{oe/rep/relpacif.i "oe-relh.r-no"}

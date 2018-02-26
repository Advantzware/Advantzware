/* ---------------------------------------------- oe/rep/relppi.p  11/05 YSK */
/* Print oe Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relppi.i "oe-relh.r-no"}

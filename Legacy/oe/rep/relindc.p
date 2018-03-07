/* ---------------------------------------------- oe/rep/relindc.p  1/05 YSK */
/* Print oe Release/Picking tickets     for Indiana Carton                                      */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relindc.i "oe-relh.r-no"}

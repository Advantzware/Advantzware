/* ---------------------------------------------- oe/rep/relrfc.p  08/08/2018 YSK */
/* Print oe Release/Picking tickets     for RFC                        */
/* -------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
    .



{oe/rep/relrfc.i "oe-relh.r-no"}

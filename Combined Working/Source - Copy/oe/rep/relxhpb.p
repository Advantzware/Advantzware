/* ---------------------------------------------- oe/rep/relxhpb.p            */
/* Print oe Release/Picking tickets                                           */
/* -------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relxhpb.i "oe-relh.r-no"}

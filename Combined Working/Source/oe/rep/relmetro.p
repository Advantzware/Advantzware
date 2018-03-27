/* ---------------------------------------------- oe/rep/relmetro.p  2/13 BPV */
/* Print oe Release/Picking tickets     for Metro                     */
/* ------------------------------------------------------------------------- */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relmetro.i "oe-relh.r-no"}

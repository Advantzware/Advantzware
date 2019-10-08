/* ---------------------------------------------- oe/rep/relStClair.p  */
/* Print oe Release/Picking tickets for St Clair Xprint Landscape      */
/* ------------------------------------------------------------------*/

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relmidwest.i "oe-relh.r-no"}

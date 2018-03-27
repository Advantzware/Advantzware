/* ------------------------------------------ oe/rep/relrosmr.p 10080912 GDM */
/* RELEASE PRINT  Program for N-K-1 RELPRINT = Rosmar                        */
/* ------------------------------------------------------------------------- */
DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relrosmr.i "oe-relh.r-no" }

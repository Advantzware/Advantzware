/* ------------------------------------------ oe/rep/relloyl.p 09220907 GDM */
/* RELEASE PRINT  Program for N-K-1 RELPRINT = LoyLang                      */
/* ------------------------------------------------------------------------ */

DEFINE TEMP-TABLE ttReleasesToPrint NO-UNDO
    FIELD OeRelHRowID AS ROWID 
    FIELD SessionID   AS CHARACTER
        .

{oe/rep/relloyl.i "oe-relh.r-no"}

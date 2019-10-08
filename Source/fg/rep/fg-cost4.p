/*****************************************************************************
PROGRAM: fg/rep/fg-cost4.p
PURPOSE:

 AUTHOR:
  NOTES:
 ****************************************************************************/
 
 DEFINE INPUT PARAMETER inExcelheader  AS CHARACTER  NO-UNDO.
 DEFINE INPUT PARAMETER inFi_file      AS CHARACTER  NO-UNDO.

 DEFINE VARIABLE logExcelDump AS LOGICAL    NO-UNDO.

 DEFINE VARIABLE chrCust AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE chrINo  AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE chrPart AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE chrName AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE chrTag  AS CHARACTER  NO-UNDO.
 DEFINE VARIABLE decCost AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE decTot  AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE decExt  AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE decGsl  AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE decGsm  AS DECIMAL    NO-UNDO.
 DEFINE VARIABLE decAll  AS DECIMAL    NO-UNDO.

 {sys/inc/var.i shared}

 {fg/rep/fg-ibtg1.i "shared"}

 logExcelDump = FALSE.
 IF inExcelheader NE "" THEN DO:
   logExcelDump = TRUE.

   OUTPUT STREAM excel TO VALUE(inFi_file).
   PUT STREAM excel UNFORMATTED '"' REPLACE(inExcelheader,',','","') '"' SKIP.
 END.

 for each tt-itemfg use-index part-no no-lock,
   {fg/rep/fg-costs.i tt-itemfg.part-cust tt-itemfg.i-no}
 end.

 IF logExcelDump THEN DO:
   OUTPUT STREAM excel CLOSE.
 END.




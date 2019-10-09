/*****************************************************************************
PROGRAM: fg/rep/fg-cost1.p
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

 DEF VAR v-tag-no AS CHAR NO-UNDO.
 
DEF   SHARED VAR cDisplay AS cha NO-UNDO.
DEF   SHARED VAR cExcelDisplay AS cha NO-UNDO.
DEF   SHARED VAR hField AS HANDLE NO-UNDO.
DEF   SHARED VAR cTmpField AS CHA NO-UNDO.
DEF   SHARED VAR cVarValue AS cha NO-UNDO.
DEF   SHARED VAR cExcelVarValue AS cha NO-UNDO.
DEF   SHARED VAR cSelectedList AS cha NO-UNDO.
DEF   SHARED VAR cFieldName AS cha NO-UNDO.
DEF   SHARED VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF   SHARED VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF   SHARED VAR str-line AS cha FORM "x(300)" NO-UNDO.
DEF   SHARED VAR v-row-id AS ROWID NO-UNDO.
DEF SHARED VAR cslist AS cha NO-UNDO.
DEF SHARED VAR cTextListToSelect AS cha NO-UNDO.
DEF SHARED VAR cFieldListToSelect AS cha NO-UNDO.
DEF SHARED VAR cFieldLength AS cha NO-UNDO.
DEF SHARED VAR cFieldType AS cha NO-UNDO.
DEF SHARED VAR iColumnLength AS INT NO-UNDO.
 
{sys/inc/ttRptSel.i}

 logExcelDump = FALSE.
 IF inExcelheader NE "" THEN DO:
   logExcelDump = TRUE.

   OUTPUT STREAM excel TO VALUE(inFi_file).
   PUT STREAM excel UNFORMATTED '"' REPLACE(inExcelheader,',','","') '"' SKIP.
 END.  

 for each tt-itemfg use-index cust-no no-lock,
  {fg/rep/fg-cosNs.i tt-itemfg.cust-no tt-itemfg.i-no}
 end.

 IF logExcelDump THEN DO:
   OUTPUT STREAM excel CLOSE.
 END. 



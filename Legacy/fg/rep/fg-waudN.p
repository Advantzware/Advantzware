/* ---------------------------------------------- fg/rep/fg-waud.p 06/2011    */
/* FINISHED GOODS - INVENTORY ON HAND BY BIN / TAG by Item Number             */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

{fg/rep/fg-waud1.i "shared"}
{sys/inc/ttRptSel.i}
DEF SHARED VAR ldummy AS LOG NO-UNDO.
DEF SHARED VAR cTextListToSelect AS cha NO-UNDO.
DEF SHARED VAR cFieldListToSelect AS cha NO-UNDO.
DEF SHARED VAR cFieldLength AS cha NO-UNDO.
DEF SHARED VAR cFieldType AS cha NO-UNDO.
DEF SHARED VAR iColumnLength AS INT NO-UNDO.
DEF SHARED VAR cTextListToDefault AS cha NO-UNDO.
DEF SHARED  VAR cDisplay AS cha NO-UNDO.
DEF SHARED VAR cExcelDisplay AS cha NO-UNDO.
DEF SHARED VAR hField AS HANDLE NO-UNDO.
DEF SHARED VAR cTmpField AS CHA NO-UNDO.
DEF SHARED VAR cVarValue AS cha NO-UNDO.
DEF SHARED VAR cExcelVarValue AS cha NO-UNDO.
DEF SHARED VAR cSelectedList AS cha NO-UNDO.
DEF SHARED VAR cFieldName AS cha NO-UNDO.
DEF SHARED VAR str-tit4 AS cha FORM "x(200)" NO-UNDO.
DEF SHARED VAR str-tit5 AS cha FORM "x(200)" NO-UNDO.
DEF SHARED VAR str-line AS cha FORM "x(300)" NO-UNDO.

DEF VAR iRandom AS INT NO-UNDO.
DEF VAR iCount AS INT NO-UNDO.
DEF VAR iRecordNum AS INT NO-UNDO.

iRandom = random(1,100) .

for each tt-itemfg use-index i-no no-lock,
  {fg/rep/fg-waudN.i tt-itemfg.i-no tt-itemfg.case-count}
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */


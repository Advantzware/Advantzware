
{sys/inc/var.i SHARED}

{fgrep/r-fgcst1.i}

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

FOR EACH tt-itemfg USE-INDEX procat NO-LOCK,
  {fg/rep/fg-cst1N.i tt-itemfg.procat tt-itemfg.i-no}
END.


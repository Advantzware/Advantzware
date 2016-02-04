/* s-defs.i - rstark - 11.7.2013 */

DEF INPUT PARAM ip-batch-seq AS INT NO-UNDO.

{custom/globdefs.i}
{sys/inc/var.i NEW SHARED}

DEF VAR list-name AS CHAR NO-UNDO.
DEF VAR lv-font-no AS INT NO-UNDO.
DEF VAR lv-ornt AS cha NO-UNDO.
DEF VAR parm-fld-list AS CHAR NO-UNDO.
DEF VAR parm-lbl-list AS CHAR NO-UNDO.
DEF VAR parm-val-list AS CHAR NO-UNDO.
DEF VAR parm-var-list AS CHAR NO-UNDO.
DEF VAR tb_excel AS LOG NO-UNDO.
DEF VAR v-copies AS INT INIT 1 NO-UNDO.
DEF VAR v-prgmname AS CHAR NO-UNDO.
DEF VAR v-prt-name AS CHAR NO-UNDO.

ASSIGN
  v-prgmname = PROGRAM-NAME(1)
  cocode = g_company
  locode = g_loc
  .

&IF '{&s-name}' NE 's-fgohbb' &THEN
DEF STREAM excel.
&ENDIF

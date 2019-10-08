/* 
10.08.98 by CAH on \\ricky\rv8 Log#0000:
1.  Corrected undo no-undo status for variables and workfile.  The workfile
should undo so that the underlying totals are correct.
*/

DEF WORKFILE wksum /* 9810 CAH removed NO UNDO */
  field per       like gltran.per
  FIELD acct      LIKE gltran.acct
  FIELD jsc       LIKE gltran.jsc
  FIELD amount    LIKE gltran.amount.

DEF VAR distrib_summary   AS LOGICAL LABEL "Print Summary?" NO-UNDO
  INITIAL TRUE.

DEF VAR sum_abbrev  LIKE gljsc.abbrev FORMAT 'X(14)' EXTENT 7 NO-UNDO.
DEF VAR sum_amt     LIKE wksum.amount FORMAT '>,>>>,>>>.99CR' EXTENT 7
    NO-UNDO.
DEF VAR jsc_list    AS CHAR NO-UNDO.
DEF VAR i AS INTEGER NO-UNDO.
DEF VAR imax AS INTEGER NO-UNDO INITIAL 7.
DEF VAR h1 AS CHAR format 'x(04)' INITIAL " Per" NO-UNDO.
def var h2 as char format 'x(15)' initial "Account#" NO-UNDO.
def var ws_glsum_hdg as char no-undo format "x(40)" initial
    'SUMMARY BY ACCOUNT/JSC'.

FORM header
    skip(1)
  h1
  h2
  sum_abbrev[1 FOR 7] skip
  "----"
  "---------------"
  "--------------"
  "--------------"
  "--------------"
  "--------------"
  "--------------"
  "--------------"
  "--------------" skip
  WITH PAGE-TOP FRAME f-sumhdr WIDTH 132 NO-LABELS NO-BOX.

{1} /* 9810 CAH to make similar to gldstopt.i */
if login_group = "field" then distrib_summary = true.
else do:
UPDATE
  distrib_summary   COLON 20
  WITH FRAME f-opt CENTER SIDE-LABELS TITLE "OPTIONS" 1 COLUMN
  color value(c_pop).
end.  

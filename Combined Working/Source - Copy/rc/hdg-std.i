/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEMO\RC\HDG-STD.I
**       By: Chris Heins
** Descript: Standard Heading Form, 80 column report.
02/27/92 B. HEINS
1. Added {3} TO pass a NAMED STREAM FOR PAGE numbering.
**
*****************************************************************************
\***************************************************************************/


FORM HEADER
  SKIP(1)
  'RUN:' TODAY AT 5 STRING(TIME,'HH:MM:SS') AT 14
  hdg_name FORMAT 'x(30)' AT 25 'P/E:' AT 58
  hdg_perdate  FORMAT '99/99/99' AT 62 'PAGE:' AT 72
  PAGE-NUMBER{3} FORMAT '999' TO 80 SKIP
  ' BY:' USERID('dictdb') AT 5 hdg_desc FORMAT 'x(30)' AT 25
  'PRG:' AT 58 hdg_rpt_code AT 62 SKIP
  hdg_text FORMAT 'x(30)' AT 25 SKIP
  WITH PAGE-TOP NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 144 FRAME hdg-std.

assign hdg_rpt_code = "{1}" hdg_desc = "{2}".

/*
08.15.94 by CAH on B28@812 Log#0000:
1.  Added hdg_left and hdg_right to allow additional page common items.
06.27.93 by CH on TI4000:
1.  Added {3} to pass named stream for page-number function.
01.14.92 by ch:
1.  Increased size of hdg_name from 30 to 40
*/

FORM HEADER
  SKIP(1)
  "RUN:" TODAY "@" STRING(TIME,"HH:MM:SS") "BY:" USERID('dictdb')
    hdg_name format 'x(40)' AT 50 "P/E:" AT 95 hdg_perdate AT 100
    "PAGE:" AT 122 PAGE-NUMBER{3} FORMAT "999" SKIP
  "PRG:" AT 1 hdg_rpt_code AT 6
    hdg_desc format 'x(40)' AT 50 SKIP
  hdg_left format 'x(49)' at 1
    space(0) hdg_text format 'x(40)' AT 50
    space(0) hdg_right format 'x(30)' at 100 SKIP
WITH PAGE-TOP NO-BOX NO-LABELS NO-ATTR-SPACE WIDTH 132 FRAME HDG-STD.


assign hdg_rpt_code = "{1}" hdg_desc = "{2}".

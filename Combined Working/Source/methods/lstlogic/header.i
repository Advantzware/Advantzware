/* header.i */

DEFINE VARIABLE rpt_title AS CHARACTER FORMAT "x(36)" NO-UNDO.
DEFINE VARIABLE rpt_name AS CHARACTER FORMAT "x(20)" NO-UNDO.

FIND users WHERE users.user_id = USERID("NOSWEAT") NO-LOCK.

ASSIGN
  SUBSTR(rpt_title,16 - INT(LENGTH(bprgrms.prgtitle) / 2)) = bprgrms.prgtitle
  SUBSTR(rpt_name,10 - INT(LENGTH("Program: " + bprgrms.prgmname) / 2)) =
         "Program: " + bprgrms.prgmname.

FORMAT HEADER
  "Date:" AT 1
  TODAY FORMAT "99/99/9999" AT 7
  rpt_title AT 24
  "Page:" AT 62
  STRING(PAGE-NUMBER) AT 68
  "Time:" AT 1
  STRING(TIME,"HH:MM:SS") AT 7
  rpt_name AT 29
  "User:" AT 62
  users.user_id FORMAT "x(8)" AT 68
  FILL("-",80) FORMAT "x(80)" AT 1
    WITH FRAME f-header PAGE-TOP NO-BOX NO-LABELS WIDTH 132.

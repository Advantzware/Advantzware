/* splchkdn.p */

{methods/defines/hndldefs.i}
{methods/prgsecur.i}

{methods/nowait.i}
IF SEARCH("splchkup") = ? THEN
DO:
  MESSAGE "Background Spool Requests Monitor is not Running."
      VIEW-AS ALERT-BOX INFORMATION.
  RETURN.
END.
MESSAGE "Shutdown Background Spool Requests Monitor?"
  VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO
    UPDATE OKshutdown AS LOGICAL.
IF NOT OKshutdown THEN
RETURN.
OUTPUT TO splchkdn.
OUTPUT CLOSE.

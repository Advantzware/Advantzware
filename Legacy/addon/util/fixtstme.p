
SESSION:SET-WAIT-STATE ("general").

CONNECT -pf VALUE(".\addon\emptrack.pf") NO-ERROR.

IF CONNECTED("emptrack") THEN do:

  for each machemp EXCLUSIVE-LOCK:
      
    {custom/calctime.i &file="machemp"}
  end.

  DISCONNECT emptrack.
END.

SESSION:SET-WAIT-STATE ("").

MESSAGE "Fixing total times complete..." VIEW-AS ALERT-BOX.

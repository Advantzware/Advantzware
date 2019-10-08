
SESSION:SET-WAIT-STATE ("general").


  for each machemp EXCLUSIVE-LOCK:
    {custom/calctime.i &file="machemp"}
  end.


SESSION:SET-WAIT-STATE ("").

MESSAGE "Fixing total times complete..." VIEW-AS ALERT-BOX.

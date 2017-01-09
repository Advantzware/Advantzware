/* addon/touch/savenoteA.p   save notes to asi nosweat database */
  DEF INPUT PARAM ip-rec_key LIKE ASI.notes.rec_key.
  DEF INPUT PARAM ip-header_value AS cha.
  IF SEARCH("..\asinos.pf") EQ ? THEN DO:
    MESSAGE "A File is missing: ..\asinos.pf"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
  CONNECT -pf VALUE("..\asinos.pf") NO-ERROR. /* DBNAME asinos */
  IF NOT CONNECTED("asinos") THEN DO:
    MESSAGE "Could not connect to the nosweat database. Notes could not be copied"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  RUN touch/copynot2A.p (ip-rec_key, ip-header_value).

   /*IF connected("asinos") THEN */ 
  DISCONNECT asinos .

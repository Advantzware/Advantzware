/* addon/touch/savenoteA.p   save notes to asi nosweat database */
  DEF INPUT PARAM ip-rec_key LIKE nosweat.notes.rec_key.
  DEF INPUT PARAM ip-header_value AS cha.
  IF SEARCH("..\nosweat-asi.pf") EQ ? THEN DO:
    MESSAGE "A File is missing: ..\nosweat-asi.pf"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
    RETURN.
  END.
  CONNECT -pf VALUE("..\nosweat-asi.pf") NO-ERROR. /* DBNAME nosweat-asi */
  IF NOT CONNECTED("nosweat-asi") THEN DO:
    MESSAGE "Could not connect to the nosweat database. Notes could not be copied"
      VIEW-AS ALERT-BOX INFO BUTTONS OK.
  END.
  RUN touch/copynot2A.p (ip-rec_key, ip-header_value).

   /*IF connected("nosweat-asi") THEN */ 
  DISCONNECT nosweat-asi .

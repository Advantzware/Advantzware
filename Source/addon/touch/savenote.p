/* addon/touch/savenote.p   save notes to asi nosweat database */
  DEF INPUT PARAM ip-rec_key LIKE nosweat.notes.rec_key.
  
  CONNECT -pf VALUE("..\nosweat-asi.pf") NO-ERROR. /* DBNAME nosweat-asi */
     
  RUN touch/copynot2.p (ip-rec_key).

   /*IF connected("nosweat-asi") THEN */ 
  DISCONNECT nosweat-asi .

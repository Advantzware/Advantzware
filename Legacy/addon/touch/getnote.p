/* addon/touch/getnote.p   */
  DEF INPUT PARAM ip-rec_key LIKE nosweat.notes.rec_key.
  
  CONNECT -pf VALUE("..\nosweat-asi.pf") NO-ERROR. /* DBNAME nosweat-asi */
     
  RUN touch/copynote.p (ip-rec_key).

   /*IF connected("nosweat-asi") THEN */ 
  DISCONNECT nosweat-asi .

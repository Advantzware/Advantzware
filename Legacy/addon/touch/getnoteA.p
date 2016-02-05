/* addon/touch/getnoteA.p  2 IP param (reckey and header-value) */
  DEF INPUT PARAM ip-rec_key LIKE nosweat.notes.rec_key.
  DEF INPUT PARAM ip-header_value AS cha.
  
  CONNECT -pf VALUE("..\nosweat-asi.pf") NO-ERROR. /* DBNAME nosweat-asi */
     
  RUN touch/copynoteA.p (ip-rec_key, ip-header_value).

   /*IF connected("nosweat-asi") THEN */ 
  DISCONNECT nosweat-asi .

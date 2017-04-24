/* addon/touch/getnoteA.p  2 IP param (reckey and header-value) */
  DEF INPUT PARAM ip-rec_key LIKE ASI.notes.rec_key.
  DEF INPUT PARAM ip-header_value AS cha.

  CONNECT -pf VALUE("..\asinos.pf") NO-ERROR. /* DBNAME asinos */

  RUN touch/copynoteA.p (ip-rec_key, ip-header_value).

   /*IF connected("asinos") THEN */ 
  DISCONNECT asinos .

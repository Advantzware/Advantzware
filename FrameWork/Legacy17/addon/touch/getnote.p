/* addon/touch/getnote.p   */
  DEF INPUT PARAM ip-rec_key LIKE ASI.notes.rec_key.

  CONNECT -pf VALUE("..\asinos.pf") NO-ERROR. /* DBNAME asinos */

  RUN touch/copynote.p (ip-rec_key).

   /*IF connected("asinos") THEN */ 
  DISCONNECT asinos .

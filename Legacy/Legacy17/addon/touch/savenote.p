/* addon/touch/savenote.p   save notes to asi nosweat database */
  DEF INPUT PARAM ip-rec_key LIKE ASI.notes.rec_key.

  CONNECT -pf VALUE("..\asinos.pf") NO-ERROR. /* DBNAME asinos */

  RUN touch/copynot2.p (ip-rec_key).

   /*IF connected("asinos") THEN */ 
  DISCONNECT asinos .

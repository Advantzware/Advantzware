/* notes.i */

{methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}
ASSIGN
  notes.rec_key = ip-rec_key
  notes.note_date = TODAY
  notes.note_time = TIME
  notes.user_id = USERID("NOSWEAT").

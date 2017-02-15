/* notes.i */

{methods/run_link.i "CONTAINER-SOURCE" "Get-ip-rec_key" "(OUTPUT ip-rec_key)"}
ASSIGN
  note.rec_key = ip-rec_key
  note.note_date = TODAY
  note.note_time = TIME
  note.user_id = USERID("NOSWEAT").

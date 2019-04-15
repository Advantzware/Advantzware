/* shownote.i */

/*
&db_table = DATABASE.table-name
&col = column where notes begin
&frame-name = frame for notes text

NOTE: this include uses the logical variable 'show-notes'
      and integer variable 'notes-type' as follows:
      1 = all notes, 2 = viewed only, 3 = not viewed only
*/

IF show-notes THEN
FOR EACH notes
    WHERE notes.rec_key = {&db_table}.rec_key NO-LOCK:
  IF (notes-type = 2 AND NOT notes.viewed) OR
     (notes-type = 3 AND notes.viewed) THEN
  NEXT.
  PUT UNFORMATTED
    "Note: " AT {&col} notes.note_title
    "Date       Time     Vw  User ID" AT {&col}
    notes.note_date AT {&col} FORMAT "99/99/9999" " "
    STRING(notes.note_time,"HH:MM:SS") " "
    notes.viewed " "
    notes.user_id
    "** NOTE **********************************" AT {&col}.
  DISPLAY notes.note_text
    WITH FRAME {&frame-name} COL {&col} NO-LABELS NO-BOX STREAM-IO.
  PUT UNFORMATTED
    "** END NOTE ******************************" AT {&col}.
END.

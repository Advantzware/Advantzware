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
FOR EACH note
    WHERE note.rec_key = {&db_table}.rec_key NO-LOCK:
  IF (notes-type = 2 AND NOT note.viewed) OR
     (notes-type = 3 AND note.viewed) THEN
  NEXT.
  PUT UNFORMATTED
    "Note: " AT {&col} note.note_title
    "Date       Time     Vw  User ID" AT {&col}
    note.note_date AT {&col} FORMAT "99/99/9999" " "
    STRING(note.note_time,"HH:MM:SS") " "
    note.viewed " "
    note.user_id
    "** NOTE **********************************" AT {&col}.
  DISPLAY note.note_text
    WITH FRAME {&frame-name} COL {&col} NO-LABELS NO-BOX STREAM-IO.
  PUT UNFORMATTED
    "** END NOTE ******************************" AT {&col}.
END.

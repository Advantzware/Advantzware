/* notes.i */

IF AVAILABLE note THEN
DISPLAY STRING(note.note_time,"HH:MM:SS am") FORMAT "X(11)" @ note.note_time
    WITH FRAME {&FRAME-NAME}.

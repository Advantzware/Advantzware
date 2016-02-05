/* notes.i */

IF AVAILABLE notes THEN
DISPLAY STRING(notes.note_time,"HH:MM:SS am") FORMAT "X(11)" @ notes.note_time
    WITH FRAME {&FRAME-NAME}.

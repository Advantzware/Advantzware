/* custom/prntnote.i   Print all note for the record */

FOR EACH notes WHERE note.rec_key = {1} NO-LOCK BY notes.note_date :
    PUT "    Notes:" skip
        "           " notes.note_date notes.note_TITLE SKIP.
END.

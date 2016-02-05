def buffer b-notes for notes.
def var i as int.

/*output to c:\tmp\notes.dat. */

for each est no-lock .
   i = 0.
   for each notes where notes.rec_key = est.rec_key :
  disp est.est-no note_title form "x(20)"
          note_text form "x(20)" view-as fill-in note_code note_form_no.
          
          
  /*   export notes.
     
     find first b-notes where b-notes.rec_key = notes.rec_key and
                              b-notes.note_title = notes.note_title and
                              b-notes.note_code = notes.note_code  and
                              b-notes.note_text = notes.note_text and
                              recid(b-notes) <> recid(notes)
                              no-lock no-error.
    if avail b-notes then  delete notes.
    */

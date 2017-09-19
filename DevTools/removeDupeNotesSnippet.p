disable triggers for load of notes.
def buffer bnotes for notes.
def var ictr as int.
for each notes:
find bnotes where
rowid(bnotes) <> rowid(notes) and
bnotes.note_code = notes.note_code and
bnotes.note_date = notes.note_date and
bnotes.note_form_no = notes.note_form_no and
bnotes.note_group = notes.note_group and
bnotes.note_text = notes.note_text and
bnotes.note_time = notes.note_time and
bnotes.note_title = notes.note_title and
bnotes.note_type = notes.note_type and
bnotes.rec_key = notes.rec_key and
bnotes.user_id = notes.user_id
exclusive no-error.
    if avail bnotes then do:
        delete bnotes.
        assign ictr = ictr + 1.
    end.
end.
disp ictr.

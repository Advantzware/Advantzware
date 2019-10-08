/* custom/notespr5.i for job ticket 
                     {1} = table name 
                     {2} = note variable name with extent 
                     {3} =  variable's extent 
                     
                     By dept.fc Sequence*/

ASSIGN {2} = ""
       v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

FOR EACH notes WHERE {4} NO-LOCK,
    FIRST dept WHERE dept.CODE = notes.note_code
    BREAK BY notes.note_form_no BY dept.fc BY notes.note_date BY notes.note_time:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent + k*/ k.
    /*FIND FIRST dept WHERE dept.CODE = notes.note_code NO-LOCK NO-ERROR.*/
    lv-form-note = /*IF notes.note_FORM_no > 0 
                   THEN ("Form: " + STRING(notes.note_FORM_no) + " " + notes.note_title + " " + notes.note_text)
                   ELSE*/
                   (IF AVAIL dept THEN caps(dept.dscr) ELSE caps(notes.note_code)) + "     " + 
                    notes.note_title + " " + notes.note_text .

    DO i = 1 TO LENGTH(lv-form-note) :        
           IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                 lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / lv-line-chars.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return +
               IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

           IF k > 0 AND k <= {3} THEN {2}[k] = {2}[k] +
                                  IF SUBSTRING(lv-form-note,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                  THEN SUBSTRING(lv-form-note,i,1)
                                  ELSE "" .              
           
           IF SUBSTRING(lv-form-note,i,1) = CHR(10) OR SUBSTRING(lv-form-note,i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.
    END.

    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0.

    IF k > {3} THEN LEAVE.
END.



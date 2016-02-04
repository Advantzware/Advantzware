/* custom/notespr5.i for job ticket Fiber folding
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
    FIRST dept WHERE dept.CODE = notes.note_code BREAK BY dept.fc:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent + k*/ k.
    /*FIND FIRST dept WHERE dept.CODE = notes.note_code NO-LOCK NO-ERROR.*/
    lv-form-note = /*IF notes.note_FORM_no > 0 
                   THEN ("Form: " + STRING(notes.note_FORM_no) + " " + notes.note_title + " " + notes.note_text)
                   ELSE*/
                   (IF AVAIL dept THEN "<B>" + caps(dept.dscr) + "</B>" ELSE caps(note.note_code)) + "     " + 
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

v-prev-note-rec = ?.
ASSIGN v-tmp-lines = 0
       j = 0 
       lv-got-return = lv-got-return + 1
       v-dept-note-line = k.
FIND FIRST itemfg WHERE itemfg.company = job-hdr.company
                    AND itemfg.i-no = bf-jobhdr.i-no NO-LOCK NO-ERROR.
IF AVAIL itemfg then
   FOR EACH notes NO-LOCK WHERE notes.rec_key   EQ itemfg.rec_key
              AND notes.note_type EQ "S"
              AND LOOKUP(notes.note_code,v-spec-list) GT 0,
       FIRST ITEM-spec NO-LOCK WHERE item-spec.company = itemfg.company
                                 AND item-spec.i-no = ""
                                 AND item-spec.CODE = notes.note_code:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent + k*/ k.
    /*FIND FIRST dept WHERE dept.CODE = notes.note_code NO-LOCK NO-ERROR.*/
    lv-form-note = /*IF notes.note_FORM_no > 0 
                   THEN ("Form: " + STRING(notes.note_FORM_no) + " " + notes.note_title + " " + notes.note_text)
                   ELSE*/
                   (IF AVAIL dept THEN "<B>" + caps(trim(item-spec.note[1])) + "</B>" ELSE caps(note.note_code)) + "     " + 
                    notes.note_title + " " + notes.note_text .

    DO i = 1 TO LENGTH(lv-form-note) :        
           IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                 lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / lv-line-chars.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-dept-note-line + v-tmp-lines + lv-got-return +
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

/* custom/notesprt.i {1} = table name 
                     {2} = note variable name with extent 
                     {3} =  variable's extent */

ASSIGN {2} = ""
       v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

FOR EACH notes WHERE notes.rec_key = {1}.rec_key NO-LOCK:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent +*/ k.
    DO i = 1 TO LENGTH(notes.note_text) :        
           IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                 lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / lv-line-chars.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return +
               IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

           IF k > 0 AND k <= {3} THEN {2}[k] = {2}[k] +
                                  IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                                  THEN SUBSTRING(notes.note_text,i,1)
                                  ELSE "" .              
           
           IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
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

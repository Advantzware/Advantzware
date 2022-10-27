/* custom/notespr10.i for job ticket 
                     {1} = table name 
                     {2} = note variable name with extent 
                     {3} =  variable's extent 
                     (Note:  This is a copy of notespr5.i that first sorts by dept.fc for Ruffino ticket 112998)
                     By dept.fc Sequence*/

ASSIGN {2} = ""
       v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

empty temp-table ttnotesort.
FOR EACH notes WHERE {4} NO-LOCK:
    create ttnotesort.
    buffer-copy notes to ttnotesort.
    FIND FIRST dept WHERE dept.CODE = notes.note_code NO-LOCK NO-ERROR.
    assign 
        ttnotesort.deptseq = dept.fc
        ttnotesort.deptdesc = dept.dscr.
end.

FOR EACH ttnotesort use-index isort:

    IF v-prev-note-rec <> ? AND
       v-prev-note-rec <> RECID(ttnotesort) THEN v-prev-extent = /* v-prev-extent + k */ k .

    ASSIGN
        lv-form-note = (IF ttnotesort.deptdesc ne "" THEN caps(ttnotesort.deptdesc) ELSE caps(ttnotesort.note_code)) + "   " +
                       (IF ttnotesort.note_FORM_no > 0 THEN ("Form: " + STRING(ttnotesort.note_FORM_no) + " " + ttnotesort.note_title + " " + ttnotesort.note_text)
                            ELSE ttnotesort.note_title + " " + ttnotesort.note_text).

    assign 
        lv-form-note = replace(lv-form-note,chr(10),chr(10) + "   ")
        lv-form-note = replace(lv-form-note,chr(13),chr(13) + "   ").

    DO i = 1 TO LENGTH(lv-form-note) :        
           IF i - j >= lv-line-chars THEN ASSIGN j = i
                                                 lv-got-return = lv-got-return + 1.
                  
           v-tmp-lines = ( i - j ) / lv-line-chars.
           {SYS/INC/ROUNDUP.I v-tmp-lines}
           k = v-tmp-lines + lv-got-return +
               IF (v-prev-note-rec <> RECID(ttnotesort) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

            
           IF k > 0 AND k <= {3} THEN {2}[k] = {2}[k] +
                                  IF SUBSTRING(lv-form-note,i,1) <> CHR(10) AND SUBSTRING(ttnotesort.note_text,i,1) <> CHR(13)
                                  THEN SUBSTRING(lv-form-note,i,1)
                                  ELSE "" .              
           
           IF SUBSTRING(lv-form-note,i,1) = CHR(10) OR SUBSTRING(lv-form-note,i,1) = CHR(13)                 
           THEN do:
                  lv-got-return = lv-got-return + 1.
                  j = i.
           END.

    END.

    ASSIGN v-prev-note-rec = RECID(ttnotesort)
           j = 0
           lv-got-return = 0.

    IF k > {3} THEN LEAVE.
END.



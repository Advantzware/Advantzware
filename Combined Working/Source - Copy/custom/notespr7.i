/* custom/notespr2.i for job ticket 
                     {1} = table name 
                     {2} = note variable name with extent 
                     {3} =  variable's extent */
                     
DEF VAR v-text  AS CHAR FORMAT "x(100)" NO-UNDO.
DEF VAR v-prvcd LIKE notes.note_code NO-UNDO.
DEF VAR v-cnt1  AS INT NO-UNDO INIT 0.
DEF VAR v-cnt2  AS INT NO-UNDO INIT 0.

ASSIGN {2} = ""
       v-tmp-lines = 0
       j = 0
       K = 0
       lv-got-return = 0.

FOR EACH notes WHERE {4} NO-LOCK:

    ASSIGN v-prvcd = notes.note_code.

    FIND FIRST item-spec NO-LOCK
     WHERE item-spec.company EQ "001"
       AND item-spec.i-no    EQ ""
       AND item-spec.code    EQ notes.note_code NO-ERROR.


    ASSIGN v-text = TRIM(notes.note_code) + " "
           v-text = v-text + " " + 
                    IF AVAIL item-spec THEN trim(item-spec.note[1])
                                       ELSE FILL(" ",24).

    IF LENGTH(v-text) LT 28 
      THEN v-text = SUBSTR(v-text,1,LENGTH(v-text)) + 
                    FILL(" ",28 - LENGTH(v-text)).
 
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

    IF k EQ 1 THEN v-cnt2 = 0.

    IF  v-cnt2 GT k THEN v-cnt2 = 0.

    DO v-cnt1 = v-cnt2 + 1 TO k:
      ASSIGN {2}[v-cnt1] = v-text + {2}[v-cnt1].
    END.
    v-cnt2 = k.

    ASSIGN v-prev-note-rec = RECID(notes)
           j = 0
           lv-got-return = 0
           v-text = "".

    IF k > {3} THEN LEAVE.

END.

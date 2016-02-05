/* t-vend.p assign rec_key to vend for notes */
DEF VAR ls-key AS cha NO-UNDO.

FOR EACH vend /*WHERE rec_key = "" */ :

    IF vend.rec_key = "" THEN DO:
       ls-key = string(today,"99999999") +
                       string(next-value(rec_key_seq,nosweat),"99999999").

       vend.rec_key = ls-key.               

       create rec_key.
       assign rec_key.rec_key = vend.rec_key
              rec_key.table_name = "vend".
    END.
    
    DISP vend.vend-no vend.rec_key.

    CREATE notes.
    ASSIGN notes.rec_key = vend.rec_key                     
           notes.note_date = TODAY
          /* notes.note_code = vend.key
           notes.note_form_no = vend.page# */
           notes.note_type = "C" /* vendor:"C",     Group: "G" */                                   
           notes.note_group  = "" /*vend.TYPE + "," + vend.key-2 */
           notes.note_title = vend.notes[1]
           notes.note_text = vend.notes[1] + chr(10) +
                             (IF vend.notes[2] <> "" THEN (vend.notes[2] + chr(10)) ELSE "" ) +
                             (IF vend.notes[3] <> "" THEN (vend.notes[3] + chr(10)) ELSE "" ) +
                             (IF vend.notes[4] <> "" THEN (vend.notes[4] + chr(10)) ELSE "" ) +
                             (IF vend.notes[5] <> "" THEN (vend.notes[5] + chr(10)) ELSE "" ) +
                             (IF vend.notes[6] <> "" THEN (vend.notes[6] + chr(10)) ELSE "" ) +
                             (IF vend.notes[7] <> "" THEN (vend.notes[7] + chr(10)) ELSE "" ) +
                             (IF vend.notes[8] <> "" THEN (vend.notes[8] + chr(10)) ELSE "" ) +
                             (IF vend.notes[9] <> "" THEN (vend.notes[9] + chr(10)) ELSE "" ) +
                             (IF vend.notes[10] <> "" THEN (vend.notes[10] + chr(10)) ELSE "" ) +
                             (IF vend.notes[11] <> "" THEN (vend.notes[11] + chr(10)) ELSE "" ) +
                             (IF vend.notes[12] <> "" THEN (vend.notes[12] + chr(10)) ELSE "" ) +
                             (IF vend.notes[13] <> "" THEN (vend.notes[13] + chr(10)) ELSE "" ) +
                             (IF vend.notes[14] <> "" THEN (vend.notes[14] + chr(10)) ELSE "" )
                          /* +
                             (IF vend.notes[15] <> "" THEN (vend.notes[15] + chr(10)) ELSE "" ) +
                             (IF vend.notes[16] <> "" THEN (vend.notes[16] + chr(10)) ELSE "" ) +
                             (IF vend.notes[17] <> "" THEN (vend.notes[17] + chr(10)) ELSE "" ) +
                             (IF vend.notes[18] <> "" THEN (vend.notes[18] + chr(10)) ELSE "" )         */                   
                 
           .                   

          IF vend.notes[15] <> "" OR vend.notes[16] <> "" OR
             vend.notes[17] <> "" OR vend.notes[18] <> "" THEN DO:  /* vendor's po notes */

             CREATE notes.
             ASSIGN notes.rec_key = vend.rec_key                    
                    notes.note_date = TODAY
                   /* notes.note_code = vend.key
                    notes.note_form_no = vend.page# */
                    notes.note_type = "G"
                    notes.note_group = "PO"
                    notes.note_title = "PO Notes" /*vend.notes[15]*/
                    notes.note_text = (IF vend.notes[15] <> "" THEN (vend.notes[15] + chr(10)) ELSE "" ) +
                                      (IF vend.notes[16] <> "" THEN (vend.notes[16] + chr(10)) ELSE "" ) +
                                      (IF vend.notes[17] <> "" THEN (vend.notes[17] + chr(10)) ELSE "" ) +
                                      (IF vend.notes[18] <> "" THEN (vend.notes[18] + chr(10)) ELSE "" )     
                    .
          END.


    PAUSE 0.

END.

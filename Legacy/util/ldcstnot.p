/* t-notes.p  Convert notes file from CH_ASI to Nosweat */
def var ls-key as cha form "x(20)" no-undo.

DEF TEMP-TABLE tt-tbl FIELD company AS cha FORM "x(3)"
                      FIELD TYPE AS cha FORM "x(15)"
                      FIELD KEY AS cha FORM "x(8)"
                      FIELD key-2 AS cha FORM "x(8)"
                      FIELD page# AS INT FORM ">>9"                     
                      FIELD contact AS cha FORM "x(30)"
                      FIELD ti-tle AS cha FORM "x(10)"
                      FIELD phone AS cha FORM "x(25)"
                      FIELD fax AS cha FORM "x(25)"                     
                      FIELD e-mail AS cha FORM "x(40)"
                      FIELD notes AS cha EXTENT 20 FORM "x(78)"
                    
                      .

INPUT FROM c:\tmp\notes.d NO-ECHO.

REPEAT:
    CREATE tt-tbl.
    IMPORT tt-tbl.
    

    IF tt-tbl.key-2 <> "" THEN DO:
       FIND FIRST cust WHERE cust.company = tt-tbl.company AND
                             cust.cust-no = tt-tbl.KEY-2 NO-LOCK NO-ERROR.
       IF AVAIL cust THEN do:
          if cust.rec_key = '' THEN DO:
              ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").             
 
             create rec_key.
             assign rec_key.rec_key = ls-key
                    rec_key.table_name = "CUST".
             cust.rec_key = ls-key.               
          END.

          CREATE notes.
          ASSIGN notes.rec_key = cust.rec_key                     
                 notes.note_code = tt-tbl.key
                 notes.note_form_no = tt-tbl.page#
                 notes.note_title = tt-tbl.notes[1]
                 notes.note_text = tt-tbl.notes[1] + CHR(13) +
                                   (IF tt-tbl.notes[2] <> "" THEN (tt-tbl.notes[2] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[3] <> "" THEN (tt-tbl.notes[3] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[4] <> "" THEN (tt-tbl.notes[4] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[5] <> "" THEN (tt-tbl.notes[5] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[6] <> "" THEN (tt-tbl.notes[6] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[7] <> "" THEN (tt-tbl.notes[7] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[8] <> "" THEN (tt-tbl.notes[8] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[9] <> "" THEN (tt-tbl.notes[9] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[10] <> "" THEN (tt-tbl.notes[10] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[11] <> "" THEN (tt-tbl.notes[11] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[12] <> "" THEN (tt-tbl.notes[12] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[13] <> "" THEN (tt-tbl.notes[13] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[14] <> "" THEN (tt-tbl.notes[14] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[15] <> "" THEN (tt-tbl.notes[15] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[16] <> "" THEN (tt-tbl.notes[16] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[17] <> "" THEN (tt-tbl.notes[17] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[18] <> "" THEN (tt-tbl.notes[18] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[19] <> "" THEN (tt-tbl.notes[19] + CHR(13)) ELSE "" ) +
                                   (IF tt-tbl.notes[20] <> "" THEN (tt-tbl.notes[20] + CHR(13)) ELSE "" ) 
                 notes.note_type = IF tt-tbl.TYPE = "Group" THEN "G"
                                   ELSE IF tt-tbl.TYPE = "Department" THEN "D"
                                   ELSE "C"
                 notes.note_group  = tt-tbl.TYPE + "," + tt-tbl.key-2
                 .                   
       END.
    END.
    ELSE DO:
        FIND FIRST cust WHERE cust.company = tt-tbl.company AND
                              cust.cust-no = tt-tbl.KEY NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
           if cust.rec_key = '' THEN DO:
              ls-key = DYNAMIC-FUNCTION("sfGetNextRecKey").             
 
             create rec_key.
             assign rec_key.rec_key = ls-key
                    rec_key.table_name = "CUST".
             cust.rec_key = ls-key.               
           END.

           CREATE notes.
           ASSIGN notes.rec_key = cust.rec_key                     
                  notes.note_code = ""
                  notes.note_form_no = tt-tbl.page#
                  notes.note_title = tt-tbl.notes[1]
                  notes.note_text = tt-tbl.notes[1] + CHR(13) +
                                    (IF tt-tbl.notes[2] <> "" THEN (tt-tbl.notes[2] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[3] <> "" THEN (tt-tbl.notes[3] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[4] <> "" THEN (tt-tbl.notes[4] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[5] <> "" THEN (tt-tbl.notes[5] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[6] <> "" THEN (tt-tbl.notes[6] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[7] <> "" THEN (tt-tbl.notes[7] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[8] <> "" THEN (tt-tbl.notes[8] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[9] <> "" THEN (tt-tbl.notes[9] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[10] <> "" THEN (tt-tbl.notes[10] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[11] <> "" THEN (tt-tbl.notes[11] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[12] <> "" THEN (tt-tbl.notes[12] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[13] <> "" THEN (tt-tbl.notes[13] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[14] <> "" THEN (tt-tbl.notes[14] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[15] <> "" THEN (tt-tbl.notes[15] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[16] <> "" THEN (tt-tbl.notes[16] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[17] <> "" THEN (tt-tbl.notes[17] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[18] <> "" THEN (tt-tbl.notes[18] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[19] <> "" THEN (tt-tbl.notes[19] + CHR(13)) ELSE "" ) +
                                    (IF tt-tbl.notes[20] <> "" THEN (tt-tbl.notes[20] + CHR(13)) ELSE "" ) 
                  notes.note_type = IF tt-tbl.TYPE = "Group" THEN "G"
                                    ELSE IF tt-tbl.TYPE = "Department" THEN "D"
                                    ELSE "C"
                  notes.note_group  = tt-tbl.TYPE + "," + tt-tbl.key-2
                  .                   
        END.

    END.


disp tt-tbl.key-2.
pause 0.

END.

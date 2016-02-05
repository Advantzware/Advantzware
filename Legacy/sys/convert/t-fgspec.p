/* t- fgspec.p  Convert item-spec. */
DEF VAR ls-key AS cha FORM "x(20)" NO-UNDO.
DEF BUFFER spec-item FOR item-spec.

FOR EACH item-spec NO-LOCK:
    FIND itemfg WHERE itemfg.company = item-spec.company
                  AND itemfg.i-no = item-spec.i-no  NO-ERROR.
    IF NOT AVAIL itemfg THEN NEXT.

    IF itemfg.rec_key = "" THEN DO:
        ls-key = string(today,"99999999") +
                       string(next-value(rec_key_seq,nosweat),"99999999").

        itemfg.rec_key = ls-key.               
        create rec_key.
        assign rec_key.rec_key = itemfg.rec_key
              rec_key.table_name = "itemfg".
    END.
    FIND FIRST spec-item WHERE spec-item.company = item-spec.company
                           AND spec-item.i-no = ""
                           AND spec-item.code eq item-spec.CODE NO-LOCK NO-ERROR.
    FIND FIRST notes WHERE notes.rec_key = itemfg.rec_key AND
                           notes.note_type = "S" AND
                           notes.note_code = item-spec.code AND
                           notes.note_title = (IF AVAIL spec-item THEN spec-item.note[1] 
                                              ELSE item-spec.note[1])
                           NO-LOCK NO-ERROR.
    IF AVAIL notes THEN NEXT.
    
    CREATE notes.
    ASSIGN notes.rec_key = itemfg.rec_key
           notes.note_code = item-spec.CODE
           /*notes.note_form_no = est-inst.line-no*/
           notes.note_type = "S"
           notes.note_title = IF AVAIL spec-item THEN trim(spec-item.note[1])
                              ELSE TRIM(item-spec.note[1])
           notes.note_text = trim(item-spec.note[1]) + CHR(10) +
                             trim(item-spec.note[2]) + CHR(10) +
                             trim(item-spec.note[3]) + CHR(10) +
                             TRIM(item-spec.note[4])
           notes.note_date = TODAY .

  DISP item-spec.i-no item-spec.note[1] FORM "x(30)".
  PAUSE 0.

END.

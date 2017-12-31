DEF TEMP-TABLE wktdf NO-UNDO
  FIELD setid         LIKE ws_setid
  FIELD segment       LIKE ws_segment
  FIELD SECTION       LIKE ws_section
  FIELD line          AS int
  FIELD seq           LIKE ws_seq
  field rec           as recid
  FIELD data-record   LIKE str_buffa
  INDEX byset IS PRIMARY setid segment SECTION line seq rec.

procedure write_segments.ip:
DEF INPUT PARAM segment_list AS char NO-UNDO.
DEF var segx AS int NO-UNDO.
DEF var seg_seq AS int NO-UNDO.

DO segx = 1 TO NUM-ENTRIES(segment_list)
    ON error UNDO, NEXT:
  ws_segment = ENTRY(segx, segment_list).
  segx = segx + 1.
  ws_seq     = INTEGER(ENTRY(segx, segment_list)) no-error.
  /* to allow for multiple N1 to N4 in the same section */
  IF ws_segment EQ "N1" AND ws_section EQ 10 AND ws_setid EQ "810" THEN 
    ws_section = 20.
    /* Skip past the N1 of 20-24 for other header segments */
    IF ws_segment NE "N1" AND ws_segment NE "N2" AND ws_segment NE "N3" AND ws_segment NE "N4"
      AND ws_section EQ 20 AND ws_setid EQ "810" THEN 
        ws_section = 25.    
  IF ERROR-STATUS:ERROR THEN DO:
       MESSAGE "error in seqment list" segx skip
       segment_list
       VIEW-AS ALERT-BOX.
      END. 

  IF top-debug THEN
  RUN rc/debugmsg.p (ws_segment + ":" + string(ws_seq)).

  IF CAN-DO(partner_segment_list, ws_segment)
    OR ws_segment = "000"   /* always writeable */
    OR ws_segment = "CTT"   /* always writeable */
    THEN
  DO:
    str_buffa = FILL(" ",255).
    ASSIGN
      {rc/outstr.i ws_setid 1 6}
      {rc/outstr.i ws_segment 7 3}
      {rc/outstr.i string(ws_seq,'999') 10 3}
      {rc/outstr.i '00000' 13 5}
      ws_char = str_buffa /* save it for debugging if needed */
      .
 
    next_program = "ed/tdf/" + ws_segment + ".p".
    IF SEARCH(next_program) <> ? THEN
    DO:
      error-status:error = FALSE.
      RUN VALUE(next_program) ("O", INPUT-OUTPUT str_buffa, OUTPUT ws_erc).
      IF ws_erc <> 0 OR error-status:error THEN
      DO:
        RUN rc/debugmsg.p ("Run of " + next_program 
            + "Ret Error Status: " + string(error-status:error) 
            + " ws_erc: " + string(ws_erc) ).
        error-status:error = FALSE.
      END.
      ELSE
      DO:
        IF str_buffa = ? THEN
        RUN rc/debugmsg.p ("Segment evaluated to ?-Value: " + ws_char + " in " + next_program).
        ELSE
        DO:
          CREATE wktdf.
          ASSIGN
            wktdf.setid = ws_setid
            wktdf.segment = ws_segment
            wktdf.section = ws_section
            wktdf.line    = ws_line
            wktdf.seq = ws_seq
            wktdf.rec = recid(wktdf)
            wktdf.data-record = TRIM(str_buffa).
	      IF ws_filetype EQ "EDI" THEN 
		    wktdf.data-record = SUBSTRING(wktdf.data-record, INDEX(wktdf.data-record, ws_elem_delim)).
        END.
        IF CAN-DO(ctt_count_segment_list, ws_segment) THEN
        {rc/incr.i number_of_line_items}.  /* 9808 CAH */
      END.
    END.
    ELSE
    DO:
      RUN rc/debugmsg.p
        ("Could not find segment procedure: " + next_program).
    END.
  END.
END.
END procedure.

PROCEDURE write_tdf.ip:
FOR EACH wktdf
    BREAK BY wktdf.setid BY wktdf.section BY wktdf.line BY wktdf.seq
    by wktdf.rec:
  PUT STREAM s-edi UNFORMATTED wktdf.section wktdf.line wktdf.seq TRIM(wktdf.data-record) SKIP.
  DELETE wktdf.
END.    /* for each */
END PROCEDURE.


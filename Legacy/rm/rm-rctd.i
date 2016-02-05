   
  FIND FIRST {1} WHERE {1}.r-no EQ {3}.r-no NO-LOCK NO-ERROR.
  IF AVAIL {1} THEN DO:
/*     FIND LAST {1} USE-INDEX r-no NO-LOCK NO-ERROR. */
/*     {3}.r-no = {1}.r-no + 1.                       */
     RUN sys/ref/asiseq.p (INPUT {1}.company, INPUT "rm_rcpt_seq", OUTPUT {3}.r-no) NO-ERROR.
  END.
  
  CREATE {1}.
  BUFFER-COPY {3} EXCEPT rec_key user-id upd-date upd-time TO {1}
  ASSIGN
   {1}.trans-date = {3}.rct-date
   {1}.post-date  = v-post-date.

  CREATE {2}.
  BUFFER-COPY {3} EXCEPT rec_key user-id upd-date upd-time TO {2}.

  IF {3}.rita-code EQ "T" THEN DO:
    {2}.qty = {3}.qty * -1.
    IF {2}.tag EQ {2}.tag2 THEN
        {2}.tag2 = "".

    CREATE {2}.
    BUFFER-COPY {3} EXCEPT rec_key user-id upd-date upd-time TO {2}
    ASSIGN
     {2}.loc     = {3}.loc2
     {2}.loc-bin = {3}.loc-bin2
     {2}.tag     = {3}.tag2
    .
    IF {2}.tag EQ {2}.tag2 THEN
        {2}.tag2 = "".

    RELEASE {2}.
  END.

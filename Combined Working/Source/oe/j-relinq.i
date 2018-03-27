
IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).


IF fi_rel-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                       ~
      OPEN QUERY {&browse-name}                   ~
          {&for-each1}                            ~
                AND oe-relh.release# EQ fi_rel-no ~
              USE-INDEX release# NO-LOCK,          ~
              {&for-each2}                        ~
              OUTER-JOIN

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_ord-no NE 0 OR fi_i-no NE "" THEN DO:
    
  assign begin_rno = 0 
         ending_rno = 0
         .  
  IF fi_ord-no NE 0 THEN DO:
      /* Set range of r-no */
      FOR EACH bf-oe-rell NO-LOCK 
         WHERE bf-oe-rell.company EQ cocode      
           AND bf-oe-rell.ord-no EQ fi_ord-no:
         IF bf-oe-rell.r-no GT ending_rno THEN 
           ending_rno = bf-oe-rell.r-no.
         IF begin_rno EQ 0 THEN begin_rno = bf-oe-rell.r-no. 
            ELSE IF bf-oe-rell.r-no LT begin_rno THEN 
              begin_rno = bf-oe-rell.r-no.
      END.
  END. 
  ELSE DO:
      /* Obtain range of r-no based on i-no entered */
      IF tb_posted THEN DO:
          /* To use index i-no */
          FOR EACH bf-oe-rell NO-LOCK 
              WHERE bf-oe-rell.company EQ cocode      
                AND bf-oe-rell.i-no BEGINS fi_i-no:
              IF bf-oe-rell.r-no GT ending_rno THEN 
                  ending_rno = bf-oe-rell.r-no.
              IF begin_rno EQ 0 THEN begin_rno = bf-oe-rell.r-no. 
                ELSE IF bf-oe-rell.r-no LT begin_rno THEN 
                        begin_rno = bf-oe-rell.r-no.
          END.
      END. 
      ELSE DO:
          /* To use posted index */
          FOR EACH bf-oe-rell NO-LOCK 
              WHERE bf-oe-rell.company EQ cocode  
                AND bf-oe-rell.posted EQ NO    
                AND bf-oe-rell.i-no BEGINS fi_i-no:
              IF bf-oe-rell.r-no GT ending_rno THEN 
                  ending_rno = bf-oe-rell.r-no.
              IF begin_rno EQ 0 THEN begin_rno = bf-oe-rell.r-no. 
                ELSE IF bf-oe-rell.r-no LT begin_rno THEN 
                        begin_rno = bf-oe-rell.r-no.
          END.
      END.

  END.     
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              AND oe-relh.r-no GE begin_rno ~
              AND oe-relh.r-no LE ending_rno ~
              USE-INDEX r-no NO-LOCK, ~
              {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_cust-no NE '' THEN DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              USE-INDEX cust NO-LOCK, ~
              {&for-each2}


  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE DO:

  IF fi_cust-no EQ "" AND fi_i-no EQ "" AND fi_po-no EQ "" AND
     fi_job-no EQ "" THEN
  DO:
     &SCOPED-DEFINE open-query              ~
        OPEN QUERY {&browse-name}          ~
            {&for-each1blank}              ~
                USE-INDEX delpost NO-LOCK, ~
                {&for-each2blank}
  END.
  ELSE
  DO:
    &SCOPED-DEFINE open-query              ~
        OPEN QUERY {&browse-name}          ~
            {&for-each1}                   ~
                USE-INDEX delpost NO-LOCK, ~
                {&for-each2}
  END.
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

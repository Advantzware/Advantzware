
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
IF fi_ord-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              USE-INDEX delpost NO-LOCK, ~
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


IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

IF fi_ord-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
               ~
            NO-LOCK,         ~
            {&for-each2} AND oe-ordl.ord-no EQ fi_ord-no

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_po-no NE "" THEN DO:
  &SCOPED-DEFINE open-query            ~
      OPEN QUERY {&browse-name}        ~
          {&for-each1}                 ~
               NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_job-no NE "" THEN DO:
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
             NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_est-no NE "" THEN DO:
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_part-no NE "" THEN DO:
  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_i-no NE "" THEN DO:
  &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}           ~
          {&for-each1}                    ~
             NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX opened NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

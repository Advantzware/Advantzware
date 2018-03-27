
IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

RELEASE po-ord.
IF fi_po-no NE 0 THEN
FIND po-ord NO-LOCK
    WHERE po-ord.company EQ cocode
      AND po-ord.po-no   EQ fi_po-no
    NO-ERROR.
    
IF AVAIL po-ord THEN DO:

  &SCOPED-DEFINE open-query                         ~
      OPEN QUERY {&browse-name}                     ~
          {&for-each1}                              ~
            AND fg-rcpth.vend-no EQ po-ord.vend-no  ~
            AND fg-rcpth.po-no   EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>")) ~
              USE-INDEX vend NO-LOCK,               ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.    

ELSE
IF fi_job-no NE "" THEN DO:
    
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              USE-INDEX job NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.    

ELSE
IF fi_i-no NE "" THEN DO:
    
  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              USE-INDEX tran NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE DO:
    
  &SCOPED-DEFINE open-query                 ~
      OPEN QUERY {&browse-name}             ~
          {&for-each1}                      ~
              USE-INDEX trans-date NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

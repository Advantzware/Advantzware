/*addon/loadtag/j-ldtag.i*/

IF tb_tag-no NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
             /* USE-INDEX tag */ NO-LOCK 
  
  IF ll-sort-asc THEN {&open-query}  {&sortby-phrase-asc} .
                 ELSE {&open-query}  {&sortby-phrase-desc} .
END.
ELSE IF tb_rfidtag NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each2}                  ~
             /* USE-INDEX tag */ NO-LOCK 
  
  IF ll-sort-asc THEN {&open-query}  {&sortby-phrase-asc} .
                 ELSE {&open-query}  {&sortby-phrase-desc} .
END.

/*
ELSE IF tb_loc NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX tag NO-LOCK
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.
ELSE IF tb_loc-bin NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX tag NO-LOCK
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.
ELSE IF tb_job-no NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX job-no NO-LOCK ~
              
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.
    
ELSE
IF fi_ord-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND job-hdr.ord-no EQ fi_ord-no ~
            USE-INDEX ord-no NO-LOCK,         ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.



ELSE
IF fi_i-no NE "" THEN DO:
  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              USE-INDEX i-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.
*/
ELSE DO:
  &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}           ~
          {&for-each1}                    ~
              /*USE-INDEX tag*/ NO-LOCK
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc} .
END.

RUN dispatch ('get-last':U).

IF AVAIL loadtag THEN
  ASSIGN
   lv-last-rowid  = ROWID(loadtag)
   .
    
RUN dispatch ('get-first':U).

IF AVAIL loadtag THEN
  ASSIGN
   lv-frst-rowid  = ROWID(loadtag)
   .

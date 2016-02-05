
IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).

IF fi_inv-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND ar-invl.inv-no EQ fi_inv-no ~
            USE-INDEX inv-no-desc NO-LOCK,    ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.
    
ELSE
IF fi_bol-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND ar-invl.bol-no EQ fi_bol-no ~
            USE-INDEX bol-no NO-LOCK,         ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.
    
ELSE
IF fi_ord-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND ar-invl.ord-no EQ fi_ord-no ~
            USE-INDEX ord-no NO-LOCK,         ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_est-no NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX est-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_po-no NE "" THEN DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              USE-INDEX cust-po NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_part-no NE "" THEN DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              USE-INDEX part-no NO-LOCK, ~
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

ELSE
IF fi_cust-no NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX inv-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_actnum NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX actnum NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.
ELSE DO:

  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each11}                 ~
              USE-INDEX posted NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

RUN dispatch ('get-last':U).

IF AVAIL ar-invl THEN
  ASSIGN
   lv-last-rowid  = ROWID(ar-invl)
   lv-last-rowid2 = ROWID(ar-inv).
   
RUN dispatch ('get-first':U).

IF AVAIL ar-invl THEN
  ASSIGN
   lv-frst-rowid  = ROWID(ar-invl)
   lv-frst-rowid2 = ROWID(ar-inv).

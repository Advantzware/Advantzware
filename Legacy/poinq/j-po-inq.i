
IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

IF fi_po-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                     ~
      OPEN QUERY {&browse-name}                 ~
          {&for-each1}                          ~
              AND po-ordl.po-no EQ fi_po-no     ~
            USE-INDEX po-no NO-LOCK,            ~
            {&for-each2}, ~
            {&for-each3}, ~
            {&for-each4}, ~
            {&for-each5}, ~
            {&for-each6}

  IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc1}.
                   ELSE {&open-query} {&sortby-phrase-desc1}.
  ELSE
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc2}.
                   ELSE {&open-query} {&sortby-phrase-desc2}.
END.

ELSE
IF fi_job-no NE "" THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
              USE-INDEX job-no NO-LOCK, ~
              {&for-each2}, ~
              {&for-each3}, ~
              {&for-each4}, ~
              {&for-each5}, ~
              {&for-each6}
  
  IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc1}.
                   ELSE {&open-query} {&sortby-phrase-desc1}.
  ELSE
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc2}.
                   ELSE {&open-query} {&sortby-phrase-desc2}.
END.

ELSE
IF fi_i-no NE "" THEN DO:
  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
              USE-INDEX item NO-LOCK, ~
              {&for-each2}, ~
              {&for-each3}, ~
              {&for-each4}, ~
              {&for-each5}, ~
              {&for-each6}
  
  IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc1}.
                   ELSE {&open-query} {&sortby-phrase-desc1}.
  ELSE
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc2}.
                   ELSE {&open-query} {&sortby-phrase-desc2}.
END.

ELSE
IF fi_vend-i-no NE "" THEN DO:
  &SCOPED-DEFINE open-query                ~
      OPEN QUERY {&browse-name}            ~
          {&for-each1}                     ~
              USE-INDEX vend-i-no NO-LOCK, ~
              {&for-each2}, ~
              {&for-each3}, ~
              {&for-each4}, ~
              {&for-each5}, ~
              {&for-each6}
  
  IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc1}.
                   ELSE {&open-query} {&sortby-phrase-desc1}.
  ELSE
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc2}.
                   ELSE {&open-query} {&sortby-phrase-desc2}.
END.

ELSE
IF tb_open NE tb_closed THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
          {&for-each1}                        ~
              USE-INDEX opened NO-LOCK,       ~
              {&for-each2}, ~
              {&for-each3}, ~
              {&for-each4}, ~
              {&for-each5}, ~
              {&for-each6}
  IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc1}.
                   ELSE {&open-query} {&sortby-phrase-desc1}.
  ELSE
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc2}.
                   ELSE {&open-query} {&sortby-phrase-desc2}.
END.

ELSE DO:
  &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}           ~
          {&for-each1}                    ~
              NO-LOCK,                    ~
              {&for-each2}, ~
              {&for-each3}, ~
              {&for-each4}, ~
              {&for-each5}, ~
              {&for-each6}
  IF LOOKUP(lv-sort-by,lv-sort-list1) GT 0 THEN
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc1}.
                   ELSE {&open-query} {&sortby-phrase-desc1}.
  ELSE
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc2}.
                   ELSE {&open-query} {&sortby-phrase-desc2}.
END.

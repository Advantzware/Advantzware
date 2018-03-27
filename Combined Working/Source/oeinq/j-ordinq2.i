
IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

IF fi_ord-no NE 0 THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-ordl.ord-no EQ fi_ord-no ~
            USE-INDEX ord-no NO-LOCK,         ~
            {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad             ~
    OPEN QUERY {&browse-name}               ~
      {&for-each1}                          ~
            AND oe-ordl.ord-no EQ fi_ord-no ~
          USE-INDEX ord-no NO-LOCK,         ~
          {&for-each2}

  {oeinq/j-ordinq1.i}
END.

ELSE
IF fi_po-no1 NE "" THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query            ~
      OPEN QUERY {&browse-name}        ~
          {&for-each1}                 ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX po-no NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad      ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX est NO-LOCK, ~
              {&for-each2}
  
  {oeinq/j-ordinq1.i}
END.

ELSE
IF fi_i-no NE "" THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query           ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
          USE-INDEX item NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad       ~
      OPEN QUERY {&browse-name}       ~
          {&for-each1}                ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX item NO-LOCK, ~
              {&for-each2}
  
  {oeinq/j-ordinq1.i}
END.

ELSE
IF fi_job-no NE "" THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX job NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad      ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX est NO-LOCK, ~
              {&for-each2}
  
  {oeinq/j-ordinq1.i}
END.

ELSE
IF fi_est-no NE "" THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX est NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad      ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX est NO-LOCK, ~
              {&for-each2}
  
  {oeinq/j-ordinq1.i}
END.

ELSE DO:

  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX opened NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad         ~
      OPEN QUERY {&browse-name}         ~
          {&for-each1}                  ~
          AND oe-ordl.ord-no GE lv-first-ord-no ~
          AND oe-ordl.ord-no LE lv-last-ord-no ~
              USE-INDEX opened NO-LOCK, ~
              {&for-each2}
  
  {oeinq/j-ordinq1.i}
END.

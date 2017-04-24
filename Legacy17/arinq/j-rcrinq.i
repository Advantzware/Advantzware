
IF fi_inv-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                    ~
      OPEN QUERY {&browse-name}                ~
        {&for-each1}                           ~
              AND ar-cashl.inv-no EQ fi_inv-no ~
            NO-LOCK,                           ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.
    
ELSE
IF fi_fchk NE 0 THEN DO:
  &SCOPED-DEFINE open-query                                           ~
      OPEN QUERY {&browse-name}                                       ~
        {&for-each1}                                                  ~
              AND (ar-cashl.check-no EQ STRING(fi_fchk,"99999999") OR ~
                   ar-cashl.check-no EQ STRING(fi_fchk,"9999999999")) ~
            NO-LOCK,                                                  ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.
    
ELSE DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              NO-LOCK,                   ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

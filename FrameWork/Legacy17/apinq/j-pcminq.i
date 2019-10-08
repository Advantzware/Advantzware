
IF fi_fchk NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND ap-pay.check-no EQ fi_fchk ~
            USE-INDEX ap-pay NO-LOCK,    ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.
    
ELSE DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              USE-INDEX vend-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.


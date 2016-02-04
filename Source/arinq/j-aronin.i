
SESSION:SET-WAIT-STATE ("general").

/*IF tb_posted EQ YES AND tb_unposted EQ NO THEN DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
        {&for-each1}                     ~
              AND ar-cashl.posted EQ YES ~
            USE-INDEX inv-no NO-LOCK,    ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF tb_posted EQ NO AND tb_unposted EQ YES THEN DO:
  &SCOPED-DEFINE open-query             ~
      OPEN QUERY {&browse-name}         ~
        {&for-each1}                    ~
              AND ar-cashl.posted EQ NO ~
            USE-INDEX inv-no NO-LOCK,   ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE*/ DO:
  &SCOPED-DEFINE open-query               ~
      OPEN QUERY {&browse-name}           ~
          {&for-each1}                    ~
              USE-INDEX inv-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

RUN dispatch ('get-last':U).

IF AVAIL ar-cashl THEN
  ASSIGN
   lv-last-rowid  = ROWID(ar-cashl)
   lv-last-rowid2 = ROWID(ar-cash).
    
RUN dispatch ('get-first':U).

IF AVAIL ar-cashl THEN
  ASSIGN
   lv-frst-rowid  = ROWID(ar-cashl)
   lv-frst-rowid2 = ROWID(ar-cash).

SESSION:SET-WAIT-STATE ("").


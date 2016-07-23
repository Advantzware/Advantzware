/* oe/j-oebolq.i */

DEF VAR lv-b-no LIKE oe-bolh.bol-no NO-UNDO.

IF fi_bol-no NE 0 THEN DO:
  FIND FIRST oe-bolh NO-LOCK
      WHERE oe-bolh.company EQ cocode
        AND oe-bolh.bol-no  EQ fi_bol-no
      NO-ERROR.

  IF AVAIL oe-bolh THEN DO:
    lv-b-no = oe-bolh.b-no.

    RELEASE oe-bolh.

    &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name}           ~
          {&for-each1}                      ~
                AND oe-boll.b-no EQ lv-b-no ~
              USE-INDEX b-no NO-LOCK,       ~
              {&for-each2},                 ~
              {&for-each3}

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
  END.
END.

ELSE
IF fi_ord-no NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND oe-boll.ord-no EQ fi_ord-no ~
            USE-INDEX ord-no NO-LOCK,         ~
            {&for-each2},                     ~
            {&for-each3}
              
            
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_po-no NE "" THEN DO:
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
        {&for-each1}                 ~
            USE-INDEX po-no NO-LOCK, ~
            {&for-each2},            ~
            {&for-each3}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE
IF fi_i-no NE "" THEN DO:
  &SCOPED-DEFINE open-query         ~
      OPEN QUERY {&browse-name}     ~
        {&for-each1}                ~
            USE-INDEX i-no NO-LOCK, ~
            {&for-each2},           ~
            {&for-each3}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE
IF fi_cust-no NE "" THEN DO:
  &SCOPED-DEFINE open-query         ~
      OPEN QUERY {&browse-name}     ~
        {&for-each1}                ~
            USE-INDEX cust NO-LOCK, ~
            {&for-each2},           ~
            {&for-each3} 

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.

ELSE DO:

  IF fi_i-name EQ "" THEN
  DO:
     &SCOPED-DEFINE open-query     ~
         OPEN QUERY {&browse-name} ~
           {&for-each11}            ~
               NO-LOCK,            ~
               {&for-each21},       ~
               {&for-each31} 
  END.
  ELSE
  DO:
     &SCOPED-DEFINE open-query     ~
         OPEN QUERY {&browse-name} ~
           {&for-each11}           ~
               NO-LOCK,            ~
               {&for-each21},      ~
               {&for-each3} 
  END.

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.

/* oe/j-oebolq.i */

DEFINE VARIABLE lv-b-no   LIKE oe-bolh.bol-no NO-UNDO.
DEFINE VARIABLE iCount    AS INTEGER          NO-UNDO.
DEFINE VARIABLE lFirst    AS LOGICAL INIT YES NO-UNDO.
DEFINE VARIABLE iFirstBNo AS INTEGER          NO-UNDO.

IF fi_bol-no NE 0 THEN DO:
    FIND FIRST oe-bolh NO-LOCK
         WHERE oe-bolh.company EQ cocode
           AND oe-bolh.bol-no  EQ fi_bol-no
         NO-ERROR.

    IF AVAILABLE oe-bolh THEN DO:
        lv-b-no = oe-bolh.b-no.

        RELEASE oe-bolh.
    END.
  
    &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name}           ~
          {&for-each1}                      ~
              AND oe-boll.b-no EQ lv-b-no   ~
              USE-INDEX b-no NO-LOCK,       ~
              {&for-each2},                 ~
              {&for-each3}

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.
  
END.

ELSE IF fi_ord-no NE 0 THEN DO:
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
         {&for-each1}                           ~
              AND oe-boll.ord-no EQ fi_ord-no   ~
              USE-INDEX ord-no NO-LOCK,         ~
              {&for-each2},                     ~
              {&for-each3}
              
    IF ll-sort-asc THEN 
        {&open-query} {&sortby-phrase-asc}.
    ELSE 
        {&open-query} {&sortby-phrase-desc}. 
END.

ELSE IF fi_po-no NE "" THEN DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
        {&for-each1}                     ~
            USE-INDEX po-no NO-LOCK,     ~
            {&for-each2},                ~
            {&for-each3}
  
    IF ll-sort-asc THEN 
        {&open-query} {&sortby-phrase-asc} MAX-ROWS 500. 
    ELSE    
        {&open-query} {&sortby-phrase-desc} MAX-ROWS 500. 
END.

ELSE IF fi_i-no NE "" THEN DO:
    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.i-no    BEGINS fi_i-no
          AND CAN-FIND (FIRST oe-bolh
                        WHERE oe-bolh.company EQ g_company
                          AND oe-bolh.b-no    EQ oe-boll.b-no
                          AND oe-bolh.posted  EQ tb_posted
                          AND oe-bolh.deleted EQ NO
                          USE-INDEX b-no)
        USE-INDEX i-no
        BREAK BY oe-boll.b-no DESCENDING :
        IF FIRST-OF(oe-boll.b-no) THEN DO:
            iCount = iCount + 1.
            IF lFirst THEN
                ASSIGN
                    iFirstBNo = oe-boll.b-no
                    lFirst    = NO
                    .
            lv-b-no = oe-boll.b-no.
        END.   
        IF iCount GE 100 THEN
            LEAVE.
    END. 
    &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name}           ~
          {&for-each1}                      ~
              AND oe-boll.b-no GE lv-b-no   ~
              AND oe-boll.b-no LE iFirstBNo ~
              USE-INDEX b-no NO-LOCK,       ~
              {&for-each2},                 ~
              {&for-each3}
    
    IF ll-sort-asc THEN 
        {&open-query} {&sortby-phrase-asc}.
    ELSE 
        {&open-query} {&sortby-phrase-desc}.
END.

ELSE IF fi_cust-no NE "" THEN DO:
    {&for-each41}
        AND oe-bolh.cust-no BEGINS fi_cust-no
        AND CAN-FIND(FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no
                      )
        USE-INDEX b-no 
        BREAK BY oe-bolh.b-no DESCENDING:
        IF FIRST-OF(oe-bolh.b-no) THEN 
            iCount = iCount + 1.
        IF lFirst THEN 
            ASSIGN 
                iFirstBNo = oe-bolh.b-no
                lFirst    = NO
                .    
        lv-b-no = oe-bolh.b-no.
        IF iCount GE 100 THEN 
            LEAVE.
    END. 

    &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name}           ~
          {&for-each1}                      ~
              AND oe-boll.b-no GE lv-b-no   ~
              AND oe-boll.b-no LE iFirstBNo ~
              USE-INDEX b-no NO-LOCK,       ~
              {&for-each2},                 ~
              {&for-each3} 
    
    IF ll-sort-asc THEN 
        {&open-query} {&sortby-phrase-asc}.
    ELSE 
        {&open-query} {&sortby-phrase-desc}.
END.
ELSE IF fi_part-no NE "" THEN DO:
    {&for-each41}
        USE-INDEX b-no,
        FIRST oe-boll NO-LOCK 
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.b-no    EQ oe-bolh.b-no
          AND CAN-FIND(FIRST oe-ordl 
                       WHERE oe-ordl.company EQ oe-boll.company 
                         AND oe-ordl.ord-no  EQ oe-boll.ord-no 
                         AND oe-ordl.i-no    EQ oe-boll.i-no                    
                         AND oe-ordl.part-no GE fi_part-no)                                  
         BREAK BY oe-bolh.b-no DESCENDING:
        IF FIRST-OF(oe-bolh.b-no) THEN 
            icount = icount + 1.
        IF lFirst THEN 
            ASSIGN 
                iFirstBNo = oe-bolh.b-no
                lFirst    = NO
                .
        lv-b-no = oe-bolh.b-no.
        IF icount GE 100 THEN 
            LEAVE.
    END.
    &SCOPED-DEFINE open-query         ~
    OPEN QUERY {&browse-name}         ~
    {&for-each11}                     ~
        AND oe-boll.b-no GE lv-b-no   ~
        AND oe-boll.b-no LE iFirstBNo ~
        USE-INDEX b-no NO-LOCK ,      ~
        {&for-each21},                ~
        {&for-each31} 
        
IF ll-sort-asc THEN
    {&open-query} {&sortby-phrase-asc}.
ELSE 
    {&open-query} {&sortby-phrase-desc}.
END.    

ELSE DO:
    IF fi_i-name EQ "" THEN
        RUN first-Query.
    ELSE DO:
        {&for-each41}
            USE-INDEX b-no,
            FIRST oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
              AND CAN-FIND(FIRST ASI.oe-ordl  
                           WHERE oe-ordl.company EQ oe-boll.company 
                             AND oe-ordl.ord-no  EQ oe-boll.ord-no 
                             AND oe-ordl.i-no    EQ oe-boll.i-no                    
                             AND oe-ordl.i-name  GE fi_i-name)
            BREAK BY oe-bolh.b-no DESCENDING:
            IF FIRST-OF(oe-bolh.b-no) THEN 
                icount = icount + 1.
            IF lFirst THEN
                ASSIGN 
                    iFirstBNo = oe-bolh.b-no
                    lFirst    = NO
                    .                
            lv-b-no = oe-bolh.b-no.
            IF icount GE 100 THEN
                 LEAVE.
        END.
        &SCOPED-DEFINE open-query             ~
            OPEN QUERY {&browse-name}         ~
            {&for-each11}                     ~
                AND oe-boll.b-no GE lv-b-no   ~
                AND oe-bolh.b-no LE iFirstBNo ~
                USE-INDEX b-no NO-LOCK,       ~
                {&for-each21},                ~
                {&for-each3} 
        IF ll-sort-asc THEN 
            {&open-query} {&sortby-phrase-asc}.
        ELSE 
            {&open-query} {&sortby-phrase-desc}.
    END.
END.

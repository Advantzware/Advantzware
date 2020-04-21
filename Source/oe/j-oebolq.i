/* oe/j-oebolq.i */

DEFINE VARIABLE lv-b-no LIKE oe-bolh.bol-no NO-UNDO.
DEFINE VARIABLE iCount  AS INTEGER          NO-UNDO.

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
    {&for-each41}
        AND CAN-FIND(FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no
                       AND oe-boll.po-no   GE fi_po-no)
        USE-INDEX b-no 
        BREAK BY oe-bolh.b-no DESCENDING:
        IF FIRST-OF(oe-bolh.b-no) THEN
            iCount = iCount + 1.
        lv-b-no = oe-bolh.b-no.
        IF iCount GE 100 THEN 
            LEAVE.
    END.
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
        {&for-each1}                     ~
            AND oe-boll.b-no GT lv-b-no  ~
            USE-INDEX b-no  NO-LOCK,    ~
            {&for-each2},                ~
            {&for-each3}
  
    IF ll-sort-asc THEN 
        {&open-query} {&sortby-phrase-asc}.
    ELSE    
        {&open-query} {&sortby-phrase-desc}. 
END.

ELSE IF fi_i-no NE "" THEN DO:
    {&for-each41}
        AND CAN-FIND(FIRST oe-boll
                     WHERE oe-boll.company EQ oe-bolh.company
                       AND oe-boll.b-no    EQ oe-bolh.b-no
                       AND oe-boll.i-no    GE fi_i-no)
        USE-INDEX b-no 
        BREAK BY oe-bolh.b-no DESCENDING:
        IF FIRST-OF(oe-bolh.b-no) THEN 
            iCount = iCount + 1.
        lv-b-no = oe-bolh.b-no.
        IF iCount GE 100 THEN 
            LEAVE.
    END.
    &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name}           ~
          {&for-each1}                      ~
              AND oe-boll.b-no GT lv-b-no   ~
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
        lv-b-no = oe-bolh.b-no.
        IF iCount GE 100 THEN 
            LEAVE.
    END. 
    &SCOPED-DEFINE open-query             ~
        OPEN QUERY {&browse-name}         ~
          {&for-each1}                    ~
              AND oe-boll.b-no GT lv-b-no ~
              USE-INDEX b-no NO-LOCK,     ~
              {&for-each2},               ~
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
        lv-b-no = oe-bolh.b-no.
        IF icount GE 100 THEN 
            LEAVE.
    END.
    &SCOPED-DEFINE open-query       ~
    OPEN QUERY {&browse-name}       ~
    {&for-each11}                   ~
        AND oe-boll.b-no GT lv-b-no ~
        USE-INDEX b-no NO-LOCK ,     ~
        {&for-each21},              ~
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
            lv-b-no = oe-bolh.b-no.
            IF icount GE 100 THEN
                 LEAVE.
        END.
        &SCOPED-DEFINE open-query           ~
            OPEN QUERY {&browse-name}       ~
            {&for-each11}                   ~
                AND oe-boll.b-no GT lv-b-no ~
                USE-INDEX b-no NO-LOCK,     ~
                {&for-each21},              ~
                {&for-each3} 
        IF ll-sort-asc THEN 
            {&open-query} {&sortby-phrase-asc}.
        ELSE 
            {&open-query} {&sortby-phrase-desc}.
    END.
END.

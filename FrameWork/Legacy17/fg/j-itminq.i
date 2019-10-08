/*fg/j-itminq.i*/
  IF fi_i-no NE "" AND fi_i-no BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
         USE-INDEX i-no :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.
  ELSE IF fi_i-no NE "" AND NOT fi_i-no BEGINS '*' THEN DO:  

    {&for-each1} NO-LOCK
         USE-INDEX i-no :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.

  ELSE IF fi_part-no NE "" AND fi_part-no BEGINS '*' THEN DO:  
   
    {&for-each2} NO-LOCK
         USE-INDEX cust-part :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK 
                                    
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_part-no NE "" AND NOT fi_part-no BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         USE-INDEX cust-part :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK                         
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_cust-no NE "" AND  fi_cust-no BEGINS '*' THEN DO:  
    {&for-each2} NO-LOCK
         USE-INDEX customer :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK                          
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_cust-no NE "" AND NOT  fi_cust-no BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         USE-INDEX customer :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK 
                                     
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_i-name NE "" AND fi_i-name BEGINS '*' THEN DO:  
    {&for-each2} NO-LOCK
         BREAK BY itemfg.i-name :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.
    
     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK 
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_i-name NE "" AND NOT fi_i-name BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         BREAK BY itemfg.i-name :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK 
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_procat NE "" AND fi_procat BEGINS '*' THEN DO:  
     
    {&for-each2} NO-LOCK
         USE-INDEX procat :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK                         
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_procat NE "" AND NOT fi_procat BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         USE-INDEX procat :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK   
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_style NE "" AND fi_style BEGINS '*' THEN DO:  
 
    {&for-each2} NO-LOCK BREAK BY itemfg.style:
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_style NE "" AND NOT fi_style BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK BREAK BY itemfg.style:
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK  

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_est-no NE "" AND fi_est-no BEGINS '*' THEN DO:

     {&for-each2} NO-LOCK
         USE-INDEX estimate :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_est-no NE "" AND NOT fi_est-no BEGINS '*' THEN DO:

     {&for-each1} NO-LOCK
         USE-INDEX estimate :
        ASSIGN
           li = li + 1
           lv-i-no = itemfg.i-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE DO:  
    {&for-eachblank} NO-LOCK
       BREAK BY itemfg.i-no :
       IF FIRST-OF(itemfg.i-no) THEN li = li + 1.
       lv-i-no = itemfg.i-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank} NO-LOCK
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

  END.

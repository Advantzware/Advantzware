/*fg/j-custinq.i*/
  IF fi_ship-id NE "" AND fi_ship-id BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
         /*USE-INDEX i-no*/
        BY shipto.ship-id :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.
  ELSE IF fi_ship-id NE "" AND NOT fi_ship-id BEGINS '*' THEN DO:  
    
    {&for-each1} NO-LOCK
         /*USE-INDEX i-no*/
        BY shipto.ship-id :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
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
         /*USE-INDEX cust-part*/ BY shipto.ship-NAME :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
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
         /*USE-INDEX cust-part*/ BY shipto.ship-NAME :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK                         
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_city NE "" AND  fi_city BEGINS '*' THEN DO:  
    {&for-each2} NO-LOCK
         /*USE-INDEX customer*/ BY shipto.ship-city :
        ASSIGN
           li = li + 1
           lv-shipto-no = fi_city.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK                          
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_city NE "" AND NOT  fi_city BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         /*USE-INDEX customer*/ BY shipto.ship-city :
        ASSIGN
           li = li + 1
           lv-shipto-no = fi_city.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK 
                                     
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_stat NE "" AND fi_stat BEGINS '*' THEN DO:  
    {&for-each2} NO-LOCK
         BREAK BY shipto.ship-state :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.
    
     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK 
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_stat NE "" AND NOT fi_stat BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         BREAK BY shipto.ship-state :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK 
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_zip NE "" AND fi_zip BEGINS '*' THEN DO:  
     
    {&for-each2} NO-LOCK
         /*USE-INDEX procat*/ BY shipto.ship-zip :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK                         
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_zip NE "" AND NOT fi_zip BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK
         /*USE-INDEX procat*/ BY shipto.ship-zip :
        ASSIGN
           li = li + 1
           lv-shipto-no = shipto.ship-id.
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
       BREAK BY shipto.ship-id :
       IF FIRST-OF(shipto.ship-id) THEN li = li + 1.
       lv-shipto-no = shipto.ship-id.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank} NO-LOCK
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

  END.

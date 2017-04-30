/*fg/j-vendinq.i*/
  IF fi_vend-no NE "" AND fi_vend-no BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
         /*USE-INDEX i-no*/
        BY vend.vend-no :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.
  ELSE IF fi_vend-no NE "" AND NOT fi_vend-no BEGINS '*' THEN DO:  

    {&for-each1} NO-LOCK
         /*USE-INDEX i-no*/
        BY vend.vend-no :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
         /*USE-INDEX cust-part*/ BY vend.NAME :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
         /*USE-INDEX cust-part*/ BY vend.NAME :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
         /*USE-INDEX customer*/ BY vend.city :
        ASSIGN
           li = li + 1
           lv-vend-no = fi_city.
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
         /*USE-INDEX customer*/ BY vend.city :
        ASSIGN
           li = li + 1
           lv-vend-no = fi_city.
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
         BREAK BY vend.state :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
         BREAK BY vend.state :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
         /*USE-INDEX procat*/ BY vend.zip :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
         /*USE-INDEX procat*/ BY vend.zip :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK   
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_type NE "" AND fi_type BEGINS '*' THEN DO:  
 
    {&for-each2} NO-LOCK BREAK BY vend.TYPE:
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_type NE "" AND NOT fi_type BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK BREAK BY vend.TYPE:
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK  

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_buyer NE "" AND fi_buyer BEGINS '*' THEN DO:

     {&for-each2} NO-LOCK
         BY vend.buyer :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_buyer NE "" AND NOT fi_buyer BEGINS '*' THEN DO:

     {&for-each1} NO-LOCK
         BY vend.buyer :
        ASSIGN
           li = li + 1
           lv-vend-no = vend.vend-no.
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
       BREAK BY vend.vend-no :
       IF FIRST-OF(vend.vend-no) THEN li = li + 1.
       lv-vend-no = vend.vend-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank} NO-LOCK
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

  END.

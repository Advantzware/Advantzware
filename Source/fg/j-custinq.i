/*fg/j-custinq.i*/
  IF fi_cust-no NE "" AND fi_cust-no BEGINS '*' THEN DO:  

    {&for-each2} NO-LOCK
         /*USE-INDEX i-no*/
        BY cust.cust-no :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.


  END.
  ELSE IF fi_cust-no NE "" AND NOT fi_cust-no BEGINS '*' THEN DO:  

    {&for-each1} NO-LOCK
         /*USE-INDEX i-no*/
        BY cust.cust-no :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
         /*USE-INDEX cust-part*/ BY cust.NAME :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
         /*USE-INDEX cust-part*/ BY cust.NAME :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
         /*USE-INDEX customer*/ BY cust.city :
        ASSIGN
           li = li + 1
           lv-cust-no = fi_city.
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
         /*USE-INDEX customer*/ BY cust.city :
        ASSIGN
           li = li + 1
           lv-cust-no = fi_city.
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
         BREAK BY cust.state :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
         BREAK BY cust.state :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
         /*USE-INDEX procat*/ BY cust.zip :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
         /*USE-INDEX procat*/ BY cust.zip :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK   
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_type NE "" AND fi_type BEGINS '*' THEN DO:  
 
    {&for-each2} NO-LOCK BREAK BY cust.TYPE:
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK  
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_type NE "" AND NOT fi_type BEGINS '*' THEN DO:  
    {&for-each1} NO-LOCK BREAK BY cust.TYPE:
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK  

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.

  ELSE IF fi_terr NE "" AND fi_terr BEGINS '*' THEN DO:

     {&for-each2} NO-LOCK
         BY cust.terr :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_terr NE "" AND NOT fi_terr BEGINS '*' THEN DO:

     {&for-each1} NO-LOCK
         BY cust.terr :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1} NO-LOCK

     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_sman NE "" AND fi_sman BEGINS '*' THEN DO:

     {&for-each2} NO-LOCK
         BY cust.sman :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
        IF li GE sys-ctrl.int-fld THEN LEAVE.
     END.

     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each2} NO-LOCK
            
     IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}.
  END.
  ELSE IF fi_sman NE "" AND NOT fi_sman BEGINS '*' THEN DO:

     {&for-each1} NO-LOCK
         BY cust.sman :
        ASSIGN
           li = li + 1
           lv-cust-no = cust.cust-no.
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
       BREAK BY cust.cust-no :
       IF FIRST-OF(cust.cust-no) THEN li = li + 1.
       lv-cust-no = cust.cust-no.
       IF li GE sys-ctrl.int-fld THEN LEAVE.
    END.

    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
           {&for-eachblank} NO-LOCK
            
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}.

  END.

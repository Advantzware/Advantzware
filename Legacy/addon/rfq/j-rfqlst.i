/* rfq/j-rfqlst.i */

IF vi_est-no NE "" THEN vi_est-no = FILL(" ",8 - LENGTH(TRIM(vi_est-no))) + TRIM(vi_est-no).

IF vi_rfq-no <> 0 THEN do:
      &SCOPED-DEFINE open-query                   ~
          OPEN QUERY {&browse-name}               ~
                   {&for-rfq} and rfq.rfq-no = vi_rfq-no NO-LOCK {&index-eb} ,            ~
                   {&for-rfqitem} NO-LOCK

            IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                           ELSE {&open-query} {&sortby-phrase-desc}. 

END.
ELSE IF vi_req-date <> ? THEN DO:

    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
              {&for-rfq} AND rfq.req-date = vi_req-date NO-LOCK, ~
          {&for-rfqitem} NO-LOCK
                 
    ll-sort-asc = NOT ll-sort-asc.
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}. 
    ll-sort-asc = NOT ll-sort-asc.
    
END.
ELSE IF vi_est-no <> "" THEN DO:
    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}      ~
                 {&for-rfq} NO-LOCK,             ~
                 {&for-rfqitem}             ~
                 AND rfqitem.est-no = vi_est-no NO-LOCK

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}. 
    
END.
ELSE IF begin_cust-no <> "" AND vi_style <> "" THEN DO:
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
             {&for-rfq} AND rfq.cust-no BEGINS begin_cust-no {&index-eb} NO-LOCK,       ~
             {&for-rfqitem} NO-LOCK

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

END.
ELSE IF begin_cust-no <> "" AND vi_len <> 0 THEN do:
    /*      &scoped index-eb USE-INDEX cust-length */

        &SCOPED-DEFINE open-query               ~
        OPEN QUERY {&browse-name}               ~
                 {&for-rfq} and rfq.cust-no BEGINS begin_cust-no {&index-eb} NO-LOCK,        ~
                 {&for-rfqitem} NO-LOCK

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

      END.
ELSE IF begin_cust-no <> "" THEN do:
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
             {&for-rfq} AND rfq.cust-no BEGINS begin_cust-no {&index-eb} NO-LOCK ,       ~
             {&for-rfqitem} NO-LOCK
             
          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 
END.

ELSE IF vi_part-no <> "" THEN do:
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-rfq} NO-LOCK  {&index-eb} ,            ~
                 {&for-rfqitem} NO-LOCK

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


      END.
ELSE IF vi_style <> "" THEN do:
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-rfq} NO-LOCK {&index-eb} ,            ~
                 {&for-rfqitem} NO-LOCK
  
          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

END.
ELSE IF vi_len <> 0 THEN do:
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-rfq} NO-LOCK {&index-eb} ,            ~
                 {&for-rfqitem} NO-LOCK
        
          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

  END.



/*
ELSE IF lv-est-date-entered OR lv-first-run THEN DO:
    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
                 {&for-est} AND est.est-date >= vi_est-date NO-LOCK,         ~
                 {&for-eqty} NO-LOCK, ~
                 {&for-ef} NO-LOCK , ~
                 {&for-eb} NO-LOCK
               
   
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}. 

END. 

ELSE IF vi_die-size <> 0 THEN DO:
    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
                 {&for-est}  NO-LOCK,         ~
                 {&for-eqty} NO-LOCK, ~
                 {&for-ef} ~
                 AND ef.die-in >= vi_die-size NO-LOCK , ~
                 {&for-eb} NO-LOCK
               
   
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                    ELSE {&open-query} {&sortby-phrase-desc}. 

END.
*/
ELSE DO:
    
  &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                   {&for-rfq} NO-LOCK ,  ~
                   {&for-rfqitem}  NO-LOCK

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

END.

lv-first-run = no.

RUN dispatch ('get-last':U).

IF AVAIL rfq THEN
  ASSIGN
   lv-last-rowid  = ROWID(rfq)
   lv-last-rowid2 = ROWID(rfqitem)
   lv-last-show-rfq-no = rfq.rfq-no.
    
RUN dispatch ('get-first':U).

IF AVAIL rfq THEN
  ASSIGN
   lv-frst-rowid  = ROWID(rfq)
   lv-frst-rowid2 = ROWID(rfqitem)
   lv-first-show-rfq-no = rfq.rfq-no.

    

  

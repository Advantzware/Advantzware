/* cec/j-esteb.i */

ll-shipto = begin_cust-no NE "" AND begin_ship NE "" AND
            CAN-FIND(FIRST shipto                              
                     WHERE shipto.company EQ g_company        
                       AND shipto.cust-no EQ begin_cust-no     
                       AND shipto.ship-id EQ begin_ship).

IF vi_est-no NE "" THEN vi_est-no = FILL(" ",8 - LENGTH(TRIM(vi_est-no))) + TRIM(vi_est-no).

IF vi_est-date <> ? THEN DO:

    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
                 {&for-eb}                          ~
                  NO-LOCK ,    ~
                 {&for-est} AND est.est-date >= vi_est-date NO-LOCK, ~
                 {&for-eqty} NO-LOCK, ~
                 {&for-ef} NO-LOCK
                 
     
  /*
    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name} ~
          FOR EACH est WHERE est.company = g_company ~
                                AND est.est-type >= 1 AND est.est-type <=4 ~
                                AND est.est-date  > vi_est-date NO-LOCK, ~
                     EACH est-qty WHERE est-qty.company = est.company ~
                                    AND est-qty.est-no = est.est-no NO-LOCK, ~
                     EACH eb WHERE eb.company = est.company ~
                               AND eb.est-no = est.est-no NO-LOCK, ~
                     EACH ef WHERE ef.company = est.company ~
                               AND ef.est-no = est.est-no NO-LOCK 
   */
    ll-sort-asc = NOT ll-sort-asc.
    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}. 
    ll-sort-asc = NOT ll-sort-asc.
    
END.
ELSE IF vi_est-no <> "" THEN DO:
    &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
                 {&for-eb}                          ~
                 AND eb.est-no = vi_est-no NO-LOCK USE-INDEX est-no ,    ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty} NO-LOCK, ~
                 {&for-ef} NO-LOCK
                 

    IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                   ELSE {&open-query} {&sortby-phrase-desc}. 
    
END.
ELSE IF begin_cust-no <> "" AND vi_style <> "" THEN DO:
    
           &scoped index-eb USE-INDEX cust        
         
                 &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-eb} NO-LOCK  {&index-eb},       ~
                 {&for-est} NO-LOCK,  ~
                 {&for-eqty} NO-LOCK, ~
                 {&for-ef}  NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


END.
ELSE IF begin_cust-no <> "" AND vi_len <> 0 THEN do:

          &scoped index-eb USE-INDEX cust-length 


        &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-eb} NO-LOCK {&index-eb} ,            ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty}  NO-LOCK, ~
                 {&for-ef} NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 



      END.
ELSE IF begin_cust-no <> "" THEN do:
    
          &scoped index-eb USE-INDEX cust 

                &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-eb}   NO-LOCK  {&index-eb} ,    ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty}  NO-LOCK, ~
                 {&for-ef} NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


      END.


 ELSE IF vi_part-no <> "" THEN do:
     
          &scoped index-eb USE-INDEX part 
          

                &SCOPED-DEFINE open-query                   ~
                 OPEN QUERY {&browse-name}               ~
                 {&for-eb}   NO-LOCK  {&index-eb} ,   ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty}  NO-LOCK, ~
                 {&for-ef}  NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


      END.
  ELSE IF vi_stock-no <> "" THEN do:
          &scoped index-eb USE-INDEX stock 


                &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-eb}  NO-LOCK  {&index-eb} ,     ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty}  NO-LOCK, ~
                 {&for-ef}  NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


  END.

  ELSE IF vi_style <> "" THEN do:
          &scoped index-eb USE-INDEX style
          
         &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-eb} NO-LOCK {&index-eb} ,   ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty}  NO-LOCK, ~
                 {&for-ef} NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 


  END.
  ELSE IF vi_len <> 0 THEN do:
/*
      &SCOPED-DEFINE sortby-phrase-len-asc  ~
                    BY eb.len       
                    

     &SCOPED-DEFINE sortby-phrase-len-desc ~
             BY eb.len  DESC  
     
*/
    &scoped index-eb USE-INDEX size 
    &SCOPED-DEFINE open-query                   ~
        OPEN QUERY {&browse-name}               ~
                 {&for-eb}  NO-LOCK {&index-eb} ,   ~
                 {&for-est} NO-LOCK, ~
                 {&for-eqty}  NO-LOCK, ~
                 {&for-ef} NO-LOCK
  
        
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
                   {&for-eb} NO-LOCK ,  ~
                   {&for-est} NO-LOCK, ~
                   {&for-eqty}  NO-LOCK, ~
                   {&for-ef}  NO-LOCK
  

          IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                         ELSE {&open-query} {&sortby-phrase-desc}. 

    /*  END. */
END.

lv-first-run = no.

RUN dispatch ('get-last':U).

IF AVAIL eb THEN
  ASSIGN
   lv-last-rowid  = ROWID(est)
   lv-last-rowid2 = ROWID(eb).
    
RUN dispatch ('get-first':U).

IF AVAIL eb THEN
  ASSIGN
   lv-frst-rowid  = ROWID(est)
   lv-frst-rowid2 = ROWID(eb).

    

  

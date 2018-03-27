
IF fi_est-no NE "" THEN fi_est-no = FILL(" ",8 - LENGTH(TRIM(fi_est-no))) + TRIM(fi_est-no).
IF fi_job-no NE "" THEN fi_job-no = FILL(" ",6 - LENGTH(TRIM(fi_job-no))) + TRIM(fi_job-no).

IF fi_ord-no NE 0 THEN DO:
  IF fi_cust-no EQ "" AND
     fi_i-no    EQ "" AND
     fi_part-no EQ "" AND
     fi_est-no  EQ "" AND
     fi_job-no  EQ "" THEN DO:
     &SCOPED-DEFINE joinScop OUTER-JOIN
     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-eachblank}                      ~
                 AND oe-ordl.ord-no EQ fi_ord-no ~
               USE-INDEX ord-no NO-LOCK,         ~
               {&for-each2}
     &SCOPED-DEFINE joinScop 
     &SCOPED-DEFINE open-query-cad             ~
       OPEN QUERY {&browse-name}               ~
         {&for-eachblank}                      ~
               AND oe-ordl.ord-no EQ fi_ord-no ~
             USE-INDEX ord-no NO-LOCK,         ~
             {&for-each2}
  END.
  ELSE DO:
     &SCOPED-DEFINE joinScop OUTER-JOIN
     &SCOPED-DEFINE open-query                   ~
         OPEN QUERY {&browse-name}               ~
           {&for-each1}                          ~
                 AND oe-ordl.ord-no EQ fi_ord-no ~
               USE-INDEX ord-no NO-LOCK,         ~
               {&for-each2}
     &SCOPED-DEFINE joinScop 
     &SCOPED-DEFINE open-query-cad             ~
       OPEN QUERY {&browse-name}               ~
         {&for-each1}                          ~
               AND oe-ordl.ord-no EQ fi_ord-no ~
             USE-INDEX ord-no NO-LOCK,         ~
             {&for-each2}
  END.
  {oeinq/j-ordinq1.i}
END. /* Ord-no was entered */
/* Ord-no was not entered */
/*ELSE IF fi_po-no1 NE "" THEN DO:           */
/*  IF INDEX(fi_po-no1, "*") GT 0  THEN DO:  */
/*    &SCOPED-DEFINE joinScop OUTER-JOIN     */
/*    &SCOPED-DEFINE open-query            ~ */
/*        OPEN QUERY {&browse-name}        ~ */
/*            {&for-each11}                 ~*/
/*                USE-INDEX po-no NO-LOCK, ~ */
/*                {&for-each2}               */
/*    &SCOPED-DEFINE joinScop                */
/*    &SCOPED-DEFINE open-query-cad      ~   */
/*        OPEN QUERY {&browse-name}      ~   */
/*            {&for-each1}               ~   */
/*                USE-INDEX po-no NO-LOCK, ~ */
/*                {&for-each2}               */
/*    {oeinq/j-ordinq1.i}                    */
/*  END.                                     */
/*  ELSE DO:                                 */
/*    &SCOPED-DEFINE joinScop OUTER-JOIN     */
/*    &SCOPED-DEFINE open-query            ~ */
/*        OPEN QUERY {&browse-name}        ~ */
/*            {&for-each1}                 ~ */
/*                USE-INDEX po-no NO-LOCK, ~ */
/*                {&for-each2}               */
/*    &SCOPED-DEFINE joinScop                */
/*    &SCOPED-DEFINE open-query-cad      ~   */
/*        OPEN QUERY {&browse-name}      ~   */
/*            {&for-each1}               ~   */
/*                USE-INDEX po-no NO-LOCK, ~ */
/*                {&for-each2}               */
/*    {oeinq/j-ordinq1.i}                    */
/*  END.                                     */
/*END.                                       */
ELSE IF fi_i-no NE "" THEN DO:
  IF INDEX(fi_i-no, "*") GT 0  THEN DO:
    &SCOPED-DEFINE joinScop OUTER-JOIN
    &SCOPED-DEFINE open-query           ~
        OPEN QUERY {&browse-name}       ~
            {&for-each11}                ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}
    &SCOPED-DEFINE joinScop 
    &SCOPED-DEFINE open-query-cad       ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}    
    {oeinq/j-ordinq1.i}
  END.
  ELSE DO:
    &SCOPED-DEFINE joinScop OUTER-JOIN
    &SCOPED-DEFINE open-query           ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}
    &SCOPED-DEFINE joinScop 
    &SCOPED-DEFINE open-query-cad       ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}    
    {oeinq/j-ordinq1.i}
  END.
END.
ELSE IF fi_part-no NE "" THEN DO:
  IF INDEX(fi_part-no, "*") GT 0  THEN DO:
    &SCOPED-DEFINE joinScop OUTER-JOIN
    &SCOPED-DEFINE open-query           ~
        OPEN QUERY {&browse-name}       ~
            {&for-each11}                ~
                USE-INDEX part NO-LOCK, ~
                {&for-each2}
    &SCOPED-DEFINE joinScop 
    &SCOPED-DEFINE open-query-cad       ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}    
    {oeinq/j-ordinq1.i}
  END.
  ELSE DO:
    &SCOPED-DEFINE joinScop OUTER-JOIN
    &SCOPED-DEFINE open-query           ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
                USE-INDEX part NO-LOCK, ~
                {&for-each2}
    &SCOPED-DEFINE joinScop 
    &SCOPED-DEFINE open-query-cad       ~
        OPEN QUERY {&browse-name}       ~
            {&for-each1}                ~
                USE-INDEX item NO-LOCK, ~
                {&for-each2}    
    {oeinq/j-ordinq1.i}
  END.
END.
ELSE IF fi_job-no NE "" THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              USE-INDEX job NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad      ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              USE-INDEX job NO-LOCK, ~
              {&for-each2}  
  {oeinq/j-ordinq1.i}
END.
ELSE IF fi_est-no NE "" THEN DO:
  &SCOPED-DEFINE joinScop OUTER-JOIN
  &SCOPED-DEFINE open-query          ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              USE-INDEX est NO-LOCK, ~
              {&for-each2}
  &SCOPED-DEFINE joinScop 
  &SCOPED-DEFINE open-query-cad      ~
      OPEN QUERY {&browse-name}      ~
          {&for-each1}               ~
              USE-INDEX est NO-LOCK, ~
              {&for-each2}  
  {oeinq/j-ordinq1.i}
END.
ELSE DO:
  IF fi_cust-no EQ "" AND
     fi_i-no    EQ "" AND
     fi_part-no EQ "" AND
     fi_est-no  EQ "" AND
     Fi_job-no  EQ "" THEN DO:
     &SCOPED-DEFINE joinScop OUTER-JOIN
     &SCOPED-DEFINE open-query             ~
        OPEN QUERY {&browse-name}         ~
            {&for-eachblank}              ~
                USE-INDEX opened NO-LOCK, ~
                {&for-each2}
     &SCOPED-DEFINE joinScop 
     &SCOPED-DEFINE open-query-cad         ~
        OPEN QUERY {&browse-name}         ~
            {&for-eachblank}              ~
                USE-INDEX opened NO-LOCK, ~
                {&for-each2}
  END.
  ELSE DO:
     &SCOPED-DEFINE joinScop OUTER-JOIN
     &SCOPED-DEFINE open-query             ~
        OPEN QUERY {&browse-name}         ~
            {&for-each1}                  ~
                USE-INDEX opened NO-LOCK, ~
                {&for-each2}
     &SCOPED-DEFINE joinScop 
     &SCOPED-DEFINE open-query-cad         ~
        OPEN QUERY {&browse-name}         ~
            {&for-each1}                  ~
                USE-INDEX opened NO-LOCK, ~
                {&for-each2}
  END.  
  {oeinq/j-ordinq1.i}
END.

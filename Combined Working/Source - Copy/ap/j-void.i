/* ap/j-void.i */
/*
CASE browse-order :
    when 1 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-pay WHERE ASI.ap-pay.company = g_company
                                AND ASI.ap-pay.reconciled = no and
                                ap-pay.check-amt <> 0 and
                                ap-pay.check-no < 90000000 AND 
                                ap-pay.bank-code BEGINS auto_find NO-LOCK
                                BY ap-pay.bank-code.
    END.
    when 2 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-pay WHERE ASI.ap-pay.company = g_company
                                AND ASI.ap-pay.reconciled = no and
                                ap-pay.check-amt <> 0 and
                                ap-pay.check-no < 90000000 AND 
                                ap-pay.check-no >= INT(auto_find) NO-LOCK
                                BY ap-pay.check-no.
              
    END.   
    when 3 THEN DO:
         OPEN QUERY {&browse-name}  FOR EACH ASI.ap-pay WHERE ASI.ap-pay.company = g_company
                                AND ASI.ap-pay.reconciled = no and
                                ap-pay.check-amt <> 0 and
                                ap-pay.check-no < 90000000 AND 
                                ap-pay.vend-no BEGINS auto_find NO-LOCK
                                BY ap-pay.vend-no.
              
    END.   
    /*
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    */
   
END CASE. */

IF fi_fchk NE 0 THEN DO:
  &SCOPED-DEFINE open-query                   ~
      OPEN QUERY {&browse-name}               ~
        {&for-each1}                          ~
              AND ap-pay.check-no EQ fi_fchk ~
            USE-INDEX ap-pay NO-LOCK,    ~
            {&for-each2}

  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}. 
END.
    
ELSE DO:
  &SCOPED-DEFINE open-query              ~
      OPEN QUERY {&browse-name}          ~
          {&for-each1}                   ~
              USE-INDEX vend-no NO-LOCK, ~
              {&for-each2}
  
  IF ll-sort-asc THEN {&open-query} {&sortby-phrase-asc}.
                 ELSE {&open-query} {&sortby-phrase-desc}.
END.



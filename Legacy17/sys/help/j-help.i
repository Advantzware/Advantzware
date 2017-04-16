/* sys/help/j-help.i */

CASE browse-order :

    when 1 THEN DO: /* Field name*/
      IF td_status THEN DO:
       OPEN QUERY {&browse-name}  FOR EACH hlp-head WHERE hlp-head.Showinglossary = td_status
                                                        AND string(hlp-head.FLD-NAME) BEGINS auto_find
                  NO-LOCK {&SORTBY-PHRASE}
                  BY hlp-head.FLD-NAME .
     END.
     ELSE DO:
       OPEN QUERY {&browse-name}  FOR EACH hlp-head WHERE string(hlp-head.FLD-NAME) BEGINS auto_find
                  NO-LOCK {&SORTBY-PHRASE}
                  BY hlp-head.FLD-NAME .
     END.
    END.
    when 2 THEN DO: /*tital*/ 
      IF td_status THEN DO:
           OPEN QUERY {&browse-name}  FOR EACH hlp-head WHERE hlp-head.Showinglossary = td_status 
                                                            AND hlp-head.FRM-TITLE BEGINS auto_find

                  NO-LOCK {&SORTBY-PHRASE}
                  BY hlp-head.FRM-TITLE.
      END.
      ELSE DO:
	 OPEN QUERY {&browse-name}  FOR EACH hlp-head WHERE hlp-head.FRM-TITLE BEGINS auto_find

                  NO-LOCK {&SORTBY-PHRASE}
                  BY hlp-head.FRM-TITLE.
      END.
    END.
END CASE.

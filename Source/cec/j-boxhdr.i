/* cec/j-boxhdr.i */

CASE browse-order :
     WHEN 1 THEN DO:
         OPEN QUERY {&browse-name} 
              FOR EACH ASI.box-design-hdr WHERE box-design-hdr.company = gcompany and
                                            box-design-hdr.design-no <> 0 and
                                            string(box-design-hdr.design-no) BEGINS auto_find NO-LOCK
                   BY box-design-hdr.design-no.
    END.
    WHEN 2 THEN DO:
         OPEN QUERY {&browse-name} 
             FOR EACH ASI.box-design-hdr WHERE box-design-hdr.company = gcompany and
                                           box-design-hdr.design-no <> 0 and
                                           box-design-hdr.DESCRIPTION BEGINS auto_find NO-LOCK
                   BY box-design-hdr.DESCRIPTION.
    END.
    WHEN 3 THEN DO:
          OPEN QUERY {&browse-name} 
              FOR EACH ASI.box-design-hdr WHERE box-design-hdr.company = gcompany and
                                            box-design-hdr.design-no <> 0 and
                                            box-design-hdr.est-no BEGINS auto_find NO-LOCK
                    BY box-design-hdr.est-no.
    END.
    
    OTHERWISE DO:
        {&open-query-{&browse-name}} 
    END.
    
END CASE.

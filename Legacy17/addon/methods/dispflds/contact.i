/* contact.i */

WHEN "{&FIRST-EXTERNAL-TABLE}.contact-loc" THEN
DO:
  IF {&FIRST-EXTERNAL-TABLE}.contact-loc:SCREEN-VALUE eq "C" then
  DO:
    FIND cust
      WHERE cust.company = gcompany
        AND cust.cust-no = {&FIRST-EXTERNAL-TABLE}.cust-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
    IF avail cust then
      ASSIGN
        contact_addr1   = cust.addr[1]
        contact_addr2   = cust.addr[2]
        contact_city    = cust.city
        contact_state   = cust.state
        contact_zip     = cust.zip
        contact_country = cust.country
        contact_territory = cust.terr
        contact_sman    = cust.sman
        contact_cust-name = cust.name.
  END.
  IF {&FIRST-EXTERNAL-TABLE}.contact-loc:SCREEN-VALUE eq "S" then
  DO:
    FIND shipto
      WHERE shipto.company = gcompany
        AND shipto.ship-id = {&FIRST-EXTERNAL-TABLE}.ship-id:SCREEN-VALUE 
        AND shipto.cust-no = {&FIRST-EXTERNAL-TABLE}.cust-no:SCREEN-VALUE
      NO-LOCK NO-ERROR.
    IF avail shipto then
    DO:
      ASSIGN
        contact_addr1   = shipto.ship-addr[1]
        contact_addr2   = shipto.ship-addr[2]
        contact_city    = shipto.ship-city
        contact_state   = shipto.ship-state
        contact_zip     = shipto.ship-zip
        contact_country = shipto.country.
      FIND cust
        WHERE cust.company = gcompany
          AND cust.cust-no = {&FIRST-EXTERNAL-TABLE}.cust-no:SCREEN-VALUE
        NO-LOCK NO-ERROR.
      IF avail cust then
        ASSIGN
          contact_territory = cust.terr
          contact_sman = cust.sman
          contact_cust-name = cust.name.
    END.
  END.
  DISPLAY contact_addr1
          contact_addr2
          contact_city
          contact_state
          contact_zip
          contact_country
          contact_territory
          contact_sman
          contact_cust-name with frame {&frame-name}.
END.

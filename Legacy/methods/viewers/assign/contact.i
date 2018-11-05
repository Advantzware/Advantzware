/* contact.i */


FIND first cust where cust.company eq gcompany
                  and cust.cust-no eq contact.cust-no:screen-value
                no-lock no-error.
if avail cust and contact_sman = "" then
   assign contact.sman = cust.sman.
else contact.sman = contact_sman.

/*IF adm-new-record THEN  */
DO WITH FRAME {&FRAME-NAME}:
         
  assign contact.cust-name = contact_cust-name
         contact.addr1 = contact_addr1
         contact.addr2 = contact_addr2
         contact.city  = contact_city
         contact.state = contact_state
         contact.zip   = contact_zip
         contact.country = contact_country
         contact.county = contact_county
         contact.territory = contact_territory
         .

    disable contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_territory
            contact_sman
    WITH FRAME {&FRAME-NAME}.
 
END.

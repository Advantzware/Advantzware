/* contact.i */

  if /* (contact_cust-name eq ? or contact_cust-name = "")
     and */
     contact.cust-no:screen-value eq ""  and contact.ship-id:screen-value = ""
     and contact.contact-loc:screen-value = ?
  then
  do:
    if contact.cust-name eq "" then contact_cust-name = "".
    if contact.addr1     eq "" then contact_addr1 = "".
    if contact.addr2     eq "" then contact_addr2 = "".
    if contact.city      eq "" then contact_city = "".
    if contact.state     eq "" then contact_state = "".
    if contact.zip       eq "" then contact_zip = "".
    if contact.country   eq "" then contact_country = "".
    if contact.territory   eq "" then contact_territory = "".
  
    assign  contact_cust-name:fgcolor = ?
            contact_addr1:fgcolor = ? 
            contact_addr2:fgcolor = ?
            contact_city:fgcolor = ?
            contact_state:fgcolor = ?
            contact_zip:fgcolor = ?
            contact_country:fgcolor = ?
            contact_county:fgcolor = ?
            contact_territory:fgcolor = ?
            contact_cust-name:bgcolor = 15
            contact_addr1:bgcolor = 15
            contact_addr2:bgcolor = 15
            contact_city:bgcolor = 15
            contact_state:bgcolor = 15
            contact_zip:bgcolor = 15
            contact_country:bgcolor = 15
            contact_county:bgcolor = 15
            contact_territory:bgcolor = 15
            .

    enable  contact_sman
            contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
            
     WITH FRAME {&FRAME-NAME}.
  end.


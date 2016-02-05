/* contact.i */
  assign  contact_cust-name:fgcolor = ?
            contact_addr1:fgcolor = ? 
            contact_addr2:fgcolor = ?
            contact_city:fgcolor = ?
            contact_state:fgcolor = ?
            contact_zip:fgcolor = ?
            contact_country:fgcolor = ?
            contact_county:fgcolor = ?
            contact_territory:fgcolor = ?
            contact_cust-name:bgcolor = 7
            contact_addr1:bgcolor = 7
            contact_addr2:bgcolor = 7
            contact_city:bgcolor = 7
            contact_state:bgcolor = 7
            contact_zip:bgcolor = 7
            contact_country:bgcolor = 7
            contact_county:bgcolor = 7
            contact_territory:bgcolor = 7
            .

    disable contact_cust-name
            contact_addr1 
            contact_addr2
            contact_city
            contact_state
            contact_zip
            contact_country
            contact_county
            contact_territory
            contact_sman
     WITH FRAME {&FRAME-NAME}.

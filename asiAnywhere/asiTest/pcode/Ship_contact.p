

/*------------------------------------------------------------------------
    File        : Ship_contact.p
    Purpose     : Download contacts for ShipTO

    Author(s)   : Kuldeep
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttContact3 NO-UNDO
    FIELD company         LIKE  contact.company     
    FIELD cust-no         LIKE contact.cust-no      
    FIELD ship-id         LIKE contact.ship-id    
    FIELD sman            LIKE contact.sman
    FIELD first-name      LIKE contact.first-name
    FIELD last-name       LIKE contact.last-name
    FIELD middle-initial  LIKE contact.middle-initial
    FIELD sirname         LIKE contact.sirname
    FIELD contact-title   LIKE contact.contact-title
    FIELD contact-loc     LIKE contact.contact-loc
    FIELD cust-name       LIKE contact.cust-name
    FIELD addr1           LIKE contact.addr1
    FIELD addr2           LIKE contact.addr2
    FIELD city            LIKE contact.city
    FIELD state           LIKE contact.state
    FIELD zip             LIKE contact.zip
    FIELD country         LIKE contact.country
    FIELD territory       LIKE contact.territory
    FIELD access-code     LIKE contact.access-code
    FIELD phone           LIKE contact.phone
    FIELD cell-phone      LIKE contact.cell-phone
    FIELD fax             LIKE contact.fax
    FIELD extension       LIKE contact.extension
    FIELD email           LIKE contact.email
    FIELD website         LIKE contact.website
    FIELD rec_key         LIKE contact.rec_key
    .
    DEFINE DATASET dsContactList3 FOR ttContact3.

    DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp    AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmCust    AS CHARACTER  NO-UNDO.
    
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsContactList3.
    def var i as int initial 0 no-undo.

    IF prmAction       = ?  THEN ASSIGN prmAction     = "Select".
    IF prmComp        = ?   THEN ASSIGN prmComp      = "".
    IF prmUser        = ?   THEN ASSIGN prmUser      = "".
    IF prmCust        = ?   THEN ASSIGN prmCust      = "".

    IF prmComp EQ "" THEN DO:
         FIND FIRST usercomp WHERE
             usercomp.user_id = prmUser AND
             usercomp.loc = '' AND
             usercomp.company_default = YES
             NO-LOCK NO-ERROR.
         prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   END.
   IF prmAction = "Select" THEN DO:
     FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.company = prmComp NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            FOR EACH shipto WHERE shipto.cust-no = cust.cust-no AND shipto.company = prmComp NO-LOCK:
                FOR EACH phone WHERE phone.table_rec_key = shipto.rec_key NO-LOCK: 
                    CREATE ttContact3.
                    ASSIGN 
                     ttContact3.company         = prmComp       
                     ttContact3.cust-no         = shipto.cust-no       
                     ttContact3.ship-id         = shipto.ship-id       
                     ttContact3.first-name      = phone.attention
                     ttContact3.contact-title   = phone.titlcode
                     ttContact3.contact-loc     = "S"   
                     ttContact3.cust-name       = shipto.ship-name     
                     ttContact3.addr1           = shipto.ship-addr[1]         
                     ttContact3.addr2           = shipto.ship-addr[2]         
                     ttContact3.city            = shipto.ship-city          
                     ttContact3.state           = shipto.ship-state         
                     ttContact3.zip             = shipto.ship-zip 
                     ttContact3.country         = shipto.country
                     ttContact3.territory       = shipto.dest-code
                     ttContact3.phone           = phone.phone         
                     ttContact3.fax             = phone.fax           
                     ttContact3.extension       = phone.phone_ext     
                     ttContact3.email           = phone.e_mail         
                     ttContact3.rec_key         = phone.rec_key .

                    if phone.attention ne "" then do i = 1 to length(trim(phone.attention)):
                    if substring(phone.attention,i,1) ne " " then      next.
                    else do:
                        assign ttContact3.first-name = substring(phone.attention,1,i)
                            ttContact3.last-name  = substring(phone.attention,i + 1,length(trim(phone.attention))).        
                        leave.
                     end.
               end.
               END. /* FOR EACH phone*/
           END. /* FOR EACH cust*/
       END.  /*FOR EACH shipto*/
     END. /* FOR EACH usercust*/
   END.
      
   IF prmAction = "Search" THEN DO:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
           AND usercust.company EQ prmComp AND (usercust.cust-no = prmCust OR usercust.cust-no BEGINS prmCust) NO-LOCK:
            FOR EACH shipto WHERE (shipto.cust-no = usercust.cust-no) AND shipto.company = prmComp NO-LOCK:
           FOR EACH phone WHERE phone.table_rec_key = shipto.rec_key NO-LOCK: 
               CREATE ttContact3.
               ASSIGN 
                     ttContact3.company         = prmComp       
                     ttContact3.cust-no         = shipto.cust-no       
                     ttContact3.ship-id         = shipto.ship-id       
                     ttContact3.first-name      = phone.attention
                     ttContact3.contact-title   = phone.titlcode
                     ttContact3.contact-loc     = "S"   
                     ttContact3.cust-name       = shipto.ship-name     
                     ttContact3.addr1           = shipto.ship-addr[1]         
                     ttContact3.addr2           = shipto.ship-addr[2]         
                     ttContact3.city            = shipto.ship-city          
                     ttContact3.state           = shipto.ship-state         
                     ttContact3.zip             = shipto.ship-zip 
                     ttContact3.country         = shipto.country
                     ttContact3.territory       = shipto.dest-code
                     ttContact3.phone           = phone.phone         
                     ttContact3.fax             = phone.fax           
                     ttContact3.extension       = phone.phone_ext     
                     ttContact3.email           = phone.e_mail         
                     ttContact3.rec_key         = phone.rec_key .

                    if phone.attention ne "" then do i = 1 to length(trim(phone.attention)):
                    if substring(phone.attention,i,1) ne " " then      next.
                    else do:
                        assign ttContact3.first-name = substring(phone.attention,1,i)
                            ttContact3.last-name  = substring(phone.attention,i + 1,length(trim(phone.attention))).        
                        leave.
                     end.
               end.
               END. /* FOR EACH phone*/
           END.  /*FOR EACH shipto*/
    END.
   END.
      








/*------------------------------------------------------------------------
    File        : Cust_conatct.p
    Purpose     : Download contacts for custumers

    Author(s)   : Kuldeep
    Created     : Apr 10, 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttContact2 NO-UNDO
    FIELD con1  AS CHARACTER
    FIELD company         LIKE  contact.company     
    FIELD cust-no         LIKE contact.cust-no      
    FIELD ship-id         LIKE contact.ship-id    
    FIELD sman            LIKE contact.sman
    FIELD first-name      LIKE contact.first-name
    FIELD last-name       LIKE contact.last-name
    FIELD middle-initial  LIKE contact.middle-initial
    FIELD sirname         LIKE contact.sirname
    FIELD contact-title   LIKE contact.contact-title
    FIELD maillist        LIKE contact.maillist 
    FIELD TYPE            LIKE contact.TYPE
    FIELD contact-loc     LIKE contact.contact-loc
    FIELD cust-name       LIKE contact.cust-name
    FIELD addr1           LIKE contact.addr1
    FIELD addr2           LIKE contact.addr2
    FIELD city            LIKE contact.city
    FIELD state           LIKE contact.state
    FIELD zip             LIKE contact.zip
    FIELD country         LIKE contact.country
    FIELD county          LIKE contact.county
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
    DEFINE DATASET dsContactList2 FOR ttContact2.

    DEFINE INPUT PARAMETER prmAction   AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmComp    AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmCust AS CHAR NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsContactList2.
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
   /*************************************************************************/
   IF prmAction = "Search" THEN DO:
       FOR EACH usercust WHERE usercust.user_id = prmUser 
           AND usercust.company EQ prmComp AND (usercust.cust-no = prmCust OR usercust.cust-no BEGINS prmCust) NO-LOCK:
           FOR EACH cust WHERE cust.cust-no = usercust.cust-no AND cust.company = prmComp NO-LOCK :
               FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK: 
                   CREATE ttContact2.
                   ASSIGN 
                    ttContact2.company         = cust.company       
                    ttContact2.cust-no         = cust.cust-no       
                    ttContact2.sman            = cust.sman          
                    ttContact2.first-name      = phone.attention
                    ttContact2.contact-title   = phone.titlcode
                    ttContact2.TYPE            = cust.TYPE          
                    ttContact2.contact-loc     = "C"   
                    ttContact2.cust-name       = cust.name     
                    ttContact2.addr1           = cust.addr[1]         
                    ttContact2.addr2           = cust.addr[2]         
                    ttContact2.city            = cust.city          
                    ttContact2.state           = cust.state         
                    ttContact2.zip             = cust.zip
                    ttContact2.country         = cust.country
                    ttContact2.territory       = cust.terr
                    ttContact2.phone           = phone.phone         
                    ttContact2.fax             = phone.fax           
                    ttContact2.extension       = phone.phone_ext     
                    ttContact2.email           = phone.e_mail         
                    ttContact2.rec_key         = phone.rec_key .
                
                if phone.attention ne "" then do i = 1 to length(trim(phone.attention)):
                    if substring(phone.attention,i,1) ne " " then      next.
                    else do:
                        assign ttContact2.first-name = substring(phone.attention,1,i)
                            ttContact2.last-name  = substring(phone.attention,i + 1,length(trim(phone.attention))).        
                        leave.
                     end.
               end.
          END. /* FOR EACH phone*/
       END. /* FOR EACH cust*/
       END.
    END.
/*************************************************************/
   
   IF prmAction = "Select" THEN DO:
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.company = prmComp NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK: 
                CREATE ttContact2.
                ASSIGN 
                    ttContact2.company         = cust.company       
                    ttContact2.cust-no         = cust.cust-no       
                    ttContact2.sman            = cust.sman          
                    ttContact2.first-name      = phone.attention
                    ttContact2.contact-title   = phone.titlcode
                    ttContact2.TYPE            = cust.TYPE          
                    ttContact2.contact-loc     = "C"   
                    ttContact2.cust-name       = cust.name     
                    ttContact2.addr1           = cust.addr[1]         
                    ttContact2.addr2           = cust.addr[2]         
                    ttContact2.city            = cust.city          
                    ttContact2.state           = cust.state         
                    ttContact2.zip             = cust.zip
                    ttContact2.country         = cust.country
                    ttContact2.territory       = cust.terr
                    ttContact2.phone           = phone.phone         
                    ttContact2.fax             = phone.fax           
                    ttContact2.extension       = phone.phone_ext     
                    ttContact2.email           = phone.e_mail         
                    ttContact2.rec_key         = phone.rec_key .
                
                if phone.attention ne "" then do i = 1 to length(trim(phone.attention)):
                    if substring(phone.attention,i,1) ne " " then      next.
                    else do:
                        assign ttContact2.first-name = substring(phone.attention,1,i)
                            ttContact2.last-name  = substring(phone.attention,i + 1,length(trim(phone.attention))).        
                        leave.
                     end.
               end.
          END. /* FOR EACH phone*/
       END. /* FOR EACH cust*/
      END. /* FOR EACH usercust*/
   END.
    



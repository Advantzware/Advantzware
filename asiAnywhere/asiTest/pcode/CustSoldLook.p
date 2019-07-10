
/*------------------------------------------------------------------------
    File        : custsoldtolook.p
    Purpose     : Cust sold To lookup 

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttCustSoldLook NO-UNDO 
    FIELD custnum AS CHARACTER
    FIELD soldid AS CHARACTER
    FIELD Name AS CHARACTER
    FIELD Addr1 AS CHARACTER
    FIELD Addr2 AS CHARACTER
    FIELD City1 AS CHARACTER
    FIELD State1 AS CHARACTER
    FIELD Zip1 AS CHARACTER
    FIELD jhlnf AS CHAR.
                                           
    
DEFINE DATASET dsCustSoldLook FOR ttCustSoldLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmSold      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustSoldLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField    = "".
IF prmSold      = ? THEN ASSIGN prmSold      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp AND (usercust.cust-no = prmSold OR prmSold="")  NO-LOCK:
    FOR EACH soldto WHERE 
        soldto.company = prmComp AND soldto.cust-no = usercust.cust-no  NO-LOCK:
        create ttCustSoldLook.
        assign                                     
            ttCustSoldLook.custnum =  soldto.cust-no
            ttCustSoldLook.soldid    = soldto.sold-id
            ttCustSoldLook.Name = soldto.sold-name 
            ttCustSoldLook.Addr1    = soldto.sold-addr[1]
            ttCustSoldLook.Addr2    = soldto.sold-addr[2]
            ttCustSoldLook.City1 = soldto.sold-City
            ttCustSoldLook.State1    = soldto.sold-state
            ttCustSoldLook.Zip1 = soldto.sold-zip
            .
    END.  /*FOR EACH usercust */
    END.  /*FOR EACH soldto WHERE*/
END.  /*ifif prmAction <> "search" */


IF prmAction = "search" then do:
    IF prmField = "soldid"  THEN DO:
        IF prmCondition = "EQUAL" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp AND (usercust.cust-no = prmSold OR prmSold="")  NO-LOCK:
    FOR EACH soldto WHERE 
        soldto.company = prmComp AND soldto.cust-no = usercust.cust-no AND soldto.sold-id = prmText  NO-LOCK:

               
                  create ttCustSoldLook.
             assign                                     
                ttCustSoldLook.custnum =  soldto.cust-no
                ttCustSoldLook.soldid    = soldto.sold-id
                ttCustSoldLook.Name = soldto.sold-name 
                ttCustSoldLook.Addr1    = soldto.sold-addr[1]
                 ttCustSoldLook.Addr2    = soldto.sold-addr[2]
                ttCustSoldLook.City1 = soldto.sold-City
                ttCustSoldLook.State1    = soldto.sold-state
                ttCustSoldLook.Zip1 = soldto.sold-zip
                .
                END.
                END.
           
                
            /*FOR EACH cust where cust.i-dscr = prmText*/
        END.  /*IF prmCondition = EQUAL*/

        IF prmCondition = "BEGIN" then do:
           
           FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp AND (usercust.cust-no = prmSold OR prmSold="")  NO-LOCK:
    FOR EACH soldto WHERE 
        soldto.company = prmComp AND soldto.cust-no = usercust.cust-no AND soldto.sold-id BEGINS prmText  NO-LOCK:

        create ttCustSoldLook.
        assign                                     
            ttCustSoldLook.custnum =  soldto.cust-no
            ttCustSoldLook.soldid    = soldto.sold-id
            ttCustSoldLook.Name = soldto.sold-name 
            ttCustSoldLook.Addr1    = soldto.sold-addr[1]
            ttCustSoldLook.Addr2    = soldto.sold-addr[2]
            ttCustSoldLook.City1 = soldto.sold-City
            ttCustSoldLook.State1    = soldto.sold-state
            ttCustSoldLook.Zip1 = soldto.sold-zip
            .
            
            END.
         END.  /*FOR EACH item where */         
    END. /*IF prmCondition = BEGIN then do:*/  
    END. /* end of field*/

IF prmField = "sold-name" then do:
         if prmCondition = "EQUAL" then do:
           FOR EACH usercust WHERE usercust.user_id = prmUser 
               AND usercust.company EQ prmComp AND (usercust.cust-no = prmSold OR prmSold="")  NO-LOCK:
               FOR EACH soldto WHERE soldto.company = prmComp AND soldto.cust-no = usercust.cust-no 
                    AND soldto.sold-name = prmText  NO-LOCK:
                   create ttCustSoldLook.
                   assign                                     
                       ttCustSoldLook.custnum =  soldto.cust-no
                       ttCustSoldLook.soldid    = soldto.sold-id
                       ttCustSoldLook.Name = soldto.sold-name 
                       ttCustSoldLook.Addr1    = soldto.sold-addr[1]
                       ttCustSoldLook.Addr2    = soldto.sold-addr[2]
                       ttCustSoldLook.City1 = soldto.sold-City
                       ttCustSoldLook.State1    = soldto.sold-state
                       ttCustSoldLook.Zip1 = soldto.sold-zip
                       .
               END.
             END. /*FOR EACH item where*/
          END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
               FOR EACH usercust WHERE usercust.user_id = prmUser 
               AND usercust.company EQ prmComp AND (usercust.cust-no = prmSold OR prmSold="")  NO-LOCK:
               FOR EACH soldto WHERE soldto.company = prmComp AND soldto.cust-no = usercust.cust-no 
                    AND soldto.sold-name BEGINS prmText  NO-LOCK:
                create ttCustSoldLook.
             assign                                     
                ttCustSoldLook.custnum =  soldto.cust-no
                ttCustSoldLook.soldid    = soldto.sold-id
                ttCustSoldLook.Name = soldto.sold-name 
                ttCustSoldLook.Addr1    = soldto.sold-addr[1]
                 ttCustSoldLook.Addr2    = soldto.sold-addr[2]
                ttCustSoldLook.City1 = soldto.sold-City
                ttCustSoldLook.State1    = soldto.sold-state
                ttCustSoldLook.Zip1 = soldto.sold-zip
                .
               END.
             END. /*FOR EACH item where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = i-no */

END.  /* end of search */
    


       
    


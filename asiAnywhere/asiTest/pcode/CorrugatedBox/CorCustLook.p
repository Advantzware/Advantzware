/*------------------------------------------------------------------------
    File      : CorCustLook
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 2 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorCustomer NO-UNDO 
        FIELD CustNum  AS character
        FIELD CustName AS CHARACTER
        FIELD sname    AS CHARACTER
        FIELD Address1 AS CHARACTER
        FIELD Address2 AS CHARACTER
        FIELD city     AS CHARACTER
        FIELD state    AS CHARACTER              
        FIELD zip    AS CHARACTER              
        FIELD ShipName    AS CHARACTER
        FIELD ShipAddr1   AS CHARACTER
        FIELD ShipAddr2   AS CHARACTER
        FIELD ShipCity    AS CHARACTER
        FIELD ShipState   AS CHARACTER
        FIELD ShipZip     AS CHARACTER
        FIELD shipto      AS CHAR
        .
    
DEFINE DATASET dsCorCustomer FOR ttCorCustomer .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorCustomer.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".


DEFINE VAR v-count AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
/*FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
*/

if prmAction <> "search" then do:
    v-count = 0 . 
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
      EACH  cust WHERE cust.company = prmComp AND cust.cust-no = usercust.cust-no NO-LOCK:
        FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
        find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.
         IF AVAIL cust THEN DO:
             create ttCorCustomer.
             assign                                         
                 ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                 ttCorCustomer.Shipto   = shipto.ship-id.
             

             v-count = v-count + 1 .
             MESSAGE "count" v-count.
             IF v-count > 100 THEN LEAVE.
         END.	 /* IF AVAIL cust */
    END. /* for each cust */
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
             EACH cust WHERE cust.company = prmComp AND (cust.cust-no = prmText or cust.name = prmText) NO-LOCK:
                FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
                find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.   
                IF AVAIL cust THEN DO:
            create ttCorCustomer.
            assign                                         
                ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                ttCorCustomer.Shipto   = shipto.ship-id
                .
             
            
                END. /*IF AVAIL cust THEN DO:*/
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/

       IF prmCondition = "BEGIN" then do:
           FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
            EACH cust WHERE cust.company = prmComp AND (cust.cust-no begins prmText or cust.name begins prmText) NO-LOCK :
               FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
              find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.   
               IF AVAIL cust THEN DO:
               create ttCorCustomer.
               assign                                         
                  ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                   ttCorCustomer.Shipto   = shipto.ship-id
             
                   .
               END.  /*FOR EACH cust where */  
         END. /*FOR EACH cust WHERE */
      END. /*IF prmCondition = BEGIN then do:*/  
 END. /*IF prmField = ANY*/     

   

IF prmField = "cust-no" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
         EACH cust WHERE cust.company = prmComp AND cust.cust-no = prmText  NO-LOCK :
            FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
             find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.   
             IF AVAIL cust THEN DO:
                 create ttCorCustomer.
                 assign                                         
                   ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                 ttCorCustomer.Shipto   = shipto.ship-id
                     .
             
             END. /*if avail cust*/
        END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
     EACH cust WHERE cust.company = prmComp AND cust.cust-no BEGINS prmText  NO-LOCK :
        FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
             find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.   
             IF AVAIL cust THEN DO:
                 create ttCorCustomer.
                 assign                                         
                 ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                 ttCorCustomer.Shipto   = shipto.ship-id
                     .
             
           END. /*if avail cust*/
     END.  /*for each cust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = cust-no */


if prmField = "name"  then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
         EACH cust WHERE cust.company = prmComp AND  cust.name = prmText NO-LOCK :
            FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
            find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.   
             IF AVAIL cust THEN DO:
                 create ttCorCustomer.
                 assign 
                 ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                  ttCorCustomer.Shipto   = shipto.ship-id
                     .
             
                 END. /*if avail cust*/
           END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL */
    


IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.company = prmComp AND usercust.user_id = prmUser NO-LOCK ,
     EACH cust WHERE cust.company = prmComp AND  cust.name = prmText NO-LOCK :
        FIND FIRST shipto WHERE shipto.company = prmComp AND shipto.cust = cust.cust-no NO-LOCK NO-ERROR.
            find FIRST sman where sman.company = prmComp AND sman.sman = cust.sman no-lock no-error.   
             IF AVAIL cust THEN DO:
                 create ttCorCustomer.
                 assign 
                 ttCorCustomer.CustNum = cust.cust-no
                 ttCorCustomer.CustName = cust.name 
                 ttCorCustomer.city = cust.city
                 ttCorCustomer.state = cust.state 
                 ttCorCustomer.zip = cust.zip
                 ttCorCustomer.Address1 = cust.addr[1]
                 ttCorCustomer.Address2 = cust.addr[2]
                 ttCorCustomer.ShipName = shipto.ship-name
                 ttCorCustomer.ShipAddr1 = shipto.ship-addr[1]
                 ttCorCustomer.ShipAddr2 = shipto.ship-addr[2]
                 ttCorCustomer.ShipCity  = shipto.ship-city
                 ttCorCustomer.ShipState = shipto.ship-state
                 ttCorCustomer.ShipZip   = shipto.ship-zip
                  ttCorCustomer.Shipto   = shipto.ship-id
                     .
             
                 END. /*if avail cust*/
           END. /*FOR EACH cust where*/
    END. /*if prmCondition = EQUAL */
END. /*if prmfield*/
END. /*if prmAction = search*/

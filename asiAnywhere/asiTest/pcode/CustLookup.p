
/*------------------------------------------------------------------------
    File         : CustomerLookup
    Purpose     : customer lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{CustLookup.i}

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustomer.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

IF prmField      = ? THEN ASSIGN prmField      = "".
DEFINE VAR v-count AS INT NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
def var li-lead-days as int no-undo.
DEF VAR v-lead-days LIKE oe-ord.lead-days NO-UNDO.
  DEF VAR v-last-date LIKE oe-ord.last-date NO-UNDO.
  DEF VAR v-due-date  LIKE oe-ord.due-date  NO-UNDO.
  DEF VAR v-ord-date  LIKE oe-ord.ord-date  NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
/*prmComp = "001".*/

DEFINE NEW SHARED VAR cocode AS CHAR NO-UNDO.
ASSIGN 
    cocode = prmComp .

{sys/inc/lastship.i}

if prmAction <> "search" then do:
    v-count = 0 .
    
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            
            create ttCustomer.
            assign                                         
                ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms 
                   
                ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days ) 
                ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days
                .
           /* FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  .   */
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error. 
                IF AVAIL sman THEN
                    ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip
                    .

               ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .


                v-count = v-count + 1 .
                
                IF v-count > 100 THEN LEAVE.
           END.	 /* IF AVAIL cust */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH usercust WHERE usercust.user_id = prmUser 
                AND usercust.company EQ prmComp NO-LOCK:
                FIND FIRST cust WHERE cust.cust-no = usercust.cust-no
                    AND (cust.cust-no = prmText or cust.name = prmText) NO-LOCK NO-ERROR.
                IF AVAIL cust THEN DO:
          /*  FOR EACH cust where cust.company = prmComp  AND (cust.cust-no = prmText or cust.name = prmText)  no-lock: */
                
               create ttCustomer.
            assign                                         
                ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms    
               
                   
               ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days )
                ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days

                .
            /*FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  .  */ 
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error. 
                 IF AVAIL sman THEN
                     ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip

                    .

                ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .

               END. /*FOR EACH cust where cust.cust-no = prmText*/
            END.
        END.  /*IF prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no
            AND (cust.cust-no begins prmText or cust.name begins prmText) NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            /* FOR EACH cust where cust.company = prmComp  AND (cust.cust-no begins prmText or cust.name begins prmText)  no-lock: */
       create ttCustomer.
            assign                                         
                ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms    
               
                   
               ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days ) 
                ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days

                .
            /*FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  .  */ 
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error.            
                 IF AVAIL sman THEN
                     ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip

                    .

                ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .

    END.  /*FOR EACH cust where */  
    END.
END. /*IF prmCondition = BEGIN then do:*/  
 END. /*IF prmField = ANY*/     

   

IF prmField = "cust-no" then do:
    if prmCondition = "EQUAL" then do:
        
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no
            AND cust.cust-no = prmText  NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            
      /*  FOR EACH cust where cust.company = prmComp  AND cust.cust-no = prmText   no-lock: */
            create ttCustomer.
            assign                                         
                ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms    
               
                   
               ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days ) 
                ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days

                .
            /*FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  .   */
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error. 
                 IF AVAIL sman THEN
                     ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip

                    .

                ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .


        END. /*FOR EACH cust where*/
        END.
    END. /*if prmCondition = EQUAL*/
IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no
            AND cust.cust-no begins prmText NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
  /*  FOR EACH cust where cust.company = prmComp  AND cust.cust-no begins prmText no-lock: */
       create ttCustomer.
            assign                                         
                ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms    
               
                   
               ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days ) 
                ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days

                .
           /* FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  .*/   
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error.    
                 IF AVAIL sman THEN
                     ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip

                    .

                ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .

     END. /*FOR EACH cust where*/
    END.
END.  /*if prmCondition = BEGIN*/
END.  /*IF prmField = cust-no */
if prmField = "name"  then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no
            AND  cust.name = prmText NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
      /*   FOR EACH cust where cust.company = prmComp  AND  cust.name = prmText  no-lock: */
            create ttCustomer.
            assign 
                ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms    
               
                   
               ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days ) 
                ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days

                .
            /*FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  . */  
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error. 
                 IF AVAIL sman THEN
                     ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip

                    .

                ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .

        END. /*FOR EACH cust where*/
        END.
    END. /*if prmCondition = EQUAL */
IF prmCondition = "BEGIN" then do:
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no
            AND cust.name begins prmText NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
   /*  FOR EACH cust where cust.company = prmComp  AND cust.name begins prmText  no-lock: */
        create ttCustomer.
        assign                                          
            ttCustomer.Customer = cust.cust-no
                ttCustomer.Name = cust.name 
                ttCustomer.city = cust.city
                ttCustomer.state = cust.state 
                ttCustomer.zip = cust.zip
                ttCustomer.type = cust.type 
                ttCustomer.sman = cust.sman
                ttCustomer.terr = cust.terr
                ttCustomer.county = cust.cc-country
                ttCustomer.country = cust.country
                ttCustomer.Address1 = cust.addr[1]
                ttCustomer.Address2 = cust.addr[2]
                ttCustomer.fob-code = cust.fob-code
                ttCustomer.carrier  = cust.carrier   
                ttCustomer.contact  = cust.contact    
                ttCustomer.over     = cust.over-pct    
                ttCustomer.under    = cust.under-pct    
                ttCustomer.terms    = cust.terms    
               
                   
               ttCustomer.Taxcode  = cust.tax-gr   
                ttCustomer.ExpDate  = cust.date-field[1]
                ttCustomer.LastShip  = ( TODAY + cust.ship-days )
                ttCustomer.DueDate  = ( TODAY + cust.ship-days )
                 ttCustomer.dueCode  = "ON" 
                li-lead-days = cust.ship-days

                .
            /*FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustomer.DueDate  = oe-ordl.req-date 
                ttCustomer.prevOrder = oe-ordl.po-no-po
                ttCustomer.dueCode  = oe-ordl.req-code  .   */
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustomer.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustomer.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustomer.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustomer.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustomer.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error.
                 IF AVAIL sman THEN
                     ASSIGN 
                       ttCustomer.comm = sman.scomm
                       ttCustomer.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustomer.soldname    =  soldto.sold-name
                    ttCustomer.soldadd1    =  soldto.sold-addr[1]
                    ttCustomer.soldadd2    =  soldto.sold-addr[2] 
                    ttCustomer.soldcity    =  soldto.sold-city
                    ttCustomer.soldstate   =  soldto.sold-state
                    ttCustomer.soldzip     =  soldto.sold-zip

                    .
                ASSIGN
                    v-lead-days = li-lead-days
                    v-last-date = ttCustomer.LastShip
                    v-due-date  = ttCustomer.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustomer.LastShip = v-last-date
                    ttCustomer.DueDate  = v-due-date .


    end.  /*FOR EACH cust wher*/
    END.
end.    /*if prmCondition = BEGIN*/    
end.  /* if prmField = name  */

END.  /* IF prmAction = search then do: */

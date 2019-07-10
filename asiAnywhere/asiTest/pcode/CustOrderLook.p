
/*------------------------------------------------------------------------
    File         : CustOrderLook
    Purpose     : customer lookup

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCustOrderLook NO-UNDO 
        FIELD Customer AS character
        FIELD Name AS CHARACTER
        FIELD Address1 AS CHARACTER
        FIELD Address2 AS CHARACTER
        FIELD city AS CHARACTER
        FIELD state AS CHARACTER              
        FIELD zip AS CHARACTER 
        FIELD type as character
        FIELD sman    AS CHARACTER
        FIELD sname    AS CHARACTER
        FIELD country    AS CHARACTER
        FIELD county    AS CHARACTER
        FIELD terr AS CHARACTER
        FIELD frtpay AS CHAR FORMAT "x(1)"
        FIELD sales AS DECIMAL FORMAT  "->>>,>>>,>>9.99"
        FIELD comm AS DECIMAL FORMAT  "->>,>>>,>>9.99"
        FIELD fob-code      AS CHAR FORMAT  "x(5)"
        FIELD carrier       AS CHAR
        FIELD contact       AS CHAR
        FIELD over          AS DECIMAL
        FIELD under         AS DECIMAL
        FIELD terms         AS CHAR
        FIELD Tdscr         AS CHAR
        FIELD prevOrder     AS INT
        FIELD Taxcode       AS CHAR
        FIELD ExpDate       LIKE cust.date-field
        FIELD DueDate       AS DATE
        FIELD LastShip      AS DATE
        FIELD dueCode       AS CHAR
       
        FIELD soldname      AS CHAR
        FIELD soldadd1       AS CHAR
        FIELD soldadd2      AS CHAR
        FIELD soldcity       AS CHAR
        FIELD soldstate      AS CHAR
        FIELD soldzip       AS CHAR
    FIELD ghuehdkn  AS CHAR
    
        .
                                           
    
DEFINE DATASET dsCustOrderLook FOR ttCustOrderLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCustOrderLook.

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

if prmAction = "OrderCust" then do:
    v-count = 0 .
   
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.cust-no = prmText NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            
            create ttCustOrderLook.
            assign                                         
                ttCustOrderLook.Customer = cust.cust-no
                ttCustOrderLook.Name = cust.name 
                ttCustOrderLook.city = cust.city
                ttCustOrderLook.state = cust.state 
                ttCustOrderLook.zip = cust.zip
                ttCustOrderLook.type = cust.type 
                ttCustOrderLook.sman = cust.sman
                ttCustOrderLook.terr = cust.terr
                ttCustOrderLook.county = cust.cc-country
                ttCustOrderLook.country = cust.country
                ttCustOrderLook.Address1 = cust.addr[1]
                ttCustOrderLook.Address2 = cust.addr[2]
                ttCustOrderLook.fob-code = cust.fob-code
                ttCustOrderLook.carrier  = cust.carrier   
                ttCustOrderLook.contact  = cust.contact    
                ttCustOrderLook.over     = cust.over-pct    
                ttCustOrderLook.under    = cust.under-pct    
                ttCustOrderLook.terms    = cust.terms 
                   
                ttCustOrderLook.Taxcode  = cust.tax-gr   
                ttCustOrderLook.ExpDate  = cust.date-field[1]
                ttCustOrderLook.LastShip  = ( TODAY + cust.ship-days )
                ttCustOrderLook.DueDate  =  ( TODAY + cust.ship-days )
                ttCustOrderLook.dueCode  = "ON"
                li-lead-days = cust.ship-days
                .
           /* FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustOrderLook.DueDate  = oe-ordl.req-date 
                ttCustOrderLook.prevOrder = oe-ordl.po-no-po
                ttCustOrderLook.dueCode  = oe-ordl.req-code  .  */ 
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustOrderLook.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustOrderLook.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustOrderLook.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustOrderLook.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustOrderLook.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error.            
                ASSIGN 
                       ttCustOrderLook.comm = sman.scomm
                       ttCustOrderLook.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustOrderLook.soldname    =  soldto.sold-name
                    ttCustOrderLook.soldadd1    =  soldto.sold-addr[1]
                    ttCustOrderLook.soldadd2    =  soldto.sold-addr[2] 
                    ttCustOrderLook.soldcity    =  soldto.sold-city
                    ttCustOrderLook.soldstate   =  soldto.sold-state
                    ttCustOrderLook.soldzip     =  soldto.sold-zip
                    .
                 ASSIGN 
                    v-lead-days = li-lead-days
                    v-last-date = ttCustOrderLook.LastShip
                    v-due-date  = ttCustOrderLook.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustOrderLook.LastShip = v-last-date
                    ttCustOrderLook.DueDate  = v-due-date .
                
                v-count = v-count + 1 .
                
                IF v-count > 100 THEN LEAVE.
           END.	 /* IF AVAIL cust */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


/********************************************************************/

if prmAction = "ViewCust" then do:
    v-count = 0 .
   
    FOR EACH usercust WHERE usercust.user_id = prmUser 
        AND usercust.company EQ prmComp NO-LOCK:
        
        FIND FIRST cust WHERE cust.cust-no = usercust.cust-no AND cust.cust-no = usercust.cust-no NO-LOCK NO-ERROR.
        IF AVAIL cust THEN DO:
            
            create ttCustOrderLook.
            assign                                         
                ttCustOrderLook.Customer = cust.cust-no
                ttCustOrderLook.Name = cust.name 
                ttCustOrderLook.city = cust.city
                ttCustOrderLook.state = cust.state 
                ttCustOrderLook.zip = cust.zip
                ttCustOrderLook.type = cust.type 
                ttCustOrderLook.sman = cust.sman
                ttCustOrderLook.terr = cust.terr
                ttCustOrderLook.county = cust.cc-country
                ttCustOrderLook.country = cust.country
                ttCustOrderLook.Address1 = cust.addr[1]
                ttCustOrderLook.Address2 = cust.addr[2]
                ttCustOrderLook.fob-code = cust.fob-code
                ttCustOrderLook.carrier  = cust.carrier   
                ttCustOrderLook.contact  = cust.contact    
                ttCustOrderLook.over     = cust.over-pct    
                ttCustOrderLook.under    = cust.under-pct    
                ttCustOrderLook.terms    = cust.terms 
                   
                ttCustOrderLook.Taxcode  = cust.tax-gr   
                ttCustOrderLook.ExpDate  = cust.date-field[1]
                ttCustOrderLook.LastShip  = ( TODAY + cust.ship-days )
                ttCustOrderLook.DueDate  =  ( TODAY + cust.ship-days )
                ttCustOrderLook.dueCode  = "ON"
                li-lead-days = cust.ship-days
                .
           /* FIND FIRST oe-ordl WHERE oe-ordl.cust-no = cust.cust-no NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl  THEN
            ASSIGN
                ttCustOrderLook.DueDate  = oe-ordl.req-date 
                ttCustOrderLook.prevOrder = oe-ordl.po-no-po
                ttCustOrderLook.dueCode  = oe-ordl.req-code  .  */ 
                FIND FIRST terms WHERE terms.t-code = cust.terms NO-LOCK NO-ERROR.
                IF AVAIL terms  THEN
                    ASSIGN
                    ttCustOrderLook.Tdscr    = terms.dscr .   
                
                if cust.frt-pay = "P" then ASSIGN  ttCustOrderLook.frtpay =   "Prepaid".
                else if cust.frt-pay = "C" then ASSIGN  ttCustOrderLook.frtpay = "Collect".
                else if cust.frt-pay = "B" THEN ASSIGN  ttCustOrderLook.frtpay =  "Bill".
                else if cust.frt-pay = "T" then ASSIGN  ttCustOrderLook.frtpay = "Third Party".
                else "".
                 find FIRST sman where sman.company = prmComp AND                                     
                    sman.sman = cust.sman no-lock no-error.            
                ASSIGN 
                       ttCustOrderLook.comm = sman.scomm
                       ttCustOrderLook.sname = sman.sname.
                FIND FIRST soldto WHERE soldto.company = prmComp AND soldto.cust-no = cust.cust-no AND soldto.sold-id = cust.cust-no NO-LOCK NO-ERROR.
                IF AVAIL soldto THEN
                    ASSIGN 
                    ttCustOrderLook.soldname    =  soldto.sold-name
                    ttCustOrderLook.soldadd1    =  soldto.sold-addr[1]
                    ttCustOrderLook.soldadd2    =  soldto.sold-addr[2] 
                    ttCustOrderLook.soldcity    =  soldto.sold-city
                    ttCustOrderLook.soldstate   =  soldto.sold-state
                    ttCustOrderLook.soldzip     =  soldto.sold-zip
                    .
                 ASSIGN 
                    v-lead-days = li-lead-days
                    v-last-date = ttCustOrderLook.LastShip
                    v-due-date  = ttCustOrderLook.DueDate
                    v-ord-date  = TODAY .

                {oe/lastship.i "v-" 1}
                    
                    ASSIGN
                    li-lead-days                  = v-lead-days
                    ttCustOrderLook.LastShip = v-last-date
                    ttCustOrderLook.DueDate  = v-due-date .
                
                v-count = v-count + 1 .
                
                IF v-count > 100 THEN LEAVE.
           END.	 /* IF AVAIL cust */
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 

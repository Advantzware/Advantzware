
/*------------------------------------------------------------------------
    File         : EstimateLookup.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Estimate

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttEstimateLookup NO-UNDO
    
    FIELD vEstimate AS CHARACTER
    FIELD vCustomer AS CHARACTER    
    FIELD vshipname AS CHARACTER
    FIELD vdscr     AS CHARACTER
    FIELD vpartno   AS CHARACTER    
    FIELD vStyle    AS CHARACTER
    FIELD vLength   AS DECIMAL
    FIELD vWidth    AS DECIMAL
    FIELD vDepth    AS DECIMAL     
    FIELD zrlkkl    AS CHARACTER
      .
DEFINE DATASET dsEstimateLookup FOR ttEstimateLookup.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsEstimateLookup.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".


DEF VAR prmComp AS CHAR NO-UNDO.
def {1} {2} NEW SHARED var cocode     as   char  format "x(3)"  no-undo.
def {1} {2} NEW SHARED VAR locode     as   char  format "x(5)"  no-undo.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
 usercomp.user_id = prmUser AND
 usercomp.loc = '' AND
 usercomp.company_default = YES
 NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN
    cocode = prmComp .

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
   ASSIGN 
        custcount = custcount + "," + usercust.cust-no .
END. /*FOR EACH usercust*/ 


IF prmAction <> "search"  THEN DO:               
         FOR EACH eb WHERE eb.company = prmComp NO-LOCK BY eb.est-no : 
            IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
           create ttEstimateLookup.
            assign                                                  
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
           END.
                   
END.  /* end of seach */

IF prmAction = "search"  THEN DO:
    if prmField = "Estimate"  then do:
        if prmCondition = "EQUAL" then do:                                    
           FOR EACH eb WHERE eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) AND eb.company = prmComp NO-LOCK BY eb.est-no:  
               IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
           create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                .
           END.  /*do transaction*/
        END. 

        IF prmCondition = "BEGIN" then DO:
           FOR EACH eb WHERE eb.est-no BEGINS FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) AND eb.company = prmComp NO-LOCK BY eb.est-no DESC :   
               IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
           create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
  
           END.  /*do transaction*/
        END.
    end.  /* if prmField = Estimate */


    if prmField = "Customer"  then do:

        if prmCondition = "EQUAL" then do:                                          
           FOR EACH eb WHERE eb.cust-no = prmText AND eb.company = prmComp  NO-LOCK BY eb.cust-no : 
               IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
           create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
            END.  /*do transaction*/
         END. 

         IF prmCondition = "BEGIN" then DO:
            FOR EACH eb WHERE eb.cust-no BEGINS prmText AND eb.company = prmComp NO-LOCK BY eb.cust-no :
                IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
            create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
  
              END.  /*do transaction*/
         END.
    end.  /* if prmField = Customer */


    if prmField = "CustomerName"  then do:
        if prmCondition = "EQUAL" then do:                                           
           FOR EACH eb WHERE eb.company = prmComp AND eb.ship-name = prmText NO-LOCK BY eb.ship-name :   
               IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
           create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
           END.  /*do transaction*/
         END. 

         IF prmCondition = "BEGIN" then DO:
            FOR EACH eb WHERE eb.company = prmComp AND eb.ship-name BEGINS prmText  NO-LOCK BY eb.ship-name :
                IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
            create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
             END.  /*do transaction*/
        END.
    end.  /* if prmField = CustomerName*/

    if prmField = "CustomerPart"  then do:

        if prmCondition = "EQUAL" then do:                                          
           FOR EACH eb WHERE eb.part-no = prmText AND eb.company = prmComp  NO-LOCK BY eb.part-no :  
               IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
           create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
          END.  /*do transaction*/
        END. 

        IF prmCondition = "BEGIN" then DO:
           FOR EACH eb WHERE eb.part-no BEGINS prmText AND eb.company = prmComp NO-LOCK BY eb.part-no :     
               IF LOOKUP(eb.cust-no, custcount) = 0 THEN NEXT.
            create ttEstimateLookup.
            assign                                     
                ttEstimateLookup.vEstimate   = eb.est-no
                ttEstimateLookup.vCustomer   = eb.cust-no 
                ttEstimateLookup.vshipname   = eb.ship-name
                ttEstimateLookup.vdscr       = eb.part-dscr1
                ttEstimateLookup.vpartno     = eb.part-no              
                ttEstimateLookup.vStyle      = eb.style
                ttEstimateLookup.vLength     = eb.len
                ttEstimateLookup.vWidth      = eb.wid
                ttEstimateLookup.vDepth      = eb.dep              
                . 
                                          
            END.  /*do transaction*/
       END.
    end.  /* if prmField = CustomerPart*/

END. /* end of search */

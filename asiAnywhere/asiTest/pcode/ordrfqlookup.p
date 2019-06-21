
/*------------------------------------------------------------------------
    File         : ordrfqlookup.p
    Purpose     :  estimate lookup

    Syntax      :

    Description : Return a Dataset of all Order Entry

    Author(s)   : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttordrfqlookup NO-UNDO 
        FIELD abc        AS CHAR
        FIELD xy         AS CHAR
        FIELD vEstimate  AS CHARACTER
        FIELD vCustomer  AS CHARACTER    
        FIELD vshipname  AS CHARACTER
        FIELD vdscr      AS CHARACTER
        FIELD vpartno    AS CHARACTER
        FIELD vRfqno     AS INTEGER
        FIELD vQuoteno   AS INTEGER
        .
DEFINE DATASET dsordrfqlookupup FOR ttordrfqlookup.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsordrfqlookupup.

DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction      = ? THEN ASSIGN prmAction      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH usercust WHERE
        usercust.user_id =  prmUser AND
        usercust.company = prmComp
         NO-LOCK:
        FOR EACH eb WHERE eb.cust-no = usercust.cust-no  no-lock:
            FIND FIRST rfqitem WHERE rfqitem.est-no = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no)  NO-LOCK NO-ERROR.
            FIND FIRST quotehd WHERE quotehd.est-no = FILL(" ",8 - LENGTH(TRIM(eb.est-no))) + TRIM(eb.est-no)  NO-LOCK NO-ERROR.
            create ttordrfqlookup.
            assign                                     
                ttordrfqlookup.vEstimate   = eb.est-no
                ttordrfqlookup.vCustomer   = eb.cust-no 
                ttordrfqlookup.vshipname   = eb.ship-name
                ttordrfqlookup.vdscr       = eb.part-dscr1
                ttordrfqlookup.vpartno     = eb.part-no 
                ttordrfqlookup.vRfqno      = rfqitem.rfq-no 
                ttordrfqlookup.vQuoteno    = quotehd.q-no 
                .
     
        END.	 /* FOR EACH eb */
    END.
    
 END.  /*if prmAction <> "search" */

 IF prmAction = "search" then do:
          
     if prmField = "estno"  then do:
         MESSAGE "test" prmUser prmText.
         if prmCondition = "EQUAL" then do:
             FOR EACH eb WHERE  eb.est-no = FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)   NO-LOCK :
                 create ttordrfqlookup.
                 assign  
                     ttordrfqlookup.vEstimate   = eb.est-no
                ttordrfqlookup.vCustomer   = eb.cust-no 
                ttordrfqlookup.vshipname   = eb.ship-name
                ttordrfqlookup.vdscr       = eb.part-dscr1
                ttordrfqlookup.vpartno     = eb.part-no 
                ttordrfqlookup.vRfqno      = rfqitem.rfq-no 
                ttordrfqlookup.vQuoteno    = quotehd.q-no 
                . 
             END.

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH eb WHERE eb.est-no BEGINS FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText)  NO-LOCK :
                      create ttordrfqlookup.
                      assign   
                          ttordrfqlookup.vEstimate   = eb.est-no
                ttordrfqlookup.vCustomer   = eb.cust-no 
                ttordrfqlookup.vshipname   = eb.ship-name
                ttordrfqlookup.vdscr       = eb.part-dscr1
                ttordrfqlookup.vpartno     = eb.part-no 
                ttordrfqlookup.vRfqno      = rfqitem.rfq-no 
                ttordrfqlookup.vQuoteno    = quotehd.q-no 
                          .
                      
                      
                            
          end.  /*FOR EACH state wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
END.  /* IF prmAction = search then do: */


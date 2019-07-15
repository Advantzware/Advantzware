


/*------------------------------------------------------------------------
    File         : Carrierlookup
    Purpose     : 
    Syntax      :

    Description : Return a Dataset of all Po Inquiry

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

 
DEFINE TEMP-TABLE ttLookUpCarr NO-UNDO 
          FIELD carrier AS CHARACTER 
          FIELD dscr AS CHARACTER
          FIELD abc AS INT
          
          .

DEFINE DATASET dsLookUpCarr FOR ttLookUpCarr .
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsLookUpCarr.

DEF VAR prmComp AS CHAR NO-UNDO.
       
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH carrier WHERE carrier.company EQ prmComp NO-LOCK:
        create ttLookUpCarr.
            assign                                         
                ttLookUpCarr.carrier = carrier.carrier
                ttLookUpCarr.dscr = carrier.dscr
                .
    END.	 /* FOR EACH carrier */
   


END.  /*if prmAction <> "search" then do*/ 

ELSE
do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH carrier where carrier.company EQ prmComp no-lock:

                IF (carrier.carrier = prmText or carrier.dscr = prmText) THEN
                DO:
                   create ttLookUpCarr.
                   assign 
                       ttLookUpCarr.carrier = carrier.carrier
                       ttLookUpCarr.dscr = carrier.dscr
                .
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH carrier where carrier.company EQ prmComp
                no-lock:

                IF (carrier.carrier begins prmText or carrier.dscr begins prmText) THEN
                DO:
                   create ttLookUpCarr.
                   assign 
                       ttLookUpCarr.carrier = carrier.carrier
                       ttLookUpCarr.dscr = carrier.dscr
                .
                END.
             END.  /*FOR EACH carrier where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/     
    IF prmField = "carrier" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH carrier where carrier.company EQ prmComp AND carrier.carrier = prmText no-lock:
                create ttLookUpCarr.
                assign    
                    ttLookUpCarr.carrier = carrier.carrier
                    ttLookUpCarr.dscr = carrier.dscr
                    .
            END.  /*FOR EACH carrier where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH carrier where carrier.company EQ prmComp AND carrier.carrier begins prmText no-lock:
                create ttLookUpCarr.
                assign  
                    ttLookUpCarr.carrier = carrier.carrier
                    ttLookUpCarr.dscr = carrier.dscr       
                                 .
            END. /*FOR EACH carrier where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = i-no */
    if prmField = "dscr"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH carrier where carrier.company EQ prmComp AND carrier.dscr = prmText no-lock:
                create ttLookUpCarr.
                assign                                         
                    ttLookUpCarr.carrier = carrier.carrier
                    ttLookUpCarr.dscr = carrier.dscr     
                .
                   
            end. /*FOR EACH item-fg where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH carrier where carrier.company EQ prmComp AND carrier.dscr begins prmText no-lock:
                create ttLookUpCarr.
                assign                                          
                   ttLookUpCarr.carrier = carrier.carrier
                    ttLookUpCarr.dscr = carrier.dscr
                .
                   
            end.  /*FOR EACH item-fg wher*/
        end.    /*if prmCondition = BEGIN*/    
    end.  /* if prmField = name  */
       
END.  /* IF prmAction = search then do: */





/*------------------------------------------------------------------------
    File        : QPartLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttQPartLook NO-UNDO 
    FIELD QEst      AS CHAR
    FIELD QDscr  AS CHAR
    FIELD QCust      AS CHAR
    FIELD QPartNo    AS CHAR
    .


DEFINE DATASET dsQPartLook FOR ttQPartLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsQPartLook.
DEFINE VARIABLE v-qry-string   AS CHARACTER  NO-UNDO.
DEFINE VARIABLE v-return-value AS LOGICAL    NO-UNDO.
DEFINE VARIABLE v-qry-handle   AS HANDLE     NO-UNDO.
DEFINE VAR vEst AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
       
IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
ASSIGN vEst =  FILL(" ",8 - LENGTH(TRIM(prmText))) + TRIM(prmText) .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH eb WHERE  eb.company eq prmComp NO-LOCK:
        create ttQPartLook.
            assign                                         
               ttQPartLook.QEst       =  eb.est-no  
               ttQPartLook.QDscr  =  eb.i-dscr[1]                         
               ttQPartLook.QCust      =  eb.cust-no
               ttQPartLook.QPartNo    =  eb.part-no  .
    END.	 /* FOR EACH eb */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:

    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH eb where  eb.company eq prmComp AND (eb.est-no = vEst OR 
                                                          eb.i-dscr[1] = prmText OR 
                                                          eb.cust-no = prmText OR
                                                          eb.part-no = prmText) no-lock:
                create ttQPartLook.
                assign
                    ttQPartLook.QEst       =  eb.est-no  
                    ttQPartLook.QDscr      =  eb.i-dscr[1]
                    ttQPartLook.QCust      =  eb.cust-no                        
                    ttQPartLook.QPartNo    =  eb.part-no  .
                
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH eb where  eb.company eq prmComp AND (eb.est-no = vEst OR 
                                                          eb.i-dscr[1] BEGINS prmText OR 
                                                          eb.cust-no BEGINS prmText OR
                                                          eb.part-no BEGINS prmText) no-lock:
                create ttQPartLook.
                assign
                    ttQPartLook.QEst       =  eb.est-no  
                    ttQPartLook.QDscr      =  eb.i-dscr[1]
                    ttQPartLook.QCust      =  eb.cust-no
                    ttQPartLook.QPartNo    =  eb.part-no  .
            END.  /*FOR EACH eb where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "est-no"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH eb where  eb.company eq prmComp AND eb.est-no = vEst no-lock:
                create ttQPartLook.
                assign 
                    ttQPartLook.QEst       =  eb.est-no  
                    ttQPartLook.QDscr      =  eb.i-dscr[1]  
                    ttQPartLook.QCust      =  eb.cust-no                        
                    ttQPartLook.QPartNo    =  eb.part-no  .
            END. /*FOR EACH eb where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGINS" then do:
             FOR EACH eb where  eb.company BEGINS prmComp AND eb.est-no = vEst no-lock:
                create ttQPartLook.
                assign
                    ttQPartLook.QEst       =  eb.est-no  
                    ttQPartLook.QDscr  =  eb.i-dscr[1]  
                    ttQPartLook.QCust      =  eb.cust-no                       
                    ttQPartLook.QPartNo    =  eb.part-no  .
            end.  /*FOR EACH eb wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "part-no" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH eb where  eb.company eq prmComp AND eb.part-no = prmText no-lock:
                 create ttQPartLook.
                 assign
                     ttQPartLook.QEst       =  eb.est-no  
                     ttQPartLook.QDscr  =  eb.i-dscr[1]  
                     ttQPartLook.QCust      =  eb.cust-no                         
                     ttQPartLook.QPartNo    =  eb.part-no  .
             END. /*FOR EACH eb where*/
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGINS" then do:
             FOR EACH eb where  eb.company BEGINS prmComp AND eb.part-no BEGINS prmText no-lock:
                 create ttQPartLook.
                 assign 
                     ttQPartLook.QEst       =  eb.est-no  
                     ttQPartLook.QDscr  =  eb.i-dscr[1]  
                     ttQPartLook.QCust      =  eb.cust-no                        
                     ttQPartLook.QPartNo    =  eb.part-no                  
                     .
             END. /*FOR EACH eb where*/
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = est-no */

    
END.  /* IF prmAction = search then do: */






/*------------------------------------------------------------------------
    File        : SalesNameLook.p
    Purpose     :
    Syntax      :
    Description : 
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttSalesNameLook NO-UNDO 
    FIELD ss AS CHARACTER
    FIELD vSman1     AS CHARACTER
    FIELD vSname1     AS CHARACTER
    FIELD SComm     AS DECIMAL
    .
                                           
    
DEFINE DATASET dsSalesNameLook FOR ttSalesNameLook .


DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSalesNameLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
    FOR EACH sman NO-LOCK:
           create ttSalesNameLook.
                 assign                                     
                    ttSalesNameLook.vSman1 = sman.sman
                    ttSalesNameLook.vSname1 = sman.sname 
                     ttSalesNameLook.SComm = sman.scomm
                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        IF prmField = "ANY" then do:
            IF prmCondition = "EQUAL" then do:
                FOR EACH sman NO-LOCK:
                    IF (sman.sman = prmText OR sman.sname = prmText) THEN
                        DO:
                        create ttSalesNameLook.
                        assign                                     
                            ttSalesNameLook.vSman1 = sman.sman  
                            ttSalesNameLook.vSname1 = sman.sname
                            ttSalesNameLook.SComm = sman.scomm.
                    END.
                END.

            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        
        IF prmCondition = "BEGIN" then do:
            FOR EACH sman NO-LOCK:
                IF (sman.sman BEGINS prmText OR sman.sname BEGINS prmText) THEN
                    DO:
                    create ttSalesNameLook.
                    assign                                     
                        ttSalesNameLook.vSman1 = sman.sman  
                        ttSalesNameLook.vSname1 = sman.sname
                        ttSalesNameLook.SComm = sman.scomm
                        .
                    END.
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
     END. /*IF prmField = ANY*/  
     if prmField = "sman"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH sman WHERE (sman.sman = prmText OR sman.sname = prmText) NO-LOCK :
                 create ttSalesNameLook.
                 assign                                     
                     ttSalesNameLook.vSman1 = sman.sman  
                     ttSalesNameLook.vSname1 = sman.sname
                     ttSalesNameLook.SComm = sman.scomm. 
             END.

          END. /*FOR EACH sman*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH sman WHERE (sman.sman BEGINS prmText OR sman.sname BEGINS prmText) NO-LOCK :
                      create ttSalesNameLook.
                      assign                                     
                          ttSalesNameLook.vSman1 = sman.sman  
                          ttSalesNameLook.vSname1 = sman.sname
                          ttSalesNameLook.SComm = sman.scomm. 
                  end.  /*FOR EACH sman wher*/
            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = sman  */
           
END.  /* IF prmAction = search then do: */





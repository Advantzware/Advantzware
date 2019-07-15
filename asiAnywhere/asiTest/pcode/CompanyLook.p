
/*------------------------------------------------------------------------
    File        : CompanyLook.p
    Purpose     : Company Look Up

    Syntax      :

    Description : Return a Dataset of all Company Look

    Author(s)   : Sewa
    Created     : 01 Apr 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCompLook NO-UNDO 
    FIELD vCompany LIKE company.company
    FIELD vName  LIKE company.NAME
    FIELD vCurrCode LIKE company.curr-code.

DEFINE DATASET dsCompLook FOR ttCompLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCompLook.

DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction      = "".

IF prmField    = ? THEN ASSIGN prmField      = "".
IF prmUser      = ? THEN ASSIGN prmUser        = "".
IF prmCondition = ? THEN ASSIGN prmCondition   = "".
IF prmText      = ? THEN ASSIGN prmText        = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    
if prmAction <> "search" then do:
    FOR EACH company  NO-LOCK:
        create ttCompLook.
            assign                                         
                ttCompLook.vCompany        = company.company
                ttCompLook.vName           = company.NAME
                ttCompLook.vCurrCode       = company.curr-code
                .
    END.	 /* FOR EACH companyat */
    
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" then do:
    IF prmField = "ANY" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH company  no-lock:

                IF company.company = prmText OR company.NAME = prmText THEN
                DO:
                   create ttCompLook.
                   assign
                       ttCompLook.vCompany  = company.company
                       ttCompLook.vName     = company.NAME
                       ttCompLook.vCurrCode = company.curr-code.
                END.
            END. /*FOR EACH cust where cust.cust-no = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH company no-lock:

                IF company.company BEGINS prmText OR company.NAME begins prmText THEN
                DO:
                   create ttCompLook.
                   assign
                      ttCompLook.vCompany  = company.company
                      ttCompLook.vName     = company.NAME
                       ttCompLook.vCurrCode  = company.curr-code.
                END.
             END.  /*FOR EACH company where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "company"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH company WHERE company.company = prmText no-lock:
                create ttCompLook.
                assign 
                    ttCompLook.vCompany        = company.company
                    ttCompLook.vName           = company.NAME
                    ttCompLook.vCurrCode       = company.curr-code

                    .
            end. /*FOR EACH company where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH company WHERE company.company BEGINS prmText  no-lock:
                create ttCompLook.
                assign
                ttCompLook.vCompany        = company.company
                ttCompLook.vName           = company.NAME
                ttCompLook.vCurrCode       = company.curr-code
                
                    
                    .
            end.  /*FOR EACH company wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = name  */
    IF prmField = "name" then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH company WHERE company.NAME = prmText no-lock:
                create ttCompLook.
                assign
                    ttCompLook.vCompany        = company.company
                    ttCompLook.vName           = company.NAME
                    ttCompLook.vCurrCode       = company.curr-code
                
                    
                    .
            END. /*FOR EACH company where*/
        END. /*if prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH company  WHERE company.NAME begins prmText no-lock:
                create ttCompLook.
                assign 
                    ttCompLook.vCompany        = company.company
                    ttCompLook.vName           = company.NAME
                    ttCompLook.vCurrCode       = company.curr-code
                
                    
                    .
            END. /*FOR EACH company where*/
        END.  /*if prmCondition = BEGIN*/
    END.  /*IF prmField = name */
   
    
END.  /* IF prmAction = search then do: */


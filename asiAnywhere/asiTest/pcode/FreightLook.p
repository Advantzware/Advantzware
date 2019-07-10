            /*------------------------------------------------------------------------
    File        : AdderLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttFreightLook NO-UNDO 
    FIELD frtclass       AS char
    FIELD frtclass-dscr  AS CHAR .

DEFINE DATASET dsFreightLook FOR ttFreightLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFreightLook.
       
DEF VAR prmComp AS CHAR NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction  = "".
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
    FOR EACH freight-class   NO-LOCK :
        create ttFreightLook.
            assign                                         
               ttFreightLook.frtclass       = freight-class.freight-class
               ttFreightLook.frtclass-dscr  =  freight-class.DESCRIPTION                       
               
                 .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
    IF prmField = "freight" then do:
        IF prmCondition = "EQUAL" then do:
             FOR EACH freight-class WHERE freight-class.freight-class = prmText  NO-LOCK :
        create ttFreightLook.
            assign                                         
               ttFreightLook.frtclass       = freight-class.freight-class
               ttFreightLook.frtclass-dscr  =  freight-class.DESCRIPTION 
                .
            END. /*FOR EACH cust where cust.i-dscr = prmText*/
        END.  /*IF prmCondition = EQUAL*/
        IF prmCondition = "BEGIN" then do:
            FOR EACH freight-class WHERE freight-class.freight-class BEGINS prmText  NO-LOCK :
        create ttFreightLook.
            assign                                         
               ttFreightLook.frtclass       = freight-class.freight-class
               ttFreightLook.frtclass-dscr  =  freight-class.DESCRIPTION  .
            END.  /*FOR EACH item where */         
         END. /*IF prmCondition = BEGIN then do:*/  
    END. /*IF prmField = ANY*/  
    if prmField = "dscr"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH freight-class WHERE freight-class.DESCRIPTION  = prmText  NO-LOCK :
        create ttFreightLook.
            assign                                         
               ttFreightLook.frtclass       = freight-class.freight-class
               ttFreightLook.frtclass-dscr  =  freight-class.DESCRIPTION . 
            END. /*FOR EACH item where*/
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
           FOR EACH freight-class WHERE freight-class.DESCRIPTION  BEGINS prmText  NO-LOCK :
        create ttFreightLook.
            assign                                         
               ttFreightLook.frtclass       = freight-class.freight-class
               ttFreightLook.frtclass-dscr  =  freight-class.DESCRIPTION . 
            end.  /*FOR EACH item wher*/
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
      
END.  /* IF prmAction = search then do: */




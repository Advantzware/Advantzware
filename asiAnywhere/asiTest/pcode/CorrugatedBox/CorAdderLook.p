/*------------------------------------------------------------------------
    File      : CorAdderLook.p
    Purpose   :  Corrugated Customer Lookup
    Syntax    :

    Description : Return a Dataset of Corrugated Box

    Author(s)   : 
    Created     : 9 march 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorrAdderLookup NO-UNDO 
        FIELD vMcode         AS character
        FIELD vAttType       AS CHARACTER
        FIELD vAttDscr       AS CHAR

        .
    
DEFINE DATASET dsCorrAdderLookup FOR ttCorrAdderLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMachine        AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrAdderLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmComp        = ? THEN ASSIGN prmComp    = "".
IF prmMachine     = ? THEN ASSIGN prmMachine    = "".

DEFINE VAR v-count AS INT NO-UNDO.


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


if prmAction <> "search" then do:
    v-count = 0 . 
    FOR EACH mach-attach WHERE  mach-attach.company = prmComp and ( mach-attach.m-code = prmMachine or prmMachine eq "") NO-LOCK :
             create ttCorrAdderLookup.
             assign  

                 ttCorrAdderLookup.vMcode         = mach-attach.m-code
                 ttCorrAdderLookup.vAttType        = mach-attach.att-type
                 ttCorrAdderLookup.vAttDscr        = mach-attach.att-dscr 
                  
                 .
                 
             v-count = v-count + 1 .
             IF v-count > 100 THEN LEAVE.

         
    END. /* for each usercust */
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Search" THEN DO:
  IF prmField = "code" then do:
    if prmCondition = "EQUAL" then do:
        FOR EACH mach-attach WHERE  mach-attach.company = prmComp and  mach-attach.m-code = prmText NO-LOCK :
            IF AVAIL mach-attach THEN
            create ttCorrAdderLookup.
             assign  

                 ttCorrAdderLookup.vMcode         = mach-attach.m-code
                 ttCorrAdderLookup.vAttType        = mach-attach.att-type
                 ttCorrAdderLookup.vAttDscr        = mach-attach.att-dscr 
                  
                 .
             
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
    FOR EACH mach-attach WHERE  mach-attach.company = prmComp and mach-attach.m-code BEGINS prmText NO-LOCK :
         IF AVAIL mach-attach THEN     
        create ttCorrAdderLookup.
             assign  

                 ttCorrAdderLookup.vMcode         = mach-attach.m-code
                 ttCorrAdderLookup.vAttType        = mach-attach.att-type
                 ttCorrAdderLookup.vAttDscr        = mach-attach.att-dscr 
                  
                 .
           
     END.  /*for each usercust */
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */

IF prmField = "name" then do:
    if prmCondition = "EQUAL" then do:
       FOR EACH mach-attach WHERE  mach-attach.company = prmComp and mach-attach.att-dscr = prmText NO-LOCK :
            IF AVAIL mach-attach THEN  
           create ttCorrAdderLookup.
             assign  

                 ttCorrAdderLookup.vMcode         = mach-attach.m-code
                 ttCorrAdderLookup.vAttType        = mach-attach.att-type
                 ttCorrAdderLookup.vAttDscr        = mach-attach.att-dscr 
                  
                 .
             
        END. /*FOR EACH item where*/
    END. /*if prmCondition = EQUAL*/


IF prmCondition = "BEGIN" then do:
   FOR EACH mach-attach WHERE  mach-attach.company = prmComp and mach-attach.att-dscr BEGINS prmText  NO-LOCK :
        IF AVAIL mach-attach THEN      
       create ttCorrAdderLookup.
             assign  

                 ttCorrAdderLookup.vMcode         = mach-attach.m-code
                 ttCorrAdderLookup.vAttType        = mach-attach.att-type
                 ttCorrAdderLookup.vAttDscr        = mach-attach.att-dscr 
                  
                 .

           
   END.
END. /*IF prmCondition = "BEGIN" t*/
END.  /*IF prmField = part-no */


END. /*ir prmAction = "search"*/



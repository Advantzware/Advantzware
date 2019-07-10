            /*------------------------------------------------------------------------
    File        : VendTypeLook.p
    Purpose     : 

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttVendorTypeLookup NO-UNDO 
    FIELD vendtype  AS CHAR
    FIELD venddscr  AS CHAR
    FIELD extra     AS CHAR.

DEFINE DATASET dsVendorTypeLookup FOR ttVendorTypeLookup.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsVendorTypeLookup.
       
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

DEF VAR g_company AS CHAR NO-UNDO.

ASSIGN g_company = prmComp.




if prmAction <> "search" then do:
    FOR EACH ventype WHERE ventype.company = prmComp NO-LOCK:
        
        create ttVendorTypeLookup.
            assign                                         
               ttVendorTypeLookup.vendtype   =  ventype.type
               ttVendorTypeLookup.venddscr   =  ventype.Dscr                        
               
                 .
    END.	 /* FOR EACH item */     
    
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "search" THEN DO:
    IF prmField = "Vend Type" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH ventype WHERE ventype.company = prmComp 
                AND ventype.TYPE EQ prmText NO-LOCK:

                create ttVendorTypeLookup.
                assign                                         
                    ttVendorTypeLookup.vendtype   =  ventype.type
                    ttVendorTypeLookup.venddscr   =  ventype.Dscr .
            END.
        END.
        IF prmCondition = "BEGIN" then do:
            FOR EACH ventype WHERE ventype.company = prmComp 
                AND ventype.TYPE BEGINS prmText NO-LOCK:

                create ttVendorTypeLookup.
                assign                                         
                    ttVendorTypeLookup.vendtype   =  ventype.type
                    ttVendorTypeLookup.venddscr   =  ventype.Dscr .
            END.
        END.
    END.

    IF prmField = "Dscr" then do:
        IF prmCondition = "EQUAL" then do:
            FOR EACH ventype WHERE ventype.company = prmComp 
                AND ventype.Dscr EQ prmText NO-LOCK:

                create ttVendorTypeLookup.
                assign                                         
                    ttVendorTypeLookup.vendtype   =  ventype.type
                    ttVendorTypeLookup.venddscr   =  ventype.Dscr .
            END.
        END.
        IF prmCondition = "BEGIN" then do:
            FOR EACH ventype WHERE ventype.company = prmComp 
                AND ventype.Dscr BEGINS prmText NO-LOCK:

                create ttVendorTypeLookup.
                assign                                         
                    ttVendorTypeLookup.vendtype   =  ventype.type
                    ttVendorTypeLookup.venddscr   =  ventype.Dscr .
            END.
        END.
    END.
END.







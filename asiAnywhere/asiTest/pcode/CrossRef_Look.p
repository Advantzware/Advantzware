



/*------------------------------------------------------------------------
    File        : vend-code-cust-xrefLook.p
    Purpose     : vend-code-cust-xref

    Syntax      :

    Description : Return a Dataset of UserMaintenance

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCrossRefLookup NO-UNDO 
    FIELD custno        AS CHARACTER
    FIELD vandercode    AS CHARACTER
    FIELD custname      AS CHAR
    FIELD mklbh AS CHARACTER
    
    .
    
DEFINE DATASET dsCrossRefLookup FOR ttCrossRefLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCrossRefLookup.
       
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

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

FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

if prmAction <> "search" then do:
        
    MAIN-LOOP:
    FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp NO-LOCK:
         IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/
END.  /*ifif prmAction <> "search" */

    IF prmAction = "search" then do:
        
     if prmField = "custno"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.cust-no = prmText   NO-LOCK:
                IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.cust-no BEGINS prmText   NO-LOCK:
                IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/

            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */

         if prmField = "vendor"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.vendor-code = prmText   NO-LOCK:
                IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.vendor-code BEGINS prmText   NO-LOCK:
                IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/

            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
           
          if prmField = "vendorname"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.cust-name = prmText   NO-LOCK:
                IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/

          END. /*FOR EACH state*/
          IF prmCondition = "BEGIN" then do:
                  FOR EACH vend-code-cust-xref WHERE  vend-code-cust-xref.company = prmComp AND vend-code-cust-xref.cust-name BEGINS prmText   NO-LOCK:
                IF LOOKUP(vend-code-cust-xref.cust-no, custcount) = 0 THEN NEXT.
                 create ttCrossRefLookup.
                 assign                                     
                    ttCrossRefLookup.custno     = vend-code-cust-xref.cust-no
                    ttCrossRefLookup.vandercode = vend-code-cust-xref.vendor-code 
                    ttCrossRefLookup.custname   = vend-code-cust-xref.cust-name

                   .
        END.  /*FOR EACH Itemfg*/

            end.    /*if prmCondition = BEGIN*/    
         end.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */





/*------------------------------------------------------------------------
    File        : UsagCustPartLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUsagCustPartLook NO-UNDO 
   
    FIELD PartNo       AS CHAR
    FIELD ItemNo        AS CHAR
    FIELD Vander       AS CHAR
    FIELD PlantCode           AS CHAR
    FIELD DeptCode          AS CHAR
    FIELD custno         AS CHAR
   FIELD klooop  AS CHAR
    .


DEFINE DATASET dsUsagCustPartLook FOR ttUsagCustPartLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCustPart  AS CHAR NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUsagCustPartLook.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmCustPart      = ? THEN ASSIGN prmCustPart      = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
 FOR EACH vend-whse-item WHERE  vend-whse-item.company = prmComp  AND 
        (vend-whse-item.cust-part-no = prmCustPart OR prmCustPart = "") NO-LOCK :
    create ttUsagCustPartLook.
                assign
                            ttUsagCustPartLook.PartNo      = vend-whse-item.cust-part-no
                            ttUsagCustPartLook.ItemNo      = vend-whse-item.fg-item-no
                            ttUsagCustPartLook.Vander      = vend-whse-item.vendor-code
                            ttUsagCustPartLook.PlantCode   = vend-whse-item.vendor-plant-code
                            ttUsagCustPartLook.DeptCode    = vend-whse-item.vendor-dept-code
                            ttUsagCustPartLook.custno       = vend-whse-item.cust-no
                           .
                           
  
    END.	 /* FOR EACH eoe- */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "Part-no"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH vend-whse-item WHERE vend-whse-item.company = prmComp  AND vend-whse-item.cust-part-no = prmText  
                AND (vend-whse-item.cust-part-no = prmCustPart OR prmCustPart = "") NO-LOCK: 
                create ttUsagCustPartLook.
                    assign
                            ttUsagCustPartLook.PartNo      = vend-whse-item.cust-part-no
                            ttUsagCustPartLook.ItemNo      = vend-whse-item.fg-item-no
                            ttUsagCustPartLook.Vander      = vend-whse-item.vendor-code
                            ttUsagCustPartLook.PlantCode   = vend-whse-item.vendor-plant-code
                            ttUsagCustPartLook.DeptCode    = vend-whse-item.vendor-dept-code
                            ttUsagCustPartLook.custno       = vend-whse-item.cust-no
                           .
                           
  
    END.	 /* FOR EACH eoe- */     
        END. /*if prmCondition = EQUAL */

        IF prmCondition = "BEGIN" then do:
         FOR EACH vend-whse-item WHERE vend-whse-item.company = prmComp AND vend-whse-item.cust-part-no BEGINS prmText 
             AND (vend-whse-item.cust-part-no = prmCustPart OR prmCustPart = "")  NO-LOCK :
                create ttUsagCustPartLook.
                    assign
                            ttUsagCustPartLook.PartNo      = vend-whse-item.cust-part-no
                            ttUsagCustPartLook.ItemNo      = vend-whse-item.fg-item-no
                            ttUsagCustPartLook.Vander      = vend-whse-item.vendor-code
                            ttUsagCustPartLook.PlantCode   = vend-whse-item.vendor-plant-code
                            ttUsagCustPartLook.DeptCode    = vend-whse-item.vendor-dept-code
                            ttUsagCustPartLook.custno       = vend-whse-item.cust-no
                           .
                           
  
    END.	 /* FOR EACH eoe- */   
        END.  /*end of begin */
     END.  /*IF prmField = est-no */

      if prmField = "fgitem"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH vend-whse-item WHERE  vend-whse-item.company = prmComp  AND vend-whse-item.fg-item-no = prmText 
                AND (vend-whse-item.cust-part-no = prmCustPart OR prmCustPart = "")  NO-LOCK: 
                create ttUsagCustPartLook.
                    assign
                            ttUsagCustPartLook.PartNo      = vend-whse-item.cust-part-no
                            ttUsagCustPartLook.ItemNo      = vend-whse-item.fg-item-no
                            ttUsagCustPartLook.Vander      = vend-whse-item.vendor-code
                            ttUsagCustPartLook.PlantCode   = vend-whse-item.vendor-plant-code
                            ttUsagCustPartLook.DeptCode    = vend-whse-item.vendor-dept-code
                            ttUsagCustPartLook.custno       = vend-whse-item.cust-no
                           .
                           
  
    END.	 /* FOR EACH eoe- */     
        END. /*if prmCondition = EQUAL */

        IF prmCondition = "BEGIN" then do:
         FOR EACH vend-whse-item WHERE vend-whse-item.company = prmComp AND vend-whse-item.fg-item-no BEGINS prmText 
             AND (vend-whse-item.cust-part-no = prmCustPart OR prmCustPart = "") NO-LOCK :
                create ttUsagCustPartLook.
                    assign
                            ttUsagCustPartLook.PartNo      = vend-whse-item.cust-part-no
                            ttUsagCustPartLook.ItemNo      = vend-whse-item.fg-item-no
                            ttUsagCustPartLook.Vander      = vend-whse-item.vendor-code
                            ttUsagCustPartLook.PlantCode   = vend-whse-item.vendor-plant-code
                            ttUsagCustPartLook.DeptCode    = vend-whse-item.vendor-dept-code
                            ttUsagCustPartLook.custno       = vend-whse-item.cust-no
                           .
                           
  
    END.	 /* FOR EACH eoe- */   
        END.  /*end of begin */
     END.  /*IF prmField = est-no */
END.  /* IF prmAction = search then do: */




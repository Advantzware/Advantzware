


/*------------------------------------------------------------------------
    File        : UsagPoLook.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all PartLook

    Author(s)   : Jyoti
    Created     : Feb 23 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttUsagPoLook NO-UNDO 
    FIELD itemno        AS CHAR
    FIELD pono          AS CHAR
    FIELD orderno       AS INT
    FIELD lineno        AS INT
    FIELD usagQty       AS DECIMAL
    FIELD usagCustPart  AS CHAR
    FIELD VanderCode    AS CHAR
    FIELD PlantCode     AS CHAR
    FIELD DeptCode      AS CHAR 
    FIELD JobNo         AS CHAR 
    FIELD JobNo2        AS INT
    FIELD Tprice        AS DECIMAL
    .


DEFINE DATASET dsUsagPoLook FOR ttUsagPoLook.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsUsagPoLook.


IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition      = ? THEN ASSIGN prmCondition      = "".
IF prmText      = ? THEN ASSIGN prmText      = "".

DEF BUFFER b-oe-rel FOR oe-rel.
DEF BUFFER b-vend-whse-item FOR vend-whse-item.
     DEF BUFFER b-oe-ordl FOR oe-ordl.
   DEF BUFFER b-vend-whse-trans FOR vend-whse-trans.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

if prmAction <> "search" then do:
  FOR EACH oe-rel  WHERE oe-rel.company = prmComp
                          AND (oe-rel.stat    = "C" OR oe-rel.stat    = "Z")
                          AND oe-rel.po-no   <> ""
                          AND oe-rel.i-no    <> "" NO-LOCK:
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = oe-rel.company
                          AND b-oe-ordl.ord-no  = oe-rel.ord-no
                          AND b-oe-ordl.LINE    = oe-rel.LINE
                          AND b-oe-ordl.i-no    = oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                    AND b-vend-whse-item.cust-no     = oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                       AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      END.
                    IF AVAILABLE(b-vend-whse-item) THEN DO:
                       
                      create ttUsagPoLook.
                        assign
                            ttUsagPoLook.pono           = oe-rel.po-no
                            ttUsagPoLook.itemno         = oe-rel.i-no
                            ttUsagPoLook.orderno        = (oe-rel.ord-no)
                            ttUsagPoLook.lineno         = (oe-rel.line)
                            ttUsagPoLook.usagCustPart   = b-vend-whse-item.cust-part-no
                            ttUsagPoLook.VanderCode     = b-vend-whse-item.vendor-code
                            ttUsagPoLook.PlantCode      = b-vend-whse-item.vendor-plant-code
                            ttUsagPoLook.DeptCode       = b-vend-whse-item.vendor-dept-code
                            ttUsagPoLook.JobNo          = b-oe-ordl.job-no
                            ttUsagPoLook.JobNo2         = (b-oe-ordl.job-no2)
                            ttUsagPoLook.usagQty        = (oe-rel.qty)
                            ttUsagPoLook.Tprice         = (b-oe-ordl.t-price)  .
      END.
   END.
             
    END.	 /* FOR EACH eb */     
    
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "search" then do:
     
    if prmField = "pono"  then do:
        if prmCondition = "EQUAL" then do:
             FOR EACH oe-rel  WHERE oe-rel.company = prmComp
                          AND (oe-rel.stat    = "C" OR oe-rel.stat    = "Z")
                          AND oe-rel.po-no   <> ""
                          AND oe-rel.i-no    <> "" AND oe-rel.po-no = prmText NO-LOCK:
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = oe-rel.company
                          AND b-oe-ordl.ord-no  = oe-rel.ord-no
                          AND b-oe-ordl.LINE    = oe-rel.LINE
                          AND b-oe-ordl.i-no    = oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                    AND b-vend-whse-item.cust-no     = oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                       AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      END.
                    IF AVAILABLE(b-vend-whse-item) THEN DO:
                       
                      create ttUsagPoLook.
                        assign
                            ttUsagPoLook.pono           = oe-rel.po-no
                            ttUsagPoLook.itemno         = oe-rel.i-no
                            ttUsagPoLook.orderno        = (oe-rel.ord-no)
                            ttUsagPoLook.lineno         = (oe-rel.line)
                            ttUsagPoLook.usagCustPart   = b-vend-whse-item.cust-part-no
                            ttUsagPoLook.VanderCode     = b-vend-whse-item.vendor-code
                            ttUsagPoLook.PlantCode      = b-vend-whse-item.vendor-plant-code
                            ttUsagPoLook.DeptCode       = b-vend-whse-item.vendor-dept-code
                            ttUsagPoLook.JobNo          = b-oe-ordl.job-no
                            ttUsagPoLook.JobNo2         = (b-oe-ordl.job-no2)
                            ttUsagPoLook.usagQty        = (oe-rel.qty)
                            ttUsagPoLook.Tprice         = (b-oe-ordl.t-price)  .
      END.
   END.
             
    END.	 /* FOR EACH eb */   
        END. /*if prmCondition = EQUAL */
        IF prmCondition = "BEGIN" then do:
            FOR EACH oe-rel  WHERE oe-rel.company = prmComp
                          AND (oe-rel.stat    = "C" OR oe-rel.stat    = "Z")
                          AND oe-rel.po-no   <> ""
                          AND oe-rel.i-no    <> "" AND oe-rel.po-no BEGINS prmText NO-LOCK:
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = oe-rel.company
                          AND b-oe-ordl.ord-no  = oe-rel.ord-no
                          AND b-oe-ordl.LINE    = oe-rel.LINE
                          AND b-oe-ordl.i-no    = oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                    AND b-vend-whse-item.cust-no     = oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                       AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      END.
                    IF AVAILABLE(b-vend-whse-item) THEN DO:
                       
                      create ttUsagPoLook.
                        assign
                            ttUsagPoLook.pono           = oe-rel.po-no
                            ttUsagPoLook.itemno         = oe-rel.i-no
                            ttUsagPoLook.orderno        = (oe-rel.ord-no)
                            ttUsagPoLook.lineno         = (oe-rel.line)
                            ttUsagPoLook.usagCustPart   = b-vend-whse-item.cust-part-no
                            ttUsagPoLook.VanderCode     = b-vend-whse-item.vendor-code
                            ttUsagPoLook.PlantCode      = b-vend-whse-item.vendor-plant-code
                            ttUsagPoLook.DeptCode       = b-vend-whse-item.vendor-dept-code
                            ttUsagPoLook.JobNo          = b-oe-ordl.job-no
                            ttUsagPoLook.JobNo2         = (b-oe-ordl.job-no2)
                            ttUsagPoLook.usagQty        = (oe-rel.qty)
                            ttUsagPoLook.Tprice         = (b-oe-ordl.t-price)  .
      END.
   END.
             
    END.	 /* FOR EACH eb */   
        end.    /*if prmCondition = BEGIN*/    
     end.  /* if prmField = est  */
       IF prmField = "i-no" then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH oe-rel  WHERE oe-rel.company = prmComp
                          AND (oe-rel.stat    = "C" OR oe-rel.stat    = "Z")
                          AND oe-rel.po-no   <> ""
                          AND oe-rel.i-no    <> "" AND oe-rel.i-no = prmText NO-LOCK:
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = oe-rel.company
                          AND b-oe-ordl.ord-no  = oe-rel.ord-no
                          AND b-oe-ordl.LINE    = oe-rel.LINE
                          AND b-oe-ordl.i-no    = oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                    AND b-vend-whse-item.cust-no     = oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                       AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      END.
                    IF AVAILABLE(b-vend-whse-item) THEN DO:
                       
                      create ttUsagPoLook.
                        assign
                            ttUsagPoLook.pono           = oe-rel.po-no
                            ttUsagPoLook.itemno         = oe-rel.i-no
                            ttUsagPoLook.orderno        = (oe-rel.ord-no)
                            ttUsagPoLook.lineno         = (oe-rel.line)
                            ttUsagPoLook.usagCustPart   = b-vend-whse-item.cust-part-no
                            ttUsagPoLook.VanderCode     = b-vend-whse-item.vendor-code
                            ttUsagPoLook.PlantCode      = b-vend-whse-item.vendor-plant-code
                            ttUsagPoLook.DeptCode       = b-vend-whse-item.vendor-dept-code
                            ttUsagPoLook.JobNo          = b-oe-ordl.job-no
                            ttUsagPoLook.JobNo2         = (b-oe-ordl.job-no2)
                            ttUsagPoLook.usagQty        = (oe-rel.qty)
                            ttUsagPoLook.Tprice         = (b-oe-ordl.t-price)  .
      END.
   END.
             
    END.	 /* FOR EACH eb */   
         END. /*if prmCondition = EQUAL*/
         IF prmCondition = "BEGIN" then do:
             FOR EACH oe-rel  WHERE oe-rel.company = prmComp
                          AND (oe-rel.stat    = "C" OR oe-rel.stat    = "Z")
                          AND oe-rel.po-no   <> ""
                          AND oe-rel.i-no    <> "" AND oe-rel.i-no BEGINS prmText NO-LOCK:
   FIND FIRST b-oe-ordl WHERE b-oe-ordl.company = oe-rel.company
                          AND b-oe-ordl.ord-no  = oe-rel.ord-no
                          AND b-oe-ordl.LINE    = oe-rel.LINE
                          AND b-oe-ordl.i-no    = oe-rel.i-no NO-LOCK NO-ERROR.
   IF AVAILABLE(b-oe-ordl) THEN DO:
      FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                    AND b-vend-whse-item.cust-no     = oe-rel.cust-no
                                    AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      IF NOT AVAILABLE(b-vend-whse-item) THEN DO:
         FIND FIRST b-vend-whse-item WHERE b-vend-whse-item.company     = oe-rel.company
                                       AND b-vend-whse-item.fg-item-no  = oe-rel.i-no NO-LOCK NO-ERROR.
      END.
                    IF AVAILABLE(b-vend-whse-item) THEN DO:
                       
                      create ttUsagPoLook.
                        assign
                            ttUsagPoLook.pono           = oe-rel.po-no
                            ttUsagPoLook.itemno         = oe-rel.i-no
                            ttUsagPoLook.orderno        = (oe-rel.ord-no)
                            ttUsagPoLook.lineno         = (oe-rel.line)
                            ttUsagPoLook.usagCustPart   = b-vend-whse-item.cust-part-no
                            ttUsagPoLook.VanderCode     = b-vend-whse-item.vendor-code
                            ttUsagPoLook.PlantCode      = b-vend-whse-item.vendor-plant-code
                            ttUsagPoLook.DeptCode       = b-vend-whse-item.vendor-dept-code
                            ttUsagPoLook.JobNo          = b-oe-ordl.job-no
                            ttUsagPoLook.JobNo2         = (b-oe-ordl.job-no2)
                            ttUsagPoLook.usagQty        = (oe-rel.qty)
                            ttUsagPoLook.Tprice         = (b-oe-ordl.t-price)  .
      END.
   END.
             
    END.	 /* FOR EACH eb */   
         END.  /*if prmCondition = BEGIN*/
     END.  /*IF prmField = est-no */
     

    
END.  /* IF prmAction = search then do: */




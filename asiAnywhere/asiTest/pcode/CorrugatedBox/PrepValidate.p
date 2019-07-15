
/*------------------------------------------------------------------------
    File        : CorrPrep.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 02  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmSnum        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBnum        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCode        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmQty         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmDesc        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmCost        AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmMl          AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmSimon       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMark        AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmAmort       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLine        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmOldQty      AS INT NO-UNDO.

DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".
IF prmEstNum     = ?  THEN ASSIGN    prmEstNum      = "".
IF prmFormNo     = ?  THEN ASSIGN    prmFormNo      = 0.

IF prmSnum       = ?  THEN ASSIGN    prmSnum        = 0.
IF prmBnum       = ?  THEN ASSIGN    prmBnum        = 0.
IF prmCode       = ?  THEN ASSIGN    prmCode        = "".
IF prmQty        = ?  THEN ASSIGN    prmQty         = 0.
IF prmDesc       = ?  THEN ASSIGN    prmDesc        = "".
IF prmCost       = ?  THEN ASSIGN    prmCost        = 0.
IF prmMl         = ?  THEN ASSIGN    prmMl          = "".
IF prmSimon      = ?  THEN ASSIGN    prmSimon       = "".
IF prmMark       = ?  THEN ASSIGN    prmMark        = 0.
IF prmAmort      = ?  THEN ASSIGN    prmAmort       = 0.
IF prmLine       = ?  THEN ASSIGN    prmLine        = 0.


{est/d-machex.i NEW}

DEF BUFFER b-ef FOR ef.
 DEF BUFFER b-eb FOR eb.
 DEF VAR li AS INT NO-UNDO.
 DEFINE VAR bi AS INT NO-UNDO.
 DEFINE VAR vEstimate AS CHAR NO-UNDO.
 def buffer bf-prep for est-prep.
 def var li-next as int no-undo.
 DEF BUFFER b-est-qty FOR est-qty.

 
    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
 

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  





IF prmAction = "PrepAdd"  THEN DO:

    FIND FIRST b-ef  WHERE b-ef.est-no = vEstimate AND  b-ef.form-no EQ prmSnum NO-LOCK NO-ERROR.
    IF NOT AVAIL b-ef THEN DO:
        ASSIGN
      cError = "Invalid  Sht, please re-enter...".
      RETURN .
    END.

    IF prmBnum <> 0 THEN DO:
       FIND FIRST b-eb  WHERE b-eb.est-no = vEstimate AND   b-eb.form-no EQ prmSnum
                      AND b-eb.blank-no EQ prmBnum NO-LOCK NO-ERROR.
    IF NOT AVAIL b-eb THEN DO:
        ASSIGN
      cError =  "Invalid  B# , please re-enter..." .
      RETURN .
    END.
  END.



  FIND FIRST prep  WHERE prep.company EQ prmComp
                      /*AND prep.loc     EQ est.loc*/
                      AND prep.code    EQ prmCode NO-LOCK NO-ERROR.
   IF NOT AVAIL prep THEN DO:
       ASSIGN
      cError =  "Invalid Code, try help...".
      RETURN .
    END.
    /* validate # inks and coat for Plate*/
    IF CAN-FIND(FIRST prep WHERE prep.company EQ prmComp
                             /*AND prep.loc     EQ est.loc*/
                             AND prep.code    EQ prmCode
                             AND prep.mat-type = "P")
        THEN DO:
           IF NOT CAN-FIND(FIRST b-eb  WHERE b-eb.est-no = vEstimate AND  b-eb.form-no EQ prmSnum
                      AND b-eb.blank-no EQ prmBnum
                      AND b-eb.i-col + b-eb.i-coat > 0)
           THEN DO:
                IF NOT CAN-FIND(FIRST b-eb  WHERE b-eb.est-no = vEstimate AND  b-eb.form-no EQ prmSnum
                      AND b-eb.i-col + b-eb.i-coat > 0)
                THEN DO:
                    cError =  "No Inks or Coats are defineded for this estimate's form  Enter Inks or Coats first before entering plates. " .
                    RETURN .
                END.                                 
           END.           
    END.   

  
  IF INDEX("SIMON",prmSimon) LE 0
    THEN DO:
      ASSIGN
      cError = "Simon code must be 'S', 'I', 'M', 'O', or 'N'..."  .
      RETURN .
    END.



  END.



IF prmAction = "PrepUpdate" THEN DO:

   FIND FIRST b-ef  WHERE b-ef.est-no = vEstimate AND  b-ef.form-no EQ prmSnum NO-LOCK NO-ERROR.
    IF NOT AVAIL b-ef THEN DO:
        ASSIGN
      cError = "Invalid  Sht, please re-enter...".
        
      RETURN .
    END.

    IF prmBnum <> 0 THEN DO:
       FIND FIRST b-eb  WHERE b-eb.est-no = vEstimate AND   b-eb.form-no EQ prmSnum
                      AND b-eb.blank-no EQ prmBnum NO-LOCK NO-ERROR.
    IF NOT AVAIL b-eb THEN DO:
        ASSIGN
      cError =  "Invalid  B# , please re-enter..." .
      RETURN .
    END.
  END.



  FIND FIRST prep  WHERE prep.company EQ prmComp
                      /*AND prep.loc     EQ est.loc*/
                      AND prep.code    EQ prmCode NO-LOCK NO-ERROR.
   IF NOT AVAIL prep THEN DO:
       ASSIGN
      cError =  "Invalid Code, try help...".
      RETURN .
    END.
    /* validate # inks and coat for Plate*/
    IF CAN-FIND(FIRST prep WHERE prep.company EQ prmComp
                             /*AND prep.loc     EQ est.loc*/
                             AND prep.code    EQ prmCode
                             AND prep.mat-type = "P")
        THEN DO:
           IF NOT CAN-FIND(FIRST b-eb  WHERE b-eb.est-no = vEstimate AND  b-eb.form-no EQ prmSnum
                      AND b-eb.blank-no EQ prmBnum
                      AND b-eb.i-col + b-eb.i-coat > 0)
           THEN DO:
                IF NOT CAN-FIND(FIRST b-eb  WHERE b-eb.est-no = vEstimate AND  b-eb.form-no EQ prmSnum
                      AND b-eb.i-col + b-eb.i-coat > 0)
                THEN DO:
                    cError =  "No Inks or Coats are defineded for this estimate's form  Enter Inks or Coats first before entering plates. " .
                    RETURN .
                END.                                 
           END.           
    END.    
  
  IF INDEX("SIMON",prmSimon) LE 0
    THEN DO:
      ASSIGN
      cError = "Simon code must be 'S', 'I', 'M', 'O', or 'N'..."  .
      RETURN .
    END.


END. /*end of validation*/

IF prmAction = "UpdateQty" THEN DO:
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FOR EACH est-qty WHERE est-qty.company = est.company AND est-qty.est-no = est.est-no NO-LOCK:
   
    FIND FIRST b-est-qty
                WHERE b-est-qty.company EQ est-qty.company
                  AND b-est-qty.est-no  EQ est-qty.est-no
                  AND b-est-qty.eqty    EQ prmQty  AND ROWID(b-est-qty)  NE ROWID(est-qty) NO-LOCK NO-ERROR.
   IF AVAIL b-est-qty THEN DO:
       ASSIGN
           cError = "Sorry, this quantity already exists..." .
              RETURN .
    END.
    

   IF  prmOldQty <> 0 THEN DO:
  
    FIND FIRST b-eb
        WHERE b-eb.company EQ est-qty.company
          AND b-eb.est-no  EQ est-qty.est-no
          AND (b-eb.eqty   EQ est-qty.eqty OR
               b-eb.eqty   EQ prmOldQty)  NO-LOCK NO-ERROR.
    
    IF AVAIL b-eb THEN DO:
        
        ASSIGN
            cError =  "Sorry, you may not change this quantity entered on the estimate..." .
      RETURN .
    END.
   END.
  
   END. /* endof for each*/


END. /*end of UpdateQty */

IF prmAction = "AddQty" THEN DO:
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST  est-qty WHERE est-qty.company = est.company AND est-qty.est-no = est.est-no AND est-qty.eqty = prmQty  NO-LOCK NO-ERROR.
   
    FIND FIRST b-est-qty
                WHERE b-est-qty.company EQ est-qty.company
                  AND b-est-qty.est-no  EQ est-qty.est-no
                  AND b-est-qty.eqty    EQ prmQty  /*AND ROWID(b-est-qty)  NE ROWID(est-qty)*/  NO-LOCK NO-ERROR.
    
   IF AVAIL b-est-qty THEN DO:
       ASSIGN
           cError = "Sorry, this quantity already exists..." .
         
              RETURN .
    END.
   

   IF  prmQty <> 0 THEN DO:
    FIND FIRST b-eb
        WHERE b-eb.company EQ est-qty.company
          AND b-eb.est-no  EQ est-qty.est-no
          AND (b-eb.eqty   EQ est-qty.eqty OR
               b-eb.eqty   EQ prmQty)  NO-LOCK NO-ERROR.
    IF AVAIL b-eb THEN DO:
        
        ASSIGN
            cError =  "Sorry, you may not change this quantity entered on the estimate..." .
      RETURN .
    END.
   END.
   

END. /* end of addqty*/


IF prmAction = "DeleteQty" THEN DO:
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
     FIND FIRST  est-qty WHERE est-qty.est-no = est.est-no AND est-qty.company = est.company AND est-qty.eqty = prmQty EXCLUSIVE-LOCK NO-ERROR.

     IF prmQty GT 0 THEN DO:
        
         FIND FIRST b-eb
             WHERE b-eb.company EQ est-qty.company
             AND b-eb.est-no  EQ est-qty.est-no
             AND b-eb.eqty    EQ est-qty.eqty  NO-LOCK NO-ERROR.
 
         IF AVAIL b-eb THEN DO:
             
             ASSIGN
             cError = "Sorry, you may not delete the quantity entered on the estimate..."  .
             
             RETURN .
           END.
         END.

END.





 
/**********************procedure*******************************/


PROCEDURE create-reft4plate :

  DEF VAR lv-prep-cnt AS INT NO-UNDO.
  DEF VAR lv-returnc AS cha NO-UNDO.
  DEF VAR lv-form# AS INT NO-UNDO.
  DEF VAR lv-line# AS INT NO-UNDO.
  DEF VAR lv-eqty AS INT NO-UNDO.

  FIND FIRST oe-ordm WHERE oe-ordm.company = est.company
                 AND oe-ordm.ord-no = est.ord-no
                 AND oe-ordm.charge = est-prep.CODE NO-LOCK NO-ERROR.
  IF AVAIL oe-ordm AND 
     NOT can-find(FIRST reftable
                  WHERE reftable.reftable EQ "oe/ordlmisc.p"
                    AND reftable.company  EQ oe-ordm.company
                    AND reftable.loc      EQ STRING(oe-ordm.ord-no,"9999999999")
                    AND reftable.code     EQ STRING(oe-ordm.line,"9999999999")
                    AND reftable.code2    EQ oe-ordm.charge
                    AND reftable.val[1] = 1)
  THEN DO:      
      CREATE reftable.
      ASSIGN reftable.reftable = "oe/ordlmisc.p"
             reftable.company  = oe-ordm.company
             reftable.loc      = STRING(oe-ordm.ord-no,"9999999999")
             reftable.code     = STRING(oe-ordm.line,"9999999999")
             reftable.code2    = oe-ordm.charge
             reftable.val[1] = 1
             reftable.val[2]   = est-prep.eqty
             reftable.val[3]   = est-prep.line
             reftable.dscr     = est-prep.est-no.    

  END.
END PROCEDURE.


PROCEDURE valid-s-num :

END PROCEDURE.


PROCEDURE valid-b-num :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER b-eb FOR eb.
  
END PROCEDURE.


PROCEDURE valid-code :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
    
END PROCEDURE.


PROCEDURE valid-simon :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
                              
  

END PROCEDURE.


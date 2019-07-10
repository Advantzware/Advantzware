


/*------------------------------------------------------------------------
    File        : GetDefaultQty1.p
    Purpose     : Get Default Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttGetDefaultQty1 NO-UNDO               
        FIELD vDoGsa AS CHARACTER
        FIELD vDoMr AS CHARACTER
        FIELD vDoSpeed AS CHARACTER
        FIELD vDropRc AS CHARACTER 
        FIELD vFormNo AS INTEGER
        FIELD vBlankNo AS INTEGER
        FIELD vCustNo AS CHARACTER
        FIELD vPartNo AS CHARACTER
        FIELD vBlQty AS INTEGER
        FIELD vYldQty AS INTEGER
        FIELD vRelease AS DECIMAL             
        .
   
       
       


    DEFINE DATASET dsGetDefaultQty1 FOR ttGetDefaultQty1 .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmEstimate   AS CHARACTER NO-UNDO.    

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGetDefaultQty1.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

    IF prmUser   = ?  THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".    
    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".  
                 

DEF VAR prmComp AS CHAR NO-UNDO.

/*def shared buffer xest for est.*/
DEF VAR ll-use-defaults AS LOG NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".


    IF prmAction = "GetDefaultValue" THEN DO:

        /*FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.

        find first sys-ctrl where sys-ctrl.company eq prmComp and sys-ctrl.name    eq "CEDFAULT" no-lock no-error.
        if avail sys-ctrl then do:        
            ll-use-defaults = sys-ctrl.log-fld.
            {est/recalc-mr.i xest}
            FIND CURRENT recalc-mr NO-LOCK.

            {sys/inc/cerun.i C}
        END.*/


        FOR EACH eb WHERE eb.company EQ prmComp AND eb.est-no  EQ FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND eb.form-no NE 0 NO-LOCK, 
            FIRST reftable WHERE reftable.reftable EQ "ce/com/selwhif1.w"             AND 
                reftable.company  EQ eb.company                      AND 
                reftable.loc      EQ eb.est-no                       AND 
                reftable.code     EQ STRING(eb.form-no,"9999999999") AND 
                reftable.code2    EQ STRING(eb.blank-no,"9999999999")  NO-LOCK BY eb.form-no:
        CREATE ttGetDefaultQty1.
        ASSIGN
            /*ttGetDefaultQty1.vDoGsa     = 
            ttGetDefaultQty1.vDoMr      = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE (recalc-mr.val[1] EQ 1)
            ttGetDefaultQty1.vDoSpeed   = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.recalc
            ttGetDefaultQty1.vDropRc    = */
            ttGetDefaultQty1.vFormNo    = eb.form-no
            ttGetDefaultQty1.vBlankNo   = eb.blank-no
            ttGetDefaultQty1.vCustNo    = eb.cust-no 
            ttGetDefaultQty1.vPartNo    = eb.part-no
            ttGetDefaultQty1.vBlQty     = eb.bl-qty
            ttGetDefaultQty1.vYldQty    = eb.yld-qty
            ttGetDefaultQty1.vRelease   = reftable.val[1]
            .
    END.                                
END.

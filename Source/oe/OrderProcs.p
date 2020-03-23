
/*------------------------------------------------------------------------
    File        : OrderProcs.p
    Purpose     : Centralization of numerous common functions in the import and manual entry of orders

    Syntax      :

    Description : Holds procedures for entering, editing and processing orders

    Author(s)   : BV
    Created     : Tue Jun 04 13:53:09 EDT 2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE VARIABLE hdOeValidate AS HANDLE.
{custom/globdefs.i}     /*Refactor - hate this*/
{sys/inc/var.i SHARED}  /*Refactor - hate this*/
{oe/relcheck.i NEW} /* temp-table definitions */
DEFINE VARIABLE glUseTransCust      AS LOGICAL   NO-UNDO. /* how to set cust no on release */
/*DEFINE VARIABLE glCheckCredit       AS LOGICAL   NO-UNDO. /* check credit hold */*/
DEFINE VARIABLE gcDefaultLocMethod  AS CHARACTER NO-UNDO. /* how to get default loc */
DEFINE VARIABLE glRecalcSkippedInTrigger   AS LOGICAL   NO-UNDO. /* not used */
DEFINE VARIABLE gcOnOrderQtyCode    LIKE sys-ctrl.char-fld NO-UNDO. /* determines if on order quantity used */  
DEFINE VARIABLE gcTagSelectionCode  AS CHARACTER NO-UNDO. /* Tag selection code */
DEFINE VARIABLE glUseItemfgLoc      AS LOGICAL   NO-UNDO. /* Get location from itemfg? */
DEFINE VARIABLE gcCompanyDefaultBin AS CHARACTER NO-UNDO.  /* default bin */

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetSettingJobCreate RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.


/* ***************************  Main Block  *************************** */


/* **********************  Internal Procedures  *********************** */

PROCEDURE CalcOrderCommission:
    /*------------------------------------------------------------------------------
     Purpose: Given an order, calculates commission related information
     Notes: Replaces oe/oe-comm.p
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriRowid AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ordm FOR oe-ordm.
    DEFINE BUFFER bf-cust    FOR cust.
    DEFINE BUFFER bf-itemfg  FOR itemfg.
    DEFINE BUFFER bf-prep    FOR prep.

    DEFINE VARIABLE iSman            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cCustID          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCustType        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cProductCat      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dCostExtended    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cCommissionBasis AS CHARACTER NO-UNDO.

    FIND FIRST bf-oe-ord WHERE ROWID(bf-oe-ord) EQ ipriRowid EXCLUSIVE-LOCK.


    FIND FIRST bf-cust NO-LOCK 
        WHERE bf-cust.company EQ bf-oe-ord.company
        AND bf-cust.cust-no EQ bf-oe-ord.cust-no
        NO-ERROR.
    IF AVAILABLE bf-cust THEN
        ASSIGN 
            cCustID   = bf-cust.cust-no
            cCustType = bf-cust.type
            . 
    
    bf-oe-ord.t-comm = 0.
    FOR EACH bf-oe-ordl OF bf-oe-ord NO-LOCK:
        dCostExtended = (bf-oe-ordl.qty / 1000) * bf-oe-ordl.cost.

        FIND FIRST bf-itemfg NO-LOCK
            WHERE bf-itemfg.company EQ bf-oe-ordl.company
            AND bf-itemfg.i-no    EQ bf-oe-ordl.i-no
            NO-ERROR.
        IF AVAILABLE bf-itemfg THEN 
            ASSIGN 
                cProductCat = bf-itemfg.procat
                .
        DO iSman = 1 TO 3:
            IF bf-oe-ordl.s-man[iSman] NE "" THEN 
            DO:
                RUN custom/combasis.p (bf-oe-ord.company, bf-oe-ordl.s-man[iSman], cCustType, cProductCat, 0, cCustID, OUTPUT cCommissionBasis).

                bf-oe-ord.t-comm = bf-oe-ord.t-comm +
                    ROUND((bf-oe-ordl.t-price - IF cCommissionBasis EQ "G" THEN dCostExtended ELSE 0) *
                    (bf-oe-ordl.s-pct[iSman] / 100) * (bf-oe-ordl.s-comm[iSman] / 100),2).
            END.
        END.
    END.

    FOR EACH bf-oe-ordm OF bf-oe-ord NO-LOCK, 
        FIRST bf-prep NO-LOCK
        WHERE bf-prep.company EQ bf-oe-ordm.company
        AND bf-prep.code    EQ bf-oe-ordm.charge
        AND bf-prep.commissionable:
             
        DO iSman = 1 TO 3:
            IF bf-oe-ordm.s-man[iSman] NE "" THEN 
            DO:
                RUN custom/combasis.p (bf-oe-ord.company, bf-oe-ordm.s-man[iSman], cCustType, bf-prep.fgcat, 0, cCustID, OUTPUT cCommissionBasis).

                bf-oe-ord.t-comm = bf-oe-ord.t-comm +
                    ROUND((bf-oe-ordm.amt - IF cCommissionBasis EQ "G" THEN bf-oe-ordm.cost ELSE 0) *
                    (bf-oe-ordm.s-pct[iSman] / 100) * (bf-oe-ordm.s-comm[iSman] / 100),2).
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE CheckPOLineStatus:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoNo    AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiPoLine  AS INTEGER   NO-UNDO.
    
    FIND FIRST po-ordl NO-LOCK
         WHERE po-ordl.company EQ ipcCompany
           AND po-ordl.po-no   EQ ipiPoNo
           AND po-ordl.line    EQ ipiPoLine
         NO-ERROR.
    IF AVAILABLE po-ordl AND NOT po-ordl.opened THEN DO:
        RUN DisplayMessage("19").
        RETURN ERROR.
    END. 

END PROCEDURE.

PROCEDURE GetReleaseType:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER ipcSCode AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER iplIsAComponent AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER opcRelType AS CHARACTER NO-UNDO.

   FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ ipcCompany NO-LOCK NO-ERROR.
   
    IF ipcSCode <> "" THEN 
        opcRelType = ipcSCode.
    ELSE 
        opcRelType = "B". /*Default */
        
    
    IF iplIsAComponent THEN opcRelType = "S".
    
    IF AVAILABLE oe-ctrl THEN DO:        
      opcRelType = IF oe-ctrl.ship-from THEN "B" ELSE "I".
    END.

END PROCEDURE.

PROCEDURE pCreateActRelLine PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  Replaced oe/cre-rell.p but with no tag selection which must be added
              - Note:  references locode
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rel FOR oe-rel.
    DEFINE PARAMETER BUFFER ipbf-oe-relh FOR oe-relh.
    DEFINE INPUT PARAMETER iRelNo AS INTEGER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oprOeRellRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.

    DEFINE VARIABLE lError            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cErrMsg           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cRelType AS CHARACTER NO-UNDO.

    IF NOT AVAIL(ipbf-oe-relh) THEN
        ASSIGN oplError = TRUE 
               opcMessage = "Release header record not found"
               .          
    IF NOT AVAIL(ipbf-oe-rel) THEN
        ASSIGN oplError = TRUE 
               opcMessage = "Scheduled release record not found"
               .
    IF oplError THEN
        RETURN.
        
    FIND FIRST bf-oe-ordl no-lock
        WHERE bf-oe-ordl.company  EQ ipbf-oe-rel.company
          AND bf-oe-ordl.ord-no   EQ ipbf-oe-rel.ord-no
          AND bf-oe-ordl.line EQ ipbf-oe-rel.line
        NO-ERROR.
        
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipbf-oe-rel.company
          AND itemfg.i-no    EQ ipbf-oe-rel.i-no
        NO-ERROR.        
    // RUN get-next-r-no.
    RUN GetReleaseType (INPUT ipbf-oe-rel.company, "" /* Existing s-code */, bf-oe-ordl.is-a-component, OUTPUT cRelType).
    CREATE bf-oe-rell.
    ASSIGN
        oprOeRellRow       = ROWID(bf-oe-rell)
        bf-oe-rell.company    = ipbf-oe-rel.company
        bf-oe-rell.r-no       = ipbf-oe-relh.r-no
        bf-oe-rell.rel-no     = iRelNo
        bf-oe-rell.loc        = IF gcDefaultLocMethod NE "ShipFromWhse" THEN locode ELSE ipbf-oe-rel.spare-char-1
        bf-oe-rell.ord-no     = ipbf-oe-rel.ord-no
        bf-oe-rell.qty        = ipbf-oe-rel.tot-qty
        bf-oe-rell.i-no       = ipbf-oe-rel.i-no
        bf-oe-rell.job-no     = bf-oe-ordl.job-no
        bf-oe-rell.job-no2    = bf-oe-ordl.job-no2
        bf-oe-rell.po-no      = ipbf-oe-rel.po-no
        bf-oe-rell.line       = ipbf-oe-rel.line
        bf-oe-rell.lot-no     = ipbf-oe-rel.lot-no
        bf-oe-rell.frt-pay    = ipbf-oe-rel.frt-pay
        bf-oe-rell.fob-code   = ipbf-oe-rel.fob-code
        bf-oe-rell.sell-price = ipbf-oe-rel.sell-price
        bf-oe-rell.zeroPrice  = ipbf-oe-rel.zeroPrice
        bf-oe-rell.printed    = NO
        bf-oe-rell.posted     = NO
        bf-oe-rell.deleted    = NO
        /** Set link to the planned releases **/
        bf-oe-rell.link-no    = ipbf-oe-rel.r-no
        bf-oe-rell.s-code     = cRelType    
        bf-oe-rell.partial = IF bf-oe-rell.s-code EQ "I" THEN bf-oe-ordl.partial ELSE 0
        bf-oe-rell.qty-case = IF AVAILABLE itemfg AND itemfg.case-count GT 0
            THEN itemfg.case-count
            ELSE
            IF bf-oe-ordl.cas-cnt GT 0 THEN bf-oe-ordl.cas-cnt
            ELSE 1
        bf-oe-rell.cases   = TRUNC((bf-oe-rell.qty - bf-oe-rell.partial) /
                            bf-oe-rell.qty-case,0)
        bf-oe-rell.partial = bf-oe-rell.qty - (bf-oe-rell.cases * bf-oe-rell.qty-case)
        .                     

    RUN oe/rel-stat-upd.p (ROWID(bf-oe-rell)).

    /* Fill in correct bin/loc */
    IF AVAILABLE bf-oe-rell  THEN
        RUN pSetActualReleaseLocation (BUFFER bf-oe-rell, BUFFER ipbf-oe-rel, OUTPUT lError, OUTPUT cErrMsg).

    /* Set values for invoice only */
    FIND CURRENT bf-oe-rell EXCLUSIVE-LOCK.
    IF bf-oe-rell.s-code = "I" THEN
       bf-oe-rell.loc-bin = "".
       
     FIND CURRENT ipbf-oe-rel EXCLUSIVE-LOCK.
     ASSIGN 
        ipbf-oe-rel.rel-no   = iRelNo
        ipbf-oe-rel.b-ord-no = ipbf-oe-relh.b-ord-no
        ipbf-oe-rel.qty      = ipbf-oe-rel.tot-qty
        .       
    RUN oe/rel-stat.p (ROWID(ipbf-oe-rel), OUTPUT ipbf-oe-rel.stat).
    
    FIND CURRENT ipbf-oe-rel NO-LOCK NO-ERROR.
    FIND CURRENT bf-oe-rell NO-LOCK NO-ERROR.
    
    IF glRecalcSkippedInTrigger THEN DO:
        /* Corrects data integrity issue until auditing can identify problem */
        /* Run if recalc is skipped in oe-rell trigger */        
        FIND CURRENT itemfg EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF AVAILABLE itemfg THEN DO:
            RUN fg/calcqa&b.p (ROWID(itemfg), 
                               OUTPUT itemfg.q-alloc,
                               OUTPUT itemfg.q-back
                               ).
            itemfg.q-avail = itemfg.q-onh +
                              (IF gcOnOrderQtyCode EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) -
                              itemfg.q-alloc.
            FOR EACH itemfg-loc WHERE itemfg-loc.company EQ itemfg.company
                                  AND itemfg-loc.i-no    EQ itemfg.i-no
                                EXCLUSIVE-LOCK:
                RUN fg/calcqabl.p (ROWID(itemfg), 
                                  itemfg-loc.loc, 
                                  OUTPUT itemfg-loc.q-alloc,
                                  OUTPUT itemfg-loc.q-back
                                  ).
                itemfg-loc.q-avail = itemfg-loc.q-onh +
                                 (IF gcOnOrderQtyCode EQ "XOnOrder" THEN 0 ELSE itemfg-loc.q-ono) -
                                 itemfg-loc.q-alloc.
            END.         
        END.                 
    END.
    FIND CURRENT itemfg NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE pCopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

    DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.  

END PROCEDURE.

PROCEDURE pCreateActRelHeader PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:  Replaces oe/cre-relh.p except for credit check
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rel FOR oe-rel.
    DEFINE OUTPUT PARAMETER oprOeRelhRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iNextRNo        AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE iNextReleaseNum AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOrigProgram    AS CHARACTER FORMAT "x(50)" NO-UNDO.
    DEFINE VARIABLE lCreditHold     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustCode       AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-cust FOR cust.

    IF NOT AVAIL(ipbf-oe-rel) THEN
        ASSIGN oplError = TRUE 
               opcMessage = "Release record not found"
               .
    
/*    ASSIGN                                  */
/*        cOrigProgram = TRIM(PROGRAM-NAME(2))*/
/*        lCreditHold  = NO                   */
/*        .                                   */
/*    IF glCheckCredit THEN DO:                                     */
/*        FIND FIRST cust NO-LOCK                                   */
/*            WHERE cust.company EQ ipbf-oe-rel.company             */
/*              AND cust.cust-no EQ ipbf-oe-rel.cust-no             */
/*            NO-ERROR.                                             */
/*        IF AVAILABLE cust AND cOrigProgram NE "fg/invrecpt.p" THEN*/
/*                RUN oe/CRcheck.p ( INPUT ROWID(cust),             */
/*                                   INPUT YES,                     */
/*                                   OUTPUT lCreditHold ).          */
/*    END.                                                          */
    RUN oe/getNextRelNo.p (INPUT "oe-relh", 
                           OUTPUT iNextRNo).

    RUN oe/release#.p (ipbf-oe-rel.company, 
                       OUTPUT iNextReleaseNum).

    IF glUseTransCust = YES  AND ipbf-oe-rel.s-code EQ 'T' THEN 
    DO:
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipbf-oe-rel.company 
              AND cust.active EQ 'X'  
            NO-ERROR.
        IF AVAILABLE cust THEN 
        DO:
            IF CAN-FIND(FIRST shipto 
                WHERE shipto.company EQ ipbf-oe-rel.company 
                  AND shipto.cust-no EQ cust.cust-no 
                  AND shipto.ship-no EQ ipbf-oe-rel.ship-no 
                  AND shipto.ship-id EQ ipbf-oe-rel.ship-id) 
                THEN ASSIGN cCustCode = cust.cust-no.

            RELEASE cust.
        END.
    END.
    ELSE 
        cCustCode = ipbf-oe-rel.cust-no.
  
    CREATE oe-relh.  
    ASSIGN     
        oprOeRelhRow      = ROWID(oe-relh)
        oe-relh.cust-no   = cCustCode  
        oe-relh.r-no      = iNextRNo
        oe-relh.company   = ipbf-oe-rel.company
        oe-relh.ship-no   = ipbf-oe-rel.ship-no
        oe-relh.ship-id   = ipbf-oe-rel.ship-id
        oe-relh.ship-i[1] = ipbf-oe-rel.ship-i[1]
        oe-relh.ship-i[2] = ipbf-oe-rel.ship-i[2]
        oe-relh.ship-i[3] = ipbf-oe-rel.ship-i[3]
        oe-relh.ship-i[4] = ipbf-oe-rel.ship-i[4]
        oe-relh.carrier   = ipbf-oe-rel.carrier
        oe-relh.printed   = NO
        oe-relh.posted    = NO
        oe-relh.deleted   = NO
        oe-relh.rel-date  = ipbf-oe-rel.rel-date
        oe-relh.release#  = iNextReleaseNum
        oe-relh.user-id   = USERID("nosweat")
        oe-relh.upd-time  = TIME
        oe-relh.upd-date  = TODAY
        /* oe-relh.w-ord     = lCreditHold */
        .
       
    RUN pCopyShipNote (ipbf-oe-rel.rec_key, oe-relh.rec_key).

/*    IF lCreditHold THEN                                              */
/*    DO:                                                              */
/*        FIND FIRST bf-cust EXCLUSIVE-LOCK                            */
/*            WHERE bf-cust.company EQ ipbf-oe-rel.company             */
/*            AND bf-cust.cust-no EQ ipbf-oe-rel.cust-no USE-INDEX cust*/
/*            NO-WAIT NO-ERROR.                                        */
/*        IF AVAILABLE bf-cust THEN                                    */
/*            ASSIGN bf-cust.cr-hold = YES                             */
/*                .                                                    */
/*    END.                                                             */
    
END PROCEDURE.

PROCEDURE pApproveImportedOrder PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Moves a given order buffer from O-W to O-U-1 
     Notes: from oe/v-ord.w hold-approve
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    
    DEFINE VARIABLE iSman       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lHold       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cHoldReason AS CHARACTER NO-UNDO.
       
    DEFINE VARIABLE dMargin     AS DECIMAL   NO-UNDO.  /*Refactor?*/
    DEFINE VARIABLE cUOM        AS CHARACTER NO-UNDO.  /*Refactor?*/

    
   
    FIND CURRENT ipbf-oe-ord EXCLUSIVE-LOCK.
    ASSIGN
        ipbf-oe-ord.user-id     = USERID("nosweat")
        ipbf-oe-ord.approved-id = USERID("nosweat")
        ipbf-oe-ord.t-freight   = 0.

    IF ipbf-oe-ord.type EQ "" THEN ipbf-oe-ord.type = "O".

    IF ipbf-oe-ord.sman[1] EQ "" THEN
        ASSIGN
            ipbf-oe-ord.sman    = ""
            ipbf-oe-ord.sman[1] = cust.sman.

    IF ipbf-oe-ord.sman[1] NE "" AND ipbf-oe-ord.s-pct[1] EQ 0 THEN
        ipbf-oe-ord.s-pct[1] = 100.00.

    DO iSman = 1 TO EXTENT(ipbf-oe-ord.sman):
        IF ipbf-oe-ord.s-comm[iSman] GE 100 THEN
            ASSIGN
                ipbf-oe-ord.s-comm = 0
                dMargin            = 0
                .

        FIND FIRST sman
            WHERE sman.company EQ ipbf-oe-ord.company
            AND sman.sman    EQ ipbf-oe-ord.sman[iSman]
            NO-LOCK NO-ERROR.
        IF AVAILABLE sman THEN 
        DO:
            ipbf-oe-ord.sname[1] = sman.sname.

            IF ipbf-oe-ord.s-comm[iSman] LE 0 THEN
            DO:
                ipbf-oe-ord.s-comm[iSman] = sman.scomm.
                IF iSman = 1 THEN
                    dMargin = 0.
            END.
        END.
    END. /* do iSman = 1 to sman */

    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
        AND bf-oe-ordl.job-no  EQ ""
        AND bf-oe-ordl.i-no    NE "",
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ bf-oe-ordl.company
        AND itemfg.i-no    EQ bf-oe-ordl.i-no:

        FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(bf-oe-ordl)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE oe-ordl THEN NEXT.

        ASSIGN
            oe-ordl.req-code  = ipbf-oe-ord.due-code
            oe-ordl.req-date  = ipbf-oe-ord.due-date
            oe-ordl.prom-code = ipbf-oe-ord.due-code
            oe-ordl.prom-date = ipbf-oe-ord.due-date.

        DO iSman = 1 TO MIN(EXTENT(oe-ordl.s-man),EXTENT(ipbf-oe-ord.sman)):
            ASSIGN
                oe-ordl.s-man[iSman]  = ipbf-oe-ord.sman[iSman]
                oe-ordl.s-pct[iSman]  = ipbf-oe-ord.s-pct[iSman]
                oe-ordl.s-comm[iSman] = ipbf-oe-ord.s-comm[iSman].

            IF iSman = 1 THEN
                ASSIGN
                    oe-ordl.q-qty = ipbf-oe-ord.t-fuel
                    dMargin       = ipbf-oe-ord.t-fuel.
        END.

        FIND FIRST po-ordl NO-LOCK
            WHERE po-ordl.company   EQ oe-ordl.company
            AND po-ordl.i-no      EQ oe-ordl.i-no
            AND po-ordl.po-no     EQ oe-ordl.po-no-po
            AND po-ordl.item-type EQ NO
            USE-INDEX item-ordno NO-ERROR.
        IF AVAILABLE po-ordl THEN
            ASSIGN
                cUOM         = po-ordl.cons-uom
                oe-ordl.cost = po-ordl.cons-cost.
        ELSE
            ASSIGN
                cUOM         = itemfg.prod-uom
                oe-ordl.cost = itemfg.total-std-cost.

        IF cUOM NE "M" THEN
            RUN sys/ref/convcuom.p(cUOM, "M", 0, 0, 0, 0,
                oe-ordl.cost, OUTPUT oe-ordl.cost).

        RUN oe/ordlfrat.p (ROWID(oe-ordl), OUTPUT oe-ordl.t-freight).
        ipbf-oe-ord.t-freight = ipbf-oe-ord.t-freight + oe-ordl.t-freight.
        FIND CURRENT oe-ordl NO-LOCK.
        RELEASE oe-ordl.
    END. /* Each oe-ordl */
  

    RUN oe/ordfrate.p (ROWID(ipbf-oe-ord)).
    
    RUN CalcOrderCommission(ROWID(ipbf-oe-ord), OUTPUT oplError, OUTPUT opcMessage).
    
    RUN oe/calcordt.p (ROWID(ipbf-oe-ord)).

    RUN pValidateOrder(BUFFER ipbf-oe-ord, OUTPUT lHold, OUTPUT cHoldReason).    
    IF ipbf-oe-ord.job-no EQ "" THEN DO:
        IF lHold THEN 
        DO:
            ipbf-oe-ord.stat = "H".
            RUN oe/syncJobHold.p (INPUT ipbf-oe-ord.company, INPUT ipbf-oe-ord.ord-no, INPUT "Hold").
        END.
        ELSE 
        DO:
            ASSIGN
                ipbf-oe-ord.stat = "A"
                ipbf-oe-ord.approved-date = TODAY.
            RUN oe/syncJobHold.p (INPUT ipbf-oe-ord.company, INPUT ipbf-oe-ord.ord-no, INPUT "Approve").
            RUN ReleaseOrder(ROWID(ipbf-oe-ord), OUTPUT oplError, OUTPUT opcMessage).          
        END.
    END.
        ELSE ipbf-oe-ord.stat = "W".  /*Refactor - if we add ability to build job, this can be removed*/
        
    FIND CURRENT ipbf-oe-ord NO-LOCK.
    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company
        AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
        AND bf-oe-ordl.job-no  EQ ""
        AND bf-oe-ordl.i-no    NE "":

        FIND oe-ordl WHERE ROWID(oe-ordl) EQ ROWID(bf-oe-ordl)
            EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
        IF NOT AVAILABLE oe-ordl THEN NEXT.
        oe-ordl.stat = (IF ipbf-oe-ord.stat EQ "W"  THEN ipbf-oe-ord.stat ELSE "").
    END.
/*    IF fGetSettingJobCreate(ipbf-oe-ord.company) AND ipbf-oe-ord.job-no NE "" THEN                          */
/*    DO TRANSACTION: /*create job*/                                                                          */
/*        FIND FIRST job NO-LOCK                                                                              */
/*            WHERE job.company EQ ipbf-oe-ord.company                                                        */
/*            AND job.job-no  EQ ipbf-oe-ord.job-no                                                           */
/*            AND job.job-no2 EQ ipbf-oe-ord.job-no2                                                          */
/*            NO-ERROR.                                                                                       */
/*                                                                                                            */
/*        IF AVAILABLE job AND TRIM(job.est-no) NE TRIM(ipbf-oe-ord.est-no) THEN                              */
/*            IF CAN-FIND(FIRST job-hdr                                                                       */
/*                WHERE job-hdr.company EQ job.company                                                        */
/*                AND job-hdr.job     EQ job.job                                                              */
/*                AND job-hdr.job-no  EQ job.job-no                                                           */
/*                AND job-hdr.job-no2 EQ job.job-no2                                                          */
/*                AND job-hdr.ord-no  NE ipbf-oe-ord.ord-no) OR                                               */
/*                CAN-FIND(FIRST bf-oe-ord                                                                    */
/*                WHERE bf-oe-ord.company EQ job.company                                                      */
/*                AND bf-oe-ord.job-no  EQ job.job-no                                                         */
/*                AND bf-oe-ord.job-no2 EQ job.job-no2                                                        */
/*                AND bf-oe-ord.est-no  EQ job.est-no)   OR                                                   */
/*                CAN-FIND(FIRST bf-oe-ordl                                                                   */
/*                WHERE bf-oe-ordl.company EQ job.company                                                     */
/*                AND bf-oe-ordl.job-no  EQ job.job-no                                                        */
/*                AND bf-oe-ordl.job-no2 EQ job.job-no2                                                       */
/*                AND bf-oe-ordl.est-no  EQ job.est-no)  THEN RELEASE job.                                    */
/*            ELSE                                                                                            */
/*            DO:                                                                                             */
/*                FIND CURRENT job NO-ERROR.                                                                  */
/*                IF AVAILABLE job THEN DELETE job.                                                           */
/*            END.                                                                                            */
/*                                                                                                            */
/*        IF NOT AVAILABLE job THEN                                                                           */
/*        DO:                                                                                                 */
/*            RUN create-job (OUTPUT lv-job-recid).                                                           */
/*            FIND job WHERE RECID(job) = lv-job-recid NO-LOCK.                                               */
/*        END.                                                                                                */
/*                                                                                                            */
/*        v-qty-mod = YES.                                                                                    */
/*                                                                                                            */
/*        IF AVAILABLE job AND INDEX("HWPRL",job.stat) NE 0 THEN                                              */
/*        DO:                                                                                                 */
/*            /*IF NOT v-qty-mod THEN                                                                         */
/*               RUN oe/job-qty.p (ROWID(ipbf-oe-ord), OUTPUT v-qty-mod).*/                                   */
/*                                                                                                            */
/*            IF v-qty-mod OR job.stat EQ "P" THEN                                                            */
/*            DO:                                                                                             */
/*                RUN jc/chkrebld.p (RECID(job), OUTPUT choice).                                              */
/*                IF NOT choice THEN                                                                          */
/*                DO:                                                                                         */
/*                    ASSIGN                                                                                  */
/*                        hld-id     = fil_id                                                                 */
/*                        hld-nufile = nufile                                                                 */
/*                        hld-stat   = job.stat                                                               */
/*                        nufile     = YES.                                                                   */
/*                                                                                                            */
/*                    RUN jc/jc-calc.p(RECID(job), NO).                                                       */
/*                    ASSIGN                                                                                  */
/*                        fil_id = hld-id                                                                     */
/*                        nufile = hld-nufile.                                                                */
/*                                                                                                            */
/*                    IF hld-stat NE "P" THEN                                                                 */
/*                    DO:                                                                                     */
/*                        FIND CURRENT job EXCLUSIVE.                                                         */
/*                        job.stat = hld-stat.                                                                */
/*                        FIND CURRENT job NO-LOCK.                                                           */
/*                    END.                                                                                    */
/*                END.                                                                                        */
/*            END.                                                                                            */
/*        END.                                                                                                */
/*                                                                                                            */
/*        FIND FIRST sys-ctrl WHERE                                                                           */
/*            sys-ctrl.company EQ cocode AND                                                                  */
/*            sys-ctrl.name    EQ "SCHEDULE"                                                                  */
/*            NO-LOCK NO-ERROR.                                                                               */
/*                                                                                                            */
/*        v-run-schedule = IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "NoDate" AND sys-ctrl.log-fld THEN NO*/
/*        ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.char-fld = "PlanDate" AND sys-ctrl.log-fld THEN YES         */
/*        ELSE NO.                                                                                            */
/*                                                                                                            */
/*        FOR EACH oe-ordl NO-LOCK                                                                            */
/*            WHERE oe-ordl.company EQ cocode                                                                 */
/*            AND oe-ordl.ord-no  EQ oe-ord.ord-no                                                            */
/*            AND oe-ordl.is-a-component EQ NO                                                                */
/*                                                                                                            */
/*            BREAK BY oe-ordl.job-no                                                                         */
/*            BY oe-ordl.job-no2:                                                                             */
/*                                                                                                            */
/*            IF LAST-OF(oe-ordl.job-no2) THEN                                                                */
/*            DO:                                                                                             */
/*                ASSIGN                                                                                      */
/*                    hld-id     = fil_id                                                                     */
/*                    hld-nufile = nufile                                                                     */
/*                    fil_id     = RECID(oe-ordl).                                                            */
/*                                                                                                            */
/*                RUN po/doPo.p (YES) /* Yes Indicates to prompt for RM */.                                   */
/*                /* check oe-ordl.due-date and calc promised date and job's start-date */                    */
/*                                                                                                            */
/*                IF oe-ordl.est-no NE "" AND v-run-schedule THEN RUN update-start-date.                      */
/*                                                                                                            */
/*                ASSIGN                                                                                      */
/*                    fil_id = hld-id                                                                         */
/*                    nufile = hld-nufile.                                                                    */
/*            END.                                                                                            */
/*        END.                                                                                                */
/*    END.  /* transaction if v-create-job */                                                                 */


END PROCEDURE.


PROCEDURE pCreateFgBinForRelease PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes: Replaces create-fg-bin procedure in oe/cre-rell.p
------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbfOeRell FOR oe-rell.
    DEFINE BUFFER bfOeRell FOR oe-rell.
    DEFINE BUFFER bfFgBin  FOR fg-bin.
    DEFINE VARIABLE xLoc LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE xBin LIKE fg-bin.loc-bin NO-UNDO.

    ASSIGN 
        xBin = ipbfOeRell.loc-bin
        xLoc = ipbfOeRell.loc
        .

    CREATE bfFgBin.
    ASSIGN
        bfFgBin.company = ipbfOeRell.company
        bfFgBin.i-no    = ipbfOeRell.i-no
        bfFgBin.job-no  = ipbfOeRell.job-no
        bfFgBin.job-no2 = ipbfOeRell.job-no2
        bfFgBin.loc     = xLoc
        bfFgBin.loc-bin = xBin
        bfFgBin.tag     = ipbfOeRell.tag
        bfFgBin.cust-no = ipbfOeRell.cust-no
        . 
    RELEASE bfFgBin.

    /*Create a bin so that is shows up in IF4 -FG Bin (blank i-no)*/
    FIND FIRST bfFgBin NO-LOCK
        WHERE bfFgBin.company EQ ipbfOeRell.company 
          AND bfFgBin.loc EQ xLoc
          AND bfFgBin.loc-bin EQ xBin
          AND bfFgBin.i-no = ""
        NO-ERROR.
    IF NOT AVAILABLE bfFgBin THEN 
    DO:
        CREATE bfFgBin.
        ASSIGN 
            bfFgBin.company = ipbfOeRell.company
            bfFgBin.i-no    = ""
            bfFgBin.loc     = xLoc
            bfFgBin.loc-bin = xBin
            .
    END.
    FIND CURRENT bfFgBin NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE pSetGlobalSettings PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReturnChar AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound   AS CHARACTER NO-UNDO.
              
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "ADDXFER", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    glUseTransCust = LOGICAL(cReturnChar) NO-ERROR.
    
/*    RUN sys/ref/nk1look.p (INPUT ipcCompany, "RELCREDT", "L" /* Logical */, NO /* check by cust */,*/
/*        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,                       */
/*        OUTPUT cReturnChar, OUTPUT lRecFound).                                                     */
/*    glCheckCredit = LOGICAL(cReturnChar) NO-ERROR.                                                 */
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "OEREORDR", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcOnOrderQtyCode = cReturnChar NO-ERROR.    
    
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "BOLWHSE", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcDefaultLocMethod = cReturnChar NO-ERROR.
         
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "RelSkipRecalc", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    glRecalcSkippedInTrigger = LOGICAL(cReturnChar) NO-ERROR.

    RUN sys/ref/nk1look.p (INPUT ipcCompany, "AUTOPOST", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    glUseItemfgLoc = cReturnChar EQ "FGFILE" NO-ERROR.    
                     
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "addrelse", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcTagSelectionCode = cReturnChar NO-ERROR.
    
    /* Set gcCompanyDefaultBin in cre-rell */
    RUN sys/ref/nk1look.p (INPUT ipcCompany, "BOLPRINT", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    gcCompanyDefaultBin = cReturnChar NO-ERROR.    
END PROCEDURE.

PROCEDURE ReleaseOrder :
    /*------------------------------------------------------------------------------
     Purpose: Given a buffer oe-ord, release all lines at full quantity into 
     Actual Releases
     Notes:   Replaces oe/actRelMerge.p but does not contain any of the merge 
              logic - this needs to be added.
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lAllOrdLinesReleased AS LOGICAL NO-UNDO.
    DEFINE VARIABLE rOeRelh AS ROWID NO-UNDO.
    DEFINE VARIABLE rOeRell AS ROWID NO-UNDO.
    DEFINE VARIABLE iRelNo AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-oe-ord FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    
    ASSIGN lAllOrdLinesReleased = YES
           iRelNo = 0
           .
    FIND FIRST bf-oe-ord NO-LOCK
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
        NO-ERROR.
    IF NOT AVAIL bf-oe-ord THEN DO:
        ASSIGN oplError = TRUE
               opcMessage = "Invalid order rowid passed in."
               .
        RETURN.
    END.        
    RUN pSetGlobalSettings (INPUT bf-oe-ord.company).
    
    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ bf-oe-ord.company 
          AND bf-oe-ordl.ord-no  EQ bf-oe-ord.ord-no
        :
        FIND FIRST bf-oe-rel NO-LOCK 
            WHERE bf-oe-rel.company EQ bf-oe-ordl.company
              AND bf-oe-rel.ord-no  EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.line    EQ bf-oe-ordl.line
            NO-ERROR.
        IF NOT AVAIL bf-oe-rel THEN  
            lAllOrdLinesReleased = NO.
     END.
     IF NOT lAllOrdLinesReleased THEN DO:
         ASSIGN oplError = TRUE
                opcMessage = "Some order lines do not have a scheduled release."
                .
         RETURN.
     END.
     REL-LINES:
     FOR EACH bf-oe-rel NO-LOCK
        WHERE bf-oe-rel.company EQ bf-oe-ord.company 
          AND bf-oe-rel.ord-no  EQ bf-oe-ord.ord-no
        BREAK BY bf-oe-rel.ord-no:
            
        IF FIRST-OF(bf-oe-rel.ord-no) THEN DO:
           RUN pCreateActRelHeader (BUFFER bf-oe-rel, OUTPUT rOeRelh, OUTPUT oplError, OUTPUT opcMessage).
             IF oplError THEN 
                 LEAVE REL-LINES.
           
           FIND FIRST bf-oe-relh NO-LOCK
              WHERE ROWID(bf-oe-relh) EQ rOeRelh
              NO-ERROR. 
           IF NOT AVAIL bf-oe-relh THEN DO:
                ASSIGN oplError   = TRUE
                       opcMessage = "Actual release header not created."
                       .
                LEAVE REL-LINES.
           END.
        END.     
        FIND FIRST bf-oe-relh NO-LOCK
            WHERE ROWID(bf-oe-relh) EQ rOeRelh
            NO-ERROR.
        iRelNo = iRelNo + 1.
        RUN pCreateActRelLine (BUFFER bf-oe-rel, BUFFER bf-oe-relh, iRelNo, OUTPUT rOeRell, OUTPUT oplError, OUTPUT opcMessage).
     END. 
     
END PROCEDURE.

PROCEDURE ProcessImportedOrder:
    /*------------------------------------------------------------------------------
     Purpose: given a Rowid for an imported Order, process the order based on settings
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriOeOrd AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-oe-ord FOR oe-ord.

    FIND FIRST bf-oe-ord NO-LOCK 
        WHERE ROWID(bf-oe-ord) EQ ipriOeOrd
        NO-ERROR.
    IF NOT AVAILABLE bf-oe-ord THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Order - Rowid Not found"
            .
    END.
    ELSE 
    DO:        
        RUN pApproveImportedOrder(BUFFER bf-oe-ord, OUTPUT oplError, OUTPUT opcMessage).

    END.

END PROCEDURE.

PROCEDURE pValidateOrder PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Runs the configured validation routines for a given order buffer, 
        creating hold tags as necessary.
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    IF NOT AVAILABLE ipbf-oe-ord THEN 
    DO:
        ASSIGN 
            oplError   = YES
            opcMessage = "Invalid Order - Buffer not found"
            .
    END.
    ELSE 
    DO:
        IF NOT VALID-HANDLE(hdOeValidate) THEN
            RUN system/oeValidate.p PERSISTENT SET hdOeValidate.
        RUN ValidateOrder IN hdOeValidate (ROWID(ipbf-oe-ord), OUTPUT oplError, OUTPUT opcMessage).
    
    END.

END PROCEDURE.

PROCEDURE pSetActualReleaseLocation PRIVATE:
    /*------------------------------------------------------------------------------
      Purpose: Assign correct location and bin to oe-rell    
      Parameters:  <none>
      Notes: Replaces procedure find-bin-loc in oe/cre-rell.p
      
      Logic For Determining OT1 (Actual Release) WHSE, BIN
    
    1)  If the N-K BOLWHSE character value = "Shipto" then the WHSE and BIN
        are pulled from the customer shipto  (AF1, ShipTo tab) specified 
        on the OU1 Release.
    
    2)  If BOLWHSE character value = "ShipFromWhse", a default WHSE location 
        that matches the ShipFrom set in the OU1 Release will be defined.  
        The BIN location is then determined by finding a bin that matches 
        on the following:
        a.  Job, Item, Exact Qty ,  ShipFrom Whse
        b.  Job, Item, Any Qty > 0, ShipFrom Whse
        c.  Job, Item, ShipFrom Whse (any Qty)
        d.  Item, Order, Any Qty > 0, ShipFrom Whse
        e.  Item, Order, ShipFrom Whse (any Qty)
        f.  Item, Any Qty > 0, ShipFrom Whse (any Job, Order)
        g.  Item, ShipFrom Whse
        If the logic above doesn't locate a bin, then the Bin is set to the 
        character value of N-K BOLPRINT.  If this is not a valid bin for the 
        ShipFrom Whse, it will be created.
    
    3)  If BOLWHSE is not "ShipTo" or "ShipFromWhse" then the default WHSE 
        is set based on the global default warehouse for the company.  
        This can be overridden if a bin exists with the following hierarchical 
        matching criteria:
        a.  Job, Item,  Exact Qty,  Default Whse
        b.  Job, Item, Exact Qty  (any Whse)
        c.  Job, Item, Any Qty > 0, Default Whse
        d.  Job, Item, Any Qty>0  (any Whse)
        e.  Job, Item, Default Whse (any Qty)
        f.  Job, Item (any Qty, any Whse)
        g.  Item, Order, Any Qty > 0, Default Whse
        h.  Item, Order, Default Whse (any Qty)
        i.  Item, Order, Any Qty > 0 (any Whse)
        j.  Item, Order
        k.  Item, Any Qty > 0,Default Whse (any Job, Order)
        l.  Item, Default Whse
        m.  Item, Any Qty > 0
        n.  Item
        If the logic above doesn't locate a bin, then the default Whse and Bin 
        from the FG item file is used.  If those values are blank, the Whse and 
        Bin for the first shipto of the Internal Customer (Customer X) is used.
          
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rell FOR oe-rell.
    DEFINE PARAMETER BUFFER ipbf-oe-rel   FOR oe-rel.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cSelectedValue    AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-fg-bin FOR fg-bin.
    DEFINE BUFFER bf-shipto FOR shipto.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    
    /* For premier, no tags are selected */
    cSelectedValue = "NoTag".
    
    FIND CURRENT ipbf-oe-rell EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-rel.company
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-rel.ord-no
          AND bf-oe-ordl.line    EQ ipbf-oe-rel.line
        NO-ERROR.
    IF NOT AVAIL bf-oe-ordl OR NOT AVAIL ipbf-oe-rel OR NOT AVAIL ipbf-oe-rell THEN 
    DO:
        ASSIGN oplError = true
               opcMessage = "Buffer not available"
               .
        RETURN. 
    END.
    
    IF ipbf-oe-rel.spare-char-1 GT "" 
       AND gcDefaultLocMethod EQ "ShipFromWhse" THEN
        ipbf-oe-rell.loc = ipbf-oe-rel.spare-char-1.
        
    /* gcDefaultLocMethod is an NK1 flag. ipbf-oe-rel.spare-char-1 is a ship-from */
    /* chosen by the user, so should try to find a bin for it       */
    IF gcDefaultLocMethod EQ "SHIPTO" THEN 
    DO:
        FIND FIRST bf-shipto NO-LOCK
            WHERE bf-shipto.company EQ ipbf-oe-rel.company
              AND bf-shipto.cust-no EQ ipbf-oe-rel.cust-no
              AND bf-shipto.ship-no EQ ipbf-oe-rel.ship-no
            USE-INDEX ship-no NO-ERROR.
        IF AVAILABLE bf-shipto THEN 
            ASSIGN
                ipbf-oe-rell.loc     = bf-shipto.loc
                ipbf-oe-rell.loc-bin = bf-shipto.loc-bin
                .
    END.  
    ELSE 
    DO:
        FIND FIRST bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
              AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
              AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
              AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
              AND bf-fg-bin.qty     GE ipbf-oe-rell.qty
              AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
            USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE bf-fg-bin AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.qty     GE ipbf-oe-rell.qty
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.job-no  EQ bf-oe-ordl.job-no
                  AND bf-fg-bin.job-no2 EQ bf-oe-ordl.job-no2
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.
   
        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND bf-fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE bf-fg-bin AND bf-oe-ordl.job-no EQ "" AND gcDefaultLocMethod NE "ShipFromWhse" THEN
            FIND FIRST bf-fg-bin NO-LOCK
                WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
                  AND bf-fg-bin.i-no    EQ ipbf-oe-rell.i-no
                USE-INDEX co-ino NO-ERROR.

        IF AVAILABLE bf-fg-bin THEN 
        DO:        
            IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN
                ASSIGN
                    ipbf-oe-rell.loc     = bf-fg-bin.loc
                    ipbf-oe-rell.loc-bin = bf-fg-bin.loc-bin
                    .

            IF gcTagSelectionCode NE "No Tags" AND cSelectedValue NE "NoTag" THEN
                ipbf-oe-rell.tag      = bf-fg-bin.tag.
        
            ASSIGN
                ipbf-oe-rell.job-no   = bf-fg-bin.job-no
                ipbf-oe-rell.job-no2  = bf-fg-bin.job-no2
                ipbf-oe-rell.qty-case = bf-fg-bin.case-count
                .       
        END.                           
        ELSE 
            IF glUseItemfgLoc THEN 
            DO:
                FIND FIRST bf-itemfg NO-LOCK 
                    WHERE bf-itemfg.company EQ ipbf-oe-rell.company
                      AND bf-itemfg.i-no    EQ ipbf-oe-rell.i-no
                    NO-ERROR.
                IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
                DO:          
                    ASSIGN
                        ipbf-oe-rell.loc     = bf-itemfg.def-loc
                        ipbf-oe-rell.loc-bin = bf-itemfg.def-loc-bin
                        .
                END.
            END.
    END. /* gcDefaultLocMethod NE "ShipTo" */
  
    IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
    DO:
        FIND FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ ipbf-oe-rell.company
              AND bf-itemfg.i-no    EQ ipbf-oe-rell.i-no
            NO-ERROR.
        IF AVAILABLE bf-itemfg THEN
            ASSIGN
                ipbf-oe-rell.loc     = bf-itemfg.def-loc
                ipbf-oe-rell.loc-bin = bf-itemfg.def-loc-bin
                .
        IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipbf-oe-rell.company
                  AND cust.active  EQ "X" 
                NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                FIND FIRST bf-shipto WHERE bf-shipto.company EQ ipbf-oe-rell.company
                    AND bf-shipto.cust-no EQ cust.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE bf-shipto THEN
                    ASSIGN   
                        ipbf-oe-rell.loc     = bf-shipto.loc
                        ipbf-oe-rell.loc-bin = bf-shipto.loc-bin.
            END.            
        END.
    END.

    /* gcCompanyDefaultBin is from an NK1 bolprint */
    IF (ipbf-oe-rell.loc-bin EQ "" 
        OR (ipbf-oe-rel.spare-char-1 NE "" AND ipbf-oe-rell.loc NE ipbf-oe-rel.spare-char-1))
        AND gcDefaultLocMethod EQ "ShipFromWhse" THEN 
    DO:
        IF ipbf-oe-rel.spare-char-1 NE "" AND ipbf-oe-rell.loc NE ipbf-oe-rel.spare-char-1 THEN
            ipbf-oe-rell.loc = ipbf-oe-rel.spare-char-1.
        ipbf-oe-rell.loc-bin = gcCompanyDefaultBin.
        FIND FIRST bf-fg-bin NO-LOCK
            WHERE bf-fg-bin.company EQ ipbf-oe-rell.company
              AND bf-fg-bin.loc     EQ ipbf-oe-rell.loc
              AND bf-fg-bin.loc-bin EQ ipbf-oe-rell.loc-bin
            NO-ERROR.
        IF NOT AVAILABLE bf-fg-bin THEN 
        DO:
            RUN pCreateFgBinForRelease (BUFFER ipbf-oe-rell).        
        END.

    END.

    FIND CURRENT ipbf-oe-rell NO-LOCK.
    ASSIGN oplError = FALSE
           opcMessage = ""
           .
END PROCEDURE.

/* Post Releases */
PROCEDURE OrderProcsPostReleases: 
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserName  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplSuccess   AS LOGICAL   NO-UNDO. 
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLID     AS INTEGER   NO-UNDO.
    
    DEFINE VARIABLE cRtnCharBol  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFoundBol AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnCharRel  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFoundRel AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lException   AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRelPost     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRelPost     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE iRelPost     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBOLDate     AS CHARACTER NO-UNDO.
    
    ASSIGN
        oplSuccess = YES
        opiBOLID   = 0
        .
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "RELPOST", 
        INPUT "L" /* Logical */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vENDOr */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharRel, 
        OUTPUT lRecFoundRel
        ).
    IF lRecFoundRel THEN
       lRelPost = LOGICAL(cRtnCharRel) NO-ERROR.
    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "RELPOST", 
        INPUT "C" /* Character */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust not vendor */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharRel, 
        OUTPUT lRecFoundRel
        ).
        
    IF lRecFoundRel THEN
        cRelPost = cRtnCharBol NO-ERROR. 
        
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "RELPOST", 
        INPUT "I" /* Integer */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vENDOr */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharRel, 
        OUTPUT lRecFoundRel
        ).
            
    IF lRecFoundRel THEN
        iRelPost = INT(cRtnCharRel) NO-ERROR. 
     
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany,
        INPUT "BOLDATE",
        INPUT "C" /* Character */,
        INPUT YES /* check by cust */,
        INPUT YES /* use cust NOT vENDOr */,
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnCharBol,
        OUTPUT lRecFoundBol
        ).
    IF lRecFoundBol THEN
       cBOLDate = sys-ctrl.char-fld NO-ERROR.
     
    FIND FIRST oe-relh NO-LOCK
         WHERE oe-relh.company  EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
           AND NOT oe-relh.posted  
           AND oe-relh.printed
         NO-ERROR.
    IF AVAILABLE oe-relh AND CAN-FIND(FIRST oe-rell
                                      WHERE oe-rell.company EQ oe-relh.company
                                        AND oe-rell.r-no    EQ oe-relh.r-no
                                      USE-INDEX r-no) THEN DO:
     
        /* Validates releases data */
        RUN pOrderProcsRelCheck (
            INPUT  ipcCompany,
            INPUT  ipiReleaseID, 
            INPUT  iRelPost,
            OUTPUT lException,
            OUTPUT opcMessage
            ).
        IF lException THEN DO:
            oplSuccess = NO.
            RETURN.
        END.
        IF opiBOLID NE 0 THEN
            FIND FIRST oe-bolh NO-LOCK
                 WHERE oe-bolh.company EQ oe-relh.company
                   AND oe-bolh.bol-no  EQ  opiBOLID
                 NO-ERROR.
            IF NOT AVAILABLE oe-bolh   OR 
                cRelPost EQ "BOL/REL"  THEN
                
                DO TRANSACTION:
                    /* Creates BOL */
                    RUN pOrderProcsCreateBOL (
                        INPUT ipcCompany,
                        INPUT ipiReleaseID, 
                        INPUT cRelPost,
                        INPUT iRelPost,
                        INPUT lRelPost,
                        INPUT cBOLDate,
                        INPUT ipcUserName,
                        INPUT-OUTPUT opiBOLID
                        ).
                END.
        FIND FIRST oe-rell NO-LOCK
             WHERE oe-rell.company EQ oe-relh.company
               AND oe-rell.r-no    EQ oe-relh.r-no
               AND oe-rell.s-code  EQ "I"
             USE-INDEX r-no NO-ERROR.
    
        cRelPost = IF AVAILABLE oe-rell THEN 
                       "Invoice" 
                   ELSE 
                       cRelPost.
        
        FIND FIRST oe-bolh NO-LOCK
             WHERE oe-bolh.company EQ ipcCompany
               AND oe-bolh.bol-no  EQ opiBOLID
             NO-ERROR.
        
        IF NOT AVAILABLE oe-bolh THEN DO:
            ASSIGN 
                opcMessage = "BOL is not created"
                oplSuccess = NO
                .

            RETURN.
        END.
        
        /* Posts release */
        RUN pOrderProcsPostRelease (
            INPUT ipcCompany,
            INPUT ipiReleaseID
            ).
        
        /* Creates BOL Lines */
        RUN pOrderProcsCreateBOLLines (
            INPUT ipcCompany,
            INPUT ipiReleaseID,
            INPUT opiBOLID
            ).
            
        FOR EACH oe-rell WHERE oe-rell.company EQ oe-relh.company
                           AND oe-rell.r-no    EQ oe-relh.r-no
                         USE-INDEX r-no
                         NO-LOCK,
            EACH oe-ordl WHERE oe-ordl.company EQ oe-relh.company
                           AND oe-ordl.ord-no EQ oe-rell.ord-no
                         EXCLUSIVE-LOCK:
            
            RUN oe/ordlsqty.p (
                INPUT ROWID(oe-ordl),
                OUTPUT oe-ordl.inv-qty,
                OUTPUT oe-ordl.ship-qty
                ).
            
            IF oe-rell.link-no EQ 0 THEN DO:
                FIND FIRST oe-rel
                     WHERE oe-rel.company  EQ oe-rell.company
                       AND oe-rel.ord-no   EQ oe-rell.ord-no
                       AND oe-rel.line     EQ oe-rell.line
                       AND oe-rel.i-no     EQ oe-rell.i-no
                       AND oe-rel.ship-id  EQ oe-relh.ship-id
                       AND oe-rel.link-no  EQ 0
                     NO-ERROR.
            
                IF NOT AVAILABLE oe-rel THEN
                    FIND FIRST oe-rel
                         WHERE oe-rel.company  EQ oe-rell.company
                           AND oe-rel.ord-no   EQ oe-rell.ord-no
                           AND oe-rel.line     EQ oe-rell.line
                           AND oe-rel.i-no     EQ oe-rell.i-no
                           AND oe-rel.link-no  EQ 0
                         NO-ERROR.
            END.            
            ELSE
                FIND FIRST oe-rel
                     WHERE oe-rel.r-no EQ oe-rell.link-no
                     USE-INDEX seq-no NO-ERROR.
            
            IF AVAILABLE oe-rel THEN DO:
                RUN oe/custxship.p (
                    INPUT oe-relh.company,
                    INPUT oe-relh.cust-no,
                    INPUT oe-relh.ship-id,
                    BUFFER shipto
                    ).
            
                IF AVAILABLE shipto AND AVAILABLE oe-rel THEN
                    ASSIGN
                        oe-rel.ship-addr[1] = shipto.ship-addr[1]
                        oe-rel.ship-addr[2] = shipto.ship-addr[2]
                        oe-rel.ship-city    = shipto.ship-city
                        oe-rel.ship-state   = shipto.ship-state
                        oe-rel.ship-zip     = shipto.ship-zip
                        .
            END.
        END.
        
    END.
    
END PROCEDURE.

PROCEDURE pOrderProcsRelCheck PRIVATE:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiRelPost   AS INTEGER   NO-UNDO.
    DEFINE OUTPUT PARAMETER oplException AS LOGICAL   NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage   AS CHARACTER NO-UNDO.
    
    DEFINE BUFFER buf-oe-relh FOR oe-relh.
    DEFINE BUFFER buf-oe-rell FOR oe-rell.
    
    FIND FIRST oe-relh NO-LOCK 
         WHERE oe-relh.company EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
         NO-ERROR.
                         
    IF AVAILABLE oe-relh THEN DO:
        FIND FIRST buf-oe-relh EXCLUSIVE-LOCK
             WHERE ROWID(buf-oe-relh) EQ ROWID(oe-relh) 
             NO-WAIT NO-ERROR.
        
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company EQ oe-relh.company
              AND oe-rell.r-no    EQ oe-relh.r-no
            USE-INDEX r-no:
            
            FIND buf-oe-rell EXCLUSIVE-LOCK
                 WHERE ROWID(buf-oe-rell) EQ ROWID(oe-rell) 
                 NO-WAIT NO-ERROR.

            IF NOT AVAILABLE buf-oe-relh OR NOT AVAILABLE buf-oe-rell THEN DO:
                CREATE tt-except.
                BUFFER-COPY oe-rell TO tt-except.
                tt-except.reason = 1.
                LEAVE.
            END.
            FIND FIRST buf-oe-rell NO-LOCK 
                 WHERE ROWID(buf-oe-rell) EQ ROWID(oe-rell) 
                 NO-ERROR.
        END.
        FIND FIRST buf-oe-relh NO-LOCK
             WHERE ROWID(buf-oe-relh) EQ ROWID(oe-relh)  
             NO-ERROR.
        IF ipiRelPost EQ 1 AND
            NOT CAN-FIND(FIRST tt-except
                         WHERE tt-except.company EQ oe-relh.company
                           AND tt-except.r-no    EQ oe-relh.r-no) THEN DO:
                
            FOR EACH tt-fg-bin:
                tt-fg-bin.qty = 0.
            END.
            
            FOR EACH oe-rell NO-LOCK
                WHERE oe-rell.company EQ oe-relh.company
                  AND oe-rell.r-no    EQ oe-relh.r-no
                USE-INDEX r-no,
            FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ oe-rell.company
              AND oe-ord.ord-no  EQ oe-rell.ord-no
              AND oe-ord.type    NE "T"
            BREAK BY oe-rell.i-no
                  BY oe-rell.job-no
                  BY oe-rell.job-no2
                  BY oe-rell.loc
                  BY oe-rell.loc-bin
                  BY oe-rell.tag:
                    
                FIND FIRST tt-fg-bin
                     WHERE tt-fg-bin.company EQ oe-rell.company
                       AND tt-fg-bin.i-no    EQ oe-rell.i-no
                       AND tt-fg-bin.job-no  EQ oe-rell.job-no
                       AND tt-fg-bin.job-no2 EQ oe-rell.job-no2
                       AND tt-fg-bin.loc     EQ oe-rell.loc
                       AND tt-fg-bin.loc-bin EQ oe-rell.loc-bin
                       AND tt-fg-bin.tag     EQ oe-rell.tag
                    NO-ERROR.
                IF NOT AVAILABLE tt-fg-bin THEN DO:
                    CREATE tt-fg-bin.
                    ASSIGN
                        tt-fg-bin.company = oe-rell.company
                        tt-fg-bin.i-no    = oe-rell.i-no
                        tt-fg-bin.job-no  = oe-rell.job-no
                        tt-fg-bin.job-no2 = oe-rell.job-no2
                        tt-fg-bin.loc     = oe-rell.loc
                        tt-fg-bin.loc-bin = oe-rell.loc-bin
                        tt-fg-bin.tag     = oe-rell.tag
                        .
                    FOR EACH oe-bolh NO-LOCK
                        WHERE oe-bolh.company EQ tt-fg-bin.company
                          AND oe-bolh.posted  EQ NO
                          AND oe-bolh.deleted EQ NO
                        USE-INDEX post,
                        EACH oe-boll NO-LOCK
                        WHERE oe-boll.b-no  EQ oe-bolh.b-no
                          AND oe-boll.i-no    EQ tt-fg-bin.i-no
                          AND oe-boll.job-no  EQ tt-fg-bin.job-no
                          AND oe-boll.job-no2 EQ tt-fg-bin.job-no2
                          AND oe-boll.loc     EQ tt-fg-bin.loc
                          AND oe-boll.loc-bin EQ tt-fg-bin.loc-bin
                          AND oe-boll.tag     EQ tt-fg-bin.tag
                        USE-INDEX b-no2:
                            tt-fg-bin.qty = tt-fg-bin.qty + oe-boll.qty.
                    END.
                END.
                tt-fg-bin.qty = tt-fg-bin.qty + oe-rell.qty.
                        
                IF LAST-OF(oe-rell.tag) THEN DO:
                    FIND FIRST fg-bin NO-LOCK 
                        WHERE fg-bin.company EQ oe-rell.company
                          AND fg-bin.i-no    EQ oe-rell.i-no
                          AND fg-bin.job-no  EQ oe-rell.job-no
                          AND fg-bin.job-no2 EQ oe-rell.job-no2
                          AND fg-bin.loc     EQ oe-rell.loc
                          AND fg-bin.loc-bin EQ oe-rell.loc-bin
                          AND fg-bin.tag     EQ oe-rell.tag
                        NO-ERROR.
                
                    IF (NOT AVAILABLE fg-bin OR fg-bin.qty LT tt-fg-bin.qty) AND
                        oe-rell.tag EQ "" THEN DO:
                        CREATE tt-except.
                        BUFFER-COPY oe-rell TO tt-except.
                        tt-except.reason = 2.
                    END.
                END.
            END.
        END.
        FIND FIRST tt-except NO-LOCK
             WHERE tt-except.company EQ oe-relh.company
               AND tt-except.r-no    EQ oe-relh.r-no
             NO-ERROR.
        IF AVAILABLE tt-except THEN DO:
            ASSIGN
                opcMessage = IF tt-except.reason EQ 1 THEN
                                 "Release is not available"
                             ELSE IF tt-except.reason EQ 2 THEN
                                 "Quantity is not available"
                             ELSE
                                 "Exception occured"  
                oplException = YES
                .
        END.
    END.

END PROCEDURE.

PROCEDURE pOrderProcsCreateBOL PRIVATE:
    DEFINE INPUT        PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcRelpost   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipiRelPost   AS INTEGER   NO-UNDO.
    DEFINE INPUT        PARAMETER iplRelPost   AS LOGICAL   NO-UNDO.
    DEFINE INPUT        PARAMETER ipcBolPost   AS CHARACTER NO-UNDO.
    DEFINE INPUT        PARAMETER ipcUserName  AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER iopiBOLNo    AS INTEGER   NO-UNDO.  

    DEFINE VARIABLE cFRTPay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFRTList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRellctr AS INTEGER   NO-UNDO.

    ASSIGN 
        iRellctr = 0 
        cFRTList = "" 
        cFOBList = ""
        .
              
    FIND FIRST oe-relh NO-LOCK
         WHERE oe-relh.company  EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
         NO-ERROR.
    
    IF AVAILABLE oe-relh THEN
        RUN pOrderProcsNewBOL (
            INPUT ipcCompany, 
            OUTPUT iopiBOLNo
            ).
    
    FOR EACH oe-rell NO-LOCK
        WHERE oe-rell.r-no EQ oe-relh.r-no
        USE-INDEX r-no:
        IF oe-rell.lot-no <> "" THEN
            ASSIGN 
                cFRTList = (IF LOOKUP(oe-rell.frt-pay,cFRTList) = 0 THEN 
                                cFRTList + "," + oe-rell.frt-pay 
                            ELSE 
                                cFRTList)
                cFOBList = (IF LOOKUP(oe-rell.fob-code,cFOBList) = 0 THEN 
                                cFOBList + "," + oe-rell.fob-code 
                            ELSE 
                                cFOBList)
                .                
    END.    
     
    IF length(cFRTList) > 0 THEN
        cFRTList = SUBSTR(cFRTList,2).
    IF length(cFOBList) > 0 THEN
        cFOBList = SUBSTR(cFOBList,2).
    
    IF NUM-ENTRIES(cFRTList) > 1 THEN
       iRellctr = NUM-ENTRIES(cFRTList).
    ELSE
       IF NUM-ENTRIES(cFOBList) > 1 THEN
           iRellctr = NUM-ENTRIES(cFOBList).
    
    /* get first order fOR release */
    FIND FIRST oe-rell NO-LOCK
         WHERE oe-rell.r-no EQ oe-relh.r-no
         USE-INDEX r-no NO-ERROR.
    
    FIND FIRST oe-ord NO-LOCK 
         WHERE oe-ord.company EQ oe-relh.company
           AND oe-ord.ord-no  EQ oe-rell.ord-no
         NO-ERROR.
    
    ASSIGN 
        cFRTPay = (IF cFRTList <> "" THEN 
                       SUBSTR(cFRTList,1,1) 
                   ELSE 
                       "")
        cFOBCode = (IF cFOBList <> "" THEN 
                       SUBSTR(cFOBList,1,1) 
                    ELSE 
                        "")
        .
        
    IF oe-rell.lot-no <> "" THEN DO:   
        ASSIGN 
            cFRTPay = (IF cFRTPay = "" THEN 
                           oe-rell.frt-pay 
                       ELSE 
                           cFRTPay)
            cFOBCode = (IF cFOBCode = "" THEN 
                           oe-rell.fob-code 
                        ELSE 
                           cFOBCode)
            .
        
        IF cFRTPay = "P" THEN 
            cFRTPay = "Prepaid". 
        ELSE IF cFRTPay = "C" THEN 
            cFRTPay = "Collect".
        ELSE IF cFRTPay = "B" THEN 
            cFRTPay = "Bill". 
        ELSE IF cFRTPay = "T" THEN 
            cFRTPay = "ThirdParty".
    
        IF cFOBCode = "O" THEN 
            cFOBCode = "Origin". 
        ELSE IF cFOBCode = "D" THEN 
            cFOBCode = "Destination".
    
        IF iRellctr > 1 THEN DO:
            IF NOT CAN-DO("P,C,B,T",substr(cFRTPay,1,1)) THEN
                cFRTPay =  oe-rell.frt-pay.

            IF NOT CAN-DO("O,D",substr(cFOBCode,1,1)) THEN 
                cFOBCode = oe-rell.fob-code.
        END.
        IF length(cFRTPay) > 1 THEN
           cFRTPay =  SUBSTR(cFRTPay,1,1).
        IF length(cFOBCode) > 1 THEN
           cFOBCode =  SUBSTR(cFOBCode,1,1).
    END.
    RELEASE reftable.
    
    FOR EACH oe-bolh EXCLUSIVE-LOCK
        WHERE oe-bolh.company  EQ oe-relh.company
          AND oe-bolh.release# EQ oe-relh.release#
          AND NOT CAN-FIND(FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company 
                                           AND oe-boll.b-no    EQ oe-bolh.b-no):      
        DELETE oe-bolh.    
    END.
  
    x = 1.
    FIND LAST oe-bolh NO-LOCK
         USE-INDEX b-no NO-ERROR.
    IF AVAILABLE oe-bolh THEN 
        x = oe-bolh.b-no + 1.
        
    FIND FIRST shipto NO-LOCK
     WHERE shipto.company EQ oe-relh.company
       AND shipto.cust-no EQ oe-relh.cust-no
       AND shipto.ship-id EQ oe-relh.ship-id
     NO-ERROR.
         
    IF NOT AVAILABLE shipto THEN 
        FIND FIRST loc NO-LOCK
             WHERE loc.company EQ ipcCompany
             NO-ERROR.
             
    CREATE oe-bolh.
    ASSIGN
        oe-bolh.company  = oe-relh.company
        oe-bolh.loc      = IF AVAILABLE shipto THEN
                               shipto.loc
                           ELSE IF AVAILABLE loc THEN
                               loc.loc
                           ELSE
                               ""
        oe-bolh.b-no     = x
        oe-bolh.bol-no   = iopiBOLNo
        oe-bolh.release# = oe-relh.release#
        oe-bolh.bol-date = IF ipcBolPost EQ "Current" THEN 
                               TODAY 
                           ELSE 
                               oe-relh.rel-date
        oe-bolh.cust-no  = oe-relh.cust-no
        oe-bolh.ship-no  = oe-relh.ship-no
        oe-bolh.ship-id  = oe-relh.ship-id
        oe-bolh.carrier  = oe-relh.carrier
        oe-bolh.stat     = STRING(iplRelPost AND ipcRelPost  BEGINS "BOL","H/R")
        oe-bolh.b-ord-no = oe-relh.b-ord-no
        oe-bolh.rel-date = oe-relh.rel-date
        oe-bolh.r-no     = oe-relh.r-no
        oe-bolh.posted   = no
        oe-bolh.printed  = no
        oe-bolh.deleted  = no
        oe-bolh.frt-pay  = IF cFRTPay <> "" THEN 
                               cFRTPay 
                           ELSE IF AVAILABLE oe-ord THEN 
                               oe-ord.frt-pay 
                           ELSE 
                               ""
        oe-bolh.trailer  = oe-relh.trailer
        oe-bolh.upd-date = TODAY
        oe-bolh.upd-time = TIME
        oe-bolh.user-id  = ipcUserName
        .
  
    IF cFOBCode <> "" THEN DO:
        FIND FIRST reftable NO-LOCK 
             WHERE reftable.reftable EQ "oe-bolh.lot-no" 
               AND reftable.rec_key  EQ oe-bolh.rec_key
             USE-INDEX rec_key
             NO-ERROR.
        IF NOT AVAILABLE reftable THEN DO:
            CREATE reftable.
            ASSIGN 
                reftable.reftable = "oe-bolh.lot-no" 
                reftable.rec_key  = oe-bolh.rec_key
                reftable.code     = cFOBCode
                .
        END.         
        RELEASE reftable.
    END.
  
    RUN oe/custxship.p (
        INPUT oe-bolh.company,
        INPUT oe-bolh.cust-no,
        INPUT oe-bolh.ship-id,
        BUFFER shipto
        ).
  
    IF NOT AVAILABLE shipto THEN
        FIND FIRST shipto
            WHERE shipto.company EQ ipcCompany
              AND shipto.cust-no EQ oe-bolh.cust-no
            NO-LOCK NO-ERROR.
  
    IF AVAILABLE shipto THEN DO:
        ASSIGN
            oe-bolh.ship-no      = shipto.ship-no
            oe-bolh.ship-id      = shipto.ship-id
            oe-bolh.ship-i[1]    = shipto.notes[1]
            oe-bolh.ship-i[2]    = shipto.notes[2]
            oe-bolh.ship-i[3]    = shipto.notes[3]
            oe-bolh.ship-i[4]    = shipto.notes[4].            
            oe-bolh.cust-no = IF shipto.bill THEN 
                                  shipto.ship-id
                              ELSE
                                  oe-bolh.cust-no
            .
    END.
    
    IF oe-relh.ship-i[1] NE "" OR oe-relh.ship-i[2] NE "" OR
       oe-relh.ship-i[3] NE "" OR oe-relh.ship-i[4] NE "" THEN DO:
        ASSIGN
            oe-bolh.ship-i[1] = oe-relh.ship-i[1]
            oe-bolh.ship-i[2] = oe-relh.ship-i[2]
            oe-bolh.ship-i[3] = oe-relh.ship-i[3]
            oe-bolh.ship-i[4] = oe-relh.ship-i[4]
            .
        RUN pCopyShipNote (
            INPUT oe-relh.rec_key, 
            INPUT oe-bolh.rec_key
            ).
    END.
END PROCEDURE.

PROCEDURE pOrderProcsNewBOL PRIVATE:
    DEFINE INPUT  PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiBOLNo   AS INTEGER   NO-UNDO.
   
    DEFINE VARIABLE cFRTPay  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBCode AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFRTList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFOBList AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iRellctr AS INTEGER   NO-UNDO.
    
    FIND FIRST oe-ctrl EXCLUSIVE-LOCK
         WHERE oe-ctrl.company EQ ipcCompany 
         NO-ERROR.
    
    DO WHILE TRUE:
        ASSIGN
            opiBOLNo      = oe-ctrl.n-bol
            oe-ctrl.n-bol = opiBOLNo + 1
            .
       
        IF oe-ctrl.n-bol GT 99999999 THEN 
            oe-ctrl.n-bol = 1.
        
        FIND FIRST oe-bolh NO-LOCK
             WHERE oe-bolh.company EQ ipcCompany
               AND oe-bolh.bol-no  EQ opiBOLNo
             USE-INDEX bol-no NO-ERROR.
        IF NOT AVAILABLE oe-bolh THEN
            LEAVE.
    END.

END PROCEDURE.

PROCEDURE pOrderProcsCreateBOLLines PRIVATE:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT PARAMETER ipiBOLID     AS INTEGER   NO-UNDO.

    DO TRANSACTION:
        FIND FIRST oe-relh NO-LOCK
             WHERE oe-relh.company  EQ ipcCompany
               AND oe-relh.release# EQ ipiReleaseID
             NO-ERROR.
             
        /* get first Order for release */
        FIND FIRST oe-rell NO-LOCK
             WHERE oe-rell.r-no EQ oe-relh.r-no 
             USE-INDEX r-no 
             NO-ERROR.
        
        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ oe-relh.company
               AND oe-ord.ord-no  EQ oe-rell.ord-no
             NO-ERROR.
        
        /* Creates records in oe-boll table */
        RUN pOrderProcsMakeBOLLs (
            INPUT ipcCompany,
            INPUT ipiReleaseID,
            INPUT ipiBOLID
            ).

        FIND FIRST oe-bolh 
             WHERE oe-bolh.company EQ ipcCompany
               AND oe-bolh.bol-no  EQ ipiBOLID
             NO-ERROR.

        FOR EACH  oe-boll EXCLUSIVE-LOCK 
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no:
              
            RUN oe/calcBolWeight.p (
                INPUT ROWID(oe-boll), 
                OUTPUT oe-boll.weight
                ).
           
        END.
        
        RUN oe/palcal2.p (
            INPUT ROWID(oe-bolh), 
            OUTPUT oe-bolh.tot-pallets
            ).
    
        RUN oe/bolhtots.p (
            INPUT ROWID(oe-bolh)
            ).
      
        RUN oe/calcBolFrt.p (
            INPUT ROWID(oe-bolh), 
            OUTPUT oe-bolh.freight
            ).
      
        IF oe-bolh.freight EQ ? THEN 
            oe-bolh.freight = 0.
      
        IF oe-rell.link-no NE 0 THEN
            FIND FIRST oe-rel 
                 WHERE oe-rel.r-no EQ oe-rell.link-no
                 USE-INDEX seq-no NO-ERROR.
      
        IF NOT AVAILABLE oe-rel THEN
            FIND FIRST oe-rel
                 WHERE oe-rel.company  EQ oe-relh.company
                   AND oe-rel.ord-no   EQ oe-rell.ord-no
                   AND oe-rel.liNE     EQ oe-rell.liNE
                   AND oe-rel.i-no     EQ oe-rell.i-no
                   AND oe-rel.ship-id  EQ oe-relh.ship-id
                   AND oe-rel.rel-date LE oe-relh.rel-date
                   AND oe-rel.rel-no   EQ 0
                   AND oe-rel.b-ord-no EQ 0
                   AND oe-rel.link-no  EQ 0
                 USE-INDEX ord-item NO-ERROR.
      
        IF NOT AVAILABLE oe-rel THEN
            FIND FIRST oe-rel
                 WHERE oe-rel.company  EQ oe-relh.company
                   AND oe-rel.ord-no   EQ oe-rell.ord-no
                   AND oe-rel.liNE     EQ oe-rell.liNE
                   AND oe-rel.i-no     EQ oe-rell.i-no
                   AND oe-rel.rel-date LE oe-relh.rel-date
                   AND oe-rel.rel-no   EQ 0
                   AND oe-rel.b-ord-no EQ 0
                   AND oe-rel.link-no  EQ 0
                 USE-INDEX ord-item NO-ERROR.
      
        IF AVAILABLE oe-rel THEN
            RUN oe/rel-stat.p (
                INPUT ROWID(oe-rel), 
                OUTPUT oe-rel.stat
                ).
    END.
END PROCEDURE.

PROCEDURE pOrderProcsMakeBOLLs PRIVATE:
    DEFINE INPUT  PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiReleaseID AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBOLID     AS INTEGER   NO-UNDO.
        
    DEFINE VARIABLE iBOLLine        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBOLQty         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cBOLPrint       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lBOLWeight      AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnBOLPrint    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFoundBOLPrint  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cRtnBOLWeight   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFoundBOLWeight AS LOGICAL   NO-UNDO.

    
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "BOLPRINT", 
        INPUT "C" /* Logical */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vendor */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnBOLPrint, 
        OUTPUT lFoundBOLPrint
        ).
    IF lFoundBOLPrint THEN
        cBOLPrint = cRtnBOLPrint NO-ERROR. 
                
    RUN sys/ref/nk1look.p (
        INPUT ipcCompany, 
        INPUT "BOLWeight", 
        INPUT "L" /* Logical */, 
        INPUT YES /* check by cust */, 
        INPUT YES /* use cust NOT vendor */, 
        INPUT "" /* cust */,
        INPUT "" /* ship-to*/,
        OUTPUT cRtnBOLWeight, 
        OUTPUT lFoundBOLWeight
        ).
        
    IF lFoundBOLWeight THEN
        lBOLWeight = LOGICAL(cRtnBOLWeight) NO-ERROR.
    
    FIND FIRST oe-relh NO-LOCK
         WHERE oe-relh.company EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID 
         NO-ERROR.
         
    FIND FIRST oe-bolh NO-LOCK
         WHERE oe-bolh.company EQ ipcCompany
           AND oe-bolh.bol-no  EQ ipiBOLID
         NO-ERROR.
         
    FOR EACH oe-rell
        WHERE oe-rell.company EQ oe-relh.company
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no NO-LOCK
        BREAK 
        BY oe-rell.ord-no
        BY oe-rell.i-no
        BY SUBSTR(oe-rell.rec_key,5,4)
        BY SUBSTR(oe-rell.rec_key,1,4)
        BY SUBSTR(oe-rell.rec_key,10,100):
    
        FIND FIRST oe-boll NO-LOCK
            WHERE oe-boll.company EQ oe-bolh.company
              AND oe-boll.b-no    EQ oe-bolh.b-no
              AND oe-boll.i-no    EQ oe-rell.i-no
              AND oe-boll.ord-no  EQ oe-rell.ord-no
            USE-INDEX b-no NO-ERROR.
        
        IF AVAILABLE oe-boll THEN 
            iBOLLine = oe-boll.bol-line.        
        ELSE DO:
            FIND LAST oe-boll NO-LOCK
                WHERE oe-boll.company EQ oe-bolh.company
                  AND oe-boll.b-no    EQ oe-bolh.b-no
                USE-INDEX bol-line NO-ERROR.
            iBOLLine  = (IF AVAILABLE oe-boll THEN 
                             oe-boll.bol-line 
                         ELSE 0) + 1.
        END.

        CREATE oe-boll.
        ASSIGN
            oe-boll.company  = oe-bolh.company
            oe-boll.b-no     = oe-bolh.b-no
            oe-boll.bol-no   = oe-bolh.bol-no
            oe-boll.bol-line = iBOLLine
            oe-boll.ord-no   = oe-rell.ord-no
            oe-boll.rel-no   = oe-rell.rel-no
            oe-boll.b-ord-no = oe-rell.b-ord-no
            oe-boll.po-no    = oe-rell.po-no
            oe-boll.loc-bin  = oe-rell.loc-bin
            oe-boll.loc      = oe-rell.loc
            oe-boll.r-no     = oe-rell.r-no
            oe-boll.i-no     = oe-rell.i-no
            oe-boll.line     = oe-rell.line
            oe-boll.tag      = oe-rell.tag
            oe-boll.job-no   = oe-rell.job-no
            oe-boll.job-no2  = oe-rell.job-no2
            oe-boll.cust-no  = oe-rell.cust-no
            oe-boll.cases    = oe-rell.cases
            oe-boll.qty-case = oe-rell.qty-case
            oe-boll.partial  = oe-rell.partial
            oe-boll.s-code   = oe-rell.s-code
            oe-boll.qty      = oe-rell.qty
            oe-boll.lot-no   = oe-rell.lot-no
            oe-boll.sell-price = oe-rell.sell-price
            oe-boll.zeroPrice = oe-rell.zeroPrice
            oe-boll.enteredBy = oe-rell.enteredBy
            oe-boll.enteredDT = oe-rell.enteredDT
            .
               
        IF oe-boll.loc-bin EQ "" THEN 
            oe-boll.loc-bin = cBOLPrint.
       
        iBOLQty  = iBOLQty  + oe-boll.qty.
        
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ cocode
              AND itemfg.i-no    EQ oe-rell.i-no
            NO-ERROR.
        
        IF AVAILABLE itemfg  AND
           oe-boll.qty-case  EQ 0 AND
           itemfg.case-count NE 0 THEN DO:
           
            oe-boll.qty-case = itemfg.case-count.
        
            IF oe-boll.qty-case NE 0 THEN
                oe-boll.cases = TRUNC((oe-boll.qty - oe-boll.partial) / oe-boll.qty-case,0).
        END.

        IF LAST-OF(oe-rell.i-no) THEN DO: 
            {oe/oe-bolpc.i ALL}
        END.
        
        IF AVAILABLE itemfg THEN
            ASSIGN
                oe-boll.weight = ((((oe-boll.cases * oe-boll.qty-case) +
                                 oe-boll.partial) / 100) * itemfg.weight-100)
                oe-bolh.tot-wt = oe-bolh.tot-wt + oe-boll.weight
                .
 
        IF lBOLWeight AND
           TRIM(oe-rell.tag) NE "" THEN 
            FIND FIRST loadtag NO-LOCK 
                 WHERE loadtag.company   EQ ipcCompany
                   AND loadtag.item-type EQ NO
                   AND loadtag.tag-no    EQ oe-rell.tag 
                 NO-ERROR.
                 
        IF AVAILABLE loadtag THEN
            ASSIGN oe-boll.weight = ((oe-rell.cases * loadtag.misc-dec[1]) +
                                     loadtag.misc-dec[3]).
        IF oe-boll.qty LT 0 THEN 
            oe-boll.tag = "".
    END. /* each oe-rell */

    RUN oe/calcBolFrt.p (
        INPUT ROWID(oe-bolh), 
        OUTPUT oe-bolh.freight
        ).
               
END PROCEDURE.


PROCEDURE pOrderProcsPostRelease PRIVATE:
    DEFINE INPUT PARAMETER ipcCompany   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiReleaseID AS INTEGER NO-UNDO.
    
    FIND FIRST oe-relh EXCLUSIVE-LOCK
         WHERE oe-relh.company EQ ipcCompany
           AND oe-relh.release# EQ ipiReleaseID
         NO-WAIT NO-ERROR. 

    FOR EACH  oe-rell
        WHERE oe-rell.company EQ ipcCompany
          AND oe-rell.r-no    EQ oe-relh.r-no
        USE-INDEX r-no:
        FIND FIRST itemfg
              WHERE itemfg.company EQ ipcCompany
                AND itemfg.i-no    EQ oe-rell.i-no
              USE-INDEX i-no NO-ERROR.
        
        IF NOT AVAILABLE itemfg THEN
            RETURN.

        FIND FIRST oe-ord NO-LOCK
             WHERE oe-ord.company EQ ipcCompany
               AND oe-ord.ord-no  EQ oe-rell.ord-no  /* used be oe-relh.ord-no */
             NO-ERROR.

        IF NOT AVAILABLE oe-ord OR
           (AVAILABLE oe-ord AND oe-ord.inv-no NE 0 AND oe-ord.stat EQ "X") THEN
            RETURN.
        ELSE DO:
            ASSIGN
                oe-rell.printed = yes
                oe-rell.posted  = yes
                .
            
            IF (oe-rell.s-code EQ "S" OR oe-rell.s-code EQ "B") AND
                NOT oe-relh.deleted THEN DO:
                itemfg.q-rel = itemfg.q-rel + oe-rell.qty.
                RUN fg/chkfgloc.p (INPUT oe-rell.i-no, INPUT oe-rell.loc).
                FIND FIRST itemfg-loc EXCLUSIVE-LOCK 
                     WHERE itemfg-loc.company EQ ipcCompany
                       AND itemfg-loc.i-no    EQ oe-rell.i-no
                       AND itemfg-loc.loc     EQ oe-rell.loc
                     NO-ERROR.
                IF AVAILABLE itemfg-loc THEN
                    itemfg-loc.q-rel = itemfg-loc.q-rel + oe-rell.qty.            
            END.            
            
            RELEASE itemfg.
            FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
            
            IF AVAILABLE oe-ord THEN
                FIND FIRST oe-ordl
                     WHERE oe-ordl.company EQ ipcCompany
                       AND oe-ordl.ord-no  EQ oe-ord.ord-no
                       AND oe-ordl.i-no    EQ oe-rell.i-no
                       AND oe-ordl.line    EQ oe-rell.line
                     USE-INDEX ord-no NO-ERROR.
            ELSE 
                RETURN.
            
            IF AVAILABLE oe-ordl AND 
                     NOT oe-relh.deleted AND 
                         oe-rell.s-code NE "I" THEN
                oe-ordl.t-rel-qty = oe-ordl.t-rel-qty + oe-rell.qty.
            
            RELEASE oe-ordl.
            
            RUN oe/upschrel.p (
                INPUT ROWID(oe-rell)
                ).
        END. /* ELSE DO: */
    END. /* each oe-rell */

    IF AVAILABLE oe-relh THEN
        oe-relh.posted = YES.

    RELEASE oe-relh.
END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetSettingJobCreate RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the setting for NK1 Jobcreat
     Notes:
    ------------------------------------------------------------------------------*/    
    DEFINE VARIABLE cReturn AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1look.p (ipcCompany, "JobCreat", "L", NO, NO, "", "", 
        OUTPUT cReturn, OUTPUT lFound).
        
    RETURN lFound AND cReturn EQ "YES".
        
END FUNCTION.


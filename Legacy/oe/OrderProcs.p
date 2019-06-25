
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

PROCEDURE CreateActRelLine:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-rel FOR oe-rel.
    DEFINE PARAMETER BUFFER ipbf-oe-relh FOR oe-relh.
    DEFINE INPUT PARAMETER iRelNo AS INTEGER NO-UNDO.    
    DEFINE OUTPUT PARAMETER oprOeRellRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lcBolWhse         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE RelSkipRecalc-log AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnChar       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound         AS CHARACTER NO-UNDO.    
    DEFINE VARIABLE oereordr-log      LIKE sys-ctrl.log-fld NO-UNDO.
    DEFINE VARIABLE oereordr-cha      LIKE sys-ctrl.char-fld NO-UNDO.  
    DEFINE VARIABLE lError            AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cErrMsg           AS CHARACTER NO-UNDO.
     

    IF NOT AVAIL(ipbf-oe-relh) THEN DO:
        ASSIGN oplError = TRUE 
               opcMessage = "Release header record not found"
               .

    END.      
        
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-relh.company, "OEREORDR", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    oereordr-log = LOGICAL(cReturnChar) NO-ERROR.
    
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-relh.company, "OEREORDR", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    oereordr-cha = cReturnChar NO-ERROR.    
    
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-relh.company, "BOLWHSE", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    lcBolWhse = cReturnChar NO-ERROR.
         
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-relh.company, "RelSkipRecalc", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    RelSkipRecalc-log = LOGICAL(cReturnChar) NO-ERROR.          
    
    IF NOT AVAIL(ipbf-oe-rel) THEN DO:
        ASSIGN oplError = TRUE 
               opcMessage = "Scheduled release record not found"
               .
    END. 
    IF oplError THEN
        RETURN.
    FIND FIRST oe-ordl no-lock
        WHERE oe-ordl.company  EQ ipbf-oe-rel.company
          AND oe-ordl.ord-no   EQ ipbf-oe-rel.ord-no
          AND oe-ordl.line EQ ipbf-oe-rel.line
        NO-ERROR.
        
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ ipbf-oe-rel.company
          AND itemfg.i-no    EQ ipbf-oe-rel.i-no
        NO-ERROR.        
    // RUN get-next-r-no.
    
    CREATE oe-rell.
    ASSIGN
        oprOeRellRow       = ROWID(oe-rell)
        oe-rell.company    = ipbf-oe-rel.company
        oe-rell.r-no       = ipbf-oe-relh.r-no
        oe-rell.rel-no     = iRelNo
        oe-rell.loc        = IF lcBolWhse NE "ShipFromWhse" THEN locode ELSE ipbf-oe-rel.spare-char-1
        oe-rell.ord-no     = ipbf-oe-rel.ord-no
        oe-rell.qty        = ipbf-oe-rel.tot-qty
        oe-rell.i-no       = ipbf-oe-rel.i-no
        oe-rell.job-no     = oe-ordl.job-no
        oe-rell.job-no2    = oe-ordl.job-no2
        oe-rell.po-no      = ipbf-oe-rel.po-no
        oe-rell.line       = ipbf-oe-rel.line
        oe-rell.lot-no     = ipbf-oe-rel.lot-no
        oe-rell.frt-pay    = ipbf-oe-rel.frt-pay
        oe-rell.fob-code   = ipbf-oe-rel.fob-code
        oe-rell.sell-price = ipbf-oe-rel.sell-price
        oe-rell.zeroPrice  = ipbf-oe-rel.zeroPrice
        oe-rell.printed    = NO
        oe-rell.posted     = NO
        oe-rell.deleted    = NO
        /** Set link to the planned releases **/
        oe-rell.link-no    = ipbf-oe-rel.r-no
        oe-rell.s-code     = IF ipbf-oe-rel.s-code <> "" THEN ipbf-oe-rel.s-code ELSE
                                 IF oe-ordl.is-a-component THEN "S" ELSE
                                    IF AVAILABLE oe-ctrl AND oe-ctrl.ship-from THEN "B" ELSE "I"     
        oe-rell.partial = IF oe-rell.s-code EQ "I" THEN oe-ordl.partial ELSE 0
        oe-rell.qty-case = IF AVAILABLE itemfg AND itemfg.case-count GT 0
            THEN itemfg.case-count
            ELSE
            IF oe-ordl.cas-cnt GT 0 THEN oe-ordl.cas-cnt
            ELSE 1
        oe-rell.cases   = TRUNC((oe-rell.qty - oe-rell.partial) /
                            oe-rell.qty-case,0)
        oe-rell.partial = oe-rell.qty - (oe-rell.cases * oe-rell.qty-case)
        .                     

    RUN oe/rel-stat-upd.p (ROWID(oe-rell)).

    /* Fill in correct bin/loc */
    IF AVAILABLE oe-rell  THEN
        RUN SetActualReleaseLocation (BUFFER oe-rell, BUFFER ipbf-oe-rel, OUTPUT lError, OUTPUT cErrMsg).

    /* Set values for invoice only */
    IF oe-rell.s-code = "I" THEN
       oe-rell.loc-bin = "".
       
     FIND CURRENT ipbf-oe-rel EXCLUSIVE-LOCK.
     ASSIGN 
        ipbf-oe-rel.rel-no   = iRelNo
        ipbf-oe-rel.b-ord-no = ipbf-oe-relh.b-ord-no
        ipbf-oe-rel.qty      = ipbf-oe-rel.tot-qty
        .       
    RUN oe/rel-stat.p (ROWID(ipbf-oe-rel), OUTPUT ipbf-oe-rel.stat).
    
    FIND CURRENT ipbf-oe-rel NO-LOCK.

     //   Shared Vars:
     //   RUN fg/calcqa&b.p (ROWID(itemfg), OUTPUT itemfg.q-alloc,
     //       OUTPUT itemfg.q-back).
     //   itemfg.q-avail = itemfg.q-onh +
     //       (IF oereordr-cha EQ "XOnOrder" THEN 0 ELSE itemfg.q-ono) -
     //       itemfg.q-alloc.

    
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

PROCEDURE CreateActRelHeader:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* ---------------------------------------------------- oe/cre-relh 01/98 JLF */
    /* Order Entry - Create actual releases from planned release line (header)    */
    /* -------------------------------------------------------------------------- */

    DEFINE PARAMETER BUFFER ipbf-oe-rel FOR oe-rel.
    DEFINE OUTPUT PARAMETER oprOeRelhRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.

    DEFINE VARIABLE iNextRNo        AS INTEGER   INIT 1 NO-UNDO.
    DEFINE VARIABLE iNextReleaseNum AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cOrigProgram    AS CHARACTER FORMAT "x(50)" NO-UNDO.
    DEFINE VARIABLE lCreditHold     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cCustCode       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE addxfer-log     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE relCredt-log    AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cReturnChar     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound       AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-cust FOR cust.

    IF NOT AVAIL(ipbf-oe-rel) THEN DO:
        ASSIGN oplError = TRUE 
               opcMessage = "Release record not found"
               .
    END.
    
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-rel.company, "ADDXFER", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    addxfer-log = LOGICAL(cReturnChar) NO-ERROR.
    
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-rel.company, "RELCREDT", "L" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    relCredt-log = LOGICAL(cReturnChar) NO-ERROR.
    
    ASSIGN 
        cOrigProgram = TRIM(PROGRAM-NAME(2))
        lCreditHold  = NO
        .

    IF relCredt-log THEN DO: 
        FIND FIRST cust NO-LOCK
            WHERE cust.company EQ ipbf-oe-rel.company
              AND cust.cust-no EQ ipbf-oe-rel.cust-no 
            NO-ERROR.
        IF AVAILABLE cust AND cOrigProgram NE "fg/invrecpt.p" THEN
                RUN oe/CRcheck.p ( INPUT ROWID(cust),
                                   INPUT YES,
                                   OUTPUT lCreditHold ).
    END.
    RUN oe/getNextRelNo.p (INPUT "oe-relh", 
                           OUTPUT iNextRNo).

    RUN oe/release#.p (ipbf-oe-rel.company, 
                       OUTPUT iNextReleaseNum).

    IF addxfer-log = YES  AND ipbf-oe-rel.s-code EQ 'T' THEN 
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
        oe-relh.w-ord     = lCreditHold
        .
       
    RUN pCopyShipNote (ipbf-oe-rel.rec_key, oe-relh.rec_key).

    IF lCreditHold THEN
    DO:
        FIND FIRST bf-cust EXCLUSIVE-LOCK
            WHERE bf-cust.company EQ ipbf-oe-rel.company
            AND bf-cust.cust-no EQ ipbf-oe-rel.cust-no USE-INDEX cust 
            NO-WAIT NO-ERROR.
        IF AVAILABLE bf-cust THEN
            ASSIGN bf-cust.cr-hold = YES
                .
    END.
    
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
            RUN pReleaseOrder(BUFFER ipbf-oe-ord, OUTPUT oplError, OUTPUT opcMessage).          
        END.
    END.
        ELSE ipbf-oe-ord.stat = "W".  /*Refactor - if we add ability to build job, this can be removed*/
        
    FIND CURRENT ipbf-oe-ord NO-LOCK.
    
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


PROCEDURE pCreateFgBinForRelease:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprOeRell AS ROWID       NO-UNDO.
    
    DEFINE BUFFER bfOeRell FOR oe-rell.
    DEFINE VARIABLE xLoc LIKE fg-bin.loc NO-UNDO.
    DEFINE VARIABLE xBin LIKE fg-bin.loc-bin NO-UNDO.
    FIND bfOeRell NO-LOCK WHERE ROWID(bfOeRell) EQ iprOeRell NO-ERROR.

    ASSIGN 
        xBin = bfOeRell.loc-bin
        xLoc = bfOeRell.loc
        .

    CREATE fg-bin.
    ASSIGN
        fg-bin.company = bfOeRell.company
        fg-bin.i-no    = bfOeRell.i-no
        fg-bin.job-no  = bfOeRell.job-no
        fg-bin.job-no2 = bfOeRell.job-no2
        fg-bin.loc     = xLoc
        fg-bin.loc-bin = xBin
        fg-bin.tag     = bfOeRell.tag
        fg-bin.cust-no = bfOeRell.cust-no
        . 
    RELEASE fg-bin.

    /*Create a bin so that is shows up in IF4 -FG Bin (blank i-no)*/
    FIND FIRST fg-bin NO-LOCK
        WHERE fg-bin.company EQ bfOeRell.company 
          AND fg-bin.loc EQ xLoc
          AND fg-bin.loc-bin EQ xBin
          AND fg-bin.i-no = ""
        NO-ERROR.
    IF NOT AVAILABLE fg-bin THEN 
    DO:
        CREATE fg-bin.
        ASSIGN 
            fg-bin.company = bfOeRell.company
            fg-bin.i-no    = ""
            fg-bin.loc     = xLoc
            fg-bin.loc-bin = xBin
            .
    END.
    FIND CURRENT fg-bin NO-LOCK NO-ERROR.
    
END PROCEDURE.

PROCEDURE ReleaseOrder :
    /*------------------------------------------------------------------------------
     Purpose: Given a buffer oe-ord, release all lines at full quantity into 
     Actual Releases
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lAllOrdLinesReleased AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lResult AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cResultMsg AS CHARACTER NO-UNDO.
    DEFINE VARIABLE rOeRelh AS ROWID NO-UNDO.
    DEFINE VARIABLE rOeRell AS ROWID NO-UNDO.
    DEFINE VARIABLE iRelNo AS INTEGER NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-rel FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    ASSIGN lAllOrdLinesReleased = YES
           iRelNo = 0
           .
    FOR EACH bf-oe-ordl NO-LOCK
        WHERE bf-oe-ordl.company EQ ipbf-oe-ord.company 
          AND bf-oe-ordl.ord-no  EQ ipbf-oe-ord.ord-no
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
        WHERE bf-oe-rel.company EQ ipbf-oe-ord.company 
          AND bf-oe-rel.ord-no  EQ ipbf-oe-ord.ord-no
        BREAK BY bf-oe-rel.ord-no:
            
        IF FIRST-OF(bf-oe-rel.ord-no) THEN DO:
           RUN CreateActRelHeader (BUFFER bf-oe-rel, OUTPUT rOeRelh, OUTPUT lResult, OUTPUT cResultMsg).
           IF  lResult THEN DO:
             ASSIGN oplError = TRUE
                    opcMessage = cResultMsg
                    .
             LEAVE REL-LINES.
           END.
           FIND FIRST bf-oe-relh NO-LOCK
              WHERE ROWID(bf-oe-relh) EQ rOeRelh
              NO-ERROR. 
           IF NOT AVAIL bf-oe-relh THEN DO:
                ASSIGN oplError = TRUE
                       opcMessage = "Actual release header not created."
                       .
                LEAVE REL-LINES.
           END.

        END.     
        MESSAGE "run create act relline"
        VIEW-AS ALERT-BOX.
           iRelNo = iRelNo + 1.
           RUN CreateActRelLine (BUFFER bf-oe-rel, BUFFER bf-oe-relh, iRelNo, OUTPUT rOeRell, OUTPUT lResult, OUTPUT cResultMsg).

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

PROCEDURE SetActualReleaseLocation:
    /*------------------------------------------------------------------------------
      Purpose: Assign correct location and bin to oe-rell    
      Parameters:  <none>
      Notes: 
      
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
    
    DEFINE VARIABLE lcbolWhse      AS CHAR      NO-UNDO.
    DEFINE VARIABLE addrelse-cha   AS CHAR      NO-UNDO.
    DEFINE VARIABLE cSelectedValue AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFgFile        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lcLocBin       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound      AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cReturnChar    AS CHARACTER NO-UNDO.
    /* For premier, no tags are selected */
    cSelectedValue = "NoTag".
    
    FIND CURRENT ipbf-oe-rell EXCLUSIVE-LOCK NO-ERROR.
    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ ipbf-oe-rel.company
          AND oe-ordl.ord-no  EQ ipbf-oe-rel.ord-no
          AND oe-ordl.line    EQ ipbf-oe-rel.line
        NO-ERROR.
    IF NOT AVAIL oe-ordl OR NOT AVAIL ipbf-oe-rel OR NOT AVAIL ipbf-oe-rell THEN 
    DO:
        ASSIGN oplError = true
               opcMessage = "Buffer not available"
               .
        RETURN. 
    END.
    
    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ ipbf-oe-rel.company
        AND sys-ctrl.NAME    EQ "AUTOPOST"
        NO-LOCK NO-ERROR.
    lFgFile = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "FGFILE".
    
    IF ipbf-oe-rel.spare-char-1 GT "" 
       AND lcBolWhse EQ "ShipFromWhse" THEN
        ipbf-oe-rell.loc = ipbf-oe-rel.spare-char-1.
    
    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ ipbf-oe-rel.company
        AND sys-ctrl.NAME    EQ "BOLWHSE"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = ipbf-oe-rel.company
            sys-ctrl.NAME    = "BOLWHSE"
            sys-ctrl.descrip = "Default Warehouse for Adding Release/BOL"
            sys-ctrl.log-fld = NO.
        MESSAGE "System control record NOT found. " sys-ctrl.descrip
            UPDATE sys-ctrl.char-fld.
    END.
    IF AVAILABLE sys-ctrl THEN lcBolWhse = sys-ctrl.char-fld.     
        
    RUN sys/ref/nk1look.p (INPUT ipbf-oe-rell.company, "addrelse", "C" /* Logical */, NO /* check by cust */, 
        INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
        OUTPUT cReturnChar, OUTPUT lRecFound).
    addrelse-cha = cReturnChar NO-ERROR.

    /* lcBolWhse is an NK1 flag. ipbf-oe-rel.spare-char-1 is a ship-from */
    /* chosen by the user, so should try to find a bin for it       */
    IF lcBolWhse EQ "SHIPTO" THEN 
    DO:
        FIND FIRST shipto NO-LOCK
            WHERE shipto.company EQ ipbf-oe-rel.company
              AND shipto.cust-no EQ ipbf-oe-rel.cust-no
              AND shipto.ship-no EQ ipbf-oe-rel.ship-no
            USE-INDEX ship-no NO-ERROR.
        IF AVAILABLE shipto THEN 
            ASSIGN
                ipbf-oe-rell.loc     = shipto.loc
                ipbf-oe-rell.loc-bin = shipto.loc-bin
                .
    END.  
    ELSE 
    DO:
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ ipbf-oe-rell.company
              AND fg-bin.job-no  EQ oe-ordl.job-no
              AND fg-bin.job-no2 EQ oe-ordl.job-no2
              AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
              AND fg-bin.qty     GE ipbf-oe-rell.qty
              AND fg-bin.loc     EQ ipbf-oe-rell.loc
            USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE fg-bin AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.qty     GE ipbf-oe-rell.qty
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.qty     GT 0
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX job NO-ERROR.

        IF NOT AVAILABLE fg-bin AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.job-no  EQ oe-ordl.job-no
                  AND fg-bin.job-no2 EQ oe-ordl.job-no2
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                USE-INDEX job NO-ERROR.
    
        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.ord-no  EQ ipbf-oe-rel.ord-no
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.loc     EQ ipbf-oe-rell.loc
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.
   
        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.loc     EQ ipbf-oe-rell.loc
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                  AND fg-bin.qty     GT 0
                USE-INDEX co-ino NO-ERROR.

        IF NOT AVAILABLE fg-bin AND oe-ordl.job-no EQ "" AND lcBolWhse NE "ShipFromWhse" THEN
            FIND FIRST fg-bin NO-LOCK
                WHERE fg-bin.company EQ ipbf-oe-rell.company
                  AND fg-bin.i-no    EQ ipbf-oe-rell.i-no
                USE-INDEX co-ino NO-ERROR.

        IF AVAILABLE fg-bin THEN 
        DO:        
            IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN
                ASSIGN
                    ipbf-oe-rell.loc     = fg-bin.loc
                    ipbf-oe-rell.loc-bin = fg-bin.loc-bin
                    .

            IF addrelse-cha NE "No Tags" AND cSelectedValue NE "NoTag" THEN
                ipbf-oe-rell.tag      = fg-bin.tag.
        
            ASSIGN
                ipbf-oe-rell.job-no   = fg-bin.job-no
                ipbf-oe-rell.job-no2  = fg-bin.job-no2
                ipbf-oe-rell.qty-case = fg-bin.case-count
                .       
        END.                           
        ELSE 
            IF lFgFile THEN 
            DO:
                FIND FIRST itemfg NO-LOCK 
                    WHERE itemfg.company EQ ipbf-oe-rell.company
                      AND itemfg.i-no    EQ ipbf-oe-rell.i-no
                    NO-ERROR.
                IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
                DO:          
                    ASSIGN
                        ipbf-oe-rell.loc     = itemfg.def-loc
                        ipbf-oe-rell.loc-bin = itemfg.def-loc-bin
                        .
                END.
            END.
    END. /* lcBolWhse NE "ShipTo" */
  
    IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
    DO:
        FIND FIRST itemfg NO-LOCK 
            WHERE itemfg.company EQ ipbf-oe-rell.company
              AND itemfg.i-no    EQ ipbf-oe-rell.i-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN
            ASSIGN
                ipbf-oe-rell.loc     = itemfg.def-loc
                ipbf-oe-rell.loc-bin = itemfg.def-loc-bin
                .
        IF ipbf-oe-rell.loc EQ "" OR ipbf-oe-rell.loc-bin EQ "" THEN 
        DO:
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ ipbf-oe-rell.company
                  AND cust.active  EQ "X" 
                NO-ERROR.
            IF AVAILABLE cust THEN 
            DO:
                FIND FIRST shipto WHERE shipto.company EQ ipbf-oe-rell.company
                    AND shipto.cust-no EQ cust.cust-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN
                    ASSIGN   
                        ipbf-oe-rell.loc     = shipto.loc
                        ipbf-oe-rell.loc-bin = shipto.loc-bin.
            END.            
        END.
    END.

    /* lcLocBin is from an NK1 bolprint */
    IF (ipbf-oe-rell.loc-bin EQ "" 
        OR (ipbf-oe-rel.spare-char-1 NE "" AND ipbf-oe-rell.loc NE ipbf-oe-rel.spare-char-1))
        AND lcBOLWhse EQ "ShipFromWhse" THEN 
    DO:
        IF ipbf-oe-rel.spare-char-1 NE "" AND ipbf-oe-rell.loc NE ipbf-oe-rel.spare-char-1 THEN
            ipbf-oe-rell.loc = ipbf-oe-rel.spare-char-1.
        ipbf-oe-rell.loc-bin = lcLocBin.
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ ipbf-oe-rell.company
              AND fg-bin.loc     EQ ipbf-oe-rell.loc
              AND fg-bin.loc-bin EQ ipbf-oe-rell.loc-bin
            NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        DO:
            RUN pCreateFgBinForRelease (INPUT ROWID(ipbf-oe-rell)).        
        END.

    END.

    FIND CURRENT ipbf-oe-rell NO-LOCK.
    ASSIGN oplError = FALSE
           opcMessage = ""
           .
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


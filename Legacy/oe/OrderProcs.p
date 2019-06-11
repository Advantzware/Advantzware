
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


PROCEDURE pReleaseOrder PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a buffer oe-ord, release all lines at full quantity into 
     Actual Releases
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-oe-ord FOR oe-ord.
    DEFINE OUTPUT PARAMETER oplError AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcMessage AS CHARACTER NO-UNDO.


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


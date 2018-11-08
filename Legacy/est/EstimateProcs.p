
/*------------------------------------------------------------------------
    File        : EstimateProcs.p
    Purpose     : Start moving some repetitive code into common procedures

    Syntax      :

    Description : Houses common procedures for calculating estimates and jobs

    Author(s)   : BV
    Created     : Thu Jun 14 18:19:14 EDT 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{est\EstimateProcs.i}

/*-----Refactor below this line----------*/

DEFINE TEMP-TABLE q-sort NO-UNDO 
    FIELD qty AS DECIMAL 
    FIELD rel AS INTEGER.
DEFINE TEMP-TABLE q-sort1 NO-UNDO 
    FIELD qty AS DECIMAL 
    FIELD rel AS INTEGER.
DEFINE TEMP-TABLE q-sort2 NO-UNDO 
    FIELD qty AS DECIMAL 
    FIELD rel AS INTEGER.


DEFINE TEMP-TABLE tt-bqty NO-UNDO 
    FIELD tt-bqty AS INTEGER 
    FIELD tt-brel AS INTEGER.

DEFINE TEMP-TABLE tt-probeit LIKE probeit
    FIELD row-id AS ROWID.
    
DEFINE SHARED BUFFER xest FOR est.
DEFINE SHARED BUFFER xef  FOR ef.
DEFINE SHARED BUFFER xeb  FOR eb.

{ce/print4.i "shared" "shared"}
{ce/print42.i "shared"}

{sys/inc/var.i "shared"}

{custom/globdefs.i}
DEFINE NEW SHARED TEMP-TABLE tt-qtty 
    FIELD qtty     LIKE qtty
    FIELD rel      LIKE rels
    FIELD lRunShip LIKE lRunShips.
                                  
DEFINE TEMP-TABLE tt-ink NO-UNDO 
    FIELD i-code LIKE ink.i-code
    FIELD i-dscr LIKE ink.i-dscr
    FIELD pass   AS INTEGER.
                              
DEFINE TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.
    
DEFINE TEMP-TABLE glu NO-UNDO
    FIELD id     AS ch
    FIELD snum   AS INTEGER
    FIELD bnum   AS INTEGER
    FIELD i-code AS ch      FORMAT "x(10)"
    FIELD i-dscr AS ch      FORMAT "x(19)"
    FIELD i-%    AS INTEGER FORMAT ">>9"
    FIELD i-qty  AS de      FORMAT ">>>9.99"
    FIELD i-cost AS de      FORMAT ">>,>>9.99".
   
DEFINE BUFFER probe-ref FOR reftable.
DEFINE SHARED VARIABLE v-prep-mat LIKE tprep-mat NO-UNDO.  /* for probemk cost */
DEFINE SHARED VARIABLE v-prep-lab LIKE tprep-lab NO-UNDO.
DEFINE SHARED VARIABLE k_frac     AS DECIMAL INIT "6.25" NO-UNDO.
DEFINE SHARED VARIABLE day_str    AS cha     FORM "x(10)" NO-UNDO.
DEFINE SHARED VARIABLE tim_str    AS cha     FORM "x(8)" NO-UNDO.
DEFINE SHARED VARIABLE maxpage    AS INTEGER FORM ">9" NO-UNDO.
DEFINE SHARED VARIABLE tmp-dir    AS cha     NO-UNDO.
DEFINE SHARED VARIABLE col-norm   AS cha     INIT "White/Blue" NO-UNDO. 
DEFINE SHARED VARIABLE qty        AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE v-do-gsa   LIKE do-gsa NO-UNDO.
DEFINE SHARED BUFFER xop FOR est-op.
DEFINE SHARED VARIABLE lv-cebrowse-dir  AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE cCEBrowseBaseDir AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE gEstSummaryOnly  AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE v-qtty           LIKE qtty NO-UNDO.
DEFINE SHARED VARIABLE v-drop-rc        AS LOG       NO-UNDO.
DEFINE        VARIABLE lv-override      AS LOGICAL   NO-UNDO.

/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fGetPromptForVendor RETURNS LOGICAL PRIVATE
    (ipcCompany AS CHARACTER) FORWARD.

FUNCTION fGetCERun RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER,
    ipcIndustry AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */

/* **********************  Internal Procedures  *********************** */

PROCEDURE CalculateEstimate:
    /*------------------------------------------------------------------------------
     Purpose: Calculates an estimate based on estimate RowID passed
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriEst AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipriEf AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipriEb AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER iplUI AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcIndustry AS CHARACTER NO-UNDO.

    DEFINE VARIABLE cCERunF     AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE lv-eb-recid AS RECID     NO-UNDO.
    DEFINE VARIABLE lv-ef-recid AS RECID     NO-UNDO.
    DEFINE VARIABLE tmp-outfile AS cha       NO-UNDO.
    DEFINE VARIABLE viewfile    AS cha       NO-UNDO.
    DEFINE VARIABLE li          AS INTEGER   NO-UNDO.

    
    FIND est NO-LOCK 
        WHERE ROWID(est) = ipriEst.
    
    cCERunF = fGetCERun(est.company, ipcIndustry).
    /*{est/checkuse.i}*/
      
    FIND xef WHERE ROWID(xef) = ipriEf.
    FIND xeb WHERE ROWID(xeb) = ipriEb.
    
    vprint = iplUI
        .
    RUN pResetCalculations.    
    RUN pValidateEstimateInputs(BUFFER est).
    IF CAN-FIND(FIRST ttCalculationErrors) THEN 
    DO:
    /*Display Errors*/
    END.
    ELSE 
    DO:
    
        RUN GetEstimateDir (est.company, OUTPUT cCeBrowseBaseDir, OUTPUT tmp-dir).
        lv-cebrowse-dir = tmp-dir.
  
        IF est.est-type >= 3 AND est.est-type <= 4 AND cCeRunF = "HOP" THEN RUN ce/dAskSum.w (OUTPUT gEstSummaryOnly).
        IF est.est-type EQ 4 THEN RUN ce/com/print4.p NO-ERROR.

        ELSE
            IF est.est-type EQ 3 THEN RUN ce/tan/print4.p NO-ERROR.

            ELSE 
            DO:
                FIND FIRST probe WHERE probe.company = est.company
                    AND probe.est-no = est.est-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE probe THEN RUN est/d-probeu.w (OUTPUT lv-override).

                IF est.est-type EQ 1 THEN 
                    RUN pCalculateSingle (iplUI) NO-ERROR.
                ELSE RUN pCalculateSet (lv-cebrowse-dir) NO-ERROR.

                IF ERROR-STATUS:ERROR THEN 
                DO:
                    SESSION:SET-WAIT-STATE("").
                    RETURN.
                END.

                RUN pEstimateSummary.
            END.

        FOR EACH est-op WHERE est-op.company = est.company AND
            est-op.est-no = est.est-no AND est-op.line > 500 
            EXCLUSIVE-LOCK :
            DELETE est-op.  
        END.
    END.
END PROCEDURE.

PROCEDURE GetEstimateDir:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBaseDir AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubDir AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound AS LOGICAL NO-UNDO.

    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CEBrowse",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT opcBaseDir,
        OUTPUT lFound).
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CEBrowse",
        INPUT "D",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT opcSubDir,
        OUTPUT lFound).

    IF opcBaseDir EQ "" THEN
        opcBaseDir = "users\".

    ASSIGN 
        opcBaseDir = REPLACE(opcBaseDir,"/","\")  /*replace slashes in wrong direction*/
        opcBaseDir = TRIM(opcBaseDir,"\") + "\"  /*ensure there is a slash on the end*/
        .

    IF DEC(opcSubDir) GT 0 THEN 
    DO:
        opcSubDir = opcBaseDir + opcSubDir + "\".
        FILE-INFO:FILE-NAME = opcSubDir.
        IF FILE-INFO:FULL-PATHNAME = ? THEN
            OS-CREATE-DIR VALUE(opcSubDir).        
    END.
    ELSE 
        opcSubDir = opcBaseDir.


END PROCEDURE.

PROCEDURE GetMaterialCost:
    /*------------------------------------------------------------------------------
     Purpose:  Given an item
     Notes:  replaces est/matcost.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipriItem AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipdQty AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER ipcVendNo AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdCost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCostUOM AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdSetup AS DECIMAL NO-UNDO.
   
    DEFINE VARIABLE iIndex     AS INTEGER NO-UNDO.
    DEFINE VARIABLE lCostFound AS LOGICAL NO-UNDO.

    ASSIGN
        lCostFound = NO
        opdCost    = 0
        opdSetup   = 0.

    FIND FIRST ITEM NO-LOCK
        WHERE ROWID(item) EQ ipriItem
        NO-ERROR.
    IF NOT AVAILABLE ITEM THEN LEAVE.
    FIND FIRST e-item OF item NO-LOCK NO-ERROR.
    
    IF AVAILABLE e-item THEN 
    DO:
        RELEASE e-item-vend.
        IF ipcVendNo NE "" THEN 
            FIND FIRST e-item-vend OF e-item NO-LOCK 
                WHERE e-item-vend.item-type EQ YES
                AND e-item-vend.vend-no EQ ipcVendNo
                NO-ERROR.
        IF NOT AVAILABLE e-item-vend THEN 
            FOR EACH e-item-vend OF e-item NO-LOCK
                WHERE e-item-vend.item-type EQ YES
                BY e-item-vend.vend-no:
                LEAVE.
            END.
 
        CREATE ttCostTable.
 
        IF AVAILABLE e-item-vend THEN
        DO:
            ASSIGN 
                ttCostTable.cRunCostUom = e-item-vend.std-uom.
            DO iIndex = 1 TO 10:
                ASSIGN
                    ttCostTable.dRunQty[iIndex]  = e-item-vend.run-qty[iIndex]
                    ttCostTable.dRunCost[iIndex] = e-item-vend.run-cost[iIndex]
                    ttCostTable.dSetups[iIndex]  = e-item-vend.setups[iIndex]
                    .
            END.

            DO iIndex = 1 TO 10:
                ASSIGN
                    ttCostTable.dRunQty[iIndex + 10]  = e-item-vend.runQtyXtra[iIndex]
                    ttCostTable.dRunCost[iIndex + 10] = e-item-vend.runCostXtra[iIndex]
                    ttCostTable.dSetups[iIndex + 10]  = e-item-vend.setupsXtra[iIndex].
            END.
        END.
        DO iIndex = 1 TO 20:
            IF ttCostTable.dRunQty[iIndex] NE 0   AND
                ttCostTable.dRunQty[iIndex] GE ipdQty THEN 
            DO:
                ASSIGN
                    lCostFound = YES
                    opdCost    = ttCostTable.dRunCost[iIndex]
                    opdSetup   = ttCostTable.dSetups[iIndex]
                    opcCostUom = ttCostTable.cRunCostUom
                    .
                LEAVE.
            END.
        END.
  
        DELETE ttCostTable.
    END.

    IF item.i-code EQ "R" AND NOT lCostFound THEN
        opdCost= IF ce-ctrl.r-cost THEN item.avg-cost ELSE item.last-cost.


END PROCEDURE.

PROCEDURE pAddError PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Adds an error to the error temp table
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcError AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiForm AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiPass AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER iplCritical AS LOGICAL NO-UNDO.

    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

    CREATE ttCalculationErrors.
    ASSIGN 
        ttCalculationErrors.cError    = ipcError
        ttCalculationErrors.iFormNo   = ipiForm
        ttCalculationErrors.iBlankNo  = ipiBlank
        ttCalculationErrors.iPassNo   = ipiPass
        ttCalculationErrors.lCritical = iplCritical
        .
    IF iplCritical THEN 
        cMessage = "Critical Error: ".
    cMessage = cMessage + "Form: " + STRING(ipiForm).
    IF ipiBlank GT 0 THEN 
        cMessage = cMessage + " Blank: " + STRING(ipiBlank).
    IF ipiPass GT 0 THEN 
        cMessage = cMessage + " Pass: " + STRING(ipiPass).
    
    ttCalculationErrors.cMessage = cMessage.

END PROCEDURE.

PROCEDURE pCalculateInks PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: replaces ce\pr4-ink.p
 Notes:
------------------------------------------------------------------------------*/
/* ----------------------------------------------------- ce/pr4-ink.p 4/92 cd */

/*{sys/inc/var.i shared}     */
/*{ce/print4.i shared shared}*/
/*def shared buffer xest for est.   */
/*def shared buffer xef for ef.     */
/*def shared buffer xeb for eb.     */
/*DEF SHARED VAR qty AS INT NO-UNDO.*/





/*    DEFINE BUFFER b-ink FOR ink.                                                                 */
/*    DEFINE BUFFER b-glu FOR glu.                                                                 */
/*                                                                                                 */
/*    DEFINE VARIABLE iqty         AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE icost        AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE ipct         AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE gqty         AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE gcost        AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE gpct         AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE rm-wt$       AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE rm-wt%       AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE vuom         LIKE item.cons-uom NO-UNDO.                                     */
/*    DEFINE VARIABLE vqty         AS CHARACTER NO-UNDO.                                           */
/*    DEFINE VARIABLE g-qty        AS de        NO-UNDO.                                           */
/*    DEFINE VARIABLE g-cost       AS de        FORMAT ">>,>>9.99" NO-UNDO.                        */
/*    DEFINE VARIABLE v-1st-frm    AS LOG       INIT YES NO-UNDO.                                  */
/*    DEFINE VARIABLE v-num-up     LIKE xeb.num-up NO-UNDO.                                        */
/*    DEFINE VARIABLE v-col-p      AS INTEGER   NO-UNDO.                                           */
/*    DEFINE VARIABLE v-first-pass AS LOG       NO-UNDO.                                           */
/*                                                                                                 */
/*                                                                                                 */
/*                                                                                                 */
/*    DEFINE BUFFER b-cost  FOR reftable.                                                          */
/*    DEFINE BUFFER b-qty   FOR reftable.                                                          */
/*    DEFINE BUFFER b-setup FOR reftable.                                                          */
/*                                                                                                 */
/*                                                                                                 */
/*    FIND FIRST ce-ctrl WHERE ce-ctrl.company EQ cocode AND ce-ctrl.loc = locode NO-LOCK NO-ERROR.*/
/*                                                                                                 */
/*    v-num-up = xeb.num-up.                                                                       */
/*                                                                                                 */
/*/* i n k s */                                                                                    */
/*    {ce/pr4-ink-single.i xeb}                                                                    */
/*                                                                                                 */
/*    j = 0.                                                                                       */
/*    FOR EACH est-op                                                                              */
/*        WHERE est-op.company EQ xef.company                                                      */
/*        AND est-op.est-no  EQ xef.est-no                                                         */
/*        AND (est-op.qty    EQ v-op-qty OR xest.est-type NE 1)                                    */
/*        AND est-op.s-num   EQ xef.form-no                                                        */
/*        AND est-op.line    GE 500                                                                */
/*        AND (est-op.dept    EQ "PR" OR est-op.dept EQ "CT")                                      */
/*        NO-LOCK,                                                                                 */
/*        FIRST mach                                                                               */
/*        {sys/ref/machW.i}                                                                        */
/*      AND mach.m-code EQ est-op.m-code                                                           */
/*        NO-LOCK                                                                                  */
/*        BY est-op.line:                                                                          */
/*        j = j + 1.                                                                               */
/*        FOR EACH tt-ink WHERE tt-ink.pass EQ j,                                                  */
/*            FIRST ink                                                                            */
/*            WHERE ink.i-code EQ tt-ink.i-code                                                    */
/*            AND ink.i-dscr EQ tt-ink.i-dscr                                                      */
/*            AND ink.snum   EQ est-op.s-num:                                                      */
/*            ink.i-qty = ink.i-qty + mach.ink-waste.                                              */
/*        END.                                                                                     */
/*    END.                                                                                         */
/*                                                                                                 */
/*    {ce/pr4-adh.i}                                                                               */
/*                                                                                                 */
/*    {ce/pr4-ink2.i}                                                                              */
/*                                                                                                 */
/*   FIND FIRST BRD WHERE BRD.form-no = ink.snum AND                                               */
/*                        BRD.blank-no =ink.bnum AND                                               */
/*                        BRD.i-no    = ink.i-code                                                 */
/*                        NO-ERROR.                                                                */
/*   IF NOT AVAILABLE BRD THEN                                                                     */
/*   DO:                                                                                           */
/*      CREATE BRD.                                                                                */
/*      ASSIGN BRD.form-no  = ink.snum                                                             */
/*             BRD.blank-no = ink.bnum                                                             */
/*             BRD.i-no     = ink.i-code                                                           */
/*             BRD.dscr     = ink.i-dscr                                                           */
/*             BRD.basis-w  = item.basis-w.                                                        */
/*   END.                                                                                          */
/*   ASSIGN                                                                                        */
/*   BRD.qty     = BRD.qty + iqty                                                                  */
/*   BRD.qty-uom = "LB"                                                                            */
/*   BRD.sc-uom  = "LB"                                                                            */
/*   BRD.cost    = icost / iqty                                                                    */
/*   BRD.cost-m  = icost / (qty / 1000).                                                           */
/*                                                                                                 */
/*   DISPLAY ink.i-dscr                                                                            */
/*           iqty                     FORMAT ">>>>>9.99"      TO 48                                */
/*           "Lbs"                                                                                 */
/*           icost / (qty / 1000)     FORMAT ">>>>9.99"       TO 68                                */
/*           icost                    FORMAT ">,>>>,>>9.99"   TO 80 SKIP WITH STREAM-IO.           */
/*END.                                                                                             */
/*                                                                                                 */
/*    {ce/pr4-adh2.i}                                                                              */
/*                                                                                                 */
/*   FIND FIRST BRD WHERE BRD.form-no = glu.snum AND                                               */
/*                        BRD.blank-no = glu.bnum AND                                              */
/*                        BRD.i-no    = glu.i-code                                                 */
/*                        NO-ERROR.                                                                */
/*   IF NOT AVAILABLE BRD THEN                                                                     */
/*   DO:                                                                                           */
/*      CREATE BRD.                                                                                */
/*      ASSIGN BRD.form-no  = glu.snum                                                             */
/*             BRD.blank-no = glu.bnum                                                             */
/*             BRD.i-no     = glu.i-code                                                           */
/*             BRD.dscr     = glu.i-dscr                                                           */
/*             BRD.basis-w  = item.basis-w.                                                        */
/*   END.                                                                                          */
/*   ASSIGN                                                                                        */
/*   BRD.qty     = BRD.qty + gqty                                                                  */
/*   BRD.qty-uom = "LB"                                                                            */
/*   BRD.sc-uom  = "LB"                                                                            */
/*   BRD.cost    = gcost / gqty                                                                    */
/*   BRD.cost-m  = gcost / (qty / 1000).                                                           */
/*                                                                                                 */
/*   DISPLAY item.i-name                                                                           */
/*           vqty                     FORMAT "x(9)"           TO 48                                */
/*           vuom                                                                                  */
/*           gcost / (qty / 1000)     FORMAT ">>>>9.99"       TO 68                                */
/*           gcost                    FORMAT ">,>>>,>>9.99"   TO 80 SKIP WITH STREAM-IO.           */
/*END.                                                                                             */
/*                                                                                                 */
/*/* end ---------------------------------- copr. 1992  advanced software, inc. */                 */


END PROCEDURE.

PROCEDURE pCalculateSingle PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: REFACTOR - print4 in probe.w
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER iplUI AS LOGICAL NO-UNDO. 
    DEFINE VARIABLE lReverseLW  AS LOGICAL   NO-UNDO.

    /*def var v-layout as log NO-UNDO.*/
    DEFINE VARIABLE v-line      LIKE probe.line NO-UNDO.  /*used in est/probeset.i*/
    DEFINE VARIABLE v-msf       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-dec       AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-skip-pct  AS LOG       NO-UNDO.
    DEFINE VARIABLE v-i-no      LIKE xeb.stock-no NO-UNDO.
    DEFINE VARIABLE v-2desc     AS LOG       NO-UNDO.
    DEFINE VARIABLE v-header    AS CHARACTER INIT "   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---" NO-UNDO. 

    DEFINE VARIABLE lv-brd-l    LIKE eb.len NO-UNDO.
    DEFINE VARIABLE lv-brd-w    LIKE lv-brd-l NO-UNDO.
    DEFINE VARIABLE lv-brd-sq   AS DECIMAL   FORMAT ">>>>9.9<<<<" NO-UNDO.
    DEFINE VARIABLE lv-brd-sf   AS DECIMAL   FORMAT ">>>>>9.9<<" NO-UNDO.
    DEFINE VARIABLE lv-brd-wu   LIKE lv-brd-sq NO-UNDO.

    DEFINE VARIABLE ld-metric   AS DECIMAL   INIT 1 NO-UNDO.
    DEFINE VARIABLE lv-format   AS CHARACTER INIT ">>>>9.9<<<<" NO-UNDO.
    DEFINE VARIABLE ld-wid      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-len      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-dep      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-fg-rate  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cJobNoPrint AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dShrink     AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bf-est            FOR est.
    DEFINE BUFFER bf-probe          FOR probe.
    DEFINE BUFFER reftable-fold-pct FOR reftable.
    DEFINE BUFFER b-item            FOR ITEM.
    DEFINE BUFFER bf-oe-ord         FOR oe-ord.
    DEFINE BUFFER bf-oe-ordl        FOR oe-ordl.

    DEFINE VARIABLE v-t-win       AS DECIMAL   DECIMALS 4 NO-UNDO.
    DEFINE VARIABLE v             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE vn-out        LIKE ef.n-out-l INITIAL 1 NO-UNDO.
    DEFINE VARIABLE v-outf        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-on-f        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-on-l        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE sh-tmp        LIKE sh-len NO-UNDO.
    DEFINE VARIABLE v-widp        AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-brd-only    LIKE sys-ctrl.log-fld INITIAL NO NO-UNDO.
    DEFINE VARIABLE v-brd-cost    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-module      AS CHARACTER FORMAT "x(60)" NO-UNDO.

    DEFINE VARIABLE v-bqty        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-gsa         AS LOGICAL   INITIAL NO NO-UNDO.
    DEFINE VARIABLE ls-outfile    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE ls-probetime  AS CHARACTER NO-UNDO.  /* time display */
    DEFINE VARIABLE v-tmp-int     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-can-update  AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-orig-gp     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-orig-cm-pct AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-ceSellPrice AS CHARACTER NO-UNDO.

  
    DEFINE VARIABLE i             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE j             AS INTEGER   NO-UNDO.
    DEFINE VARIABLE call_id       AS RECID     NO-UNDO.  
    DEFINE VARIABLE fil_id        AS RECID     NO-UNDO.
    DEFINE VARIABLE lv-error      AS LOG       NO-UNDO.
    DEFINE VARIABLE v-vend-no     LIKE e-item-vend.vend-no INIT "".
    DEFINE VARIABLE lv-ef-recid   AS RECID     NO-UNDO.
 
    ASSIGN
        lv-ef-recid = RECID(ef)
        tmp-dir     = lv-cebrowse-dir.

    IF xest.metric THEN
        ASSIGN
            ld-metric = 25.4
            lv-format = "->>,>>>mm".

    RUN pGetVendor(xest.company, iplUI, OUTPUT v-vend-no).  /*Refactor - Prompt for all materials/vendors*/
/*    {cec/get-vend.i}  /* get vendor number */*/

    /*Should all be part of ttCostMaster for a given Qty*/
/*    FIND FIRST ce-ctrl WHERE ce-ctrl.company EQ cocode AND ce-ctrl.loc EQ locode NO-LOCK NO-ERROR.*/
/*    ASSIGN                                                                                        */
/*        qtty     = 0                                                                              */
/*        ctrl[1]  = ce-ctrl.whse-mrkup / 100                                                       */
/*        ctrl[2]  = ce-ctrl.hand-pct / 100                                                         */
/*        ctrl[3]  = ce-ctrl.rm-rate                                                                */
/*        ctrl[4]  = ce-ctrl.spec-%[1]                                                              */
/*        ctrl[5]  = int(ce-ctrl.comm-add)                                                          */
/*        ctrl[6]  = int(ce-ctrl.shp-add)                                                           */
/*        ctrl[7]  = int(ce-ctrl.sho-labor)                                                         */
/*        ctrl[8]  = int(ce-ctrl.trunc-99)                                                          */
/*        ctrl[11] = ce-ctrl.spec-%[2]                                                              */
/*        ctrl[12] = ce-ctrl.spec-%[3]                                                              */
/*        ctrl[13] = int(ce-ctrl.spec-add[1])                                                       */
/*        ctrl[14] = int(ce-ctrl.spec-add[2])                                                       */
/*        ctrl[15] = int(ce-ctrl.spec-add[3])                                                       */
/*        ctrl[16] = int(ce-ctrl.spec-add[6])                                                       */
/*        ctrl[17] = int(ce-ctrl.spec-add[7])                                                       */
/*        ctrl[18] = int(ce-ctrl.spec-add[8]).                                                      */
/*                                                                                                  */
/*                                                                                                  */
/*    ctrl[19] = ce-ctrl.fold-pct.                                                                  */

    IF RETRY THEN OUTPUT close.

    FIND FIRST xef WHERE xef.company = xest.company 
        AND xef.est-no EQ xest.est-no.
    FIND FIRST xeb WHERE xeb.company = xest.company 
        AND xeb.est-no EQ xest.est-no
        AND xeb.form-no = xef.form-no.
    FIND FIRST xop WHERE xop.company = xest.company 
        AND xop.est-no    EQ xest.est-no
        AND xop.op-speed EQ 0
        NO-LOCK NO-ERROR.

    fg-rate-f = ce-ctrl.fg-rate-farm.

    rm-rate-f = ce-ctrl.rm-rate-farm.
  
    hand-pct-f = ce-ctrl.hand-pct-farm / 100.

    ld-fg-rate = IF xeb.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate.

    DO TRANSACTION:
    {est/recalc-mr.i xest}
        FIND CURRENT recalc-mr NO-LOCK.

        ASSIGN
            do-speed = xest.recalc
            do-mr    = recalc-mr.val[1] EQ 1
            do-gsa   = xest.override.
    END.

    ASSIGN
        save-qty  = qty
        save-lock = xef.op-lock.

    DO TRANSACTION:
        {sys/inc/cerun.i F}
        vmclean = LOOKUP(cerunf,"McLean,HOP,CERunF 2") GT 0.

        {ce/msfcalc.i}

    /*  find first sys-ctrl where sys-ctrl.company eq cocode                             */
    /*                        and sys-ctrl.name    eq "CEPg2"                            */
    /*                        no-lock no-error.                                          */
    /*  if not avail sys-ctrl then do:                                                   */
    /*    create sys-ctrl.                                                               */
    /*    assign                                                                         */
    /*     sys-ctrl.company = cocode                                                     */
    /*     sys-ctrl.name    = "CEPg2"                                                    */
    /*     sys-ctrl.descrip = "Reverse W & L labels for press, die, & # Up on Estimate?".*/
    /*    MESSAGE sys-ctrl.descrip                                                       */
    /*        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO                                   */
    /*        UPDATE sys-ctrl.log-fld.                                                   */
    /*  end.                                                                             */
    /*  v-layout = sys-ctrl.log-fld.                                                     */
    END.

    IF vprint THEN 
    DO:
        /*DO i = 1 TO 4:
          IF xest.est-qty[i] NE 0 THEN qtty[i] = xest.est-qty[i].
        END.*/

        FIND FIRST est-qty
            WHERE est-qty.company EQ xest.company
            AND est-qty.est-no  EQ xest.est-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE est-qty THEN 
        DO i = 1 TO 20:
            IF est-qty.qty[i] NE 0 THEN
                ASSIGN
                    qtty[i + 4] = est-qty.qty[i]
                    rels[i + 4] = est-qty.qty[i + 20].
        END.

        {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}

        FIND FIRST tt-qtty NO-ERROR.
        IF AVAILABLE tt-qtty THEN DELETE tt-qtty.
        CREATE tt-qtty.

        DO i = 1 TO 28:
            ASSIGN
                tt-qtty.qtty[i] = qtty[i]
                tt-qtty.rel[i]  = IF qtty[i] EQ 0 THEN 0
                       ELSE
                       IF rels[i] EQ 0 THEN 1 ELSE rels[i].
        END.

        v-do-all-forms-ink = NO.

        RUN est/getqty.w (INPUT-OUTPUT do-speed, INPUT-OUTPUT do-mr, INPUT-OUTPUT do-gsa, INPUT-OUTPUT v-drop-rc, INPUT-OUTPUT v-match-up,
            INPUT-OUTPUT v-do-all-forms-ink, INPUT-OUTPUT v-board-cost-from-blank, INPUT NO, OUTPUT lv-error).
        IF lv-error THEN RETURN ERROR.

        IF lv-override THEN
            FOR EACH probe
                WHERE probe.company EQ xest.company
                AND probe.est-no  EQ xest.est-no:
                DELETE probe.                 
            END.
  
        DO i = 1 TO 28:
            qtty[i] = tt-qtty.qtty[i].
            rels[i] = tt-qtty.rel[i].
        END.
  {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}
        DO i = 1 TO 28:
            IF qtty[i] EQ 0 THEN rels[i] = 0.
            ELSE IF rels[i] EQ 0 THEN rels[i] = 1.
        END.
    END.
    ELSE qtty[1] = qty.

    DO TRANSACTION:
    {est/op-lock.i xest}

        FIND bf-est WHERE RECID(bf-est) EQ RECID(xest).
        FIND CURRENT recalc-mr.
        ASSIGN
            bf-est.recalc    = do-speed
            recalc-mr.val[1] = INT(do-mr)
            bf-est.override  = do-gsa
            op-lock.val[1]   = INT(bf-est.recalc)
            op-lock.val[2]   = recalc-mr.val[1].
        FIND CURRENT bf-est NO-LOCK.
        FIND CURRENT recalc-mr NO-LOCK.
        FIND CURRENT op-lock NO-LOCK.
    END.

    SESSION:SET-WAIT-STATE("General").

    FIND FIRST sman WHERE sman.sman EQ xeb.sman NO-LOCK NO-ERROR.
    FIND FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ xeb.cust-no
        NO-LOCK NO-ERROR.
    FIND FIRST shipto
        WHERE shipto.cust-no EQ cust.cust-no
        /*and shipto.ship-no eq xeb.ship-no*/
        AND shipto.ship-id EQ xeb.ship-id
        NO-LOCK NO-ERROR.
    FIND FIRST style
        WHERE style.company EQ cocode
        AND style.style   EQ xeb.style
        NO-LOCK NO-ERROR.
    IF AVAILABLE style THEN ctrl2[18] = style.royalty.
    IF cust.cust-no NE "Temp" THEN
        ASSIGN
            cust-ad[1] = cust.name
            cust-ad[2] = cust.addr[1]
            cust-ad[3] = cust.addr[2]
            cust-ad[4] = cust.city + ", " + cust.state + " " + cust.zip.
    ELSE
        ASSIGN
            cust-ad[1] = xeb.ship-name
            cust-ad[2] = xeb.ship-addr[1]
            cust-ad[3] = xeb.ship-addr[2]
            cust-ad[4] = xeb.ship-city + ", " + xeb.ship-state + " " + xeb.ship-zip.

    IF cust-ad[3] EQ "" THEN
        ASSIGN
            cust-ad[3] = cust-ad[4]
            cust-ad[4] = "".

    IF cust-ad[2] EQ "" THEN
        ASSIGN
            cust-ad[2] = cust-ad[3]
            cust-ad[3] = cust-ad[4]
            cust-ad[4] = "".

    ASSIGN
        ship-ad[1] = ship.ship-name
        ship-ad[2] = ship.ship-addr[1]
        ship-ad[3] = ship.ship-addr[2]
        ship-ad[4] = ship.ship-city + ", " + ship.ship-state + " " + ship.ship-zip.

    IF ship-ad[3] EQ "" THEN
        ASSIGN
            ship-ad[3] = ship-ad[4]
            ship-ad[4] = "".
    IF ship-ad[2] EQ "" THEN
        ASSIGN
            ship-ad[2] = ship-ad[3]
            ship-ad[3] = ship-ad[4]
            ship-ad[4] = "".

    ASSIGN
        dsc[1]   = xeb.part-dscr1
        dsc[2]   = xeb.part-dscr2
        brd-l[1] = xeb.t-len
        brd-l[2] = xef.trim-l
        brd-w[1] = xeb.t-wid
        brd-w[2] = xef.trim-w
        ld-len   = xeb.len * ld-metric
        ld-wid   = xeb.wid * ld-metric
        ld-dep   = xeb.dep * ld-metric.

    IF LOOKUP(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ THEN 
    DO:
        IF dsc[2] = "" THEN 
            ASSIGN dsc[2]  = xeb.stock-no
                v-i-no  = ""   
                v-2desc = NO.
        ELSE
            ASSIGN v-i-no  = xeb.stock-no 
                v-2desc = YES.
    END.

    IF ld-metric NE 1 THEN 
    DO:
    {sys/inc/roundup.i ld-len}
    {sys/inc/roundup.i ld-wid}
    {sys/inc/roundup.i ld-dep}
    END.

    ASSIGN
        sizcol[1]  = TRIM(STRING(ld-len,lv-format)) + "x" +
              trim(STRING(ld-wid,lv-format)) + "x" +
              trim(STRING(ld-dep,lv-format))
        sizcol[2]  = xeb.i-coldscr
        stypart[1] = IF AVAILABLE style THEN style.dscr ELSE ""               /*djk*/
        stypart[2] = xeb.part-no.

    IF cerunf EQ "HOP" THEN
        ASSIGN
            brd-l[3] = xef.nsh-len
            brd-w[3] = xef.nsh-wid.
    ELSE
        ASSIGN
            lv-brd-l = xef.nsh-len
            lv-brd-w = xef.nsh-wid
            brd-l[3] = xef.gsh-len
            brd-w[3] = xef.gsh-wid.

    IF xef.roll EQ YES THEN brd-l[4] = xef.gsh-len.
    IF brd-l[3] EQ 0 AND brd-w[3] EQ 0 THEN
        ASSIGN
            brd-l[3] = xef.lsh-len
            brd-w[3] = xef.lsh-wid.
    IF xef.roll EQ YES THEN brd-w[4] = xef.roll-wid.
    ELSE brd-w[4] = 0.
    ASSIGN
        brd-sq[1] = xeb.t-sqin
        brd-sq[2] = brd-l[2] * brd-w[2]
        brd-sq[3] = brd-l[3] * brd-w[3]
        brd-sq[4] = brd-l[4] * brd-w[4]
        lv-brd-sq = lv-brd-l * lv-brd-w
        brd-sf[1] = IF v-corr THEN (brd-sq[1] * .007) ELSE (brd-sq[1] / 144)
        brd-sf[2] = IF v-corr THEN (brd-sq[2] * .007) ELSE (brd-sq[2] / 144)
        brd-sf[3] = IF v-corr THEN (brd-sq[3] * .007) ELSE (brd-sq[3] / 144)
        brd-sf[4] = IF v-corr THEN (brd-sq[4] * .007) ELSE (brd-sq[4] / 144)
        lv-brd-sf = IF v-corr THEN (lv-brd-sq * .007) ELSE (lv-brd-sq / 144)
        call_id   = RECID(xeb).

    DO TRANSACTION:
        /* take out window if any */
        FIND xeb WHERE RECID(xeb) EQ call_id NO-ERROR.
        ASSIGN
            v-t-win   = 0
            xeb.t-win = 0.

        IF xeb.est-type EQ 1 THEN
        DO i = 1 TO 4:
            FIND FIRST b-item WHERE
                b-item.company EQ xef.company AND
                b-item.i-no EQ xef.leaf[i]
                NO-LOCK NO-ERROR.

            IF AVAILABLE b-item AND b-item.mat-type EQ "W" AND
                (xef.leaf-l[i] NE 0 AND xef.leaf-w[i] NE 0) THEN
            DO:
                xeb.t-win = xeb.t-win + (xef.leaf-l[i] * xef.leaf-w[i]).
          
                /*sheet fed windowing*/ 
                IF xef.leaf-bnum[i] EQ 0 THEN
                    v-t-win = v-t-win + (xef.leaf-l[i] * xef.leaf-w[i] / xeb.num-up).
                ELSE
                    v-t-win = v-t-win + (xef.leaf-l[i] * xef.leaf-w[i]).
            END.
        END.
        ELSE
            v-t-win = xeb.t-win.

        FIND xeb WHERE RECID(xeb) EQ call_id NO-LOCK NO-ERROR.
    END.

    FIND FIRST item WHERE (item.company = cocode) AND item.i-no EQ xef.board NO-LOCK NO-ERROR.
    ASSIGN
        brd-wu[1] = brd-sf[1] * item.basis-w
        brd-wu[2] = brd-sf[2] * item.basis-w
        brd-wu[3] = brd-sf[3] * item.basis-w
        brd-wu[4] = brd-sf[4] * item.basis-w
        lv-brd-wu = lv-brd-sf * item.basis-w.

    FOR EACH xjob:
        DELETE xjob.
    END.

    /******************************* l  o  o  p  **********************************/
    loupe:
    DO k = 1 TO 28:
        ASSIGN
            v-op-qty = 0    
            op-tot   = 0
            qty      = qtty[k]
            ctrl2    = 0.

        IF vprint THEN 
        DO:
        {custom/statusMsg.i " 'Calculating... Est#  '  + xest.est-no  + ' Qty - ' + string(qty) "}
        END.

        FOR EACH est-op
            WHERE est-op.company EQ xest.company 
            AND est-op.est-no  EQ xest.est-no 
            AND est-op.line    LT 500
            NO-LOCK
            BREAK BY est-op.qty:
    
            IF FIRST-OF(est-op.qty) THEN 
            DO:
                IF FIRST(est-op.qty) OR
                    CAN-FIND(FIRST est-qty
                    WHERE est-qty.company EQ est-op.company
                    AND est-qty.est-no  EQ est-op.est-no
                    AND est-qty.eqty    EQ est-op.qty)
                    THEN v-op-qty = est-op.qty.
                IF est-op.qty GE qty THEN LEAVE.
            END.
        END.

        /*  for each kli:*/
        /*    delete kli.*/
        /*  end.         */
        /*               */
        /*  for each ink:*/
        /*    delete ink.*/
        /*  end.         */
        /*               */
        /*  for each flm:*/
        /*    delete flm.*/
        /*  end.         */
        /*               */
        /*  for each cas:*/
        /*    delete cas.*/
        /*  end.         */
        /*               */
        /*  for each car:*/
        /*    delete car.*/
        /*  end.         */
        /*               */
        /*  for each blk:*/
        /*    delete blk.*/
        /*  end.         */

        IF qty EQ 0 THEN LEAVE loupe.

        DO TRANSACTION:
            FOR EACH est-op WHERE est-op.company = xest.company 
                AND est-op.est-no EQ xest.est-no
                AND est-op.line  GT 500:
                DELETE est-op.
            END.
            FOR EACH est-op WHERE est-op.company = xest.company 
                AND est-op.est-no EQ xest.est-no
                AND est-op.line  LT 500:
                CREATE xop.
                BUFFER-COPY est-op TO xop
                    ASSIGN
                    xop.line = est-op.line + 500.
            END.
        END.

        maxpage = k.

        RUN ce/prokalk.p .

        ASSIGN
            k   = maxpage
            qty = qtty[k].

        FIND FIRST xop WHERE xop.company = xest.company
            AND xop.est-no EQ xest.est-no
            AND xop.line  GE 500
            NO-LOCK NO-ERROR.
        FIND FIRST ITEM WHERE (item.company = cocode)
            AND item.i-no EQ xef.board
            NO-LOCK NO-ERROR.
        IF AVAILABLE item THEN FIND FIRST e-item OF item NO-LOCK NO-ERROR.
        ASSIGN
            brd-sf[4] = xef.gsh-len * xef.gsh-wid * xef.gsh-qty / 1000   /*tot msf*/
            brd-sf[4] = IF v-corr THEN (brd-sf[4] * .007) ELSE (brd-sf[4] / 144).
        IF AVAILABLE item THEN brd-wu[4] = (brd-sf[4] * item.basis-w) / 2000.

        {est/probeset.i qtty[k] v-match-up}

        IF probe.LINE LT 100 THEN
            ASSIGN
                outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
                outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
                outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99").
        ELSE
            ASSIGN
                outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"999")
                outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"999")
                outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"999").

        ASSIGN 
            day_str = STRING(TODAY,"99/99/9999")
            tim_str = STRING(TIME,"hh:mm am") .
        FORM day_str
            v-module
            tim_str TO 79  SKIP(1)
            WITH FRAME hdr PAGE-TOP WIDTH 80 NO-LABELS NO-BOX STREAM-IO.

        OUTPUT to value(outfile1).

        ASSIGN
            v-module = IF cerunf EQ "HOP" THEN "FCD-0101" ELSE ""
            v-module = FILL(" ",59 - LENGTH(TRIM(v-module))) + TRIM(v-module).
        IF cerunf = "HOP" THEN 
        DO:
            cJobNoPrint = "Job #: " .
            FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ xeb.company
                AND bf-oe-ordl.ord-no EQ xeb.ord-no
                AND bf-oe-ordl.i-no EQ xeb.stock-no NO-LOCK NO-ERROR.
            IF AVAILABLE bf-oe-ordl THEN
                cJobNoPrint = cJobNoPrint + bf-oe-ordl.job-no + "-" + string(bf-oe-ordl.job-no2).
            ELSE 
            DO:
                FIND FIRST bf-oe-ord WHERE bf-oe-ord.company EQ xeb.company
                    AND bf-oe-ord.ord-no EQ xeb.ord-no NO-LOCK NO-ERROR.
                IF AVAILABLE bf-oe-ord THEN
                    cJobNoPrint = cJobNoPrint + bf-oe-ord.job-no + "-" + string(bf-oe-ord.job-no2).
            END.
        END.
        ELSE cJobNoPrint = "".

        DISPLAY day_str v-module tim_str WITH FRAME hdr STREAM-IO.
        DISPLAY "Est#" TRIM(xest.est-no) FORMAT "x(8)"
            "SlsRep:" sman.sname 
            WHEN AVAILABLE sman
            "UserID:" xest.updated-id
            "Prober:" probe.probe-user
            cJobNoPrint FORMAT "X(20)"
            SKIP
            "Cust:" xeb.cust-no
            cust-ad[1] FORMAT "x(29)" TO 44
            "Ship:" ship-ad[1] FORMAT "x(29)" TO 80 SKIP
            WITH NO-LABELS NO-BOX FRAME qwqw STREAM-IO.
 
        IF cust-ad[2] NE "" OR ship-ad[2] NE "" THEN
            PUT cust-ad[2] FORMAT "x(29)" TO 44
                ship-ad[2] FORMAT "x(29)" TO 80 SKIP.
        IF cust-ad[3] NE "" OR ship-ad[3] NE "" THEN
            PUT cust-ad[3] FORMAT "x(29)" TO 44
                ship-ad[3] FORMAT "x(29)" TO 80 SKIP.
        IF cust-ad[4] NE "" OR ship-ad[4] NE "" THEN
            PUT cust-ad[4] FORMAT "x(29)" TO 44
                ship-ad[4] FORMAT "x(29)" TO 80 SKIP.
        IF LOOKUP(cerunf,"ASI,CERunF 1") NE 0  /*cerunf = "ASI"*/ THEN v-header = "   Qty      --- Desc/FG Item ----- -- Size / Color ----- --- Style / Part No ---".
        ELSE v-header = "   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---".
        DISPLAY SKIP(1)
            v-header FORMAT "x(80)"

            qty FORMAT ">>>,>>>,>>>"
            dsc[1] FORMAT "x(22)"
            sizcol[1] FORMAT "x(21)"
            stypart[1] FORMAT "x(23)" SKIP
            SPACE(12)
            dsc[2] FORMAT "x(22)"
            sizcol[2] FORMAT "x(21)"
            stypart[2] FORMAT "x(23)" /*SKIP
    SPACE(12)
    v-i-no FORMAT "x(22)"       */
            /*    SKIP(1)*/
            /*    IF cerunf = "ASI" AND v-2desc THEN SPACE(12) v-i-no FORMAT "x(22)"*/
            WITH NO-BOX NO-LABELS COLOR VALUE("blu/brown") WIDTH 80 FRAME aa1 STREAM-IO.
        IF LOOKUP(cerunf,"ASI,CERunF 1") NE 0 /*cerunf = "ASI"*/ AND v-2desc THEN      
            PUT  SPACE(12) v-i-no FORMAT "x(22)" SKIP(1).
        ELSE
            PUT SKIP(1).
      
        DISPLAY
            SPACE(15) "Width    Length   Sq.Inches  Sq.Feet/Sheet    Weight per Units"
            SKIP
            " Blank Size:" brd-w[1] TO 21 brd-l[1] TO 30 brd-sq[1] TO 42
            " # up:" xeb.num-up FORMAT ">>>9" brd-wu[1] TO 70 SPACE(0) "/M     " SKIP
            "   Die Size:" brd-w[2] TO 21 brd-l[2] TO 30 brd-sq[2] TO 42
            /*xef.trim-l when v-layout @ brd-w[2]
            xef.trim-w when v-layout @ brd-l[2]*/
            brd-sf[2] TO 52 "Sf/Sht"  brd-wu[2] TO 70 SPACE(0) "/M Shts" SKIP
            WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa4 DOWN STREAM-IO.

        IF cerunf NE "HOP" THEN DISPLAY
                "  Feed Size:" lv-brd-w TO 21 lv-brd-l TO 30 lv-brd-sq TO 42
                " #out:" xef.n-out * xef.n-out-l FORMAT ">>9" lv-brd-wu TO 70 SPACE(0) "/M Shts" SKIP
                WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa2 DOWN STREAM-IO.

        DISPLAY
            " Sheet Size:" brd-w[3] TO 21 brd-l[3] TO 30 brd-sq[3] TO 42
            brd-sf[3] TO 52 "Sf/Sht"  brd-wu[3] TO 70 SPACE(0) "/M Shts" SKIP
            " Roll Size :"                    
            WHEN brd-l[4] NE 0
            brd-w[4]  TO 21  
            WHEN brd-l[4] NE 0
            brd-sf[4] TO 52 "MSF"
            brd-wu[4] TO 70 "Tons"  SKIP(1)
            "Materials            Weight Caliper          QTY/Unit  MR $  Matl$/M    TOTAL"
            SKIP
            WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa3 DOWN STREAM-IO.

        /* board */     RUN ce/pr4-brd.p (v-vend-no).
        v-brd-cost = dm-tot[5].

        /* i n k s */   RUN ce/pr4-ink.p.

        /* films */     RUN ce/pr4-flm.p.

        /* cas/tr/pal*/ RUN ce/pr4-cas.p.

        /* special */   RUN ce/pr4-spe.p.

        DO WITH FRAME ac5 NO-LABELS NO-BOX:
            DISPLAY "TOTAL  DIRECT  MATERIALS "
                dm-tot[3] FORMAT ">>>9.99" TO 59
                dm-tot[4] FORMAT ">>>>9.99" TO 68
                dm-tot[5] FORMAT ">,>>>,>>9.99" TO 80 SKIP(1) WITH STREAM-IO.
        END.

        RUN ce/pr4-prp.p . /* Do Prep Charges */
        RUN ce/pr4-mis.p .

        PUT SKIP(1)
            "Machine Description    MR (Hrs) Run  Speed    Rate   MR $    Run $    Total Cost" .

        RUN ce/pr4-mch.p.
        IF ctrl2[2] NE 0 OR ctrl2[3] NE 0 THEN 
        DO:
            PUT "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) TO 80 SKIP.
            op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
        END.

        FIND FIRST carrier
            WHERE carrier.company EQ cocode
            AND carrier.loc     EQ locode
            AND carrier.carrier EQ xeb.carrier
            NO-LOCK NO-ERROR.
        IF AVAILABLE carrier THEN
            FIND FIRST carr-mtx
                WHERE carr-mtx.company  EQ cocode
                AND carr-mtx.loc      EQ locode
                AND carr-mtx.carrier  EQ carrier.carrier
                AND carr-mtx.del-zone EQ xeb.dest-code
                NO-LOCK NO-ERROR.

        FIND FIRST item
            WHERE (item.company = cocode)
            AND item.i-no     EQ xef.board
            AND item.mat-type EQ "B"
            AND item.avg-w    GT 0
            NO-LOCK NO-ERROR.

        ASSIGN
            v-msf = (xeb.t-sqin - v-t-win) * qty / 144000 /* msf */
            v-msf = v-msf * IF AVAILABLE item THEN item.avg-w ELSE 1
            xxx   = v-msf * xef.weight.

        IF xef.medium NE "" THEN 
        DO:
            FIND FIRST item
                WHERE (item.company = cocode)
                AND item.i-no EQ xef.medium
                NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN 
            DO:
                /*override item shrink % with shrink entered in BOM button on Layout screen*/
                IF xef.spare-dec-1 NE 0 
                    THEN dShrink = xef.spare-dec-1.
                ELSE dShrink = ITEM.shrink.
                xxx = xxx + (item.basis-w * (1 - (dShrink / 100)) * v-msf).
            END.
        END.

        IF xef.flute NE "" THEN 
        DO:
            FIND FIRST item
                WHERE (item.company = cocode)
                AND item.i-no EQ xef.flute
                NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN xxx = xxx +
                    (item.basis-w * v-msf).
        END.

        IF xef.lam-code NE "" THEN 
        DO:
            FIND FIRST item
                WHERE (item.company = cocode)
                AND item.i-no EQ xef.lam-code
                NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN xxx = xxx +
                    ((INT(xef.medium NE "") + INT(xef.flute NE "")) *
                    qty * xef.adh-sqin / xeb.num-up / item.sqin-lb).
        END.

        IF xef.adh-code NE "" THEN 
        DO:
            FIND FIRST item
                WHERE (item.company = cocode)
                AND item.i-no EQ xef.adh-code
                NO-LOCK NO-ERROR.
            IF AVAILABLE item THEN xxx = xxx +
                    ((INT(xef.medium NE "") + INT(xef.flute NE "")) *
                    qty * xef.adh-sqin / xeb.num-up / item.sqin-lb).
        END.

        FOR EACH brd,
            FIRST item NO-LOCK
            WHERE (item.company = cocode)
            AND item.i-no EQ brd.i-no
            AND CAN-DO("5,6",item.mat-type):

            xxx = xxx + (brd.qty / 100 * item.weight-100).
        END.

        ASSIGN
            xxx    = xxx + (p-qty * ce-ctrl.def-pal-w) +
            (c-qty * ce-ctrl.def-cas-w) /* add pallet & case */
            fr-tot = 0
            fg-wt  = xxx.

        IF xeb.fr-out-c NE 0 THEN
            fr-tot = xeb.fr-out-c * xxx / 100.
    
        ELSE
            IF xeb.fr-out-m NE 0 THEN
                fr-tot = xeb.fr-out-m * qty / 1000.
    
            ELSE  
                IF AVAILABLE carr-mtx THEN 
                DO:
                    IF carrier.chg-method EQ "P" THEN
                    DO i = 1 TO 10:
                        fr-tot = carr-mtx.rate[i] * p-qty.
                        IF carr-mtx.weight[i] GE p-qty THEN LEAVE.
                    END.
    
                    ELSE
                        IF carrier.chg-method EQ "W" THEN
                        DO i = 1 TO 10:
                            fr-tot = carr-mtx.rate[i] * xxx / 100.
                            IF carr-mtx.weight[i] GE xxx THEN LEAVE.
                        END.
    
                        ELSE 
                        DO:
                            FIND FIRST item
                                WHERE (item.company = cocode)
                                AND item.i-no  EQ xef.board
                                AND item.avg-w GT 0
                                NO-LOCK NO-ERROR.
                            v-msf = v-msf * IF AVAILABLE item THEN item.avg-w ELSE 1.
      
                            DO i = 1 TO 10:
                                fr-tot = carr-mtx.rate[i] * v-msf.
                                IF carr-mtx.weight[i] GE v-msf THEN LEAVE.
                            END.
                        END.
       
                    IF fr-tot LT carr-mtx.min-rate THEN fr-tot = carr-mtx.min-rate.
      
                    fr-tot = fr-tot + (carr-mtx.min-rate * (rels[k] - 1)).
                END.

                {sys/inc/roundup.i fg-wt}
        IF (fg-wt / 100) * fg-rate-f NE 0 THEN
            PUT "Finished Goods Handling" (fg-wt / 100) * fg-rate-f TO 80 SKIP.

        ASSIGN
            op-tot[5] = op-tot[5] + ((fg-wt / 100) * fg-rate-f)
            ctrl2[2]  = ctrl2[2] + ((fg-wt / 100) * fg-rate-f).

        PUT "TOTAL  OPERATIONS        "
            op-tot[3] FORMAT ">>>>9.99"     TO 57
            op-tot[4] FORMAT ">>>>>>9.99"   TO 68
            op-tot[5] FORMAT ">,>>>,>>9.99" TO 80 SKIP(1).

        IF cerunf EQ "HOP" THEN 
        DO:
            FOR EACH brd
                WHERE CAN-FIND(FIRST item
                WHERE item.company EQ xest.company
                AND item.i-no    EQ brd.i-no
                AND CAN-DO("B,P,R,1,2,3,4",item.mat-type)):
                ACCUM brd.qty (TOTAL).
                ACCUM brd.qty-mr + brd.qty-wst (TOTAL).
            END.
            PUT "Total Waste Percentage"
                (ACCUM TOTAL brd.qty-mr + brd.qty-wst) / (ACCUM TOTAL brd.qty) * 100
                FORMAT ">>,>>9.99" TO 80
                SKIP(1).
        END.
  
        /* mat */
        DO i = 1 TO 6:
            ctrl[9] = ce-ctrl.mat-pct[i] / 100.
            IF ce-ctrl.mat-cost[i] GT dm-tot[5] THEN LEAVE.
        END.
        /* lab */
        DO i = 1 TO 6:
            ctrl[10] = ce-ctrl.lab-pct[i] / 100.
            IF ce-ctrl.lab-cost[i] GT op-tot[5] THEN LEAVE.
        END.
        DO TRANSACTION:
            {est/calcpcts.i xest}
            ASSIGN
                calcpcts.val[1] = ctrl[9] * 100
                calcpcts.val[2] = v-brd-cost.
            FIND CURRENT calcpcts NO-LOCK NO-ERROR.
        END.

        ASSIGN
            gsa-mat = ctrl[9]  * 100
            gsa-lab = ctrl[10] * 100
            gsa-com = ce-ctrl.comm-mrkup
            gsa-war = ce-ctrl.whse-mrkup.

        IF AVAILABLE probe THEN
            gsa-fm = int(probe.gsa-fm).
        ELSE
            gsa-fm  = ctrl[19].

        OUTPUT CLOSE.

        RUN ce/gsa.p (ROWID(probe), qtty[k], rels[k]).

        SESSION:SET-WAIT-STATE("general").

        ASSIGN
            ctrl[9]  = gsa-mat / 100
            ctrl[10] = gsa-lab / 100
            ctrl[1]  = gsa-war / 100
            ctrl[19] = gsa-fm / 100
            vmcl     = k.

        OUTPUT to value(outfile1) append .
        RUN ce/pr4-tots.p .
        OUTPUT close.

        ASSIGN
            v-prep-mat = tprep-mat
            v-prep-lab = tprep-lab.

        RUN ce/pr4-mis2.p.
        v-dec = qtty[k] / (xeb.num-up * xef.n-out * xef.n-out-l).
        {sys/inc/roundup.i v-dec}
        IF xef.gsh-qty EQ 0 THEN 
        DO TRANSACTION:
            xef.gsh-qty = v-dec + spo + r-spo[1].
        END.
        RUN ce/probemk.p (ROWID(probe)).

        FIND FIRST blk WHERE blk.id EQ xeb.part-no NO-ERROR.
        FIND FIRST xjob
            WHERE xjob.i-no EQ blk.id
            AND xjob.qty  EQ qtty[k]
            NO-ERROR.
        IF NOT AVAILABLE xjob THEN 
        DO:
            CREATE xjob.
            ASSIGN
                xjob.i-no     = blk.id
                xjob.qty      = qtty[k]
                xjob.cust-no  = xeb.cust-no
                xjob.form-no  = xeb.form-no
                xjob.blank-no = xeb.blank-no
                xjob.pct      = 1.00
                xjob.stock-no = xeb.stock-no.
        END.

        ASSIGN
            xjob.mat = (dm-tot[5]   + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
            xjob.lab = (opsplit$[1] + mis-tot[3] + v-prep-lab + ctrl2[2] + ctrl2[3]) /
              (qtty[k] / 1000)
            xjob.voh = opsplit$[2]                             / (qtty[k] / 1000)
            xjob.foh = opsplit$[3]                             / (qtty[k] / 1000).

        IF NOT do-speed AND NOT do-mr AND xest.est-qty[1] EQ qtty[k] THEN
            FOR EACH xop WHERE xop.company = xest.company AND xop.est-no EQ xest.est-no
                AND xop.line GT 500
                TRANSACTION:
                FIND FIRST est-op WHERE est-op.company = xop.company
                    AND est-op.est-no EQ xop.est-no
                    AND est-op.line  EQ xop.line - 500 NO-ERROR.
                IF AVAILABLE est-op THEN est-op.num-sh = xop.num-sh.
            END.

        IF NOT vprint THEN 
        DO TRANSACTION:

            IF probe.LINE LT 100 THEN
            DO:
                IF OPSYS = "unix" THEN
                    UNIX SILENT rm VALUE(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"99")).
                ELSE
                    DOS SILENT DEL VALUE(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"99")).
            END.
            ELSE
            DO:
                IF OPSYS = "unix" THEN
                    UNIX SILENT rm VALUE(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"999")).
                ELSE
                    DOS SILENT DEL VALUE(tmp-dir + trim(xest.est-no) + ".*" + string(probe.line,"999")).
            END.

            FIND CURRENT probe.
            DELETE probe.
        END.
    END.  /* do k=1to28 */

    IF vprint THEN 
    DO k = 1 TO 28:
        IF qtty[k] EQ 0 THEN LEAVE.

        FOR EACH bf-probe
            WHERE bf-probe.company    EQ xest.company
            AND bf-probe.est-no     EQ xest.est-no
            AND bf-probe.probe-date EQ TODAY
            AND bf-probe.est-qty    EQ qtty[k]
            AND bf-probe.freight    EQ rels[k]
            NO-LOCK
            BY bf-probe.probe-time DESCENDING:
            LEAVE.
        END.

        IF bf-probe.LINE LT 100 THEN
            ASSIGN
                outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(bf-probe.line,"99")
                outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(bf-probe.line,"99")
                outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(bf-probe.line,"99").
        ELSE
            ASSIGN
                outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(bf-probe.line,"999")
                outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(bf-probe.line,"999")
                outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(bf-probe.line,"999").
      
        IF vmclean THEN 
        DO:
            OUTPUT to value(outfile3) append.

            ASSIGN
                vmcl = IF k LT 6  THEN 1  ELSE
            IF k LT 11 THEN 6  ELSE
            IF k LT 16 THEN 11 ELSE
            IF k LT 21 THEN 16 ELSE
            IF k LT 28 THEN 21 ELSE 28
                vhld = vmcl.
 
            {ce/mclean.i vmcl}
    
            PUT SKIP.

            OUTPUT close.
        END.

        IF OPSYS = "unix" THEN
            UNIX SILENT cat VALUE(outfile2) >> VALUE(outfile3).
        ELSE
            DOS SILENT TYPE VALUE(outfile2) >> VALUE(outfile3).

        IF bf-probe.LINE LT 100 THEN
            ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(bf-probe.line,"99").
        ELSE
            ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(bf-probe.line,"999").

        IF SEARCH(outfile1) <> ? THEN
            DOS SILENT  TYPE VALUE(outfile3) > VALUE(ls-outfile).

        RUN ce/probeu3.p (ROWID(bf-probe)).
    END.

    DO TRANSACTION:
        FIND CURRENT op-lock NO-ERROR.
        IF AVAILABLE op-lock THEN DELETE op-lock.
    END.

    SESSION:SET-WAIT-STATE("").
    IF vprint THEN 
    DO:
    {custom/statusMsg.i " 'Calculating Complete....  '  "}
    END.

/* end ---------------------------------- copr. 1992  advanced software, inc. */


END PROCEDURE.

PROCEDURE pEstimateSummary PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: REFACTOR - probe.w - est-summ
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li  AS INTEGER NO-UNDO.
    DEFINE VARIABLE li1 AS INTEGER NO-UNDO.
    DEFINE BUFFER bff-probe FOR probe .
  
    FIND LAST est-summ NO-LOCK
        WHERE est-summ.company EQ est.company
        AND est-summ.est-no  EQ est.est-no
        USE-INDEX est-qty  NO-ERROR.
    li1 = IF AVAILABLE est-summ THEN est-summ.eqty ELSE 0.
  
    FOR EACH mclean:
        DO li = 1 TO 28:
            FOR EACH bff-probe NO-LOCK
                WHERE bff-probe.company    EQ est.company
                AND bff-probe.est-no     EQ est.est-no
                AND bff-probe.probe-date EQ TODAY
                AND bff-probe.est-qty    EQ qtty[li]
                AND bff-probe.freight    EQ IF est.est-type LE 6 THEN rels[li] ELSE 1
          
                BY bff-probe.probe-time DESCENDING:

                CREATE est-summ.
                ASSIGN
                    est-summ.company  = bff-probe.company
                    est-summ.est-no   = bff-probe.est-no
                    li1               = li1 + 1
                    est-summ.eqty     = li1
                    est-summ.summ-tot = STRING(mclean.rec-type,"x(20)")     +
                             STRING(mclean.form-no,"9999999999") +
                             mclean.descr
                    est-summ.e-num    = bff-probe.line
                    est-summ.per-m    = mclean.cost[li].

                LEAVE.
            END.
        END.

        DELETE mclean.
    END.

    RELEASE est-summ.

    IF est.est-type EQ 6 AND vmclean THEN
    DO li = 1 TO 28:
        IF qtty[li] EQ 0 THEN NEXT.

        FOR EACH bff-probe NO-LOCK
            WHERE bff-probe.company    EQ xest.company
            AND bff-probe.est-no     EQ xest.est-no
            AND bff-probe.probe-date EQ TODAY
            AND bff-probe.est-qty    EQ qtty[li]
            AND bff-probe.freight    EQ rels[li]
        
            BY bff-probe.probe-time DESCENDING:
            LEAVE.
        END.

        IF AVAILABLE bff-probe THEN RUN cec/pr4-mcl1.p (ROWID(bff-probe)).
    END.


END PROCEDURE.

PROCEDURE pGetCEVendor PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Returns the options stored in CEVendor Config
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER oplPrompt AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER oplUserInGroup AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcUserGroups AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCategory AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendor AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lFound       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cParseString AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount       AS INTEGER   NO-UNDO.

    oplPrompt = fGetPromptForVendor(ipcCompany).
    IF oplPrompt THEN 
    DO:
    
        RUN sys/ref/nk1Look.p(INPUT ipcCompany,
            INPUT "CEVendor",
            INPUT "C",
            INPUT NO,
            INPUT NO,
            INPUT "",
            INPUT "",
            OUTPUT cParseString,
            OUTPUT lFound).
        IF lFound AND cParseString NE "" THEN 
        DO:
            ASSIGN 
                opcUserGroups  = SUBSTRING(cParseString,1,INDEX(TRIM(cParseString)," ") - 1)
                cParseString = SUBSTRING(cParseString,INDEX(TRIM(cParseString),"(") + 1)
                cParseString = TRIM(cParseString,")")
                opcCategory  = SUBSTRING(cParseString,1, INDEX(cParseString,"=") - 1)
                opcVendor    = SUBSTRING(cParseString,INDEX(cParseString,"=") + 1)
                .
        END.
        IF opcCategory NE "" THEN 
            FIND FIRST procat NO-LOCK 
                WHERE procat.company EQ ipcCompany
                AND procat.procat EQ opcCategory
                NO-ERROR.
        IF NOT AVAILABLE procat THEN opcCategory = "".
        IF opcVendor NE "" THEN 
            FIND FIRST vend NO-LOCK 
                WHERE vend.company EQ ipcCompany
                AND vend.vend-no EQ opcVendor
                NO-ERROR.
        IF NOT AVAILABLE vend THEN opcVendor = "".
        REPEAT iCount = 1 TO NUM-ENTRIES(opcUserGroups):
            IF TRIM(ENTRY(iCount,opcUserGroups)) NE "" THEN
            DO:
                FIND FIRST usergrps WHERE
                    usergrps.usergrps = ENTRY(iCount,opcUserGroups)
                    NO-LOCK NO-ERROR.
                IF AVAILABLE usergrps AND
                    (CAN-DO(usergrps.users,USERID("ASI")) OR
                    TRIM(usergrps.users) EQ "*") THEN
                DO:
                    oplUserInGroup = YES.
                    LEAVE.
                END.
            END.
        END.
    END.    
END PROCEDURE.

PROCEDURE pGetVendor PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: replaces cec/getVend.i
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplUI AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcVendor AS CHARACTER NO-UNDO.

    DEFINE VARIABLE lPromptForVendor AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lUserInGroup     AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cExcludeCategory AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cExcludeVendor   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cUserGroups      AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iCount           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lError           AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lValid           AS LOGICAL   NO-UNDO INIT YES.

    RUN pGetCEVendor(ipcCompany, OUTPUT lPromptForVendor, OUTPUT lUserInGroup, OUTPUT cUserGroups, OUTPUT cExcludeCategory, OUTPUT cExcludeVendor).

    IF lPromptForVendor AND iplUI THEN
    DO:
        lValid = lUserInGroup OR cUserGroups EQ "".
        IF lValid EQ NO AND cExcludeCategory NE "" THEN
            RUN cec/get-exclude-vend.p(INPUT ROWID(xest),
                INPUT cExcludeCategory,
                INPUT cExcludeVendor,
                OUTPUT opcVendor).

        IF lValid THEN
            RUN cec/est-vend.w (RECID(xest), OUTPUT opcVendor, OUTPUT lError) NO-ERROR.
    END.
    IF lError THEN 
    DO:
        RETURN ERROR.
    END.


END PROCEDURE.

PROCEDURE pResetCalculations PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Clears temp-tables for calculations
     Notes:
    ------------------------------------------------------------------------------*/
    EMPTY TEMP-TABLE xprep.
    EMPTY TEMP-TABLE mclean.
    EMPTY TEMP-TABLE kli.
    EMPTY TEMP-TABLE ink.
    EMPTY TEMP-TABLE flm.
    EMPTY TEMP-TABLE cas.
    EMPTY TEMP-TABLE car.
    FOR EACH blk:
        DELETE blk.
    END.

END PROCEDURE.

PROCEDURE pValidateEstimateInputs PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given an estimate, validates all critical values and builds 
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bf-est FOR est.

    FOR EACH ef NO-LOCK 
        WHERE ef.company EQ bf-est.company
        AND ef.est-no EQ bf-est.est-no:
    /*Add form validations here*/
    END.
    FOR EACH eb NO-LOCK 
        WHERE eb.company EQ bf-est.company
        AND eb.est-no EQ bf-est.est-no:
    /*Add Blank validations here*/
    END.
    FOR EACH est-op NO-LOCK
        WHERE est-op.company EQ bf-est.company
        AND est-op.est-no  EQ bf-est.est-no
        AND est-op.line    LT 500,
        FIRST mach NO-LOCK
        WHERE mach.company EQ bf-est.company
        AND mach.loc EQ bf-est.loc
        AND mach.m-code EQ est-op.m-code:
    
        IF mach.obsolete THEN 
            RUN pAddError ("Machine: " + TRIM(mach.m-code) + " is obsolete", est-op.s-num, est-op.b-num, est-op.op-pass,YES).
    
    /*Other per operation validations*/
    END.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fGetPromptForVendor RETURNS LOGICAL PRIVATE
    ( ipcCompany AS CHARACTER ):
    /*------------------------------------------------------------------------------
     Purpose: Returns the log value of CEVendor
     Notes:
    ------------------------------------------------------------------------------*/	
    
    DEFINE VARIABLE cPromptForVendor AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound           AS LOGICAL   NO-UNDO.
    
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CEVendor",
        INPUT "L",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cPromptForVendor,
        OUTPUT lFound).
    
    RETURN lFound AND cPromptForVendor EQ "YES".
		
END FUNCTION.

FUNCTION fGetCERun RETURNS CHARACTER PRIVATE
    (ipcCompany AS CHARACTER, ipcIndustry AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose: Returns the NK1 char value of CERUNF
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE cFormat AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lFound  AS LOGICAL   NO-UNDO.
	
    RUN sys/ref/nk1Look.p(INPUT ipcCompany,
        INPUT "CERun" + ipcIndustry,
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFormat,
        OUTPUT lFound).
    IF NOT lFound THEN cFormat = "ASI".
	
    RETURN cFormat.
		
END FUNCTION.


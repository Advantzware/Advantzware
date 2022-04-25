/* ---------------------------------------------------------------------------*/
/* po detail line - add adder cost to board                                   */
/* ---------------------------------------------------------------------------*/

DEFINE INPUT PARAMETER v-recid          AS RECID NO-UNDO.
DEFINE INPUT PARAMETER v-recid1         AS RECID NO-UNDO.
DEFINE INPUT PARAMETER ipchCompanyID    AS CHARACTER NO-UNDO.


DEFINE VARIABLE v-tot-cost   AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-cost       AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-add-cost   AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-qty-comp   AS DECIMAL NO-UNDO.
DEFINE VARIABLE v-setup      LIKE e-item-vend.setup NO-UNDO.
DEFINE VARIABLE dadder-setup AS DECIMAL NO-UNDO .

DEFINE SHARED VARIABLE v-part-dscr1       AS CHARACTER FORMAT "x(30)".
DEFINE SHARED VARIABLE v-part-dscr2       AS CHARACTER FORMAT "x(30)".
DEFINE SHARED VARIABLE v-basis-w          LIKE item.basis-w NO-UNDO.
DEFINE SHARED VARIABLE v-len              LIKE item.s-len NO-UNDO.
DEFINE SHARED VARIABLE v-wid              LIKE item.s-wid NO-UNDO.
DEFINE SHARED VARIABLE v-dep              LIKE item.s-dep NO-UNDO.
DEFINE SHARED VARIABLE v-gl-desc          AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE SHARED VARIABLE v-tot-msf          AS DECIMAL   FORMAT ">>,>>9.999" INIT 0 NO-UNDO.
DEFINE SHARED VARIABLE v-adder            AS DECIMAL   EXTENT 2.
DEFINE SHARED VARIABLE v-po-qty           AS LOG       INITIAL TRUE NO-UNDO.

DEFINE VARIABLE dCostTotal         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostPerUOM        AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostSetup         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCostUOM           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage           AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdPOProcs          AS HANDLE    NO-UNDO.
DEFINE VARIABLE lNewVendorItemCost AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cReturn            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iCount             AS INTEGER   NO-UNDO.

DEFINE BUFFER xjob-mat FOR job-mat.


DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups   AS DECIMAL DECIMALS 2 EXTENT 20.

/* RUN NK1LOOKUP.P instead of using sys/inc/.i */
RUN sys/ref/nk1look.p (ipchCompanyID, "VendItemCost", "L", NO, NO, "", "", OUTPUT cReturn, OUTPUT lFound).
IF lFound THEN lNewVendorItemCost = IF cReturn EQ "Yes" THEN YES ELSE NO.

RUN po/POProcs.p PERSISTENT SET hdPOProcs.

FIND xjob-mat WHERE RECID(xjob-mat) EQ v-recid1 NO-LOCK.

FIND po-ordl WHERE RECID(po-ordl) EQ v-recid EXCLUSIVE-LOCK.
FIND FIRST po-ord WHERE
    po-ord.company EQ po-ordl.company AND
    po-ord.po-no EQ po-ordl.po-no
    NO-LOCK.

ASSIGN
    v-adder[1] = po-ordl.cost
    v-adder[2] = po-ordl.cons-cost.

DO WITH FRAME po-ordlf:
    FOR EACH job-mat NO-LOCK
        WHERE job-mat.company  EQ xjob-mat.company
        AND job-mat.job      EQ xjob-mat.job
        AND job-mat.frm      EQ xjob-mat.frm
        AND job-mat.job-no   EQ xjob-mat.job-no
        AND job-mat.job-no2  EQ xjob-mat.job-no2
        USE-INDEX seq-idx,

        FIRST item NO-LOCK
        WHERE item.company  EQ job-mat.company
        AND item.i-no     EQ job-mat.i-no
        AND item.mat-type EQ "A":
        ASSIGN 
            v-cost  = 0
            v-setup = 0
            .
        IF lNewVendorItemCost THEN 
        DO:     
            RUN GetVendorCost(
                INPUT  po-ordl.company, 
                INPUT  item.i-no, 
                INPUT  "RM", 
                INPUT  po-ord.vend-no, 
                INPUT  po-ord.cust-no, 
                INPUT  "", 
                INPUT  0, 
                INPUT  0,
                INPUT  po-ordl.ord-qty, 
                INPUT  po-ordl.pr-qty-uom,
                INPUT  po-ordl.s-len,
                INPUT  po-ordl.s-wid, 
                INPUT  po-ordl.s-dep,
                INPUT  "IN", 
                INPUT  item.basis-w, 
                INPUT  "LB/EA", 
                INPUT  YES,
                OUTPUT dCostPerUOM, 
                OUTPUT dCostSetup, 
                OUTPUT cCostUOM,
                OUTPUT dCostTotal, 
                OUTPUT lError, 
                OUTPUT cMessage).  
            
            ASSIGN 
                v-cost  = dCostPerUOM
                v-setup = dCostSetup
                .  
        END.
    
        ELSE 
        DO:
            FIND FIRST e-item NO-LOCK
                WHERE e-item.company EQ po-ordl.company
                AND e-item.i-no    EQ po-ordl.i-no
                NO-ERROR.
        
            FIND FIRST e-item-vend NO-LOCK
                WHERE e-item-vend.company EQ item.company
                AND e-item-vend.i-no    EQ item.i-no
                AND e-item-vend.vend-no EQ po-ord.vend-no
                NO-ERROR.
        END.   
        IF AVAILABLE e-item AND AVAILABLE e-item-vend AND po-ord.vend-no NE "" THEN 
        DO:
            IF po-ordl.pr-qty-uom EQ e-item.std-uom THEN
                v-qty-comp = po-ordl.ord-qty.
            ELSE
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, e-item.std-uom,
                    v-basis-w, v-len, v-wid, v-dep,
                    po-ordl.ord-qty, OUTPUT v-qty-comp).

            v-setup = 0.

            EMPTY TEMP-TABLE tt-eiv.
            CREATE tt-eiv.
            DO iCount = 1 TO 10:
                ASSIGN
                    tt-eiv.run-qty[iCount]  = e-item-vend.run-qty[iCount]
                    tt-eiv.run-cost[iCount] = e-item-vend.run-cost[iCount]
                    tt-eiv.setups[iCount]   = e-item-vend.setups[iCount].
            END.
      
            IF AVAILABLE e-item-vend THEN
            DO:      
                DO iCount = 1 TO 10:
                    ASSIGN
                        tt-eiv.run-qty[iCount + 10]  = e-item-vend.runQtyXtra[iCount]
                        tt-eiv.run-cost[iCount + 10] = e-item-vend.runCostXtra[iCount]
                        tt-eiv.setups[iCount + 10]   = e-item-vend.setupsXtra[iCount].
                END.
            END.

            DO iCount = 1 TO 20:
                IF v-qty-comp LE tt-eiv.run-qty[iCount] THEN
                    LEAVE.
            END.
    
            ASSIGN
                v-setup      = tt-eiv.setups[iCount]
                v-cost       = /*((*/ tt-eiv.run-cost[iCount] /** v-qty-comp) + v-setup ) / v-qty-comp*/
                dadder-setup = dadder-setup + v-setup .
            /* This adds the Adder cost in */
            IF e-item.std-uom NE po-ordl.pr-uom THEN
                RUN sys/ref/convcuom.p(e-item.std-uom, po-ordl.pr-uom, job-mat.basis-w,
                    job-mat.len, job-mat.wid, item.s-dep,
                    v-cost, OUTPUT v-cost).
        END.
        FIND FIRST po-ordl-add NO-LOCK 
            WHERE po-ordl-add.company    EQ po-ordl.company
            AND po-ordl-add.po-no      EQ po-ordl.po-no  
            AND po-ordl-add.line       EQ po-ordl.line   
            AND po-ordl-add.adder-i-no EQ job-mat.i-no 
            NO-ERROR. 
        IF AVAILABLE po-ordl-add THEN 
            RUN PO_UpdatePoAdders IN hdPOProcs(
                INPUT po-ordl.company,
                INPUT po-ordl.po-no,
                INPUT po-ordl.line,
                INPUT job-mat.i-no,
                INPUT v-cost,
                INPUT v-setup,
                INPUT po-ordl.pr-uom
                ). 
        ELSE
            RUN PO_CreatePoAdders IN hdPOProcs(
                INPUT po-ordl.company,
                INPUT po-ordl.po-no,
                INPUT po-ordl.line,
                INPUT job-mat.i-no,
                INPUT v-cost,
                INPUT v-setup,
                INPUT po-ordl.pr-uom
                ).
        ASSIGN 
            v-add-cost   = v-add-cost + v-cost
            dadder-setup = dadder-setup + v-setup
            .
    END.
    ASSIGN
        po-ordl.cost      = po-ordl.cost + v-add-cost
        po-ordl.cons-cost = po-ordl.cost
        po-ordl.setup     = po-ordl.setup + dadder-setup .

    IF po-ordl.pr-uom NE po-ordl.cons-uom THEN
        RUN sys/ref/convcuom.p(po-ordl.pr-uom, po-ordl.cons-uom,
            v-basis-w, v-len, v-wid, v-dep,
            po-ordl.cost, OUTPUT po-ordl.cons-cost).
END.

ASSIGN
    v-adder[1] = po-ordl.cost      - v-adder[1]
    v-adder[2] = po-ordl.cons-cost - v-adder[2].
 
IF VALID-HANDLE(hdPOProcs) THEN 
    DELETE PROCEDURE hdPOProcs.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

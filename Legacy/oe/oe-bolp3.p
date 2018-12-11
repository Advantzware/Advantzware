
/*------------------------------------------------------------------------
    File        : oe-bolp3new.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : WFK - Refactor
    Created     : Mon Dec 10 13:19:46 EST 2018
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE INPUT PARAMETER v-term AS CHARACTER NO-UNDO.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE NEW SHARED BUFFER xinv-head   FOR inv-head.
DEFINE NEW SHARED BUFFER xoe-relh    FOR oe-relh.
DEFINE NEW SHARED BUFFER yoe-relh    FOR oe-relh.
DEFINE NEW SHARED BUFFER xoe-rell    FOR oe-rell.
DEFINE NEW SHARED BUFFER xoe-boll    FOR oe-boll.

DEFINE SHARED     BUFFER xoe-ord     FOR oe-ord.
DEFINE            BUFFER xfg-bin     FOR fg-bin.
DEFINE            BUFFER b-oe-ordl   FOR oe-ordl.
DEFINE            BUFFER xreport     FOR report.
DEFINE            BUFFER b-bolh      FOR oe-bolh.
DEFINE            BUFFER b-invl      FOR inv-line.
DEFINE            BUFFER b-itemfg    FOR itemfg.
DEFINE            BUFFER b-reftable2 FOR reftable.
DEFINE            BUFFER bf-report   FOR report.
DEFINE NEW SHARED VARIABLE v-tax-rate     AS DECIMAL FORMAT ">,>>9.99<<<".
DEFINE NEW SHARED VARIABLE v-frt-tax-rate LIKE v-tax-rate.
DEFINE NEW SHARED VARIABLE v-u-inv        LIKE oe-ctrl.u-inv INIT NO.
DEFINE NEW SHARED VARIABLE v-i-item       LIKE oe-ordl.i-no NO-UNDO. /* INPUT ITEM */
DEFINE NEW SHARED VARIABLE v-i-qty        LIKE oe-ordl.qty NO-UNDO. /* INPUT QUANTITY */
DEFINE NEW SHARED VARIABLE price-ent      AS LOG     NO-UNDO.
DEFINE NEW SHARED VARIABLE fil_id         AS RECID   NO-UNDO.
DEFINE NEW SHARED VARIABLE save_id        AS RECID   NO-UNDO.
DEFINE NEW SHARED VARIABLE matrixExists   AS LOG     NO-UNDO.

{oe/oe-bolpi.i NEW}

DEFINE NEW SHARED TEMP-TABLE tt-bolh NO-UNDO LIKE oe-bolh.
DEFINE NEW SHARED TEMP-TABLE tt-boll NO-UNDO LIKE oe-boll.

DEFINE VARIABLE v-relstat          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-ref-no           AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-rcpth-no         AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-frst             AS LOG       NO-UNDO.
DEFINE VARIABLE v-ext-price        LIKE inv-line.t-price NO-UNDO.
DEFINE VARIABLE v-fg-qty           LIKE oe-boll.qty NO-UNDO.
DEFINE VARIABLE v-po-no            LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE v-uom              LIKE itemfg.prod-uom NO-UNDO.

DEFINE VARIABLE f                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-ASSIGN-comm      AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE exist-amt          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE exist-flag         AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE exist-comm         AS DECIMAL   EXTENT 3 INIT 0 NO-UNDO.
DEFINE VARIABLE temp-tax           AS DECIMAL   INIT 0 NO-UNDO.
DEFINE VARIABLE v-format           LIKE sys-ctrl.char-fld NO-UNDO.
DEFINE VARIABLE v-rel-qty          LIKE oe-rell.qty NO-UNDO.
DEFINE VARIABLE v-complete         AS LOG       NO-UNDO.
DEFINE VARIABLE bo-try             AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-rel-no           LIKE oe-rell.rel-no NO-UNDO.
DEFINE VARIABLE v-b-ord-no         LIKE oe-relh.b-ord-no NO-UNDO.
DEFINE VARIABLE ld-sets            AS DECIMAL   DECIMALS 10 NO-UNDO.
DEFINE VARIABLE ll-calc-disc-FIRST AS LOG       NO-UNDO.
DEFINE VARIABLE v-cost             AS DECIMAL   EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-basis            LIKE sman.commbasis INIT "" NO-UNDO.
DEFINE VARIABLE fg-uom-list        AS cha       NO-UNDO.
DEFINE BUFFER b-reftable  FOR reftable.
DEFINE BUFFER b-reftable3 FOR reftable.
DEFINE VARIABLE ls                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-line-count       AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-start-pos        AS INTEGER   INIT 1 NO-UNDO.
DEFINE VARIABLE li                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-new-lot-code     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-new-frt-pay      AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-new-fob-code     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-fob-code         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-new-sell-price   AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-new-zero-price   AS LOG       NO-UNDO.
DEFINE VARIABLE v-q-back           LIKE itemfg.q-back NO-UNDO.
DEFINE VARIABLE v-rtn-char         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found        AS LOG       NO-UNDO.
DEFINE VARIABLE invstatus-char     AS CHARACTER NO-UNDO.
DEFINE VARIABLE invstatus-log      AS LOG       NO-UNDO.
DEFINE VARIABLE ldShip-qty         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE ld-Inv             AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cRelSCode          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iTotalIScode       AS INTEGER   NO-UNDO.
DEFINE VARIABLE iTotalSScode       AS INTEGER   NO-UNDO.
DEFINE VARIABLE cTermPrefix        AS CHARACTER NO-UNDO.
DEFINE VARIABLE rCurrentInvHeadRow AS ROWID     NO-UNDO.
DEFINE VARIABLE lFirstReportKey3   AS LOG       NO-UNDO.
DEFINE VARIABLE lLastReportKey3    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lUseLogs  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cDebugLog AS CHARACTER NO-UNDO.
DEFINE STREAM sDebug.
DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
DEFINE BUFFER bf-oe-boll FOR oe-boll.

RUN sys/ref/uom-ea.p (OUTPUT fg-uom-list).

DEFINE TEMP-TABLE w-inv NO-UNDO 
    FIELD w-rowid AS ROWID.

DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report.

DEFINE VARIABLE upsFile AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE ttblUPS NO-UNDO
    FIELD company      AS CHARACTER
    FIELD ord-no       AS INTEGER
    FIELD bol-no       AS INTEGER
    FIELD sold-to      AS CHARACTER
    FIELD invHeadRowID AS ROWID
    FIELD cod          AS LOGICAL
    INDEX ttblUPS IS PRIMARY UNIQUE company ord-no sold-to.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.
RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.

DO TRANSACTION:
    {sys/inc/boltransfer.i}
END.

{fg/fullset.i NEW}
{sys/ref/relpost.i}

DO TRANSACTION:
    {sys/inc/invdate.i}
    {sys/inc/relmerge.i}
    {sys/inc/invlotline.i}
    {sys/inc/boreldate.i}
END.

/* Needed for oe/relbo.i & oe/actrelmerg.p */
DEFINE NEW SHARED VARIABLE out-recid  AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE relh-recid AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE v-auto     AS LOG   NO-UNDO.
{oe/chkordl.i NEW}
{oe/relemail.i NEW}


/* ************************  Function Prototypes ********************** */
FUNCTION fLogMsg RETURNS CHARACTER 
    (INPUT ipcMessage AS CHARACTER) FORWARD.

FUNCTION fTabChr RETURNS CHARACTER 
    (ipValue AS CHARACTER) FORWARD.

/* ***************************  Main Block  *************************** */
STATUS DEFAULT .

RUN ipSetNK1Values.
RUN ipStartLog.

   
EMPTY TEMP-TABLE w-inv.

DO TRANSACTION:
    
    /* Delete report if already posted */
    RUN ipCheckPosted.
    
    /* Create temp records for transfers */
    RUN ipTransferTempRecs.
    
    /* Delete report if already posted */
    RUN ipCheckPosted.
    
    /* Check sets for insufficient quantity */
    RUN ipQtyCheckSets.
    
    RUN ipCheckCollision.    
    
    /* Delete report if already posted */    
    RUN ipCheckPosted.
    
    /* Post of BOLs */
    RUN ipPostBols.
    
    RUN ipProcessBackorders.
    RUN ipUpdateReleaseStat.    

END. /* Do trans */

RUN ipProcessShipOnly.
RUN ipCalcHeaderTotals.

RUN ipUpsFile.

DELETE OBJECT hNotesProcs.

STATUS DEFAULT.

RETURN.

/* **********************  Internal Procedures  *********************** */

PROCEDURE ipCalcHeaderTotals:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    FOR EACH w-inv:
        /*RUN oe/invcheck.p (w-rowid).*/
        IF CAN-FIND(inv-head WHERE ROWID(inv-head) EQ w-rowid) THEN 
        DO:      
            /* update total cost, etc on invoice header */
            RUN oe/oe-invup.p (w-rowid, INPUT YES).      
        END.
        RUN ipCheckInvLn (w-rowid).    
    END.


END PROCEDURE.

PROCEDURE ipCheckCollision:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH report NO-LOCK WHERE report.term-id BEGINS cTermPrefix
        AND report.term-id NE v-term,
        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id:
        /* Check that someone else isn't posting the same BOL */
      
        FOR EACH bf-oe-boll NO-LOCK WHERE bf-oe-boll.b-no EQ oe-boll.b-no:
            FIND FIRST bf-report NO-LOCK 
                WHERE bf-report.term-id BEGINS cTermPrefix
                AND bf-report.rec-id  EQ report.rec-id
                AND bf-report.rec_key NE report.rec_key
                NO-ERROR.
            /* Found overlap with another user */
            IF AVAILABLE bf-report THEN 
                fLogMsg("Collision with other user oe-bolp3.p " + " BOL# " + STRING(oe-boll.bol-no) + " Key03: " + report.key-03
                    + " term-id: " + bf-report.term-id + " rpt.term-id: " + report.term-id).          
        END.
    END. 


END PROCEDURE.

PROCEDURE ipCheckInvLn:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER iprInvHeadRow AS ROWID NO-UNDO.
    DEFINE BUFFER bf-inv-line FOR inv-line.
    DEFINE BUFFER bf-inv-head FOR inv-head.
    DEFINE VARIABLE iCorrectRno AS INTEGER NO-UNDO.
  
    /* spare-char-3 will contain the rowid of the inv-head at the time the inv-line was created */
    FOR EACH inv-line NO-LOCK 
        WHERE inv-line.spare-char-3 = STRING(iprInvHeadRow)    
        AND NOT CAN-FIND(FIRST inv-head of inv-line) :
       
        FIND FIRST bf-inv-head WHERE ROWID(bf-inv-head) EQ iprInvHeadRow NO-LOCK NO-ERROR.
    
        IF AVAIL bf-inv-head THEN 
            iCorrectRno = bf-inv-head.r-no.
        ELSE
            iCorrectRno = 0.
        
        IF iCorrectRNo EQ 0 THEN 
            fLogMsg("ChkInv: Correct Invoice NOT Found " + " Rno# " + STRING(iCorrectRno) + " BNO: " + STRING(inv-line.b-no) + " Item: " + inv-line.i-no).

        IF iCorrectRno NE 0 THEN 
        DO:
            fLogMsg("ChkInv: Correct Invoice Found " + " Rno# " + STRING(iCorrectRno) + " BNO: " + STRING(inv-line.b-no) + " Item: " + inv-line.i-no).
            FIND FIRST bf-inv-line WHERE ROWID(bf-inv-line) EQ rowid(inv-line)
                EXCLUSIVE-LOCK.
            bf-inv-line.r-no = iCorrectRno.  
        END.         
         
    END.

END PROCEDURE.

PROCEDURE ipCheckPosted:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH report WHERE report.term-id EQ v-term,
        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
        FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no   EQ oe-boll.b-no:

        IF (oe-bolh.deleted OR oe-bolh.posted)
            AND oe-boll.s-code <> "T"  THEN DELETE report.
    END.

END PROCEDURE.

PROCEDURE ipCreateInvLine:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER bf-oe-boll FOR oe-boll.
    DEFINE PARAMETER BUFFER bf-oe-ordl FOR oe-ordl.   
    DEFINE INPUT  PARAMETER ipiRNo AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcEstType LIKE oe-ord.est-type NO-UNDO.
    DEFINE INPUT  PARAMETER ipdOrdDate AS DATE NO-UNDO.
    DEFINE INPUT  PARAMETER ipdRelPrice AS DECIMAL NO-UNDO.
    DEFINE INPUT  PARAMETER iprInvHeadRow AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER oprInvLinerow AS ROWID NO-UNDO.

    CREATE inv-line.

    ASSIGN
        inv-line.r-no         = ipiRno
        inv-line.company      = bf-oe-boll.company
        inv-line.ord-no       = bf-oe-boll.ord-no
        inv-line.b-no         = bf-oe-boll.b-no
        inv-line.line         = bf-oe-boll.line
        inv-line.i-no         = bf-oe-boll.i-no
        inv-line.stat         = bf-oe-boll.s-code
        inv-line.est-no       = bf-oe-ordl.est-no
        inv-line.est-type     = ipcEstType
        inv-line.ord-date     = ipdOrdDate
        inv-line.part-no      = bf-oe-ordl.part-no
        inv-line.i-name       = bf-oe-ordl.i-name
        inv-line.i-dscr       = bf-oe-ordl.i-dscr
        inv-line.pr-uom       = bf-oe-ordl.pr-uom
        inv-line.price        = (IF ipdRelPrice GT 0 THEN ipdRelPrice ELSE bf-oe-ordl.price)
        inv-line.cas-cnt      = IF bf-oe-ordl.pr-uom EQ "CS" THEN bf-oe-ordl.cas-cnt
                                                   ELSE bf-oe-boll.qty-case
        inv-line.req-code     = bf-oe-ordl.req-code
        inv-line.req-date     = bf-oe-ordl.req-date
        inv-line.prom-code    = bf-oe-ordl.prom-code
        inv-line.prom-date    = bf-oe-ordl.prom-date
        inv-line.part-dscr1   = bf-oe-ordl.part-dscr1
        inv-line.part-dscr2   = bf-oe-ordl.part-dscr2
        inv-line.po-no-po     = bf-oe-ordl.po-no-po
        inv-line.e-num        = bf-oe-ordl.e-num
        inv-line.form-no      = bf-oe-ordl.form-no
        inv-line.blank-no     = bf-oe-ordl.blank-no
        inv-line.j-no         = bf-oe-ordl.j-no
        inv-line.job-no       = bf-oe-ordl.job-no
        inv-line.job-no2      = bf-oe-ordl.job-no2
        inv-line.tax          = bf-oe-ordl.tax
        inv-line.disc         = bf-oe-ordl.disc
        inv-line.qty          = bf-oe-ordl.qty
        inv-line.p-c          = bf-oe-boll.p-c
        inv-line.po-no        = bf-oe-boll.po-no
        inv-line.lot-no       = bf-oe-boll.lot-no
        inv-line.spare-char-3 = STRING(iprInvHeadRow)
        inv-line.spare-char-4 = v-term
        .
    
    IF bf-oe-boll.zeroPrice EQ 1 THEN
        inv-line.price = 0.
    ELSE IF bf-oe-boll.sell-price NE 0 THEN
            inv-line.price = oe-boll.sell-price.
    oprInvLinerow = rowid(inv-line).
END PROCEDURE.

PROCEDURE ipCreateUPS:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipSoldTo AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipOrdNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipBolNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipTerms AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipRowID AS ROWID NO-UNDO.

    IF CAN-FIND(ttblUPS WHERE ttblUPS.company EQ ipCompany
        AND ttblUPS.ord-no EQ ipOrdNo
        AND ttblUPS.bol-no EQ ipBolNo
        AND ttblUPS.sold-to EQ ipSoldTo) THEN RETURN.
    FIND terms NO-LOCK WHERE terms.company EQ ipCompany
        AND terms.t-code EQ ipTerms NO-ERROR.
    IF AVAILABLE terms THEN

        CREATE ttblUPS.
    ASSIGN
        ttblUPS.company      = ipCompany
        ttblUPS.ord-no       = ipOrdNO
        ttblUPS.bol-no       = ipBolNo
        ttblUPS.sold-to      = ipSoldTo
        ttblUPS.invHeadRowID = ipRowID                                         
        ttblUPS.cod          = IF AVAILABLE terms THEN terms.cod ELSE FALSE.

END PROCEDURE.

PROCEDURE ipGetOeRelSCode:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* This should be in a common function library */
    DEFINE INPUT  PARAMETER iprOeRelRow AS ROWID       NO-UNDO.
    DEFINE OUTPUT PARAMETER opcS-code AS CHARACTER   NO-UNDO.
    DEF BUFFER bf-oe-rel FOR oe-rel.
    DEF VAR v-reltype   AS CHAR NO-UNDO.
    DEF VAR ll-transfer AS LOG  NO-UNDO.

    FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ iprOeRelRow NO-LOCK NO-ERROR.

    /* task 04011103*/
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name EQ "RelType" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
            AND sys-ctrl-ship.ship-id = bf-oe-rel.ship-id NO-LOCK NO-ERROR.
    IF NOT AVAIL sys-ctrl-shipto THEN
        FIND FIRST sys-ctrl-shipto OF sys-ctrl WHERE sys-ctrl-shipto.cust-vend-no = oe-ordl.cust-no
            AND sys-ctrl-ship.ship-id = "" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl-shipto AND sys-ctrl-shipto.log-fld THEN v-reltype = sys-ctrl-shipto.char-fld.
    ELSE IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-reltype = sys-ctrl.char-fld.

    opcS-code = /*IF v-reltype <> "" THEN reftable.CODE
                     ELSE */ IF ll-transfer            THEN "T"
    ELSE
        IF oe-ordl.is-a-component AND
        (bf-oe-rel.s-code = "" OR
        bf-oe-rel.s-code NE "T")   THEN "S"
        ELSE
        IF bf-oe-rel.s-code <> ""           THEN bf-oe-rel.s-code
        ELSE "B".

END PROCEDURE.

PROCEDURE ipPostBols:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    rCurrentInvHeadRow = ?.
    FOR EACH report WHERE report.term-id EQ v-term,

        FIRST oe-boll WHERE RECID(oe-boll) EQ report.rec-id,
        
        FIRST oe-bolh WHERE oe-bolh.b-no EQ oe-boll.b-no,
        
        FIRST oe-ordl
        WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
        USE-INDEX ord-no,
    
        FIRST oe-rell
        WHERE oe-rell.company EQ oe-boll.company
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        AND oe-rell.r-no    EQ oe-boll.r-no
        AND oe-rell.i-no    EQ oe-boll.i-no
        AND oe-rell.line    EQ oe-boll.line,

        FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no,
      
        FIRST cust NO-LOCK
        WHERE cust.company EQ oe-bolh.company
        AND cust.cust-no EQ oe-bolh.cust-no,

        FIRST oe-ord
        WHERE oe-ord.company EQ oe-boll.company
        AND oe-ord.ord-no  EQ oe-boll.ord-no,
      
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
        USE-INDEX i-no
    
        BREAK BY report.key-01
        BY report.key-02
        BY report.key-03
        BY report.key-04
        BY report.key-05
        BY report.key-06
        BY report.key-07:

        STATUS DEFAULT "Processing BOL Posting 2........ BOL#: " + STRING(oe-bolh.bol-no).

        lFirstReportKey3 = (IF FIRST-OF(report.key-03) THEN TRUE ELSE FALSE).
        lLastReportKey3 = (IF LAST-OF(report.key-03) THEN TRUE ELSE FALSE).
        /* Note: oe-bolp3 requires locks on oe-ord, oe-ordl, oe-bolh, oe-boll */
        RUN ipPostSingleBOL.
    
        CREATE tt-report.
        BUFFER-COPY report TO tt-report.
        DELETE report.
            
    END.
 


END PROCEDURE.

PROCEDURE ipPostSingleBOL:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE rCreatedInvLine AS ROWID NO-UNDO.
    RELEASE inv-head.

    IF rCurrentInvHeadRow NE ? THEN 
        FIND FIRST inv-head WHERE ROWID(inv-head) EQ rCurrentInvHeadRow NO-ERROR.

    IF lFirstReportKey3                  
       OR NOT AVAILABLE inv-head                  
       OR inv-head.cust-no NE oe-bolh.cust-no THEN 
    DO:
       
        /* Original Code */  
        v-ref-no = NEXT-VALUE(inv_r_no_seq). 
       
  
        RUN oe/custxship.p (oe-bolh.company,
            oe-bolh.cust-no,
            oe-bolh.ship-id,
            BUFFER shipto).
        
        IF NOT AVAILABLE shipto THEN
            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ oe-bolh.company
                AND shipto.cust-no EQ oe-bolh.cust-no
                USE-INDEX ship-no NO-ERROR.

        FIND FIRST reftable WHERE
            reftable.reftable EQ "oe-bolh.lot-no" AND
            reftable.rec_key  EQ oe-bolh.rec_key
            USE-INDEX rec_key
            NO-LOCK NO-ERROR.

        IF AVAILABLE reftable THEN
            ASSIGN v-fob-code = IF reftable.CODE EQ "O" THEN "ORIG"
                          ELSE IF reftable.CODE EQ "D" THEN "DEST"
                          ELSE reftable.CODE.
        ELSE
            ASSIGN v-fob-code = "".        
        RELEASE reftable.
       
        CREATE inv-head.
        ASSIGN
            inv-head.sold-no      = shipto.ship-id
            inv-head.sold-name    = shipto.ship-name
            inv-head.sold-addr[1] = shipto.ship-addr[1]
            inv-head.sold-addr[2] = shipto.ship-addr[2]
            inv-head.sold-state   = shipto.ship-state
            inv-head.sold-city    = shipto.ship-city
            inv-head.sold-zip     = shipto.ship-zip
            inv-head.r-no         = v-ref-no
            inv-head.company      = oe-bolh.company
            inv-head.bol-no       = oe-bolh.bol-no
            inv-head.bill-to      = oe-bolh.cust-no
            inv-head.cust-no      = oe-bolh.cust-no
            inv-head.frt-pay      = oe-bolh.frt-pay
            inv-head.carrier      = oe-bolh.carrier
            inv-head.ship-i[1]    = oe-bolh.ship-i[1]
            inv-head.ship-i[2]    = oe-bolh.ship-i[2]
            inv-head.ship-i[3]    = oe-bolh.ship-i[3]
            inv-head.ship-i[4]    = oe-bolh.ship-i[4]
            inv-head.fob-code     = (IF v-fob-code <> "" THEN v-fob-code ELSE oe-ord.fob-code)
            inv-head.contact      = oe-ord.contact
            inv-head.terms        = oe-ord.terms
            inv-head.terms-d      = oe-ord.terms-d
            inv-head.sman[1]      = oe-ord.sman[1]
            inv-head.sman[2]      = oe-ord.sman[2]
            inv-head.sman[3]      = oe-ord.sman[3]
            inv-head.s-pct[1]     = oe-ord.s-pct[1]
            inv-head.s-pct[2]     = oe-ord.s-pct[2]
            inv-head.s-pct[3]     = oe-ord.s-pct[3]
            inv-head.s-comm[1]    = oe-ord.s-comm[1]
            inv-head.s-comm[2]    = oe-ord.s-comm[2]
            inv-head.s-comm[3]    = oe-ord.s-comm[3]
            inv-head.f-bill       = NO
            inv-head.tax-gr       = IF AVAILABLE shipto AND shipto.tax-code NE ""
                           THEN shipto.tax-code ELSE oe-ord.tax-gr
            inv-head.tot-ord      = 0
            inv-head.inv-no       = 0
            inv-head.stat         = ""
            inv-head.deleted      = NO
            inv-head.posted       = NO
            inv-head.inv-date     = IF invdate-chr EQ "Current" THEN TODAY
                           ELSE oe-bolh.bol-date
            inv-head.cust-name    = cust.name
            inv-head.addr[1]      = cust.addr[1]
            inv-head.addr[2]      = cust.addr[2]
            inv-head.city         = cust.city
            inv-head.state        = cust.state
            inv-head.zip          = cust.zip
            inv-head.curr-code[1] = cust.curr-code
            rCurrentInvHeadRow    = ROWID(inv-head)
            .
        RUN CopyShipNote IN hNotesProcs (oe-bolh.rec_key, inv-head.rec_key).
    
        IF invStatus-log THEN
            inv-head.stat = "W".
  
        FIND FIRST usergrps WHERE
            usergrps.usergrps = "IN"
            NO-LOCK NO-ERROR.

        IF AVAILABLE usergrps AND TRIM(usergrps.users) NE "" THEN
        DO:
            ASSIGN
                v-line-count = 0
                v-start-pos  = 1.

            DO li = 1 TO LENGTH(usergrps.users):
                ls = SUBSTR(usergrps.users,li,1).
       
                IF v-line-count < 5 AND ls EQ CHR(10) OR ls EQ CHR(13) THEN
                    ASSIGN
                        v-line-count                  = v-line-count + 1
                        inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos)
                        v-start-pos                   = li + 1.
     
                IF v-line-count < 5 AND li = LENGTH(usergrps.users) AND
                    NOT(ls EQ CHR(10) OR ls EQ CHR(13)) THEN
                    ASSIGN
                        v-line-count                  = v-line-count + 1
                        inv-head.bill-i[v-line-count] = SUBSTR(usergrps.users,v-start-pos,li - v-start-pos + 1).
            END.
     
            RELEASE usergrps.
        END.

        DO li = 1 TO 4:
            IF inv-head.bill-i[li] = "" THEN
                inv-head.bill-i[li] = oe-ord.bill-i[li].
        END.

        CREATE w-inv.
        w-rowid = ROWID(inv-head).
    END. /* first {1}.{2} */

    IF oe-bolh.freight NE 0 AND inv-head.frt-pay EQ "B" THEN inv-head.f-bill = YES.

    FOR EACH oe-ordm
        WHERE oe-ordm.company EQ oe-boll.company
        AND oe-ordm.ord-no  EQ oe-boll.ord-no
        AND oe-ordm.bill    EQ "Y":
          
        CREATE inv-misc.
        BUFFER-COPY oe-ordm EXCEPT rec_key TO inv-misc
            ASSIGN
            inv-misc.r-no           = inv-head.r-no
            inv-misc.posted         = NO
            inv-misc.deleted        = NO
            inv-misc.inv-i-no       = oe-ordm.ord-i-no
            inv-misc.inv-line       = oe-ordm.ord-line
            inv-misc.s-commbasis[1] = oe-ordm.commbasis[1]
            oe-ordm.bill = "I".   /** Set billing flag to (I)nvoiced **/
    END.

    FIND FIRST job-hdr
        WHERE job-hdr.company EQ oe-boll.company
        AND job-hdr.loc     EQ oe-boll.loc
        AND job-hdr.i-no    EQ oe-boll.i-no
        AND job-hdr.ord-no  EQ oe-boll.ord-no
        AND job-hdr.job-no  EQ oe-ordl.job-no
        AND job-hdr.job-no2 EQ oe-ordl.job-no2
        NO-LOCK NO-ERROR.

    /** update release **/

    ASSIGN
        oe-relh.ship-no   = oe-bolh.ship-no
        oe-relh.ship-id   = oe-bolh.ship-id
        oe-relh.ship-i[1] = oe-bolh.ship-i[1]
        oe-relh.ship-i[2] = oe-bolh.ship-i[2]
        oe-relh.ship-i[3] = oe-bolh.ship-i[3]
        oe-relh.ship-i[4] = oe-bolh.ship-i[4].
    RUN CopyShipNote IN hNotesProcs (oe-bolh.rec_key, oe-relh.rec_key).

    IF oe-rell.link-no EQ 0 THEN 
    DO:
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

    IF AVAILABLE oe-rel THEN 
    DO:

        ASSIGN
            oe-rel.ship-no   = oe-relh.ship-no
            oe-rel.ship-id   = oe-relh.ship-id
            oe-rel.ship-i[1] = oe-relh.ship-i[1]
            oe-rel.ship-i[2] = oe-relh.ship-i[2]
            oe-rel.ship-i[3] = oe-relh.ship-i[3]
            oe-rel.ship-i[4] = oe-relh.ship-i[4]
            oe-rel.po-no     = report.key-07.
   
        RUN CopyShipNote IN hNotesProcs (oe-relh.rec_key, oe-rel.rec_key).
    END. /* avail oe-rel */
   
    /** Use ship-no to find customer shipto because ship-no is the
        primary index. **/
    FIND FIRST shipto
        WHERE shipto.company EQ oe-relh.company
        AND shipto.cust-no EQ oe-relh.cust-no
        AND shipto.ship-no EQ oe-relh.ship-no
        NO-LOCK NO-ERROR.
    IF AVAILABLE shipto AND AVAILABLE oe-rel THEN
        ASSIGN
            oe-rel.ship-addr[1] = shipto.ship-addr[1]
            oe-rel.ship-addr[2] = shipto.ship-addr[2]
            oe-rel.ship-city    = shipto.ship-city
            oe-rel.ship-state   = shipto.ship-state
            oe-rel.ship-zip     = shipto.ship-zip.
    
    ASSIGN
        oe-bolh.posted = YES
        oe-boll.posted = YES.

    IF invlotline-log EQ NO THEN
        FIND FIRST inv-line
            WHERE inv-line.r-no   EQ inv-head.r-no
            AND inv-line.ord-no EQ oe-boll.ord-no
            AND inv-line.b-no   EQ oe-bolh.b-no
            AND inv-line.i-no   EQ oe-boll.i-no
            AND inv-line.line   EQ oe-boll.line
            AND inv-line.po-no  EQ oe-boll.po-no
            USE-INDEX r-no NO-ERROR.
    ELSE
    DO:
        FIND FIRST inv-line
            WHERE inv-line.r-no   EQ inv-head.r-no
            AND inv-line.ord-no EQ oe-boll.ord-no
            AND inv-line.b-no   EQ oe-bolh.b-no
            AND inv-line.i-no   EQ oe-boll.i-no
            AND inv-line.line   EQ oe-boll.line
            AND inv-line.po-no  EQ oe-boll.po-no
            AND inv-line.lot-no EQ oe-boll.lot-no
            USE-INDEX r-no NO-ERROR.
    END.
    
    IF NOT AVAILABLE inv-line THEN 
    DO:
        /* If not avail oe-rel then error */
        RUN ipCreateInvLine (BUFFER oe-boll, BUFFER oe-ordl, inv-head.r-no,  oe-ord.est-type, oe-ord.ord-date, 
                             (IF AVAILABLE(oe-rel) THEN oe-rel.price ELSE 0), ROWID(inv-head), OUTPUT rCreatedInvLine).
        FIND FIRST inv-line EXCLUSIVE-LOCK WHERE ROWID(inv-line) EQ rCreatedInvLine NO-ERROR.

    END. /* Create inv-line */
    
    ASSIGN
        inv-line.t-weight      = inv-line.t-weight + oe-boll.weight
        inv-head.t-inv-weight  = inv-head.t-inv-weight + oe-boll.weight
        inv-line.t-freight     = inv-line.t-freight + oe-boll.freight
        inv-head.t-inv-freight = inv-head.t-inv-freight + oe-boll.freight.

    /* Moved to before extended price calc for inv-qty */
    /** Increase invoice Qty when invoice or invoice & ship **/
    IF oe-boll.s-code NE "S" AND NOT oe-ordl.is-a-component THEN
        inv-line.inv-qty = inv-line.inv-qty + oe-boll.qty.
  
    /** Increase ship Qty when ship or invoice & ship **/
    IF oe-boll.s-code NE "I"                                            OR
        CAN-FIND(FIRST b-oe-ordl 
        WHERE b-oe-ordl.company        EQ oe-ordl.company
        AND b-oe-ordl.ord-no         EQ oe-ordl.ord-no
        AND b-oe-ordl.is-a-component EQ YES
        AND b-oe-ordl.set-hdr-line   EQ oe-ordl.line
        ) THEN
        inv-line.ship-qty = inv-line.ship-qty + oe-boll.qty.

    IF AVAILABLE oe-rel THEN
        RUN ipGetOeRelSCode (INPUT ROWID(oe-rel), OUTPUT cRelScode).

    /** 12301405 - If ship only, price must be zero **/

    oe-ordl.stat = "".

    RUN oe/ordlsqty.p (ROWID(oe-ordl), OUTPUT oe-ordl.inv-qty, OUTPUT oe-ordl.ship-qty).
    IF cRelScode EQ "S"  THEN
        ASSIGN inv-line.qty     = 0 inv-line.price   = 0 inv-line.inv-qty = 0.

    inv-line.t-price = inv-line.inv-qty / 1000 * inv-line.price.

    IF inv-line.pr-uom BEGINS "L" AND inv-line.pr-uom NE "LB" THEN
        inv-line.t-price = inv-line.price *
            IF inv-line.inv-qty LT 0 THEN -1 ELSE IF inv-line.inv-qty EQ 0 THEN 0 ELSE 1.
    ELSE IF inv-line.pr-uom EQ "CS" THEN
            inv-line.t-price = inv-line.inv-qty /
                (IF inv-line.cas-cnt NE 0 THEN
                inv-line.cas-cnt
                ELSE
                IF itemfg.case-count NE 0 THEN
                itemfg.case-count ELSE 1) *
                inv-line.price.
        ELSE IF LOOKUP(inv-line.pr-uom,fg-uom-list) GT 0 THEN
                inv-line.t-price = inv-line.inv-qty * inv-line.price.
            ELSE
                FOR EACH uom
                    WHERE uom.uom  EQ inv-line.pr-uom
                    AND uom.mult NE 0
                    NO-LOCK:
                    inv-line.t-price = inv-line.inv-qty / uom.mult * inv-line.price.
                    LEAVE.
                END.
    inv-line.t-price = ROUND(inv-line.t-price,2).

    IF inv-line.disc NE 0 THEN
        inv-line.t-price = 
            IF ll-calc-disc-first THEN 
            (inv-line.t-price - ROUND(inv-line.t-price * inv-line.disc / 100,2))
            ELSE
            ROUND(inv-line.t-price * (1 - (inv-line.disc / 100)),2).

    RUN oe/invlcost.p (ROWID(inv-line),
        OUTPUT v-cost[1], OUTPUT v-cost[2],
        OUTPUT v-cost[3], OUTPUT v-cost[4],
        OUTPUT inv-line.cost, OUTPUT inv-line.t-cost).

    DO i = 1 TO 3:          /** Calculate Commission Amount **/
        ASSIGN
            inv-line.sname[i]  = oe-ord.sname[i]
            inv-line.s-comm[i] = oe-ordl.s-comm[i]
            inv-line.s-pct[i]  = oe-ordl.s-pct[i]
            inv-line.sman[i]   = oe-ordl.s-man[i].
    END.

    DO i = 1 TO EXTENT(inv-line.sman):    /** Calculate Commission Amount **/
        RUN custom/combasis.p (oe-boll.company, inv-line.sman[i], cust.type, itemfg.procat, 0,
            cust.cust-no,
            OUTPUT v-basis).

        IF v-basis EQ "G" THEN
            inv-line.comm-amt[i] = ROUND(((inv-line.t-price - inv-line.t-cost)
                * inv-line.s-comm[i]) / 100,2).

        ELSE
            inv-line.comm-amt[i] = ROUND((((inv-line.t-price
                * inv-line.s-pct[i]) / 100)
                * inv-line.s-comm[i]) / 100,2).        
      
    END.

    IF AVAILABLE oe-rel THEN
        RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT oe-rel.stat).

    IF v-u-inv THEN 
    DO:
    {oe/oe-bolp.i "oe-ordl"}
 
    END.
    
    /** update order status **/
    ASSIGN
        oe-ord.stat          = "P"
        oe-ord.inv-no        = 0
        oe-ord.inv-date      = ?
        oe-ord.t-inv-weight  = 0
        oe-ord.t-inv-tax     = 0
        oe-ord.t-inv-freight = 0
        oe-ord.t-inv-rev     = 0
        oe-ord.t-inv-cost    = 0.

    IF NOT v-u-inv THEN oe-bolh.w-ord = YES.

    IF lLastReportKey3 AND AVAILABLE inv-head THEN 
    DO:
            
        FIND xinv-head WHERE ROWID(xinv-head) EQ ROWID(inv-head) NO-LOCK NO-ERROR.

        IF AVAILABLE xinv-head THEN
            FOR EACH b-invl WHERE b-invl.r-no EQ inv-head.r-no NO-LOCK,
                FIRST xoe-ord
                WHERE xoe-ord.company EQ inv-line.company
                AND xoe-ord.ord-no  EQ inv-line.ord-no
                NO-LOCK
                BREAK BY b-invl.b-no:
      
                /* Lookup Price Matrix for Billable Shipto */
                IF inv-head.cust-no EQ inv-head.sold-no                          AND
                    inv-head.cust-no NE xoe-ord.cust-no                           AND
                    CAN-FIND(FIRST oe-prmtx
                    {oe/oe-prmtxW.i}
                  AND oe-prmtx.cust-no            EQ inv-head.cust-no
                  AND oe-prmtx.i-no               BEGINS b-invl.i-no
                  AND SUBSTR(oe-prmtx.i-no,1,100) EQ b-invl.i-no)    THEN 
                DO:
                    ASSIGN
                        fil_id    = RECID(b-invl)
                        save_id   = fil_id
                        price-ent = YES
                        v-i-item  = b-invl.i-no
                        v-i-qty   = b-invl.inv-qty.

                    RUN oe/oe-ipric.p.
                END.

                IF v-u-inv THEN RUN oe/invpost4.p (RECID(b-invl), 1).
    
                IF oe-bolh.trailer EQ 'UPS' THEN
                    RUN ipCreateUPS (inv-head.company,inv-head.sold-no,inv-line.ord-no,
                        inv-head.bol-no,xoe-ord.terms,ROWID(inv-head)).

            END. /* each b-invl */
        
    END. /* if last-of */

END PROCEDURE.

PROCEDURE ipProcessBackorders:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* Check for and Create Backorder Releases */
    FOR EACH tt-report WHERE tt-report.term-id EQ v-term,

        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ tt-report.rec-id,
        
        FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no,
        
        FIRST oe-ordl
        WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
        USE-INDEX ord-no,
    
        FIRST cust NO-LOCK
        WHERE cust.company EQ oe-bolh.company
        AND cust.cust-no EQ oe-bolh.cust-no,

        FIRST oe-ord NO-LOCK
        WHERE oe-ord.company EQ oe-boll.company
        AND oe-ord.ord-no  EQ oe-boll.ord-no,
      
        FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ oe-boll.company
        AND itemfg.i-no    EQ oe-boll.i-no
        USE-INDEX i-no
      
        BREAK BY oe-boll.i-no
        BY oe-boll.r-no
        BY oe-boll.line
        BY oe-boll.ord-no
        BY oe-boll.rel-no
        BY oe-boll.b-ord-no:
        STATUS DEFAULT "Processing BOL Posting 3........ BOL#: " + STRING(oe-bolh.bol-no).       
    
        {oe/oe-relbo.i oe-boll.b-ord-no}

    END.


END PROCEDURE.

PROCEDURE ipProcessShipOnly:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH tt-report WHERE tt-report.term-id EQ v-term,

        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ tt-report.rec-id,

        FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no
        BREAK BY oe-boll.i-no BY oe-boll.ord-no:

        /* If the release is ship-only and we're posting the BOL,               */
        /* clean up the fg-bin records that are placeholders, total the + and - */
        IF  oe-boll.s-code EQ "S" THEN 
        DO: 
          
            FIND FIRST oe-rell WHERE oe-rell.r-no EQ oe-boll.r-no
                AND oe-rell.i-no EQ oe-boll.i-no
                NO-LOCK NO-ERROR.          

            IF AVAILABLE oe-rell THEN 
                RUN oe/cleanShipOnlyBins.p (INPUT ROWID(oe-rell)).
 
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ oe-boll.company
                AND oe-ordl.ord-no EQ oe-boll.ord-no
                AND oe-ordl.LINE EQ oe-boll.LINE
                NO-LOCK NO-ERROR.
          
            IF NOT AVAILABLE oe-ordl THEN
                NEXT.

            iTotalISCode = 0.
            iTotalSScode = 0.
            FOR EACH oe-rel WHERE oe-rel.company EQ oe-boll.company
                AND oe-rel.ord-no EQ oe-boll.ord-no
                AND oe-rel.i-no   EQ oe-boll.i-no
                NO-LOCK:
                RUN ipGetOeRelSCode (INPUT ROWID(oe-rel), OUTPUT cRelSCode).

                IF cRelSCode EQ "I" THEN
                    iTotalISCode = iTotaliSCode + oe-rel.qty.
                ELSE
                    IF cRelSCode EQ "S" THEN
                        iTotalSSCode = iTotalSSCode + oe-rel.qty.

            END.
       
            IF iTotalISCode GT 0 AND iTotalISCode NE iTotalSSCode THEN 
            DO:
 
                /* Missing a placeholder records, so run fg-mkbin */
                FIND FIRST itemfg WHERE itemfg.company EQ oe-boll.company
                    AND itemfg.i-no EQ oe-boll.i-no
                    NO-LOCK NO-ERROR.
                IF AVAILABLE itemfg THEN
                    RUN fg/fg-mkbin.p (INPUT RECID(itemfg)).

            END.
            LEAVE.
        END.
    END.


END PROCEDURE.

PROCEDURE ipQtyCheckSets:
    /*------------------------------------------------------------------------------
     Purpose: Check for enough components to invoice set header,
              create invoice only release and BOL lines 
     Notes:
    ------------------------------------------------------------------------------*/
  
    FOR EACH report NO-LOCK WHERE report.term-id EQ v-term,

        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ report.rec-id,
        
        FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no,
    
        FIRST oe-rell NO-LOCK
        WHERE oe-rell.company EQ oe-boll.company
        AND oe-rell.ord-no  EQ oe-boll.ord-no
        AND oe-rell.r-no    EQ oe-boll.r-no
        AND oe-rell.i-no    EQ oe-boll.i-no
        AND oe-rell.line    EQ oe-boll.line,

        FIRST oe-relh NO-LOCK WHERE oe-relh.r-no EQ oe-rell.r-no,
        
        FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company EQ oe-boll.company
        AND oe-ordl.ord-no  EQ oe-boll.ord-no
        AND oe-ordl.line    EQ oe-boll.line
        AND oe-ordl.i-no    EQ oe-boll.i-no
        AND oe-ordl.is-a-component EQ YES
        AND oe-ordl.set-hdr-line   NE 0
        USE-INDEX ord-no

        BREAK BY oe-ordl.ord-no
        BY oe-ordl.set-hdr-line
        BY oe-ordl.line:
    
        STATUS DEFAULT "Processing BOL Posting 1........ BOL#: " + STRING(oe-bolh.bol-no).
    
    
        IF LAST-OF(oe-ordl.set-hdr-line) THEN 
        DO:
            /* Find the set header line */
            FIND FIRST b-oe-ordl NO-LOCK
                WHERE b-oe-ordl.company EQ oe-ordl.company
                AND b-oe-ordl.ord-no  EQ oe-ordl.ord-no
                AND b-oe-ordl.line    EQ oe-ordl.set-hdr-line
                AND b-oe-ordl.is-a-component EQ NO
                AND ROWID(b-oe-ordl) NE ROWID(oe-ordl)
                NO-ERROR.

            IF AVAILABLE b-oe-ordl THEN 
            DO:
                /* Calc all qty shipped for this set header oe-ordl */
                RUN oe/ordlsets.p (ROWID(b-oe-ordl), v-term, OUTPUT ld-sets).
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ b-oe-ordl.company
                    AND itemfg.i-no    EQ b-oe-ordl.i-no
                    USE-INDEX i-no NO-ERROR.
                IF AVAILABLE itemfg AND itemfg.isaset THEN 
                DO:
                    ld-sets = TRUNC(ld-sets, 0).
                END.
        
                /* Sets for header item GT header item t-inv-qty*/
                IF ld-sets GT b-oe-ordl.t-inv-qty THEN 
                DO:
                    FIND FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ b-oe-ordl.company
                        AND itemfg.i-no    EQ b-oe-ordl.i-no
                        USE-INDEX i-no NO-ERROR.

                    ldShip-qty = 0.

                    RUN oe/ordlsqty.p (ROWID(b-oe-ordl),
                        OUTPUT ld-inv,
                        OUTPUT ldShip-Qty).

          
                    ld-sets = ld-sets - /* b-oe-ordl.ship-qty */ ldShip-Qty.
          
                    CREATE xoe-rell.
                    BUFFER-COPY oe-rell EXCEPT rec_key TO xoe-rell
                        ASSIGN
                        xoe-rell.i-no     = b-oe-ordl.i-no
                        xoe-rell.line     = b-oe-ordl.line
                        xoe-rell.rel-no   = 0
                        xoe-rell.b-ord-no = 0
                        xoe-rell.s-code   = "I"
                        xoe-rell.qty      = ld-sets
                        xoe-rell.qty-case = 1
                        xoe-rell.cases    = ld-sets
                        xoe-rell.partial  = 0
                        xoe-rell.loc      = ""
                        xoe-rell.loc-bin  = ""
                        xoe-rell.tag      = ""
                        xoe-rell.cust-no  = "".

                    RELEASE fg-bin.

                    FOR EACH fg-bin FIELDS(qty loc loc-bin tag) NO-LOCK
                        WHERE fg-bin.company EQ xoe-rell.company
                        AND fg-bin.job-no  EQ xoe-rell.job-no
                        AND fg-bin.job-no2 EQ xoe-rell.job-no2
                        AND fg-bin.i-no    EQ xoe-rell.i-no
                        USE-INDEX job
                        BY fg-bin.qty DESCENDING:
                        ASSIGN
                            xoe-rell.loc     = fg-bin.loc
                            xoe-rell.loc-bin = fg-bin.loc-bin
                            xoe-rell.tag     = fg-bin.tag.
                        LEAVE.
                    END.

                    IF NOT AVAILABLE fg-bin AND AVAILABLE itemfg THEN
                        RUN fg/autopost.p (ROWID(itemfg),
                            xoe-rell.job-no,
                            xoe-rell.job-no2,
                            OUTPUT xoe-rell.loc,
                            OUTPUT xoe-rell.loc-bin).

                    FIND CURRENT xoe-rell NO-LOCK NO-ERROR.
                    IF NOT AVAILABLE xoe-rell THEN
                        MESSAGE "Error during posting - please notify ASI (oe-bolp3-281)"
                            VIEW-AS ALERT-BOX.
                    CREATE xoe-boll.
                    BUFFER-COPY oe-boll EXCEPT rec_key TO xoe-boll
                        ASSIGN
                        xoe-boll.i-no     = b-oe-ordl.i-no
                        xoe-boll.line     = b-oe-ordl.line
                        xoe-boll.rel-no   = 0
                        xoe-boll.b-ord-no = 0
                        xoe-boll.s-code   = "I"
                        xoe-boll.qty      = ld-sets
                        xoe-boll.qty-case = 1
                        xoe-boll.cases    = ld-sets
                        xoe-boll.partial  = 0
                        xoe-boll.loc      = xoe-rell.loc
                        xoe-boll.loc-bin  = xoe-rell.loc-bin
                        xoe-boll.tag      = xoe-rell.tag
                        xoe-boll.cust-no  = xoe-rell.cust-no.

                    CREATE xreport.
                    BUFFER-COPY report EXCEPT rec_key TO xreport
                        ASSIGN
                        xreport.rec-id = RECID(xoe-boll)
                        xreport.key-05 = STRING(xoe-boll.rel-no,"9999999999")
                        xreport.key-06 = STRING(xoe-boll.b-ord-no,"9999999999").
                    RELEASE xoe-boll.
                END. /*  ld-sets GT b-oe-ordl.t-inv-qty */
            END. /* IF AVAIL b-oe-ordl */
        END. /* LAST-OF(oe-ordl.set-hdr-line) */
    END. /* Each report */
    

END PROCEDURE.

PROCEDURE ipStartLog:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    lUseLogs = TRUE. /* Use debug logging unless it's turned off */
    IF SEARCH("custfiles\logs\" + "block-oe-bolp3-logging.txt") NE ? THEN 
        lUseLogs = FALSE.
    cDebugLog = "custfiles\logs\" + "oe-bolp3" + STRING(TODAY,"99999999") + STRING(TIME) + STRING(RANDOM(1,1000)) + ".txt".
    IF lUseLogs THEN 
        OUTPUT STREAM sDebug TO VALUE(cDebugLog).
    IF ERROR-STATUS:ERROR THEN 
        lUseLogs = FALSE.

    /* First part of the term value */
    cTermPrefix = STRING(YEAR(TODAY),"9999")      +
        string(MONTH(TODAY),"99")       +
        string(DAY(TODAY),"99").

END PROCEDURE.

PROCEDURE ipTransferTempRecs:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH report WHERE report.term-id EQ v-term,
        FIRST oe-boll NO-LOCK
        WHERE RECID(oe-boll) EQ report.rec-id
        AND oe-boll.s-code EQ "T":

        CREATE tt-report.
        BUFFER-COPY report TO tt-report.
        DELETE report.    
    END.

END PROCEDURE.

PROCEDURE ipUpdateReleaseStat:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    FOR EACH tt-report WHERE tt-report.term-id EQ v-term,

        FIRST oe-boll NO-LOCK WHERE RECID(oe-boll) EQ tt-report.rec-id,
        
        FIRST oe-bolh NO-LOCK WHERE oe-bolh.b-no EQ oe-boll.b-no
        BREAK BY oe-boll.r-no BY oe-boll.i-no BY oe-boll.ord-no:
        STATUS DEFAULT "Processing BOL Posting 4........ BOL#: " + STRING(oe-bolh.bol-no).




        IF LAST-OF(oe-boll.ord-no) THEN 
        DO:
            FIND FIRST oe-rell WHERE oe-rell.r-no EQ oe-boll.r-no
                AND oe-rell.i-no EQ oe-boll.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-rell THEN           
                FIND FIRST oe-rel WHERE oe-rel.link-no = oe-rell.r-no
                    EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE oe-rel THEN 
            DO:
                RUN oe/rel-stat.p (INPUT ROWID(oe-rel), OUTPUT v-relstat).
                IF v-relstat NE oe-rel.stat THEN
                    oe-rel.stat = v-relstat.
                FIND itemfg-loc WHERE itemfg-loc.company EQ oe-boll.company
                    AND itemfg-loc.i-no EQ oe-boll.i-no
                    AND itemfg-loc.loc  EQ oe-rel.spare-char-1
                    EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                IF AVAILABLE itemfg-loc THEN
                    RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, 
                        OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
                RELEASE oe-rel.
                RELEASE itemfg-loc.
            END.
            ELSE 
            DO:        
                FOR EACH oe-rel WHERE oe-rel.company = oe-boll.company
                    AND oe-rel.ord-no  = oe-boll.ord-no
                    AND oe-rel.i-no    = oe-boll.i-no
                    EXCLUSIVE-LOCK.
                    RUN oe/rel-stat.p (INPUT ROWID(oe-rel), OUTPUT v-relstat).
                    IF v-relstat NE oe-rel.stat THEN
                        oe-rel.stat = v-relstat.
                    FIND itemfg-loc WHERE itemfg-loc.company EQ oe-boll.company
                        AND itemfg-loc.i-no EQ oe-boll.i-no
                        AND itemfg-loc.loc  EQ oe-rel.spare-char-1
                        EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
                    IF AVAILABLE itemfg-loc THEN
                        RUN fg/calcqabl.p (ROWID(itemfg), itemfg-loc.loc, 
                            OUTPUT itemfg-loc.q-alloc, OUTPUT v-q-back).
                    RELEASE oe-rel.
                    RELEASE itemfg-loc.
  
                END. /* Each oe-rel */
            END. /* Oe-rel not found */
        END. /* last-of ord-no */

    END. /* each tt-report */


END PROCEDURE.

PROCEDURE ipSetNK1Values:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "L", NO, NO, "", "", 
        OUTPUT v-rtn-char, OUTPUT v-rec-found).
    invstatus-log = LOGICAL(v-rtn-char).
    /* Invstatus to determine invoice status when created  */
    RUN sys/ref/nk1look.p (cocode, "INVSTATUS", "C", NO, NO, "", "", 
        OUTPUT invstatus-char, OUTPUT v-rec-found).
    /* UPSFILE to for CSV creation in oe-bolh.trailer = "UPS"  */
    RUN sys/ref/nk1look.p (cocode, "UPSFILE", "C", NO, NO, "", "", 
        OUTPUT upsFile, OUTPUT v-rec-found).


    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "INVPRINT"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN 
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = cocode
            sys-ctrl.name    = "INVPRINT"
            sys-ctrl.descrip = "Print Invoice Headers on Invoice Form".
        MESSAGE "Invoice Format:" UPDATE sys-ctrl.char-fld.
    END.
    ASSIGN
        v-format           = sys-ctrl.char-fld
        ll-calc-disc-FIRST = v-format EQ "Dayton".
    IF v-format EQ "HARWELL" THEN invdate-chr = "Current".
                              
    FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.
    v-u-inv = oe-ctrl.u-inv.


END PROCEDURE.

PROCEDURE ipUpsFile:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE attnContact     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE emailAddr       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE printHeaderLine AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE codAmount       AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bCust    FOR cust.
    DEFINE BUFFER bInvHead FOR inv-head.

    IF NOT CAN-FIND(FIRST ttblUPS) OR upsFile EQ '' THEN RETURN.
    printHeaderLine = SEARCH(upsFile) EQ ?.
    OUTPUT TO VALUE(upsFile) APPEND.
    IF printHeaderLine THEN
        EXPORT DELIMITER '~t'
            'Order#' 'BOL#' 'Ship ID' 'Name' 'Address 1' 'Address 2' 'City' 'State' 'Zip'
            'Contact' 'EMail' 'COD Amt'.
    FOR EACH ttblUPS NO-LOCK:
        FIND bInvHead NO-LOCK WHERE ROWID(bInvHead) EQ ttblUPS.invHeadRowID NO-ERROR.
        IF NOT AVAILABLE bInvHead THEN NEXT.
        FIND FIRST bCust NO-LOCK WHERE bCust.company EQ bInvHead.company
            AND bCust.cust-no EQ bInvHead.cust-no NO-ERROR.
        IF NOT AVAILABLE bCust THEN NEXT.
        ASSIGN
            emailAddr   = ''
            attnContact = ''.
        FOR EACH phone NO-LOCK WHERE phone.table_rec_key EQ bCust.rec_key:
            IF CAN-FIND(FIRST emaildtl
                WHERE emaildtl.emailcod EQ 'R-BolPrt.'
                AND emaildtl.table_rec_key EQ phone.rec_key) THEN 
            DO:
                ASSIGN
                    emailAddr   = IF phone.e_mail NE '' THEN phone.e_mail ELSE bCust.email
                    attnContact = phone.attention.
                LEAVE. /* use FIRST one found */
            END. /* if can-find */
        END. /* each phone */
        codAmount = IF ttblUPS.cod THEN bInvHead.t-inv-rev ELSE 0.
        EXPORT DELIMITER '~t'
            ttblUPS.ord-no
            ttblUPS.bol-no
            ttblUPS.sold-to
            bInvHead.sold-name
            bInvHead.sold-addr[1]
            bInvHead.sold-addr[2]
            bInvHead.sold-city
            bInvHead.sold-state
            bInvHead.sold-zip
            attnContact
            emailAddr
            codAmount.
    END. /* each ttblups */
    OUTPUT CLOSE.

END PROCEDURE.

/* ************************  Function Implementations ***************** */

FUNCTION fLogMsg RETURNS CHARACTER 
    (INPUT ipcMessage AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cResult AS CHARACTER NO-UNDO.
    IF lUseLogs THEN 
    DO:
        OUTPUT STREAM sDebug CLOSE. 
        OUTPUT STREAM sDebug TO VALUE(cDebugLog) append.
        PUT STREAM sDebug UNFORMATTED ipcMessage SKIP.
    END.
    RETURN cResult.
END FUNCTION.

FUNCTION fTabChr RETURNS CHARACTER 
    (ipValue AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	
    RETURN IF ipValue NE '' THEN '~t' ELSE ''.
END FUNCTION.


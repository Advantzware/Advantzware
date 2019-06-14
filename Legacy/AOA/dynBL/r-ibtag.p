/*------------------------------------------------------------------------
  File:         r-ibtag.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 6.6.2019
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

DEFINE TEMP-TABLE ttTempTable NO-UNDO
    FIELD warehouse    AS CHARACTER FORMAT "x(5)"             LABEL "Whse"
    FIELD itemNo       AS CHARACTER FORMAT "x(15)"            LABEL "FG Item"
    FIELD descrip      AS CHARACTER FORMAT "x(30)"            LABEL "Description"
    FIELD bin          AS CHARACTER FORMAT "x(8)"             LABEL "Bin"
    FIELD tag          AS CHARACTER FORMAT "x(22)"            LABEL "Tag"
    FIELD rolls        AS INTEGER   FORMAT "->,>>>,>>9"       LABEL "Rolls"
    FIELD lastTrans    AS DATE      FORMAT "99/99/9999"       LABEL "Last Trans"
    FIELD qty          AS DECIMAL   FORMAT "->>>,>>>,>>9.999" LABEL "Quantity"
    FIELD unitCost     AS DECIMAL   FORMAT ">,>>>,>>9.99<<<<" LABEL "Unit Cost"
    FIELD costValue    AS DECIMAL   FORMAT "->>>,>>>,>>9.99"  LABEL "Cost Value"
    FIELD msf          AS DECIMAL   FORMAT "->>>,>>9.99"      LABEL "MSF"
    FIELD tons         AS DECIMAL   FORMAT "->>>,>>9.99"      LABEL "Tons"
    FIELD costMSF      AS DECIMAL   FORMAT "->>>,>>>,>>9.99"  LABEL "Cost/MSF"
    FIELD vendorTag    AS CHARACTER FORMAT "x(8)"             LABEL "Vendor Tag"
    FIELD vendorPO     AS CHARACTER FORMAT "x(10)"            LABEL "Vendor PO"
    FIELD certLotMill  AS CHARACTER FORMAT "x(30)"            LABEL "Cert/Lot/Mill"
    FIELD vendor       AS CHARACTER FORMAT "x(10)"            LABEL "Vendor"
    FIELD lastReceived AS DATE      FORMAT "99/99/9999"       LABEL "Last Recd"
    FIELD caliper      AS DECIMAL   FORMAT "9.99999"          LABEL "Caliper"
    FIELD weightMSF    AS DECIMAL   FORMAT ">>9.99"           LABEL "Wt/MSF"
    FIELD poGLAccount  AS CHARACTER FORMAT "x(25)"            LABEL "PO GL Account"
    FIELD itemName     AS CHARACTER FORMAT "x(30)"            LABEL "Item Name"
    FIELD job          AS CHARACTER FORMAT "x(10)"            LABEL "Job"
    FIELD wid          AS DECIMAL   FORMAT ">,>>99.999"       LABEL "Width"
    FIELD len          AS DECIMAL   FORMAT ">,>>99.999"       LABEL "Length"
    FIELD depth        AS DECIMAL   FORMAT ">,>>99.999"       LABEL "Depth"
    FIELD rollWid      AS DECIMAL   FORMAT ">,>>99.999"       LABEL "Roll WID"
    FIELD sheetSize    AS CHARACTER FORMAT "x(20)"            LABEL "Sheet Size"
    FIELD addr         AS CHARACTER FORMAT "x(30)"            LABEL "Adders"
    FIELD firstRouting AS CHARACTER FORMAT "x(6)"             LABEL "Routing"
    .
DEFINE TEMP-TABLE ttRM-Bin NO-UNDO LIKE rm-bin
    FIELD trans-date LIKE rm-rcpth.trans-date
    FIELD tag2       LIKE rm-rdtlh.tag2
    FIELD po-line      AS INTEGER
    .
/* Local Variable Definitions ---                                       */

&Scoped-define subjectID 5
{AOA/includes/subjectID{&subjectID}Defs.i}

DEFINE NEW SHARED VARIABLE cocode AS CHARACTER NO-UNDO.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE cJobNo         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dItemRolls     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE dtFirstDate    AS DATE      NO-UNDO.
    DEFINE VARIABLE dtLastDate     AS DATE      NO-UNDO.
    DEFINE VARIABLE dRollMultipier AS DECIMAL   NO-UNDO DECIMALS 4.
    DEFINE VARIABLE dQty           AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE iBlankNo       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iFrm           AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iItemRolls     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iJobNo2        AS INTEGER   NO-UNDO.
    
    cocode = cCompany.
    FIND FIRST ce-ctrl NO-LOCK WHERE ce-ctrl.company EQ cCompany NO-ERROR.
    FIND FIRST uom NO-LOCK
         WHERE uom.uom EQ "ROLL"
         NO-ERROR.
    IF AVAILABLE uom THEN DO:
        dRollMultipier = uom.mult.
        RELEASE uom.
    END. /* if avail */
    
    FOR EACH item NO-LOCK
        WHERE item.company  EQ cCompany
          AND item.i-no     GE cStartRMItem
          AND item.i-no     LE cEndRMItem
          AND item.i-no     GT ""
          AND item.procat   GE cStartProCat
          AND item.procat   LE cEndProCat
          AND item.mat-type GE cStartMat
          AND item.mat-type LE cEndMat
          AND (item.i-code  EQ cItemCode
           OR cItemCode     EQ "B") 
        :
        RUN pRMBin.
        IF lZeroBalance AND item.q-onh EQ 0 AND
           NOT CAN-FIND(FIRST ttRM-Bin
                        WHERE ttRM-Bin.company EQ item.company
                          AND ttRM-Bin.i-no EQ item.i-no) THEN DO:
            CREATE ttRM-Bin.
            ASSIGN
                ttRM-Bin.company    = item.company
                ttRM-Bin.i-no       = item.i-no
                ttRM-Bin.trans-date = TODAY
                .
            RELEASE ttRM-Bin.
        END. /* if zerobalance */
    END. /* each item */
    FOR EACH ttRM-Bin NO-LOCK
        WHERE ttRM-Bin.loc        GE cStartLoc
          AND ttRM-Bin.loc        LE cEndLoc
          AND ttRM-Bin.trans-date GE dtStartReceiptDate
          AND ttRM-Bin.trans-date LE dtEndReceiptDate
          AND (lZeroBalance       OR ttRM-Bin.qty NE 0),    
        FIRST ITEM NO-LOCK
        WHERE item.company EQ ttRM-Bin.company
          AND item.i-no    EQ ttRM-Bin.i-no
        BREAK BY ttRM-Bin.loc
              BY ttRM-Bin.i-no
              BY ttRM-Bin.loc-bin
              BY ttRM-Bin.tag
        :
        RUN pGetReceiptDates (OUTPUT dtFirstDate, OUTPUT dtLastDate).
        CREATE ttTempTable.
        ASSIGN
            ttTempTable.warehouse    = item.loc
            ttTempTable.itemNo       = item.i-no
            ttTempTable.descrip      = item.i-dscr
            ttTempTable.bin          = ttRM-Bin.loc-bin
            ttTempTable.tag          = ttRM-Bin.tag
            ttTempTable.lastTrans    = dtLastDate
            ttTempTable.qty          = ttRM-Bin.qty
            ttTempTable.unitCost     = IF ce-ctrl.r-cost THEN item.avg-cost ELSE ttRM-Bin.cost
            ttTempTable.vendorPO     = STRING(ttRM-Bin.po-no)
            ttTempTable.certLotMill  = ttRM-Bin.tag2
            ttTempTable.lastReceived = dtFirstDate
            ttTempTable.caliper      = item.cal
            ttTempTable.weightMSF    = item.basis-w
            ttTempTable.itemName     = item.i-name
            .
        ttTempTable.costValue = ttTempTable.qty * ttTempTable.unitCost.
        IF ttRM-Bin.tag NE "" THEN DO:
            FIND FIRST loadTag NO-LOCK
                 WHERE loadTag.company   EQ cCompany
                   AND loadTag.item-type EQ YES /*rm*/
                   AND loadTag.tag-no    EQ ttRM-Bin.tag
                 NO-ERROR.
            IF AVAILABLE loadTag THEN
            ttTempTable.vendorTag = loadTag.misc-char[1].                
        END. /* if tag ne "" */
        IF item.r-wid GT 0 THEN DO:
            dQty = ttRM-Bin.qty.
            IF ttRM-Bin.tag NE "" AND ttRM-Bin.qty NE 0 THEN
            iItemRolls = iItemRolls + 1.
            ELSE DO:
                IF item.cons-uom NE "LF" THEN
                RUN sys/ref/convquom.p (
                    item.cons-uom, "LF", item.basis-w,
                   (IF item.r-wid EQ 0 THEN item.s-len ELSE 12),
                   (IF item.r-wid EQ 0 THEN item.s-wid ELSE item.r-wid),
                    item.s-dep,                    
                    ttRM-Bin.qty,
                    OUTPUT dQty
                    ).
                ELSE dQty = ttRM-Bin.qty.          
                IF item.s-len NE 0 THEN DO:
                    dItemRolls = dQty / item.s-len.
                    {sys/inc/roundup.i dItemRolls}
                    iItemRolls = iItemRolls + dItemRolls.
                END.
                ELSE IF dRollMultipier NE 0 THEN DO:
                    dItemRolls = dQty / dRollMultipier.
                    {sys/inc/roundup.i dItemRolls}
                    iItemRolls = iItemRolls + dItemRolls.
                END. /* if multiplier */
            END. /* else */
        END. /* if r-wid */
        ASSIGN
            ttTempTable.rolls = dItemRolls
            cJobNo   = ""
            iJobNo2  = 0
            iFrm     = 0
            iBlankNo = 0
            .
        FIND FIRST po-ord NO-LOCK
             WHERE po-ord.company EQ ttRM-Bin.company 
               AND po-ord.po-no   EQ ttRM-Bin.po-no
             NO-ERROR.
        IF AVAILABLE po-ord THEN
        ttTempTable.vendor = po-ord.vend-no.    
        IF ttRM-Bin.po-no NE 0 AND AVAILABLE po-ord THEN DO:
            FIND FIRST po-ordl NO-LOCK
                 WHERE po-ordl.company  EQ ttRM-Bin.company 
                   AND po-ordl.po-no    EQ po-ord.po-no
                   AND po-ordl.i-no     EQ ttRM-Bin.i-no
                   AND (po-ordl.LINE    EQ ttRM-Bin.po-line
                    OR ttRM-Bin.po-line EQ 0)
                 NO-ERROR.        
            IF AVAILABLE po-ordl THEN DO:
                ASSIGN
                    ttTempTable.poGLAccount = po-ordl.actnum
                    ttTempTable.job         = IF po-ordl.job-no NE "" THEN 
                                              po-ordl.job-no + "-" + STRING(po-ordl.job-no2)
                                              ELSE ""
                    cJobNo                  = po-ordl.job-no
                    iJobNo2                 = po-ordl.job-no2
                    iFrm                    = po-ordl.s-num
                    iBlankNo                = po-ordl.b-num
                    .
                FIND FIRST job-hdr NO-LOCK
                     WHERE job-hdr.company EQ cCompany
                       AND job-hdr.job-no  EQ po-ordl.job-no
                       AND job-hdr.job-no2 EQ po-ordl.job-no2
                     NO-ERROR.
                IF AVAILABLE job-hdr THEN
                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ cCompany
                      AND job-mat.job     EQ job-hdr.job
                      AND job-mat.job-no  EQ job-hdr.job-no
                      AND job-mat.job-no2 EQ job-hdr.job-no2
                      AND job-mat.frm     EQ job-hdr.frm
                      AND CAN-FIND(FIRST item
                                   WHERE item.company  EQ cCompany
                                     AND item.i-no     EQ job-mat.i-no
                                     AND item.mat-type EQ "A")
                    BREAK BY job-mat.i-no
                    :
                    ttTempTable.addr = ttTempTable.addr + job-mat.i-no + ",".
                END. /* each job-mat */
                ttTempTable.addr = TRIM(ttTempTable.addr,",").
            END. /* if avail */
        END. /* if po-no ne 0 and avail */
        ASSIGN 
            ttTempTable.msf     = IF item.r-wid GT 0 THEN dQty * item.r-wid / 12 / 1000
                                  ELSE ttRM-Bin.qty * item.s-wid * item.s-len / 144 / 1000
            ttTempTable.tons    = ttTempTable.msf * item.basis-w / 2000 /*Lbs*/
            ttTempTable.costMSF = ttRM-Bin.qty * ttTempTable.unitCost / ttTempTable.msf
            . 
        IF ttTempTable.costMSF EQ ? THEN
        ttTempTable.costMSF = 0.
        IF item.i-code EQ "R" THEN DO:
            IF item.industry = "1" THEN
            ASSIGN
                ttTempTable.wid     = item.case-w 
                ttTempTable.len     = item.case-l
                ttTempTable.depth   = item.case-d
                ttTempTable.rollWid = item.r-wid
                .
            ELSE
            ASSIGN
                ttTempTable.wid     = item.s-wid 
                ttTempTable.len     = item.s-len
                ttTempTable.depth   = item.s-dep
                ttTempTable.rollWid = item.r-wid
                .
        END. /* if i-code eq r */
        ELSE DO:
            FIND FIRST job-mat NO-LOCK
                 WHERE job-mat.company  EQ cCompany
                   AND job-mat.job-no   EQ cJobNo
                   AND job-mat.job-no2  EQ iJobNo2
                   AND job-mat.i-no     EQ item.i-no
                   AND job-mat.frm      EQ iFrm
                   AND job-mat.blank-no EQ iBlankNo
                 NO-ERROR.
            IF AVAILABLE job-mat THEN
            ASSIGN
                ttTempTable.wid     = job-mat.wid 
                ttTempTable.len     = job-mat.len
                ttTempTable.depth   = 0
                ttTempTable.rollWid = 0
                .
        END. /* else */
        ttTempTable.sheetSize = TRIM(STRING(ttTempTable.len,">,>>99.99")) + " X "
                              + TRIM(STRING(ttTempTable.wid,">,>>99.99"))
                              .
        IF cJobNo NE "" THEN
        FOR EACH job-mch NO-LOCK
            WHERE job-mch.company  EQ cCompany
              AND job-mch.job-no   EQ cJobNo
              AND job-mch.job-no2  EQ iJobNo2
              AND job-mch.frm      EQ iFrm
              AND job-mch.blank-no EQ iBlankNo
            BY job-mch.line
            :
            ttTempTable.firstRouting = job-mch.m-code.
            LEAVE.
        END. /* each job-mch */
        IF ttTempTable.qty EQ ? THEN
        ttTempTable.qty = 0.
        IF ttTempTable.unitCost EQ ? THEN
        ttTempTable.unitCost = 0.
        IF ttTempTable.costValue EQ ? THEN
        ttTempTable.costValue = 0.
        IF ttTempTable.msf EQ ? THEN
        ttTempTable.msf = 0.
        IF ttTempTable.tons EQ ? THEN
        ttTempTable.tons = 0.
    END. /* each ttRm-Bin */
END PROCEDURE.

PROCEDURE pGetReceiptDates:
    DEFINE OUTPUT PARAMETER opdtFirstDate AS DATE NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtLastDate  AS DATE NO-UNDO.

    DEFINE VARIABLE lReceiptFound  AS LOGICAL   NO-UNDO.

    IF STRING(ttRM-Bin.po-no) NE "0"  THEN DO:
        FOR EACH rm-rcpth  NO-LOCK                            
            WHERE rm-rcpth.company    EQ ttRM-Bin.company
              AND rm-rcpth.i-no       EQ ttRM-Bin.i-no
              AND rm-rcpth.rita-code  NE "S"
              AND (rm-rcpth.po-no     EQ STRING(ttRM-Bin.po-no))
              AND rm-rcpth.trans-date GE dtStartReceiptDate
              AND rm-rcpth.trans-date LE dtEndReceiptDate
            USE-INDEX i-no                                                                                  
            BREAK BY rm-rcpth.trans-date
            DESCENDING
            :
            IF FIRST(rm-rcpth.trans-date) THEN 
            opdtLastDate = rm-rcpth.trans-date.
            IF LAST(rm-rcpth.trans-date) THEN 
            opdtFirstDate = rm-rcpth.trans-date.
        END. /* each rm-rcpth */
    END. /* if po-no ne 0 */
    ELSE IF ttRM-Bin.tag NE "" THEN DO:
        lReceiptFound = NO.
        /* find without transfers */
        FOR EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.company    EQ ttRM-Bin.company
              AND rm-rdtlh.loc        EQ ttRM-Bin.loc
              AND rm-rdtlh.loc-bin    EQ ttRM-Bin.loc-bin
              AND rm-rdtlh.tag        EQ ttRM-Bin.tag
              AND rm-rdtlh.rita-code  NE "S"
              AND rm-rdtlh.rita-code  NE "T"
            USE-INDEX tag,        
            EACH rm-rcpth NO-LOCK 
            WHERE rm-rcpth.r-no       EQ rm-rdtlh.r-no
              AND rm-rcpth.rita-code  EQ rm-rdtlh.rita-code
              AND rm-rcpth.i-no       EQ ttRM-Bin.i-no
              AND rm-rcpth.trans-date GE dtStartReceiptDate
              AND rm-rcpth.trans-date LE dtEndReceiptDate
            USE-INDEX r-no
            BREAK BY rm-rcpth.trans-date
            DESCENDING
            :
            IF FIRST(rm-rcpth.trans-date) THEN
            opdtLastDate = rm-rcpth.trans-date.
            IF LAST(rm-rcpth.trans-date) THEN
            opdtFirstDate = rm-rcpth.trans-date.
            lReceiptFound = TRUE.
        END. /* each rm-rdtlh */
        IF NOT lReceiptFound THEN DO:
            FOR EACH rm-rdtlh NO-LOCK
                WHERE rm-rdtlh.company    EQ ttRM-Bin.company
                  AND rm-rdtlh.tag        EQ ttRM-Bin.tag
                  AND rm-rdtlh.rita-code  NE "S"
                  AND rm-rdtlh.rita-code  NE "T"
                USE-INDEX tag,          
                EACH rm-rcpth NO-LOCK 
                WHERE rm-rcpth.r-no       EQ rm-rdtlh.r-no
                  AND rm-rcpth.rita-code  EQ rm-rdtlh.rita-code
                  AND rm-rcpth.i-no       EQ ttRM-Bin.i-no
                  AND rm-rcpth.trans-date GE dtStartReceiptDate
                  AND rm-rcpth.trans-date LE dtEndReceiptDate
                USE-INDEX r-no
                BREAK BY rm-rcpth.trans-date
                DESCENDING
                :
                IF FIRST(rm-rcpth.trans-date) THEN
                opdtLastDate = rm-rcpth.trans-date.
                IF LAST(rm-rcpth.trans-date) THEN
                opdtFirstDate = rm-rcpth.trans-date.
                lReceiptFound = TRUE.
            END. /* each rm-rdtlh */
        END. /* if not lreceiptfound */
        /* if not found, find with transfers */
        IF NOT lReceiptFound THEN DO:
            FOR EACH rm-rdtlh NO-LOCK
                WHERE rm-rdtlh.company    EQ ttRM-Bin.company
                  AND rm-rdtlh.loc        EQ ttRM-Bin.loc
                  AND rm-rdtlh.loc-bin    EQ ttRM-Bin.loc-bin
                  AND rm-rdtlh.tag        EQ ttRM-Bin.tag
                  AND rm-rdtlh.rita-code  NE "S"
                USE-INDEX tag,            
                EACH rm-rcpth NO-LOCK     
                WHERE rm-rcpth.r-no       EQ rm-rdtlh.r-no
                  AND rm-rcpth.rita-code  EQ rm-rdtlh.rita-code
                  AND rm-rcpth.i-no       EQ ttRM-Bin.i-no
                  AND rm-rcpth.trans-date GE dtStartReceiptDate
                  AND rm-rcpth.trans-date LE dtEndReceiptDate
                USE-INDEX r-no
                BREAK BY rm-rcpth.trans-date
                DESCENDING
                :
                IF FIRST(rm-rcpth.trans-date) THEN
                opdtLastDate = rm-rcpth.trans-date.
                IF LAST(rm-rcpth.trans-date) THEN
                opdtFirstDate = rm-rcpth.trans-date.
            END. /* each rm-rdtlh */
        END. /* if not lreceiptfound */
    END. /* if tag ne "" */
    ELSE DO:
        FOR EACH rm-rcpth NO-LOCK
            WHERE rm-rcpth.company    EQ ttRM-Bin.company
              AND rm-rcpth.i-no       EQ ttRM-Bin.i-no
              AND rm-rcpth.rita-code  NE "S" 
              AND rm-rcpth.trans-date GE dtStartReceiptDate
              AND rm-rcpth.trans-date LE dtEndReceiptDate
            USE-INDEX i-no                                                                                  
            BREAK BY rm-rcpth.trans-date
            DESCENDING
            :
            IF FIRST(rm-rcpth.trans-date) THEN 
            opdtLastDate = rm-rcpth.trans-date.
            IF LAST(rm-rcpth.trans-date) THEN 
            opdtFirstDate = rm-rcpth.trans-date.       
        END. /* each rm-rdtlh */
    END. /* else */
END PROCEDURE.

PROCEDURE pRMBin:
    DEFINE VARIABLE v-r-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-i-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-t-qty AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-qty  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cst  AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE lv-uom  AS CHARACTER NO-UNDO.

    DEFINE BUFFER bRM-Bin FOR rm-bin.

    IF dtAsOfDate GE TODAY THEN
        FOR EACH rm-bin NO-LOCK
            WHERE rm-bin.company EQ item.company
              AND rm-bin.i-no    EQ item.i-no
            :
            CREATE ttRM-Bin.
            BUFFER-COPY rm-bin TO ttRM-Bin.
        END.
    ELSE DO:
        {rm/rmmkbin1.i dtAsOfDate tt}
    END.

    FOR EACH ttRM-Bin
        WHERE ttRM-Bin.company EQ item.company
          AND ttRM-Bin.i-no    EQ item.i-no
        :
        RELEASE rm-rcpth.
        RELEASE rm-rcpth.
        ASSIGN
            ttRM-Bin.trans-date = ?
            ttRM-Bin.tag2       = ""
            .
        IF TRIM(ttRM-Bin.tag) EQ "" THEN
        FOR EACH rm-rcpth NO-LOCK
            WHERE rm-rcpth.company   EQ ttRM-Bin.company
              AND rm-rcpth.i-no      EQ ttRM-Bin.i-no
              AND rm-rcpth.rita-code NE "S"
            USE-INDEX i-no,
            EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.r-no      EQ rm-rcpth.r-no
              AND rm-rdtlh.rita-code EQ rm-rcpth.rita-code
              AND rm-rdtlh.loc       EQ ttRM-Bin.loc
              AND rm-rdtlh.loc-bin   EQ ttRM-Bin.loc-bin
              AND rm-rdtlh.tag       EQ ttRM-Bin.tag
            USE-INDEX rm-rdtl    
            BY rm-rcpth.trans-date
            BY rm-rcpth.r-no
            :
            ASSIGN
                ttRM-Bin.trans-date = rm-rcpth.trans-date
                ttRM-Bin.tag2 = rm-rdtlh.tag2
                .
            IF rm-rcpth.po-no NE "" THEN
            ASSIGN
                ttRM-Bin.po-no   = INTEGER(rm-rcpth.po-no )
                ttRM-Bin.po-line = rm-rcpth.po-line
                .
            LEAVE.
        END. /* each rm-rcpth */
        ELSE
        FOR EACH rm-rdtlh NO-LOCK
            WHERE rm-rdtlh.company   EQ ttRM-Bin.company
              AND rm-rdtlh.loc       EQ ttRM-Bin.loc
              AND rm-rdtlh.loc-bin   EQ ttRM-Bin.loc-bin
              AND rm-rdtlh.tag       EQ ttRM-Bin.tag
              AND rm-rdtlh.rita-code NE "S"
            USE-INDEX tag,    
            EACH rm-rcpth NO-LOCK 
            WHERE rm-rcpth.r-no      EQ rm-rdtlh.r-no
              AND rm-rcpth.rita-code EQ rm-rdtlh.rita-code
              AND rm-rcpth.i-no      EQ ITEM.i-no
            USE-INDEX r-no
            BY rm-rcpth.trans-date
            BY rm-rcpth.r-no
            :
            ASSIGN
                ttRM-Bin.trans-date = rm-rcpth.trans-date
                ttRM-Bin.tag2       = rm-rdtlh.tag2
                .
            IF rm-rcpth.po-no NE "" THEN
            ASSIGN
                ttRM-Bin.po-no   = INTEGER(rm-rcpth.po-no) 
                ttRM-Bin.po-line = rm-rcpth.po-line
                .
            LEAVE.
        END. /* each rm-rdtlh */
        IF ttRM-Bin.trans-date EQ ? THEN DO:
            FIND FIRST rm-bin NO-LOCK
                WHERE rm-bin.company EQ ttRM-Bin.company
                  AND rm-bin.i-no    EQ ttRM-Bin.i-no
                  AND rm-bin.loc     EQ ttRM-Bin.loc
                  AND rm-bin.loc-bin EQ ttRM-Bin.loc-bin
                  AND rm-bin.tag     EQ ttRM-Bin.tag
                USE-INDEX loc-bin NO-ERROR.
            ttRM-Bin.trans-date = IF fg-bin.rec_key BEGINS "2" THEN
                     DATE(SUBSTRING(fg-bin.rec_key,5,4) + SUBSTRING(fg-bin.rec_key,1,4))
                ELSE DATE(SUBSTRING(rm-bin.rec_key,1,8)) NO-ERROR.
            IF ERROR-STATUS:ERROR THEN
            ttRM-Bin.trans-date = TODAY.
        END. /* if trans-date */
    END. /* each ttrm-bin */
END PROCEDURE.

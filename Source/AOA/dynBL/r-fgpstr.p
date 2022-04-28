/*------------------------------------------------------------------------
  File:         r-fgpstr.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 5.12.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */
&Scoped-define ttTempTable ttFGPostHist
DEFINE TEMP-TABLE ttFGPostHist NO-UNDO
    FIELD transDate   AS DATE      FORMAT "99/99/9999"         LABEL "Date"       
    FIELD itemNo      AS CHARACTER FORMAT "x(15)"              LABEL "Item"       
    FIELD itemName    AS CHARACTER FORMAT "x(30)"              LABEL "Description"
    FIELD poNo        AS CHARACTER FORMAT "x(9)"               LABEL "PO"         
    FIELD vendorNo    AS CHARACTER FORMAT "x(8)"               LABEL "Vendor"     
    FIELD v-tran-type AS CHARACTER FORMAT "x"                  LABEL "Transaction Type"
    FIELD v-tag       AS CHARACTER FORMAT "x(9)"               LABEL "Tag"
    FIELD v-cases     AS INTEGER   FORMAT "->>>,>>9"           LABEL "Units"
    FIELD v-qty-case  AS INTEGER   FORMAT ">>>,>>9"            LABEL "Count"
    FIELD bin         AS CHARACTER FORMAT "x(8)"               LABEL "Bin"          
    FIELD lv-cost-uom AS CHARACTER FORMAT "x(3)"               LABEL "CUOM"
    FIELD v-fg-qty    AS INTEGER   FORMAT "->>>,>>>,>>9"       LABEL "Total QTY"
    FIELD v-fg-cost   AS DECIMAL   FORMAT "->>>,>>>,>>9.99"    LABEL "Total Cost"
    FIELD v-fg-value  AS DECIMAL   FORMAT "->>>,>>>,>>9.99"    LABEL "Tot Sell Val"
    FIELD jobNo       AS CHARACTER FORMAT "x(9)"               LABEL "Job"        
    FIELD v-rfid#     AS CHARACTER FORMAT "x(10)"              LABEL "RFID"
    FIELD partNo      AS CHARACTER FORMAT "x(12)"              LABEL "Customer Part"
    FIELD dieNo       AS CHARACTER FORMAT "x(15)"              LABEL "Die"          
    FIELD v-numUp     AS INTEGER   FORMAT ">>9"                LABEL "UP"
    FIELD cadNo       AS CHARACTER FORMAT "x(12)"              LABEL "CAD"          
    FIELD plateNo     AS CHARACTER FORMAT "x(12)"              LABEL "Plate"        
    FIELD v-numColors AS INTEGER   FORMAT ">9"                 LABEL "Num of Colors"
    FIELD v-SheetSize AS CHARACTER FORMAT "x(15)"              LABEL "Sheet Size"
    FIELD v-Caliper   AS CHARACTER FORMAT "x(7)"               LABEL "Caliper"
    FIELD user-id     AS CHARACTER FORMAT "x(10)"              LABEL "User ID"      
    FIELD loc         AS CHARACTER FORMAT "x(5)"               LABEL "Whse"
    FIELD wt-h        AS DECIMAL   FORMAT ">>,>>9.99"          LABEL "WT/100"
    FIELD rec-time    AS CHARACTER FORMAT "x(5)"               LABEL "Rec Time"
    FIELD postDate    AS DATE      FORMAT "99/99/9999"         LABEL "Posted"       
    FIELD ProdCat     AS CHARACTER FORMAT "x(5)"               LABEL "Catgy"        
    FIELD unt-cst     AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Unit Cost"
    FIELD unt-sel     AS DECIMAL   FORMAT "->>,>>>,>>9.99<<<<" LABEL "Unit Sell"
    FIELD suom        AS CHARACTER FORMAT "x(5)"               LABEL "SUOM"
    FIELD prom-date   AS DATE      FORMAT "99/99/9999"         LABEL "Promise Date"
    FIELD due-date    AS DATE      FORMAT "99/99/9999"         LABEL "Ord Due Date"
    FIELD job-start   AS DATE      FORMAT "99/99/9999"         LABEL "Start Date"
    FIELD shipto      AS CHARACTER FORMAT "x(8)"               LABEL "Ship To"             
    FIELD shipname    AS CHARACTER FORMAT "x(30)"              LABEL "Ship To Name"        
    FIELD order-no    AS INTEGER   FORMAT ">>>>>>9"            LABEL "Order"
    FIELD bef-qty     AS INTEGER   FORMAT "->>>>>>>>9"         LABEL "Before Qty"
    FIELD bin-qty     AS INTEGER   FORMAT "->>>>>>>>9"         LABEL "Bin Change"
    FIELD bol-no      AS INTEGER   FORMAT ">>>>>>9"            LABEL "BOL"
    FIELD Reason      AS CHARACTER FORMAT "x(30)"              LABEL "Reason"            
    FIELD Reason-cd   AS CHARACTER FORMAT "x(2)"               LABEL "Reason Code"       
    FIELD Reason-dscr AS CHARACTER FORMAT "x(25)"              LABEL "Reason Description"
    FIELD custName    AS CHARACTER FORMAT "x(30)"              LABEL "Customer Name"
    FIELD item-mat-cost     AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Item Mat Cost"
    FIELD item-dl-cost      AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Item DL Cost"
    FIELD item-voh-cost     AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Item VOH Cost"
    FIELD item-foh-cost     AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Item FOH Cost"
    FIELD bin-mat-cost      AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Bin Mat Cost"
    FIELD bin-dl-cost       AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Bin DL Cost"
    FIELD bin-voh-cost      AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Bin VOH Cost"
    FIELD bin-foh-cost      AS DECIMAL   FORMAT "->>>,>>>,>>9.99<<"  LABEL "Bin FOH Cost"
    .
DEFINE TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD rec-id2 AS RECID
    FIELD rptDate AS DATE
    INDEX rec-id2 rec-id2
    .
{sys/ref/CustList.i NEW}

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 174
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

/* **********************  Internal Procedures  *********************** */

PROCEDURE calc-case-and-tag:
    DEFINE INPUT PARAMETER ipr-fg-rcpth AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER ipr-fg-rdtlh AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER v-fg-qty     AS INTEGER NO-UNDO.

    DEFINE OUTPUT PARAMETER opv-cases      AS INTEGER           NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-qty-case LIKE fg-bin.case-count NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-tag      LIKE fg-rdtlh.tag      NO-UNDO.

    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.

    FIND bf-fg-rcpth NO-LOCK WHERE ROWID(bf-fg-rcpth) EQ ipr-fg-rcpth NO-ERROR.
    FIND bf-fg-rdtlh NO-LOCK WHERE ROWID(bf-fg-rdtlh) EQ ipr-fg-rdtlh NO-ERROR.
    IF NOT AVAILABLE bf-fg-rcpth THEN
        RETURN.
    IF NOT AVAILABLE bf-fg-rdtlh THEN
        RETURN.

    IF bf-fg-rdtlh.qty-case EQ 0 THEN DO:
    FIND FIRST fg-bin NO-LOCK
         WHERE fg-bin.company EQ bf-fg-rcpth.company
           AND fg-bin.job-no  EQ bf-fg-rcpth.job-no
           AND fg-bin.job-no2 EQ bf-fg-rcpth.job-no2
           AND fg-bin.i-no    EQ bf-fg-rcpth.i-no
           AND fg-bin.loc     EQ bf-fg-rdtlh.loc
           AND fg-bin.loc-bin EQ bf-fg-rdtlh.loc-bin
           AND fg-bin.tag     EQ bf-fg-rdtlh.tag
         USE-INDEX job
         NO-ERROR. 
        IF AVAILABLE fg-bin THEN
        ASSIGN
            opv-cases    = trunc((v-fg-qty / fg-bin.case-count),0)
            opv-qty-case = fg-bin.case-count
            .
        ELSE DO:
            FIND FIRST itemfg NO-LOCK
                 WHERE itemfg.company EQ cCompany
                   AND itemfg.i-no    EQ bf-fg-rcpth.i-no
                 NO-ERROR.
            IF AVAILABLE itemfg THEN
            ASSIGN
                opv-cases    = trunc((v-fg-qty / itemfg.case-count),0)
                opv-qty-case = itemfg.case-count
                .
        END.
    END.
    ELSE
    ASSIGN
        opv-cases    = bf-fg-rdtlh.cases
        opv-qty-case = bf-fg-rdtlh.qty-case
        .             
    opv-tag = IF SUBSTRING(bf-fg-rdtlh.tag,1,15) EQ bf-fg-rcpth.i-no
            THEN SUBSTRING(bf-fg-rdtlh.tag,16,8) ELSE bf-fg-rdtlh.tag.

END PROCEDURE.

PROCEDURE calc-fg-value:
    DEFINE INPUT  PARAMETER ipv-sell-price AS DECIMAL   NO-UNDO.
    DEFINE INPUT  PARAMETER ipv-sell-uom   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipr-fg-rdtlh   AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-fg-value   AS DECIMAL   NO-UNDO.

    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.

    FIND FIRST bf-fg-rdtlh NO-LOCK
         WHERE ROWID(bf-fg-rdtlh) = ipr-fg-rdtlh
         NO-ERROR.
    IF NOT AVAILABLE bf-fg-rdtlh THEN
        RETURN.

    IF ipv-sell-uom = "L" THEN
    opv-fg-value = ipv-sell-price. /*  * bf-fg-rdtlh.qty.*/
    ELSE
    IF ipv-sell-uom = "CS" THEN
        opv-fg-value = ipv-sell-price * (bf-fg-rdtlh.qty / bf-fg-rdtlh.qty-case).
    ELSE IF ipv-sell-uom = "M" THEN
        opv-fg-value = ipv-sell-price * (bf-fg-rdtlh.qty / 1000).
    ELSE DO:
        FIND FIRST uom NO-LOCK
             WHERE uom.uom  EQ ipv-sell-uom
               AND uom.mult NE 0
             NO-ERROR.
        IF AVAILABLE uom THEN
        opv-fg-value = ipv-sell-price * (bf-fg-rdtlh.qty / uom.mult).
    END.

END PROCEDURE.

PROCEDURE calc-msf-for-r:
    DEFINE INPUT PARAMETER ipr-fg-rcpth      AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER ipr-fg-rdtlh      AS ROWID   NO-UNDO.
    DEFINE INPUT PARAMETER ipr-lastof-key-02 AS LOGICAL NO-UNDO.
    DEFINE INPUT PARAMETER ipv-corr          AS LOGICAL NO-UNDO.

    DEFINE OUTPUT PARAMETER opv-on         AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-qty-pallet AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-msf-1      AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-msf-2      AS DECIMAL NO-UNDO.

    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.

    FIND FIRST bf-fg-rcpth NO-LOCK
         WHERE ROWID(bf-fg-rcpth) EQ ipr-fg-rcpth
         NO-ERROR.
    IF NOT AVAILABLE bf-fg-rcpth THEN
        RETURN.
    FIND FIRST bf-fg-rdtlh NO-LOCK
         WHERE ROWID(bf-fg-rdtlh) EQ ipr-fg-rdtlh
         NO-ERROR.
    IF NOT AVAILABLE bf-fg-rdtlh THEN
        RETURN.

    opv-on = 1.
    FIND FIRST job-hdr NO-LOCK
         WHERE job-hdr.company EQ cCompany
           AND job-hdr.job-no  EQ bf-fg-rcpth.job-no
           AND job-hdr.job-no2 EQ bf-fg-rcpth.job-no2
           AND job-hdr.i-no    EQ bf-fg-rcpth.i-no
         USE-INDEX job-no
         NO-ERROR.
    /* For calculating the quantity per pallet. */
    IF AVAILABLE job-hdr THEN DO:
        FIND FIRST fg-bin NO-LOCK
             WHERE fg-bin.company EQ cCompany
               AND fg-bin.job-no  EQ job-hdr.job-no
               AND fg-bin.job-no2 EQ job-hdr.job-no2
               AND fg-bin.i-no    EQ job-hdr.i-no
               AND fg-bin.loc-bin EQ bf-fg-rdtlh.loc-bin
               AND fg-bin.tag     EQ bf-fg-rdtlh.tag
            NO-ERROR.
        opv-qty-pallet = bf-fg-rdtlh.cases * IF AVAILABLE fg-bin THEN
                         fg-bin.cases-unit ELSE 1.
    END.
    IF AVAILABLE job-hdr AND job-hdr.est-no = "" THEN DO:
        RELEASE ef.
        RUN sys/inc/numup.p (job-hdr.company, job-hdr.est-no, job-hdr.frm, OUTPUT opv-on).
        FIND FIRST ef NO-LOCK
             WHERE ef.company   EQ job-hdr.company
               AND ef.est-no    EQ job-hdr.est-no
               AND ef.form-no   EQ job-hdr.frm
             NO-ERROR.
        IF AVAILABLE ef THEN
        RUN est/ef-#out.p (ROWID(ef), OUTPUT opv-on).
        IF ipr-lastof-key-02 THEN           
        FOR EACH mch-act FIELDS(waste) NO-LOCK
            WHERE mch-act.company  EQ cCompany
              AND mch-act.job      EQ job-hdr.job
              AND mch-act.job-no   EQ job-hdr.job-no
              AND mch-act.job-no2  EQ job-hdr.job-no2
              AND mch-act.frm      EQ job-hdr.frm
            USE-INDEX job
            :
            opv-msf-2 = opv-msf-2 + (mch-act.waste * job-hdr.sq-in / 100).
        END.
        FOR EACH job-mat NO-LOCK
            WHERE job-mat.company EQ cCompany
              AND job-mat.job     EQ job-hdr.job
              AND job-mat.job-no  EQ job-hdr.job-no
              AND job-mat.job-no2 EQ job-hdr.job-no2
              AND job-mat.frm     EQ job-hdr.frm,
            FIRST item NO-LOCK
            WHERE item.company    EQ cCompany
              AND item.i-no       EQ job-mat.i-no
              AND item.mat-type   EQ "B"
            :
            LEAVE.
        END.
        IF AVAILABLE job-mat THEN DO:
            ASSIGN
                opv-msf-1 = bf-fg-rdtlh.qty / opv-on * (job-mat.len * job-mat.wid)
                opv-msf-2 = opv-msf-2      / opv-on * (job-mat.len * job-mat.wid)
                .
            IF ipv-corr THEN
            ASSIGN
                opv-msf-1 = opv-msf-1 * .007
                opv-msf-2 = opv-msf-2 * .007
                .
            ELSE
            ASSIGN
                opv-msf-1 = opv-msf-1 / 144
                opv-msf-2 = opv-msf-2 / 144
                .
        END.
    END.

END PROCEDURE.

PROCEDURE calc-sell-price:
    DEFINE INPUT  PARAMETER ipr-fg-rcpth   AS ROWID            NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-sell-price LIKE oe-ordl.price  NO-UNDO.
    DEFINE OUTPUT PARAMETER opv-sell-uom   LIKE oe-ordl.pr-uom NO-UNDO.

    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-itemfg   FOR itemfg.

    FIND FIRST bf-fg-rcpth NO-LOCK
         WHERE ROWID(bf-fg-rcpth) EQ ipr-fg-rcpth
         NO-ERROR.
    IF NOT AVAILABLE bf-fg-rcpth THEN
        RETURN.

    /* If there is a job number for receipt... */
    IF TRIM(bf-fg-rcpth.job-no) NE "" THEN
    FOR EACH job-hdr FIELDS(company ord-no i-no job-no job-no2) NO-LOCK
        WHERE job-hdr.company EQ bf-fg-rcpth.company
          AND job-hdr.job-no  EQ bf-fg-rcpth.job-no
          AND job-hdr.job-no2 EQ bf-fg-rcpth.job-no2
          AND job-hdr.i-no    EQ bf-fg-rcpth.i-no
          AND job-hdr.ord-no  NE 0
        USE-INDEX job-no,
        FIRST oe-ordl FIELDS(price pr-uom) NO-LOCK
        WHERE oe-ordl.company EQ job-hdr.company
          AND oe-ordl.ord-no  EQ job-hdr.ord-no
          AND oe-ordl.i-no    EQ job-hdr.i-no
          AND oe-ordl.job-no  EQ job-hdr.job-no
          AND oe-ordl.job-no2 EQ job-hdr.job-no2
          AND (oe-ordl.pr-uom NE "CS"
           OR oe-ordl.cas-cnt NE 0)
        USE-INDEX item-ord
        BY job-hdr.ord-no DESCENDING
        :
        ASSIGN
            opv-sell-price = oe-ordl.price
            opv-sell-uom   = oe-ordl.pr-uom
            .
        LEAVE.
    END.
    /* Else if there is a PO number for receipt... */
    ELSE
    IF INT(bf-fg-rcpth.po-no) NE 0 THEN
    FOR EACH po-ordl FIELDS(company ord-no i-no) NO-LOCK
        WHERE po-ordl.company EQ bf-fg-rcpth.company
          AND po-ordl.po-no   EQ INTEGER(bf-fg-rcpth.po-no)
          AND po-ordl.i-no    EQ bf-fg-rcpth.i-no
          AND po-ordl.ord-no  NE 0
        USE-INDEX item-ordno,
        FIRST oe-ordl FIELDS(price pr-uom) NO-LOCK
        WHERE oe-ordl.company EQ po-ordl.company
          AND oe-ordl.ord-no  EQ po-ordl.ord-no
          AND oe-ordl.i-no    EQ po-ordl.i-no
          AND (oe-ordl.pr-uom NE "CS"
           OR oe-ordl.cas-cnt NE 0)
        USE-INDEX item-ord
        BY po-ordl.ord-no DESCENDING
        :
        ASSIGN
            opv-sell-price = oe-ordl.price
            opv-sell-uom   = oe-ordl.pr-uom
            .
        LEAVE.
    END.
    IF opv-sell-price EQ 0 THEN DO:
        FIND FIRST bf-itemfg NO-LOCK
             WHERE bf-itemfg.company EQ bf-fg-rcpth.company
               AND bf-itemfg.i-no    EQ bf-fg-rcpth.i-no
             NO-ERROR.
        IF AVAILABLE bf-itemfg THEN
        ASSIGN
            opv-sell-price = bf-itemfg.sell-price
            opv-sell-uom   = bf-itemfg.sell-uom
            .
    END.

END PROCEDURE.

PROCEDURE create-tt-report:
    DEFINE VARIABLE cItemNo LIKE fg-rcpth.i-no NO-UNDO.
    DEFINE VARIABLE cType     AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE cTypes    AS CHARACTER     NO-UNDO.
    DEFINE VARIABLE dtDate    AS DATE          NO-UNDO.
    DEFINE VARIABLE idx       AS INTEGER       NO-UNDO.

    EMPTY TEMP-TABLE tt-report.

    cTypes = TRIM(STRING(lReceipts,"R/"))    + TRIM(STRING(lCreditRefunds,"E/"))
            + TRIM(STRING(lTransfers,"T/"))   + TRIM(STRING(lShipments,"S/"))
            + TRIM(STRING(lAdjustments,"A/")) + TRIM(STRING(lCycleCount,"C/"))
            .
    IF NOT (cStartFGItem EQ "" AND cEndFGItem EQ CHR(254)) THEN DO:
        FOR EACH fg-rcpth NO-LOCK
            WHERE fg-rcpth.company EQ cCompany
              AND fg-rcpth.i-no    GE cStartFGItem
              AND fg-rcpth.i-no    LE cEndFGItem,
            FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cCompany
              AND itemfg.i-no    EQ fg-rcpth.i-no
              AND itemfg.cust-no GE cStartCustNo
              AND itemfg.cust-no LE cEndCustNo
              AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
                                              WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                AND ttCustList.log-fld EQ TRUE) ELSE TRUE)
            :
            LEAVE.
        END.
        DO WHILE AVAILABLE fg-rcpth:
            cItemNo = fg-rcpth.i-no.
            /* Create tt-report file for History Records */
            DO idx = 1 TO LENGTH(TRIM(cTypes)):
                IF INDEX("RSTAEC",SUBSTRING(cTypes,idx,1)) GT 0 THEN DO:
                    cType = SUBSTRING(cTypes,idx,1).
                    FOR EACH fg-rcpth NO-LOCK
                        WHERE fg-rcpth.company     EQ cCompany
                          AND fg-rcpth.i-no        EQ cItemNo
                          AND fg-rcpth.rita-code   EQ cType
                          AND ((fg-rcpth.post-date GE dtStartDate
                          AND fg-rcpth.post-date   LE dtEndDate
                          AND fg-rcpth.post-date   NE ?)
                           OR (fg-rcpth.trans-date GE dtStartDate
                          AND fg-rcpth.trans-date  LE dtEndDate
                          AND fg-rcpth.post-date   EQ ?))
                          AND fg-rcpth.USER-ID     GE cStartUserID
                          AND fg-rcpth.USER-ID     LE cEndUserID
                        USE-INDEX i-no,
                        EACH fg-rdtlh NO-LOCK
                        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code,
                        FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ cCompany
                          AND itemfg.i-no    EQ fg-rcpth.i-no
                          AND itemfg.cust-no GE cStartCustNo
                          AND itemfg.cust-no LE cEndCustNo
                          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
                                                          WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                            AND ttCustList.log-fld EQ TRUE) ELSE TRUE)
                        :
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = ""
                            tt-report.key-01  = fg-rdtlh.loc
                            tt-report.key-02  = fg-rcpth.i-no
                            tt-report.key-03  = fg-rdtlh.loc-bin
                            tt-report.key-04  = fg-rdtlh.tag
                            tt-report.rec-id  = RECID(fg-rdtlh)
                            tt-report.rptDate = fg-rcpth.trans-date
                            .
                    END.
                END.
            END.
            FOR EACH fg-rcpth NO-LOCK
                WHERE fg-rcpth.company EQ cCompany
                  AND fg-rcpth.i-no    GT cItemNo
                  AND fg-rcpth.i-no    GE cStartFGItem
                  AND fg-rcpth.i-no    LE cEndFGItem,
                FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ cCompany
                  AND itemfg.i-no    EQ fg-rcpth.i-no
                  AND itemfg.cust-no GE cStartCustNo
                  AND itemfg.cust-no LE cEndCustNo
                  AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
                                                  WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                    AND ttCustList.log-fld EQ TRUE) ELSE TRUE)
                :
                LEAVE.
            END.
        END.
    END. /*cStartFGItem eq blank*/
    ELSE
    DO:
        DO idx = 1 TO LENGTH(TRIM(cTypes)):
            IF INDEX("RSTAEC",SUBSTRING(cTypes,idx,1)) GT 0 THEN DO:
                cType = SUBSTRING(cTypes,idx,1).
                IF NOT (cStartCustNo EQ "" AND cEndCustNo EQ CHR(254)) THEN
                DO dtDate = dtStartDate TO dtEndDate:
                    FOR EACH fg-rcpth FIELDS(r-no rita-code i-no trans-date) NO-LOCK 
                        WHERE fg-rcpth.company   EQ cCompany
                          AND fg-rcpth.rita-code EQ cType
                          AND fg-rcpth.post-date EQ dtDate
                          AND fg-rcpth.USER-ID   GE cStartUserID
                          AND fg-rcpth.USER-ID   LE cEndUserID
                        USE-INDEX post-date,
                        EACH fg-rdtlh FIELDS(loc loc-bin tag) NO-LOCK
                        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                        :                  
                        IF CAN-FIND(FIRST itemfg
                                    WHERE itemfg.company EQ cCompany
                                      AND itemfg.i-no    EQ fg-rcpth.i-no
                                      AND itemfg.cust-no GE cStartCustNo
                                      AND itemfg.cust-no LE cEndCustNo
                                      AND (IF lCustList THEN CAN-FIND(FIRST ttCustList 
                                                                      WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                                        AND ttCustList.log-fld EQ TRUE) ELSE TRUE)) THEN DO:
                            CREATE tt-report.
                            ASSIGN
                                tt-report.term-id = ""
                                tt-report.key-01  = fg-rdtlh.loc
                                tt-report.key-02  = fg-rcpth.i-no
                                tt-report.key-03  = fg-rdtlh.loc-bin
                                tt-report.key-04  = fg-rdtlh.tag
                                tt-report.rec-id  = RECID(fg-rdtlh)
                                tt-report.rec-id2 = RECID(fg-rcpth)
                                tt-report.rptDate = fg-rcpth.trans-date
                                .
                            RELEASE tt-report.
                        END.
                    END.               
                    FOR EACH fg-rcpth FIELDS(r-no rita-code i-no trans-date) NO-LOCK
                        WHERE fg-rcpth.company    EQ cCompany 
                          AND fg-rcpth.rita-code  EQ cType 
                          AND fg-rcpth.post-date  EQ ? 
                          AND fg-rcpth.trans-date EQ dtDate 
                          AND fg-rcpth.user-id    GE cStartUserID
                          AND fg-rcpth.user-id    LE cEndUserID
                        USE-INDEX post-date,                  
                        EACH fg-rdtlh FIELDS(loc loc-bin tag) NO-LOCK
                        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                        :                  
                        IF NOT CAN-FIND(FIRST tt-report 
                                        WHERE tt-report.rec-id2 EQ recid(fg-rcpth))
                                          AND CAN-FIND(FIRST itemfg WHERE itemfg.company EQ cCompany
                                                                      AND itemfg.i-no    EQ fg-rcpth.i-no
                                                                      AND itemfg.cust-no GE cStartCustNo
                                                                      AND itemfg.cust-no LE cEndCustNo
                                          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
                                                                          WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                                            AND ttCustList.log-fld EQ TRUE) ELSE TRUE)) THEN DO:
                            CREATE tt-report.
                            ASSIGN
                                tt-report.term-id = ""
                                tt-report.key-01  = fg-rdtlh.loc
                                tt-report.key-02  = fg-rcpth.i-no
                                tt-report.key-03  = fg-rdtlh.loc-bin
                                tt-report.key-04  = fg-rdtlh.tag
                                tt-report.rec-id  = RECID(fg-rdtlh)
                                tt-report.rptDate = fg-rcpth.trans-date
                                . 
                            RELEASE tt-report.
                        END.
                    END.
                END. /*dtDate loop*/
                ELSE
                DO dtDate = dtStartDate TO dtEndDate:
                    FOR EACH fg-rcpth FIELDS(r-no rita-code i-no trans-date) NO-LOCK
                        WHERE fg-rcpth.company   EQ cCompany
                          AND fg-rcpth.rita-code EQ cType
                          AND fg-rcpth.post-date EQ dtDate
                        USE-INDEX post-date,
                        EACH fg-rdtlh FIELDS(loc loc-bin tag) NO-LOCK
                        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                          AND fg-rcpth.USER-ID   GE cStartUserID
                          AND fg-rcpth.USER-ID   LE cEndUserID,
                        FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ cCompany
                          AND itemfg.i-no    EQ fg-rcpth.i-no
                          AND itemfg.cust-no GE cStartCustNo
                          AND itemfg.cust-no LE cEndCustNo
                          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
                                                          WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                            AND ttCustList.log-fld EQ TRUE) ELSE TRUE)
                        :
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = ""
                            tt-report.key-01  = fg-rdtlh.loc
                            tt-report.key-02  = fg-rcpth.i-no
                            tt-report.key-03  = fg-rdtlh.loc-bin
                            tt-report.key-04  = fg-rdtlh.tag
                            tt-report.rec-id  = RECID(fg-rdtlh)
                            tt-report.rec-id2 = RECID(fg-rcpth)
                            tt-report.rptDate = fg-rcpth.trans-date
                            .
                        RELEASE tt-report.
                    END.
                    FOR EACH fg-rcpth FIELDS(r-no rita-code i-no trans-date) NO-LOCK
                        WHERE fg-rcpth.company    EQ cCompany
                          AND fg-rcpth.rita-code  EQ cType
                          AND fg-rcpth.post-date  EQ ?
                          AND fg-rcpth.trans-date EQ dtDate
                          AND NOT CAN-FIND(FIRST tt-report
                                           WHERE tt-report.rec-id2 EQ RECID(fg-rcpth))
                                             AND fg-rcpth.USER-ID  GE cStartUserID
                                             AND fg-rcpth.USER-ID  LE cEndUserID
                        USE-INDEX post-date,
                        EACH fg-rdtlh FIELDS(loc loc-bin tag) NO-LOCK
                        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code,
                        FIRST itemfg NO-LOCK
                        WHERE itemfg.company EQ cCompany
                          AND itemfg.i-no    EQ fg-rcpth.i-no
                          AND itemfg.cust-no GE cStartCustNo
                          AND itemfg.cust-no LE cEndCustNo
                          AND (IF lCustList THEN CAN-FIND(FIRST ttCustList
                                                          WHERE ttCustList.cust-no EQ itemfg.cust-no
                                                            AND ttCustList.log-fld EQ TRUE) ELSE TRUE)
                        :
                        CREATE tt-report.
                        ASSIGN
                            tt-report.term-id = ""
                            tt-report.key-01  = fg-rdtlh.loc
                            tt-report.key-02  = fg-rcpth.i-no
                            tt-report.key-03  = fg-rdtlh.loc-bin
                            tt-report.key-04  = fg-rdtlh.tag
                            tt-report.rec-id  = RECID(fg-rdtlh)
                            tt-report.rptDate = fg-rcpth.trans-date
                            . 
                        RELEASE tt-report.
                    END.
                END. /* dtDate loop*/
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE lv-cost-uom     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-current-job   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cReason         AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-shipto        AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-shipto-name   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-whse          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iBol-no         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-current-job2  AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-stnd-cost     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-new-job       AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE v-fg-qty        AS INTEGER   FORMAT "->>>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-fg-cost       AS DECIMAL   FORMAT "->>>,>>9.99<<" NO-UNDO.
    DEFINE VARIABLE v-tot-qty       LIKE v-fg-qty NO-UNDO.
    DEFINE VARIABLE v-tot-cost      LIKE v-fg-cost NO-UNDO.
    DEFINE VARIABLE v-fg-value      AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-msf           AS DECIMAL   FORMAT ">,>>9.999" EXTENT 6 NO-UNDO.
    DEFINE VARIABLE v-grd-tot-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99<<" NO-UNDO.                     
    DEFINE VARIABLE v-tot-value     AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.                  
    DEFINE VARIABLE v-grd-tot-value AS DECIMAL   FORMAT "->>,>>>,>>9.99" NO-UNDO.
    DEFINE VARIABLE v-cum-tot       AS de        NO-UNDO.                                   
    DEFINE VARIABLE v-tran-type     AS CHARACTER FORMAT "x(1)" NO-UNDO.      
    DEFINE VARIABLE v-entrytype     AS CHARACTER INITIAL "REC  ,TRAN ,ADJ  ,SHIP ,RET  ,COUNT" NO-UNDO.
    DEFINE VARIABLE v-on            LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-qty-pallet    AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-cases         LIKE fg-rdtlh.cases NO-UNDO.
    DEFINE VARIABLE v-qty-case      LIKE fg-rdtlh.qty-case NO-UNDO.
    DEFINE VARIABLE v-i-no          LIKE fg-rcpth.i-no NO-UNDO.
    DEFINE VARIABLE v-tag           AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lv-sell-price   LIKE itemfg.sell-price NO-UNDO.
    DEFINE VARIABLE lv-sell-uom     LIKE itemfg.sell-uom NO-UNDO.
    DEFINE VARIABLE v-caliper       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-numColors     LIKE eb.i-col NO-UNDO.
    DEFINE VARIABLE v-numup         LIKE eb.num-up NO-UNDO.
    DEFINE VARIABLE v-sheetsize     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE job-start       AS DATE      NO-UNDO.
    DEFINE VARIABLE prom-date       AS DATE      NO-UNDO. 
    DEFINE VARIABLE due-date        AS DATE      NO-UNDO.
    DEFINE VARIABLE order-no        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE iBinQtyb        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE cocode          AS CHARACTER NO-UNDO.
    DEFINE VARIABLE dBinMatCost     AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE dBinDLCost      AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE dBinVOHCost     AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE dBinFOHCost     AS DECIMAL INIT 0 NO-UNDO.

    DEFINE BUFFER bfg-rcpth   FOR fg-rcpth.
    DEFINE BUFFER b-fgrdtlh   FOR fg-rdtlh.
    DEFINE BUFFER bpo-ord     FOR po-ord.
    DEFINE BUFFER bf-fg-rcpth FOR fg-rcpth.
    DEFINE BUFFER bf-fg-rdtlh FOR fg-rdtlh.
    DEFINE BUFFER bitemfg     FOR itemfg.

    cocode = cCompany.
    {ce/msfcalc.i}
    
    RUN create-tt-report.
    FOR EACH tt-report
        WHERE tt-report.term-id EQ "",
        FIRST fg-rdtlh WHERE RECID(fg-rdtlh) EQ tt-report.rec-id NO-LOCK,
        FIRST fg-rcpth NO-LOCK
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
          AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        BREAK BY tt-report.key-01
              BY tt-report.key-02
              BY tt-report.key-03
              BY tt-report.key-04
              BY tt-report.rptDate
        :
        BUFFER bfg-rcpth:FIND-BY-ROWID(ROWID(fg-rcpth), NO-LOCK).
        BUFFER b-fgrdtlh:FIND-BY-ROWID(ROWID(fg-rdtlh), NO-LOCK).
        IF fg-rcpth.job-no NE v-current-job THEN
        ASSIGN
            v-new-job      = YES
            v-current-job  = fg-rcpth.job-no
            v-current-job2 = fg-rcpth.job-no2
            .
        ELSE
        v-new-job = NO.
        IF FIRST-OF(tt-report.key-01) THEN
        v-whse = fg-rdtlh.loc.      
        ASSIGN
            v-stnd-cost = 0
            dBinMatCost = 0
            dBinDLCost  = 0
            dBinVOHCost = 0
            dBinFOHCost = 0
            .
        IF fg-rcpth.rita-code EQ "S" THEN DO:
          FIND FIRST fg-bin NO-LOCK
               WHERE fg-bin.company EQ fg-rcpth.company
                 AND fg-bin.job-no  EQ fg-rcpth.job-no
                 AND fg-bin.job-no2 EQ fg-rcpth.job-no2
                 AND fg-bin.i-no    EQ fg-rcpth.i-no
                 AND fg-bin.loc     EQ fg-rdtlh.loc
                 AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                 AND fg-bin.tag     EQ fg-rdtlh.tag
              USE-INDEX job
              NO-ERROR.
            lv-cost-uom = IF AVAILABLE fg-bin THEN fg-bin.pur-uom ELSE "M".
        END.
        ELSE
        lv-cost-uom = fg-rcpth.pur-uom.
        FIND FIRST oe-bolh NO-LOCK
             WHERE oe-bolh.company EQ cCompany
               AND oe-bolh.b-no    EQ fg-rcpth.b-no
             NO-ERROR.
        IF AVAILABLE oe-bolh  THEN 
        FIND FIRST shipto NO-LOCK
             WHERE shipto.company EQ cCompany
               AND shipto.cust-no EQ oe-bolh.cust-no
               AND shipto.ship-id EQ oe-bolh.ship-id 
             NO-ERROR.
        IF AVAILABLE shipto THEN
        ASSIGN 
            v-shipto = shipto.ship-id
            v-shipto-name =  shipto.ship-name
            .
        ELSE
        ASSIGN 
            v-shipto = ""
            v-shipto-name = ""
            .
        IF fg-rcpth.rita-code EQ "S" THEN DO:
            FIND FIRST oe-boll NO-LOCK
                 WHERE oe-boll.company EQ oe-bolh.company
                   AND oe-boll.b-no    EQ oe-bolh.b-no
                 NO-ERROR.
            IF AVAILABLE oe-boll THEN
            iBol-no = IF AVAILABLE oe-boll THEN oe-boll.bol-no ELSE 0.
        END.
        ELSE iBol-no = 0.
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cCompany
              AND itemfg.i-no    EQ fg-rcpth.i-no
            USE-INDEX i-no
            NO-ERROR.
        IF AVAILABLE itemfg THEN DO:
            lv-cost-uom = itemfg.prod-uom.
            /* calculate the cost based on fg-rcpth.pur-uom. */
            ASSIGN
                 v-fg-qty   = fg-rdtlh.qty
                 v-fg-cost  = fg-rdtlh.cost * (v-fg-qty / IF lv-cost-uom EQ "M" THEN 1000 ELSE 1)
                 v-fg-value = 0
                 v-msf[1]   = 0
                 v-msf[2]   = 0
                 .        
            RELEASE job-mat.
            IF fg-rcpth.rita-code EQ "R" THEN
            RUN calc-msf-for-r (
                ROWID(fg-rcpth),
                ROWID(fg-rdtlh),
                LAST-OF(tt-report.key-02),
                v-corr,
                OUTPUT v-on,
                OUTPUT v-qty-pallet,
                OUTPUT v-msf[1],
                OUTPUT v-msf[2]
                ).    
            IF fg-rcpth.rita-code EQ "R" THEN DO:
                IF v-msf[1] GT fg-rdtlh.qty * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2]
                         + (v-msf[1] - (fg-rdtlh.qty * itemfg.t-sqft))
                         .
                v-msf[1] = fg-rdtlh.qty * itemfg.t-sqft.
            END.
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ itemfg.company
                   AND eb.est-no   EQ itemfg.est-no
                   AND eb.stock-no EQ itemfg.i-no
                 NO-ERROR.
            FIND ef OF eb NO-LOCK NO-ERROR.
            v-caliper = IF AVAILABLE ef THEN STRING(ef.cal) ELSE "".
            IF AVAILABLE eb THEN
            ASSIGN
                v-numColors = eb.i-col
                v-numup     = eb.num-up
                v-sheetsize = IF AVAILABLE ef THEN STRING(ef.gsh-wid) + "x" + STRING(ef.gsh-len) ELSE ""
                .
        END.
        job-start = ? .
        FIND FIRST job NO-LOCK
             WHERE job.company EQ fg-rcpth.company 
               AND job.job-no  EQ fg-rcpth.job-no
               AND job.job-no2 EQ fg-rcpth.job-no2
             NO-ERROR.
        IF AVAILABLE job THEN
        job-start = job.start-date.
        FIND FIRST job-hdr NO-LOCK
             WHERE job-hdr.company EQ fg-rcpth.company
               AND job-hdr.job-no  EQ fg-rcpth.job-no
               AND job-hdr.job-no2 EQ fg-rcpth.job-no2
               AND job-hdr.i-no    EQ fg-rcpth.i-no
             NO-ERROR.
        IF AVAILABLE job-hdr AND job-hdr.est-no NE "" THEN DO:
            FIND FIRST eb NO-LOCK
                 WHERE eb.company  EQ job-hdr.company
                   AND eb.est-no   EQ job-hdr.est-no
                   AND eb.stock-no EQ job-hdr.i-no
                 NO-ERROR.
            IF AVAILABLE eb THEN
            ASSIGN
                v-numColors = (eb.i-col)
                v-numup     = (eb.num-up)
                v-sheetsize = IF AVAILABLE ef THEN STRING(ef.gsh-wid) + "x" + string(ef.gsh-len) ELSE ""
                .
            FIND ef OF eb NO-LOCK NO-ERROR.
            v-caliper = IF AVAILABLE ef THEN STRING(ef.cal) ELSE "".
        END.
        ASSIGN 
            prom-date = ? 
            due-date  = ?
            order-no  = 0
            .        
        IF AVAILABLE job-hdr AND job-hdr.ord-no NE 0 THEN DO:
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ cCompany
                   AND oe-ordl.ord-no  EQ INTEGER(job-hdr.ord-no )
                   AND oe-ordl.i-no    EQ itemfg.i-no
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
            ASSIGN
                prom-date = oe-ordl.prom-date 
                due-date  = oe-ordl.req-date 
                order-no  = oe-ordl.ord-no
                .
        END.
        ELSE
        IF AVAILABLE oe-bolh THEN DO:
            FIND FIRST oe-boll NO-LOCK
                 WHERE oe-boll.company EQ oe-bolh.company 
                   AND oe-boll.b-no    EQ oe-bolh.b-no
                   AND oe-boll.i-no    EQ itemfg.i-no
                 NO-ERROR.
            IF AVAILABLE oe-boll THEN 
            FIND FIRST oe-ordl NO-LOCK
                 WHERE oe-ordl.company EQ cCompany
                   AND oe-ordl.ord-no  EQ INTEGER(oe-boll.ord-no )
                   AND oe-ordl.i-no    EQ itemfg.i-no
                 NO-ERROR.
            IF AVAILABLE oe-ordl THEN
            ASSIGN
                prom-date = oe-ordl.prom-date 
                due-date  = oe-ordl.req-date 
                order-no  = oe-ordl.ord-no
                .
        END.
        RUN calc-sell-price (
            ROWID(fg-rcpth), 
            OUTPUT lv-sell-price, 
            OUTPUT lv-sell-uom
            ).
        RUN calc-fg-value (
            lv-sell-price,
            lv-sell-uom,
            ROWID(fg-rdtlh),
            OUTPUT v-fg-value
            ).
        ASSIGN
            v-msf[1] = v-msf[1] / 1000
            v-msf[2] = v-msf[2] / 1000
            .
        IF INDEX("RTASEC", fg-rcpth.rita-code) NE 0 THEN
        v-tran-type = ENTRY(INDEX("RTASEC", fg-rcpth.rita-code),v-entrytype).
        ELSE v-tran-type = "".
        IF fg-rcpth.po-no NE " " THEN
        FIND FIRST po-ord NO-LOCK
             WHERE po-ord.company EQ cCompany
               AND po-ord.po-no   EQ INTEGER(fg-rcpth.po-no)
             NO-ERROR.   
        IF AVAILABLE po-ord THEN
        BUFFER bpo-ord:FIND-BY-ROWID(ROWID(po-ord), NO-LOCK).
        RUN calc-case-and-tag (
            ROWID(fg-rcpth),
            ROWID(fg-rdtlh),
            v-fg-qty,
            OUTPUT v-cases,
            OUTPUT v-qty-case,
            OUTPUT v-tag
            ).
        FIND FIRST loadtag NO-LOCK
             WHERE loadtag.item-type EQ NO
               AND loadtag.company   EQ cCompany
               AND loadtag.i-no      EQ fg-rcpth.i-no
               AND loadtag.tag-no    EQ fg-rdtlh.tag
             NO-ERROR.
        IF AVAILABLE loadtag THEN
        FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.   
        FIND FIRST fg-bin NO-LOCK
              WHERE fg-bin.company EQ fg-rcpth.company
                AND fg-bin.job-no  EQ fg-rcpth.job-no
                AND fg-bin.job-no2 EQ fg-rcpth.job-no2
                AND fg-bin.i-no    EQ fg-rcpth.i-no
                AND fg-bin.loc     EQ fg-rdtlh.loc
                AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
                AND fg-bin.tag     EQ fg-rdtlh.tag
                AND fg-bin.po-no   EQ fg-rcpth.po-no
                AND fg-bin.bol-no  EQ fg-rdtlh.bol-no
              USE-INDEX job
              NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        FIND FIRST fg-bin NO-LOCK
             WHERE fg-bin.company EQ fg-rcpth.company
               AND fg-bin.job-no  EQ fg-rcpth.job-no
               AND fg-bin.job-no2 EQ fg-rcpth.job-no2
               AND fg-bin.i-no    EQ fg-rcpth.i-no
               AND fg-bin.loc     EQ fg-rdtlh.loc
               AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
               AND fg-bin.tag     EQ fg-rdtlh.tag
             USE-INDEX job 
             NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        FIND FIRST fg-bin NO-LOCK
             WHERE fg-bin.company EQ fg-rcpth.company
               AND fg-bin.i-no    EQ fg-rcpth.i-no
               AND fg-bin.po-no   EQ fg-rcpth.po-no
               AND fg-bin.loc     EQ fg-rdtlh.loc
               AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
               AND fg-bin.tag     EQ fg-rdtlh.tag
             USE-INDEX po-no
             NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        FIND FIRST fg-bin NO-LOCK
             WHERE fg-bin.company EQ fg-rcpth.company
               AND fg-bin.i-no    EQ fg-rcpth.i-no
               AND fg-bin.loc     EQ fg-rdtlh.loc
               AND fg-bin.loc-bin EQ fg-rdtlh.loc-bin
               AND fg-bin.tag     EQ fg-rdtlh.tag
             USE-INDEX co-ino NO-ERROR.
            IF AVAILABLE fg-bin THEN DO:
                ASSIGN    
                    v-stnd-cost = fg-bin.std-tot-cost
                    dBinMatCost = fg-bin.std-mat-cost
                    dBinDLCost  = fg-bin.std-lab-cost
                    dBinVOHCost = fg-bin.std-var-cost
                    dBinFOHCost = fg-bin.std-fix-cost
                    .
            END.
            ELSE
            IF AVAILABLE itemfg THEN
            v-stnd-cost = itemfg.total-std-cost.
            iBinQtyb = 0.
            IF fg-rcpth.rita-code = "C" THEN
            FOR EACH bf-fg-rcpth NO-LOCK
                WHERE bf-fg-rcpth.company   EQ cCompany 
                  AND bf-fg-rcpth.i-no      EQ fg-rcpth.i-no
                  AND bf-fg-rcpth.job-no    EQ fg-rcpth.job-no
                  AND bf-fg-rcpth.job-no2   EQ fg-rcpth.job-no2
                  AND bf-fg-rcpth.po-no     EQ fg-rcpth.po-no
                  AND bf-fg-rcpth.rita-code NE "C",
                 EACH bf-fg-rdtlh NO-LOCK
                WHERE bf-fg-rdtlh.r-no    EQ bf-fg-rcpth.r-no
                  AND bf-fg-rdtlh.loc     EQ fg-rdtlh.loc
                  AND bf-fg-rdtlh.loc-bin EQ fg-rdtlh.loc-bin
                  AND bf-fg-rdtlh.tag     EQ fg-rdtlh.tag
                  AND bf-fg-rdtlh.cust-no EQ fg-rdtlh.cust-no 
                  AND bf-fg-rdtlh.bol-no  EQ fg-rdtlh.bol-no
                  AND bf-fg-rdtlh.inv-no  EQ fg-rdtlh.inv-no
                BY fg-rcpth.trans-date DESCENDING
                :
                iBinQtyb = iBinQtyb +  bf-fg-rdtlh.qty.
                LEAVE.
            END.
            cReason = "".
            FIND FIRST rejct-cd NO-LOCK
                 WHERE rejct-cd.CODE EQ fg-rdtlh.reject-code[1]
                 NO-ERROR.
            IF fg-rdtlh.reject-code[1] NE "" THEN
            cReason = fg-rdtlh.reject-code[1] + IF AVAILABLE rejct-cd AND rejct-cd.dscr NE "" THEN ( " - " + rejct-cd.dscr) ELSE "".
            ELSE cReason = "".
       
        BUFFER bitemfg:FIND-BY-ROWID(ROWID(itemfg), NO-LOCK).
        CREATE ttFGPostHist.
        ASSIGN
            ttFGPostHist.transDate   = fg-rcpth.trans-date
            ttFGPostHist.itemNo      = fg-rcpth.i-no      
            ttFGPostHist.itemName    = fg-rcpth.i-name    
            ttFGPostHist.poNo        = fg-rcpth.po-no     
            ttFGPostHist.jobNo       = fg-rcpth.job-no    
            ttFGPostHist.vendorNo    = IF AVAILABLE po-ord THEN po-ord.vend-no ELSE ""     
            ttFGPostHist.bin         = fg-rdtlh.loc-bin   
            ttFGPostHist.partNo      = itemfg.part-no     
            ttFGPostHist.dieNo       = itemfg.die-no      
            ttFGPostHist.cadNo       = itemfg.cad-no      
            ttFGPostHist.plateNo     = itemfg.plate-no    
            ttFGPostHist.user-id     = fg-rcpth.user-id   
            ttFGPostHist.postDate    = fg-rcpth.post-date 
            ttFGPostHist.ProdCat     = itemfg.procat      
            ttFGPostHist.custName    = itemfg.cust-name   
            ttFGPostHist.loc         = fg-rdtlh.loc 
            ttFGPostHist.v-tran-type = SUBSTRING(v-tran-type,1,1)
            ttFGPostHist.v-tag       = v-tag
            ttFGPostHist.v-rfid#     = IF AVAILABLE rfidtag THEN SUBSTRING(rfidtag.rfidtag,13) ELSE ""
            ttFGPostHist.v-cases     = v-cases
            ttFGPostHist.v-qty-case  = v-qty-case
            ttFGPostHist.lv-cost-uom = lv-cost-uom
            ttFGPostHist.v-fg-qty    = v-fg-qty
            ttFGPostHist.v-fg-cost   = v-fg-cost
            ttFGPostHist.v-fg-value  = v-fg-value
            ttFGPostHist.v-numUp     = v-numUP
            ttFGPostHist.v-numColors = v-numColors
            ttFGPostHist.v-SheetSize = v-SheetSize
            ttFGPostHist.v-Caliper   = v-Caliper
            ttFGPostHist.wt-h        = fg-rdtlh.tot-wt
            ttFGPostHist.rec-time    = STRING(fg-rdtlh.trans-time,'HH:MM')
            ttFGPostHist.unt-sel     = lv-sell-price
            ttFGPostHist.suom        = lv-sell-uom
            ttFGPostHist.unt-cst     = IF v-stnd-cost NE 0 THEN v-stnd-cost ELSE 0
            ttFGPostHist.prom-date   = prom-date
            ttFGPostHist.due-date    = due-date
            ttFGPostHist.job-start   = job-start
            ttFGPostHist.shipto      = v-shipto
            ttFGPostHist.shipname    = v-shipto-name
            ttFGPostHist.order-no    = order-no
            ttFGPostHist.bef-qty     = iBinQtyb
            ttFGPostHist.bin-qty     =  v-fg-qty - iBinQtyb
            ttFGPostHist.bol-no      = iBol-no
            ttFGPostHist.Reason      = cReason
            ttFGPostHist.Reason-cd   = IF AVAILABLE fg-rdtlh AND fg-rdtlh.reject-code[1] NE "" THEN fg-rdtlh.reject-code[1] ELSE ""
            ttFGPostHist.Reason-dscr = IF AVAILABLE rejct-cd AND rejct-cd.dscr NE "" THEN rejct-cd.dscr ELSE ""
            ttFGPostHist.item-mat-cost = itemfg.std-mat-cost
            ttFGPostHist.item-dl-cost  = itemfg.std-lab-cost
            ttFGPostHist.item-voh-cost = itemfg.std-var-cost
            ttFGPostHist.item-foh-cost = itemfg.std-fix-cost
            ttFGPostHist.bin-mat-cost  = IF dBinMatCost NE 0 THEN dBinMatCost ELSE 0
            ttFGPostHist.bin-dl-cost   = IF dBinDLCost  NE 0 THEN dBinDLCost  ELSE 0
            ttFGPostHist.bin-voh-cost  = IF dBinVOHCost NE 0 THEN dBinVOHCost ELSE 0
            ttFGPostHist.bin-foh-cost  = IF dBinFOHCost NE 0 THEN dBinFOHCost ELSE 0
            .
    END.

END PROCEDURE.

{AOA/dynBL/pBuildCustList.i}

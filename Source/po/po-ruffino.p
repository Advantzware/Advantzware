/* -------------------------------------------------------------------------- */ 
/* po/po-ruffino.p                                                              */
/* Purchase Order Print Program for POPRINT = XPRINT                      */
/*                                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ip-multi-faxout AS LOG NO-UNDO. /* fax multiple recipents or single */
DEFINE INPUT PARAMETER ip-lines-per-page AS INTEGER NO-UNDO.

DEFINE STREAM st-fax.

{sys/inc/var.i shared}
{sys/form/s-top.f}

DEFINE BUFFER b-ref1 FOR reftable.
DEFINE BUFFER b-ref2 FOR reftable.

{po/po-print.i}
DEFINE SHARED VARIABLE s-group-notes   AS LOG       NO-UNDO.

DEFINE        VARIABLE v-wid           AS DECIMAL   FORMAT ">>9.99<<" NO-UNDO.
DEFINE        VARIABLE v-dep           AS DECIMAL   FORMAT ">>9.99<<" NO-UNDO.    
DEFINE        VARIABLE v-basis-w       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-len           AS DECIMAL   FORM "->>9.99<<" NO-UNDO.
DEFINE        VARIABLE v-wid2          LIKE po-ordl.s-wid FORMAT ">>9.99<<" NO-UNDO. /* for recalc extened cost */
DEFINE        VARIABLE v-len2          LIKE po-ordl.s-len FORMAT ">>9.99<<" NO-UNDO. /* for recalc extened cost */
DEFINE        VARIABLE pol-counter     AS INTEGER   NO-UNDO.
DEFINE        VARIABLE save_id         AS RECID.
DEFINE        VARIABLE time_stamp      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE v-exp-limit     AS INTEGER   NO-UNDO INIT 10.
DEFINE        VARIABLE v-line-number   AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-page-counter  AS INTEGER   FORMAT ">>9" NO-UNDO.
DEFINE        VARIABLE v-lines-to-skip AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-sname         LIKE shipto.ship-name NO-UNDO.
DEFINE        VARIABLE v-saddr         LIKE shipto.ship-addr NO-UNDO.
DEFINE        VARIABLE v-scity         LIKE shipto.ship-city NO-UNDO.
DEFINE        VARIABLE v-sstate        LIKE shipto.ship-state.
DEFINE        VARIABLE v-szip          LIKE shipto.ship-zip.
DEFINE        VARIABLE v-po-type       AS CHARACTER FORMAT "x(10)".
DEFINE        VARIABLE v-freight-dscr  AS CHARACTER FORMAT "x(7)".
DEFINE        VARIABLE v-change-dscr   AS CHARACTER FORMAT "x(7)".
DEFINE        VARIABLE v-dash-line     AS CHARACTER FORMAT "x(80)" EXTENT 3.
DEFINE        VARIABLE v-dec-fld       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-adders        AS LOG.
DEFINE        VARIABLE xg-flag         AS LOG       INIT NO NO-UNDO.
DEFINE        VARIABLE v-space         AS LOG       INIT YES.
DEFINE        VARIABLE len-score       AS CHARACTER.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER xitem    FOR item.
DEFINE VARIABLE same-score      AS ch        NO-UNDO.
DEFINE VARIABLE v-test-scr      AS LOG       NO-UNDO.
DEFINE VARIABLE v-hdr           AS CHARACTER FORMAT "x(15)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-ino-job       AS CHARACTER FORMAT "x(15)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-change-ord    AS CHARACTER FORMAT "x(35)" INITIAL "" NO-UNDO.
DEFINE VARIABLE lv-got-return   AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-dep          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-dep2         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-cost         LIKE po-ordl.cost NO-UNDO.
DEFINE VARIABLE lv-pr-uom       LIKE po-ordl.pr-uom NO-UNDO.
DEFINE VARIABLE lv-pr-qty-uom   LIKE po-ordl.pr-qty-uom NO-UNDO.

DEFINE VARIABLE lv-Format       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-Ord-Qty      LIKE po-ordl.ord-qty NO-UNDO.

DEFINE VARIABLE v-tel           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact       AS cha       FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-line-total    AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-quo-total     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-t-tax         AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-bot-lab       AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE VARIABLE v-q-no          LIKE oe-ord.q-no NO-UNDO.
DEFINE VARIABLE v-printline     AS INTEGER   NO-UNDO.

DEFINE VARIABLE v-qty           LIKE po-ordl.ord-qty NO-UNDO.
DEFINE VARIABLE v-tot-sqft      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-vend-item     AS cha       NO-UNDO.
DEFINE VARIABLE v-adder         AS cha       FORM "x(30)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-num-add       AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-job-no        AS cha       NO-UNDO.
DEFINE VARIABLE v-cost          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-setup         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-tmp-lines     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-inst-lines    AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-inst          AS cha       FORM "x(80)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE lv-display-comp AS LOG       NO-UNDO.
DEFINE VARIABLE lv-email        AS cha       FORM "x(40)" NO-UNDO.
DEFINE VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.
DEFINE VARIABLE lv-flute        LIKE ITEM.flute NO-UNDO.
DEFINE VARIABLE lv-reg-no       LIKE ITEM.reg-no NO-UNDO.
DEFINE VARIABLE lv-item-rec     AS cha       NO-UNDO.
{custom/formtext.i NEW}
DEFINE VARIABLE lv-text             AS CHARACTER NO-UNDO.
DEFINE VARIABLE li                  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-dept-note         AS cha       FORM "x(80)" EXTENT 50 NO-UNDO.
DEFINE VARIABLE lv-text-line        AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-text-line-length AS INTEGER   NO-UNDO.
DEFINE VARIABLE lv-char             AS cha       NO-UNDO.
DEFINE VARIABLE lv-char-list        AS cha       NO-UNDO.
DEFINE VARIABLE cRtnChar            AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound           AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1        AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE dCoreDia            AS DECIMAL   FORMAT ">,>>9.99<<" NO-UNDO.
DEFINE VARIABLE cFlueTest           AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE cMachCode           AS CHARACTER NO-UNDO .
DEFINE VARIABLE lPrintMsf           AS LOGICAL   NO-UNDO .
DEFINE VARIABLE cGrandTotMsf        AS DECIMAL   NO-UNDO .
DEFINE TEMP-TABLE tt-text NO-UNDO
    FIELD TYPE     AS cha
    FIELD tt-line  AS INTEGER
    FIELD tt-text  AS cha 
    FIELD tt-recid AS RECID
    INDEX tt-text IS PRIMARY TYPE tt-line.
DEFINE VARIABLE lValid   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

RUN FileSys_GetBusinessFormLogo(cocode, "" /* cust */ , "" /* location */ , OUTPUT cRtnChar, OUTPUT lValid, OUTPUT cMessage).

IF NOT lValid THEN
DO:
    MESSAGE cMessage VIEW-AS ALERT-BOX ERROR.
END.

ASSIGN ls-full-img1 = cRtnChar + ">" .

v-dash-line = FILL ("_",80).

IF ip-multi-faxout THEN 
DO:

    DEFINE VARIABLE lv-file-name AS cha FORM "x(60)" NO-UNDO.
    OS-CREATE-DIR VALUE("c:\temp\fax").
    INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
    REPEAT:
        SET lv-file-name.  
        IF lv-file-name <> "." AND lv-file-name <> ".." THEN 
        DO:     
            OS-DELETE VALUE("C:\temp\fax\" + lv-file-name) .       
        END.
    END.
END.

{po/po-print.f}

ASSIGN 
    v-hdr = "VEND ITEM".
       
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "POPRINT" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.
v-dec-fld = sys-ctrl.dec-fld.
FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".


FUNCTION FNmetric RETURNS CHARACTER (INPUT aa AS DECIMAL,
    INPUT bb AS DECIMAL,
    INPUT dim AS CHARACTER).  

    DEFINE VARIABLE lv-format AS CHARACTER EXTENT 2 NO-UNDO.
 
    ASSIGN
     lv-format[1] = ">>,>>>,>>>"
     lv-format[2] = ">>>,>>9.99<<<<". 
   
    IF v-metric THEN 
        RETURN STRING(ROUND(bb * 25.4,0),lv-format[1]).
    ELSE
        RETURN STRING(aa,lv-format[2]).

END FUNCTION.

ASSIGN 
    v-comp-add1 = ""
    v-comp-add2 = "" 
    v-comp-add3 = ""
    v-comp-add4 = ""
    v-comp-add5 = "".
IF lv-display-comp THEN 
DO:
    FIND FIRST cust WHERE cust.company = cocode AND
        cust.active = "X" NO-LOCK NO-ERROR.
    IF AVAILABLE cust THEN
        ASSIGN v-comp-add1  = cust.addr[1]
            v-comp-add2  = cust.addr[2]
            v-comp-add3  = cust.city + ", " + cust.state + "  " + cust.zip
            v-comp-add4  = "Phone:  " + string(cust.area-code,"(999)") + string(cust.phone,"999-9999") 
            v-comp-add5  = "Fax     :  " + string(cust.fax,"(999)999-9999") 
            lv-email     = "Email:  " + cust.email 
            lv-comp-name = cust.NAME.
END.

v-tot-sqft = 0.
cGrandTotMsf = 0 .
print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
    BREAK BY po-ord.vend-no BY PO-ORD.PO-NO:

    /*      if po-ord.type eq "D" then  */
    ASSIGN 
        v-sname    = po-ord.ship-name
        v-saddr[1] = po-ord.ship-addr[1]
        v-saddr[2] = po-ord.ship-addr[2]
        v-scity    = po-ord.ship-city
        v-sstate   = po-ord.ship-state
        v-szip     = po-ord.ship-zip.

    {po/exportpo.i}

    ASSIGN 
        v-page-counter = 1
        v-change-ord   = "".

    IF po-ord.stat EQ "N" THEN
        ASSIGN po-ord.stat = "O".
    ELSE
        IF po-ord.stat EQ "U" THEN
            v-change-ord = "(CHANGED ORDER ONLY)".

    FIND FIRST vend WHERE vend.company EQ po-ord.company 
        AND vend.vend-no EQ po-ord.vend-no NO-LOCK NO-ERROR.
    FIND FIRST terms WHERE terms.company EQ po-ord.company AND terms.t-code EQ po-ord.terms NO-LOCK NO-ERROR.
    FIND FIRST carrier WHERE carrier.company EQ po-ord.company 
        AND carrier.carrier EQ po-ord.carrier NO-LOCK NO-ERROR.

    IF ip-multi-faxout AND AVAILABLE vend AND FIRST-OF(po-ord.vend-no) THEN 
    DO:
        OUTPUT CLOSE.
        OUTPUT STREAM st-fax CLOSE.
        OUTPUT TO value("c:\temp\fax\fx" + po-ord.vend-no + ".xpr") PAGE-SIZE value(ip-lines-per-page).
        OUTPUT STREAM st-fax TO value("c:\temp\fax\fx" + po-ord.vend-no + ".txt").
        PUT STREAM st-fax UNFORMATTED 
            "FAX#:" TRIM(STRING(vend.fax-prefix)) + STRING(vend.fax-area,"x(3)") + STRING(vend.fax,"xxxxxxx") SKIP.
        PUT CONTROL "<PRINT=NO>".       
        PUT UNFORMATTED 
            "<EXPORT=c:\temp\fax\fx" TRIM(vend.vend-no) ".tif,BW>" .
    /*
    PUT "FAX#:" cust.fax SKIP.*/
    END.

    IF po-ord.type EQ "R" THEN
        ASSIGN v-po-type = "Regular".
    ELSE
        ASSIGN v-po-type = "Drop Ship".

    IF po-ord.frt-pay EQ "P" THEN
        ASSIGN v-freight-dscr = "Prepaid".
    ELSE IF po-ord.frt-pay EQ "C" THEN
            ASSIGN v-freight-dscr = "Collect".
        ELSE
            ASSIGN v-freight-dscr = "Bill".

    v-printline = 0.
    {po/po-ruffino.i}

    FOR EACH po-ordl WHERE
        po-ordl.company EQ po-ord.company AND
        po-ordl.po-no EQ po-ord.po-no BY po-ordl.line:
        ASSIGN 
            xg-flag = NO.
        IF NOT v-printde-po AND po-ordl.deleted THEN NEXT.
        ASSIGN 
            v-change-dscr = "".

        IF po-ordl.stat EQ "A" THEN ASSIGN v-change-dscr = "Added".
        ELSE IF po-ordl.stat EQ "U" THEN ASSIGN v-change-dscr = "Updated".
            ELSE IF po-ordl.stat EQ "O" THEN ASSIGN v-change-dscr = "Open".
                ELSE IF po-ordl.stat EQ "P" THEN ASSIGN v-change-dscr = "Partial".
                    ELSE IF po-ordl.stat EQ "C" THEN ASSIGN v-change-dscr = "Closed".

        IF po-ordl.deleted EQ YES THEN   ASSIGN v-change-dscr = "Deleted".

        ASSIGN
            v-ino-job   = po-ordl.vend-i-no
            V-ADDER     = ""
            v-vend-item = "".

        FIND item WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
            AND po-ordl.item-type
            NO-LOCK NO-ERROR.
        IF NOT po-ordl.item-type THEN /* fg item */ 
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company = po-ordl.company
                AND itemfg.i-no = po-ordl.i-no NO-ERROR.

        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").

        

        ASSIGN 
            v-wid   = po-ordl.s-wid
            v-len   = po-ordl.s-len
            lv-dep  = IF po-ordl.s-dep GT 0 THEN po-ordl.s-dep
                        ELSE IF AVAILABLE ITEM AND ITEM.mat-type = "C" THEN item.case-d
                        ELSE IF AVAILABLE ITEM THEN ITEM.s-dep
                        ELSE 0
            v-wid2  = po-ordl.s-wid
            v-len2  = po-ordl.s-len
            lv-dep2 = lv-dep.

        
        IF AVAILABLE item AND item.mat-type EQ "B" THEN 
        DO:
            IF v-shtsiz THEN 
            DO:
                IF v-dec-fld = 0.08 THEN
                    ASSIGN v-wid  = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                        v-wid  = ( v-wid * 16 ) / 100
                        v-wid  = TRUNCATE(po-ordl.s-wid,0) + v-wid
                        v-len  = po-ordl.s-len - truncate(po-ordl.s-len,0)
                        v-len  = ( v-len * 16 ) / 100
                        v-len  = TRUNCATE(po-ordl.s-len,0) + v-len
                        lv-dep = lv-dep2 - truncate(lv-dep2,0)
                        lv-dep = ( lv-dep * 16 ) / 100
                        lv-dep = TRUNCATE(lv-dep2,0) + lv-dep.
                ELSE
                    ASSIGN v-wid = po-ordl.s-wid 
                        v-len = po-ordl.s-len.
            
                ASSIGN 
                    v-num-add = 0.

                FIND FIRST job WHERE job.company EQ cocode 
                    AND job.job-no EQ po-ordl.job-no 
                    AND job.job-no2 EQ po-ordl.job-no2
                    NO-LOCK NO-ERROR.
                IF AVAILABLE job THEN
                DO:
                    FOR EACH job-mat
                        WHERE job-mat.company  EQ cocode
                        AND job-mat.job      EQ job.job
                        AND job-mat.job-no   EQ job.job-no
                        AND job-mat.job-no2  EQ job.job-no2
                        AND job-mat.i-no     EQ po-ordl.i-no
                        AND job-mat.frm      EQ po-ordl.s-num
                        USE-INDEX job NO-LOCK
                        BREAK BY job-mat.blank-no DESCENDING:
                        IF LAST(job-mat.blank-no)            OR
                            job-mat.blank-no EQ po-ordl.b-num THEN LEAVE.
                    END.

                    IF AVAILABLE job-mat AND 
                        job-mat.i-no     EQ po-ordl.i-no  /* gmd - 06190903*/
                        THEN 
                    DO:                  
                        /* Adder i-no and i-name to po of exist */
                        FOR EACH xjob-mat WHERE xjob-mat.company  EQ cocode
                            AND xjob-mat.job      EQ job-mat.job
                            AND xjob-mat.job-no   EQ job-mat.job-no
                            AND xjob-mat.job-no2  EQ job-mat.job-no2
                            AND xjob-mat.frm      EQ job-mat.frm
                            AND xjob-mat.blank-no EQ job-mat.blank-no
                            AND xjob-mat.i-no     NE job-mat.i-no
                            NO-LOCK:
                            FIND FIRST xitem WHERE xitem.company        EQ cocode
                                AND xitem.i-no      EQ xjob-mat.i-no
                                AND xitem.mat-type  EQ "A" NO-LOCK NO-ERROR.
                            IF AVAILABLE xitem THEN
                            DO:
                                /*
                              put xitem.i-no at 25 xitem.i-name at 38.
                              assign v-line-number = v-line-number + 1.
                              */
                                ASSIGN 
                                    v-num-add = v-num-add + 1.
                                IF v-num-add EQ 1 THEN ASSIGN v-adder[1] = xitem.i-name.
                                ELSE IF v-num-add EQ 2 THEN ASSIGN v-adder[2] = xitem.i-name.
                                    ELSE IF v-num-add EQ 3 THEN ASSIGN v-adder[3] = xitem.i-name.
                                        ELSE IF v-num-add EQ 4 THEN ASSIGN v-adder[4] = xitem.i-name.
                                            ELSE IF v-num-add EQ 5 THEN ASSIGN v-adder[5] = xitem.i-name.
                            END.

                        END.

                        FIND FIRST ef WHERE EF.COMPANY EQ JOB.COMPANY
                            AND ef.est-no  EQ job.est-no
                            AND ef.form-no EQ job-mat.frm
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE ef AND (ef.xgrain EQ "S" OR ef.xgrain EQ "B") THEN ASSIGN xg-flag = YES.
                    END. /* avail job-mat */
                END. /* avail job */
            END. /* v-shtsiz */        
        /*
        IF AVAIL ITEM AND ITEM.industry = "2" THEN DO:
           assign v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0).
           assign v-wid = ( v-wid * 16 ) / 100.
           assign v-wid = truncate(po-ordl.s-wid,0) + v-wid.
           assign v-len = po-ordl.s-len - truncate(po-ordl.s-len,0).
           assign v-len = ( v-len * 16 ) / 100.
           assign v-len = truncate(po-ordl.s-len,0) + v-len.
        END.
        */
        END. /* avail item and item.mat-type eq "B" */
        ELSE 
        DO:  /*not rm*/
            IF v-dec-fld = 0.08 THEN
                ASSIGN v-wid  = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                    v-wid  = ( v-wid * 16 ) / 100
                    v-wid  = TRUNCATE(po-ordl.s-wid,0) + v-wid
                    v-len  = po-ordl.s-len - truncate(po-ordl.s-len,0)
                    v-len  = ( v-len * 16 ) / 100
                    v-len  = TRUNCATE(po-ordl.s-len,0) + v-len
                    lv-dep = lv-dep2 - truncate(lv-dep2,0)
                    lv-dep = ( lv-dep * 16 ) / 100
                    lv-dep = TRUNCATE(lv-dep2,0) + lv-dep.
            ELSE
                ASSIGN v-wid = po-ordl.s-wid 
                    v-len = po-ordl.s-len.
        END.
        /* v-job-no = po-ordl.job-no + "-" + STRING(po-ordl.job-no2,">>").*/
        v-job-no = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', po-ordl.job-no, po-ordl.job-no2) +
            "-" + string(po-ordl.s-num,"99"))).

        IF po-ordl.job-no = "" THEN v-job-no = "".

        IF v-job-no = "-" THEN v-job-no = "".
       
        IF v-printline + 4 > 46 THEN 
        DO:         
            PAGE.
            v-printline = 0.
            {po/po-ruffino.i}
        END.


        ASSIGN
            lv-ord-qty    = po-ordl.ord-qty
            lv-cost       = po-ordl.cost
            lv-pr-uom     = po-ordl.pr-uom
            lv-pr-qty-uom = po-ordl.pr-qty-uom 
            lv-format     = IF CAN-DO("MSF,TON", po-ordl.pr-qty-uom) 
                      THEN "->>,>>>,>>9.99"
                      ELSE
                      IF CAN-DO("LF,EA", po-ordl.pr-qty-uom)
                      THEN "->>>,>>>,>>9"
                      ELSE "->>>,>>>,>>9.9<<<<<".

        /* Indicates ord-qty is in 'EA' to be converted to 'CS' */
        IF po-ordl.spare-int-1 EQ 1 AND AVAIL(itemfg) THEN
            ASSIGN 
                lv-ord-qty    = lv-ord-qty / itemfg.case-count
                lv-pr-qty-uom = "CS"
                .

        /* Indicates cost is in 'EA' to be converted to 'CS */
        IF po-ordl.spare-int-2 EQ 1 AND AVAIL(itemfg) THEN
            ASSIGN
                lv-cost   = lv-cost * itemfg.case-count
                lv-pr-uom = "CS"
                .

        IF po-ordl.pr-qty-uom EQ "LF" THEN 
        DO:
            {sys/inc/roundup.i lv-ord-qty}
        END.

        IF NOT(lv-cost GT 9999.99 OR
            po-ordl.t-cost GT 99999.99) THEN 
        DO:
            PUT po-ordl.LINE FORM ">>9"
                STRING(lv-ord-qty, lv-format) FORMAT "x(14)" SPACE(2)
                lv-pr-qty-uom SPACE(1)
                po-ordl.i-no FORM "x(20)" .
               
            IF v-adder[1] NE "" THEN 
                PUT "<C46>" "YES" .
           
            PUT "<C48>" v-job-no FORM "x(16)" SPACE(1)
                lv-cost FORM "->>>9.99<<" SPACE(1)
                lv-pr-uom
                (po-ordl.t-cost - po-ordl.setup) FORM "->>,>>9.99"          
                SKIP.
        END.
        ELSE
        DO:
            PUT po-ordl.LINE FORM ">>9"
                STRING(lv-ord-qty, lv-format) FORMAT "x(14)" SPACE(2)
                lv-pr-qty-uom SPACE(1)
                po-ordl.i-no FORM "x(20)" .
               
            IF v-adder[1] NE "" THEN 
                PUT "<C46>" "YES" .
           
            PUT "<C48>" v-job-no FORM "x(16)" SPACE(1)
                SKIP
                SPACE(68)
                lv-cost FORM "->>>,>>9.99<<" SPACE(1)
                lv-pr-uom
                (po-ordl.t-cost - po-ordl.setup) FORM "->>>,>>9.99"
                SKIP.

            v-printline = v-printline + 1.
        END.

        v-printline = v-printline + 1.

        PUT int(po-ordl.over-pct) FORM ">>9" AT 7 " / "
            int(po-ordl.under-pct) FORM ">>9" 
            po-ordl.i-name AT 25 FORM "x(30)" 
            SKIP.          
        ASSIGN
            v-printline   = v-printline + 1
            v-line-number = v-line-number + 3.


        ASSIGN 
            v-basis-w = 0
            v-dep     = 0.

        RELEASE ITEM.

        IF po-ordl.item-type THEN
            FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                AND ITEM.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN v-basis-w = item.basis-w
                v-dep     = ITEM.s-dep.
                  

        IF po-ordl.pr-qty-uom EQ "MSF" THEN v-qty = po-ordl.ord-qty.
        ELSE RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                po-ordl.ord-qty, OUTPUT v-qty).
        ASSIGN
            v-tot-sqft = v-qty  .
        cGrandTotMsf = cGrandTotMsf + (v-qty * 1000) .
        lPrintMsf = NO .
        IF AVAILABLE ITEM AND (item.mat-type EQ "B" OR item.mat-type EQ "P") AND
            ITEM.industry EQ "2" THEN 
        DO:
            ASSIGN 
                lPrintMsf = YES .
        END.
        
        IF po-ordl.dscr[1] NE "" THEN 
        DO:
            PUT po-ordl.dscr[1] FORMAT "x(30)"  AT 25 .
            IF lPrintMsf THEN
                PUT "<C50.5>MSF: " TRIM(STRING(v-tot-sqft,">>>>>9.99<<")) .
            PUT SKIP.
            lPrintMsf = FALSE .
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline   = v-printline + 1.
        END.
    
        IF po-ordl.dscr[2] NE "" THEN 
        DO:
            PUT po-ordl.dscr[2] FORMAT "x(30)" AT 25 .
            IF lPrintMsf  THEN
                PUT "<C50.5>MSF: " TRIM(STRING(v-tot-sqft,">>>>>9.99<<")) .
            lPrintMsf = FALSE.
            PUT SKIP .
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline   = v-printline + 1.
        END.
        
        IF v-vend-item <> "" THEN 
        DO:
            PUT v-vend-item  FORM "x(30)" AT 25 .
            IF lPrintMsf  THEN
                PUT "<C50.5>MSF: " TRIM(STRING(v-tot-sqft,">>>>>9.99<<")) .
            lPrintMsf = FALSE.
            PUT SKIP .
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline   = v-printline + 1.
        END.
        /* calc total sq feet */
    
        IF v-itemDescription AND NOT po-ordl.item-type THEN /* fg item */ 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company = po-ordl.company
                AND itemfg.i-no = po-ordl.i-no NO-ERROR.
            IF AVAILABLE itemfg THEN 
            DO:
                IF itemfg.part-dscr3 NE '' THEN 
                DO:
                    PUT itemfg.part-dscr3 AT 25.
                    ASSIGN
                        v-line-number = v-line-number + 1
                        v-printline   = v-printline + 1.
                END. /* if part-dscr3 */
            END. /* avail itemfg */
        END. /* if v-itemdescription */
        ELSE IF v-itemDescription THEN /* fg item */ 
            DO:
                IF po-ordl.dscr[2] NE '' THEN 
                DO:
                    PUT po-ordl.dscr[2] AT 25.
                    IF lPrintMsf  THEN
                        PUT "<C50.5>MSF: " TRIM(STRING(v-tot-sqft,">>>>>9.99<<")) .
                    lPrintMsf = FALSE.
                    ASSIGN
                        v-line-number = v-line-number + 1
                        v-printline   = v-printline + 1.
                END.

            END.
            ELSE 
            DO:
                IF po-ordl.vend-i-no NE '' THEN 
                DO:
                    PUT po-ordl.vend-i-no AT 25.
                    IF lPrintMsf  THEN
                        PUT "<C50.5>MSF: " TRIM(STRING(v-tot-sqft,">>>>>9.99<<")) .
                    lPrintMsf = FALSE.
                    ASSIGN
                        v-line-number = v-line-number + 1
                        v-printline   = v-printline + 1.
                END.
            END.

        ASSIGN
            v-cost = lv-cost. /* reclac cost from setup */
        dCoreDia = 0.
        IF AVAILABLE ITEM THEN 
        DO:
            IF ITEM.industry EQ "2" THEN
                ASSIGN dCoreDia = IF item.mat-type EQ "P" THEN (item.ect / 10000) ELSE item.ect.
            ELSE dCoreDia =  IF item.mat-type NE "A" THEN (item.ect / 10000) ELSE item.ect.
        END.

        IF AVAILABLE ITEM AND ITEM.mat-type EQ "B" AND ITEM.industry EQ "2" THEN
            ASSIGN lv-flute  = "  Flute: " + ITEM.flute
                lv-reg-no = "Test: " + ITEM.reg-no.
        ELSE
            ASSIGN lv-flute  = ""
                lv-reg-no = "".

        IF AVAILABLE ITEM AND ITEM.mat-type EQ "B" AND ITEM.industry = "2" AND  ITEM.flute NE "" AND ITEM.reg-no NE "" THEN
            ASSIGN cFlueTest = STRING(lv-flute,"x(13)") + string(lv-reg-no,"x(12)").
        ELSE
            ASSIGN cFlueTest = IF dCoreDia GT 0 AND ITEM.mat-type EQ "P" THEN "Core Dia: " + string(dCoreDia,">,>>9.99<<") ELSE ""
                dCoreDia  = 0.

        IF v-wid GT 0 THEN 
        DO:
            PUT "W: " AT 25 FNmetric(v-wid, v-wid2, "W") FORMAT "x(8)" SPACE(1).
            IF v-len GT 0 THEN
                PUT "L: "  FNmetric(v-len, v-len2, "L") FORMAT "x(10)" SPACE(1).
            IF lv-dep GT 0 THEN
                PUT "D: "  FNmetric(lv-dep, lv-dep2, "D") FORMAT "x(8)" SPACE(1).
        END.

        /* IF AVAIL ITEM AND ITEM.mat-type EQ "B" AND ITEM.industry = "2" THEN
            PUT lv-flute FORM "x(13)" /*"Test:" */ lv-reg-no FORM "x(10)".*/
        PUT cFlueTest SKIP.
        
          
        /*dCoreDia = 0.*/
        IF dCoreDia GT 0 AND ITEM.mat-type EQ "P" THEN 
        DO:
            PUT "Core Dia: " AT 25 dCoreDia FORMAT ">,>>9.99<<" SKIP.
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline   = v-printline + 1.
        END.
        ELSE
            PUT SKIP.

        IF lPrintMsf  THEN 
        DO:
            PUT  "MSF: " AT 30 TRIM(STRING(v-tot-sqft,">>>>>9.99<<")) SKIP .
            lPrintMsf = FALSE.
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline   = v-printline + 1.
        END.

        DO i = 1 TO 5:
            IF v-adder[i] NE "" THEN 
            DO: 
                PUT "Adder: " AT 3 v-adder[i] SKIP.
                v-printline = v-printline + 1.
            END.
        END.
        
        ASSIGN 
            v-line-number = v-line-number + 1
            v-printline   = v-printline + 1
            len-score     = "".

        

        {po/poprints.i}
        IF NOT po-ordl.item-type THEN         
            IF AVAILABLE itemfg AND v-score-types THEN 
            DO:
                PUT 
                    "Score: " AT 3
                    len-score FORMAT "x(80)" SKIP .
                
                ASSIGN
                    v-line-number = v-line-number + 1
                    v-printline   = v-printline + 1.
            END.

        IF AVAILABLE ITEM AND lookup("1,2,3,4",ITEM.mat-type) > 0 THEN 
        DO: 
        END.
        ELSE 
        DO:
            IF NOT v-test-scr AND (AVAILABLE ITEM AND ITEM.mat-type = "B") THEN 
            DO:
                PUT 
                    "Score: " AT 3
                    len-score FORMAT "x(80)" SKIP .
                      
                ASSIGN
                    v-line-number = v-line-number + 1
                    v-printline   = v-printline + 1.
            END.
          
            ELSE
                IF v-test-scr AND AVAILABLE ITEM AND ITEM.mat-type = "B" AND dec(TRIM(len-score)) NE v-wid THEN 
                DO:
                    PUT "Score: " AT 3
                        len-score FORMAT "x(80)"  SKIP.
                      
                    ASSIGN
                        v-line-number = v-line-number + 1
                        v-printline   = v-printline + 1.
                END.
        END.
    END.
END.
END.
         
IF po-ordl.setup NE 0 THEN 
DO:
    PUT "<C12> 1  " SPACE(2)   "EA" SPACE(2) " SETUP <C59>" po-ordl.setup FORM "->>>,>>9.99<<" SPACE(1) "EA "  po-ordl.setup FORM "->>>,>>9.99" SKIP. 
    PUT "<C63>SUB TOTAL" SPACE(1) po-ordl.t-cost FORM "->>>,>>9.99" SKIP .
    v-printline = v-printline + 1 .
    v-line-number = v-line-number + 1.
END. 

FOR EACH tt-formtext:
    DELETE tt-formtext.
END.
lv-text = "".
FOR EACH notes NO-LOCK WHERE notes.rec_key = po-ordl.rec_key :
    lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
END.
DO li = 1 TO 20:
    CREATE tt-formtext.
    ASSIGN 
        tt-line-no = li
        tt-length  = 80.
END.
RUN custom/formtext.p (lv-text).
ASSIGN
    i           = 0
    v-dept-note = "".

FOR EACH tt-formtext:
    i = i + 1.
    IF i <= 20 THEN
        v-dept-note[i] = tt-formtext.tt-text.      
END.
li = 0.
DO i = 20 TO 1 BY -1:
    li = i.
    IF v-dept-note[i] <> "" THEN LEAVE.
END.
IF s-group-notes AND v-printline + li > 46 THEN 
DO:
    PAGE.
    v-printline = 0.
    {po/po-ruffino.i}
END.

DO i = 1 TO li: 
    IF v-dept-note[i] NE "" THEN 
    DO:
        PUT v-dept-note[i] SKIP.
        v-printline = v-printline + 1.
    END.
    IF v-printline > 46 THEN 
    DO:                  
        PAGE.
        v-printline = 0.
        {po/po-ruffino.i}
    END.
END.
PUT SKIP(1).
ASSIGN
    v-line-number = v-line-number + 1
    v-printline   = v-printline + 1.
  
IF v-printline > 46 THEN 
DO:
    PAGE.
    v-printline = 0.
    {po/po-ruffino.i}
END.
IF v-print-sn THEN 
DO:
    IF po-ordl.item-type THEN
        FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
            AND ITEM.i-no    EQ po-ordl.i-no NO-LOCK NO-ERROR.
    ELSE FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
    ASSIGN
        lv-item-rec         = IF po-ordl.item-type AND AVAILABLE ITEM THEN ITEM.rec_key
                      ELSE IF AVAILABLE itemfg THEN itemfg.rec_key
                      ELSE ""
        lv-text             = ""
        lv-text-line        = 0
        lv-text-line-length = 0
        lv-char             = ""
        lv-char-list        = "".
    FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
        notes.note_code = "PO" NO-LOCK:
        lv-text = lv-text + notes.note_text + CHR(10).
    END.
    ASSIGN
        lv-text-line        = 0
        lv-text-line-length = 80.
    DO i = 1 TO LENGTH(lv-text):
        ASSIGN 
            lv-char = SUBSTR(lv-text,i,1).
        IF lv-char EQ CHR(10) OR lv-char EQ CHR(13) THEN 
        DO: 
        END.
        ELSE 
        DO:
            lv-char-list = lv-char-list + lv-char.
        END.
        IF  lv-char EQ CHR(10) OR lv-char EQ CHR(13) OR 
            length(lv-char-list) >= lv-text-line-length THEN 
        DO:
            lv-text-line = lv-text-line + 1.
            CREATE tt-text.
            ASSIGN 
                tt-text.TYPE     = "SPECNote"
                tt-text.tt-line  = lv-text-line
                tt-text.tt-text  = lv-char-list
                tt-text.tt-recid = RECID(po-ordl)
                lv-char-list     = "".
        END.
    END.
    FOR EACH tt-text WHERE tt-text.TYPE = "specnote" AND tt-text.tt-recid = recid(po-ordl) BREAK BY tt-text.tt-line:
        IF v-printline > 46 THEN 
        DO:         
            PAGE.
            v-printline = 0.
            {po/po-ruffino.i}
        END. 
        IF FIRST(tt-text.tt-line) THEN 
        DO:
            PUT SKIP(1).
            ASSIGN
                v-line-number = v-line-number + 1
                v-printline   = v-printline + 1.
        END.
              
        PUT tt-text.tt-text FORM "x(80)"  SKIP.
        v-printline = v-printline + 1.
    END.
END.  /* v-print-sn */

IF lCustCode THEN 
DO:
    PUT po-ordl.cust-no FORM "x(8)"  SKIP.
    v-printline = v-printline + 1.
    IF v-printline > 46 THEN 
    DO:         
        PAGE.
        v-printline = 0.
        {po/po-ruffino.i}
    END.
END.

IF lPrintMach THEN 
DO:
    cMachCode = "" .
    FOR EACH job-mch WHERE job-mch.company EQ cocode
        AND job-mch.job-no EQ po-ordl.job-no
        AND job-mch.job-no2 EQ po-ordl.job-no2
        AND job-mch.frm EQ po-ordl.s-num USE-INDEX line-idx NO-LOCK:
             
        ASSIGN 
            cMachCode = job-mch.m-code .
        LEAVE.
    END.
    IF cMachCode NE "" THEN 
    DO:
        PUT "First Resource: " cMachCode FORM "x(8)"  SKIP.
        v-printline = v-printline + 1.
    END.
END.
    
END. /* for each po-ordl record */

ASSIGN 
    v-inst        = ""
    v-tmp-lines   = 0
    j             = 0
    K             = 0
    lv-got-return = 0.
  
FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
    
    DO i = 1 TO LENGTH(notes.note_text) :        
        IF i - j >= 80 THEN
            ASSIGN j             = i
                lv-got-return = lv-got-return + 1.
              
        v-tmp-lines = ( i - j ) / 80.
        {SYS/INC/ROUNDUP.I v-tmp-lines}
        k = v-tmp-lines + lv-got-return.

        IF k < 5 THEN v-inst[k] = v-inst[k] + 
                IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                THEN SUBSTRING(notes.note_text,i,1)
                ELSE "".

        IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13) THEN
            ASSIGN
                lv-got-return = lv-got-return + 1
                j             = i.
    END.
END.

IF v-printline > 46 THEN 
DO:                  
    PAGE.
    v-printline = 0.
    {po/po-ruffino.i}
END.

IF lPrintGrandTotMsf THEN
    IF AVAILABLE ITEM AND ITEM.industry EQ "2" OR AVAILABLE itemfg THEN 
    DO:
        PUT "Grand Total MSF: " +
            TRIM(STRING(cGrandTotMsf / 1000,">>>,>>9.9<<")) AT 50 FORMAT "x(30)"
            SKIP.
    END.

ASSIGN
    v-tot-sqft   = 0
    cGrandTotMsf = 0 
    v-bot-lab[1] = "Tax        :" 
               + STRING(po-ord.tax,"->>,>>9.99").

PUT "<R53><C1>" v-inst[1] 
    "<R54><C1>" v-inst[2]
    "<R55><C1>" v-inst[3]
    "<R56><C1>" v-inst[4]
    "<R58><C60><#8><FROM><R+5><C+20><RECT> " 
    "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM "->>,>>9.99"
    "<=8><R+2> "  v-bot-lab[1] 
    "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
    /*v-bot-lab[2] */
    "<=8><R+4> Grand Total:" po-ord.t-cost FORM "->>,>>9.99" .

PUT "<FArial><R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
    " " SKIP
    " " SKIP
    " " SKIP(1)     
    "  I acknowledge the pricing on this P.O. is correct. _________________________(please sign and fax)" SKIP
    .

v-printline = v-printline + 6.

IF v-printline < 60 THEN PUT SKIP(80 - v-printline).

END. /* for each po-ord record */.
 
/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */


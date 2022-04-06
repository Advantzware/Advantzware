/* --------------------------------------------- po/po-capcity.p */
/* Purchase Order Print Program for S-8-POPRINT = CapCity                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/s-top.f}

{po/po-print.i}


DEFINE VARIABLE v-wid           AS DECIMAL   FORM ">>9.99<<" NO-UNDO.
DEFINE VARIABLE v-dep           AS DECIMAL   NO-UNDO.    
DEFINE VARIABLE v-basis-w       AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-len           AS DECIMAL   FORM "->,>>9.99" NO-UNDO.
DEFINE VARIABLE v-wid2          LIKE po-ordl.s-wid FORMAT ">>9.99" NO-UNDO. /* for recalc extened cost */
DEFINE VARIABLE v-len2          LIKE po-ordl.s-len FORMAT ">>9.99" NO-UNDO. /* for recalc extened cost */
DEFINE VARIABLE pol-counter     AS INTEGER   NO-UNDO.
DEFINE VARIABLE save_id         AS RECID.
DEFINE VARIABLE time_stamp      AS CHARACTER.
DEFINE VARIABLE v-exp-limit     AS INTEGER   NO-UNDO INIT 10.
DEFINE VARIABLE v-line-number   AS INTEGER.
DEFINE VARIABLE v-page-counter  AS INTEGER   FORMAT ">>9".
DEFINE VARIABLE v-lines-to-skip AS INTEGER.
DEFINE VARIABLE v-sname         LIKE shipto.ship-name.
DEFINE VARIABLE v-saddr         LIKE shipto.ship-addr.
DEFINE VARIABLE v-scity         LIKE shipto.ship-city.
DEFINE VARIABLE v-sstate        LIKE shipto.ship-state.
DEFINE VARIABLE v-szip          LIKE shipto.ship-zip.
DEFINE VARIABLE v-po-type       AS CHARACTER FORMAT "x(10)".
DEFINE VARIABLE v-freight-dscr  AS CHARACTER FORMAT "x(7)".
DEFINE VARIABLE v-change-dscr   AS CHARACTER FORMAT "x(7)".
DEFINE VARIABLE v-dash-line     AS CHARACTER FORMAT "x(80)" EXTENT 3.
DEFINE VARIABLE v-adders        AS LOG.
DEFINE VARIABLE xg-flag         AS LOG       INIT NO NO-UNDO.
DEFINE VARIABLE v-space         AS LOG       INIT YES.
DEFINE VARIABLE len-score       AS CHARACTER.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE BUFFER xitem    FOR item.
DEFINE VARIABLE same-score    AS ch        NO-UNDO.
DEFINE VARIABLE v-test-scr    AS LOG       NO-UNDO.
DEFINE VARIABLE v-hdr         AS CHARACTER FORMAT "x(15)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-ino-job     AS CHARACTER FORMAT "x(15)" INITIAL "" NO-UNDO.
DEFINE VARIABLE v-change-ord  AS CHARACTER FORMAT "x(35)" INITIAL "" NO-UNDO.
DEFINE VARIABLE lv-got-return AS INTEGER   NO-UNDO.


DEFINE TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty  AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20
    FIELD setups   AS DECIMAL DECIMALS 2 EXTENT 20.

/* === with xprint ====*/
DEFINE VARIABLE ls-image1    AS cha NO-UNDO.
DEFINE VARIABLE ls-image2    AS cha NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE ls-full-img2 AS cha FORM "x(200)" NO-UNDO.

DEFINE        VARIABLE v-tel           AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-fax           AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-contact       AS cha       FORM "x(20)" NO-UNDO .

DEFINE        VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE v-line-total    AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-quo-total     AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-t-tax         AS DECIMAL   EXTENT 3 NO-UNDO.
DEFINE        VARIABLE v-bot-lab       AS CHARACTER FORMAT "x(63)" EXTENT 3 NO-UNDO.
DEFINE        VARIABLE v-q-no          LIKE oe-ord.q-no NO-UNDO.
DEFINE        VARIABLE v-printline     AS INTEGER   NO-UNDO.

DEFINE        VARIABLE v-qty           LIKE po-ordl.ord-qty NO-UNDO.
DEFINE        VARIABLE v-tot-sqft      AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-vend-item     AS cha       NO-UNDO.
DEFINE        VARIABLE v-adder         AS cha       FORM "x(15)" EXTENT 5 NO-UNDO.
DEFINE        VARIABLE v-num-add       AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE        VARIABLE v-job-no        AS cha       NO-UNDO.
DEFINE        VARIABLE v-cost          AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-setup         AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-tmp-lines     AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE v-inst-lines    AS INTEGER   NO-UNDO.
DEFINE        VARIABLE v-inst          AS cha       FORM "x(80)" EXTENT 4 NO-UNDO.
DEFINE        VARIABLE lv-display-comp AS LOG       NO-UNDO.
DEFINE        VARIABLE lv-email        AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE        VARIABLE lv-cust-part    AS CHARACTER FORM "x(15)" NO-UNDO.
DEFINE        VARIABLE lv-i-name       AS cha       FORM "x(30)" NO-UNDO.

DEFINE        VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE        VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.
DEFINE        VARIABLE lv-flute        LIKE ITEM.flute NO-UNDO.
DEFINE        VARIABLE lv-reg-no       LIKE ITEM.reg-no NO-UNDO.
DEFINE        VARIABLE lv-item-rec     AS cha       NO-UNDO.
DEFINE        VARIABLE v-tot-msf       AS DECIMAL   FORM ">>>>,>>9.999" NO-UNDO.
DEFINE        VARIABLE v-out-qty       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE lv-add-line     AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE s-print-prices  AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE cCustPo         LIKE oe-ord.po-no NO-UNDO.

DEFINE        VARIABLE v-dec-fld       AS DECIMAL   NO-UNDO.  

DEFINE        VARIABLE v-lstloc        AS CHARACTER FORM "x(20)" NO-UNDO.
DEFINE        VARIABLE iPOLoadtagInt   AS INTEGER   NO-UNDO.
DEFINE        VARIABLE lRecFound       AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE cRtnChar        AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lPrintPrice     AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE cShiptoCustomer AS CHARACTER NO-UNDO.

DEFINE VARIABLE lValid   AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (
    INPUT  cocode, 
    INPUT  "BusinessFormLogo", 
    INPUT  "C" /* Logical */, 
    INPUT  NO /* check by cust */, 
    INPUT  YES /* use cust not vendor */, 
    INPUT  "" /* cust */, 
    INPUT  "" /* ship-to*/,
    OUTPUT cRtnChar, 
    OUTPUT lRecFound
    ).
    
IF lRecFound AND cRtnChar NE "" THEN DO:
    cRtnChar = DYNAMIC-FUNCTION (
                   "fFormatFilePath",
                   cRtnChar
                   ).
                   
    /* Validate the N-K-1 BusinessFormLogo image file */
    RUN FileSys_ValidateFile(
        INPUT  cRtnChar,
        OUTPUT lValid,
        OUTPUT cMessage
        ) NO-ERROR.

    IF NOT lValid THEN DO:
        MESSAGE "Unable to find image file '" + cRtnChar + "' in N-K-1 setting for BusinessFormLogo"
            VIEW-AS ALERT-BOX ERROR.
    END.
END.

ls-full-img1 = cRtnChar + ">".

v-dash-line = FILL ("_",80).

{po/po-print.f}
{ce/msfcalc.i}

RUN sys/ref/nk1look.p (INPUT cocode,
    INPUT "POLoadtag",
    INPUT "I" ,
    INPUT NO /* check by cust */,
    INPUT YES /* use cust not vendor */,
    INPUT "" /* cust */,
    INPUT "" /* ship-to*/,
    OUTPUT cRtnChar,
    OUTPUT lRecFound).
IF lRecFound THEN
    iPOLoadtagInt = INTEGER(cRtnChar) NO-ERROR. 
lPrintPrice =  s-print-prices .
IF iPOLoadtagInt EQ 2 THEN
    lPrintPrice = NO.

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

FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 
/*if avail company then
assign
 v-sname     = company.name
 v-saddr [1] = company.addr [1]
 v-saddr [2] = company.addr [2]
 v-scity     = company.city
 v-sstate    = company.state
 v-szip      = company.zip
 v-comp-add1 = company.addr[1]
 v-comp-add2 = company.city + ", " + company.st + "  " + company.zip
 v-comp-add3 = "Phone: 604.533.2545" 
 v-comp-add4 = "Fax  : 604.533.2633".
 .
 */
ASSIGN 
    v-comp-add1 = ""
    v-comp-add2 = "" 
    v-comp-add3 = ""
    v-comp-add4 = ""
    v-comp-add5 = ""
    .
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
            lv-comp-name = cust.NAME   
            .
END.

v-tot-sqft = 0.
print-po-blok:
FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,
    FIRST po-ord WHERE RECID(po-ord) EQ report.rec-id
    BREAK BY PO-ORD.PO-NO:

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
    FIND FIRST terms WHERE terms.t-code EQ po-ord.terms NO-LOCK NO-ERROR.
    FIND FIRST carrier WHERE carrier.company EQ po-ord.company 
        AND carrier.carrier EQ po-ord.carrier NO-LOCK NO-ERROR.

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
    /*
          if v-pre-printed-forms eq yes then do:
            display po-ord.po-no v-page-counter po-ord.po-date v-po-type
                    po-ord.po-change-date vend.name vend.add1 vend.add2 vend.city
                    vend.state vend.zip v-sname v-saddr [1] v-saddr [2] v-scity
                    v-sstate v-szip po-ord.buyer po-ord.contact
                    terms.dscr when avail terms po-ord.acknowledge
                    po-ord.fob-code carrier.dscr po-ord.frt-pay
                    with frame po-head.
    
            assign v-line-number = 24.
          end.
          else
          do:
            if v-company eq yes then
            do:
              display v-change-ord company.name company.addr [1] company.addr [2] po-ord.po-no
                      v-page-counter company.city company.state company.zip
                      po-ord.po-date v-po-type po-ord.po-change-date vend.name
                      vend.add1 vend.add2 vend.city vend.state vend.zip v-sname
                      v-saddr [1] v-saddr [2] v-scity v-sstate v-szip
                      v-dash-line [1]
                      po-ord.buyer po-ord.contact
                      terms.dscr when avail terms
                      po-ord.acknowledge po-ord.fob-code
                      carrier.dscr v-freight-dscr v-dash-line [2] v-dash-line [3]
                      v-hdr with frame po-head-2.
              assign v-line-number = 26.
            end.
            else
            do:
              display v-change-ord po-ord.po-no v-page-counter
                      po-ord.po-date v-po-type po-ord.po-change-date vend.name
                      vend.add1 vend.add2 vend.city vend.state vend.zip v-sname
                      v-saddr [1] v-saddr [2] v-scity v-sstate v-szip
                      v-dash-line [1]
                      po-ord.buyer po-ord.contact
                      terms.dscr when avail terms
                      po-ord.acknowledge po-ord.fob-code
                      carrier.dscr v-freight-dscr v-dash-line [2] v-dash-line [3]
                      v-hdr with frame po-head-3.
              assign v-line-number = 26.
            end.
          end.
     =========*/
    v-printline = 0.
    {po/po-capcity.i}

    FOR EACH po-ordl 
        WHERE po-ordl.company EQ po-ord.company 
        AND po-ordl.po-no EQ po-ord.po-no 
        NO-LOCK
        BY po-ordl.line:

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
            v-ino-job = po-ordl.vend-i-no.
        V-ADDER = "".
        v-vend-item = "".

        FIND item WHERE item.company EQ po-ordl.company
            AND item.i-no    EQ po-ordl.i-no
            AND po-ordl.item-type
            NO-LOCK NO-ERROR.
        v-vend-item = (IF (AVAIL ITEM AND ITEM.vend-no = po-ord.vend) THEN ITEM.vend-item ELSE "")
                        +
                      (IF (AVAIL ITEM AND ITEM.vend2-no = po-ord.vend) THEN (" " + ITEM.vend2-item) ELSE "").
        
        IF v-dec-fld = 0.08 THEN
            ASSIGN v-wid  = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                v-wid  = ( v-wid * 16 ) / 100
                v-wid  = TRUNCATE(po-ordl.s-wid,0) + v-wid
                v-len  = po-ordl.s-len - truncate(po-ordl.s-len,0)
                v-len  = ( v-len * 16 ) / 100
                v-len  = TRUNCATE(po-ordl.s-len,0) + v-len
                v-wid2 = po-ordl.s-wid
                v-len2 = po-ordl.s-len .
        ELSE

            ASSIGN v-wid  = po-ordl.s-wid
                v-len  = po-ordl.s-len
                v-wid2 = po-ordl.s-wid
                v-len2 = po-ordl.s-len.
            .

        IF AVAILABLE item AND item.mat-type EQ "B" THEN 
        DO:
            IF v-shtsiz THEN 
            DO:
                IF v-dec-fld = 0.08 THEN
                    ASSIGN v-wid = po-ordl.s-wid - truncate(po-ordl.s-wid,0)
                        v-wid = ( v-wid * 16 ) / 100
                        v-wid = TRUNCATE(po-ordl.s-wid,0) + v-wid
                        v-len = po-ordl.s-len - truncate(po-ordl.s-len,0)
                        v-len = ( v-len * 16 ) / 100
                        v-len = TRUNCATE(po-ordl.s-len,0) + v-len.
            
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
                        AND (job-mat.frm      EQ po-ordl.s-num OR
                        po-ordl.s-num EQ ?)
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
          
        END. /* avail item and item.mat-type eq "B" */
       
        v-job-no = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', po-ordl.job-no, po-ordl.job-no2))) +
            (IF po-ordl.s-num NE ? THEN "-" + string(po-ordl.s-num,"99")
            ELSE "").

        IF po-ordl.job-no = "" THEN v-job-no = "".

        IF v-job-no = "-" THEN v-job-no = "".
       
        DO i = 1 TO 5:
            IF v-adder[i] <> "" AND LENGTH(v-adder[i]) < 15 THEN
                v-adder[i] = FILL(" ", 15 - LENGTH(v-adder[i])) + v-adder[i].
        END.

        IF v-printline > 46 THEN 
        DO:         
            PAGE.
            v-printline = 0.
           {po/po-capcity.i}
        END.
       
        PUT "<C1>" po-ordl.LINE FORM ">>9"
            "<C5>" po-ordl.ord-qty FORM ">>,>>>,>>9"
            "<C16>" po-ordl.pr-qty-uom 
            "<C21>" po-ordl.i-no FORM "x(20)"
            v-adder[1] 
            "<C52>" v-job-no FORM "x(16)" SPACE(1).

        IF lPrintPrice THEN
            PUT
                "<C61.5>" po-ordl.cost FORM "->>>9.99"
                "<C69>" po-ordl.pr-uom
                "<C70>" po-ordl.t-cost FORM ">,>>>,>>9.99". 

        PUT  SKIP.

        v-printline = v-printline + 1.

        /***    gdm - 05210907
                IF po-ordl.pr-qty-uom EQ "EA" THEN
                   v-tot-msf = IF v-corr THEN ((v-len * v-wid * .007 * po-ordl.ord-qty) / 1000)
                               ELSE ((((v-len * v-wid) / 144) * po-ordl.ord-qty) / 1000).
                else do:
                     /*convert whatever the UOM is into "EACH" first*/
                     v-tot-msf = 0.
                     if po-ordl.pr-qty-uom NE "EA" then do:
                        v-tot-msf = 0.
                        run sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                                               "EA",
                                               v-basis-w,
                                               v-len,
                                               v-wid,
                                               v-dep,
                                               po-ordl.ord-qty,
                                               output v-out-qty).
                              /*now convert from "EACH" into MSF*/   
                        v-tot-msf = if v-corr THEN ((v-len * v-wid * .007 * v-out-qty) / 1000)
                                    ELSE ((((v-len * v-wid) / 144) * v-out-qty) / 1000).
                     end. 
                end.
        ***/
        /* gdm - 05210907 */
        IF po-ordl.pr-qty-uom EQ "EA" THEN 
        DO:
            RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                v-basis-w,po-ordl.s-len,po-ordl.s-wid,v-dep,
                po-ordl.ord-qty, OUTPUT v-qty).
             
            ASSIGN 
                v-tot-msf = v-qty.
        END.
        ELSE 
        DO:
            ASSIGN 
                v-tot-msf = 0.
            IF po-ordl.pr-qty-uom NE "EA" THEN 
            DO:
                ASSIGN 
                    v-tot-msf = 0.
                RUN sys/ref/convquom.p(po-ordl.pr-qty-uom:SCREEN-VALUE,
                    "MSF", v-basis-w, v-len, v-wid,v-dep,
                    po-ordl.ord-qty, OUTPUT v-out-qty).
                ASSIGN 
                    v-tot-msf = v-tot-msf.
            END.
        END.

        /* gdm - 05210907 end */

        /* gdm - 11040905 */
        ASSIGN 
            v-lstloc = "".
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ po-ordl.company
            AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.
        IF AVAILABLE itemfg THEN
            FIND LAST fg-rcpth NO-LOCK
                WHERE fg-rcpth.company   EQ itemfg.company 
                AND fg-rcpth.i-no      EQ itemfg.i-no
                AND fg-rcpth.rita-code EQ "R" NO-ERROR.
        IF AVAILABLE fg-rcpth THEN 
            FIND LAST ASI.fg-rdtlh NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code NO-ERROR.
        IF AVAILABLE fg-rdtlh THEN ASSIGN v-lstloc = fg-rdtlh.loc + "/" +
                                                     TRIM(fg-rdtlh.loc-bin). 

        IF AVAILABLE itemfg  THEN
            FIND FIRST cust-part WHERE cust-part.company = itemfg.company 
                AND cust-part.i-no = itemfg.i-no
                AND cust-part.cust-no = po-ordl.cust-no NO-LOCK NO-ERROR .

        IF AVAILABLE cust-part THEN
            lv-cust-part = cust-part.part-no .
        ELSE 
            lv-cust-part = "" .

        IF lv-cust-part = "" AND AVAILABLE itemfg THEN
            ASSIGN lv-cust-part = itemfg.part-no .


        ASSIGN 
            lv-i-name = po-ordl.i-name .
        IF lv-cust-part = "" THEN 
        DO:

            lv-cust-part = po-ordl.i-name .
            lv-i-name = "" .
        END.


        
        IF TRIM(v-lstloc) NE "" THEN
            PUT "WHS/BIN" AT 7.              
        /* gdm - 11040905 end */

        PUT "<C21>" lv-cust-part FORM "x(30)"
            " "/*v-adder[2]*/ FORM "x(8)" SPACE(1)
            "<C53>" po-ordl.due-date
            "<C63>" v-change-dscr.
        IF lPrintPrice THEN
            PUT "<C70>" v-tot-msf.
        PUT SKIP.
        v-printline = v-printline + 1.
        ASSIGN 
            v-line-number = v-line-number + 3.
        
        lv-add-line = NO.
        IF lv-i-name NE ""  THEN 
        DO:
            PUT "<C21>" po-ordl.i-name FORMAT "x(30)"             
                SKIP.
            lv-add-line = NO.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.

        /* gdm - 11040905 */
        IF TRIM(v-lstloc) NE "" THEN 
        DO:
            PUT v-lstloc AT 7 FORM "x(15)".
            lv-add-line = YES.
        END.
       
        IF po-ordl.dscr[1] NE "" OR v-setup <> 0 OR v-adder[3] <> "" THEN 
        DO:
            PUT "<C21>" po-ordl.dscr[1] FORMAT "x(30)"  
                " " v-adder[3]
                SKIP.
            lv-add-line = NO.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.
        
    
        IF po-ordl.dscr[2] NE "" OR v-adder[4] <> "" THEN 
        DO:
            PUT "<C21>" po-ordl.dscr[2] FORMAT "x(30)"              
                " " v-adder[4] SKIP.
            lv-add-line = NO.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.
        IF v-adder[5] <> "" OR v-vend-item <> "" THEN 
        DO:
            PUT "<C21>" v-vend-item  FORM "x(30)"       
                " " v-adder[5] SKIP.
            lv-add-line = NO.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.
        cCustPo = "".
        FIND FIRST oe-ord NO-LOCK
            WHERE oe-ord.company EQ po-ordl.company               /*Task# 10041302*/
            AND oe-ord.ord-no EQ po-ordl.ord-no  NO-ERROR.
        IF AVAILABLE oe-ord AND oe-ord.po-no NE "" THEN
            ASSIGN cCustPo = oe-ord.po-no .

        IF po-ordl.ord-no NE 0 THEN 
        DO:
            PUT "<C21>Order#:" STRING(po-ordl.ord-no,">>>>>>>9").
            PUT "<C35>Cust PO#:" cCustPo           /*Task# 10041302*/
                SKIP.
            lv-add-line = NO.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.          

        IF lv-add-line THEN 
        DO:
            PUT SKIP.
            v-line-number = v-line-number + 1.
            v-printline = v-printline + 1.
        END.            

        /* calc total sq feet */
        ASSIGN 
            v-basis-w = 0
            v-dep     = 0.

        IF po-ordl.item-type THEN
            FIND FIRST ITEM WHERE ITEM.company EQ po-ord.company
                AND ITEM.i-no    EQ po-ordl.i-no
                NO-LOCK NO-ERROR.
        IF AVAILABLE ITEM THEN
            ASSIGN v-basis-w = item.basis-w
                v-dep     = item.s-dep.

        IF po-ordl.pr-qty-uom EQ "MSF" THEN v-qty = po-ordl.ord-qty.
        ELSE RUN sys/ref/convquom.p(po-ordl.pr-qty-uom, "MSF",
                v-basis-w, po-ordl.s-len, po-ordl.s-wid, v-dep,
                po-ordl.ord-qty, OUTPUT v-qty).

        v-tot-sqft = v-tot-sqft + (v-qty * 1000).


        RUN calc-cost (RECID(po-ordl),OUTPUT v-cost,OUTPUT v-setup).                    

        v-cost = po-ordl.cost - (ROUND(v-setup /  v-qty,4)). /* reclac cost from setup */
        IF AVAILABLE ITEM THEN ASSIGN lv-flute  = "  Flute: " + ITEM.flute
                lv-reg-no = "Test: " + ITEM.reg-no.
        ELSE ASSIGN lv-flute  = ""
                lv-reg-no = "".

        PUT "W: " AT 25 v-wid SPACE(2) "L: " v-len  
        /*"                   "*/
        /*  "  Flute:"*/  lv-flute FORM "x(13)" /*"Test:" */ lv-reg-no FORM "x(10)".
        IF lPrintPrice THEN
            PUT  "<C61.5>" STRING(v-cost,"->>,>>9.99<<") + po-ordl.pr-uom + " $" +
                STRING(v-setup) + "SETUP" FORM "x(25)" .
        PUT SKIP.
        /* space(2) v-vend-item FORM "x(20)" */  .
     
        ASSIGN 
            v-line-number = v-line-number + 1
            v-printline   = v-printline + 1.

        RUN po/POProcs.p PERSISTENT SET hdPOProcs.
        
        RUN PO_GetLineScoresAndTypes IN hdPOProcs (
            INPUT  po-ordl.company,
            INPUT  po-ordl.po-no,
            INPUT  po-ordl.line,
            OUTPUT lv-val,
            OUTPUT lv-typ
            ).
        
        DELETE PROCEDURE hdPOProcs.        

        {po/poprints2.i}  
        
            IF v-score-types AND TRIM(len-score) NE "" THEN DO:
                IF NOT v-test-scr THEN 
                DO:
                    PUT "Score: " AT 3
                        len-score FORMAT "x(80)" SKIP.
                        
                    ASSIGN
                        v-line-number = v-line-number + 1
                        v-printline   = v-printline + 1
                        .
                END.
                ELSE IF DECIMAL(TRIM(len-score)) NE v-wid THEN 
                    DO:
                        PUT "Score: " AT 3
                            len-score FORMAT "x(80)"  SKIP.
                   
                        ASSIGN
                            v-line-number = v-line-number + 1
                            v-printline   = v-printline + 1
                            .
                    END.
            END.
        END. /* DO v-int from po/poprints2.i  */

        
        
    /*  
      repeat pol-counter = 1 to 4:
        if po-ordl.spec-i[pol-counter] ne "" then
        do:
          put po-ordl.spec-i[pol-counter] format "x(65)" at 10 skip.
          assign v-line-number = v-line-number + 1.
          v-printline = v-printline + 1.
        end.
      end.
    */
      
    /*========= old problem with return key
       ASSIGN v-tmp-lines = 0
              v-inst-lines = 0.
       FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
             IF notes.note_text <> "" THEN DO:
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                v-inst-lines = v-inst-lines + v-tmp-lines. 
             END.
       END.
    
     /*  if v-inst-lines gt 0 then v-inst-lines = v-inst-lines + 1. */
      v-printline = v-printline + v-inst-lines.
  
      IF v-printline > 46 THEN DO:                  
         PAGE.
         v-printline = 0.
         {po/po-xprnt.i}
      END.
  
      FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
          v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
              {SYS/INC/ROUNDUP.I v-tmp-lines}
              IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
                 PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                 v-printline = v-printline + 1.
              END.
             
      end.
  ==    */
    ASSIGN 
        v-inst        = ""
        v-tmp-lines   = 0
        j             = 0
        K             = 0
        lv-got-return = 0.

   
    FOR EACH notes WHERE notes.rec_key = po-ordl.rec_key NO-LOCK:
        DO i = 1 TO LENGTH(notes.note_text) :        
            IF i - j >= 80 THEN ASSIGN j             = i
                    lv-got-return = lv-got-return + 1.
                  
            v-tmp-lines = ( i - j ) / 80.
            {SYS/INC/ROUNDUP.I v-tmp-lines}
            k = v-tmp-lines + lv-got-return.

            IF k < 5 THEN v-inst[k] = v-inst[k] +
                    IF SUBSTRING(notes.note_text,i,1) <> CHR(10) THEN SUBSTRING(notes.note_text,i,1)
                    ELSE "" .              
           
            IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
                THEN 
            DO:
                lv-got-return = lv-got-return + 1.
                j = i.
            END.
        END.
    END.
    v-printline = v-printline + /* IF k < 5 THEN 4 ELSE*/  k.
    IF v-printline > 46 THEN 
    DO:                  
        PAGE.
        v-printline = 0.
       {po/po-capcity.i}
    END.

    DO i = 1 TO 4:
        IF v-inst[i] <> "" THEN 
        DO:
            PUT v-inst[i]  SKIP.          
        /*   v-printline = v-printline + 1.*/
        END.
    END.    

    PUT SKIP(1).
    ASSIGN 
        v-line-number = v-line-number + 1.
    v-printline = v-printline + 1.
  
    IF v-printline > 46 THEN 
    DO:
        PAGE.
        v-printline = 0.
          {po/po-capcity.i}
    END.
    /* === spec note print */
    IF v-print-sn THEN 
    DO:
        ASSIGN 
            v-tmp-lines  = 0
            v-inst-lines = 0
            lv-item-rec  = "".
        FIND FIRST itemfg WHERE itemfg.company = po-ordl.company
            AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
        lv-item-rec = IF po-ordl.item-type AND AVAILABLE ITEM THEN ITEM.rec_key
        ELSE IF AVAILABLE itemfg THEN itemfg.rec_key
        ELSE "".
        IF lv-item-rec <> "" THEN 
        DO:
            FOR EACH notes WHERE notes.rec_key = lv-item-rec 
                AND /*notes.note_type = "S" */  notes.note_code = "PO" NO-LOCK:
                IF notes.note_text <> "" THEN 
                DO:
                    v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                    v-inst-lines = v-inst-lines + v-tmp-lines. 
                END.
            END.
            IF v-inst-lines GT 0 THEN v-inst-lines = v-inst-lines + 1.
            v-printline = v-printline + v-inst-lines.
            IF v-printline > 46 THEN 
            DO:         
                PAGE.
                v-printline = 0.
              {po/po-capcity.i}
            END.     
    
            FOR EACH notes WHERE notes.rec_key = lv-item-rec AND 
            /*notes.note_type = "S" */  notes.note_code = "PO"  NO-LOCK:
                v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
               {SYS/INC/ROUNDUP.I v-tmp-lines}
                IF notes.note_text <> "" THEN 
                DO i = 1 TO v-tmp-lines:
                    PUT {1} SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                /*    v-printline = v-printline + 1. */
                END.
            END.
        END. /* lv-item-spec <> "" */
    END.
/* === end of specnote print */

END. /* for each po-ordl record */
  
ASSIGN 
    v-inst        = ""
    v-tmp-lines   = 0
    j             = 0
    K             = 0
    lv-got-return = 0.
  
FOR EACH notes WHERE notes.rec_key = po-ord.rec_key NO-LOCK:
    /* v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
     {SYS/INC/ROUNDUP.I v-tmp-lines}
    */
    DO i = 1 TO LENGTH(notes.note_text) :        
        IF i - j >= 80 THEN ASSIGN j             = i
                lv-got-return = lv-got-return + 1.
                    
        v-tmp-lines = ( i - j ) / 80.
              {SYS/INC/ROUNDUP.I v-tmp-lines}
        k = v-tmp-lines + lv-got-return.

        IF k < 5 THEN v-inst[k] = v-inst[k] + 
                IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                THEN SUBSTRING(notes.note_text,i,1)
                ELSE "".

        IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
            THEN 
        DO:
            lv-got-return = lv-got-return + 1.
            j = i.
        END.

    END.
          
/*   
   IF notes.note_text <> "" THEN DO i = 1 TO v-tmp-lines:
      /*PUT substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
      v-printline = v-printline + 1.                                           */
      IF i < 5  THEN  /* display upto 4 lines */
          ASSIGN v-inst[i] =  substring(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80)
                 v-printline = v-printline + 1.
      ELSE LEAVE.
   END.
*/   
END.

IF v-printline > 46 THEN 
DO:                  
    PAGE.
    v-printline = 0.
     {po/po-capcity.i}
END.

/*v-printline 46*/
IF lPrintPrice THEN
    PUT "Grand Total MSF: " +
        TRIM(STRING(v-tot-sqft / 1000,">>>,>>9.9<<")) AT 50 FORMAT "x(30)".
PUT SKIP.
IF po-ord.TYPE = "D" THEN 
DO:
  
    FIND FIRST cust NO-LOCK
        WHERE cust.company = cocode 
        AND cust.active = "X" NO-ERROR.
    cShiptoCustomer = IF po-ord.cust-no NE "" THEN po-ord.cust-no ELSE cust.cust-no .                       
    
    FIND FIRST shipto WHERE shipto.company = cocode AND 
        shipto.cust-no = cShiptoCustomer AND 
        shipto.ship-id = po-ord.ship-id NO-LOCK NO-ERROR.

    IF v-printline > 46 THEN 
    DO:                  
        PAGE.
        v-printline = 0.
       {po/po-capcity.i}
    END.

    IF AVAILABLE shipto THEN
    DO i = 1 TO 4:
        IF shipto.notes[i] <> "" THEN 
        DO:
            IF i = 1 THEN PUT SKIP(1) .
            PUT shipto.notes[i] FORMAT "x(80)" SKIP .
            v-printline = v-printline + 1 .
        END.
    END.
    
    IF v-printline > 46 THEN 
    DO:                  
        PAGE.
        v-printline = 0.
       {po/po-capcity.i}
    END.

END. /* type d */

v-tot-sqft = 0.
v-bot-lab[1] = "Tax        :"
/*vend.tax-gr + "        :       " */ + STRING(po-ord.tax,">,>>>,>>9.99").

PUT "<R53><C1>" v-inst[1] 
    "<R54><C1>" v-inst[2]
    "<R55><C1>" v-inst[3]
    "<R56><C1>" v-inst[4].
IF lPrintPrice THEN
    PUT "<R58><C59><#8><FROM><R+5><C+21><RECT> " 
        "<=8><R+1> Sub Total  :" po-ord.t-cost - po-ord.tax FORM ">,>>>,>>9.99"
        "<=8><R+2> "  v-bot-lab[1] 
        "<=8><R+3> "  " " /*PST        :" inv-head.t-inv-tax FORM "->>,>>9.99"*/
        /*v-bot-lab[2] */
        "<=8><R+4> Grand Total:" po-ord.t-cost FORM ">,>>>,>>9.99" .

/*Please acknowledge receipt and that pricing is correct on this P.O." */ 
IF v-print-terms THEN
    PUT "<FArial><R58><C1><P12><B> Terms and Conditions </B> <P9> " SKIP
        "Acknowledge this order verifying price, freight terms, quantity and delivery date." SKIP  /*Task# 10041302*/
        "Invoices will be paid to this purchase order only. " SKIP
        " " SKIP(1)     
        "" SKIP  
        /*"  I acknowledge the pricing on this P.O. is correct. _________________________(please sign and fax)"*/ SKIP
        .

v-printline = v-printline + 6.

IF v-printline < 60 THEN PUT SKIP(80 - v-printline).

END. /* for each po-ord record */.

/* END ---------------------------- Copr. 1992 - 1994  Advanced Software Inc. */


PROCEDURE calc-cost:

    DEFINE INPUT PARAMETER ip-recid AS RECID NO-UNDO.
    DEFINE OUTPUT PARAMETER op-cost AS DECIMAL NO-UNDO.
    DEFINE OUTPUT PARAMETER op-setup AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vv-qty             AS DECIMAL NO-UNDO.
    DEFINE VARIABLE vv-cost            AS DECIMAL NO-UNDO.
 
    DEFINE VARIABLE vv-setup           AS DECIMAL NO-UNDO.
    DEFINE VARIABLE li                 AS INTEGER NO-UNDO.

    DEFINE VARIABLE vv-basis-w         AS DECIMAL NO-UNDO.
    /*DEF VAR v-len AS DEC NO-UNDO.
    DEF VAR v-wid AS DEC NO-UNDO. */
    DEFINE VARIABLE vv-dep             AS DECIMAL NO-UNDO.
 
    DEFINE VARIABLE v-ord-qty          AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-added-cons-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-adder-setup     AS DECIMAL NO-UNDO.
    DEFINE BUFFER b-po-ordl FOR po-ordl.
    DEFINE BUFFER b-po-ord  FOR po-ord.
 
    FIND b-po-ordl WHERE RECID(b-po-ordl) = ip-recid NO-LOCK .
    FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ b-po-ordl.company AND
        b-po-ord.po-no   EQ b-po-ordl.po-no
        NO-LOCK.

    /*
     RUN po/po-sysct.p.
    */
    ASSIGN
        /*  v-len = (po-ordl.s-len)
          v-wid = (po-ordl.s-wid) */
        v-ord-qty = (b-po-ordl.ord-qty).
    /*{po/calc10.i v-len}
    {po/calc10.i v-wid}.
    */
    FIND FIRST e-item
        WHERE e-item.company EQ cocode
        AND e-item.i-no    EQ b-po-ordl.i-no
        NO-LOCK NO-ERROR.

    IF AVAILABLE e-item THEN
        FIND FIRST e-item-vend OF e-item
            WHERE e-item-vend.vend-no EQ b-po-ord.vend-no
            NO-LOCK NO-ERROR.

    IF AVAILABLE e-item-vend THEN 
    DO:
        FIND FIRST ITEM
            WHERE ITEM.company EQ cocode
            AND ITEM.i-no    EQ b-po-ordl.i-no
            NO-LOCK NO-ERROR.

        ASSIGN
            vv-basis-w = IF AVAILABLE ITEM THEN ITEM.basis-w ELSE vv-basis-w
            vv-dep     = IF AVAILABLE ITEM THEN ITEM.s-dep ELSE vv-dep
            vv-cost    = (b-po-ordl.cost)
            vv-qty     = (b-po-ordl.ord-qty).

        IF e-item.std-uom NE b-po-ordl.pr-qty-uom THEN
            RUN sys/ref/convquom.p(b-po-ordl.pr-qty-uom,
                e-item.std-uom, vv-basis-w,
                v-len, v-wid, vv-dep,
                vv-qty, OUTPUT vv-qty).

        vv-setup = 0.

        EMPTY TEMP-TABLE tt-eiv.
        CREATE tt-eiv.
        DO li = 1 TO 10:
            ASSIGN
                tt-eiv.run-qty[li]  = e-item-vend.run-qty[li]
                tt-eiv.run-cost[li] = e-item-vend.run-cost[li]
                tt-eiv.setups[li]   = e-item-vend.setups[li].
        END.

     
     
        IF AVAILABLE e-item-vend THEN
        DO:
        
     
            DO li = 1 TO 10:
                ASSIGN
                    tt-eiv.run-qty[li + 10]  = e-item-vend.runQtyXtra[li]
                    tt-eiv.run-cost[li + 10] = e-item-vend.runCostXtra[li]
                    tt-eiv.setups[li + 10]   = e-item-vend.setupsXtra[li].
            END.
        END.

        DO li = 1 TO 20:
            IF tt-eiv.run-qty[li] LT vv-qty THEN NEXT.
            ASSIGN
                vv-cost  = tt-eiv.run-cost[li] * vv-qty
                vv-setup = tt-eiv.setups[li].
            LEAVE.
        END.
     
        IF vv-qty <> 0 THEN vv-cost = vv-cost / vv-qty.  
        ELSE vv-cost = vv-cost.

        IF e-item.std-uom NE b-po-ordl.pr-uom THEN
            RUN sys/ref/convcuom.p(e-item.std-uom,
                b-po-ordl.pr-uom, vv-basis-w,
                v-len, v-wid, vv-dep,
                vv-cost, OUTPUT vv-cost).     
    END.
    ELSE vv-cost = b-po-ordl.cost.

    /* for adders */
    FIND FIRST job-mat WHERE job-mat.company EQ po-ordl.company
        AND job-mat.job-no  EQ po-ordl.job-no
        AND job-mat.job-no2 EQ po-ordl.job-no2
        AND job-mat.frm = po-ordl.s-num
        AND job-mat.blank-no = po-ordl.b-num
        NO-LOCK NO-ERROR.
    IF AVAILABLE job-mat THEN
        RUN po/po-adder2.p (RECID(po-ordl), RECID(job-mat),OUTPUT vv-cost, OUTPUT lv-added-cons-cost,OUTPUT lv-adder-setup).

    ASSIGN 
        op-cost  = vv-cost
        op-setup = vv-setup + lv-adder-setup.

END PROCEDURE.


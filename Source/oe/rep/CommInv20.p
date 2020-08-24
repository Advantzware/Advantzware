/* ---------------------------------------------- oe/rep/CommInv20.p  */
/* PRINT INVOICE   Xprint Standard Form             */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER ipiBolNo AS INTEGER NO-UNDO .
DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO .
{sys/inc/var.i NEW shared}
ASSIGN 
    cocode = ipcCompany .

{oe/rep/invoice.i}
{custom/notesdef.i}
DEFINE VARIABLE v-inst         AS cha       FORM "x(80)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE cStockNotes    AS cha       FORM "x(80)" EXTENT 6 NO-UNDO.

DEFINE VARIABLE v-salesman     AS CHARACTER FORMAT "x(14)" NO-UNDO.
DEFINE VARIABLE v-fob          AS CHARACTER FORMAT "x(27)" NO-UNDO.
DEFINE VARIABLE v-shipvia      LIKE carrier.dscr NO-UNDO.
DEFINE VARIABLE v-addr3        AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-sold-addr3   AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-shipto-name  AS CHARACTER FORMAT "x(30)" NO-UNDO.
DEFINE VARIABLE v-shipto-addr  AS CHARACTER FORMAT "x(30)" EXTENT 2 NO-UNDO.
DEFINE VARIABLE v-shipto-city  AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-shipto-state AS CHARACTER FORMAT "x(2)" NO-UNDO.
DEFINE VARIABLE v-shipto-zip   AS CHARACTER FORMAT "x(10)" NO-UNDO.
DEFINE VARIABLE v-line         AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-printline    AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-invhead      AS CHARACTER FORMAT "x(13)" INITIAL
    "I N V O I C E".
DEFINE VARIABLE v-pitch        LIKE asi.printer.pitch NO-UNDO.
DEFINE VARIABLE v-len          AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-hldpitch     LIKE asi.printer.pitch NO-UNDO.
DEFINE VARIABLE v-t-weight     LIKE inv-line.t-weight NO-UNDO.
DEFINE VARIABLE v-tot-cas      AS DECIMAL   FORMAT "->>>9.9999" NO-UNDO.
DEFINE VARIABLE v-tot-pallets  AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-tot-qty      AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-inv-date     AS DATE      INITIAL TODAY FORM "99/99/9999" NO-UNDO.
/*def shared var v-fr-tax as logical initial no NO-UNDO.*/
DEFINE VARIABLE v-tax-rate     AS DECIMAL   FORMAT "->>>.99" NO-UNDO.
DEFINE VARIABLE v-tax-code     LIKE stax.tax-code NO-UNDO.
DEFINE VARIABLE v-tx-rate      LIKE stax.tax-rate NO-UNDO.
DEFINE VARIABLE v-ans          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE v-date-ship    AS DATE      INITIAL TODAY NO-UNDO.
DEFINE VARIABLE v-del-no       AS INTEGER   FORMAT ">>>>>>" NO-UNDO.
DEFINE VARIABLE v-bol-cases    LIKE oe-boll.cases NO-UNDO.
DEFINE VARIABLE v-set-qty      AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-part-qty     AS DECIMAL   FORMAT "999.9999" NO-UNDO.
DEFINE VARIABLE v-net          LIKE inv-head.t-inv-rev NO-UNDO.
DEFINE VARIABLE v-case-cnt     AS CHARACTER FORMAT "x(80)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-case-line    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-part-line    AS CHARACTER NO-UNDO.
DEFINE VARIABLE tmp1           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE tmp2           AS DATE      NO-UNDO.
DEFINE VARIABLE net1           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE net2           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE net3           AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cnt            AS INTEGER   NO-UNDO.
DEFINE VARIABLE disp-frt       AS CHARACTER INIT "Freight:" FORMAT "x(8)" NO-UNDO.
DEFINE VARIABLE minus-ship     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-int          AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cBillNotes     LIKE inv-head.bill-i NO-UNDO.

DEFINE BUFFER bf-oe-bolh FOR oe-bolh.
DEFINE BUFFER bf-oe-boll FOR oe-boll.

DEFINE BUFFER xinv-head  FOR inv-head .
DEFINE BUFFER xinv-line  FOR inv-line .

DEFINE WORKFILE w-sman
    FIELD sman AS CHARACTER FORMAT "x(4)".

DEFINE VARIABLE v-ord-del-hdr  AS CHARACTER FORMAT "x(3)" INIT "Del".
DEFINE VARIABLE v-beeler-lines AS INTEGER.
DEFINE VARIABLE v-part-info    AS CHARACTER FORMAT "x(30)".
DEFINE VARIABLE v              AS INTEGER.
DEFINE VARIABLE v-bo-qty       AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-inv-qty      AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ship-qty     AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-ord-qty      AS INTEGER   FORMAT "99999" NO-UNDO.
DEFINE VARIABLE v-i-no         AS CHARACTER FORMAT "x(15)" NO-UNDO.
DEFINE VARIABLE v-i-dscr       AS CHARACTER FORMAT "x(18)" NO-UNDO.
DEFINE VARIABLE v-price        AS DECIMAL   FORMAT ">>>>9.9999" NO-UNDO.
DEFINE VARIABLE v-t-price      AS DECIMAL   FORMAT ">>>>>>9.99" NO-UNDO.
DEFINE VARIABLE v-po-no        LIKE inv-line.po-no NO-UNDO.
DEFINE VARIABLE v-bill-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-no       LIKE oe-ord.ord-no NO-UNDO.
DEFINE VARIABLE v-ord-date     LIKE oe-ord.ord-date NO-UNDO.
DEFINE VARIABLE v-ship-i       AS CHARACTER FORMAT "x(25)" NO-UNDO.
DEFINE VARIABLE v-ord-po-no    LIKE oe-ord.po-no NO-UNDO.
DEFINE VARIABLE v-price-head   AS CHARACTER FORMAT "x(5)" NO-UNDO.
DEFINE VARIABLE v-subtot-lines AS DECIMAL   NO-UNDO.
DEFINE WORKFILE w-tax
    FIELD w-dsc AS   CHARACTER
    FIELD w-tax AS   DECIMAL.
DEFINE VARIABLE v-t-tax       AS DECIMAL   EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-bot-lab     AS CHARACTER FORMAT "x(63)" EXTENT 5 NO-UNDO.
DEFINE VARIABLE v-lines       AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-inv-freight LIKE inv-head.t-inv-freight NO-UNDO.
DEFINE VARIABLE v-frt-tax     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE lv-inv-list   AS CHARACTER NO-UNDO.

FIND FIRST inv-head NO-LOCK NO-ERROR.

DEFINE VARIABLE v-tel           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-fax           AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-contact       AS cha       FORM "x(20)" NO-UNDO .

DEFINE VARIABLE v-comp-add1     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add2     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add3     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add4     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE v-comp-add5     AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-display-comp AS LOG       NO-UNDO.
DEFINE VARIABLE lv-email        AS cha       FORM "x(48)" NO-UNDO.
DEFINE VARIABLE lv-comp-name    AS cha       FORM "x(30)" NO-UNDO.
DEFINE VARIABLE lv-comp-color   AS cha       NO-UNDO.
DEFINE VARIABLE lv-other-color  AS cha       INIT "BLACK" NO-UNDO.
DEFINE VARIABLE v-page-num      AS INTEGER   NO-UNDO.
DEFINE VARIABLE vRelPo          LIKE oe-rel.po-no NO-UNDO.
DEFINE VARIABLE iPoCheck        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cPo-No          AS CHARACTER NO-UNDO.
DEFINE VARIABLE iCount          AS INTEGER   NO-UNDO.
DEFINE BUFFER bf-cust FOR cust.

DEFINE VARIABLE cRtnChar     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS CHARACTER FORMAT "x(200)" NO-UNDO.
DEFINE VARIABLE cTaxGroup    AS CHARACTER NO-UNDO .
DEFINE VARIABLE cFobCode     AS CHARACTER NO-UNDO .
DEFINE VARIABLE cCurrCode    AS CHARACTER NO-UNDO .
DEFINE VARIABLE lValid         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage       AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormLogo", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
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

ASSIGN 
    ls-full-img1 = cRtnChar + ">" .


/* ************************  Function Prototypes ********************** */
FUNCTION fnGetFOB RETURNS CHARACTER 
    (ipcRecKey AS CHARACTER) FORWARD.


FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl AND sys-ctrl.log-fld THEN lv-display-comp = YES.
ELSE lv-display-comp = NO.

FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "LOGOCOLR" NO-LOCK NO-ERROR.
IF AVAILABLE sys-ctrl THEN lv-comp-color = sys-ctrl.char-fld.
ELSE lv-comp-color = "BLACK".

FIND FIRST company WHERE company.company EQ cocode NO-LOCK. 

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
     
FIND FIRST oe-ctrl WHERE oe-ctrl.company = cocode NO-LOCK NO-ERROR.

FIND FIRST oe-bolh WHERE oe-bolh.company = cocode AND
    oe-bolh.bol-no = ipiBolNo USE-INDEX bol-no NO-LOCK NO-ERROR.

FIND FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company 
    AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK NO-ERROR.
IF AVAILABLE oe-boll THEN
    FIND FIRST oe-ord NO-LOCK
        WHERE oe-ord.company = cocode AND
        oe-ord.ord-no = oe-boll.ord-no NO-ERROR.

FIND FIRST cust WHERE cust.company = oe-bolh.company
    AND cust.cust-no = oe-bolh.cust-no NO-LOCK NO-ERROR.
cCurrCode = IF AVAILABLE cust AND cust.curr-code NE "" THEN cust.curr-code ELSE "USD" .
v-del-no = 0.
      
IF AVAILABLE oe-bolh THEN 
DO:
       
    FIND FIRST shipto WHERE shipto.company  = oe-bolh.company AND
        shipto.cust-no = oe-bolh.cust-no AND
        shipto.ship-id = oe-bolh.ship-id NO-LOCK NO-ERROR.
    IF AVAILABLE shipto THEN
        ASSIGN  v-shipto-name    = shipto.ship-name
            v-shipto-addr[1] = shipto.ship-addr[1]
            v-shipto-addr[2] = shipto.ship-addr[2]
            v-shipto-city    = shipto.ship-city
            v-shipto-state   = shipto.ship-state
            v-shipto-zip     = shipto.ship-zip.

    cFobCode = fnGetFOB(oe-bolh.rec_key) .

END. /* avail oe-bolh */

      
DO TRANSACTION:
       
    IF oe-bolh.bol-date NE ? THEN v-inv-date = oe-bolh.bol-date .
    IF cFobCode EQ "" THEN cFobCode = IF AVAILABLE oe-ord THEN oe-ord.fob-code ELSE "" .
        
    IF cFobCode BEGINS "ORIG" THEN
        ASSIGN v-fob = "Origin".
    ELSE
        ASSIGN v-fob = "Destination".
        
    ASSIGN 
        cBillNotes[1] = ""
        cBillNotes[2] = ""
        cBillNotes[3] = ""
        cBillNotes[4] = "".
        
    ASSIGN
        cBillNotes[1] = oe-bolh.ship-i[1]
        cBillNotes[2] = oe-bolh.ship-i[2]
        cBillNotes[3] = oe-bolh.ship-i[3]
        cBillNotes[4] = oe-bolh.ship-i[4]
        .

    FIND FIRST carrier WHERE carrier.company = oe-bolh.company AND
        carrier.carrier = oe-bolh.carrier NO-LOCK NO-ERROR.
    IF AVAILABLE carrier THEN
        ASSIGN v-shipvia = carrier.dscr.
    ELSE
        ASSIGN v-shipvia = "".
    ASSIGN
        v-addr3      = cust.city + ", " + cust.state + "  " + cust.zip
        v-sold-addr3 = v-shipto-city + ", " + v-shipto-state +
              "  " + v-shipto-zip
        v-line       = 1
        v-printline  = 0.
    cTaxGroup = IF AVAILABLE shipto AND shipto.tax-code NE ""
        THEN shipto.tax-code ELSE oe-ord.tax-gr .
    FIND FIRST stax WHERE stax.company = cocode AND
        stax.tax-group = cTaxGroup NO-LOCK NO-ERROR.
        
    IF NOT AVAILABLE stax THEN
        FIND FIRST stax WHERE stax.tax-group EQ cTaxGroup
            NO-LOCK NO-ERROR.
   
    IF AVAILABLE stax THEN
        ASSIGN v-tax-rate    = stax.tax-rate[1] +
                              stax.tax-rate[2] + stax.tax-rate[3]
            v-tax-code[1] = stax.tax-code[1]
            v-tax-code[2] = stax.tax-code[2]
            v-tax-code[3] = stax.tax-code[3]
            v-tx-rate[1]  = stax.tax-rate[1]
            v-tx-rate[2]  = stax.tax-rate[2]
            v-tx-rate[3]  = stax.tax-rate[3].

    ASSIGN 
        v-tot-pallets = 0.
    cPo-No = "".
        
    /* for each xinv-line no-lock where xinv-line.r-no = inv-head.r-no
       break by xinv-line.i-no:*/
    FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK BREAK BY oe-boll.i-no:

        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company = cocode 
            AND oe-ordl.ord-no = oe-boll.ord-no 
            AND oe-ordl.i-no = oe-boll.i-no NO-ERROR.
        
        DO i = 1 TO 3:
            IF oe-ord.sman[i] NE "" THEN 
            DO:
                CREATE w-sman.
                ASSIGN 
                    w-sman.sman = oe-ord.sman[i].
            END.
        END.
        ASSIGN 
            v-tot-qty  = v-tot-qty + oe-boll.qty
            v-t-weight = v-t-weight + (ROUND(oe-boll.weight /
                            oe-ordl.qty, 2) * oe-boll.qty).

        IF oe-boll.po-no NE "" THEN 
        DO:
            cPo-No = cPo-No + oe-boll.po-no + ",". 
        END.
         
        FOR EACH bf-oe-bolh NO-LOCK WHERE bf-oe-bolh.b-no = oe-bolh.b-no:
            FOR EACH bf-oe-boll NO-LOCK WHERE bf-oe-boll.company = oe-bolh.company AND
                bf-oe-boll.b-no = oe-bolh.b-no AND
                bf-oe-boll.i-no = oe-boll.i-no AND
                bf-oe-boll.ord-no = oe-boll.ord-no:

                /** Bill Of Lading TOTAL CASES **/
                ASSIGN 
                    v-bol-cases = v-bol-cases + oe-boll.cases.
                RUN oe/pallcalc.p (ROWID(oe-boll), OUTPUT v-int).
                v-tot-pallets = v-tot-pallets + v-int.
            END. /* each oe-boll */
            ASSIGN 
                v-date-ship = oe-bolh.bol-date.

        END. /* each bf-oe-bolh */
        IF LAST-OF(oe-boll.i-no) THEN 
        DO:
            IF oe-ordl.est-no NE "" THEN
            DO:
                FIND FIRST eb WHERE eb.company = oe-boll.company AND
                    eb.est-no = oe-ordl.est-no AND
                    eb.e-num = oe-ordl.e-num AND
                    eb.form-no = oe-ordl.form-no AND
                    eb.blank-no = oe-ordl.blank-no NO-LOCK NO-ERROR.

                IF oe-ordl.form-no = 0 AND oe-ord.est-type = 2 THEN
                DO:
                    FOR EACH fg-set NO-LOCK WHERE fg-set.company = oe-ordl.company
                        AND fg-set.set-no = oe-ordl.i-no:
                        ASSIGN 
                            v-set-qty = v-set-qty + fg-set.QtyPerSet.
                    END.
                    IF v-set-qty = 0 THEN
                        ASSIGN v-set-qty = 1.
                    FOR EACH eb NO-LOCK WHERE eb.company = oe-ordl.company AND
                        eb.est-no = oe-ordl.est-no AND
                        eb.e-num = oe-ordl.e-num AND
                        eb.form-no NE 0:
                        FIND fg-set WHERE fg-set.company = oe-ordl.company AND
                            fg-set.set-no = oe-ordl.i-no  AND
                            fg-set.part-no = eb.stock-no NO-LOCK NO-ERROR.

                        IF AVAILABLE fg-set AND fg-set.QtyPerSet NE 0 THEN
                            ASSIGN v-part-qty = fg-set.QtyPerSet / v-set-qty.
                        ELSE
                            ASSIGN v-part-qty = 1 / v-set-qty.


                        IF eb.cas-cnt = 0 THEN
                            ASSIGN v-tot-cas = ROUND((v-t-weight * v-part-qty) /
                                      eb.cas-wt, 2).
                        ELSE
                            ASSIGN v-tot-cas = ROUND((v-tot-qty * v-part-qty) /
                                      eb.cas-cnt, 2).
                        IF v-bol-cases NE 0 THEN
                            ASSIGN v-tot-cas = v-bol-cases.
                    END. /* each eb */
                END. /* do */
                ELSE
                    IF AVAILABLE eb THEN
                    DO:
                        IF eb.cas-cnt = 0 THEN
                            ASSIGN v-tot-cas = ROUND(v-t-weight / eb.cas-wt, 2).
                        ELSE
                            ASSIGN v-tot-cas = ROUND(v-tot-qty / eb.cas-cnt, 2).
                        IF v-bol-cases NE 0 THEN
                            ASSIGN v-tot-cas = v-bol-cases.
                    END. /* do */
            END. /* est-no ne "" */
            ASSIGN
                v-t-weight = 0
                v-tot-cas  = 0
                v-tot-qty  = 0.
        END. /* last-of i-no */
    END. /* each oe-boll */

    DO iCount = 1 TO NUM-ENTRIES(cPo-No):
        IF ENTRY(1,cPo-No) NE ENTRY(iCount,cPo-No) AND ENTRY(iCount,cPo-No) NE ""  THEN iPoCheck = YES. 
    END.
    
    /** Build Salesman Id String **/
    v-salesman = "".
    FOR EACH w-sman BREAK BY w-sman.sman:
        IF FIRST-OF(w-sman.sman) THEN
            ASSIGN v-salesman = v-salesman + w-sman.sman.
        DELETE w-sman.
    END.

        
    FIND FIRST oe-ordl NO-LOCK
        WHERE oe-ordl.company = oe-bolh.company
        AND oe-ordl.ord-no EQ oe-ord.ord-no NO-ERROR.

    IF AVAILABLE oe-ordl THEN
    DO:
        ASSIGN 
            v-price-head = oe-ordl.pr-uom.
          
        IF AVAILABLE oe-ord THEN
        DO:
            ASSIGN 
                v-bill-i   = oe-ord.bill-i[1]
                v-ord-no   = oe-ord.ord-no
                v-ord-date = oe-ord.ord-date
                .
        END.
    END.

    FIND FIRST oe-boll WHERE oe-boll.company EQ oe-bolh.company 
        AND oe-boll.b-no EQ oe-bolh.b-no AND oe-boll.po-no NE "" NO-LOCK NO-ERROR.
    IF AVAILABLE oe-boll THEN 
        ASSIGN v-ord-po-no = IF iPoCheck EQ YES THEN "See below" ELSE oe-boll.po-no.
         

    {oe/rep/CommInv20.i}

    v-subtot-lines = 0.
    v-t-tax = 0.
    FOR EACH oe-boll WHERE oe-boll.company EQ oe-bolh.company
        AND oe-boll.b-no EQ oe-bolh.b-no NO-LOCK:
        ASSIGN 
            v-case-line = ""
            v-part-line = ""
            v-case-cnt  = "".
        v-frt-tax = v-frt-tax + oe-boll.freight .
        v-inv-freight = v-inv-freight + oe-boll.freight .

        IF v-printline > 50 THEN 
        DO:
            PAGE.
            v-printline = 0.
            {oe/rep/CommInv20.i}
        END.

        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company = cocode 
            AND oe-ordl.ord-no = oe-boll.ord-no 
            AND oe-ordl.i-no = oe-boll.i-no NO-ERROR.

        FOR EACH bf-oe-boll
            WHERE bf-oe-boll.company EQ oe-boll.company
            AND bf-oe-boll.ord-no  EQ oe-boll.ord-no
            AND bf-oe-boll.b-no    EQ oe-boll.b-no
            AND bf-oe-boll.i-no    EQ oe-boll.i-no
            AND bf-oe-boll.line    EQ oe-boll.line
            AND bf-oe-boll.po-no   EQ oe-boll.po-no
            USE-INDEX bol-no NO-LOCK:

            /** Build Case Count Display Lines **/
            IF bf-oe-boll.cases NE 0 AND bf-oe-boll.qty-case NE 0 THEN
                ASSIGN v-case-line = STRING(bf-oe-boll.cases) + " @ " +
                                     string(bf-oe-boll.qty-case).
            ELSE ASSIGN v-case-line = "".
            IF bf-oe-boll.partial NE 0 THEN
                ASSIGN v-part-line = "1" + " @ " + string(bf-oe-boll.partial).
            ELSE ASSIGN v-part-line = "".

            DO i = 1 TO 5:
                IF (80 - length(v-case-cnt[i])) > length(v-case-line) AND
                    v-case-line NE "" THEN
                    ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-case-line + "  "
                        v-case-line   = "".
                IF (80 - length(v-case-cnt[i])) > length(v-part-line) AND
                    v-part-line NE "" THEN
                    ASSIGN v-case-cnt[i] = v-case-cnt[i] + v-part-line + "  "
                        v-part-line   = "".
            END. /* 1 to 5 */
        END. /* each oe-boll */

        ASSIGN 
            v-line = v-line + 1
            .

        ASSIGN
            lv-inv-list = ""
            v-ship-qty  = oe-boll.qty .
             

           

        IF AVAILABLE oe-ordl THEN 
        DO:
            v-bo-qty = IF (oe-ordl.qty - v-ship-qty -
                oe-ordl.t-ship-qty) < 0 then 0 else
                             (oe-ordl.qty - v-ship-qty -
                              oe-ordl.t-ship-qty).
        END.
        /*else
          assign v-bo-qty = if ( inv-line.qty - v-ship-qty ) < 0
                              then 0 else inv-line.qty - v-ship-qty.*/

        v-beeler-lines = 0.
        DO v = 1 TO 3:
            v-part-info = IF v EQ 1 THEN oe-ordl.part-dscr1
            ELSE
                IF v EQ 2 THEN oe-ordl.part-dscr2
                ELSE           TRIM(lv-inv-list).

            IF v-part-info NE "" OR (v = 1 AND oe-ordl.part-no <> "") THEN
                v-beeler-lines = v-beeler-lines + 1.
        END.
           
        /*v-printline = v-printline + v-beeler-lines.*/
 
        ASSIGN 
            v-inv-qty      = oe-ordl.qty /* task 03170618 inv-qty*/
            v-i-no         = oe-ordl.i-no
            v-i-dscr       = oe-ordl.i-name
            v-price        = oe-ordl.price * (1 - (oe-ordl.disc / 100))
            v-t-price      = oe-ordl.t-price
            v-subtot-lines = v-subtot-lines + oe-ordl.t-price
            v-ord-qty      = oe-ordl.qty .
            
        IF oe-ordl.tax AND AVAILABLE stax THEN
        DO i = 1 TO 5:
            IF stax.tax-code1[i] NE "" THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr1[i]
                    w-tax      = ROUND((IF stax.company EQ "yes" THEN v-t-price
                                                              ELSE oe-ordl.t-price) *
                                    stax.tax-rate1[i] / 100,2)
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END.

        IF v-t-price NE oe-ordl.t-price THEN 
        DO:
            CREATE w-tax.
            ASSIGN
                w-dsc   = "******ITEM TOTAL:"
                w-tax   = v-t-price
                v-lines = v-lines + 1.
        END.
            
        v-price-head = oe-ordl.pr-uom.
  
        IF NOT lPrintQtyAll THEN 
        DO:
            PUT SPACE(1) v-inv-qty FORMAT "->>>>>>9" SPACE(1)
                v-ship-qty  FORMAT "->>>>>>9" SPACE(1)
                oe-ordl.ord-no FORMAT ">>>>>>9" SPACE(2)
                v-i-no  FORMAT "x(15)" SPACE(3)
                v-i-dscr  FORMAT "x(25)" SPACE(3)
                v-price  FORMAT "$->>>,>>9.99" /*"$->>,>>9.99<<"*/ SPACE(1)
                v-price-head 
                oe-ordl.t-price  FORMAT "$->>>>,>>9.99" /*"$->>>,>>9.99"*/                     
                SKIP.
        END.
        ELSE 
        DO:
            PUT SPACE(1)v-ord-qty  FORMAT "->>>>>>9" SPACE(1)
                v-inv-qty  FORMAT "->>>>>>9" SPACE(1)
                oe-ordl.ord-no FORMAT ">>>>>>9" SPACE(3)
                v-i-no  FORMAT "x(15)" SPACE(3)
                v-i-dscr  FORMAT "x(25)" SPACE(2)
                v-price  FORMAT "$->>>,>>9.99" /*"$->>,>>9.99<<"*/ SPACE(2)
                v-price-head 
                oe-ordl.t-price  FORMAT "$->>>>,>>9.99" /*"$->>>,>>9.99"                     */
                SKIP.

        END.

        v-printline = v-printline + 1.
      

        DO v = 1 TO 3:
            v-part-info = IF v EQ 1 THEN oe-ordl.part-dscr1
            ELSE
                IF v EQ 2 THEN oe-ordl.part-dscr2
                ELSE           TRIM(lv-inv-list).
            IF v-part-info NE "" OR  (v = 1 AND oe-ordl.part-no <> "") THEN 
            DO:
                IF v = 1 THEN 
                DO:
                    IF lPrintQtyAll THEN 
                    DO:
                        PUT SPACE(1) v-ship-qty FORMAT "->>>>>>9" .
                        IF LENGTH(oe-boll.po-no) LE 8 THEN 
                        DO:
                            PUT  SPACE(11) oe-boll.po-no FORMAT "x(8)" SPACE(1)   oe-ordl.part-no SPACE(3) v-part-info SKIP.
                        END.
                        ELSE 
                        DO: 
                            PUT  SPACE(2) oe-boll.po-no FORMAT "x(15)" SPACE(3)   oe-ordl.part-no SPACE(3) v-part-info SKIP.
                        END.
                    END.
                    ELSE 
                    DO:
                        IF LENGTH(oe-boll.po-no) LE 8 THEN 
                        DO:
                            PUT  SPACE(19) oe-boll.po-no FORMAT "x(8)" SPACE(1)   oe-ordl.part-no SPACE(3) v-part-info SKIP.
                        END.
                        ELSE 
                        DO: 
                            PUT  SPACE(10) oe-boll.po-no FORMAT "x(15)" SPACE(3)   oe-ordl.part-no SPACE(3) v-part-info SKIP.
                        END.
                    END. /* else do*/

                END.
                ELSE
                    IF v = 2 THEN  PUT /*SPACE(10)  v-po-no FORMAT "x(15)"*/ SPACE(47) v-part-info SKIP.
                    ELSE          PUT SPACE(26) "Previous Invoice(s): " v-part-info SKIP.
                v-printline = v-printline + 1.
            END.
        END.
        PUT SKIP(1).
        v-printline = v-printline + 1.
       
        IF v-print-dept AND AVAILABLE oe-ordl THEN
        DO:
            FIND FIRST job-hdr WHERE
                job-hdr.company EQ cocode AND
                job-hdr.job-no  EQ oe-ordl.job-no AND
                job-hdr.job-no2 EQ oe-ordl.job-no2
                NO-LOCK NO-ERROR.

            IF AVAILABLE job-hdr THEN
            DO:
                FIND FIRST job WHERE
                    job.company EQ cocode AND
                    job.job     EQ job-hdr.job AND
                    job.job-no  EQ job-hdr.job-no AND
                    job.job-no2 EQ job-hdr.job-no2
                    NO-LOCK NO-ERROR.
                 
                IF AVAILABLE job THEN
                DO:
                    FOR EACH notes WHERE
                        notes.rec_key EQ job.rec_key AND /*for capitol task 06180806*/
                        CAN-DO(v-depts,notes.note_code)
                        NO-LOCK
                        BY notes.note_code:
                        
                        v-tmp-lines = LENGTH(NOTES.NOTE_TEXT) / 80.
                        {SYS/INC/ROUNDUP.I v-tmp-lines}
                    
                        DO i = 1 TO v-tmp-lines:
                            IF v-printline > 50 THEN 
                            DO:
                                PAGE.
                                v-printline = 0.
                                {oe/rep/CommInv20.i}
                            END.
                    
                            PUT SUBSTRING(NOTES.NOTE_TEXT,(1 + 80 * (i - 1)), 80) FORM "x(80)" SKIP.              
                            v-printline = v-printline + 1.
                        END.
                    END.

                    /* custom/notesprt.i {1} = table name 
                    {2} = note variable name with extent 
                    {3} =  variable's extent */

                    RELEASE job.
                END.

                RELEASE job-hdr.
            END.
        END.

    END. /* each inv-line */

    FOR EACH oe-ordm
        WHERE oe-ordm.company EQ oe-ord.company
        AND oe-ordm.ord-no  EQ oe-ord.ord-no
        AND oe-ordm.bill    EQ "Y" BREAK BY oe-ordm.ord-no WITH FRAME detailm:
        IF FIRST(oe-ordm.ord-no) THEN
        DO:
            IF v-printline > 50 THEN 
            DO:
                PAGE.
                v-printline = 0.
                {oe/rep/CommInv20.i}
            END.
            PUT "** Miscellaneous Items **" AT 23 SKIP(1).
            ASSIGN 
                v-printline = v-printline + 2.
        END.
            
        PUT oe-ordm.charge AT 10 oe-ordm.dscr oe-ordm.amt FORMAT "$->>,>>9.99"  SKIP.
        ASSIGN
            v-subtot-lines = v-subtot-lines + oe-ordm.amt
            v-printline    = v-printline + 1.
        IF oe-ordm.tax AND AVAILABLE stax THEN
        DO i = 1 TO 5:
            IF stax.tax-code1[i] NE "" THEN 
            DO:
                CREATE w-tax.
                ASSIGN
                    w-dsc      = stax.tax-dscr1[i]
                    w-tax      = IF stax.company EQ "yes" THEN v-t-price
                            ELSE oe-ordm.amt
                    w-tax      = ROUND(w-tax * (1 + (stax.tax-rate1[i] / 100)),2) - w-tax
                    v-t-price  = v-t-price + w-tax
                    v-t-tax[i] = v-t-tax[i] + w-tax
                    v-lines    = v-lines + 1.
            END.
        END.

        IF v-t-price NE oe-ordm.amt THEN 
        DO:
            CREATE w-tax.
            ASSIGN
                w-dsc   = "******ITEM TOTAL:"
                w-tax   = v-t-price
                v-lines = v-lines + 1.
        END.
        IF v-printline > 53 THEN 
        DO:
            PAGE.
            v-printline = 0.
            {oe/rep/CommInv20.i}
        END.

    END. /* each inv-misc */

    IF v-prntinst THEN 
    DO:

        {custom/notesprt.i oe-bolh v-inst 4}
        DO i = 1 TO 4:
            IF v-inst[i] <> "" THEN 
            DO:                
                IF v-printline > 50 THEN 
                DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/CommInv20.i}
                END.
                PUT v-inst[i] SKIP.
                v-printline = v-printline + 1.
            END.
        END.

        DO i = 1 TO 4:
            IF cBillNotes[i] <> "" THEN 
            DO:
                IF v-printline > 50 THEN 
                DO:
                    PAGE.
                    v-printline = 0.
                    {oe/rep/CommInv20.i}
                END.
                PUT cBillNotes[i] SKIP.
                v-printline = v-printline + 1.
            END.
        END.
    END.
        
    /*v-frt-tax = inv-head.t-inv-freight.*/
        
    IF cTaxGroup <> "" AND
        oe-bolh.freight NE 0 AND oe-bolh.frt-pay EQ "B" AND v-frt-tax <> 0 AND AVAILABLE stax THEN
    DO i = 1 TO 5:

        IF stax.tax-code1[i] NE "" AND stax.tax-frt1[i] EQ YES THEN 
        DO:
            CREATE w-tax.
            ASSIGN
                w-dsc      = stax.tax-dscr1[i]
                w-tax      = ROUND((v-frt-tax) *
                                        stax.tax-rate1[i] / 100,2)
                v-frt-tax  = v-frt-tax + w-tax
                v-t-tax[i] = v-t-tax[i] + w-tax
                v-lines    = v-lines + 1.
        END.
    END. 

        
END. /* DO TRANSACTION avail inv-head */

DO i = 1 TO 5:
    v-bot-lab[i] = IF v-t-tax[i] NE 0 THEN
        ((IF AVAILABLE stax THEN STRING(CAPS(stax.tax-code1[i] + " TAX"),"x(7)") 
        ELSE FILL(" ",7) ) +
        fill(" ",4) + ":" +
        string(v-t-tax[i],"->>,>>>,>>9.99")) ELSE "".
END.
v-inv-freight = IF oe-bolh.freight NE 0 AND oe-bolh.frt-pay EQ "B" THEN v-inv-freight ELSE 0.
FOR EACH bf-cust NO-LOCK
    WHERE bf-cust.company EQ cocode
    AND bf-cust.ACTIVE EQ "X":

    RUN pNotes(INPUT bf-cust.rec_key, OUTPUT cStockNotes).
        
    PUT "<p8><R56><C3>" cStockNotes[1] SKIP
        "<R57><C3>" cStockNotes[2] SKIP
        "<R58><C3>" cStockNotes[3] SKIP
        "<R59><C3>" cStockNotes[4] SKIP
        "<p10>".
        
END.
PUT "<R60><C3>Made in the USA" .

IF v-bot-lab[4] <> "" THEN
    PUT "<R56><C56><#8><FROM><R+8><C+27><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->,>>>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORMAT "->>,>>>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2] 
        "<=8><R+4> " v-bot-lab[3] 
        "<=8><R+5> " v-bot-lab[4] 
        "<=8><R+6> " v-bot-lab[5] 
        "<=8><R+7> Grand Total:" (v-subtot-lines + v-inv-freight + v-t-tax[1] + v-t-tax[2] + v-t-tax[3] + v-t-tax[4] + v-t-tax[5]) FORM "$->,>>>,>>9.99" SPACE(1) cCurrCode .
ELSE
    PUT "<R56><C56><#8><FROM><R+6><C+27><RECT> " 
        "<=8> Sub Total  :" v-subtot-lines FORM "$->,>>>,>>9.99"
        "<=8><R+1> Freight    :" v-inv-freight FORMAT "->>,>>>,>>9.99"
        "<=8><R+2> " v-bot-lab[1] 
        "<=8><R+3> " v-bot-lab[2] 
        "<=8><R+4> " v-bot-lab[3] 
        "<=8><R+5> Grand Total:" (v-subtot-lines + v-inv-freight + v-t-tax[1] + v-t-tax[2] + v-t-tax[3])  FORM "$->,>>>,>>9.99" SPACE(1) cCurrCode .

ASSIGN
    v-printline = v-printline + 6
    v-page-num  = PAGE-NUMBER.
PAGE.
 
/*   end. /* each xinv-head */*/

PROCEDURE pNotes:

    DEFINE INPUT PARAMETER reckey LIKE cust.rec_key NO-UNDO.
    DEFINE OUTPUT PARAMETER cNotes AS cha FORM "x(80)" EXTENT 6 NO-UNDO.
    
    ASSIGN 
        v-tmp-lines   = 0
        j             = 0
        K             = 0
        lv-got-return = 0.

    FOR EACH notes WHERE notes.rec_key = reckey 
        AND notes.note_type = "G"
        AND notes.note_group = "BN" NO-LOCK:

        IF v-prev-note-rec <> ? AND
            v-prev-note-rec <> RECID(notes) THEN v-prev-extent = /*v-prev-extent +*/ k.
        DO i = 1 TO LENGTH(notes.note_text) :        
            IF i - j >= lv-line-chars THEN ASSIGN j             = i
                    lv-got-return = lv-got-return + 1.
                  
            v-tmp-lines = ( i - j ) / lv-line-chars.
            {SYS/INC/ROUNDUP.I v-tmp-lines}
            k = v-tmp-lines + lv-got-return +
                IF (v-prev-note-rec <> RECID(notes) AND v-prev-note-rec <> ?) THEN v-prev-extent ELSE 0.

            IF k > 0 AND k <= 6 THEN cNotes[k] = cNotes[k] + 
                    IF SUBSTRING(notes.note_text,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                    THEN SUBSTRING(notes.note_text,i,1)
                    ELSE "" .              
           
            IF SUBSTRING(note_text,i,1) = CHR(10) OR SUBSTRING(note_text,i,1) = CHR(13)                 
                THEN 
            DO:
                lv-got-return = lv-got-return + 1.
                j = i.
            END.
        END.
        ASSIGN 
            v-prev-note-rec = RECID(notes)
            j               = 0
            lv-got-return   = 0.
    
        IF k > 6 THEN LEAVE.
    END.

END PROCEDURE.




FUNCTION fnGetFOB RETURNS CHARACTER 
    (ipcRecKey AS CHARACTER):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEFINE VARIABLE result     AS CHARACTER NO-UNDO.
    DEFINE VARIABLE v-fob-code AS CHARACTER NO-UNDO.
    
    FIND FIRST reftable WHERE
        reftable.reftable EQ "oe-bolh.lot-no" AND
        reftable.rec_key  EQ ipcRecKey
        USE-INDEX rec_key
        NO-LOCK NO-ERROR.

    IF AVAILABLE reftable THEN
        ASSIGN v-fob-code = IF reftable.CODE EQ "O" THEN "ORIG"
                          ELSE IF reftable.CODE EQ "D" THEN "DEST"
                          ELSE reftable.CODE.
    ELSE
        ASSIGN v-fob-code = "".
        
    RELEASE reftable.
    
    RESULT = v-fob-code.
    RETURN result.

END FUNCTION.

/* END ---------------------------------- copr. 1996 Advanced Software, Inc. */

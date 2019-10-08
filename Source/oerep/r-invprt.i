
/*------------------------------------------------------------------------
    File        : r-invprt.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon Jul 10 16:13:32 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE BUFFER b-{&head}1  FOR {&head}.
DEFINE BUFFER buf-{&head} FOR {&head}.
DEFINE BUFFER b2-{&head}  FOR {&head}.
DEFINE BUFFER buf-{&line} FOR {&line}.
DEFINE BUFFER buf-{&line}1 FOR {&line}.
/* ********************  Preprocessor Definitions  ******************** */
&Scoped-define FRAME-NAME FRAME-A

/* ***************************  Main Block  *************************** */
ON CHOOSE OF btn-ok IN FRAME FRAME-A /* OK */
    DO:
        DEFINE    VARIABLE      lv-fax-type    AS cha     NO-UNDO.
        DEFINE    VARIABLE      vlSkipRec      AS LOG     NO-UNDO.
        DEFINE    VARIABLE      ll-secure      AS LOG     INIT YES NO-UNDO.
        DEFINE VARIABLE lCheckHoldStat AS LOGICAL NO-UNDO .
        DEFINE BUFFER bf-cust FOR cust.

        DO WITH FRAME {&FRAME-NAME}:
            ASSIGN {&DISPLAYED-OBJECTS}
                tb_collate lv-scr-num-copies
                tb_cust-copy tb_office-copy tb_sman-copy
                /* gdm - 12080817 */ tb_setcomp.

            IF fi_broker-bol:SENSITIVE THEN
                ASSIGN fi_broker-bol.
        END.

        IF rs_no_PN:HIDDEN = NO THEN
            ASSIGN rs_no_PN
                svi-print-item = rs_no_PN.

        IF tb_prt-zero-qty:HIDDEN = NO THEN
            ASSIGN tb_prt-zero-qty
                s-print-zero-qty = tb_prt-zero-qty.
        /*   Ticket - 18922  */
        /*IF tb_posted AND begin_inv <> END_inv THEN DO:
           MESSAGE "Beginning Invoice# and Ending Invoice# must be the same for Reprint Posted Invoice."
               VIEW-AS ALERT-BOX ERROR.
           APPLY "entry" TO begin_inv.
           RETURN .
        END.*/

        IF fi_broker-bol:SENSITIVE AND
            fi_broker-bol:SCREEN-VALUE NE "" AND
            begin_inv NE end_inv THEN
        DO:
            MESSAGE "For Broker BOL# to be used, Beginning and Ending Invoice# must be the same."
                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
            LEAVE.
        END.

        /* gdm - 12080817 */
        ASSIGN
            tb_setcomp      = LOGICAL(tb_setcomp:SCREEN-VALUE)
            lv-multi-faxout = IF rd-dest = 4 AND begin_cust <> END_cust THEN YES 
                       ELSE NO.

        IF is-xprint-form AND rd-dest = 4 THEN lv-multi-faxout = YES.

        lv-fax-type = IF lv-multi-faxout THEN "MULTI" ELSE "CUSTOMER".

        IF lv-multi-faxout AND rd_sort <> "Customer" THEN 
        DO:
            MESSAGE "Invoice must be sorted by Customer for Fax ." VIEW-AS ALERT-BOX ERROR.
            APPLY "entry" TO rd_sort.
            RETURN.
        END.

        IF v-invpass THEN
        DO:
            RUN sys/ref/d-passwd.w(6, OUTPUT ll-secure).
            IF NOT ll-secure THEN LEAVE.
        END.

        IF begin_bol EQ end_bol THEN 
        DO:

            FOR EACH buf-{&head} WHERE
                buf-{&head}.company EQ cocode AND
                buf-{&head}.cust-no GE begin_cust AND
                buf-{&head}.cust-no LE end_cust AND
                      INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                buf-{&head}.inv-no GE begin_inv AND
                buf-{&head}.inv-no LE end_inv)) AND
                buf-{&head}.{&bolno} EQ begin_bol
                NO-LOCK:
                ASSIGN 
                    lCheckHoldStat = YES .
                LEAVE .
            END.
            IF NOT lCheckHoldStat THEN
                FOR EACH buf-{&head} WHERE
                    buf-{&head}.company EQ cocode AND
                    buf-{&head}.cust-no GE begin_cust AND
                    buf-{&head}.cust-no LE end_cust AND
                      INDEX(vcHoldStats, buf-{&head}.stat) <> 0 AND
                    ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv)) AND
                    buf-{&head}.{&bolno} EQ begin_bol
                    NO-LOCK:
                    MESSAGE "Invoice " + STRING(buf-{&head}.inv-no) + " with Bol " + 
                        STRING(buf-{&head}.{&bolno}) + " will not print, status must be approved" 
                        VIEW-AS ALERT-BOX ERROR.
                    APPLY "entry" TO begin_inv.
                    RETURN.
                END.
        END.

        CASE rd-dest:
            WHEN 1 THEN 
                ASSIGN 
                    LvOutputSelection = "Printer".
            WHEN 2 THEN 
                ASSIGN 
                    LvOutputSelection = "Screen". 
            WHEN 3 THEN 
                ASSIGN 
                    LvOutputSelection = "File".
            WHEN 4 THEN 
                ASSIGN 
                    LvOutputSelection = "Fax".
            WHEN 5 THEN 
                ASSIGN 
                    LvOutputSelection = "Email".
            WHEN 6 THEN 
                ASSIGN 
                    LvOutputSelection = "Port".
        END CASE.

        /*#BL# - If not emailing and a customer in the range is "Paperless" then abort */
        IF rd-dest <> 5 
            /*       AND CAN-FIND(FIRST cust                    */
            /*                 WHERE cust.company EQ cocode     */
            /*                   AND cust.cust-no GE begin_cust */
            /*                   AND cust.cust-no LE end_cust   */
            /*                   AND cust.log-field[1])         */
            AND NOT tb_override-email
            THEN 
        DO:
            /* To be removed after version 16.4.8 ***
            IF tb_posted THEN 
            DO:
                FOR EACH b-ar-inv FIELDS(company cust-no ship-id) WHERE
                    b-ar-inv.company EQ cocode AND
                    b-ar-inv.inv-no GE begin_inv AND
                    b-ar-inv.inv-no LE end_inv AND
                    b-ar-inv.cust-no GE begin_cust AND
                    b-ar-inv.cust-no LE end_cust AND
                    b-ar-inv.printed EQ tb_reprint
                    NO-LOCK
                    BREAK BY b-ar-inv.company
                    BY b-ar-inv.cust-no:
                    IF FIRST-OF(b-ar-inv.cust-no) THEN 
                    DO:      
                        FIND FIRST bf-cust NO-LOCK 
                            WHERE bf-cust.company EQ cocode
                            AND bf-cust.cust-no EQ b-ar-inv.cust-no 
                            AND bf-cust.log-field[1] NO-ERROR.
                        IF AVAILABLE bf-cust THEN 
                        DO:
                            MESSAGE 'Customer ' bf-cust.cust-no ' is set as "Paperless Invoice".' SKIP
                                'Please select "Output To Email" or check "Ignore Paperless Setting".'
                                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                            RETURN.
                        END.
                    END.
                END.  /* for each b-ar-inv */
            END.  /* tb_posted */
            ELSE 
            **** end to be removed version */
            DO:
                FOR EACH buf-{&head} WHERE
                    buf-{&head}.company EQ cocode AND
                    buf-{&head}.cust-no GE begin_cust AND
                    buf-{&head}.cust-no LE end_cust AND
                    ("{&head}" NE "ar-inv" OR buf-{&head}.posted = tb_posted) AND 
                  INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                    ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv)) AND
                    (IF "{&head}" EQ "ar-inv" THEN buf-{&head}.inv-date GE begin_date
                      AND buf-{&head}.inv-date LE end_date ELSE TRUE) AND
                    buf-{&head}.{&bolno} GE begin_bol AND
                    buf-{&head}.{&bolno} LE end_bol
                    NO-LOCK
                    BREAK BY buf-{&head}.company
                    BY buf-{&head}.cust-no:
                    IF FIRST-OF(buf-{&head}.cust-no) THEN 
                    DO:
                        FIND FIRST bf-cust NO-LOCK
                            WHERE bf-cust.company EQ cocode
                            AND bf-cust.cust-no EQ buf-{&head}.cust-no 
                            AND bf-cust.log-field[1] NO-ERROR.
                        IF AVAILABLE bf-cust THEN 
                        DO:
                            MESSAGE 'Customer ' bf-cust.cust-no ' is set as "Paperless Invoice".' SKIP
                                'Please select "Output To Email" or check "Ignore Paperless Setting".'
                                VIEW-AS ALERT-BOX ERROR BUTTONS OK.
                            RETURN.
                        END.
                    END.
                END.  /* for each buf-{&head} */

            END.    /* else do tb_posted */
        END.


        IF NOT rd-dest:SCREEN-VALUE = '5' AND NOT rd-dest:SCREEN-VALUE = '1' THEN
        DO:
            /* To be removed after version 17 *****
            IF tb_posted THEN
            DO:
                /* If sys-ctrl-shipto "INVPRINT" found, then do this. */
                IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT") THEN
                DO:
                    FOR EACH b-ar-inv FIELDS(company cust-no ship-id) WHERE
                        b-ar-inv.company EQ cocode AND
                        b-ar-inv.inv-no GE begin_inv AND
                        b-ar-inv.inv-no LE end_inv AND
                        b-ar-inv.cust-no GE begin_cust AND
                        b-ar-inv.cust-no LE end_cust AND
                        b-ar-inv.printed EQ tb_reprint
                        NO-LOCK
                        BREAK BY b-ar-inv.company
                        BY b-ar-inv.cust-no
                        BY b-ar-inv.ship-id:

                        IF FIRST-OF(b-ar-inv.ship-id) THEN
                        DO:
                            /* Find INVPRINT shipto for customer, ship location and a form name. */
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                                sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                            /* If not found, then find INVPRINT shipto for customer and a form name. */
                            IF NOT AVAILABLE sys-ctrl-shipto THEN
                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company = cocode AND
                                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                                    sys-ctrl-shipto.cust-vend = YES AND
                                    sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                                    sys-ctrl-shipto.ship-id = '' AND /* stacey */
                                    sys-ctrl-shipto.char-fld > ''
                                    NO-LOCK NO-ERROR.

                            IF AVAILABLE sys-ctrl-shipto THEN
                            DO:                                
                                RUN SetInvPostForm(sys-ctrl-shipto.char-fld).
                                v-print-fmt = sys-ctrl-shipto.char-fld.
                            END.
                            ELSE
                            DO:
                                RUN SetInvPostForm(vcDefaultForm).
                                v-print-fmt = vcDefaultForm.
                            END.

                            RUN run-report-posted(b-ar-inv.cust-no, TRUE).
                            RUN GenerateReport(INPUT lv-fax-type,
                                INPUT b-ar-inv.cust-no,
                                INPUT b-ar-inv.cust-no,
                                INPUT lv-fax-image).
                        END.
                    END.
                END. /*sys-ctrl-ship-to*/
                ELSE
                DO:
                    RUN SetInvPostForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.

                    RUN run-report-posted("", FALSE).
                    RUN GenerateReport(INPUT lv-fax-type,
                        INPUT begin_cust,
                        INPUT end_cust,
                        INPUT lv-fax-image).

                END.
            END.
            ELSE /* not posted*/
            *** end of to be removed */
            DO:

                IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT") THEN
                DO:
                    FOR EACH buf-{&head} WHERE
                        buf-{&head}.company EQ cocode AND
                        buf-{&head}.cust-no GE begin_cust AND
                        buf-{&head}.cust-no LE end_cust AND
                        ("{&head}" NE "ar-inv" OR buf-{&head}.posted = tb_posted) AND 
                  INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                        ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                        (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                        buf-{&head}.inv-no GE begin_inv AND
                        buf-{&head}.inv-no LE end_inv)) AND
                        (IF "{&head}" EQ "ar-inv" THEN buf-{&head}.inv-date GE begin_date
                             AND buf-{&head}.inv-date LE end_date ELSE TRUE) AND                        
                        buf-{&head}.{&bolno} GE begin_bol AND
                        buf-{&head}.{&bolno} LE end_bol
                        NO-LOCK,
                        FIRST b-cust WHERE
                        b-cust.company EQ cocode AND
                        b-cust.cust-no EQ buf-{&head}.cust-no AND
                        ((b-cust.inv-meth EQ ? AND buf-{&head}.{&multiinvoice}) OR
                        (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice})) 
                        NO-LOCK
                        BREAK BY buf-{&head}.company
                        BY buf-{&head}.cust-no
                        BY buf-{&head}.sold-no:

                        IF FIRST-OF(buf-{&head}.sold-no) THEN
                        DO:
                            /* Find INVPRINT shipto for customer, ship location and a form name. */
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                                sys-ctrl-shipto.ship-id = STRING(buf-{&head}.sold-no) AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                            /* If not found, then find INVPRINT shipto for customer and a form name. */
                            IF NOT AVAILABLE sys-ctrl-shipto THEN
                                FIND FIRST sys-ctrl-shipto WHERE
                                    sys-ctrl-shipto.company = cocode AND
                                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                                    sys-ctrl-shipto.cust-vend = YES AND
                                    sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                                    sys-ctrl-shipto.ship-id = '' AND /* stacey */
                                    sys-ctrl-shipto.char-fld > ''
                                    NO-LOCK NO-ERROR.

                            IF AVAILABLE sys-ctrl-shipto THEN
                            DO:
                                IF "{&head}" EQ "ar-inv" THEN
                                    RUN SetInvPostForm(sys-ctrl-shipto.char-fld). 
                                ELSE 
                                  RUN SetInvForm(sys-ctrl-shipto.char-fld).
                                v-print-fmt = sys-ctrl-shipto.char-fld.
                            END.
                            ELSE
                            DO:
                                IF "{&head}" EQ "ar-inv" THEN
                                    RUN SetInvPostForm(vcDefaultForm). 
                                ELSE                                 
                                    RUN SetInvForm(vcDefaultForm).
                                v-print-fmt = vcDefaultForm.
                            END.

                            RUN run-report(buf-{&head}.cust-no,buf-{&head}.sold-no, TRUE).
                            RUN GenerateReport(INPUT lv-fax-type,
                                INPUT buf-{&head}.cust-no,
                                INPUT buf-{&head}.cust-no,
                                INPUT lv-fax-image).
                        END.
                    END.
                END. /*can-find sys-ctrl and not posted*/
                ELSE
                DO:
                    IF "{&head}" EQ "ar-inv" THEN
                        RUN SetInvPostForm(vcDefaultForm). 
                    ELSE                                          
                        RUN SetInvForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.

                    RUN run-report("","", FALSE).
                    RUN GenerateReport(INPUT lv-fax-type,
                        INPUT begin_cust,
                        INPUT end_cust,
                        INPUT lv-fax-image).
                END.
            END. /* not posted */
        END. /*not rd-dest = 5*/

        IF rd-dest:SCREEN-VALUE = '5' THEN 
        DO:

            IF NOT tb_BatchMail:CHECKED THEN 
            DO:
                IF begin_cust <> end_cust THEN 
                DO:

                    /*          IF NOT tb_BatchMail:SENSITIVE THEN DO: */

                    MESSAGE 'Please check Batch E-Mail to send to multiple customers in the specified range.'
                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
                    APPLY 'ENTRY':U TO end_cust.
                    RETURN NO-APPLY.
                /* These statements will not run due to the return no-apply           */
                /*          END. */       
                /*         FIND FIRST b1-cust NO-LOCK                                 */
                /*              WHERE b1-cust.company = cocode                        */
                /*                AND b1-cust.active  = 'X' NO-ERROR.                 */
                /*                                                                    */
                /*         IF AVAIL b1-cust THEN RUN output-to-mail (b1-cust.cust-no).*/
                /*                                                                    */
                /*         ELSE DO:                                                   */
                /*           MESSAGE 'In-House Customer not defined.'                 */
                /*             VIEW-AS ALERT-BOX INFO BUTTONS OK.                     */
                /*           RETURN.                                                  */
                /*         END.                                                       */
                END.

                ELSE RUN BatchMail (begin_cust, begin_cust).

            END.

            ELSE RUN BatchMail (begin_cust, end_cust).
        END.

        IF rd-dest:SCREEN-VALUE = '1' THEN 
        DO:
            /* To be removed after version 16.4.8 ****
            IF tb_posted THEN 
            DO:
                FIND FIRST b-ar-inv  WHERE
                    b-ar-inv.company EQ cocode AND
                    b-ar-inv.inv-no GE begin_inv AND
                    b-ar-inv.inv-no LE end_inv AND
                    b-ar-inv.cust-no GE begin_cust AND
                    b-ar-inv.cust-no LE end_cust AND
                    b-ar-inv.printed EQ tb_reprint
                    NO-LOCK NO-ERROR.

                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                    sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE sys-ctrl-shipto THEN
                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company = cocode AND
                        sys-ctrl-shipto.NAME = "INVPRINT" AND
                        sys-ctrl-shipto.cust-vend = YES AND
                        sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                        sys-ctrl-shipto.ship-id = '' AND /* stacey */
                        sys-ctrl-shipto.char-fld > ''
                        NO-LOCK NO-ERROR.

                IF AVAILABLE sys-ctrl-shipto THEN
                DO:
                    RUN SetInvPostForm(sys-ctrl-shipto.char-fld).
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                END.
                ELSE
                DO:
                    RUN SetInvPostForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.
                END.

                RUN run-report-posted("", FALSE).
                RUN GenerateReport(INPUT lv-fax-type,
                    INPUT begin_cust,
                    INPUT end_cust,
                    INPUT lv-fax-image).
            END.

            ELSE
            **** end of to be removed section **/ 
            DO:
                FIND FIRST buf-{&head} WHERE
                    buf-{&head}.company EQ cocode AND
                    buf-{&head}.cust-no GE begin_cust AND
                    buf-{&head}.cust-no LE end_cust AND
             INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND
                    ("{&head}" NE "ar-inv" OR buf-{&head}.posted = tb_posted) AND 
                    ((NOT tb_reprint AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv))
                    NO-LOCK NO-ERROR.

                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                    sys-ctrl-shipto.ship-id = STRING(buf-{&head}.sold-no) AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

                IF NOT AVAILABLE sys-ctrl-shipto THEN
                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company = cocode AND
                        sys-ctrl-shipto.NAME = "INVPRINT" AND
                        sys-ctrl-shipto.cust-vend = YES AND
                        sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                        sys-ctrl-shipto.ship-id = '' AND /* stacey */
                        sys-ctrl-shipto.char-fld > ''
                        NO-LOCK NO-ERROR.

                IF AVAILABLE sys-ctrl-shipto THEN
                DO:
                    IF "{&head}" EQ "ar-inv" THEN
                        RUN SetInvPostForm(sys-ctrl-shipto.char-fld). 
                    ELSE                       
                    RUN SetInvForm(sys-ctrl-shipto.char-fld).
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                END.
                ELSE 
                DO:
                    IF "{&head}" EQ "ar-inv" THEN
                        RUN SetInvPostForm(vcDefaultForm). 
                    ELSE                       
                        RUN SetInvForm(vcDefaultForm).
                    v-print-fmt = vcDefaultForm.
                END.

                RUN run-report("","", FALSE).
                RUN GenerateReport(INPUT lv-fax-type,
                    INPUT begin_cust,
                    INPUT end_cust,
                    INPUT lv-fax-image).
            END.
        END.  /* rd-dest:Screen-value = 1*/


        IF v-ftp-done THEN MESSAGE "File Export/FTP is completed." VIEW-AS ALERT-BOX INFORMATION.
        OS-DELETE VALUE(init-dir + "\Invoice.pdf").
        RELEASE {&head} .
        RELEASE {&line} .
        RELEASE inv-misc .

    END.
    
ON LEAVE OF end_inv IN FRAME FRAME-A /* Ending Invoice# */
    DO:
        ASSIGN {&self-name}.
        IF begin_inv = END_inv THEN 
        DO:
            FIND FIRST {&head} WHERE {&head}.company = g_company
                AND {&head}.inv-no = begin_inv NO-LOCK NO-ERROR.
            IF AVAILABLE {&head} THEN ASSIGN begin_cust:SCREEN-VALUE = {&head}.cust-no
                    end_cust:SCREEN-VALUE   = {&head}.cust-no.

            IF AVAILABLE {&head} AND {&head}.{&bolno} NE 0 THEN
                FIND FIRST oe-bolh
                    WHERE oe-bolh.company EQ cocode
                    AND oe-bolh.{&bolno}  EQ {&head}.{&bolno}
                    NO-LOCK NO-ERROR.

            IF AVAILABLE oe-bolh THEN
                ASSIGN begin_date:SCREEN-VALUE = STRING(oe-bolh.bol-date)
                    end_date:SCREEN-VALUE   = STRING(oe-bolh.bol-date).

            RUN set-broker-bol-proc.
        END.
    END.
    
PROCEDURE setBOLRange:
  
    DO WITH FRAME frame-a: 
        IF INT(begin_bol:SCREEN-VALUE) NE 0                         AND
            INT(begin_bol:SCREEN-VALUE) EQ INT(end_bol:SCREEN-VALUE) THEN 
        DO :

            /* Multi Invoice Customer must print all of their Invoices */
            FOR FIRST oe-bolh NO-LOCK
                WHERE oe-bolh.company EQ cocode
                AND oe-bolh.{&bolno}  EQ INT(begin_bol:SCREEN-VALUE):

                ASSIGN
                    begin_cust:SCREEN-VALUE = oe-bolh.cust-no
                    end_cust:SCREEN-VALUE   = oe-bolh.cust-no.

                FOR EACH {&head} NO-LOCK
                    WHERE {&head}.company EQ oe-bolh.company
                    AND {&head}.cust-no EQ oe-bolh.cust-no
                    AND {&head}.inv-no GE INT(begin_inv:SCREEN-VALUE)
                    AND {&head}.inv-no LE INT(end_inv:SCREEN-VALUE)
                    AND {&head}.{&multiinvoice}              
                    AND INDEX(vcHoldStats, {&head}.stat) EQ 0:

                    ASSIGN
                        tb_reprint:SCREEN-VALUE = STRING({&head}.printed)
                        tb_posted:SCREEN-VALUE  = STRING({&head}.posted).

                    FOR EACH b-{&head}1 NO-LOCK
                        WHERE b-{&head}1.company EQ {&head}.company
                        AND b-{&head}1.cust-no EQ {&head}.cust-no
                        AND b-{&head}1.inv-no  EQ {&head}.inv-no
                        AND b-{&head}1.{&multiinvoice} EQ NO                
                        AND INDEX(vcHoldStats, b-{&head}1.stat) EQ 0:

                        IF b-{&head}1.{&bolno} LT INT(begin_bol:SCREEN-VALUE) THEN
                            begin_bol:SCREEN-VALUE = STRING(b-{&head}1.{&bolno}).

                        IF b-{&head}1.{&bolno} GT INT(end_bol:SCREEN-VALUE) THEN
                            end_bol:SCREEN-VALUE = STRING(b-{&head}1.{&bolno}).
                    END.
                    IF int(begin_bol:SCREEN-VALUE) EQ 0 THEN begin_bol:SCREEN-VALUE = "0".
                    IF int(end_bol:SCREEN-VALUE) EQ 0 THEN end_bol:SCREEN-VALUE = "99999999".
                END.
            END.
        END. 
    
    FOR EACH oe-bolh NO-LOCK
        WHERE oe-bolh.company EQ cocode
        AND oe-bolh.{&bolno}  GE INT(begin_bol:SCREEN-VALUE)
        AND oe-bolh.{&bolno}  LE INT(end_bol:SCREEN-VALUE):

        IF oe-bolh.bol-date LT DATE(begin_date:SCREEN-VALUE) THEN
            begin_date:SCREEN-VALUE = STRING(oe-bolh.bol-date).

        IF oe-bolh.bol-date GT DATE(end_date:SCREEN-VALUE) THEN
            end_date:SCREEN-VALUE = STRING(oe-bolh.bol-date).
    END.
    END.     
END PROCEDURE.
    
PROCEDURE BatchMail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER icBegCustNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icEndCustNo  AS CHARACTER NO-UNDO.

    DEFINE BUFFER b1-{&head}2      FOR {&head}.
    DEFINE BUFFER b2-cust   FOR cust.
    DEFINE BUFFER b1-ar-inv FOR ar-inv.
    DEFINE VARIABLE lEmailed AS LOG NO-UNDO.

    ASSIGN                   
        finv         = begin_inv
        tinv         = end_inv
        fdate        = begin_date
        tdate        = end_date
        fbol         = begin_bol
        tbol         = end_bol
        v-reprint    = tb_reprint
        v-sort       = rd_sort BEGINS "Customer"
        v-prntinst   = tb_prt-inst
        v-print-dept = tb_print-dept.

    IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
            v-depts      = fi_depts:SCREEN-VALUE.
    /**** to be removed after version 16.4.8 ***
    IF tb_posted  THEN 
    DO:

        FOR EACH  b1-ar-inv
            WHERE  b1-ar-inv.company              EQ cocode
            AND  b1-ar-inv.inv-no               GE finv
            AND  b1-ar-inv.inv-no               LE tinv  
            AND  b1-ar-inv.cust-no              GE icBegCustNo
            AND  b1-ar-inv.cust-no              LE icEndCustNo
            /*AND (b1-ar-inv.posted               EQ NO OR
                 b1-ar-inv.posted               EQ v-posted)
            AND  b1-ar-inv.printed              EQ v-print                                           */
            AND  b1-ar-inv.printed  = tb_reprint
            AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = b1-ar-inv.x-no /*AND ar-invl.amt <> 0*/ )
            USE-INDEX inv-no NO-LOCK,
            FIRST b2-cust OF b1-ar-inv WHERE (b2-cust.log-field[1] OR NOT tb_Batchmail OR tb_override-email) NO-LOCK                    
            BREAK BY b1-ar-inv.cust-no:

            IF FIRST-OF (b1-ar-inv.cust-no) THEN 
            DO:

                ASSIGN  
                    vlSkipRec   = YES
                    vcBegCustNo = b1-ar-inv.cust-no
                    vcEndCustNo = b1-ar-inv.cust-no.
                FIND FIRST ar-invl WHERE ar-invl.x-no = b1-ar-inv.x-no
                    NO-LOCK NO-ERROR.
                ASSIGN 
                    vSoldToNo = ""
                    vShipToNo = "". 
                IF AVAILABLE ar-invl THEN 
                DO:
                    FIND oe-ord WHERE oe-ord.company = b1-ar-inv.company
                        AND oe-ord.ord-no = ar-invl.ord-no
                        NO-LOCK NO-ERROR.
                    vSoldToNo = IF AVAILABLE oe-ord THEN oe-ord.sold-id ELSE "". 
                    vShipToNo = b1-ar-inv.ship-id.
                END.
                RUN output-to-mail (b1-ar-inv.cust-no).
            END.
            lEmailed = YES.
        END.
        /* =-=== end of posted invoices =====*/

        FOR EACH b1-{&head}2
            WHERE b1-{&head}2.company         EQ cocode
            AND b1-{&head}2.cust-no         GE icBegCustNo
            AND b1-{&head}2.cust-no         LE icEndCustNo         
            AND INDEX(vcHoldStats, b1-{&head}2.stat) EQ 0
            AND (((NOT v-reprint) AND b1-{&head}2.inv-no EQ 0) OR
            (v-reprint AND b1-{&head}2.inv-no NE 0 AND
            b1-{&head}2.inv-no        GE finv AND
            b1-{&head}2.inv-no        LE tinv))
            NO-LOCK,

            FIRST b2-cust
            WHERE b2-cust.company EQ cocode
            AND b2-cust.cust-no EQ b1-{&head}2.cust-no
            AND (b2-cust.log-field[1] OR NOT tb_Batchmail OR tb_override-email)
            AND ((b2-cust.inv-meth EQ ? AND b1-{&head}2.{&multiinvoice}) OR
            (b2-cust.inv-meth NE ? AND NOT b1-{&head}2.{&multiinvoice}))

            /*AND CAN-FIND(FIRST {&line} OF b1-{&head}2)*/
            NO-LOCK
            BREAK BY b2-cust.cust-no:

            IF FIRST-OF (b2-cust.cust-no) THEN 
            DO:

                ASSIGN  
                    vlSkipRec   = YES
                    vcBegCustNo = b1-{&head}2.cust-no
                    vcEndCustNo = b1-{&head}2.cust-no.
                vSoldToNo = "".
                IF b1-{&head}2.{&multiinvoice} THEN 
                DO: 
                    FIND FIRST b2-{&head} 
                        WHERE b2-{&head}.company       EQ b1-{&head}2.company
                        AND b2-{&head}.cust-no       EQ b1-{&head}2.cust-no
                        AND b2-{&head}.inv-no        EQ b1-{&head}2.inv-no
                        AND b2-{&head}.{&multiinvoice} EQ NO            
                        AND INDEX(vcHoldStats, b2-{&head}.stat) EQ 0 NO-LOCK NO-ERROR.
                    IF AVAILABLE b2-{&head} THEN
                        FIND FIRST {&line} NO-LOCK WHERE {&line}.{&rno} EQ b2-{&head}.{&rno}  
                          AND {&line}.ord-no NE 0 NO-ERROR.  
                END.  
                ELSE FIND FIRST {&line} /* OF b1-{&head}2 */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.
                IF AVAILABLE {&line} THEN 
                DO:
                    FIND oe-ord WHERE oe-ord.company = b1-{&head}2.company
                        AND oe-ord.ord-no = {&line}.ord-no
                        NO-LOCK NO-ERROR.
                    vSoldToNo = IF AVAILABLE oe-ord THEN oe-ord.sold-id ELSE "". 
                    vShipToNo = STRING(b1-{&head}2.sold-no).
                END.

                RUN output-to-mail (b1-{&head}2.cust-no).
            END.
            lEmailed = YES.
        END.

    END.

    ELSE
    **** end section to be removed */ 
    DO:  /* not tb_post */

        FOR EACH b1-{&head}2
            WHERE b1-{&head}2.company         EQ cocode
            AND b1-{&head}2.cust-no         GE icBegCustNo
            AND b1-{&head}2.cust-no         LE icEndCustNo         
            AND INDEX(vcHoldStats, b1-{&head}2.stat) EQ 0
            AND (((NOT v-reprint) AND b1-{&head}2.inv-no EQ 0) OR
                  (v-reprint AND b1-{&head}2.inv-no NE 0 AND          
                  b1-{&head}2.inv-no        GE finv AND 
                  b1-{&head}2.inv-no        LE tinv))
            AND (IF "{&head}" EQ "ar-inv" THEN b1-{&head}2.inv-date GE begin_date
                 AND b1-{&head}2.inv-date LE end_date ELSE TRUE)
            NO-LOCK,
            FIRST b2-cust
            WHERE b2-cust.company EQ cocode
            AND b2-cust.cust-no EQ b1-{&head}2.cust-no
            AND (b2-cust.log-field[1] OR NOT tb_Batchmail OR tb_override-email)
            AND ((b2-cust.inv-meth EQ ? AND b1-{&head}2.{&multiinvoice}) OR
            (b2-cust.inv-meth NE ? AND NOT b1-{&head}2.{&multiinvoice}))
            /*AND CAN-FIND(FIRST {&line} OF b1-{&head}2)*/
            NO-LOCK
            BREAK BY b2-cust.cust-no :

            IF FIRST-OF (b2-cust.cust-no) THEN 
            DO:

                ASSIGN  
                    vlSkipRec   = YES
                    vcBegCustNo = b1-{&head}2.cust-no
                    vcEndCustNo = b1-{&head}2.cust-no.
                vSoldToNo = "".
                IF b1-{&head}2.{&multiinvoice} THEN 
                DO: 
                    FIND FIRST b2-{&head} 
                        WHERE b2-{&head}.company       EQ b1-{&head}2.company
                        AND b2-{&head}.cust-no       EQ b1-{&head}2.cust-no
                        AND b2-{&head}.inv-no        EQ b1-{&head}2.inv-no
                        AND b2-{&head}.{&multiinvoice} EQ NO            
                        AND INDEX(vcHoldStats, b2-{&head}.stat) EQ 0 NO-LOCK NO-ERROR.
                    IF AVAILABLE b2-{&head} THEN
                        FIND FIRST {&line} /*OF b2-{&head} */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.  
                END.  
                ELSE FIND FIRST {&line} /* OF b1-{&head}2 */ NO-LOCK WHERE {&line}.ord-no NE 0 NO-ERROR.
                IF AVAILABLE {&line} THEN 
                DO:
                    FIND oe-ord WHERE oe-ord.company = b1-{&head}2.company
                        AND oe-ord.ord-no = {&line}.ord-no
                        NO-LOCK NO-ERROR.
                    vSoldToNo = IF AVAILABLE oe-ord THEN oe-ord.sold-id ELSE "". 
                    vShipToNo = STRING(b1-{&head}2.sold-no).
                END.
                /*           MESSAGE "3 batchMail:  " AVAIL {&line} "   soldTo:"  vSoldToNo "," {&line}.ord-no      */
                /*               "," AVAIL oe-ord "," oe-ord.sold-no RECID(oe-ord) SKIP                               */
                /*               "cust: " b2-cust.cust-no  "  inv#: " b1-{&head}2.inv-no  " r-no:  " b1-{&head}2.{&rno} */
                /*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                   */

                RUN output-to-mail (b1-{&head}2.cust-no).
            END.
            lEmailed = YES.
        END.
    END.

    IF NOT lEmailed AND NOT tb_override-email THEN 
    DO:
        FIND FIRST b2-cust WHERE b2-cust.company EQ cocode
            AND b2-cust.cust-no GE begin_cust
            AND b2-cust.cust-no LE end_cust
            AND b2-cust.log-field[1] = NO
            NO-LOCK NO-ERROR.
        IF AVAILABLE b2-cust THEN
            MESSAGE 'Only customers with "Paperless Invoice" checked can be emailed.  To email all customers in the range, please check "Ignore Paperless Setting" or update the customer file.'
                VIEW-AS ALERT-BOX WARNING BUTTONS OK.
    /*      FIND b2-cust WHERE b2-cust.company = cocode                       */
    /*                     AND b2-cust.cust-no = begin_cust NO-LOCK NO-ERROR. */
    /*      IF AVAIL b2-cust AND b2-cust.log-field[1] THEN                                                                                         */
    /*         MESSAGE "Sorry, Invoice will not print,  the customer is email only because the Email Only toggle box in customer file is checked." */
    /*             VIEW-AS ALERT-BOX WARNING BUTTONS OK.                                                                                           */
    /*      ELSE                                                                                                                                   */
    /*          IF AVAIL b2-cust AND NOT b2-cust.log-field[1] THEN                                           */
    /*          MESSAGE "Customer is Mail Only because Email Only toggle box in customer file is unchecked." */
    /*              VIEW-AS ALERT-BOX WARNING BUTTONS OK.                                                    */

    END.

END PROCEDURE.
    
PROCEDURE create-save-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DISABLE TRIGGERS FOR LOAD OF {&line}.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.

    FOR EACH {&line} WHERE {&line}.{&rno} EQ b-{&head}1.{&rno}:
        CREATE save-line.
        ASSIGN
            save-line.reftable = "save-line" + v-term-id
            save-line.val[1]   = {&line}.{&rno}
            save-line.val[2]   = {&head}.{&rno}
            save-line.val[3]   = INT(RECID({&line}))
            {&line}.{&rno}     = {&head}.{&rno}.
    END.

    FOR EACH inv-misc WHERE inv-misc.{&miscrno} EQ b-{&head}1.{&rno}:
        CREATE save-line.
        ASSIGN
            save-line.reftable  = "save-line" + v-term-id
            save-line.val[1]    = inv-misc.{&miscrno}
            save-line.val[2]    = {&head}.{&rno}
            save-line.val[3]    = INT(RECID(inv-misc))
            inv-misc.{&miscrno} = {&head}.{&rno}.
    END.

END PROCEDURE.


PROCEDURE output-to-mail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:
        /*** To be removed after version 16.4.8 ***
        IF tb_posted THEN
        DO:
            IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "INVPRINT") THEN
                FOR EACH b-ar-inv FIELDS(company cust-no ship-id ) WHERE
                    b-ar-inv.company EQ cocode AND
                    b-ar-inv.inv-no GE begin_inv AND
                    b-ar-inv.inv-no LE end_inv AND
                    ((b-ar-inv.cust-no GE begin_cust AND b-ar-inv.cust-no LE end_cust AND NOT tb_BatchMail)
                    OR 
                    (b-ar-inv.cust-no EQ icCustNo AND tb_BatchMail)
                    ) AND
                    b-ar-inv.printed EQ tb_reprint
                    NO-LOCK
                    BREAK BY b-ar-inv.company
                    BY b-ar-inv.cust-no:

                    IF FIRST-OF(b-ar-inv.cust-no) THEN
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company = cocode AND
                            sys-ctrl-shipto.NAME = "INVPRINT" AND
                            sys-ctrl-shipto.cust-vend = YES AND
                            sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                            sys-ctrl-shipto.ship-id = b-ar-inv.ship-id AND
                            sys-ctrl-shipto.char-fld > ''
                            NO-LOCK NO-ERROR.

                        IF NOT AVAILABLE sys-ctrl-shipto THEN
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                        IF AVAILABLE sys-ctrl-shipto THEN
                        DO:
                            IF "{&head}" EQ "ar-inv" THEN
                                RUN SetInvPostForm(sys-ctrl-shipto.char-fld). 
                            ELSE                               
                                RUN SetInvPostForm(sys-ctrl-shipto.char-fld).
                            v-print-fmt = sys-ctrl-shipto.char-fld.
                        END.
                        ELSE
                        DO:
                            IF "{&head}" EQ "ar-inv" THEN
                                RUN SetInvPostForm(vcDefaultForm). 
                            ELSE                               
                                RUN SetInvPostForm(vcDefaultForm).
                            v-print-fmt = vcDefaultForm.
                        END.

                        ASSIGN
                            vcInvNums   = ""
                            lv-pdf-file = init-dir + "\Inv".

                        RUN run-report-posted(b-ar-inv.cust-no, TRUE).
                        FIND FIRST ar-invl WHERE ar-invl.x-no = b-ar-inv.x-no
                            NO-LOCK NO-ERROR.
                        vSoldToNo = "". 
                        IF AVAILABLE ar-invl THEN 
                        DO:
                            FIND oe-ord WHERE oe-ord.company = b-ar-inv.company
                                AND oe-ord.ord-no = ar-invl.ord-no
                                NO-LOCK NO-ERROR.
                            vSoldToNo = IF AVAILABLE oe-ord THEN oe-ord.sold-id ELSE "". 
                        END.
                        vShipToNo = b-ar-inv.ship-id.
                        RUN GenerateEmail(b-ar-inv.cust-no).
                    END.
                END.
            ELSE
            DO:
                IF "{&head}" EQ "ar-inv" THEN
                    RUN SetInvPostForm(vcDefaultForm). 
                ELSE                   
                    RUN SetInvPostForm(vcDefaultForm).

                ASSIGN
                    vcInvNums   = ""
                    lv-pdf-file = init-dir + "\Inv"
                    v-print-fmt = vcDefaultForm.

                RUN run-report-posted("", FALSE).
                RUN GenerateEmail(icCustNo).
            END.
        END.
        ELSE /*not posted*/
        **** end of section to be removed */
        DO:         
            IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                sys-ctrl-shipto.company = cocode AND
                sys-ctrl-shipto.NAME = "INVPRINT") THEN
                FOR EACH buf-{&head} FIELDS(company cust-no sold-no) WHERE
                    buf-{&head}.company EQ cocode AND
                    ((buf-{&head}.cust-no GE begin_cust AND buf-{&head}.cust-no LE end_cust AND NOT tb_BatchMail)
                    OR
                    (buf-{&head}.cust-no EQ icCustNo AND tb_BatchMail)
                    ) AND
               INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 AND 
                    (((NOT tb_reprint) AND buf-{&head}.inv-no EQ 0) OR
                    (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                    (IF "{&head}" EQ "ar-inv" THEN buf-{&head}.inv-date GE begin_date
                         AND buf-{&head}.inv-date LE end_date ELSE TRUE) AND                    
                    buf-{&head}.inv-no GE begin_inv AND
                    buf-{&head}.inv-no LE end_inv))
                    NO-LOCK,
                    FIRST b-cust WHERE
                    b-cust.company EQ cocode AND
                    b-cust.cust-no EQ buf-{&head}.cust-no AND
                    ((b-cust.inv-meth EQ ? AND buf-{&head}.{&multiinvoice}) OR
                    (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice}))
                    NO-LOCK
                    BREAK BY buf-{&head}.company
                    BY buf-{&head}.cust-no:

                    IF FIRST-OF(buf-{&head}.cust-no) THEN
                    DO:
                        FIND FIRST sys-ctrl-shipto WHERE
                            sys-ctrl-shipto.company = cocode AND
                            sys-ctrl-shipto.NAME = "INVPRINT" AND
                            sys-ctrl-shipto.cust-vend = YES AND
                            sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                            sys-ctrl-shipto.ship-id = STRING(buf-{&head}.sold-no) AND
                            sys-ctrl-shipto.char-fld > ''
                            NO-LOCK NO-ERROR.

                        IF NOT AVAILABLE sys-ctrl-shipto THEN
                            FIND FIRST sys-ctrl-shipto WHERE
                                sys-ctrl-shipto.company = cocode AND
                                sys-ctrl-shipto.NAME = "INVPRINT" AND
                                sys-ctrl-shipto.cust-vend = YES AND
                                sys-ctrl-shipto.cust-vend-no = buf-{&head}.cust-no AND
                                sys-ctrl-shipto.char-fld > ''
                                NO-LOCK NO-ERROR.

                        IF AVAILABLE sys-ctrl-shipto THEN
                        DO:
                            IF "{&head}" EQ "ar-inv" THEN
                                RUN SetInvPostForm(sys-ctrl-shipto.char-fld). 
                            ELSE                               
                                RUN SetInvForm(sys-ctrl-shipto.char-fld).
                            v-print-fmt = sys-ctrl-shipto.char-fld.
                        END.
                        ELSE
                        DO:
                            IF "{&head}" EQ "ar-inv" THEN
                                RUN SetInvPostForm(vcDefaultForm). 
                            ELSE                               
                                RUN SetInvForm(vcDefaultForm).
                            v-print-fmt = vcDefaultForm.
                        END.

                        ASSIGN
                            vcInvNums   = ""
                            lv-pdf-file = init-dir + "\Inv".
                        RUN run-report(buf-{&head}.cust-no,"", TRUE).
                        ASSIGN 
                            vSoldToNo = ""
                            vShipToNo = "".
                        IF buf-{&head}.{&multiinvoice} THEN 
                        DO: 
                            FIND FIRST b2-{&head} 
                                WHERE b2-{&head}.company       EQ buf-{&head}.company
                                AND b2-{&head}.cust-no       EQ buf-{&head}.cust-no
                                AND b2-{&head}.inv-no        EQ buf-{&head}.inv-no
                                AND b2-{&head}.{&multiinvoice} EQ NO            
                                AND INDEX(vcHoldStats, b2-{&head}.stat) EQ 0 NO-LOCK NO-ERROR.
                            IF AVAILABLE b2-{&head} THEN
                                FIND FIRST {&line} NO-LOCK WHERE {&line}.{&rno} EQ b2-{&head}.{&rno} 
                                  AND {&line}.ord-no NE 0 NO-ERROR.  
                        END.  
                        ELSE         
                            FIND FIRST {&line} NO-LOCK WHERE {&line}.{&rno} EQ buf-{&head}.{&rno}
                              NO-ERROR.
                        IF AVAILABLE {&line} THEN 
                        DO:
                            FIND oe-ord WHERE oe-ord.company = buf-{&head}.company
                                AND oe-ord.ord-no = {&line}.ord-no
                                NO-LOCK NO-ERROR.
                            vSoldToNo = IF AVAILABLE oe-ord THEN oe-ord.sold-id ELSE "". 
                        END.
                        vShipToNo = STRING(buf-{&head}.sold-no).
                        RUN GenerateEmail(buf-{&head}.cust-no).
                    END.
                END.

            ELSE
            DO:
                IF "{&head}" EQ "ar-inv" THEN
                    RUN SetInvPostForm(vcDefaultForm). 
                ELSE                   
                    RUN SetInvForm(vcDefaultForm).

                ASSIGN
                    v-print-fmt = vcDefaultForm
                    vcInvNums   = ""
                    lv-pdf-file = init-dir + "\Inv".

                RUN run-report("","", FALSE).
                RUN GenerateEmail(icCustNo).
            END.
        END.
    END.

END PROCEDURE.

PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/invoice.p  9/94 RM */
    /* PRINT INVOICE - O/E MODULE                                                 */
    /* -------------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sold-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

    DEFINE BUFFER bf-{&line} FOR {&line}.

    DEFINE VARIABLE ll-consolidate AS LOG NO-UNDO.
    DEFINE VARIABLE lv-copy#       AS INTEGER NO-UNDO.      
    DEFINE VARIABLE dtl-ctr        AS INTEGER NO-UNDO.

    {sys/form/r-top.i}

    ASSIGN                   
        finv           = begin_inv
        tinv           = end_inv
        fdate          = begin_date
        tdate          = end_date
        fbol           = begin_bol
        tbol           = end_bol
        v-reprint      = tb_reprint
        v-sort         = rd_sort BEGINS "Customer"
        v-prntinst     = tb_prt-inst
        v-print-dept   = tb_print-dept
        ll-consolidate = rd_sort EQ "Customer2".

    /* gdm - 12080807 */
    ASSIGN 
        nsv_setcomp = tb_setcomp.

    IF ip-sys-ctrl-shipto THEN
        ASSIGN
            fcust = ip-cust-no
            tcust = ip-cust-no.
    ELSE
    DO:
        IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' AND 
            tb_BatchMail:CHECKED THEN
            ASSIGN
                vcBegCustNo = vcBegCustNo
                vcEndCustNo = vcEndCustNo.
        ELSE
            ASSIGN
                vcBegCustNo = begin_cust
                vcEndCustNo = end_cust.

        ASSIGN
            fcust = vcBegCustNo
            tcust = vcEndCustNo.
    END.

    IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
            v-depts      = fi_depts:SCREEN-VALUE.

        {sys/inc/print1.i}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    {sa/sa-sls01.i}

    v-term-id = v-term + USERID("nosweat").

    SESSION:SET-WAIT-STATE ("general").

    build-report:
    FOR EACH {&head} NO-LOCK
        WHERE {&head}.company         EQ cocode
        AND {&head}.cust-no         GE fcust
        AND {&head}.cust-no         LE tcust 
        AND (STRING({&head}.sold-no)         EQ ip-sold-no OR ip-sold-no = "")
        AND INDEX(vcHoldStats, {&head}.stat) EQ 0
        AND ("{&head}" NE "ar-inv" OR {&head}.posted = tb_posted)
        AND (IF "{&head}" EQ "ar-inv" THEN {&head}.inv-date GE begin_date
              AND {&head}.inv-date LE end_date ELSE TRUE)        
        AND (((NOT v-reprint) AND {&head}.inv-no EQ 0) OR
        (v-reprint AND {&head}.inv-no NE 0 AND
        {&head}.inv-no        GE finv AND
        {&head}.inv-no        LE tinv)),
        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ {&head}.cust-no
        AND ((cust.inv-meth EQ ? AND {&head}.{&multiinvoice}) OR
        (cust.inv-meth NE ? AND NOT {&head}.{&multiinvoice}) OR
        "{&head}" EQ "ar-inv"
        )
        NO-LOCK BY {&head}.{&bolno} :
        FIND FIRST buf-{&line} NO-LOCK 
                    WHERE buf-{&line}.{&rno} EQ {&head}.{&rno}
                    AND buf-{&line}.bol-no GE fbol
                    AND buf-{&line}.bol-no LE tbol 
                    NO-ERROR. 


         IF NOT ( ({&head}.{&multiinvoice} EQ NO AND AVAILABLE(buf-{&line})) OR {&head}.{&multiinvoice} ) THEN NEXT.            

        vcBOLFiles = "".    
        IF {&head}.{&multiinvoice} THEN 
        DO:
            dtl-ctr = 0.
            FOR EACH b-{&head}1 NO-LOCK
                WHERE b-{&head}1.company       EQ {&head}.company
                AND b-{&head}1.cust-no       EQ {&head}.cust-no
                AND b-{&head}1.inv-no        EQ {&head}.inv-no
                AND b-{&head}1.{&multiinvoice} EQ NO            
                AND INDEX(vcHoldStats, b-{&head}1.stat) EQ 0,
                EACH buf-{&line}1 NO-LOCK 
                  WHERE buf-{&line}1.{&rno} EQ b-{&head}1.{&rno} :

                /* {oe/rep/bolcheck.i b-{&head}1 build-report} */
                
                            
                &SCOPED-DEFINE bol-check                            ~
                       IF oe-bolh.bol-no   GE fbol  AND             ~
                          oe-bolh.bol-no   LE tbol  AND             ~
                          oe-bolh.bol-date GE fdate AND             ~
                          oe-bolh.bol-date LE tdate THEN DO:        ~
                         RELEASE {&line}.                          ~
                         RELEASE oe-bolh.                           ~
                         LEAVE.                                     ~
                       END.                                         ~
                       ELSE NEXT build-report.
                
                
                RELEASE oe-bolh.
                  
                IF buf-{&line}1.{&bolno} EQ 0 THEN
                    
                    FOR EACH oe-bolh NO-LOCK 
                          WHERE oe-bolh.company EQ buf-{&line}1.company
                            AND oe-bolh.bol-no EQ buf-{&line}1.bol-no:
                        
                        {&bol-check}
                END.
                
                ELSE
                FOR EACH oe-bolh NO-LOCK
                    WHERE oe-bolh.company EQ cocode
                      AND oe-bolh.bol-no  EQ buf-{&line}1.bol-no
                    BREAK BY oe-bolh.b-no:
                        
                {&bol-check}
                END.
                                               
                IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(b-{&head}1.{&bolno}) + ".pdf") NE ?  THEN 
                    vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(buf-{&line}1.bol-no) + ".pdf").
                RUN create-save-line.
                ASSIGN 
                    dtl-ctr = dtl-ctr + 1.
            END.
        END.

        ELSE 
        DO:
            /* Was bolcheck.i */
                        
            RELEASE oe-bolh.
              
            IF buf-{&line}.bol-no EQ 0 THEN
                FOR EACH oe-bolh NO-LOCK 
                      WHERE oe-bolh.company EQ {&line}.company
                        AND oe-bolh.bol-no EQ buf-{&line}1.bol-no:
                    
                    {&bol-check}
            END.
            
            ELSE
            FOR EACH oe-bolh NO-LOCK
                WHERE oe-bolh.company EQ cocode
                  AND oe-bolh.bol-no  EQ buf-{&line}1.bol-no
                BREAK BY oe-bolh.b-no:
                    
            {&bol-check}
            END.
            
            
            IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(buf-{&line}1.bol-no) + ".pdf") NE ?  THEN 
                vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(buf-{&line}1.bol-no) + ".pdf").

        END.


        /* dont include {&head} on printing when it's a MASTER invoice (multi-inv) AND 
           no SLAVE invoices found which is NOT on HOLD Status ({&head}.stat NE "H") AH 07/08/10 */
        IF cust.inv-meth EQ ? 
             AND {&head}.{&multiinvoice} 
             AND dtl-ctr LE 0 
             AND NOT "{&head}" EQ "ar-inv" THEN NEXT.

        vlSkipRec = NO.

        /* WFK - 15063 - Removed this change since it was causing lots of problems */
        /*  IF  NOT v-reprint OR {&head}.inv-no EQ 0 THEN do:*/
        /*        RUN oe/get-inv#.p (ROWID({&head})).        */
        /*        v-reprint = YES  .                          */
        /*  END.                                              */

        CREATE report.
        ASSIGN
            report.term-id = v-term-id
            report.key-01  = IF v-sort THEN
                      IF v-sort-name THEN cust.name
                      ELSE {&head}.cust-no
                    ELSE ""
            report.key-02  = STRING({&head}.{&bolno},"9999999999")
            report.rec-id  = RECID({&head})
            vcInvNums      = vcInvNums + '-' + STRING ({&head}.inv-no)
            vcInvNums      = LEFT-TRIM (vcInvNums, '-')  
            report.key-03  = IF v-sort THEN STRING({&head}.inv-no,"9999999999") ELSE ""  .

        IF vcInvNums MATCHES '*-*' THEN
            vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') + SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).

        /* update loadtag status - Bill of lading task#: 10190414 */
        IF NOT {&head}.printed THEN
            FOR EACH bf-{&line} /* wfk  OF {&head} */ NO-LOCK:
                FOR EACH oe-boll WHERE oe-boll.company EQ bf-{&line}.company
                    AND oe-boll.b-no    EQ bf-{&line}.b-no
                    AND oe-boll.ord-no  EQ bf-{&line}.ord-no
                    AND oe-boll.i-no    EQ bf-{&line}.i-no
                    AND oe-boll.line    EQ bf-{&line}.line
                    AND oe-boll.po-no   EQ bf-{&line}.po-no
                    AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no   EQ bf-{&line}.b-no
                    AND oe-bolh.posted EQ YES) NO-LOCK:

                    FIND FIRST loadtag EXCLUSIVE-LOCK WHERE loadtag.company EQ {&head}.company
                        AND loadtag.item-type EQ NO
                        AND loadtag.i-no EQ bf-{&line}.i-no
                        AND loadtag.job-no EQ oe-boll.job-no
                        AND loadtag.job-no2 EQ oe-boll.job-no2
                        AND loadtag.tag-no EQ oe-boll.tag NO-ERROR.

                    IF AVAILABLE loadtag THEN loadtag.sts = "Invoiced".
                    RELEASE loadtag.
                END.
            END.
    END.

    FOR EACH save-line WHERE save-line.reftable EQ "save-line" + v-term-id,
        FIRST {&head}
        WHERE {&head}.{&rno} EQ INT(save-line.val[2])
        AND NOT CAN-FIND(FIRST report
        WHERE report.term-id EQ v-term-id
        AND report.rec-id  EQ RECID({&head})):
        RUN undo-save-line.
    END.

    v-lines-per-page = lines-per-page.

    IF v-print-fmt NE "Fibrex" THEN
    DO:
        FIND FIRST sys-ctrl WHERE
            sys-ctrl.company EQ cocode AND
            sys-ctrl.name    EQ "INVCOPYS"
            NO-LOCK NO-ERROR.

        lv-copy# = IF AVAILABLE sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.
    END.
    ELSE
        lv-copy# = lv-scr-num-copies.

    /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
    IF fi_broker-bol:SENSITIVE = YES AND
        LOOKUP(v-print-fmt,"Capitol,APC,ALLWEST,Bell,LoyLang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") GT 0 AND
        begin_inv EQ end_inv THEN
    DO:
        FIND FIRST b-broker-bol WHERE
            b-broker-bol.reftable EQ "brokerbol" AND
            b-broker-bol.CODE EQ STRING(begin_inv)
            NO-ERROR.

        IF NOT AVAILABLE b-broker-bol AND
            fi_broker-bol:SCREEN-VALUE NE "" THEN
        DO:
            CREATE b-broker-bol.
            ASSIGN
                b-broker-bol.reftable = "brokerbol"
                b-broker-bol.CODE     = STRING(begin_inv).
        END.

        IF AVAILABLE b-broker-bol THEN
        DO:
            b-broker-bol.code2 = fi_broker-bol:SCREEN-VALUE.
            RELEASE b-broker-bol.
        END.
    END.

    IF is-xprint-form THEN 
    DO:

        IF v-print-fmt EQ "Fibrex" AND
            tb_collate:HIDDEN EQ NO AND tb_collate THEN
            PUT "<COLLATE=YES,ALWAYS>".

        CASE rd-dest :

            WHEN 1 THEN 
                PUT "<COPIES=" + string(lv-copy#) + "><PRINTER?>" FORM "x(30)".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW><MODAL=NO>" FORM "x(30)".
                    ELSE
                        PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
                END.
            WHEN 5 THEN 
                DO:
                    IF vcInvNums = "0" OR vcInvNums = "0-0" THEN 
                        vcInvNums = STRING(RANDOM(1, 1000)).
                    IF v-print-fmt EQ "CentBox" THEN
                    DO:
                        IF NOT tb_BatchMail:CHECKED THEN
                            PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        ELSE 
                            PUT "<PREVIEW=PDF><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        cActualPDF = lv-pdf-file + vcInvNums  + ".pdf".
                    END.
                    ELSE IF v-print-fmt EQ "Southpak-XL" OR v-print-fmt EQ "PrystupExcel" THEN 
                        DO:
                            PUT "<PDF=DIRECT><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(180)".
                            cActualPDF = list-name + ".pdf".
                        END.
                        ELSE IF v-print-fmt EQ "Protagon" OR v-print-fmt = "Protagon2" THEN 
                            DO:
                                PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=0.5mm><PDF-TOP=-0.5mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                            END.
                            ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "Coburn" OR v-print-fmt = "PremierS" OR v-print-fmt = "Axis" THEN 
                                DO:
                                    PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=5mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                    cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                                END.
                                ELSE 
                                DO: 
                                    PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                    cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                                END.  
                END.

        END CASE.

        PUT "</PROGRESS>".
    END.

    ASSIGN 
        lXMLOutput  = rd-dest EQ iXMLOutput /* rstark 05181205 */
        clXMLOutput = NO /* rstark 05291402 */
        .

    IF LOOKUP(v-print-fmt,"SOUTHPAK,southpak-xl,PrystupExcel,ASIXprnt,Southpakl,Badger,Badger-Emailed") > 0 THEN 
    DO: 
        RUN value(v-program) (lv-multi-faxout,lines-per-page). 
    END.
    ELSE IF v-print-fmt EQ "1/2 Page" AND rd-dest = 6 THEN 
        DO:
            PUT CONTROL CHR(27) CHR(67) CHR(44). 
            RUN value(v-program). 
            PUT CONTROL CHR(18).
        END.

        ELSE IF LOOKUP(v-print-fmt,"BlueRX,ColoniaX,ABC,Nosco,Nosco1,Central,Rosmar,ACPI,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN 
            DO:
                RUN value(v-program) (""). 
                v-reprint = YES.
                IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
                IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
                IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
            END.
            ELSE IF LOOKUP(v-print-fmt,"ColorX") > 0 THEN 
                DO:
                    v-reprint = YES.
                    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
                    IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
                    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
                END.
                ELSE IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis") > 0 THEN 
                    DO: 
                        RUN value(v-program) ("",NO). 
                        v-reprint = YES.
                        IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",NO).
                        IF tb_office-copy THEN RUN value(v-program) ("Office Copy",NO).
                        IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",NO).
                    END.
                    ELSE IF LOOKUP(v-print-fmt,"PremierS") > 0 THEN 
                        DO:    
                            RUN value(v-program) ("",YES). 
                            v-reprint = YES.
                            IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",YES).
                            IF tb_office-copy THEN RUN value(v-program) ("Office Copy",YES).
                            IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",YES).
                        END.
                        ELSE RUN value(v-program). 

    vcInvNums = "".
    FOR EACH report WHERE report.term-id EQ v-term-id NO-LOCK,        
        FIRST {&head} WHERE RECID({&head}) EQ report.rec-id NO-LOCK
        BREAK BY {&head}.inv-no:

        ASSIGN 
            vcInvNums = vcInvNums + '-' + STRING ({&head}.inv-no)
            vcInvNums = LEFT-TRIM (vcInvNums, '-').

        /* Extract first and last inv# with '-' in between */
        IF vcInvNums MATCHES '*-*' THEN
            vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') +     
                SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).

    END.

    FOR EACH save-line WHERE save-line.reftable EQ "save-line" + v-term-id:
        RUN undo-save-line.
    END.

    FOR EACH report WHERE report.term-id EQ v-term-id: 
        DELETE report.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

PROCEDURE run-report-posted :
    /* ------------------------------------------------ ar/rep/invoice.p  9/94 RM */
    /* PRINT INVOICE - A/R MODULE                                                 */
    /* -------------------------------------------------------------------------- */

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-ship AS LOG NO-UNDO.

    DEFINE VARIABLE lv-copy# AS INTEGER NO-UNDO.

{sys/form/r-top.i}

    ASSIGN
        finv         = begin_inv
        tinv         = end_inv
        v-print      = tb_reprint
        v-posted     = tb_posted
        v-prntinst   = tb_prt-inst
        v-print-dept = tb_print-dept.

    IF fi_depts:HIDDEN IN FRAME {&FRAME-NAME} = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept:SCREEN-VALUE)
            v-depts      = fi_depts:SCREEN-VALUE.

    IF ip-sys-ctrl-ship THEN
        ASSIGN
            fcust = ip-cust-no
            tcust = ip-cust-no.
    ELSE
    DO:
        IF rd-dest:SCREEN-VALUE IN FRAME {&FRAME-NAME} = '5' AND 
            tb_BatchMail:CHECKED THEN
            ASSIGN vcBegCustNo = vcBegCustNo
                vcEndCustNo = vcEndCustNo.
        ELSE
            ASSIGN vcBegCustNo = begin_cust
                vcEndCustNo = end_cust.

        ASSIGN
            fcust = vcBegCustNo
            tcust = vcEndCustNo.
    END.

{sys/inc/print1.i}

    {sys/inc/outprint.i VALUE(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

{sa/sa-sls01.i}

    v-term-id = v-term.

    SESSION:SET-WAIT-STATE ("general").

    FOR EACH ar-inv
        WHERE ar-inv.company              EQ cocode
        AND ar-inv.inv-no                 GE finv
        AND ar-inv.inv-no                 LE tinv
        AND ar-inv.cust-no                GE fcust
        AND ar-inv.cust-no                LE tcust
        AND ar-inv.printed                EQ v-print
        AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no = ar-inv.x-no)
        USE-INDEX inv-no NO-LOCK:

        IF NOT(ar-inv.posted EQ NO OR
            ar-inv.posted EQ v-posted) THEN NEXT.

        CREATE report.
        ASSIGN
            report.term-id = v-term-id
            report.key-01  = STRING(ar-inv.inv-no,"9999999999")
            report.rec-id  = RECID(ar-inv)
            vlSkipRec      = NO
            vcInvNums      = vcInvNums + '-' + STRING (ar-inv.inv-no)
            vcInvNums      = LEFT-TRIM (vcInvNums, '-')  
            report.key-03  = IF v-sort THEN STRING(ar-inv.inv-no,"9999999999") ELSE "" .

        IF vcInvNums MATCHES '*-*' THEN
            vcInvNums = RIGHT-TRIM (SUBSTRING (vcInvNums, 1, INDEX (vcInvNums,'-')), '-') + SUBSTRING (vcInvNums, R-INDEX (vcInvNums, '-')).
    END.

    v-lines-per-page = lines-per-page.

    IF v-print-fmt NE "Fibrex" THEN
    DO:
        FIND FIRST sys-ctrl WHERE
            sys-ctrl.company EQ cocode AND
            sys-ctrl.name    EQ "INVCOPYS"
            NO-LOCK NO-ERROR.

        lv-copy# = IF AVAILABLE sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.
    END.
    ELSE
        lv-copy# = lv-scr-num-copies.

    /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
    IF fi_broker-bol:SENSITIVE = YES AND
        lookup(v-print-fmt,"Capitol,APC,ALLWEST,Bell,Loylang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") GT 0 AND
        begin_inv EQ end_inv THEN
    DO:
        FIND FIRST b-broker-bol WHERE
            b-broker-bol.reftable EQ "brokerbol" AND
            b-broker-bol.CODE EQ STRING(begin_inv)
            NO-ERROR.

        IF NOT AVAILABLE b-broker-bol AND
            fi_broker-bol:SCREEN-VALUE NE "" THEN
        DO:
            CREATE b-broker-bol.
            ASSIGN
                b-broker-bol.reftable = "brokerbol"
                b-broker-bol.CODE     = STRING(begin_inv).
        END.

        IF AVAILABLE b-broker-bol THEN
        DO:
            b-broker-bol.code2 = fi_broker-bol:SCREEN-VALUE.
            RELEASE b-broker-bol.
        END.
    END.

    IF is-xprint-form THEN 
    DO:

        IF v-print-fmt EQ "Fibrex" AND
            tb_collate:HIDDEN EQ NO AND tb_collate THEN
            PUT "<COLLATE=YES,ALWAYS>".

        CASE rd-dest :
            WHEN 1 THEN 
                PUT "<COPIES=" + string(lv-copy#) + "><PRINTER?>" FORM "x(30)".
            WHEN 2 THEN 
                DO:
                    IF NOT lBussFormModle THEN
                        PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
                    ELSE
                        PUT "<COPIES=" + string(lv-copy#) + "><PREVIEW>" FORM "x(30)".
                END.
            WHEN 4 THEN 
                DO:
                    ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                    PUT UNFORMATTED 
                        "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
                END.
            WHEN 5 THEN 
                DO:
                    IF v-print-fmt = "CENTBOX" THEN
                    DO:
                        IF NOT tb_BatchMail:CHECKED THEN
                            PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        ELSE
                            PUT "<PREVIEW=PDF><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                    END.
                    ELSE IF v-print-fmt = "CSCIN" OR v-print-fmt = "CSCINStamp" THEN
                            PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        ELSE
                            PUT "<PREVIEW><PDF-LEFT=5mm><PDF-TOP=1mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".              

                END.
        END CASE.

        PUT "</PROGRESS>".
    END.

    IF LOOKUP(v-print-fmt,"SOUTHPAK,southpak-xl,PrystupExcel,ASIXprnt,Badger,Badger-Emailed,Southpakl") > 0 THEN 
    DO: 
        RUN value(v-program) (lv-multi-faxout,lines-per-page).
    END.
    ELSE IF v-print-fmt = "1/2 Page" AND rd-dest = 6 THEN 
        DO:
            PUT CONTROL CHR(27) CHR(67) CHR(44). 
            RUN value(v-program). 
            PUT CONTROL CHR(18).
        END.

        ELSE IF LOOKUP(v-print-fmt,"BlueRX,ColoniaX,ABC,Nosco,Nosco1,Central,Rosmar,ACPI,ColorX,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN 
            DO:  
                RUN value(v-program) ("").
                IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
                IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
                IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
            END.
            ELSE IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis") > 0 THEN 
                DO:    
                    RUN value(v-program) ("", NO).
                    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy", NO).
                    IF tb_office-copy THEN RUN value(v-program) ("Office Copy", NO).
                    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy", NO).
                END.
                ELSE IF LOOKUP(v-print-fmt,"PremierS") > 0 THEN 
                    DO:
                        RUN value(v-program) ("", YES).
                        IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy", YES).
                        IF tb_office-copy THEN RUN value(v-program) ("Office Copy", YES).
                        IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy", YES).
                    END.
                    ELSE RUN value(v-program). 
    OUTPUT CLOSE.

    FOR EACH report WHERE report.term-id EQ v-term-id: 
        DELETE report.
    END.

    RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

PROCEDURE undo-save-line :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DISABLE TRIGGERS FOR LOAD OF {&line}.
    DISABLE TRIGGERS FOR LOAD OF inv-misc.


    RELEASE {&line}.
    RELEASE inv-misc.

    FIND FIRST {&line} WHERE RECID({&line}) EQ INT(save-line.val[3]) NO-ERROR.

    IF AVAILABLE {&line} THEN {&line}.{&rno} = save-line.val[1].

    ELSE
        FIND FIRST inv-misc WHERE RECID(inv-misc) EQ INT(save-line.val[3]) NO-ERROR.

    IF AVAILABLE inv-misc THEN inv-misc.{&miscrno} = save-line.val[1].

    DELETE save-line.

END PROCEDURE.    
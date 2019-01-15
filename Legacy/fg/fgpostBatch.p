
/*------------------------------------------------------------------------
    File        : fgpostBatchp
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Wed Aug 16 09:05:30 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/
{fg/invrecpt.i NEW}
{fg/fgPostBatch.i}

DEFINE INPUT  PARAMETER v-post-date AS DATE NO-UNDO.
DEFINE INPUT  PARAMETER tg-recalc-cost AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER ip-run-what AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER gv-fgemail      AS LOGICAL   NO-UNDO INIT ?. 
DEFINE INPUT PARAMETER TABLE FOR w-fg-rctd.
DEFINE INPUT PARAMETER TABLE FOR tt-fgemail. 
DEFINE INPUT PARAMETER TABLE FOR tt-email. 
DEFINE INPUT PARAMETER TABLE FOR tt-inv.  
/* Need to return w-job for prompting */
DEFINE VARIABLE ll        AS LOG     NO-UNDO.
DEFINE VARIABLE dBillAmt  AS DECIMAL NO-UNDO. /* set, not used */
DEFINE VARIABLE lEmailBol AS LOG     NO-UNDO. /* set and used internally */
DEFINE VARIABLE lInvFrt         AS LOG       NO-UNDO. /* set not used? */
DEFINE VARIABLE lvReturnChar    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvFound         AS LOG       NO-UNDO.
DEFINE VARIABLE autofgissue-log AS LOGICAL   NO-UNDO. /* set and used */
DEFINE VARIABLE v-fgpostgl      AS CHARACTER NO-UNDO. /* set by fgpostgl nk1 value */

DEFINE VARIABLE fg-uom-list     AS CHARACTER NO-UNDO.
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}
/* {methods/prgsecur.i} */

/* temp prgsecur.i */
{methods/defines/globdefs.i}
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO INIT "fgpstall.".
DEFINE BUFFER b-prgrms FOR prgrms.

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.
DO TRANSACTION:
    {sys/inc/fgpost.i}   
END.

{jc/jcgl-sh.i }
{fg/fg-post3.i NEW}


DO TRANSACTION:
    {sys/inc/closejob.i FGPost}
    {sys/inc/fgpostgl.i}   

    {sys/inc/fgemails.i}
    {sys/inc/postdate.i}
END.
{sys/inc/adjustgl.i}

ASSIGN
    v-fgpostgl = fgpostgl.

/* Needed for fg/rep/fg-post.i */
DEFINE VARIABLE v-fg-value      AS DECIMAL   FORMAT "->,>>>,>>9.99".
DEFINE VARIABLE v-msf           AS DECIMAL   FORMAT ">,>>9.999" EXTENT 6.
DEFINE VARIABLE is-xprint-form  AS LOGICAL   NO-UNDO.
DEFINE VARIABLE ls-fax-file     AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-corr          AS LOGICAL.
DEFINE VARIABLE rd-Itm#Cst#     AS INTEGER.
DEFINE VARIABLE rd-ItmPo        AS INTEGER.
DEFINE VARIABLE ip-rowid        AS ROWID     NO-UNDO.
DEFINE VARIABLE t-setup         AS LOGICAL   NO-UNDO.

DEFINE VARIABLE tb_excel        AS LOGICAL   NO-UNDO.
DEFINE VARIABLE li AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt-posted-items
    FIELD i-no LIKE w-fg-rctd.i-no
    INDEX i-no i-no.
        
{sys/ref/fgoecost.i}

DEF BUFFER ref-lot-no FOR reftable.

RUN sys/ref/nk1look.p (cocode, "AUTOFGISSUE", "L", NO, NO, "", "", 
    OUTPUT lvReturnChar, OUTPUT lvFound).
IF lvFound THEN
    autofgissue-log = LOGICAL(lvReturnChar).

IF ip-run-what EQ "" THEN 
DO:
PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEF INPUT PARAM mailTo AS CHAR.
    DEF INPUT PARAM mailsubject AS CHAR.
    DEF INPUT PARAM mailText AS CHAR.
    DEF INPUT PARAM mailFiles AS CHAR.
    DEF INPUT PARAM mailDialog AS LONG.
    DEF OUTPUT PARAM retCode AS LONG.
END.
END.
    
DEFINE STREAM logFile.
DEF STREAM st-email.
{fg/fgPostProc.i}
/* ********************  Preprocessor Definitions  ******************** */
&SCOPED-DEFINE NOOUTPUT NOOUTPUT
/* ************************  Function Prototypes ********************** */


FUNCTION get-act-rel-qty RETURNS INTEGER 
    (  ) FORWARD.

FUNCTION get-tot-rcv-qty RETURNS INTEGER 
    (  ) FORWARD.

/* ***************************  Main Block  *************************** */


{fg/rep/fg-post.i "itemxA" "v-fg-cost" "itempxA" "v-tot-cost"}
RUN fg-post.


/* **********************  Internal Procedures  *********************** */

PROCEDURE add-rel-assign-logic:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
    DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.
    DEF VAR lv-rel-recid   AS RECID NO-UNDO.
    DEF VAR adm-new-record AS LOG   NO-UNDO.
    DEF BUFFER bf-oe-ordl FOR oe-ordl.
    DEF BUFFER bf-oe-ord  FOR oe-ord.
    DEF BUFFER bf-oe-rel  FOR oe-rel.

    /* custom code */
    DEF BUFFER s-code     FOR reftable.
    DEF VAR lv-stat AS CHAR NO-UNDO.
    FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ipr-rel-row
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-oe-rel THEN
        RETURN.
    lv-rel-recid = RECID(bf-oe-rel).

    /* Local assign code from oe/b-ordrel.w, local-assign */
    DEF VAR ll-ans   AS LOG  NO-UNDO.
    DEF VAR ldt-ship AS DATE FORM "99/99/9999" NO-UNDO.
    DEF BUFFER bf-rel FOR oe-rel .
    DEF VAR ld-prev-rel-qty AS INT NO-UNDO.
    DEF VAR v-qty-sum       AS INT NO-UNDO.
    DEF VAR ls-key-02       LIKE tt-report.key-02 NO-UNDO.

    DEF BUFFER b-ordl FOR oe-ordl.


    FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ bf-oe-rel.company
        AND bf-oe-ordl.ord-no  EQ bf-oe-rel.ord-no
        AND bf-oe-ordl.LINE    EQ bf-oe-rel.LINE
        NO-LOCK NO-ERROR.
    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT AVAIL bf-oe-rel AND lv-rel-recid <> ? THEN
        FIND bf-oe-rel WHERE RECID(bf-oe-rel) = lv-rel-recid.
    ld-prev-rel-qty = IF adm-new-record THEN 0 ELSE bf-oe-rel.qty.

    FIND bf-oe-ord OF bf-oe-ordl NO-LOCK.



    FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(bf-oe-ordl).
    b-ordl.t-rel-qty = b-ordl.t-rel-qty + bf-oe-rel.qty - ld-prev-rel-qty.
    FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(bf-oe-ordl) NO-LOCK.
    RUN fg/fgitmloc.p (INPUT bf-oe-rel.i-no, INPUT ROWID(bf-oe-rel)).

END PROCEDURE.

PROCEDURE add-rel-for-qty:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
    DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.


    DEF BUFFER bf-oe-ordl     FOR oe-ordl.
    DEF BUFFER bf-oe-ord      FOR oe-ord.
    DEF BUFFER bf-orig-oe-rel FOR oe-rel.

    /* Code added to implement the procedure below */
    DEF VAR v-last-shipto AS CHAR.
    DEF VAR lv-rel-recid  AS RECID.
    DEF BUFFER s-code FOR reftable.
    DEF VAR lv-cust-x    AS CHAR.

    DEF VAR oereleas-log LIKE sys-ctrl.log-fld NO-UNDO.
    DEF VAR oereleas-cha LIKE sys-ctrl.char-fld NO-UNDO.

    /* Calling program validates this exists */
    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OERELEAS"
        NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN
        ASSIGN
            oereleas-log = sys-ctrl.log-fld
            oereleas-cha = sys-ctrl.char-fld.


    /* custom code */
    FIND bf-orig-oe-rel WHERE ROWID(bf-orig-oe-rel) EQ ipr-rel-row
        NO-LOCK NO-ERROR.

    IF NOT AVAIL bf-orig-oe-rel THEN
        RETURN.
    FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ bf-orig-oe-rel.company
        AND bf-oe-ordl.ord-no  EQ bf-orig-oe-rel.ord-no
        AND bf-oe-ordl.i-no    EQ bf-orig-oe-rel.i-no
        NO-LOCK NO-ERROR.

    IF NOT AVAIL(bf-oe-ordl) THEN
        RETURN.
    FIND bf-oe-ord WHERE bf-oe-ord.company EQ bf-oe-ordl.company
        AND bf-oe-ord.ord-no  EQ bf-oe-ordl.ord-no
        NO-LOCK NO-ERROR.
    IF NOT AVAIL bf-oe-ord THEN
        RETURN.


    /* Code from oe/b-ordrel.w, local-create */
    DEF VAR v-qty-sum    AS INT  NO-UNDO.
    DEF VAR v-nxt-r-no   AS INT  NO-UNDO.
    DEF VAR v-lst-rel    AS DATE INIT TODAY NO-UNDO.
    DEF VAR v-pct-chg    AS DEC  NO-UNDO.
    DEF VAR v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
    DEF VAR v-carrier    LIKE oe-rel.carrier NO-UNDO.
    DEF VAR v-num-shipto AS INT  NO-UNDO.
    DEF VAR v-qty-mod    AS LOG  NO-UNDO.
    DEF BUFFER bf-rel  FOR oe-rel.
    DEF BUFFER bf-cust FOR cust.
    DEF VAR v-first-ship-id AS cha NO-UNDO.


    RUN oe/getNextRelNo.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
    FIND FIRST bf-cust WHERE bf-cust.cust-no EQ bf-oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN
        v-ship-id = IF AVAIL oe-rel THEN oe-rel.ship-id ELSE ""
        v-carrier = IF AVAIL oe-rel THEN oe-rel.carrier ELSE ""
        .
    IF AVAIL(bf-cust) AND bf-cust.ACTIVE = "X" AND v-last-shipto GT "" THEN
        v-ship-id = v-last-shipto.
    FIND FIRST bf-rel WHERE bf-rel.company = bf-oe-ord.company
        AND bf-rel.ord-no = bf-oe-ord.ord-no
        AND bf-rel.i-no = bf-oe-ordl.i-no 
        AND bf-rel.LINE = bf-oe-ordl.LINE
        NO-LOCK NO-ERROR.
    v-first-ship-id = IF AVAIL bf-rel THEN bf-rel.ship-id ELSE "".


    lv-rel-recid = RECID(oe-rel).
    ASSIGN 
        v-qty-sum = 0.

    CREATE oe-rel.

    IF AVAIL bf-oe-ordl THEN 
    DO:

        FIND FIRST bf-oe-ord OF bf-oe-ordl NO-LOCK.
        FOR EACH bf-rel WHERE bf-rel.company = bf-oe-ord.company
            AND bf-rel.ord-no = bf-oe-ord.ord-no
            AND bf-rel.i-no = bf-oe-ordl.i-no 
            AND bf-rel.LINE = bf-oe-ordl.LINE
            NO-LOCK:
            FIND FIRST s-code
                WHERE s-code.reftable EQ "oe-rel.s-code"
                AND s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
                NO-LOCK NO-ERROR.
            IF NOT AVAIL s-code OR CAN-DO("B,S",s-code.code) THEN
                v-qty-sum = v-qty-sum + bf-rel.qty. 
        END.

        /*      if v-qty-sum GE bf-oe-ordl.qty + (bf-oe-ordl.qty * (bf-oe-ordl.over-pct / 100)) then */
        /*         message "Total Planned release quantity will exceed the Or" +                     */
        /*                         "der quantity + the Underrun %."                                  */
        /*                 view-as alert-box warning.                                                */

        /* Calling program validates this record exists */
        FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "OECARIER"
            NO-LOCK NO-ERROR.

        RELEASE shipto.

        IF v-carrier = "" THEN 
        DO:  /* NK1 OECARIER */
            IF sys-ctrl.char-fld EQ "ShipTo" THEN 
            DO:
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ bf-oe-ord.company
                    AND shipto.cust-no EQ bf-oe-ord.cust-no
                    AND shipto.ship-id EQ v-ship-id NO-ERROR.
                v-carrier = IF AVAIL shipto THEN shipto.carrier ELSE "".
            END.
            ELSE IF sys-ctrl.char-fld EQ "Header" THEN v-carrier = bf-oe-ord.carrier.
        END.

        /* wfk - not implemented since there is no tt-report */
        /*     IF NOT AVAIL shipto THEN                                                         */
        /*     FOR EACH shipto                                                                  */
        /*         WHERE shipto.company  EQ cocode                                              */
        /*            AND shipto.cust-no EQ (IF lv-cust-x NE ""         AND                     */
        /*                                      tt-report.s-code EQ "T" THEN lv-cust-x          */
        /*                                                              ELSE bf-oe-ord.cust-no) */
        /*         NO-LOCK                                                                      */
        /*         BREAK BY shipto.ship-no DESC:                                                */
        /*       IF shipto.ship-id EQ bf-oe-ord.cust-no OR LAST(shipto.ship-no) THEN LEAVE.     */
        /*     END.                                                                             */

        IF v-carrier EQ "" AND AVAIL shipto THEN v-carrier = shipto.carrier.

        ASSIGN 
            oe-rel.company      = cocode
            oe-rel.loc          = locode
            oe-rel.ord-no       = bf-orig-oe-rel.ord-no
            oe-rel.i-no         = bf-orig-oe-rel.i-no
            oe-rel.cust-no      = bf-orig-oe-rel.cust-no
            oe-rel.po-no        = bf-orig-oe-rel.po-no 
            oe-rel.qty          = ipi-qty
            oe-rel.line         = bf-orig-oe-rel.line
            oe-rel.s-comm[1]    = bf-orig-oe-rel.s-comm[1]
            oe-rel.s-comm[2]    = bf-orig-oe-rel.s-comm[2]
            oe-rel.s-comm[3]    = bf-orig-oe-rel.s-comm[3]
            oe-rel.s-name[1]    = bf-orig-oe-rel.s-name[1]
            oe-rel.s-name[2]    = bf-orig-oe-rel.s-name[2]
            oe-rel.s-name[3]    = bf-orig-oe-rel.s-name[3]
            oe-rel.s-pct[1]     = bf-orig-oe-rel.s-pct[1]
            oe-rel.s-pct[2]     = bf-orig-oe-rel.s-pct[2]
            oe-rel.s-pct[3]     = bf-orig-oe-rel.s-pct[3]
            oe-rel.sman[1]      = bf-orig-oe-rel.sman[1]
            oe-rel.sman[2]      = bf-orig-oe-rel.sman[2]
            oe-rel.sman[3]      = bf-orig-oe-rel.sman[3]
            oe-rel.sold-no      = bf-orig-oe-rel.sold-no
            oe-rel.carrier      = bf-orig-oe-rel.carrier
            oe-rel.spare-char-1 = bf-orig-oe-rel.spare-char-1
            oe-rel.r-no         = v-nxt-r-no.


        oe-rel.rel-date = bf-orig-oe-rel.rel-date.


        ASSIGN 
            oe-rel.ship-addr[1] = bf-orig-oe-rel.ship-addr[1]
            oe-rel.ship-city    = bf-orig-oe-rel.ship-city
            oe-rel.ship-state   = bf-orig-oe-rel.ship-state
            oe-rel.ship-zip     = bf-orig-oe-rel.ship-zip
            oe-rel.ship-no      = bf-orig-oe-rel.ship-no
            oe-rel.ship-id      = bf-orig-oe-rel.ship-id
            oe-rel.ship-i[1]    = bf-orig-oe-rel.ship-i[1]
            oe-rel.ship-i[2]    = bf-orig-oe-rel.ship-i[2]
            oe-rel.ship-i[3]    = bf-orig-oe-rel.ship-i[3]
            oe-rel.ship-i[4]    = bf-orig-oe-rel.ship-i[4]
            oe-rel.lot-no       = bf-orig-oe-rel.lot-no.

        RUN CopyShipNote (bf-orig-oe-rel.rec_key, oe-rel.rec_key).
        
        IF oe-rel.qty LT 0 THEN oe-rel.qty = 0.

        oe-rel.tot-qty = oe-rel.qty.

        IF oe-rel.rel-date LE v-lst-rel THEN oe-rel.rel-date = v-lst-rel + 1.

        RUN add-rel-assign-logic (INPUT ipr-rel-row, INPUT ipi-qty).

  
    END.

END PROCEDURE.

PROCEDURE CopyShipNote PRIVATE:
/*------------------------------------------------------------------------------
 Purpose: Copies Ship Note from rec_key to rec_key
 Notes:
------------------------------------------------------------------------------*/
DEFINE INPUT PARAMETER ipcRecKeyFrom AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcRecKeyTo AS CHARACTER NO-UNDO.

DEFINE VARIABLE hNotesProcs AS HANDLE NO-UNDO.

    RUN "sys/NotesProcs.p" PERSISTENT SET hNotesProcs.  

    RUN CopyShipNote IN hNotesProcs (ipcRecKeyFrom, ipcRecKeyTo).

    DELETE OBJECT hNotesProcs.   

END PROCEDURE.

PROCEDURE farmoutComp:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cJob    AS CHAR NO-UNDO.
    DEF VAR iJobNo2 AS INT  NO-UNDO.
    cJob = "".
    iJobNo2 = 0.   

    IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN 
    DO:

        /* Find a job for this po if this is a farmout */
        IF w-fg-rctd.job-no GT "" THEN
            ASSIGN cJob    = w-fg-rctd.job-no
                iJobNo2 = w-fg-rctd.job-no2.
        ELSE IF w-fg-rctd.po-no GT "" THEN  
            DO:
                FIND FIRST po-ordl WHERE po-ordl.company EQ w-fg-rctd.company
                    AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
                    AND po-ordl.i-no  EQ w-fg-rctd.i-no
                    NO-LOCK NO-ERROR.

                IF AVAIL(po-ordl) AND po-ordl.ord-no GT 0 THEN 
                DO:

                    FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                        AND oe-ordl.ord-no EQ po-ordl.ord-no
                        AND oe-ordl.i-no   EQ po-ordl.i-no
                        NO-LOCK NO-ERROR.

                    /* assumption is that for farm jobs, order and job are always the same */
                    /* This is to obtain the job-no2 since job-no is assumed to be the order # */
                    IF NOT AVAIL oe-ordl THEN
                        FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                            AND oe-ordl.ord-no EQ po-ordl.ord-no
                            AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                            NO-LOCK NO-ERROR.

                    IF AVAIL oe-ordl AND oe-ordl.job-no GT "" THEN
                        ASSIGN cJob    = oe-ordl.job-no
                            iJobNo2 = oe-ordl.job-no2.
                END.
                ELSE IF AVAIL(po-ordl) AND po-ordl.job-no GT "" THEN 
                    DO:
                        ASSIGN 
                            cJob    = po-ordl.job-no
                            iJobNo2 = po-ordl.job-no2.
                    END.

            END.


        FIND FIRST job WHERE job.company EQ w-fg-rctd.company
            AND job.job-no EQ cJob
            AND job.job-no2 EQ iJobNo2
            NO-LOCK NO-ERROR.

        IF AVAIL job AND cJob GT "" 
            AND w-fg-rctd.rita-code EQ "R" 
            AND w-fg-rctd.qty GT 0 THEN 
        DO:             
            /* Copy fg-rctd for the jobs farmout tab */
            CREATE job-farm-rctd.
            BUFFER-COPY w-fg-rctd EXCEPT rec_key rita-code TO job-farm-rctd.
            ASSIGN 
                job-farm-rctd.rita-code = "F"
                job-farm-rctd.job-no    = cJob
                job-farm-rctd.job-no2   = iJobNo2.
            /* ASSIGN job-farm-rctd.job = job.job. */
            RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
        END.

    END.


END PROCEDURE.

PROCEDURE fg-post:
    DEFINE BUFFER b-fg-rcpts  FOR fg-rcpts.
    DEFINE BUFFER b-fg-rdtl   FOR fg-rdtl.
    DEFINE BUFFER b-fg-bin    FOR fg-bin.
    DEFINE BUFFER b-itemfg    FOR itemfg.
    DEFINE BUFFER b-itemfg1   FOR itemfg.
    DEFINE BUFFER ps-rctd     FOR fg-rctd .
    DEFINE BUFFER b-po-ordl   FOR po-ordl.
    DEFINE BUFFER b-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER b-w-fg-rctd FOR w-fg-rctd.  
    DEFINE VARIABLE v-one-item     AS LOG.
    DEFINE VARIABLE v-dec          AS DECIMAL   DECIMALS 10.
    DEFINE VARIABLE v-po-no        LIKE rm-rcpt.po-no NO-UNDO.
    DEFINE VARIABLE x              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE i              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-r-qty        LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-i-qty        LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-t-qty        LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-overrun-qty  LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-underrun-qty LIKE fg-rctd.qty NO-UNDO.
    DEFINE VARIABLE v-reduce-qty   AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-est-no       AS cha       NO-UNDO.
    DEFINE VARIABLE v-recid        AS RECID     NO-UNDO.
    DEFINE VARIABLE v-cost         AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-binqty       AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-qty          AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-tagcost      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cvt-qty     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE ld-cvt-cost    AS DECIMAL   DECIMALS 10 NO-UNDO.
    DEFINE VARIABLE v-autobin      AS cha       NO-UNDO.
    DEFINE VARIABLE v-newhdr       AS LOG       NO-UNDO. 
    DEFINE VARIABLE v-fin-qty      AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE choice         AS LOG       NO-UNDO.
    DEFINE VARIABLE v-trnum        LIKE gl-ctrl.trnum NO-UNDO.
    DEFINE VARIABLE uperiod        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE sysdate        AS DATE      INIT TODAY NO-UNDO.    
    DEFINE VARIABLE v-date         LIKE sysdate NO-UNDO.
    DEFINE VARIABLE v-underrun     AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE v-qty-received AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-got-fgemail  AS LOG       NO-UNDO.
    DEFINE VARIABLE v-fgemail-file AS cha       NO-UNDO.
    DEFINE VARIABLE li-tag-no      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE ll-qty-changed AS LOG       NO-UNDO.
    DEFINE VARIABLE ll-whs-item    AS LOG       NO-UNDO.
    DEFINE VARIABLE v-calc-cost    AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE cJob           LIKE oe-ordl.job-no NO-UNDO.
    DEFINE VARIABLE iJobNo2        LIKE oe-ordl.job-no2 NO-UNDO.
    DEFINE VARIABLE iRNo           AS INTEGER   NO-UNDO.
    /*##PN - variable for FGSetAssembly setting*/

    DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFGSetAssembly AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFGSetAssembly AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cFGSetAdjust   AS CHARACTER NO-UNDO.

    SESSION:SET-WAIT-STATE ("general").

    FIND FIRST period NO-LOCK
        WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

    FIND FIRST sys-ctrl  WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "AUTOPOST"
        NO-LOCK NO-ERROR.
    v-autobin = IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "".
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAssembly",
        INPUT "L",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAssembly,
        OUTPUT lFound).
    IF lFound THEN
        lFGSetAssembly = cFGSetAssembly EQ "YES".
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAssembly",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAssembly,
        OUTPUT lFound).
    RUN sys/ref/nk1look.p (INPUT cocode,
        INPUT "FGSetAdjustReason",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cFGSetAdjust,
        OUTPUT lFound).
    
    DISABLE TRIGGERS FOR LOAD OF itemfg.
    DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

    /* Handle Manually created job farm out records */
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code = "F":       
        RUN manualFarmOut.
        DELETE w-fg-rctd.
    END.
    
    FIND FIRST w-fg-rctd NO-ERROR.
    /* In case only processing rita-code F */
    IF NOT AVAILABLE w-fg-rctd THEN 
    DO:   
        RETURN.
    END.

     
    /* #pn# Setting rita-code to A since the negative R was causing problems */
    /* #pn# task 08211305                                                    */   
    /* b-w-fg-rctd are components with negative qty, w-fg-rctd are sets */
    FOR EACH b-w-fg-rctd WHERE b-w-fg-rctd.qty LT 0,
        FIRST fg-rctd EXCLUSIVE-LOCK WHERE ROWID(fg-rctd) = b-w-fg-rctd.row-id  
          AND fg-rctd.SetHeaderRno GT 0:
      
        FIND FIRST w-fg-rctd NO-LOCK WHERE w-fg-rctd.r-no EQ fg-rctd.SetHeaderRno NO-ERROR. 
        IF NOT AVAILABLE w-fg-rctd THEN 
            NEXT. 
      
        FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode 
            AND itemfg.i-no    EQ w-fg-rctd.i-no
            NO-ERROR.

        IF AVAILABLE fg-rctd  THEN 
        DO:
            ASSIGN 
                b-w-fg-rctd.rita-code = "A"
                fg-rctd.rita-code   = "A"
                b-w-fg-rctd.reject-code = cFGSetAdjust
                fg-rctd.reject-code   = cFGSetAdjust
                .
        END.
        RELEASE fg-rctd.

    END.
  
    FOR EACH w-fg-rctd
        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.trans-time
        BY w-fg-rctd.r-no:

        IF NOT CAN-FIND(FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no    EQ w-fg-rctd.i-no) THEN
            NEXT.

        loop1:
        REPEAT:

            FIND FIRST itemfg WHERE
                itemfg.company EQ cocode AND
                itemfg.i-no    EQ w-fg-rctd.i-no
                EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

            IF AVAILABLE itemfg THEN
            DO:           
                /* If FGEMAIL is active and quantity on hand is zero and item is allocated,
                   then process user data into a temp-table for processing emails later. */
                IF gv-fgemail = YES AND (itemfg.q-onh = 0 AND itemfg.q-alloc > 0) THEN
                    RUN Process-FGemail-Data (INPUT itemfg.i-no, w-fg-rctd.t-qty,w-fg-rctd.po-no).

          /* itemfg gets updated here. */
                    {fg/fg-post.i w-fg-rctd w-fg-rctd}

                IF autofgissue-log THEN
                    RUN farmOutComp.

                FIND CURRENT itemfg NO-LOCK NO-ERROR.
                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                FIND CURRENT po-ordl NO-LOCK NO-ERROR.
                FIND CURRENT fg-bin NO-LOCK NO-ERROR.
                LEAVE loop1.
            END. /* IF AVAIL itemfg */
        END. /* loop1 REPEAT */

        IF w-fg-rctd.rita-code = "R" THEN 
        DO:
            /* Creates tt-email records */
        {fg/fgemails.i}
        END.

        FIND FIRST fg-rctd EXCLUSIVE-LOCK 
                     WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

        IF AVAILABLE fg-rctd THEN 
        DO:
            ASSIGN
                fg-rctd.rita-code  = "P"  /* posted */
                fg-rctd.post-date  = v-post-date
                fg-rctd.trans-time = TIME
                fg-rctd.tag2       = w-fg-rctd.tag2.

            FOR EACH fg-rcpts EXCLUSIVE-LOCK
                WHERE fg-rcpts.company EQ fg-rctd.company
                  AND fg-rcpts.r-no    EQ fg-rctd.r-no:
                ASSIGN 
                    fg-rcpts.rita-code = fg-rctd.rita-code.
            END.

            
        END.
        FIND CURRENT fg-rctd NO-LOCK NO-ERROR.
    END.  /* for each w-fg-rctd */

    FOR EACH w-fg-rctd
        BREAK BY w-fg-rctd.i-no
        BY w-fg-rctd.job-no
        BY w-fg-rctd.job-no2
        BY w-fg-rctd.loc
        BY w-fg-rctd.loc-bin
        BY w-fg-rctd.tag:

        IF FIRST-OF(w-fg-rctd.i-no) THEN 
        DO:
            FIND FIRST tt-posted-items WHERE tt-posted-items.i-no = w-fg-rctd.i-no
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tt-posted-items THEN 
            DO:
                CREATE tt-posted-items.
                ASSIGN 
                    tt-posted-items.i-no = w-fg-rctd.i-no.
            END.
        END.

        IF LAST-OF(w-fg-rctd.tag) THEN 
        DO:
            IF TRIM(w-fg-rctd.tag) NE "" THEN 
                /* Ensure Bin/Tags Qty is correct.  Task 01270602 */
                FOR EACH fg-bin NO-LOCK
                    WHERE fg-bin.company EQ g_company
                    AND fg-bin.i-no    EQ w-fg-rctd.i-no
                    AND fg-bin.tag     EQ w-fg-rctd.tag
                    USE-INDEX tag:

                    RUN fg/calcbinq.p (ROWID(fg-bin)).
                END.

            FIND FIRST loadtag
                WHERE loadtag.company   EQ g_company
                AND loadtag.item-type EQ NO
                AND loadtag.tag-no    EQ w-fg-rctd.tag
                AND loadtag.i-no      EQ w-fg-rctd.i-no
                AND loadtag.job-no    EQ w-fg-rctd.job-no
                USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.

            IF AVAILABLE loadtag THEN 
            DO:
                FIND FIRST fg-bin
                    WHERE fg-bin.company EQ g_company
                    AND fg-bin.i-no    EQ loadtag.i-no
                    AND fg-bin.tag     EQ loadtag.tag-no
                    AND fg-bin.qty     GT 0
                    USE-INDEX tag NO-LOCK NO-ERROR.
                IF AVAILABLE fg-bin AND w-fg-rctd.rita-code = "T" AND
                    TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN /* full qty transfer*/ 

                    ASSIGN
                        loadtag.loc          = w-fg-rctd.loc2   
                        loadtag.loc-bin      = w-fg-rctd.loc-bin2
                        loadtag.qty          = fg-bin.qty
                        loadtag.pallet-count = fg-bin.qty
                        loadtag.partial      = fg-bin.partial-count
                        loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.

                ELSE /*partial transfer */
                    ASSIGN
                        loadtag.loc     = w-fg-rctd.loc
                        loadtag.loc-bin = w-fg-rctd.loc-bin.

                FIND CURRENT loadtag NO-LOCK.
            END.
        END.

    
    END.

    FOR EACH w-inv:
        /* Save w-inv data to send email bol's */
        DELETE w-inv.
    END.

    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

        CREATE w-inv.
        w-inv.row-id = w-fg-rctd.row-id.
    END.

    RUN fg/invrecpt.p (?, 2).

    FOR EACH w-inv:
        /* Save w-inv data to send email bol's */
        CREATE tt-inv.
        BUFFER-COPY w-inv TO tt-inv.

    END.

    FOR EACH w-fg-rctd WHERE (TRIM(w-fg-rctd.tag) EQ "" OR v-cost-from-receipt = "TransferCost"),
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:

        IF LAST-OF(w-fg-rctd.i-no) THEN 
        DO:

            RUN fg/updfgcs1.p (RECID(itemfg), NO).

            /* Calculate this once per item instead of per order line */
            IF v-cost-from-receipt = "TransferCost" AND itemfg.spare-dec-1 EQ 0 THEN 
            DO:
                /* override for v-cost-from-receipt */
                IF w-fg-rctd.job-no GT "" THEN 
                DO:
                    FIND job-hdr WHERE job-hdr.company = cocode
                        AND job-hdr.job-no  = w-fg-rctd.job-no
                        AND job-hdr.job-no2 = w-fg-rctd.job-no2
                        AND job-hdr.i-no    = w-fg-rctd.i-no
                        NO-LOCK NO-ERROR.
                END. /* Job-no gt "" */
                IF w-fg-rctd.po-no GT "" THEN
                    FIND FIRST po-ordl WHERE po-ordl.company EQ cocode
                        AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
                        AND po-ordl.i-no  EQ w-fg-rctd.i-no
                        NO-LOCK NO-ERROR.

                IF NOT ((AVAIL(job-hdr) AND job-hdr.ord-no GT 0) OR
                    (AVAIL(po-ordl) AND po-ordl.ord-no GT 0)) THEN

                    RUN sys/ref/convcuom.p("EA",
                        "M", 0, 0, 0, 0,
                        w-fg-rctd.ext-cost / w-fg-rctd.t-qty, OUTPUT v-calc-cost).

            END. /* If v-cost-from-receipt = TransferCost */


            FOR EACH oe-ordl
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.opened  EQ YES
                AND oe-ordl.i-no    EQ w-fg-rctd.i-no
                AND oe-ordl.job-no  EQ ""
                /*  AND oe-ordl.cost    EQ 0*/
                USE-INDEX opened NO-LOCK
                BREAK BY oe-ordl.ord-no
                TRANSACTION:

                v-calc-cost = oe-ordl.cost.

                IF oe-ordl.cost NE 0 AND NOT v-cost-from-receipt = "TransferCost" THEN
                    NEXT.

                /* Default to standard cost, or accept calculated value from code above */
                IF NOT (v-cost-from-receipt = "TransferCost" AND itemfg.spare-dec-1 EQ 0) THEN 
                DO:
                    IF oe-ordl.cost EQ 0 THEN 
                    DO:

                        IF itemfg.prod-uom EQ "M" THEN
                            v-calc-cost = itemfg.total-std-cost.
                        ELSE
                            RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                THEN "EA" ELSE itemfg.prod-uom),
                                "M", 0, 0, 0, 0,
                                itemfg.total-std-cost, OUTPUT v-calc-cost).
                    END. /* If cost EQ 0 */

                END. /* Not TransferCost */

                /* WFK - process is too slow, so only update if its available */
                FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
                IF b-oe-ordl.cost NE v-calc-cost THEN
                    b-oe-ordl.cost = v-calc-cost.

            END. /* each oe-ordl */
        END. /* last of i-no */
    END. /* each w-fg-rctd */

    /* If overage, reset quantity or create a new release */
    RUN process-releases.

    IF v-fgpostgl NE "None" THEN 
    DO TRANSACTION:

        loop2:
        REPEAT:
            FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN
            DO:
                ASSIGN
                    v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE loop2.
            END.
        END.

        RUN gl-from-work (1, v-trnum).
        RUN gl-from-work (2, v-trnum).
    END.
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    FIND FIRST w-job NO-ERROR.
    IF AVAILABLE w-job THEN 
    DO:
        RUN jc/d-jclose.w.
    END.

    IF v-adjustgl THEN 
    DO TRANSACTION:
        /** GET next G/L TRANS. POSTING # **/
        REPEAT:
            FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.

            IF AVAILABLE gl-ctrl THEN
            DO:
                ASSIGN
                    v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum.
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END.
        END.

        FOR EACH work-job BREAK BY work-job.actnum:
            CREATE gltrans.
            ASSIGN
                gltrans.company = cocode
                gltrans.actnum  = work-job.actnum
                gltrans.jrnl    = "ADJUST"
                gltrans.tr-date = v-post-date
                gltrans.period  = period.pnum
                gltrans.trnum   = v-trnum.

            IF work-job.fg THEN
                ASSIGN
                    gltrans.tr-amt  = - work-job.amt
                    gltrans.tr-dscr = "ADJUSTMENT FG".
            ELSE
                ASSIGN
                    gltrans.tr-amt  = work-job.amt
                    gltrans.tr-dscr = "ADJUSTMENT COGS".

            RELEASE gltrans.
        END. /* each work-job */
    END.

    IF tg-recalc-cost THEN 
    DO:
        FOR EACH tt-posted-items:        
            RUN fg/updfgcst.p (tt-posted-items.i-no).
        END.
    END.

/*    Now handled in calling program                                 */
/*    IF v-got-fgemail THEN                                          */
/*    DO:                                                            */
/*        IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').*/
/*        RUN send-fgemail (v-fgemail-file).                         */
/*        IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').  */
/*    END.                                                           */

/*    Now handled in calling program                                                                            */
/*    FOR EACH w-fg-rctd ,                                                                                      */
/*        FIRST itemfg                                                                                          */
/*        WHERE itemfg.company EQ cocode                                                                        */
/*        AND itemfg.i-no    EQ w-fg-rctd.i-no NO-LOCK ,                                                        */
/*        EACH tt-inv WHERE tt-inv.row-id EQ w-fg-rctd.row-id                                                   */
/*        BREAK BY tt-inv.bol-no:                                                                               */
/*                                                                                                              */
/*        FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-LOCK NO-ERROR.                         */
/*        RUN get-ord-recs (ROWID(fg-rctd),                                                                     */
/*            BUFFER po-ordl,                                                                                   */
/*            BUFFER po-ord,                                                                                    */
/*            BUFFER oe-ordl,                                                                                   */
/*            BUFFER oe-ord,                                                                                    */
/*            BUFFER reftable).                                                                                 */
/*        IF AVAIL(reftable) AND (reftable.val[2] GT 0 OR reftable.val[3] EQ 1) THEN                            */
/*            ASSIGN ll        = reftable.val[1] NE 0                                                           */
/*                dBillAmt  = reftable.val[2]                                                                   */
/*                lEmailBol = reftable.val[3] EQ 1                                                              */
/*                lInvFrt   = reftable.val[1] GT 0.                                                             */
/*        IF lEmailBol AND last-of(tt-inv.bol-no) THEN                                                          */
/*        DO:                                                                                                   */
/*            FIND FIRST oe-bolh WHERE oe-bolh.company EQ g_company                                             */
/*                AND oe-bolh.bol-no EQ tt-inv.bol-no NO-LOCK NO-ERROR.                                         */
/*                                                                                                              */
/*            RUN custom/setUserPrint.p (g_company,'oe-boll_.',                                                 */
/*                'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert',*/
/*                oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +                                               */
/*                STRING(oe-bolh.bol-no) + ',' + STRING(oe-bolh.bol-no) +                                       */
/*                ',,99999999,' + STRING(oe-bolh.printed) + ',' +                                               */
/*                STRING(oe-bolh.posted) + ',BOL').                                                             */
/*            RUN listobjs/oe-boll_.w.                                                                          */
/*                                                                                                              */
/*        END. /* If email bol */                                                                               */
/*    END. /* each w-fg-rctd */                                                                                 */

    /* WFK - no error message was being returned, so set to no if */
    /*       no return error was encountered                      */
    ERROR-STATUS:ERROR = NO.

    SESSION:SET-WAIT-STATE ("").
END.

PROCEDURE fgpostlog:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

    PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
        STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.


END PROCEDURE.

PROCEDURE get-ord-recs:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEF INPUT PARAM ip-rowid1  AS  ROWID NO-UNDO.

    DEF PARAM BUFFER b-po-ordl FOR po-ordl.
    DEF PARAM BUFFER b-po-ord  FOR po-ord.
    DEF PARAM BUFFER b-oe-ordl FOR oe-ordl.
    DEF PARAM BUFFER b-oe-ord  FOR oe-ord.
    DEF PARAM BUFFER b-ref     FOR reftable.

    DEF       BUFFER b-fg-rctd FOR fg-rctd.

    RELEASE b-po-ordl.
    RELEASE b-po-ord.
    RELEASE b-oe-ordl.
    RELEASE b-oe-ord.
    RELEASE b-ref.


    FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ ip-rowid1 NO-LOCK NO-ERROR.

    IF AVAIL b-fg-rctd THEN 
    DO:
        IF INT(b-fg-rctd.po-no) NE 0 AND b-fg-rctd.qty NE 0 THEN
            FIND FIRST b-po-ordl
                WHERE b-po-ordl.company   EQ b-fg-rctd.company
                AND b-po-ordl.po-no     EQ INT(b-fg-rctd.po-no)
                AND b-po-ordl.i-no      EQ b-fg-rctd.i-no
                AND b-po-ordl.job-no    EQ b-fg-rctd.job-no
                AND b-po-ordl.job-no2   EQ b-fg-rctd.job-no2
                AND b-po-ordl.item-type EQ NO
                AND b-po-ordl.ord-no    NE 0
                NO-LOCK NO-ERROR.

        IF AVAIL b-po-ordl THEN
            FIND FIRST b-po-ord
                WHERE b-po-ord.company EQ b-po-ordl.company
                AND b-po-ord.po-no   EQ b-po-ordl.po-no
                AND b-po-ord.type    EQ "D"
                NO-LOCK NO-ERROR.

        IF AVAIL b-po-ord THEN
            FIND FIRST b-oe-ordl
                WHERE b-oe-ordl.company  EQ b-po-ordl.company
                AND b-oe-ordl.ord-no   EQ b-po-ordl.ord-no
                AND b-oe-ordl.i-no     EQ b-po-ordl.i-no
                AND b-oe-ordl.vend-no  EQ b-po-ord.vend-no
                AND b-oe-ordl.po-no-po EQ b-po-ord.po-no
                NO-LOCK NO-ERROR.

        IF AVAIL b-oe-ordl THEN
            FIND FIRST b-oe-ord
                WHERE b-oe-ord.company EQ b-oe-ordl.company
                AND b-oe-ord.ord-no  EQ b-oe-ordl.ord-no
                NO-LOCK NO-ERROR.


    END.


END PROCEDURE.

PROCEDURE gl-from-work:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAM ip-run AS INT NO-UNDO.
    DEF INPUT PARAM ip-trnum AS INT NO-UNDO.

    DEF VAR credits AS DEC INIT 0 NO-UNDO.
    DEF VAR debits  AS DEC INIT 0 NO-UNDO. 


    FIND FIRST period
        WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
        NO-LOCK.

    FOR EACH work-gl 
        WHERE (ip-run EQ 1 AND work-gl.job-no NE "")
        OR (ip-run EQ 2 AND work-gl.job-no EQ "")
        BREAK BY work-gl.actnum:

        ASSIGN
            debits  = debits  + work-gl.debits
            credits = credits + work-gl.credits.

        IF LAST-OF(work-gl.actnum) THEN 
        DO:
            CREATE gltrans.
            ASSIGN
                gltrans.company = cocode
                gltrans.actnum  = work-gl.actnum
                gltrans.jrnl    = "FGPOST"
                gltrans.period  = period.pnum
                gltrans.tr-amt  = debits - credits
                gltrans.tr-date = v-post-date
                gltrans.tr-dscr = IF work-gl.job-no NE "" THEN "FG Receipt from Job"
                                                 ELSE "FG Receipt from PO"
                gltrans.trnum   = ip-trnum
                debits          = 0
                credits         = 0.

            RELEASE gltrans.
        END.
    END.


END PROCEDURE.

PROCEDURE manualFarmout:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR cJob    AS CHAR NO-UNDO.
    DEF VAR iJobNo2 AS INT  NO-UNDO.
    DEF BUFFER bf-fg-rctd FOR fg-rctd.

    FIND itemfg WHERE itemfg.company EQ cocode 
        AND itemfg.i-no EQ w-fg-rctd.i-no
        NO-LOCK NO-ERROR.

    IF NOT AVAIL itemfg THEN
        RETURN.
    cJob = "".
    iJobNo2 = 0.   

    IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN 
    DO:

        /* Find a job for this po if this is a farmout */
        IF w-fg-rctd.job-no GT "" THEN
            ASSIGN cJob    = w-fg-rctd.job-no
                iJobNo2 = w-fg-rctd.job-no2.
        ELSE IF w-fg-rctd.po-no GT "" THEN  
            DO:
                FIND FIRST po-ordl WHERE po-ordl.company EQ w-fg-rctd.company
                    AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
                    AND po-ordl.i-no  EQ w-fg-rctd.i-no
                    NO-LOCK NO-ERROR.

                IF AVAIL(po-ordl) AND po-ordl.ord-no GT 0 THEN 
                DO:

                    FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                        AND oe-ordl.ord-no EQ po-ordl.ord-no
                        AND oe-ordl.i-no   EQ po-ordl.i-no
                        NO-LOCK NO-ERROR.
                    /* assumption is that for farm jobs, order and job are always the same */
                    /* This is to obtain the job-no2 since job-no is assumed to be the order # */
                    IF NOT AVAIL oe-ordl THEN
                        FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                            AND oe-ordl.ord-no EQ po-ordl.ord-no
                            AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                            NO-LOCK NO-ERROR.

                    IF AVAIL oe-ordl AND oe-ordl.job-no GT "" THEN
                        ASSIGN cJob    = oe-ordl.job-no
                            iJobNo2 = oe-ordl.job-no2.
                END.

            END.

        FIND FIRST job WHERE job.company EQ w-fg-rctd.company
            AND job.job-no EQ cJob
            AND job.job-no2 EQ iJobNo2
            NO-LOCK NO-ERROR.

        IF AVAIL job AND cJob GT "" 
            AND w-fg-rctd.rita-code EQ "F" 
            THEN 
        DO:             

            /* Copy fg-rctd for the jobs farmout tab */
            CREATE job-farm-rctd.
            BUFFER-COPY w-fg-rctd EXCEPT rec_key TO job-farm-rctd.
            ASSIGN 
                job-farm-rctd.job-no  = cJob
                job-farm-rctd.job-no2 = iJobNo2.
            /* ASSIGN job-farm-rctd.job = job.job. */

            RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
        END.
        FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ w-fg-rctd.row-id
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAIL bf-fg-rctd THEN
            DELETE bf-fg-rctd.


    END.

END PROCEDURE.

PROCEDURE process-FGEmail-data:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pc-i-no  AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER pi-qty   AS DEC NO-UNDO.
    DEFINE INPUT PARAMETER pc-po-no AS CHAR NO-UNDO.


    DEFINE BUFFER buf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER buf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    DEFINE VARIABLE cUserList AS CHAR NO-UNDO INIT "".
    DEFINE VARIABLE xOrdNo    LIKE oe-ordl.ord-no NO-UNDO.


    FIND FIRST bf-po-ordl 
        WHERE bf-po-ordl.company EQ g_company
        AND bf-po-ordl.po-no EQ INT(pc-po-no)
        AND bf-po-ordl.i-no EQ pc-i-no
        AND bf-po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL bf-po-ordl THEN 
    DO:
        xOrdNo = bf-po-ordl.ord-no.
        /* Find user ID on each open order of item. */
        FOR EACH buf-oe-ordl   
            WHERE buf-oe-ordl.company   = g_company 
            AND buf-oe-ordl.opened    = YES 
            AND buf-oe-ordl.i-no      = pc-i-no
            AND buf-oe-ordl.ord-no    = xOrdNo
            NO-LOCK:

            FIND buf-oe-ord OF buf-oe-ordl NO-LOCK NO-ERROR.

            /* If no order header, then skip this orderline. */
            IF NOT AVAILABLE buf-oe-ord THEN NEXT.

            FIND FIRST users NO-LOCK WHERE
                users.USER_id = buf-oe-ord.USER-ID NO-ERROR.

            /* If no user, then skip. */
            IF NOT AVAILABLE users THEN NEXT.

            /* If no user email address, then skip. */
            IF users.image_filename = "" OR users.image_filename = ? THEN NEXT.

            /* Create email record for this received item. */
            CREATE tt-fgemail.
            ASSIGN 
                tt-fgemail.i-no      = pc-i-no
                tt-fgemail.po-no     = pc-po-no
                tt-fgemail.ord-no    = buf-oe-ordl.ord-no
                tt-fgemail.qty-rec   = pi-qty
                tt-fgemail.Recipient = TRIM(users.image_filename).

        END.  /*each buf-oe-ordl*/
    END. /*Avail bf-po-ordl*/

    RETURN.

END PROCEDURE.

PROCEDURE process-rel-stat:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
    DEF OUTPUT PARAMETER opi-stat AS INT NO-UNDO.
    DEF OUTPUT PARAMETER opi-rel-qty AS INT NO-UNDO.
    DEF OUTPUT PARAMETER opr-release AS ROWID NO-UNDO.

    DEF VAR v-highest-stat AS INT   NO-UNDO.
    DEF VAR v-tot-rqty     AS INT   NO-UNDO.
    DEF VAR stat-type      AS INT   NO-UNDO.
    DEF VAR v-chosen-rel   AS ROWID NO-UNDO.
    DEF BUFFER bf-oe-ordl FOR oe-ordl.
    FIND FIRST bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ipr-ordl-row NO-LOCK NO-ERROR.

    v-highest-stat = 0.
    FOR EACH oe-rel WHERE oe-rel.company EQ bf-oe-ordl.company
        AND oe-rel.ord-no  EQ bf-oe-ordl.ord-no
        AND oe-rel.i-no    EQ bf-oe-ordl.i-no
        AND oe-rel.LINE    EQ bf-oe-ordl.LINE
        NO-LOCK.
        /* Determine the status of each to know how to proceed */
        FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
            USE-INDEX ord-no NO-LOCK NO-ERROR.
        IF AVAIL oe-rel THEN
            v-tot-rqty = v-tot-rqty + oe-rel.qty.

        IF LOOKUP(oe-rel.stat, "S,I,L") GT 0 THEN
            stat-type = 1.
        ELSE 
        DO:
            IF LOOKUP(oe-rel.stat, "A,B") GT 0 AND AVAIL oe-rell AND NOT oe-rell.printed THEN
                stat-type = 2.
            IF LOOKUP(oe-rel.stat, "A,B,P,Z,C") GT 0 AND AVAIL oe-rell AND oe-rell.printed THEN
                stat-type = 3.
        END.
        IF stat-type GT v-highest-stat THEN
            ASSIGN v-highest-stat = stat-type v-chosen-rel   = ROWID(oe-rel).
    END.

    ASSIGN
        opi-stat    = v-highest-stat
        opi-rel-qty = v-tot-rqty
        opr-release = v-chosen-rel.

END PROCEDURE.

PROCEDURE process-releases:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF VAR v-tot-rqty     AS INT   NO-UNDO.
    DEF VAR v-rel-qty      AS INT   NO-UNDO.
    DEF VAR v-found-order  LIKE oe-ord.ord-no NO-UNDO.
    DEF VAR v-managed      AS LOG   NO-UNDO.
    DEF VAR stat-type      AS INT   NO-UNDO.
    DEF VAR new-qty        AS INT   NO-UNDO.
    DEF VAR add-qty        AS INT   NO-UNDO.
    DEF VAR v-highest-stat AS INT   NO-UNDO.
    DEF VAR v-chosen-rel   AS ROWID NO-UNDO.
    DEF VAR v-tot-rcv-qty  AS INT   NO-UNDO.

    /* To Be Implemented */
    DEF VAR nk-set         AS LOG   NO-UNDO.

    IF fgpost-int = 1 THEN
        nk-set = TRUE.

    FOR EACH w-fg-rctd
        BREAK BY w-fg-rctd.i-no:

        v-tot-rqty = v-tot-rqty + w-fg-rctd.qty.

        IF LAST-OF(w-fg-rctd.i-no) THEN 
        DO:


            FIND FIRST job-hdr WHERE job-hdr.company = w-fg-rctd.company
                AND job-hdr.job-no = w-fg-rctd.job-no
                AND job-hdr.job-no2 = w-fg-rctd.job-no2
                NO-LOCK NO-ERROR.
            IF AVAIL job-hdr THEN
                FIND FIRST oe-ord WHERE oe-ord.company = job-hdr.company
                    AND oe-ord.ord-no  = integer(job-hdr.job-no)
                    NO-LOCK NO-ERROR.
            IF AVAIL oe-ord THEN
                FIND FIRST oe-ordl WHERE oe-ordl.company = oe-ord.company
                    AND oe-ordl.ord-no  = oe-ord.ord-no
                    AND oe-ordl.i-no    = w-fg-rctd.i-no
                    NO-LOCK NO-ERROR.

            IF AVAIL oe-ordl THEN
                FIND FIRST oe-rel WHERE oe-rel.company = oe-ordl.company
                    AND oe-rel.ord-no  = oe-ordl.ord-no
                    AND oe-rel.LINE    = oe-ordl.LINE
                    NO-LOCK NO-ERROR.

            FIND FIRST itemfg WHERE itemfg.company EQ cocode 
                AND itemfg.i-no    EQ w-fg-rctd.i-no
                NO-LOCK NO-ERROR.
            IF AVAIL oe-ordl THEN
                RUN process-rel-stat (INPUT ROWID(oe-ordl), 
                    OUTPUT v-highest-stat, 
                    OUTPUT v-rel-qty,
                    OUTPUT v-chosen-rel).
            FIND oe-rel WHERE ROWID(oe-rel) EQ v-chosen-rel NO-LOCK NO-ERROR.
            v-tot-rcv-qty = get-tot-rcv-qty().

            /* Managed Inventory indicator */
            IF AVAIL(oe-ordl) AND avail(oe-ord) THEN
                v-managed = oe-ord.whsed OR oe-ordl.whsed.

            IF AVAIL(oe-rel) 
                AND v-managed = YES
                AND avail(oe-ordl) 
                AND nk-set 
                AND v-tot-rcv-qty GT oe-ordl.qty THEN 
            DO:

                /* Set release qty to total received - scheduled release */
                add-qty = v-tot-rcv-qty - v-rel-qty.
                new-qty = v-tot-rcv-qty.
                stat-type = v-highest-stat.

                CASE stat-type:

                    WHEN 1 THEN 
                        DO:              
                            /* Replace release qty */
                            RUN replace-rel-qty (INPUT ROWID(oe-rel), INPUT new-qty).

                        END.
                    WHEN 2 THEN 
                        DO:
                            FIND FIRST oe-rell
                                WHERE oe-rell.company  EQ oe-rel.company                                
                                AND oe-rell.ord-no   EQ oe-rel.ord-no
                                AND oe-rell.i-no     EQ oe-rel.i-no
                                AND oe-rell.line     EQ oe-rel.line
                                AND oe-rell.printed  EQ NO
                                AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                                USE-INDEX r-no NO-LOCK NO-ERROR.

                            /* Replace release qty and actual release qty */
                            RUN replace-rel-qty (INPUT ROWID(oe-rel), INPUT new-qty).
                            IF AVAIL oe-rell THEN
                                RUN replace-actrel-qty (INPUT ROWID(oe-rell), INPUT new-qty).

                        END.
                    WHEN 3 THEN 
                        DO:
                            /* Create Release from existing */
                            RUN add-rel-for-qty (INPUT ROWID(oe-rel), INPUT add-qty).

                        END.

                END CASE.

                ASSIGN
                    v-rel-qty     = 0
                    v-tot-rcv-qty = 0
                    v-found-order = 0.

            END. /* all conditions met */
        END. /* last i-no */
    END. /* each w-fg-rctd */


END PROCEDURE.

PROCEDURE replace-actrel-qty:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipr-rell-row AS ROWID NO-UNDO.
    DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.
    /* wfk - 11/12 - did not find any extra logic that needs to be applied 
       when oe-rell.qty is changed */
    DEF BUFFER bf-oe-rell FOR oe-rell.
    FIND bf-oe-rell WHERE ROWID(bf-oe-rell) EQ ipr-rell-row
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL bf-oe-rell THEN
        RETURN.
    bf-oe-rell.qty = ipi-qty.

END PROCEDURE.

PROCEDURE replace-rel-qty:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
    DEF INPUT PARAMETER ipi-qty     AS INT   NO-UNDO.
    
    DEF BUFFER bf-orig-oe-rel FOR oe-rel.
    FIND bf-orig-oe-rel WHERE ROWID(bf-orig-oe-rel) EQ ipr-rel-row
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAIL bf-orig-oe-rel THEN
        RETURN.

    IF bf-orig-oe-rel.tot-qty EQ bf-orig-oe-rel.qty THEN 
    DO:
        bf-orig-oe-rel.tot-qty = ipi-qty.

    END.
    bf-orig-oe-rel.qty = ipi-qty.
    RUN add-rel-assign-logic (INPUT ipr-rel-row, INPUT ipi-qty).
    RELEASE bf-orig-oe-rel.

END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION get-act-rel-qty RETURNS INTEGER 
	(  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR li      AS INT  NO-UNDO.
    DEF VAR lv-stat AS CHAR NO-UNDO.

    IF AVAIL oe-ordl THEN
        FOR EACH oe-rel WHERE 
            oe-rel.company EQ cocode AND
            oe-rel.ord-no  EQ oe-ordl.ord-no AND
            oe-rel.i-no    EQ oe-ordl.i-no AND
            oe-rel.line    EQ oe-ordl.line
            NO-LOCK:

            RUN oe/rel-stat.p (ROWID(oe-rel), OUTPUT lv-stat).

            IF INDEX("A,B,P",lv-stat) > 0 THEN
                li = li + oe-rel.qty.
        END.

    RETURN li.
		
END FUNCTION.

FUNCTION get-tot-rcv-qty RETURNS INTEGER 
	(  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/	

    DEF VAR v-tot-qty AS INT NO-UNDO.           
    FOR EACH fg-rcpth
        WHERE fg-rcpth.company    EQ oe-ordl.company
        AND fg-rcpth.i-no       EQ oe-ordl.i-no
        AND fg-rcpth.job-no     EQ oe-ordl.job-no
        AND fg-rcpth.rita-code  EQ "R"
        USE-INDEX tran NO-LOCK,
        EACH fg-rdtlh
        WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
        AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code
        NO-LOCK:
        v-tot-qty = v-tot-qty + fg-rdtlh.qty.
    END.
    RETURN v-tot-qty.   /* Function return value. */
		
END FUNCTION.

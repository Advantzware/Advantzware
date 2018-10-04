 /*------------------------------------------------------------------------

  File: fg\fgpstallBatch.w
  
  Usage:
         RUN fg/fgpstallbatch.w (input '001', input 'asi', today, 'R').
         
------------------------------------------------------------------------*/


/* ***************************  Definitions  ************************** */
/* WFK note;  Must determine if v-corr is used anywhere !!!!!!!!!!!!!! */
/* Skipping d-jclose.w !!! */

/* Batch Input Parameters Definitions ---                                           */
DEFINE INPUT PARAMETER ipcCompany       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ipcUserID        AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER ip-post-eom-date AS DATE      NO-UNDO.
DEFINE INPUT PARAMETER ip-run-what      AS CHARACTER NO-UNDO. /* "SETUP" from initial setup (addon/sshoot/sssetups.w), else "" */

/* Actual Selection Parmeters */
DEFINE VARIABLE begin_fg-r-no AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.
DEFINE VARIABLE end_fg-r-no   AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999 NO-UNDO.
DEFINE VARIABLE begin_i-no    AS CHARACTER FORMAT "X(15)":U NO-UNDO.
DEFINE VARIABLE end_i-no      AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz" NO-UNDO.
DEFINE VARIABLE begin_job-no  AS CHARACTER FORMAT "X(6)":U NO-UNDO.
DEFINE VARIABLE end_job-no    AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" NO-UNDO.
DEFINE VARIABLE begin_whs     AS CHARACTER FORMAT "X(5)":U NO-UNDO.
DEFINE VARIABLE end_whs       AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz" NO-UNDO.
DEFINE VARIABLE begin_userid  AS CHARACTER FORMAT "X(8)":U NO-UNDO.
DEFINE VARIABLE end_userid    AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz" NO-UNDO.
DEFINE VARIABLE ldt-from      AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE ldt-to        AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 LABEL "To Date" NO-UNDO.
DEFINE VARIABLE v-post-date   AS DATE      FORMAT "99/99/9999":U LABEL "Post Date" NO-UNDO.
DEFINE VARIABLE tb_excel      AS LOGICAL   NO-UNDO.
/* List of Rita codes: */
DEFINE VARIABLE v-postlst     AS cha       NO-UNDO.

/* Local Variable Definitions ---                                       */

{methods/defines/hndldefs.i NEW}
/*{methods/prgsecur.i} */
{methods/defines/globdefs.i &NEW=NEW GLOBAL}
{custom/gcompany.i}
{custom/gloc.i}
{sys/inc/var.i new shared}
def  stream sDebug.  
output stream sDebug to c:\temp\debug.txt.  

if g_loc = "" then   
g_loc = "main".
g_sysdate   = TODAY.
RUN pDefaultAsiValues.
ASSIGN
    g_company = ipcCompany    
    gcompany  = ipcCompany
    gLoc      = g_loc
    cocode    = gcompany
    locode    = gloc
    .
    
/* assign local vars from input parameters */
ASSIGN
    begin_userid = ipcUserID
    end_userid   = ipcUserID
    v-postlst    = ip-run-what
    v-post-date  = ip-post-eom-date  
    .

DEFINE            VARIABLE list-name      AS cha       NO-UNDO.
DEFINE            VARIABLE init-dir       AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE choice         AS LOG       NO-UNDO.

DEFINE            VARIABLE v-fgpostgl     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-fg-value     AS DECIMAL   FORMAT "->,>>>,>>9.99".
DEFINE            VARIABLE v-msf          AS DECIMAL   FORMAT ">,>>9.999" EXTENT 6.
DEFINE            VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE            VARIABLE ls-fax-file    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE fg-uom-list    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-list-name   LIKE list-name EXTENT 2 NO-UNDO.
DEFINE            VARIABLE t-setup        AS LOG       NO-UNDO.
DEFINE            VARIABLE lInvFrt        AS LOG       NO-UNDO.
DEFINE            VARIABLE dBillAmt       AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE lEmailBol      AS LOG       NO-UNDO.
DEFINE            VARIABLE ll             AS LOG       NO-UNDO.

DEFINE            VARIABLE gv-fgemail     AS LOGICAL   NO-UNDO INIT ?.

DEFINE TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id      AS ROWID
    FIELD has-rec     AS LOG       INIT NO
    FIELD invoiced    AS LOG       INIT NO
    FIELD old-tag     AS CHARACTER
    FIELD ret-loc     AS CHARACTER
    FIELD ret-loc-bin AS CHARACTER.

DEFINE TEMP-TABLE tt-email NO-UNDO 
    FIELD tt-recid AS RECID
    FIELD job-no   LIKE job-hdr.job-no
    FIELD job-no2  LIKE job-hdr.job-no2
    FIELD i-no     LIKE itemfg.i-no
    FIELD qty      AS INTEGER
    FIELD cust-no  AS cha
    INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEFINE TEMP-TABLE tt-fgemail NO-UNDO
    FIELD i-no      LIKE itemfg.i-no
    FIELD po-no     LIKE oe-ordl.po-no
    FIELD ord-no    LIKE oe-ordl.ord-no
    FIELD qty-rec   AS DECIMAL
    FIELD recipient AS CHARACTER.

DEFINE TEMP-TABLE tt-posted-items
    FIELD i-no LIKE w-fg-rctd.i-no
    INDEX i-no i-no.

DEFINE TEMP-TABLE tt-set
    FIELD part-no LIKE fg-set.part-no
    INDEX i1 part-no.

{fg/fullset.i NEW}

{jc/jcgl-sh.i NEW}

{fg/fg-post3.i NEW}

{fg/invrecpt.i NEW}

{sys/ref/fgoecost.i}
DEFINE TEMP-TABLE tt-inv LIKE w-inv.
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

DEFINE STREAM st-email.
DEFINE STREAM logFile.
DEFINE STREAM before.
DEFINE STREAM after.

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.

DEFINE BUFFER b-fg-rctd  FOR fg-rctd.
DEFINE BUFFER b2-fg-rctd FOR fg-rctd.

{sys/inc/ssfgretc.i}

DO TRANSACTION:
    {sys/inc/fgpost.i}   
END.

{oerep/r-loadtg.i NEW}  /*w-ord for loadtag reprint */

DEFINE VARIABLE lvReturnChar    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lvFound         AS LOG       NO-UNDO.
DEFINE VARIABLE autofgissue-log AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "AUTOFGISSUE", "L", NO, NO, "", "", 
    OUTPUT lvReturnChar, OUTPUT lvFound).
IF lvFound THEN
    autofgissue-log = LOGICAL(lvReturnChar).

DEFINE VARIABLE v-loadtag  AS cha       INIT "ASI" NO-UNDO.
DEFINE VARIABLE v-mult     AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-cas-lab  AS LOG       NO-UNDO.
DEFINE VARIABLE v-tags     AS DECIMAL   NO-UNDO.
DEFINE VARIABLE form_fid   AS CHARACTER NO-UNDO INITIAL "barcode.frm" FORMAT "X(40)".
DEFINE VARIABLE form#      AS INTEGER   NO-UNDO FORMAT "9" INITIAL 3.
DEFINE VARIABLE n          AS INTEGER   NO-UNDO INITIAL 0.
DEFINE VARIABLE char_units AS CHARACTER NO-UNDO.
DEFINE VARIABLE copy_count AS INTEGER   NO-UNDO INITIAL 2.
DEFINE VARIABLE v-out      AS CHARACTER FORMAT "x(40)" NO-UNDO.

DEFINE STREAM s-form.
DEFINE STREAM s-bar.
DEFINE VARIABLE v-po-no-source AS CHARACTER FORMAT "!" INIT "R".

DEFINE VARIABLE stx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~002".
DEFINE VARIABLE etx            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~003".
DEFINE VARIABLE esc            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~033".
DEFINE VARIABLE etb            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~027".
DEFINE VARIABLE cr             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~015".
DEFINE VARIABLE can            AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~030".
DEFINE VARIABLE rs             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~036".
DEFINE VARIABLE us             AS CHARACTER FORMAT 'x(01)' NO-UNDO INITIAL "~037".
DEFINE VARIABLE tb_16ths       AS LOG       NO-UNDO.
DEFINE BUFFER ref-lot-no FOR reftable.
DEFINE VARIABLE SSLoadTag-log AS LOGICAL NO-UNDO.

RUN sys/ref/nk1look.p (cocode, "SSLoadTag", "L", NO, NO, "", "", 
    OUTPUT lvReturnChar, OUTPUT lvFound).
IF lvFound THEN
    SSLoadTag-log = LOGICAL(lvReturnChar).
FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ gcompany
    AND sys-ctrl.name    EQ "CEMENU"
    NO-LOCK NO-ERROR.
ASSIGN
    tb_16ths = AVAILABLE sys-ctrl AND sys-ctrl.char-fld EQ "Corrware"
    .
DEFINE VARIABLE v-uid-sec       AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-close  AS LOG       NO-UNDO.
DEFINE VARIABLE v-access-list   AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-source-handle AS HANDLE    NO-UNDO.

/* Check if authorized to create PO's */
RUN methods/prgsecur.p
    (INPUT "FGPostUID",
    INPUT "ALL", /* based on run, create, update, delete or all */
    INPUT NO,    /* use the directory in addition to the program */
    INPUT NO,    /* Show a message if not authorized */
    INPUT NO,    /* Group overrides user security? */
    OUTPUT v-uid-sec, /* Allowed? Yes/NO */
    OUTPUT v-access-close, /* used in template/windows.i  */
    OUTPUT v-access-list). /* list 1's and 0's indicating yes or no to run, create, update, delete */


/* ************************  Function Prototypes ********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
    (  )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD get-tot-rcv-qty C-Win 
FUNCTION get-tot-rcv-qty RETURNS INTEGER
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ )  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION-FORWARD removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER)  FORWARD.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* ***********************  Control Definitions  ********************** */
/* Definitions of the field level widgets                               */
DEFINE BUTTON Btn_Cancel AUTO-END-KEY 
    LABEL "Ca&ncel" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .

DEFINE BUTTON Btn_OK AUTO-GO 
    LABEL "&OK" 
    SIZE 15 BY 1.14
    BGCOLOR 8 .


DEFINE VARIABLE lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 LABEL "Lines Per Page" NO-UNDO.
DEFINE VARIABLE lv-font-name   AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" LABEL "Font" NO-UNDO.

DEFINE VARIABLE v-trans-lbl    AS CHARACTER FORMAT "X(256)":U INITIAL "Transaction Types" NO-UNDO.
DEFINE VARIABLE rd-Itm#Cst#    AS INTEGER   NO-UNDO.
DEFINE VARIABLE rd-ItmPo       AS INTEGER   NO-UNDO.
DEFINE VARIABLE rd-UOMJob      AS INTEGER   NO-UNDO.
DEFINE VARIABLE rd_print       AS CHARACTER NO-UNDO.
DEFINE VARIABLE t-adj          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE t-receipt      AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE t-ret          AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE t-ship         AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE t-trans        AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_glnum       AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_grndtotal   AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_totCstVal   AS LOGICAL   INITIAL NO NO-UNDO. 
DEFINE VARIABLE tg-recalc-cost AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE tgIssue        AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE tgl-itemCD     AS LOGICAL   INITIAL NO NO-UNDO.
DEFINE VARIABLE fi_file        AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgpstall.csv" LABEL "If Yes, File Name" .

/* *********************** Procedure Settings ************************ */


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _CUSTOM _MAIN-BLOCK C-Win 



IF ip-run-what EQ "" THEN 
DO:
PROCEDURE mail EXTERNAL "xpMail.dll" :
    DEFINE INPUT PARAMETER mailTo AS CHARACTER.
    DEFINE INPUT PARAMETER mailsubject AS CHARACTER.
    DEFINE INPUT PARAMETER mailText AS CHARACTER.
    DEFINE INPUT PARAMETER mailFiles AS CHARACTER.
    DEFINE INPUT PARAMETER mailDialog AS LONG.
    DEFINE OUTPUT PARAMETER retCode AS LONG.
END.
END. /* if ip-run-what eq "" */


MAIN-BLOCK:
DO ON ERROR   UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK
    ON END-KEY UNDO MAIN-BLOCK, LEAVE MAIN-BLOCK:
  

    DO TRANSACTION:
        {sys/inc/closejob.i FGPost}
        {sys/inc/fgpostgl.i}   
   
        {sys/inc/fgemails.i}
        {sys/inc/postdate.i}
    END.
    {sys/inc/adjustgl.i}  
	put stream sdebug "Main" cocode begin_userid end_userid  ip-run-what skip.
    ASSIGN
        v-fgpostgl = fgpostgl
        tb_glnum   = v-fgpostgl NE "None" OR v-adjustgl
        tgl-itemCD = YES   
        .

    IF ip-run-what EQ "" THEN 
    DO :

        ASSIGN
            v-fgpostgl = fgpostgl
            tb_glnum   = v-fgpostgl NE "None" OR v-adjustgl
            .

        IF NOT LOGICAL(tb_glnum) THEN DISABLE tb_glnum.
 

        IF postdate-log THEN
        DO:
            v-post-date = TODAY.
        END.
        ELSE
        DO:
            v-post-date = ?.
        END.
 
        RUN init-values.

        FIND FIRST period
            WHERE period.company EQ cocode
            AND period.pst     LE v-post-date
            AND period.pend    GE v-post-date
            NO-LOCK NO-ERROR.
        IF NOT AVAILABLE period THEN 
            RETURN NO-APPLY.

        RUN print-and-post.
    END.
    ELSE 
    DO:
        /*FIND fg-rctd NO-LOCK WHERE RECID(fg-rctd) EQ INT(ip-run-what) NO-ERROR.
            
        IF AVAIL fg-rctd THEN
          ASSIGN
           ip-rowid    = ROWID(fg-rctd)
           ip-run-what = fg-rctd.rita-code.
            
        ELSE ip-rowid = ?.*/

        ASSIGN
            v-post-date   = TODAY
            t-receipt     = ip-run-what EQ "R"
            t-ship        = ip-run-what EQ "S"
            t-trans       = ip-run-what EQ "T"
            t-adj         = NO
            t-ret         = NO
            t-setup       = ip-run-what EQ "SETUP"
            begin_fg-r-no = 0
            end_fg-r-no   = 2147483647
            begin_i-no    = ""
            end_i-no      = "zzzzzzzzzzzzzzzzzzzzzzzz"
            ldt-from      = 01/01/0001
            ldt-to        = 12/31/9999
            begin_job-no  = ""
            end_job-no    = "zzzzzzzzzzzzzzzzzzzzzzzz"
            .

        RUN print-and-post.


    END.
    output stream sdebug close.
END.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


/* **********************  Internal Procedures  *********************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-rel-assign-logic C-Win 
PROCEDURE add-rel-assign-logic :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipi-qty     AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-rel-recid   AS RECID NO-UNDO.
    DEFINE VARIABLE adm-new-record AS LOG   NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-oe-rel  FOR oe-rel.

    /* custom code */
    DEFINE BUFFER s-code     FOR reftable.
    DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.
    FIND bf-oe-rel WHERE ROWID(bf-oe-rel) EQ ipr-rel-row
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-oe-rel THEN
        RETURN.
    lv-rel-recid = RECID(bf-oe-rel).

    /* Local assign code from oe/b-ordrel.w, local-assign */
    DEFINE VARIABLE ll-ans   AS LOG  NO-UNDO.
    DEFINE VARIABLE ldt-ship AS DATE FORM "99/99/9999" NO-UNDO.
    DEFINE BUFFER bf-rel FOR oe-rel .
    DEFINE VARIABLE ld-prev-rel-qty AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty-sum       AS INTEGER NO-UNDO.
    DEFINE VARIABLE ls-key-02       LIKE tt-report.key-02 NO-UNDO.

    DEFINE BUFFER b-ordl FOR oe-ordl.
  

    FIND FIRST bf-oe-ordl WHERE bf-oe-ordl.company EQ bf-oe-rel.company
        AND bf-oe-ordl.ord-no  EQ bf-oe-rel.ord-no
        AND bf-oe-ordl.LINE    EQ bf-oe-rel.LINE
        NO-LOCK NO-ERROR.
    /* Code placed here will execute PRIOR to standard behavior. */
    IF NOT AVAILABLE bf-oe-rel AND lv-rel-recid <> ? THEN
        FIND bf-oe-rel WHERE RECID(bf-oe-rel) = lv-rel-recid.
    ld-prev-rel-qty = IF adm-new-record THEN 0 ELSE bf-oe-rel.qty.
  
    FIND bf-oe-ord OF bf-oe-ordl NO-LOCK.



    FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(bf-oe-ordl).
    b-ordl.t-rel-qty = b-ordl.t-rel-qty + bf-oe-rel.qty - ld-prev-rel-qty.
    FIND b-ordl WHERE ROWID(b-ordl) EQ ROWID(bf-oe-ordl) NO-LOCK.
    RUN fg/fgitmloc.p (INPUT bf-oe-rel.i-no, INPUT ROWID(bf-oe-rel)).

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE add-rel-for-qty C-Win 
PROCEDURE add-rel-for-qty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipi-qty     AS INTEGER   NO-UNDO.


    DEFINE BUFFER bf-oe-ordl     FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord      FOR oe-ord.
    DEFINE BUFFER bf-orig-oe-rel FOR oe-rel.

    /* Code added to implement the procedure below */
    DEFINE VARIABLE v-last-shipto AS CHARACTER.
    DEFINE VARIABLE lv-rel-recid  AS RECID.
    DEFINE BUFFER s-code FOR reftable.
    DEFINE VARIABLE lv-cust-x    AS CHARACTER.

    DEFINE VARIABLE oereleas-log LIKE sys-ctrl.log-fld NO-UNDO.
    DEFINE VARIABLE oereleas-cha LIKE sys-ctrl.char-fld NO-UNDO.


    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "OERELEAS"
        NO-LOCK NO-ERROR.
    IF AVAILABLE sys-ctrl THEN
        ASSIGN
            oereleas-log = sys-ctrl.log-fld
            oereleas-cha = sys-ctrl.char-fld
            .


    /* custom code */
    FIND bf-orig-oe-rel WHERE ROWID(bf-orig-oe-rel) EQ ipr-rel-row
        NO-LOCK NO-ERROR.

    IF NOT AVAILABLE bf-orig-oe-rel THEN
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
    IF NOT AVAILABLE bf-oe-ord THEN
        RETURN.


    /* Code from oe/b-ordrel.w, local-create */
    DEFINE VARIABLE v-qty-sum    AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-nxt-r-no   AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-lst-rel    AS DATE    INIT TODAY NO-UNDO.
    DEFINE VARIABLE v-pct-chg    AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-ship-id    LIKE oe-rel.ship-id NO-UNDO.
    DEFINE VARIABLE v-carrier    LIKE oe-rel.carrier NO-UNDO.
    DEFINE VARIABLE v-num-shipto AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-qty-mod    AS LOG     NO-UNDO.
    DEFINE BUFFER bf-rel  FOR oe-rel.
    DEFINE BUFFER bf-cust FOR cust.
    DEFINE VARIABLE v-first-ship-id AS cha NO-UNDO.
  

    RUN oe/get-r-no.p (INPUT "oe-rel", OUTPUT v-nxt-r-no).
    FIND FIRST bf-cust WHERE bf-cust.cust-no EQ bf-oe-ord.cust-no NO-LOCK NO-ERROR.
    ASSIGN
        v-ship-id = IF AVAILABLE oe-rel THEN oe-rel.ship-id ELSE ""
        v-carrier = IF AVAILABLE oe-rel THEN oe-rel.carrier ELSE ""
        .
    IF AVAIL(bf-cust) AND bf-cust.ACTIVE = "X" AND v-last-shipto GT "" THEN
        v-ship-id = v-last-shipto.
    FIND FIRST bf-rel WHERE bf-rel.company = bf-oe-ord.company
        AND bf-rel.ord-no = bf-oe-ord.ord-no
        AND bf-rel.i-no = bf-oe-ordl.i-no 
        AND bf-rel.LINE = bf-oe-ordl.LINE
        NO-LOCK NO-ERROR.
    v-first-ship-id = IF AVAILABLE bf-rel THEN bf-rel.ship-id ELSE "".


    lv-rel-recid = RECID(oe-rel).
    ASSIGN 
        v-qty-sum = 0
        .

    CREATE oe-rel.

    IF AVAILABLE bf-oe-ordl THEN 
    DO:

        FIND FIRST bf-oe-ord  OF bf-oe-ordl NO-LOCK.
        FOR EACH bf-rel NO-LOCK WHERE bf-rel.company = bf-oe-ord.company
            AND bf-rel.ord-no = bf-oe-ord.ord-no
            AND bf-rel.i-no = bf-oe-ordl.i-no 
            AND bf-rel.LINE = bf-oe-ordl.LINE
            :
            FIND FIRST s-code NO-LOCK
                WHERE s-code.reftable EQ "oe-rel.s-code"
                AND s-code.company  EQ STRING(bf-rel.r-no,"9999999999")
                NO-ERROR.
            IF NOT AVAILABLE s-code OR CAN-DO("B,S",s-code.code) THEN
                v-qty-sum = v-qty-sum + bf-rel.qty. 
        END.
     
        
        FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ cocode
            AND sys-ctrl.name    EQ "OECARIER"
            NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN 
        DO:
            CREATE sys-ctrl.
            ASSIGN 
                sys-ctrl.company  = cocode
                sys-ctrl.name     = "OECARIER"
                sys-ctrl.descrip  = "Default carrier from Header or ShipTo:"
                sys-ctrl.char-fld = "ShipTo"
                .       
            DO WHILE TRUE:
                sys-ctrl.char-fld = "Shipto".
                IF sys-ctrl.char-fld = "Header" OR sys-ctrl.char-fld = "ShipTo" THEN LEAVE. 
            END.
        END.

        RELEASE shipto.

        IF v-carrier = "" THEN 
        DO:  /* NK1 OECARIER */
            IF sys-ctrl.char-fld EQ "ShipTo" THEN 
            DO:
                FIND FIRST shipto NO-LOCK
                    WHERE shipto.company EQ bf-oe-ord.company
                    AND shipto.cust-no EQ bf-oe-ord.cust-no
                    AND shipto.ship-id EQ v-ship-id NO-ERROR.
                v-carrier = IF AVAILABLE shipto THEN shipto.carrier ELSE "".
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

        IF v-carrier EQ "" AND AVAILABLE shipto THEN v-carrier = shipto.carrier.
     
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
            oe-rel.r-no         = v-nxt-r-no
            .

    
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
            .

        FIND FIRST ref-lot-no WHERE 
            ref-lot-no.reftable EQ "oe-rel.lot-no" AND
            ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
            NO-ERROR.
  
        IF AVAILABLE ref-lot-no THEN
            ASSIGN
                oe-rel.lot-no = ref-lot-no.CODE
                .
    
                       
        IF oe-rel.qty LT 0 THEN oe-rel.qty = 0.

        oe-rel.tot-qty = oe-rel.qty.

        IF oe-rel.rel-date LE v-lst-rel THEN oe-rel.rel-date = v-lst-rel + 1.

        RUN add-rel-assign-logic (INPUT ipr-rel-row, INPUT ipi-qty).

    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-comp-tables C-Win 
PROCEDURE build-comp-tables :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-fg-rctd   FOR fg-rctd.
    DEFINE BUFFER bf-w-fg-rctd FOR w-fg-rctd.

    /* Make sure all components are included in w-fg-rctd */
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "R":

        /* Check using fg-rcpts, then reftable since either could be used */
        FOR EACH fg-rcpts NO-LOCK 
            WHERE fg-rcpts.company EQ w-fg-rctd.company 
            AND fg-rcpts.linker EQ "fg-rctd: " + STRING(w-fg-rctd.r-no,"9999999999") 
            .
            FIND FIRST fg-set  NO-LOCK WHERE fg-set.part-no = fg-rcpts.i-no 
                AND fg-set.set-no EQ w-fg-rctd.i-no
                AND fg-set.company = w-fg-rctd.company
                NO-ERROR.
            FIND fg-rctd  NO-LOCK WHERE fg-rctd.r-no EQ fg-rcpts.r-no
                NO-ERROR.
            IF AVAILABLE fg-rctd /* AND AVAIL fg-set */ THEN 
            DO:
                FIND FIRST bf-w-fg-rctd  NO-LOCK WHERE bf-w-fg-rctd.row-id EQ ROWID(fg-rctd)
                    NO-ERROR.
                IF NOT AVAILABLE bf-w-fg-rctd AND fg-rctd.rita-code EQ w-fg-rctd.rita-code THEN
                    RUN build-tables.
            END.
        END.
    END.
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "R":
        /* Checking a second time using reftable */
        FOR EACH fg-set NO-LOCK 
            WHERE fg-set.company EQ w-fg-rctd.company
            AND fg-set.set-no  EQ w-fg-rctd.i-no
            :
    
            FIND FIRST reftable NO-LOCK
                WHERE reftable.reftable EQ "fg-rctd.user-id"
                AND reftable.company  EQ w-fg-rctd.company
                AND reftable.loc      EQ STRING(w-fg-rctd.r-no,"9999999999")
                NO-ERROR.
    
            IF AVAILABLE reftable THEN 
            DO:
            
                FOR EACH bf-fg-rctd NO-LOCK 
                    WHERE bf-fg-rctd.company EQ w-fg-rctd.company
                    AND bf-fg-rctd.i-no EQ fg-set.part-no
                    AND bf-fg-rctd.rita-code EQ "R" 
                    :
                                  
                    FOR EACH reftable  NO-LOCK
                        WHERE reftable.reftable EQ "fg-rctd.user-id"
                        AND reftable.company  EQ bf-fg-rctd.company
                        AND reftable.loc      EQ STRING(bf-fg-rctd.r-no,"9999999999")        
                        AND (reftable.dscr EQ "fg-rctd: " + STRING(w-fg-rctd.r-no, "9999999999") AND reftable.dscr BEGINS "fg-rctd: ")  
                        USE-INDEX loc   .
    
                        FIND fg-rctd WHERE ROWID(fg-rctd) EQ ROWID(bf-fg-rctd)
                            NO-LOCK NO-ERROR.
                        IF AVAILABLE fg-rctd AND fg-rctd.rita-code NE "P" THEN 
                        DO:
                            FIND FIRST bf-w-fg-rctd  NO-LOCK WHERE bf-w-fg-rctd.row-id EQ ROWID(fg-rctd)
                                NO-ERROR.
                            IF NOT AVAILABLE bf-w-fg-rctd AND fg-rctd.rita-code EQ w-fg-rctd.rita-code THEN
                                RUN build-tables.
                        END.
                      
                    END. /* each reftable */
                END. /* each bf-fg-rctd */
            END. /* avail reftable for header item */
        END. /* each fg-set */

    END. /* each w-fg-rctd, check for set components */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE build-tables C-Win 
PROCEDURE build-tables :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/  
    DEFINE VARIABLE li-max-qty AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-part-qty AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-set-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-cost     AS DECIMAL NO-UNDO.

    DEFINE BUFFER b-fg-rctd FOR fg-rctd.
    DEFINE BUFFER b-itemfg  FOR itemfg.
    DEFINE BUFFER use-job   FOR reftable.

    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
        NO-ERROR.
    FIND FIRST loc  NO-LOCK WHERE loc.company EQ itemfg.company
        AND loc.loc = itemfg.loc
        NO-ERROR.
    IF AVAILABLE itemfg /*AND AVAIL loc*/ THEN 
    DO TRANSACTION:
        li-max-qty = fg-rctd.t-qty.
        put stream sdebug "i-no" fg-rctd.i-no "tag" fg-rctd.tag "t-qty " fg-rctd.t-qty  " li-max-qty " li-max-qty skip.
        IF li-max-qty GE fg-rctd.t-qty THEN 
        DO:
            CREATE w-fg-rctd.
            BUFFER-COPY fg-rctd TO w-fg-rctd
                ASSIGN
                w-fg-rctd.row-id  = ROWID(fg-rctd)
                w-fg-rctd.has-rec = YES
                .

            IF ip-run-what EQ "SETUP" THEN
                ASSIGN
                    w-fg-rctd.old-tag     = fg-rctd.tag
                    w-fg-rctd.ret-loc     = fg-rctd.loc
                    w-fg-rctd.ret-loc-bin = fg-rctd.loc-bin
                    .
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-partial C-Win 
PROCEDURE calc-partial :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-ERROR.
        
    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.sell-uom
            AND uom.mult NE 0
            NO-LOCK NO-ERROR.
          
        IF itemfg.sell-uom BEGINS "L" THEN
            v-fg-value = 0.

        ELSE
            IF itemfg.sell-uom EQ "CS" THEN
                v-fg-value = 0.

            ELSE
                IF AVAILABLE uom THEN
                    v-fg-value = itemfg.sell-price * w-fg-rctd.partial / uom.mult.

                ELSE
                    v-fg-value = itemfg.sell-price * w-fg-rctd.partial / 1000.

        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT w-fg-rctd.partial * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.partial * itemfg.t-sqft)).

            v-msf[1] = w-fg-rctd.partial * itemfg.t-sqft.
        END.
    END. /* avail */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE calc-total C-Win 
PROCEDURE calc-total :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    
    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg NO-LOCK
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-ERROR.
        
    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.sell-uom
            AND uom.mult NE 0
            NO-LOCK NO-ERROR.

        IF itemfg.sell-uom BEGINS "L" THEN
            v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

        ELSE
            IF itemfg.sell-uom EQ "CS" THEN
                v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

            ELSE
                IF AVAILABLE uom THEN
                    v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / uom.mult).

                ELSE
                    v-fg-value = itemfg.sell-price * ((w-fg-rctd.cases * w-fg-rctd.qty-case) / 1000).

        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT w-fg-rctd.t-qty * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] + (v-msf[1] - ((w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft)).

            v-msf[1] = (w-fg-rctd.cases * w-fg-rctd.qty-case) * itemfg.t-sqft.
        END.
    END. /* avail itemfg */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Check-Fgemail-Parm C-Win 
PROCEDURE Check-Fgemail-Parm :
    /*------------------------------------------------------------------------------
      Purpose:    Get FGEMAILS option and create if it does not exist. 
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER buf-sys-ctrl FOR sys-ctrl.
 
    /* Find the FGEMAILS control record. */
    FIND FIRST buf-sys-ctrl NO-LOCK
        WHERE buf-sys-ctrl.company EQ g_company
        AND buf-sys-ctrl.name EQ 'FGEMAILS' 
        NO-ERROR.

    /* If not found, prompt user whether to send these emails or not. */
    IF NOT AVAILABLE buf-sys-ctrl THEN 
    DO:
        DEFINE VARIABLE l-fgemail AS LOG NO-UNDO. 
        l-fgemail = NO.
        /* Create the record. */
        CREATE buf-sys-ctrl.
        ASSIGN 
            buf-sys-ctrl.company = g_company
            buf-sys-ctrl.name    = 'FGEMAILS'
            buf-sys-ctrl.descrip = 'Customer Service Emails for Out of Stock Inventory'
            buf-sys-ctrl.int-fld = IF l-fgemail = YES THEN 1 ELSE 0
            .
    END.

    /* If found (or created), save the email option for later. */
    IF AVAILABLE buf-sys-ctrl THEN
        ASSIGN gv-fgemail = (IF buf-sys-ctrl.int-fld = 1 THEN YES ELSE NO)
            .

    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-loadtag C-Win 
PROCEDURE create-loadtag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT-OUTPUT PARAMETER io-tag-no AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-total-unit LIKE w-ord.total-unit NO-UNDO.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-phy-count-proc C-Win 
PROCEDURE create-phy-count-proc :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rno AS INTEGER NO-UNDO.

    DEFINE BUFFER b-fg-bin FOR fg-bin.

    CREATE b2-fg-rctd.

    FIND LAST b-fg-rctd  NO-LOCK USE-INDEX fg-rctd NO-ERROR.
    IF AVAILABLE b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN
        lv-rno = b-fg-rctd.r-no.

    FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN
        lv-rno = fg-rcpth.r-no.

    DO WHILE TRUE:
        lv-rno = lv-rno + 1.
        IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
            CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
            NEXT.
        LEAVE.
    END.

    /*task 06101005*/
    IF w-fg-rctd.rita-code EQ "I" THEN
        ASSIGN
            w-fg-rctd.job-no   = ""
            w-fg-rctd.job-no2  = 0
            w-fg-rctd.cost     = 0
            w-fg-rctd.std-cost = 0
            w-fg-rctd.ext-cost = 0
            .

    ASSIGN
        b2-fg-rctd.company    = cocode
        b2-fg-rctd.r-no       = lv-rno
        b2-fg-rctd.rita-code  = "C"
        b2-fg-rctd.s-num      = 0
        b2-fg-rctd.rct-date   = TODAY
        b2-fg-rctd.trans-time = TIME 
        b2-fg-rctd.tag        = w-fg-rctd.old-tag
        b2-fg-rctd.loc        = w-fg-rctd.ret-loc
        b2-fg-rctd.loc-bin    = w-fg-rctd.ret-loc-bin
        b2-fg-rctd.i-no       = w-fg-rctd.i-no
        b2-fg-rctd.i-name     = w-fg-rctd.i-name
        b2-fg-rctd.job-no     = w-fg-rctd.job-no
        b2-fg-rctd.job-no2    = w-fg-rctd.job-no2
        b2-fg-rctd.t-qty      = w-fg-rctd.inv-no
        b2-fg-rctd.cases      = w-fg-rctd.cases
        b2-fg-rctd.cases-unit = w-fg-rctd.cases-unit
        b2-fg-rctd.qty-case   = w-fg-rctd.qty-case
        b2-fg-rctd.std-cost   = w-fg-rctd.std-cost
        b2-fg-rctd.cust-no    = w-fg-rctd.cust-no
        b2-fg-rctd.cost       = w-fg-rctd.cost
        b2-fg-rctd.cost-uom   = w-fg-rctd.cost-uom
        b2-fg-rctd.ext-cost   = w-fg-rctd.ext-cost
        b2-fg-rctd.tot-wt     = w-fg-rctd.tot-wt 
        .

    IF b2-fg-rctd.t-qty NE w-fg-rctd.t-qty AND
        b2-fg-rctd.qty-case NE 0 THEN
        ASSIGN
            b2-fg-rctd.cases   = TRUNC(b2-fg-rctd.t-qty / b2-fg-rctd.qty-case,0)
            b2-fg-rctd.partial = b2-fg-rctd.t-qty - (b2-fg-rctd.cases * b2-fg-rctd.qty-case)
            .

    FIND FIRST b-fg-bin  NO-LOCK
        WHERE b-fg-bin.company EQ b2-fg-rctd.company
        AND b-fg-bin.i-no    EQ b2-fg-rctd.i-no
        AND b-fg-bin.job-no  EQ b2-fg-rctd.job-no
        AND b-fg-bin.job-no2 EQ b2-fg-rctd.job-no2
        AND b-fg-bin.loc     EQ b2-fg-rctd.loc
        AND b-fg-bin.loc-bin EQ b2-fg-rctd.loc-bin
        AND b-fg-bin.tag     EQ b2-fg-rctd.tag
        AND b-fg-bin.cust-no EQ b2-fg-rctd.cust-no
        NO-ERROR.

    IF AVAILABLE b-fg-bin THEN
        ASSIGN
            b2-fg-rctd.ext-cost = b2-fg-rctd.t-qty /
                           (IF b-fg-bin.pur-uom EQ "M" THEN 1000 ELSE 1) *
                           b-fg-bin.std-tot-cost
            b2-fg-rctd.cost     = b2-fg-rctd.ext-cost / b2-fg-rctd.t-qty
            b2-fg-rctd.cost-uom = b-fg-bin.pur-uom
            .
  
    IF b2-fg-rctd.ext-cost EQ ? THEN b2-fg-rctd.ext-cost = 0.
    IF b2-fg-rctd.cost     EQ ? THEN b2-fg-rctd.cost = 0.

    FIND FIRST loadtag NO-LOCK WHERE
        loadtag.company = g_company AND
        loadtag.item-type = NO AND
        loadtag.tag-no = b2-fg-rctd.tag
        NO-ERROR.
        
    IF AVAILABLE loadtag  AND
        CAN-FIND(FIRST fg-bin WHERE
        fg-bin.company EQ cocode AND
        fg-bin.i-no    EQ b2-fg-rctd.i-no AND
        fg-bin.tag     EQ b2-fg-rctd.tag AND
        fg-bin.job-no  EQ b2-fg-rctd.job-no AND
        fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
        (fg-bin.loc    NE b2-fg-rctd.loc OR
        fg-bin.loc-bin NE b2-fg-rctd.loc-bin)
        USE-INDEX tag) AND
        (loadtag.loc <> b2-fg-rctd.loc OR 
        loadtag.loc-bin <> b2-fg-rctd.loc-bin) THEN 
        RUN crt-transfer.

    RELEASE b2-fg-rctd.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-text-file C-Win 
PROCEDURE create-text-file :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE i                AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li               AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-text          AS cha       NO-UNDO.
    DEFINE VARIABLE v-dept-note      AS cha       FORM "x(80)" EXTENT 18 NO-UNDO.
    DEFINE VARIABLE lv-middlesex-job AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-middlesex-po  AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE lv-tag-no        AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-how-many-tags AS INTEGER   NO-UNDO.

    /* gdm - 10160905*/
    DEFINE VARIABLE v-fgdsc1         LIKE itemfg.part-dscr1 NO-UNDO.
    DEFINE VARIABLE v-fgdsc2         LIKE itemfg.part-dscr2 NO-UNDO.
    DEFINE VARIABLE v-fgdsc3         LIKE itemfg.part-dscr3 NO-UNDO.
    DEFINE VARIABLE cRFIDTag         AS cha       NO-UNDO.
    DEFINE VARIABLE v-job            AS CHARACTER FORMAT "x(9)" NO-UNDO.
    DEFINE VARIABLE v-count          AS INTEGER   NO-UNDO.

    FIND FIRST w-ord NO-ERROR.

    FIND FIRST sys-ctrl
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "LOADTAG"
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company  = gcompany
            sys-ctrl.name     = "LOADTAG"
            sys-ctrl.descrip  = "Special Load tag print options, e.g. barcode printer"
            sys-ctrl.char-fld = "ASI"
            .

        FIND CURRENT sys-ctrl NO-LOCK.
    END.

    ASSIGN 
        v-loadtag = sys-ctrl.char-fld
        v-mult    = sys-ctrl.int-fld
        v-cas-lab = sys-ctrl.log-fld
        v-tags    = sys-ctrl.dec-fld
        .

    IF v-loadtag = "TRIAD" THEN 
    DO:
        IF form_fid > "" THEN 
        DO:   /* download the form file into the printer ~*/
            INPUT stream s-form from value(form_fid) no-echo.
            _form: DO WHILE TRUE:
                READKEY STREAM s-form.
                IF LASTKEY < 0 THEN LEAVE _form.
                PUT STREAM s-bar CONTROL CHR(LASTKEY).
            END.
            INPUT stream s-form close.
        END.

        FOR EACH w-ord:
            v-job = w-ord.job-no + "-" + string(w-ord.job-no2,"99").
            IF v-job BEGINS "-" OR v-job = ? /* 9901 CAH */
                THEN v-job = STRING(W-ORD.ORD-NO).   /* 9812 CAH in case blank */
            FIND FIRST itemfg WHERE itemfg.company = cocode
                AND itemfg.i-no = w-ord.i-no NO-LOCK NO-ERROR.
            IF w-ord.total-tags GT -1 THEN 
            DO:
                DO i = 1 TO (w-ord.total-tags + 1):
                    /* select the form */
                    PUT STREAM s-bar CONTROL stx esc "E" STRING(form#) ",1" can etx.
                    char_units = (IF i <= w-ord.total-tags 
                        THEN STRING(w-ord.total-unit) ELSE "    ").  
                    DEFINE VARIABLE char_date AS CHARACTER FORMAT 'x(10)' NO-UNDO.
                    char_date = STRING(TODAY,"99/99/9999").
                    /* 9901 CAH: Only room for 19 chars in the standard 48 pt font */
                    IF LENGTH(w-ord.ship-name) > 19
                        THEN w-ord.ship-name = SUBSTRING(w-ord.ship-name,1,19).
            
                    DEFINE VARIABLE vcFGItem AS CHARACTER NO-UNDO.
                    vcFGItem = 
                        IF AVAILABLE itemfg THEN itemfg.i-no ELSE w-ord.i-no.
                    DO n = copy_count TO 1 BY -1:
                        /* send the variable data to the printer */
                        PUT STREAM s-bar UNFORMATTED
                            stx w-ord.cust-po-no    cr etx
                            stx w-ord.cust-po-no    cr etx
                            stx w-ord.cust-part-no  cr etx
                            stx w-ord.cust-part-no  cr etx
                            stx char_units          cr etx
                            stx char_units          cr etx
                            stx char_date           cr etx
                            stx v-job               cr etx
                            stx w-ord.ord-qty       cr etx /* 9902 CAH was total-unit */
                            stx STRING(i)           cr etx /* 08.20 was n */
                            stx STRING(w-ord.total-tags + 1) cr etx /* 08.20 was copy_count */
                            stx w-ord.ship-name     cr etx
                            stx vcFGItem            cr etx.
                        /* issue the print command */    
                        PUT STREAM s-bar CONTROL     
                            stx rs "1" us "1" etb etx.
                    END.
                END.   /* tag count loop */
            END.  /* non zero */  
        END.    /* each w-ord */
        /*  {sys/inc/close.i "" "stream s-bar"} */
        OUTPUT CLOSE.
    END.    /* TRIAD INTERMEC BARCODE PRINT ROUTINE */
    ELSE
    DO:
        OUTPUT TO VALUE(v-out).
        PUT UNFORMATTED
            "CUSTOMER,ORDNUMBER,JOBNUMBER,ITEM,CUSTPARTNO,CUSTPONO,PCS,BUNDLE,TOTAL," +
            "SHIPCODE,SHIPNAME,SHIPADD1,SHIPADD2,SHIPCITY,SHIPSTATE,SHIPCOUNTRY,SHIPZIP," +
            "SOLDCODE,SOLDNAME,SOLDADD1,SOLDADD2,SOLDCITY,SOLDSTATE,SOLDCOUNTRY,SOLDZIP," +
            "INAME,DUEDATE,RELDATE,UPCNO,LENGTH,WIDTH,DEPTH,FLUTE,TEST,VENDOR,GROSSWGT," +
            "TAREWGT,NETWGT,SHEETWGT,UOM,STYLE,STYLEDESC,RELLOTNO,MIDDLESEXJOBNUMBER,MIDDLESEXCUSTPONO,"
            "TAG#,PARTIAL,CASECODE,SN1,SN2,SN3,SN4,SN5,SN6,SN7,SN8,PONO,DN1,DN2,DN3,DN4,"+
            "DN5,DN6,DN7,DN8,DN9,DN10,EST#,ORDDESC1,ORDDESC2".
        IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN
            PUT UNFORMATTED ",COUNTER#,RFIDTag".

        PUT UNFORMATTED 
            ",DUEDATEJOBLINE,DUEDATEJOB,LINE#,UnitWt,PalletWt,FGdesc1,FGdesc2,FGdesc3,FG Lot#".
      
        PUT SKIP.
        FOR EACH w-ord:

            IF tb_16ths THEN
                ASSIGN
                    w-ord.box-len = ROUND((w-ord.box-len - TRUNC(w-ord.box-len,0)) / 6.25,2) +
                           TRUNC(w-ord.box-len,0)
                    w-ord.box-wid = ROUND((w-ord.box-wid - TRUNC(w-ord.box-wid,0)) / 6.25,2) +
                           TRUNC(w-ord.box-wid,0)
                    w-ord.box-dep = ROUND((w-ord.box-dep - TRUNC(w-ord.box-dep,0)) / 6.25,2) +
                           TRUNC(w-ord.box-dep,0)
                    .

            ASSIGN
                lv-text     = ""
                v-dept-note = ""

                /* gdm - 10160905 */
                v-fgdsc1    = ""
                v-fgdsc2    = ""
                v-fgdsc3    = ""
                .

            FIND FIRST itemfg WHERE itemfg.company EQ cocode
                AND itemfg.i-no    EQ w-ord.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE itemfg THEN 
            DO:        
                ASSIGN 
                    w-ord.net-wt       = itemfg.weight-100 * w-ord.total-unit / 100
                    w-ord.sheet-wt     = itemfg.weight-100 / 100 
                    w-ord.cust-part-no = itemfg.part-no
                    .

                /* gdm - 101610905 */
                ASSIGN 
                    v-fgdsc1 = itemfg.part-dscr1
                    v-fgdsc2 = itemfg.part-dscr2
                    v-fgdsc3 = itemfg.part-dscr3
                    .

            END.  /* avail itemfg */
            ASSIGN
                w-ord.gross-wt = w-ord.net-wt + w-ord.tare-wt
                v-job          = w-ord.job-no + "-" + string(w-ord.job-no2,"99")
                .
            IF v-job BEGINS "-" THEN v-job = "".
            ASSIGN
                lv-middlesex-po  = SUBSTR(TRIM(w-ord.job-no),1,6)
                lv-middlesex-job = IF lv-middlesex-job EQ "" THEN "" ELSE
                            "%MX" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-job))) +
                            TRIM(lv-middlesex-job)
                lv-middlesex-po  = SUBSTR(TRIM(w-ord.cust-po-no),1,6)
                lv-middlesex-po  = IF lv-middlesex-po EQ "" THEN "" ELSE
                            "BNJ" +
                            FILL("0",6 - LENGTH(TRIM(lv-middlesex-po))) +
                            TRIM(lv-middlesex-po)
                .

            IF w-ord.total-tags GT 0 THEN 
            DO:
                lv-how-many-tags =  IF LOOKUP(v-loadtag,"SSLABEL,CentBox") > 0 OR w-ord.total-tags = 1 THEN w-ord.total-tags
                ELSE (w-ord.total-tags - 1).
                DO i = 1 TO (lv-how-many-tags * w-ord.mult):
                    /* loadtags generation */
                    /* IF i MOD w-ord.mult = 1 OR i = 1 OR w-ord.mult = 1  THEN DO:
                        IF i = 1 THEN lv-tag-no = i.
                        RUN create-loadtag (INPUT-OUTPUT lv-tag-no, w-ord.total-unit).
                     END.
                     */
                    PUT UNFORMATTED 
                        "~""  removeChars(w-ord.cust-name)  "~","
                        w-ord.ord-no  ","
                        "~""  v-job  "~","
                        "~""  CAPS(removeChars(w-ord.i-no))  FORM "x(15)" "~","
                        "~""  removeChars(w-ord.cust-part-no) "~","
                        "~""  removeChars(w-ord.cust-po-no)  "~","
                        w-ord.pcs  ","
                        w-ord.bundle  ","
                        w-ord.total-unit FORM ">>>>>>>9" ","
                        "~""  removeChars(w-ord.ship-code)  "~","
                        "~""  removeChars(w-ord.ship-name)  "~","
                        "~""  removeChars(w-ord.ship-add1)  "~","
                        "~""  removeChars(w-ord.ship-add2)  "~","
                        "~""  removeChars(w-ord.ship-city)  "~","
                        "~""  removeChars(w-ord.ship-state) "~","
                        "~""  removeChars(w-ord.ship-ctry)  "~","
                        "~""  removeChars(w-ord.ship-zip)   "~","
                        "~""  removeChars(w-ord.sold-code)  "~","
                        "~""  removeChars(w-ord.sold-name)  "~","
                        "~""  removeChars(w-ord.sold-add1)  "~","
                        "~""  removeChars(w-ord.sold-add2)  "~","
                        "~""  removeChars(w-ord.sold-city)  "~","
                        "~""  removeChars(w-ord.sold-state) "~","
                        "~""  removeChars(w-ord.sold-ctry)  "~","
                        "~""  removeChars(w-ord.sold-zip)   "~","
                        "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
                        "~""  w-ord.due-date  "~","
                        "~""  w-ord.rel-date  "~","
                        "~""  w-ord.upc-no  "~","
                        "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
                        "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
                        "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
                        "~""  w-ord.flute  "~","
                        "~""  w-ord.test  "~","
                        "~""  w-ord.vendor  "~","
                        w-ord.gross-wt  ","
                        w-ord.tare-wt  ","
                        w-ord.net-wt  ","
                        w-ord.sheet-wt  ","
                        "~""  w-ord.uom  "~","
                        "~""  removeChars(w-ord.style) "~","
                        "~""  removeChars(w-ord.style-desc) "~","
                        "~""  removeChars(w-ord.rel-lot#) "~","
                        "~""  lv-middlesex-job  "~","
                        "~""  lv-middlesex-po  "~","
                        "~""  loadtag.tag-no "~"," 
                        "~""  loadtag.partial "~","
                        "~""  w-ord.cas-no  "~","
                        "~""  removeChars(v-dept-note[1]) "~","
                        "~""  removeChars(v-dept-note[2]) "~","
                        "~""  removeChars(v-dept-note[3]) "~","
                        "~""  removeChars(v-dept-note[4]) "~","
                        "~""  removeChars(v-dept-note[5]) "~","
                        "~""  removeChars(v-dept-note[6]) "~","
                        "~""  removeChars(v-dept-note[7]) "~","
                        "~""  removeChars(v-dept-note[8]) "~","
                        w-ord.po-no ","
                        "~""  removeChars(v-dept-note[9]) "~","
                        "~""  removeChars(v-dept-note[10]) "~","
                        "~""  removeChars(v-dept-note[11]) "~","
                        "~""  removeChars(v-dept-note[12]) "~","
                        "~""  removeChars(v-dept-note[13]) "~","
                        "~""  removeChars(v-dept-note[14]) "~","
                        "~""  removeChars(v-dept-note[15]) "~","
                        "~""  removeChars(v-dept-note[16]) "~","   
                        "~""  removeChars(v-dept-note[17]) "~","
                        "~""  removeChars(v-dept-note[18]) "~","
                        "~""  removeChars(w-ord.est-no) "~","
                        "~""  removeChars(w-ord.ord-desc1)    "~","
                        "~""  removeChars(w-ord.ord-desc2)    "~","
                        .
                    IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN 
                    DO:
                
                        FIND FIRST rfidtag OF loadtag NO-LOCK NO-ERROR.
                        cRFIDTag = IF AVAILABLE rfidtag THEN rfidtag.rfidtag ELSE "".
                        PUT UNFORMATTED 
                            "~"" SUBSTR(loadtag.tag-no,16,5) "~"," 
                            "~"" cRFIDTag "~"," .
                    END.
                    PUT UNFORMATTED 
                        "~"" w-ord.due-date-jobhdr "~"," 
                        "~"" w-ord.due-date-job "~","
                        /* gdm - 08130804 */
                        "~"" w-ord.linenum "~","
                        /* gdm - 07170905 */
                        "~"" w-ord.unit-wt  "~","
                        "~"" w-ord.pallt-wt  "~","          
             
                        /* gdm - 10160905 */
                        "~"" removeChars(v-fgdsc1) "~","
                        "~"" removeChars(v-fgdsc2) "~","
                        "~"" removeChars(v-fgdsc3) "~","
                        "~"" removeChars(w-ord.lot) "~",".

                    PUT SKIP.

                END.
                IF LOOKUP(v-loadtag,"SSLABEL,CentBox") = 0 THEN
                DO v-count = 1 TO w-ord.mult: /* for partial print */
                    /* loadtags generation */
                    /* IF v-count EQ 1 THEN RUN create-loadtag (INPUT-OUTPUT lv-tag-no, 0).
                    */
                    PUT UNFORMATTED 
                        "~""  removeChars(w-ord.cust-name)  "~","
                        w-ord.ord-no  ","
                        "~""  v-job  "~","
                        "~""  CAPS(removeChars(w-ord.i-no))  FORM "x(15)" "~","
                        "~""  removeChars(w-ord.cust-part-no)  "~","
                        "~""  removeChars(w-ord.cust-po-no)  "~","
                        w-ord.pcs  ","
                        w-ord.bundle  ", ,"
                        "~""  removeChars(w-ord.ship-code)  "~","
                        "~""  removeChars(w-ord.ship-name)  "~","
                        "~""  removeChars(w-ord.ship-add1)  "~","
                        "~""  removeChars(w-ord.ship-add2)  "~","
                        "~""  removeChars(w-ord.ship-city)  "~","
                        "~""  removeChars(w-ord.ship-state) "~","
                        "~""  removeChars(w-ord.ship-ctry)  "~","
                        "~""  removeChars(w-ord.ship-zip)   "~","
                        "~""  removeChars(w-ord.sold-code)  "~","
                        "~""  removeChars(w-ord.sold-name)  "~","
                        "~""  removeChars(w-ord.sold-add1)  "~","
                        "~""  removeChars(w-ord.sold-add2)  "~","
                        "~""  removeChars(w-ord.sold-city)  "~","
                        "~""  removeChars(w-ord.sold-state) "~","
                        "~""  removeChars(w-ord.sold-ctry)  "~","
                        "~""  removeChars(w-ord.sold-zip)   "~","
                        "~""  removeChars(w-ord.i-name) FORMAT "X(30)"  "~","
                        "~""  w-ord.due-date  "~","
                        "~""  w-ord.rel-date  "~","
                        "~""  w-ord.upc-no  "~","
                        "~""  w-ord.box-len FORMAT ">>>9.99<<<" "~","
                        "~""  w-ord.box-wid FORMAT ">>>9.99<<<" "~","
                        "~""  w-ord.box-dep FORMAT ">>>9.99<<<" "~","
                        "~""  w-ord.flute  "~","
                        "~""  w-ord.test  "~","
                        "~""  w-ord.vendor  "~","
                        w-ord.gross-wt  ","
                        w-ord.tare-wt  ","
                        w-ord.net-wt  ","
                        w-ord.sheet-wt  ","
                        "~""  w-ord.uom  "~","
                        "~""  removeChars(w-ord.style) "~","
                        "~""  removeChars(w-ord.style-desc) "~","
                        "~""  removeChars(w-ord.rel-lot#) "~","
                        "~""  lv-middlesex-job  "~","
                        "~""  lv-middlesex-po  "~","
                        "~""  loadtag.tag-no "~"," 
                        "~""  loadtag.partial "~"," 
                        "~""  w-ord.cas-no  "~","
                        "~""  removeChars(v-dept-note[1]) "~","
                        "~""  removeChars(v-dept-note[2]) "~","
                        "~""  removeChars(v-dept-note[3]) "~","
                        "~""  removeChars(v-dept-note[4]) "~","
                        "~""  removeChars(v-dept-note[5]) "~","
                        "~""  removeChars(v-dept-note[6]) "~","
                        "~""  removeChars(v-dept-note[7]) "~","
                        "~""  removeChars(v-dept-note[8]) "~","
                        w-ord.po-no ","
                        "~""  removeChars(v-dept-note[9]) "~","
                        "~""  removeChars(v-dept-note[10]) "~","
                        "~""  removeChars(v-dept-note[11]) "~","
                        "~""  removeChars(v-dept-note[12]) "~","
                        "~""  removeChars(v-dept-note[13]) "~","
                        "~""  removeChars(v-dept-note[14]) "~","
                        "~""  removeChars(v-dept-note[15]) "~","
                        "~""  removeChars(v-dept-note[16]) "~","      
                        "~""  removeChars(v-dept-note[17]) "~","
                        "~""  removeChars(v-dept-note[18]) "~","
                        "~""  removeChars(w-ord.est-no) "~","
                        "~""  removeChars(w-ord.ord-desc1)    "~","
                        "~""  removeChars(w-ord.ord-desc2)    "~","
                        .   
                    IF LOOKUP(v-loadtag,"ASI,SSLABEL") GT 0 THEN
                        PUT UNFORMATTED "~"" SUBSTR(loadtag.tag-no,16,5) "~",".
             
                    /* gdm - 11040801 */
                    PUT UNFORMATTED
                        "~"" w-ord.linenum "~","
                        /* gdm - 07170905 */
             
                        "~"" w-ord.unit-wt  "~","
                        "~"" w-ord.pallt-wt  "~","
                        /* gdm - 10160905 */
             
                        "~"" removeChars(v-fgdsc1) "~","
                        "~"" removeChars(v-fgdsc2) "~","
                        "~"" removeChars(v-fgdsc3) "~","
                        "~"" removeChars(w-ord.lot) "~",".

                    PUT SKIP.
                END.
            END.
            DELETE w-ord.
        END.
        OUTPUT close.
    END.    /* NOT TRIAD */


END PROCEDURE.



/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE create-w-ord C-Win 
PROCEDURE create-w-ord :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rel-date AS DATE NO-UNDO.
    DEFINE BUFFER b-job     FOR job.
    DEFINE BUFFER b-job-hdr FOR job-hdr.

    FIND FIRST company WHERE company.company = loadtag.company NO-LOCK NO-ERROR.
    FIND FIRST itemfg WHERE itemfg.company = loadtag.company
        AND itemfg.i-no = loadtag.i-no NO-LOCK NO-ERROR.
    FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company = loadtag.company
        AND oe-ord.ord-no = loadtag.ord-no NO-ERROR.
    IF AVAILABLE oe-ord THEN 
        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company = loadtag.company
            AND oe-ordl.ord-no = loadtag.ord-no
            AND oe-ordl.i-no = loadtag.i-no NO-ERROR.
    IF AVAILABLE oe-ord AND AVAILABLE oe-ordl THEN 
    DO:

        FIND FIRST cust NO-LOCK WHERE cust.company = loadtag.company
            AND cust.cust-no = oe-ord.cust-no 
            NO-ERROR.

        FIND FIRST b-job NO-LOCK WHERE b-job.company = loadtag.company
            AND b-job.job-no  = loadtag.job-no
            AND b-job.job-no2 = loadtag.job-no2  
            NO-ERROR.
        IF AVAILABLE b-job THEN
            FIND FIRST b-job-hdr  NO-LOCK WHERE b-job-hdr.company EQ b-job.company
                AND b-job-hdr.job     EQ b-job.job
                AND b-job-hdr.job-no  EQ b-job.job-no
                AND b-job-hdr.job-no2 EQ b-job.job-no2
                AND b-job-hdr.i-no    EQ loadtag.i-no 
                NO-ERROR.
      
        CREATE w-ord.
        ASSIGN 
            w-ord.ord-no       = loadtag.ord-no
            w-ord.job-no       = loadtag.job-no
            w-ord.job-no2      = loadtag.job-no2
            w-ord.cust-no      = oe-ord.cust-no
            w-ord.cust-name    = oe-ord.cust-name
            w-ord.i-no         = loadtag.i-no
            w-ord.cust-part-no = oe-ordl.part-no
            w-ord.ord-qty      = loadtag.qty
            w-ord.po-no        = oe-ordl.po-no-po
            w-ord.i-name       = loadtag.i-name
            w-ord.due-date     = IF oe-ord.due-date NE ? THEN
                                   oe-ord.due-date
                                 ELSE
                                 IF oe-ordl.req-date NE ? THEN
                                   oe-ordl.req-date
                                 ELSE TODAY
            w-ord.est-no       = oe-ordl.est-no
            w-ord.form-no      = oe-ordl.form-no
            w-ord.vendor       = company.name
            w-ord.tare-wt      = 10
            w-ord.uom          = "EA"
            w-ord.mult         = IF cust.int-field[1] NE 0 THEN
                                   cust.int-field[1] ELSE v-mult
            w-ord.dont-run-set = oe-ordl.is-a-component
            w-ord.ord-desc1    = oe-ordl.part-dscr1
            w-ord.ord-desc2    = oe-ordl.part-dscr2
            w-ord.sold-code    = oe-ord.sold-id
            w-ord.sold-name    = oe-ord.sold-name
            w-ord.sold-add1    = oe-ord.sold-add[1]
            w-ord.sold-add2    = oe-ord.sold-add[2]
            w-ord.sold-city    = oe-ord.sold-city
            w-ord.sold-state   = oe-ord.sold-state
            w-ord.sold-zip     = oe-ord.sold-zip
            w-ord.linenum      = oe-ordl.e-num
            w-ord.lot          = loadtag.misc-char[2]
            .

        IF AVAILABLE b-job-hdr THEN
            w-ord.due-date-jobhdr = IF b-job-hdr.due-date <> ? THEN STRING(b-job-hdr.due-date, "99/99/9999") ELSE "".
        IF AVAILABLE b-job THEN
            w-ord.due-date-job = IF b-job.due-date <> ? THEN STRING(b-job.due-date, "99/99/9999") ELSE "".
             
        RUN get-rel-info (OUTPUT w-ord.cust-po-no,
            OUTPUT w-ord.rel-date,
            OUTPUT w-ord.rel-lot#).

        IF AVAILABLE itemfg THEN
            ASSIGN w-ord.upc-no  = itemfg.upc-no
                w-ord.box-len = itemfg.l-score[50]
                w-ord.box-wid = itemfg.w-score[50]
                w-ord.box-dep = itemfg.d-score[50]
                w-ord.flute   = itemfg.flute
                w-ord.test    = itemfg.test
                w-ord.pcs     = loadtag.qty-case
                w-ord.bundle  = loadtag.case-bundle
                w-ord.style   = itemfg.style
                .

        IF w-ord.style NE "" THEN
        DO:
            FIND FIRST style  NO-LOCK WHERE
                style.company EQ cocode AND
                style.style EQ w-ord.style
                NO-ERROR.

            IF AVAILABLE style THEN
            DO:
                w-ord.style-desc = style.dscr.
                RELEASE style.
            END.
        END.

        FIND FIRST shipto WHERE shipto.company EQ cocode
            AND shipto.cust-no EQ oe-ord.cust-no
            AND shipto.ship-id EQ oe-ord.cust-no
            USE-INDEX ship-id NO-LOCK NO-ERROR.
        IF AVAILABLE shipto THEN
            ASSIGN
                w-ord.ship-name  = shipto.ship-name
                w-ord.ship-add1  = shipto.ship-add[1]
                w-ord.ship-add2  = shipto.ship-add[2]
                w-ord.ship-city  = shipto.ship-city
                w-ord.ship-state = shipto.ship-state
                w-ord.ship-zip   = shipto.ship-zip
                .

        IF NOT AVAILABLE eb AND AVAILABLE itemfg AND itemfg.est-no NE "" THEN
            FIND FIRST eb
                WHERE eb.company  EQ itemfg.company
                AND eb.est-no   EQ itemfg.est-no
                AND eb.stock-no EQ itemfg.i-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE eb THEN
            ASSIGN
                w-ord.flute  = eb.flute
                w-ord.test   = eb.test
                w-ord.pcs    = eb.cas-cnt
                w-ord.bundle = eb.cas-pal
                w-ord.cas-no = eb.cas-no
                .

        ASSIGN 
            w-ord.total-tags = 1
            w-ord.ord-qty    = loadtag.qty 
            w-ord.pcs        = loadtag.qty-case
            w-ord.bundle     = loadtag.case-bundle
            w-ord.partial    = loadtag.partial
            w-ord.total-unit = w-ord.pcs * w-ord.bundle + w-ord.partial
            .      
    END.  /* avail oe-ord*/
    ELSE IF loadtag.job-no <> "" THEN 
        DO:
            FIND FIRST job NO-LOCK WHERE job.company = loadtag.company
                AND job.job-no = loadtag.job-no
                AND job.job-no2 = loadtag.job-no2  NO-ERROR.
            IF AVAILABLE job THEN
                FIND FIRST job-hdr WHERE job-hdr.company EQ job.company
                    AND job-hdr.job     EQ job.job
                    AND job-hdr.job-no  EQ job.job-no
                    AND job-hdr.job-no2 EQ job.job-no2
                    AND job-hdr.i-no    EQ loadtag.i-no NO-LOCK NO-ERROR.
            IF AVAILABLE job-hdr THEN 
            DO:
      
                FIND FIRST cust WHERE cust.company EQ cocode
                    AND cust.cust-no EQ job-hdr.cust-no NO-LOCK NO-ERROR.
                FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ job-hdr.i-no NO-LOCK NO-ERROR.
         
                CREATE w-ord.
                ASSIGN
                    w-ord.ord-no    = job-hdr.ord-no
                    w-ord.job-no    = job-hdr.job-no
                    w-ord.job-no2   = job-hdr.job-no2
                    w-ord.cust-no   = cust.cust-no
                    w-ord.cust-name = cust.name
                    w-ord.i-no      = job-hdr.i-no
                    w-ord.ord-qty   = job-hdr.qty
                    w-ord.due-date  = job.start-date
                    w-ord.est-no    = job.est-no
                    w-ord.form-no   = job-hdr.frm
                    w-ord.vendor    = company.name
                    w-ord.tare-wt   = 10
                    w-ord.uom       = "EA"
                    w-ord.mult      = IF cust.int-field[1] NE 0 THEN
                                   cust.int-field[1] ELSE v-mult
                    w-ord.lot       = loadtag.misc-char[2]
                    .

                IF AVAILABLE itemfg THEN
                    ASSIGN
                        w-ord.cust-part-no = itemfg.part-no
                        w-ord.style        = itemfg.style
                        w-ord.i-name       = itemfg.i-name
                        w-ord.upc-no       = itemfg.upc-no
                        w-ord.upc-no       = itemfg.upc-no
                        w-ord.box-len      = itemfg.l-score[50]
                        w-ord.box-wid      = itemfg.w-score[50]
                        w-ord.box-dep      = itemfg.d-score[50]
                        .

                IF w-ord.style NE "" THEN
                DO:
                    FIND FIRST style WHERE
                        style.company EQ cocode AND
                        style.style EQ w-ord.style
                        NO-LOCK NO-ERROR.

                    IF AVAILABLE style THEN
                    DO:
                        w-ord.style-desc = style.dscr.
                        RELEASE style.
                    END.
                END.

                FIND FIRST shipto
                    WHERE shipto.company EQ cocode
                    AND shipto.cust-no EQ job-hdr.cust-no
                    AND shipto.ship-id EQ job-hdr.cust-no
                    USE-INDEX ship-id NO-LOCK NO-ERROR.
                IF AVAILABLE shipto THEN
                    ASSIGN
                        w-ord.ship-name  = shipto.ship-name
                        w-ord.ship-add1  = shipto.ship-add[1]
                        w-ord.ship-add2  = shipto.ship-add[2]
                        w-ord.ship-city  = shipto.ship-city
                        w-ord.ship-state = shipto.ship-state
                        w-ord.ship-zip   = shipto.ship-zip
                        .

                FIND FIRST est WHERE est.company EQ job.company
                    AND est.est-no  EQ job.est-no
                    NO-LOCK NO-ERROR.
                RELEASE eb.
                IF AVAILABLE est THEN
                    FIND FIRST eb
                        WHERE eb.company   EQ est.company
                        AND eb.est-no    EQ est.est-no
                        AND eb.form-no   EQ job-hdr.frm
                        AND (eb.blank-no EQ job-hdr.blank-no OR job-hdr.blank-no EQ 0)
                        NO-LOCK NO-ERROR.

                IF AVAILABLE eb THEN
                    ASSIGN
                        w-ord.flute      = eb.flute
                        w-ord.test       = eb.test
                        w-ord.pcs        = eb.cas-cnt
                        w-ord.bundle     = eb.cas-pal
                        w-ord.total-unit = w-ord.pcs * w-ord.bundle
                        w-ord.partial    = 0 /* w-ord.ord-qty - w-ord.total-unit*/
                        w-ord.cas-no     = eb.cas-no
                        .

                ASSIGN 
                    w-ord.total-tags = 1
                    w-ord.ord-qty    = loadtag.qty 
                    w-ord.pcs        = loadtag.qty-case
                    w-ord.bundle     = loadtag.case-bundle
                    w-ord.partial    = loadtag.partial
                    w-ord.total-unit = w-ord.pcs * w-ord.bundle  
                    .      

            END.  /* avail job*/
        END. /* job-no <> "" */
        ELSE IF loadtag.po-no <> 0 THEN 
            DO:
                FIND FIRST po-ord WHERE po-ord.company = loadtag.company
                    AND po-ord.po-no = loadtag.po-no NO-LOCK NO-ERROR.
                IF AVAILABLE po-ord THEN
                    FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ po-ord.company
                        AND po-ordl.po-no EQ po-ord.po-no
                        AND po-ordl.i-no = loadtag.i-no
                        USE-INDEX po-no  NO-ERROR.
                IF AVAILABLE po-ordl THEN 
                DO:
                    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                        AND cust.cust-no EQ po-ord.cust-no NO-ERROR.
                    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                        AND vend.vend-no EQ po-ord.vend-no NO-ERROR.
                    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                        AND itemfg.i-no EQ po-ordl.i-no NO-ERROR.
         
                    CREATE w-ord.
                    ASSIGN
                        w-ord.cust-name = IF AVAILABLE cust THEN cust.name ELSE ''
                        w-ord.cust-no   = po-ord.cust-no
                        w-ord.due-date  = po-ord.due-date
                        w-ord.i-no      = po-ordl.i-no
                        w-ord.i-name    = po-ordl.i-name
                        w-ord.mult      = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                         cust.int-field[1] ELSE v-mult
                        w-ord.ord-qty   = po-ordl.ord-qty
                        w-ord.po-no     = po-ord.po-no
                        w-ord.tare-wt   = 10
                        w-ord.uom       = 'EA'
                        w-ord.vendor    = IF AVAILABLE vend THEN vend.name ELSE ''
                        w-ord.lot       = loadtag.misc-char[2]
                        . 
                    IF AVAILABLE itemfg THEN
                        ASSIGN w-ord.est-no  = itemfg.est-no
                            w-ord.upc-no  = itemfg.upc-no
                            w-ord.box-len = itemfg.l-score[50]
                            w-ord.box-wid = itemfg.w-score[50]
                            w-ord.box-dep = itemfg.d-score[50]
                            w-ord.flute   = itemfg.flute
                            w-ord.test    = itemfg.test
                            w-ord.pcs     = itemfg.case-count
                            w-ord.bundle  = IF itemfg.case-pall NE 0 THEN itemfg.case-pall ELSE 1
                            w-ord.style   = itemfg.style
                            .

                    IF w-ord.style NE "" THEN
                    DO:
                        FIND FIRST style WHERE
                            style.company EQ cocode AND
                            style.style EQ w-ord.style
                            NO-LOCK NO-ERROR.
         
                        IF AVAILABLE style THEN
                        DO:
                            w-ord.style-desc = style.dscr.
                            RELEASE style.
                        END.
                    END.

                    IF AVAILABLE itemfg AND itemfg.est-no NE '' THEN
                        FIND FIRST eb NO-LOCK WHERE eb.company EQ itemfg.company
                            AND eb.est-no EQ itemfg.est-no
                            AND eb.stock-no EQ itemfg.i-no NO-ERROR.
                    IF AVAILABLE eb THEN
                        ASSIGN w-ord.flute  = eb.flute
                            w-ord.test   = eb.test
                            w-ord.pcs    = eb.cas-cnt
                            w-ord.bundle = eb.cas-pal
                            w-ord.cas-no = eb.cas-no
                            .
      
                    FIND FIRST shipto NO-LOCK WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ po-ord.cust-no
                        AND shipto.ship-id EQ po-ord.cust-no
                        USE-INDEX ship-id NO-ERROR.
                    IF AVAILABLE shipto THEN
                        ASSIGN w-ord.ship-name  = shipto.ship-name
                            w-ord.ship-add1  = shipto.ship-add[1]
                            w-ord.ship-add2  = shipto.ship-add[2]
                            w-ord.ship-city  = shipto.ship-city
                            w-ord.ship-state = shipto.ship-state
                            w-ord.ship-zip   = shipto.ship-zip
                            .
      
                    ASSIGN 
                        w-ord.total-tags = 1
                        w-ord.ord-qty    = loadtag.qty 
                        w-ord.pcs        = loadtag.qty-case
                        w-ord.bundle     = loadtag.case-bundle
                        w-ord.partial    = loadtag.partial
                        w-ord.total-unit = w-ord.pcs * w-ord.bundle  
                        .      

                END. /* AVAIL PO-ORDL */
            END. /* po-no <> ""*/
            ELSE 
            DO:
                FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ cocode
                    AND itemfg.i-no EQ loadtag.i-no NO-ERROR.
                IF AVAILABLE itemfg THEN 
                DO:
                    FIND FIRST vend NO-LOCK WHERE vend.company EQ cocode
                        AND vend.vend-no EQ itemfg.vend-no NO-ERROR.
                    FIND FIRST cust NO-LOCK WHERE cust.company EQ cocode
                        AND cust.cust-no EQ itemfg.cust-no NO-ERROR.
          
                    CREATE w-ord.
                    ASSIGN 
                        w-ord.i-no         = itemfg.i-no
                        w-ord.i-name       = itemfg.i-name
                        w-ord.cust-no      = itemfg.cust-no
                        w-ord.cust-name    = itemfg.cust-name
                        w-ord.cust-part-no = itemfg.part-no
                        w-ord.mult         = IF AVAILABLE cust AND cust.int-field[1] NE 0 THEN
                              cust.int-field[1] ELSE v-mult
                        w-ord.box-len      = itemfg.l-score[50]
                        w-ord.box-wid      = itemfg.w-score[50]
                        w-ord.box-dep      = itemfg.d-score[50]
                        w-ord.flute        = itemfg.flute
                        w-ord.upc-no       = itemfg.upc-no
                        w-ord.test         = itemfg.test
                        w-ord.vendor       = IF AVAILABLE vend THEN vend.name ELSE company.name
                        w-ord.tare-wt      = 10
                        w-ord.uom          = "EA"
                        w-ord.pcs          = itemfg.case-count
                        w-ord.bundle       = itemfg.case-pall
                        w-ord.total-tags   = 1
                        w-ord.ord-qty      = loadtag.qty 
                        w-ord.pcs          = loadtag.qty-case
                        w-ord.bundle       = loadtag.case-bundle
                        w-ord.partial      = loadtag.partial
                        w-ord.total-unit   = w-ord.pcs * w-ord.bundle + w-ord.partial
                        w-ord.style        = itemfg.style
                        w-ord.lot          = loadtag.misc-char[2]
                        .

                    IF w-ord.style NE "" THEN
                    DO:
                        FIND FIRST style WHERE
                            style.company EQ cocode AND
                            style.style EQ w-ord.style
                            NO-LOCK NO-ERROR.
          
                        IF AVAILABLE style THEN
                        DO:
                            w-ord.style-desc = style.dscr.
                            RELEASE style.
                        END.
                    END.
                END. /* avail itemfg */
            END. /* loadtag.po-no = 0 */
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE createComponentList C-Win 
PROCEDURE createComponentList :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/


    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-fg-set FOR fg-set.
    
    FOR EACH bf-itemfg NO-LOCK
        WHERE bf-itemfg.company EQ cocode
        AND bf-itemfg.i-no GE begin_i-no
        AND bf-itemfg.i-no LE end_i-no
        AND bf-itemfg.isaset
        ,
        EACH bf-fg-set  NO-LOCK
        WHERE bf-fg-set.company EQ bf-itemfg.company
        AND bf-fg-set.set-no EQ bf-itemfg.i-no
        :

        FIND FIRST tt-set WHERE tt-set.part-no = bf-fg-set.part-no NO-ERROR.
        IF NOT AVAILABLE tt-set THEN 
        DO:
            CREATE tt-set.
            ASSIGN 
                tt-set.part-no = bf-fg-set.part-no
                .
        END.
 
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE crt-transfer C-Win 
PROCEDURE crt-transfer :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-rno AS INTEGER NO-UNDO.
    DEFINE BUFFER b-fg-rctd FOR fg-rctd.
    DEFINE VARIABLE lv-rctd-rowid AS ROWID NO-UNDO.

    /* Code placed here will execute PRIOR to standard behavior. */
    FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAILABLE b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

    FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.
    IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

    DO WHILE TRUE:
        lv-rno = lv-rno + 1.
        IF CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no) OR
            CAN-FIND(FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd) THEN
            NEXT.
        LEAVE.
    END.

    FOR EACH fg-bin NO-LOCK WHERE
        fg-bin.company EQ cocode AND
        fg-bin.i-no    EQ b2-fg-rctd.i-no AND
        fg-bin.job-no  EQ b2-fg-rctd.job-no AND
        fg-bin.job-no2 EQ b2-fg-rctd.job-no2 AND
        fg-bin.tag     EQ b2-fg-rctd.tag
        :

        IF fg-bin.loc NE b2-fg-rctd.loc OR
            fg-bin.loc-bin NE b2-fg-rctd.loc-bin THEN 
        DO:
            CREATE b-fg-rctd.
            BUFFER-COPY b2-fg-rctd EXCEPT b2-fg-rctd.r-no TO b-fg-rctd
                ASSIGN 
                b-fg-rctd.r-no = lv-rno
                b-fg-rctd.loc = fg-bin.loc
                b-fg-rctd.loc-bin = fg-bin.loc-bin
                b-fg-rctd.cases = 0
                b-fg-rctd.qty-case = 0
                b-fg-rctd.cases-unit = 0
                b-fg-rctd.partial = 0
                b-fg-rctd.t-qty = 0
                lv-rno = lv-rno + 1
                .
            RELEASE b-fg-rctd.
        END.
    END.  /* for each fg-bin*/
END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE farmOutComp C-Win 
PROCEDURE farmOutComp :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE cJob    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2 AS INTEGER   NO-UNDO.
    cJob = "".
    iJobNo2 = 0.   
  
    IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN 
    DO:
      
        /* Find a job for this po if this is a farmout */
        IF w-fg-rctd.job-no GT "" THEN
            ASSIGN cJob    = w-fg-rctd.job-no
                iJobNo2 = w-fg-rctd.job-no2
                .
        ELSE IF w-fg-rctd.po-no GT "" THEN  
            DO:
                FIND FIRST po-ordl NO-LOCK WHERE po-ordl.company EQ w-fg-rctd.company
                    AND po-ordl.po-no EQ INTEGER(w-fg-rctd.po-no)
                    AND po-ordl.i-no  EQ w-fg-rctd.i-no
                    NO-ERROR.
           
                IF AVAIL(po-ordl) AND po-ordl.ord-no GT 0 THEN 
                DO:
           
                    FIND FIRST oe-ordl  NO-LOCK WHERE oe-ordl.company EQ g_company
                        AND oe-ordl.ord-no EQ po-ordl.ord-no
                        AND oe-ordl.i-no   EQ po-ordl.i-no
                        NO-ERROR.
                    /* assumption is that for farm jobs, order and job are always the same */
                    /* This is to obtain the job-no2 since job-no is assumed to be the order # */
                    IF NOT AVAILABLE oe-ordl THEN
                        FIND FIRST oe-ordl NO-LOCK WHERE oe-ordl.company EQ g_company
                            AND oe-ordl.ord-no EQ po-ordl.ord-no
                            AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                            NO-ERROR.
               
                    IF AVAILABLE oe-ordl AND oe-ordl.job-no GT "" THEN
                        ASSIGN cJob    = oe-ordl.job-no
                            iJobNo2 = oe-ordl.job-no2
                            .
                END.
                ELSE IF AVAIL(po-ordl) AND po-ordl.job-no GT "" THEN 
                    DO:
                        ASSIGN 
                            cJob    = po-ordl.job-no
                            iJobNo2 = po-ordl.job-no2
                            .
                    END.
          
            END.

     
        FIND FIRST job NO-LOCK WHERE job.company EQ w-fg-rctd.company
            AND job.job-no EQ cJob
            AND job.job-no2 EQ iJobNo2
            NO-ERROR.
    
        IF AVAILABLE job AND cJob GT "" 
            AND w-fg-rctd.rita-code EQ "R" 
            AND w-fg-rctd.qty GT 0 THEN 
        DO:             
            /* Copy fg-rctd for the jobs farmout tab */
            CREATE job-farm-rctd.
            BUFFER-COPY w-fg-rctd EXCEPT rec_key rita-code TO job-farm-rctd.
            ASSIGN 
                job-farm-rctd.rita-code = "F"
                job-farm-rctd.job-no    = cJob
                job-farm-rctd.job-no2   = iJobNo2
                .
            /* ASSIGN job-farm-rctd.job = job.job. */
            RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
        END.

    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fg-post C-Win 
PROCEDURE fg-post :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
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
    DEFINE VARIABLE fgPostLog      AS LOGICAL   NO-UNDO.
    /*##PN - variable for FGSetAssembly setting*/

    DEFINE VARIABLE lFound         AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE lFGSetAssembly AS LOGICAL   NO-UNDO.
    DEFINE VARIABLE cFGSetAssembly AS CHARACTER NO-UNDO.

    fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
    IF fgPostLog THEN
        OUTPUT STREAM logFile TO VALUE('logs/fgpstall.' +
            STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

    SESSION:SET-WAIT-STATE ("general").
    IF fgPostLog THEN RUN fgPostLog ('Started').
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

    /* Check for invalid transfers */
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code = "T"
        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.trans-time
        BY w-fg-rctd.r-no:

        IF NOT CAN-FIND(FIRST itemfg WHERE
            itemfg.company EQ cocode AND
            itemfg.i-no    EQ w-fg-rctd.i-no) THEN
            NEXT.

        FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
            AND fg-bin.i-no    EQ w-fg-rctd.i-no
            AND fg-bin.job-no  EQ w-fg-rctd.job-no
            AND fg-bin.job-no2 EQ w-fg-rctd.job-no2
            AND fg-bin.loc     EQ w-fg-rctd.loc
            AND fg-bin.loc-bin EQ w-fg-rctd.loc-bin
            AND fg-bin.tag     EQ w-fg-rctd.tag
            AND fg-bin.cust-no EQ w-fg-rctd.cust-no
            USE-INDEX co-ino NO-ERROR.
        IF NOT AVAILABLE fg-bin THEN 
        DO:
            /*            MESSAGE "A transfer exists for item " w-fg-rctd.i-no SKIP*/
            /*                "with an invalid location:" SKIP                     */
            /*                "  Warehouse = " w-fg-rctd.loc SKIP                  */
            /*                "  Bin = " w-fg-rctd.loc-bin SKIP                    */
            /*                "  Tag = " w-fg-rctd.tag SKIP                        */
            /*                "Please correct and re-run the posting process." SKIP*/
            /*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.            */
            RETURN "Invalid Location".
        END.
        ELSE IF fg-bin.pur-uom EQ "" THEN 
            DO:
                /*                MESSAGE "A blank UOM exists for item bin " w-fg-rctd.i-no SKIP*/
                /*                    "with location:" SKIP                                     */
                /*                    "  Warehouse = " w-fg-rctd.loc SKIP                       */
                /*                    "  Bin = " w-fg-rctd.loc-bin SKIP                         */
                /*                    "  Tag = " w-fg-rctd.tag SKIP                             */
                /*                    "Please correct and re-run the posting process." SKIP     */
                /*                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                 */
                RETURN "Invalid Location".
            END.
            ELSE IF itemfg.prod-uom EQ "" THEN 
                DO:
                    /*                    MESSAGE "A blank cost UOM exists for item " w-fg-rctd.i-no SKIP*/
                    /*                        "Please correct and re-run the posting process." SKIP      */
                    /*                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                  */
                    RETURN "Invalid Location".          
                END.
    END.
    /* #pn# Setting rita-code to A since the negative R was causing problems */
    /* #pn# task 08211305                                                    */   
    FOR EACH w-fg-rctd:
        FOR EACH b-w-fg-rctd WHERE b-w-fg-rctd.qty LT 0,
          
            FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" 
            AND reftable.company  EQ w-fg-rctd.company 
            AND reftable.loc      EQ STRING(b-w-fg-rctd.r-no,"9999999999")        
            AND (reftable.dscr EQ "fg-rctd: " + STRING(w-fg-rctd.r-no, "9999999999") AND reftable.dscr BEGINS "fg-rctd: ")  
            USE-INDEX loc   NO-LOCK .

            FIND fg-rctd WHERE ROWID(fg-rctd) = b-w-fg-rctd.row-id EXCLUSIVE-LOCK NO-ERROR.
            FIND FIRST itemfg NO-LOCK WHERE
                itemfg.company EQ cocode AND
                itemfg.i-no    EQ w-fg-rctd.i-no
                NO-ERROR.
        
            IF AVAILABLE fg-rctd  THEN 
            DO:
                /*##BL - FGSetAssembly requires the bin to match that of the character*/
                /*##BL of FGSetAssembly N-K.  If it doesn't, abort posting  */
                IF lFGSetAssembly 
                    AND fg-rctd.loc-bin NE cFGSetAssembly 
                    AND avail(itemfg) 
                    AND itemfg.alloc  EQ NO THEN 
                DO:
                    /*                    MESSAGE "The Bin location for Component " fg-rctd.i-no " must be " cFGSetAssembly "." SKIP*/
                    /*                        "Please correct on the Set Parts tab of FG Receiving and re-run the posting process." */
                    /*                        VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                                             */
                    RETURN ERROR.
                END.
                ASSIGN 
                    b-w-fg-rctd.rita-code = "A"
                    fg-rctd.rita-code     = "A"
                    .
            END.
            RELEASE fg-rctd.

        END.
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

            FIND FIRST itemfg EXCLUSIVE-LOCK WHERE
                itemfg.company EQ cocode AND
                itemfg.i-no    EQ w-fg-rctd.i-no
                NO-ERROR NO-WAIT.
       
            IF AVAILABLE itemfg THEN
            DO:           
                /* If FGEMAIL is active and quantity on hand is zero and item is allocated,
                   then process user data into a temp-table for processing emails later. */
                IF gv-fgemail = YES AND (itemfg.q-onh = 0 AND itemfg.q-alloc > 0) THEN
                    RUN Process-FGemail-Data (INPUT itemfg.i-no, w-fg-rctd.t-qty,w-fg-rctd.po-no).

                IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-postBatch.i ' + TRIM(itemfg.i-no)).
          
          /* itemfg gets updated here. */
                    {fg/fg-postBatch.i w-fg-rctd w-fg-rctd}
          
                IF autofgissue-log THEN
                    RUN farmOutComp.

                FIND CURRENT itemfg NO-LOCK NO-ERROR.
                FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
                FIND CURRENT po-ordl NO-LOCK NO-ERROR.
                FIND CURRENT fg-bin NO-LOCK NO-ERROR.
                LEAVE loop1.
            END. /* IF AVAIL itemfg */
        END. /* loop1 REPEAT */
    
        IF fgPostLog THEN RUN fgPostLog ('End fg/fg-postBatch.i - Start fg/fgemails.i').
        IF w-fg-rctd.rita-code = "R" THEN 
        DO:
        {fg/fgemails.i}
        END.
    
        IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

        FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

        IF AVAILABLE fg-rctd THEN 
        DO:
            ASSIGN
                fg-rctd.rita-code  = "P"  /* posted */
                fg-rctd.post-date  = v-post-date
                fg-rctd.trans-time = TIME
                fg-rctd.tag2       = w-fg-rctd.tag2
                .

            FOR EACH fg-rcpts EXCLUSIVE-LOCK
                WHERE fg-rcpts.company EQ fg-rctd.company
                AND fg-rcpts.r-no    EQ fg-rctd.r-no:
                ASSIGN 
                    fg-rcpts.rita-code = fg-rctd.rita-code
                    .
            END.

            FIND CURRENT fg-rctd NO-LOCK.
        END.

        IF fgPostLog THEN RUN fgPostLog ('End loop'). 
    END.  /* for each w-fg-rctd */


    IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
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
                    tt-posted-items.i-no = w-fg-rctd.i-no
                    .
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

            IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').
      
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
                        loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case
                        .

                ELSE /*partial transfer */
                    ASSIGN
                        loadtag.loc     = w-fg-rctd.loc
                        loadtag.loc-bin = w-fg-rctd.loc-bin
                        .
      
                FIND CURRENT loadtag NO-LOCK.
            END. /* If available loadtag */
        END. /* last-of tag */
          
    END. /* for each w-fg-rctd */

    FOR EACH w-inv:
        /* Save w-inv data to send email bol's */
        DELETE w-inv.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
    FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK:

        CREATE w-inv.
        w-inv.row-id = w-fg-rctd.row-id.
    END.
    IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

    IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
    RUN fg/invrecpt.p (?, 2).
    IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

    FOR EACH w-inv:
        /* Save w-inv data to send email bol's */
        CREATE tt-inv.
        BUFFER-COPY w-inv TO tt-inv.
    
    END.

  
    IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').

    FOR EACH w-fg-rctd WHERE (TRIM(w-fg-rctd.tag) EQ "" OR v-cost-from-receipt = "TransferCost"),
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        NO-LOCK
        BREAK BY w-fg-rctd.i-no:



        IF LAST-OF(w-fg-rctd.i-no) THEN 
        DO:

            IF fgPostLog THEN RUN fgPostLog ('Third loop  -  Start Last i-no').

            IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
            RUN fg/updfgcs1.p (RECID(itemfg), NO).
            IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

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


            FOR EACH oe-ordl NO-LOCK
                WHERE oe-ordl.company EQ cocode
                AND oe-ordl.opened  EQ YES
                AND oe-ordl.i-no    EQ w-fg-rctd.i-no
                AND oe-ordl.job-no  EQ ""
                /*  AND oe-ordl.cost    EQ 0*/
                USE-INDEX opened
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

                IF fgPostLog THEN RUN fgPostLog ('Third loop - End Last i-no').

            END. /* each oe-ordl */
        END. /* last of i-no */
    END. /* each w-fg-rctd */

    IF fgPostLog THEN RUN fgPostLog ('Start process releases').
    /* If overage, reset quantity or create a new release */
    RUN process-releases.
    IF fgPostLog THEN RUN fgPostLog ('End process releases').

    IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

    IF v-fgpostgl NE "None" THEN 
    DO TRANSACTION:

        loop2:
        REPEAT:
            FIND FIRST gl-ctrl WHERE gl-ctrl.company EQ cocode EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE gl-ctrl THEN
            DO:
                ASSIGN
                    v-trnum       = gl-ctrl.trnum + 1
                    gl-ctrl.trnum = v-trnum
                    .
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE loop2.
            END.
        END.

        IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').
        RUN gl-from-work (1, v-trnum).
        IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
        RUN gl-from-work (2, v-trnum).
        IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
    END.
    FIND CURRENT itemfg-loc NO-LOCK NO-ERROR.
    FIND FIRST w-job NO-ERROR.
    IF AVAILABLE w-job THEN 
    DO:
        IF fgPostLog THEN RUN fgPostLog ('Start  jc/d-jclose.p').
        /* RUN jc/d-jclose.w. */
        IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
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
                    gl-ctrl.trnum = v-trnum
                    .
                FIND CURRENT gl-ctrl NO-LOCK.
                LEAVE.
            END.
        END.

        IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
        FOR EACH work-job BREAK BY work-job.actnum:
            CREATE gltrans.
            ASSIGN
                gltrans.company = cocode
                gltrans.actnum  = work-job.actnum
                gltrans.jrnl    = "ADJUST"
                gltrans.tr-date = v-post-date
                gltrans.period  = period.pnum
                gltrans.trnum   = v-trnum
                .
    
            IF work-job.fg THEN
                ASSIGN
                    gltrans.tr-amt  = - work-job.amt
                    gltrans.tr-dscr = "ADJUSTMENT FG"
                    .
            ELSE
                ASSIGN
                    gltrans.tr-amt  = work-job.amt
                    gltrans.tr-dscr = "ADJUSTMENT COGS"
                    .

            RELEASE gltrans.
        END. /* each work-job */
        IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
    END.

    IF tg-recalc-cost THEN 
    DO:
        FOR EACH tt-posted-items:        
            RUN fg/updfgcst.p (tt-posted-items.i-no).
        END.
    END.

    IF v-got-fgemail THEN 
    DO:
        IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
        /*        RUN send-fgemail (v-fgemail-file).*/
        IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
    END.  
  
    FOR EACH w-fg-rctd ,
        FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no NO-LOCK , 
        EACH tt-inv WHERE tt-inv.row-id EQ w-fg-rctd.row-id 
        BREAK BY tt-inv.bol-no:

        FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-LOCK NO-ERROR.
        RUN get-ord-recs (ROWID(fg-rctd),
            BUFFER po-ordl,
            BUFFER po-ord,
            BUFFER oe-ordl,
            BUFFER oe-ord,
            BUFFER reftable).
        IF AVAIL(reftable) AND (reftable.val[2] GT 0 OR reftable.val[3] EQ 1) THEN
            ASSIGN ll        = reftable.val[1] NE 0
                dBillAmt  = reftable.val[2]
                lEmailBol = reftable.val[3] EQ 1
                lInvFrt   = reftable.val[1] GT 0
                .
        IF lEmailBol AND last-of(tt-inv.bol-no) THEN 
        DO:
            FIND FIRST oe-bolh WHERE oe-bolh.company EQ g_company
                AND oe-bolh.bol-no EQ tt-inv.bol-no NO-LOCK NO-ERROR.
         
            RUN custom/setUserPrint.p (g_company,'oe-boll_.',
                'begin_cust,end_cust,begin_bol#,end_bol#,begin_ord#,end_ord#,tb_reprint,tb_posted,rd_bolcert',
                oe-bolh.cust-no + ',' + oe-bolh.cust-no + ',' +
                STRING(oe-bolh.bol-no) + ',' + STRING(oe-bolh.bol-no) +
                ',,99999999,' + STRING(oe-bolh.printed) + ',' +
                STRING(oe-bolh.posted) + ',BOL').
            RUN listobjs/oe-boll_.w.
  
        END. /* If email bol */
    END. /* each w-fg-rctd */

    IF fgPostLog THEN RUN fgPostLog ('End').
    IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
    /* WFK - no error message was being returned, so set to no if */
    /*       no return error was encountered                      */
    ERROR-STATUS:ERROR = NO.
 
    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgpostdoti C-Win
PROCEDURE fgpostdoti:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-time          AS CHARACTER FORMAT "X(5)" NO-UNDO.
    DEFINE VARIABLE v-whse          LIKE fg-rctd.loc. 
    DEFINE VARIABLE v-cstprt        AS CHARACTER FORMAT "x(15)" NO-UNDO.          
    DEFINE VARIABLE ll-wip          AS LOG.
    DEFINE VARIABLE v-qty-pallet    AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-corr          AS LOG       NO-UNDO.
    DEFINE VARIABLE v-tran-type     AS CHARACTER FORMAT "x(1)".  
    
    DEFINE VARIABLE v-cum-tot       AS de. 
    DEFINE VARIABLE v-tot-cost      AS DECIMAL   FORMAT "->>>,>>9.99<<".
    DEFINE VARIABLE v-grd-tot-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99<<". 
    DEFINE VARIABLE v-tot-qty       AS INTEGER   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-pr-tots2      LIKE v-pr-tots NO-UNDO.
    DEFINE VARIABLE v-grd-tot-qty   AS INTEGER   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-grd-tot-value AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".          
    DEFINE VARIABLE v-pr-tots       AS LOG       NO-UNDO.
    DEFINE VARIABLE v-entrytype     AS CHARACTER INITIAL "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
    DEFINE VARIABLE v-fg-qty        LIKE fg-rctd.t-qty.
    DEFINE VARIABLE v-fg-cost       AS DECIMAL   FORMAT "->,>>>,>>9.99<<".  
          
    FOR EACH w-fg-rctd

        BREAK BY w-fg-rctd.loc
        BY w-fg-rctd.i-no
        BY w-fg-rctd.loc-bin
        BY w-fg-rctd.tag:

            
        IF FIRST-OF(w-fg-rctd.loc) THEN 
        DO:
            v-whse = w-fg-rctd.loc.
        END.
    
        ASSIGN 
            v-cstprt = "".

        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ cocode
            AND itemfg.i-no    EQ w-fg-rctd.i-no
            NO-ERROR.

        RELEASE prod.
        /* gdm - 06150904 */
        IF AVAILABLE itemfg THEN ASSIGN v-cstprt = itemfg.part-no.

        IF AVAILABLE itemfg THEN
            FIND FIRST prodl NO-LOCK
                WHERE prodl.company EQ cocode
                AND prodl.procat  EQ itemfg.procat
                AND CAN-FIND(FIRST prod
                WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin)
                NO-ERROR.

        IF AVAILABLE prodl THEN
            FIND FIRST prod
                WHERE prod.company EQ cocode
                AND prod.prolin  EQ prodl.prolin
                NO-LOCK NO-ERROR.

        ASSIGN
            /*v-fg-qty   = w-fg-rctd.t-qty*/
            /*v-fg-cost  = w-fg-rctd.ext-cost*/
            v-fg-value = 0
            v-msf[1]   = 0
            v-msf[2]   = 0
            .

        RELEASE job-mat.
    
        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            FIND FIRST job-hdr NO-LOCK
                WHERE job-hdr.company EQ cocode
                AND job-hdr.job-no  EQ w-fg-rctd.job-no
                AND job-hdr.job-no2 EQ w-fg-rctd.job-no2
                AND job-hdr.i-no    EQ w-fg-rctd.i-no
                USE-INDEX job-no NO-ERROR.

            ll-wip = NO.

            IF AVAILABLE job-hdr THEN 
            DO:
                /* For calculating the quantity per pallet. */
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ w-fg-rctd.company
                    AND fg-bin.job-no  EQ job-hdr.job-no
                    AND fg-bin.job-no2 EQ job-hdr.job-no2
                    AND fg-bin.i-no    EQ job-hdr.i-no
                    AND fg-bin.loc-bin EQ w-fg-rctd.loc-bin
                    AND fg-bin.tag     EQ w-fg-rctd.tag
                    NO-ERROR.

                v-qty-pallet = w-fg-rctd.cases *
                    IF AVAILABLE fg-bin THEN fg-bin.cases-unit ELSE 1.

                FIND FIRST job NO-LOCK
                    WHERE job.company EQ cocode
                    AND job.job     EQ job-hdr.job
                    AND job.job-no  EQ job-hdr.job-no
                    AND job.job-no2 EQ job-hdr.job-no2
                    NO-ERROR.

                IF AVAILABLE job AND INT(w-fg-rctd.po-no) EQ 0 AND
                    v-fgpostgl EQ "AllItems" AND AVAILABLE prod THEN 
                DO:
                    ASSIGN
                        wip-amt = w-fg-rctd.t-qty / 1000 * job-hdr.std-mat-cost
                        wip-lab = w-fg-rctd.t-qty / 1000 * job-hdr.std-lab-cost
                        wip-foh = w-fg-rctd.t-qty / 1000 * job-hdr.std-fix-cost
                        wip-voh = w-fg-rctd.t-qty / 1000 * job-hdr.std-var-cost
                        .

                    IF wip-amt NE ? AND wip-lab NE ? AND wip-foh NE ? AND wip-voh NE ? THEN 
                    DO:
                    {jc/jcglcrt.i prod.fg-mat 0 wip-amt}    /* Finished Goods Material */
                    {jc/jcglcrt.i prod.fg-lab 0 wip-lab}    /* Finished Goods Direct Labor */
                    {jc/jcglcrt.i prod.fg-fo  0 wip-foh}    /* Finished Goods Fixed Ovrhd */
                    {jc/jcglcrt.i prod.fg-vo  0 wip-voh}    /* Finished Goods Variable O/H */
                    {jc/jcglcrt.i prod.wip-mat wip-amt 0}   /* Work in Process Material */
                    {jc/jcglcrt.i prod.wip-lab wip-lab 0}   /* WIP Direct Labor */
                    {jc/jcglcrt.i prod.wip-fo  wip-foh 0}   /* WIP Fixed Overhead */
                        {jc/jcglcrt.i prod.wip-vo  wip-voh 0}   /* WIP Variable Overhead */
                        ll-wip = YES.
                    END.
                END.
            END.
      
            IF w-fg-rctd.ext-cost NE 0 AND NOT ll-wip THEN 
            DO:
                RELEASE po-ord.
                RELEASE po-ordl.
                IF int(w-fg-rctd.po-no) NE 0 THEN
                    FIND FIRST po-ord
                        WHERE po-ord.company EQ cocode
                        AND po-ord.po-no   EQ int(w-fg-rctd.po-no)
                        NO-LOCK NO-ERROR.
                IF AVAILABLE po-ord THEN
                    FIND FIRST po-ordl NO-LOCK
                        WHERE po-ordl.company   EQ cocode
                        AND po-ordl.po-no     EQ po-ord.po-no
                        AND po-ordl.i-no      EQ w-fg-rctd.i-no
                        AND po-ordl.deleted   EQ NO
                        AND po-ordl.item-type EQ NO
                        NO-ERROR.
                IF AVAILABLE itemfg AND v-fgpostgl NE "None"       AND
                    (AVAILABLE po-ordl OR v-fgpostgl EQ "AllItems") THEN 
                DO:
                    IF w-fg-rctd.ext-cost NE 0  AND
                        w-fg-rctd.ext-cost NE ?  AND
                        AVAILABLE prod         AND  
                        prod.fg-mat NE ""  AND
                        prod.wip-mat NE "" THEN 
                    DO:                          
                        /* Debit FG Material */
                        FIND FIRST work-gl WHERE work-gl.actnum EQ prod.fg-mat NO-LOCK NO-ERROR.      
                        IF NOT AVAILABLE work-gl THEN 
                        DO:
                            CREATE work-gl.
                            work-gl.actnum = prod.fg-mat.
                        END.
                        work-gl.debits = work-gl.debits + w-fg-rctd.ext-cost.             
                        /* Credit WIP Material */
                        FIND FIRST work-gl WHERE work-gl.actnum EQ prod.wip-mat NO-LOCK NO-ERROR.      
                        IF NOT AVAILABLE work-gl THEN 
                        DO:
                            CREATE work-gl.
                            work-gl.actnum = prod.wip-mat.
                        END.
                        work-gl.credits = work-gl.credits + w-fg-rctd.ext-cost.
                    END.  /* if w-fg-rctd.ext-cost */
                END.  
            END.

            IF AVAILABLE job-hdr THEN 
            DO:
                IF LAST-OF(w-fg-rctd.i-no) THEN
                    FOR EACH mch-act FIELDS(waste)
                        WHERE mch-act.company  EQ cocode
                        AND mch-act.job      EQ job-hdr.job
                        AND mch-act.job-no   EQ job-hdr.job-no
                        AND mch-act.job-no2  EQ job-hdr.job-no2
                        AND mch-act.frm      EQ job-hdr.frm
                        USE-INDEX job NO-LOCK:
                        v-msf[2] = v-msf[2] + mch-act.waste.
                    END.

                FOR EACH job-mat
                    WHERE job-mat.company EQ cocode
                    AND job-mat.job     EQ job-hdr.job
                    AND job-mat.job-no  EQ job-hdr.job-no
                    AND job-mat.job-no2 EQ job-hdr.job-no2
                    AND job-mat.frm     EQ job-hdr.frm
                    NO-LOCK,
                    FIRST item
                    WHERE item.company    EQ cocode
                    AND item.i-no       EQ job-mat.i-no
                    AND lookup(item.mat-type,"B,1,2,3,4") GT 0
                    NO-LOCK:
                    LEAVE.
                END.

                IF AVAILABLE job-mat THEN 
                DO:
                    ASSIGN 
                        v-msf[1] = w-fg-rctd.t-qty *
                      (job-mat.len * job-mat.wid * (job-hdr.sq-in / 100))
                        v-msf[2] = v-msf[2]        *
                      (job-mat.len * job-mat.wid * (job-hdr.sq-in / 100))
                        .

                    IF v-corr THEN
                        ASSIGN
                            v-msf[1] = v-msf[1] * .007
                            v-msf[2] = v-msf[2] * .007
                            .
                    ELSE
                        ASSIGN
                            v-msf[1] = v-msf[1] / 144
                            v-msf[2] = v-msf[2] / 144
                            .
                END.
            END.
        END.
    
        ELSE
            IF w-fg-rctd.rita-code EQ "A" AND v-adjustgl THEN 
            DO:
                FIND FIRST fg-bin NO-LOCK
                    WHERE fg-bin.company EQ cocode
                    AND fg-bin.i-no    EQ w-fg-rctd.i-no
                    AND fg-bin.job-no  EQ w-fg-rctd.job-no
                    AND fg-bin.job-no2 EQ w-fg-rctd.job-no2
                    AND fg-bin.loc     EQ w-fg-rctd.loc
                    AND fg-bin.loc-bin EQ w-fg-rctd.loc-bin
                    AND fg-bin.tag     EQ w-fg-rctd.tag
                    NO-ERROR.
        
                IF AVAILABLE fg-bin THEN
                    RUN oe/invposty.p (0, fg-bin.i-no, w-fg-rctd.t-qty * -1, fg-bin.pur-uom,
                        fg-bin.std-lab-cost, fg-bin.std-fix-cost,
                        fg-bin.std-var-cost, fg-bin.std-mat-cost).
            END.


        IF INDEX("RTASEI",w-fg-rctd.rita-code) NE 0 THEN
            v-tran-type = ENTRY(INDEX("RTASEI",w-fg-rctd.rita-code),v-entrytype).
        ELSE v-tran-type = "".

        IF w-fg-rctd.po-no NE " " THEN
            FIND po-ord 
                WHERE po-ord.company EQ w-fg-rctd.company
                AND po-ord.po-no = int(w-fg-rctd.po-no)            
                NO-LOCK.
        ELSE
            RELEASE po-ord.

        ASSIGN
            v-fg-qty  = w-fg-rctd.cases * w-fg-rctd.qty-case
            v-fg-cost = w-fg-rctd.ext-cost / w-fg-rctd.t-qty * v-fg-qty
            .

        RUN calc-total.

        IF rd-Itm#Cst# = 1
            THEN ASSIGN v-cstprt = w-fg-rctd.i-no.

        /*djk: get the total quantity for the partial*/
        ASSIGN
            v-fg-qty  = w-fg-rctd.partial
            v-fg-cost = w-fg-rctd.ext-cost / w-fg-rctd.t-qty * v-fg-qty
            .

        RUN calc-partial.

        ASSIGN
            v-fg-qty  = w-fg-rctd.t-qty
            v-fg-cost = w-fg-rctd.ext-cost
            .

        RUN orig. 

        IF v-pr-tots OR v-pr-tots2  THEN 
        DO:
            DEFINE VARIABLE v-tot-value AS DECIMAL FORMAT "->>,>>>,>>9.99".
            v-tot-qty = v-tot-qty + v-fg-qty.
            v-grd-tot-qty = v-grd-tot-qty + v-fg-qty.
            v-tot-cost = v-tot-cost + v-fg-cost.
            v-grd-tot-cost  = v-grd-tot-cost  + v-tot-cost.         
            v-tot-value = v-tot-value + round(v-fg-value,2).
         
            v-grd-tot-value = v-grd-tot-value + v-fg-value.   
         
            v-msf[3] = v-msf[3] + v-msf[1].
            v-msf[4] = v-msf[4] + v-msf[2].

            IF w-fg-rctd.rita-code EQ "R" OR
                w-fg-rctd.rita-code EQ "A" OR
                w-fg-rctd.rita-code EQ "E" THEN
                v-cum-tot  = v-cum-tot + v-fg-cost.

            ELSE
                IF w-fg-rctd.rita-code EQ "S" THEN v-cum-tot  = v-cum-tot - v-fg-cost.
        END.

        IF LAST-OF(w-fg-rctd.i-no) THEN 
        DO:

      
            ASSIGN
                v-msf[5]    = v-msf[5] + v-msf[3]
                v-msf[6]    = v-msf[6] + v-msf[4]
                v-tot-qty   = 0
                v-tot-cost  = 0
                v-tot-value = 0
                v-msf[3]    = 0
                v-msf[4]    = 0
                .
        END.  /* if last-of(w-fg-rctd.i-no) */        
    END. /*for each*/
  


END PROCEDURE.
	
/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME



&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE fgPostLog C-Win 
PROCEDURE fgPostLog :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

    PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
        STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-matrix C-Win 
PROCEDURE get-matrix :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-first-disp AS LOG NO-UNDO.
    DEFINE OUTPUT PARAMETER ext-cost AS DECIMAL NO-UNDO.
    DEFINE VARIABLE v-len       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-wid       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE v-dep       LIKE po-ordl.s-len NO-UNDO. 
    DEFINE VARIABLE v-bwt       LIKE po-ordl.s-len NO-UNDO.
    DEFINE VARIABLE lv-out-qty  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE lv-out-cost AS DECIMAL NO-UNDO.

    IF NOT AVAILABLE fg-rctd THEN RETURN.  /* no records */
   
    cocode = fg-rctd.company.

    FOR EACH tt-email:
        DELETE tt-email.
    END.

    IF ip-first-disp  AND AVAILABLE fg-rctd AND fg-rctd.i-no <> "" THEN 
    DO: /* for row-display */
        FIND itemfg  WHERE itemfg.company EQ cocode                           /* no screen-value used */
            AND itemfg.i-no  EQ fg-rctd.i-no /* in browse {&browse-name}*/
            USE-INDEX i-no NO-LOCK NO-ERROR.

        FIND FIRST po-ordl WHERE po-ordl.company = fg-rctd.company
            AND po-ordl.po-no = int(fg-rctd.po-no)
            AND po-ordl.i-no  = fg-rctd.i-no
            AND po-ordl.job-no = (fg-rctd.job-no)
            AND po-ordl.job-no2 = fg-rctd.job-no2
            AND po-ordl.item-type = NO
            NO-LOCK NO-ERROR.

        IF NOT AVAILABLE po-ordl AND fg-rctd.po-no <> "" THEN RETURN.
  
        lv-out-qty = fg-rctd.t-qty . /* fg-rctd.qty-case. ??? */
        /* convert cost pr-uom*/
        RUN rm/convcuom.p(fg-rctd.cost-uom, IF AVAILABLE po-ordl THEN po-ordl.cons-uom ELSE "EA",
            0,0,0,0,fg-rctd.std-cost, OUTPUT lv-out-cost).
        ext-cost = lv-out-qty * lv-out-cost.

    END. /* avail fg-rctd */
    /* ======================================================================= */
    ELSE IF AVAILABLE fg-rctd AND fg-rctd.i-no <> "" THEN 
        DO: /* in update mode - use screen-value */
            FIND itemfg  WHERE itemfg.company EQ cocode
                AND itemfg.i-no  EQ fg-rctd.i-no
                USE-INDEX i-no NO-LOCK NO-ERROR.
            /*  if avail itemfg then v-dep = itemfg.s-dep.    */
            FIND FIRST po-ordl WHERE po-ordl.company = fg-rctd.company
                /*    and po-ordl.po-no = integer(fg-rctd.po-no in browse {&browse-name}) */
                AND po-ordl.i-no  = fg-rctd.i-no
                AND po-ordl.job-no = fg-rctd.job-no
                AND po-ordl.job-no2 = fg-rctd.job-no2
                AND po-ordl.item-type = NO
                NO-LOCK NO-ERROR.

            IF NOT AVAILABLE po-ordl AND fg-rctd.po-no <> "" THEN RETURN.
    
            lv-out-qty = fg-rctd.t-qty  .
            /* convert cost */
            IF AVAILABLE po-ordl THEN ASSIGN v-len = po-ordl.s-len
                    v-wid = po-ordl.s-wid.
            ELSE ASSIGN v-len = 0
                    v-wid = 0.
                                                                       
            RUN rm/convcuom.p( fg-rctd.cost-uom,
                IF AVAILABLE po-ordl THEN po-ordl.cons-uom ELSE "EA" ,
                0,v-len,v-wid,0,
                fg-rctd.std-cost, OUTPUT lv-out-cost).

            ext-cost = lv-out-qty * lv-out-cost.
  
        END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-ord-recs C-Win 
PROCEDURE get-ord-recs :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER ip-rowid1  AS  ROWID NO-UNDO.

    DEFINE PARAMETER BUFFER b-po-ordl FOR po-ordl.
    DEFINE PARAMETER BUFFER b-po-ord  FOR po-ord.
    DEFINE PARAMETER BUFFER b-oe-ordl FOR oe-ordl.
    DEFINE PARAMETER BUFFER b-oe-ord  FOR oe-ord.
    DEFINE PARAMETER BUFFER b-ref     FOR reftable.

    DEFINE           BUFFER b-fg-rctd FOR fg-rctd.

    RELEASE b-po-ordl.
    RELEASE b-po-ord.
    RELEASE b-oe-ordl.
    RELEASE b-oe-ord.
    RELEASE b-ref.


    FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ ip-rowid1 NO-LOCK NO-ERROR.

    IF AVAILABLE b-fg-rctd THEN 
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

        IF AVAILABLE b-po-ordl THEN
            FIND FIRST b-po-ord
                WHERE b-po-ord.company EQ b-po-ordl.company
                AND b-po-ord.po-no   EQ b-po-ordl.po-no
                AND b-po-ord.type    EQ "D"
                NO-LOCK NO-ERROR.
      
        IF AVAILABLE b-po-ord THEN
            FIND FIRST b-oe-ordl
                WHERE b-oe-ordl.company  EQ b-po-ordl.company
                AND b-oe-ordl.ord-no   EQ b-po-ordl.ord-no
                AND b-oe-ordl.i-no     EQ b-po-ordl.i-no
                AND b-oe-ordl.vend-no  EQ b-po-ord.vend-no
                AND b-oe-ordl.po-no-po EQ b-po-ord.po-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE b-oe-ordl THEN
            FIND FIRST b-oe-ord
                WHERE b-oe-ord.company EQ b-oe-ordl.company
                AND b-oe-ord.ord-no  EQ b-oe-ordl.ord-no
                NO-LOCK NO-ERROR.

        IF AVAILABLE oe-ord THEN
            FIND FIRST b-ref
                WHERE b-ref.reftable EQ "fg-rctd.user-id"
                AND b-ref.company  EQ b-fg-rctd.company
                AND b-ref.loc      EQ STRING(b-fg-rctd.r-no,"9999999999")
                NO-LOCK NO-ERROR.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE get-rel-info C-Win 
PROCEDURE get-rel-info :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER op-pono LIKE w-ord.cust-po-no NO-UNDO.
    DEFINE OUTPUT PARAMETER op-date LIKE w-ord.rel-date NO-UNDO.
    DEFINE OUTPUT PARAMETER op-lot# LIKE w-ord.rel-lot# NO-UNDO.


    RELEASE oe-rell.
    RELEASE oe-rel.

    IF v-po-no-source EQ "R" THEN 
    DO:
        FOR EACH oe-rell NO-LOCK
            WHERE oe-rell.company  EQ oe-ordl.company
            AND oe-rell.ord-no   EQ oe-ordl.ord-no
            AND oe-rell.i-no     EQ oe-ordl.i-no
            AND oe-rell.line     EQ oe-ordl.line,

            FIRST oe-relh NO-LOCK
            WHERE oe-relh.r-no     EQ oe-rell.r-no
            AND oe-relh.posted   EQ NO
            AND oe-relh.rel-date GE ldt-from
            AND oe-relh.rel-date LE ldt-to
            BY oe-relh.rel-date
            BY oe-relh.r-no:

            ASSIGN
                op-pono = oe-rell.po-no
                op-date = oe-relh.rel-date
                .
            LEAVE.
        END.

        IF AVAILABLE oe-rell THEN
            FIND FIRST oe-rel WHERE oe-rel.r-no EQ oe-rell.link-no NO-LOCK NO-ERROR.

        ELSE
            FOR EACH oe-rel NO-LOCK
                WHERE oe-rel.company  EQ oe-ordl.company
                AND oe-rel.ord-no   EQ oe-ordl.ord-no
                AND oe-rel.i-no     EQ oe-ordl.i-no
                AND oe-rel.line     EQ oe-ordl.line
                AND oe-rel.rel-no   EQ 0
                AND oe-rel.rel-date GE ldt-from
                AND oe-rel.rel-date LE ldt-to
                BY oe-rel.rel-date
                BY oe-rel.r-no:

                ASSIGN
                    op-pono = oe-rel.po-no
                    op-date = oe-rel.rel-date
                    .
                LEAVE.
            END.
    END.

    IF NOT AVAILABLE oe-rel THEN
        FOR EACH oe-rel NO-LOCK
            WHERE oe-rel.company  EQ oe-ordl.company
            AND oe-rel.ord-no   EQ oe-ordl.ord-no
            AND oe-rel.i-no     EQ oe-ordl.i-no
            AND oe-rel.line     EQ oe-ordl.line
            BY oe-rel.rel-date
            BY oe-rel.r-no:

            op-date = oe-rel.rel-date.
            LEAVE.
        END.

    IF AVAILABLE oe-rel THEN 
    DO:
        FIND FIRST ref-lot-no NO-LOCK
            WHERE ref-lot-no.reftable EQ "oe-rel.lot-no"
            AND ref-lot-no.company  EQ STRING(oe-rel.r-no,"9999999999")
            NO-ERROR.
        IF AVAILABLE ref-lot-no THEN op-lot# = ref-lot-no.code.
    END.

    IF v-po-no-source NE "R"                    OR
        (NOT AVAILABLE oe-rel AND NOT AVAILABLE oe-rell) THEN
        op-pono = IF v-po-no-source EQ "L" THEN oe-ordl.po-no
        ELSE IF AVAILABLE oe-ord THEN oe-ord.po-no
        ELSE "".

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE gl-from-work C-Win 
PROCEDURE gl-from-work :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ip-run AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ip-trnum AS INTEGER NO-UNDO.
  
    DEFINE VARIABLE credits AS DECIMAL INIT 0 NO-UNDO.
    DEFINE VARIABLE debits  AS DECIMAL INIT 0 NO-UNDO. 

  
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
            credits = credits + work-gl.credits
            .

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
                credits         = 0
                .

            RELEASE gltrans.
        END.
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE init-values C-Win 
PROCEDURE init-values :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
  


    /* Check and set FGEMAIL parameter. */
    RUN Check-Fgemail-Parm.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE manualFarmOut C-Win 
PROCEDURE manualFarmOut :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE cJob    AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iJobNo2 AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-fg-rctd FOR fg-rctd.

    FIND itemfg WHERE itemfg.company EQ cocode 
        AND itemfg.i-no EQ w-fg-rctd.i-no
        NO-LOCK NO-ERROR.
  
    IF NOT AVAILABLE itemfg THEN
        RETURN.
    cJob = "".
    iJobNo2 = 0.   
  
    IF (w-fg-rctd.job-no GT "" OR w-fg-rctd.po-no GT "") AND itemfg.pur-man THEN 
    DO:
      
        /* Find a job for this po if this is a farmout */
        IF w-fg-rctd.job-no GT "" THEN
            ASSIGN cJob    = w-fg-rctd.job-no
                iJobNo2 = w-fg-rctd.job-no2
                .
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
                    IF NOT AVAILABLE oe-ordl THEN
                        FIND FIRST oe-ordl WHERE oe-ordl.company EQ g_company
                            AND oe-ordl.ord-no EQ po-ordl.ord-no
                            AND oe-ordl.job-no EQ string(po-ordl.ord-no)
                            NO-LOCK NO-ERROR.
               
                    IF AVAILABLE oe-ordl AND oe-ordl.job-no GT "" THEN
                        ASSIGN cJob    = oe-ordl.job-no
                            iJobNo2 = oe-ordl.job-no2
                            .
                END.
          
            END.

        FIND FIRST job WHERE job.company EQ w-fg-rctd.company
            AND job.job-no EQ cJob
            AND job.job-no2 EQ iJobNo2
            NO-LOCK NO-ERROR.
      
        IF AVAILABLE job AND cJob GT "" 
            AND w-fg-rctd.rita-code EQ "F" 
            THEN 
        DO:             
          
            /* Copy fg-rctd for the jobs farmout tab */
            CREATE job-farm-rctd.
            BUFFER-COPY w-fg-rctd EXCEPT rec_key TO job-farm-rctd.
            ASSIGN 
                job-farm-rctd.job-no  = cJob
                job-farm-rctd.job-no2 = iJobNo2
                .
        
            RUN jc/updJobFarmActual.p (INPUT ROWID(job), INPUT w-fg-rctd.i-no).
        END.
        FIND bf-fg-rctd WHERE ROWID(bf-fg-rctd) EQ w-fg-rctd.row-id
            EXCLUSIVE-LOCK NO-ERROR.
        IF AVAILABLE bf-fg-rctd THEN
            DELETE bf-fg-rctd.
      
      
    END.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE orig C-Win 
PROCEDURE orig :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
   
    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.
        
    IF AVAILABLE itemfg THEN 
    DO:
        FIND FIRST uom
            WHERE uom.uom  EQ itemfg.sell-uom
            AND uom.mult NE 0
            NO-LOCK NO-ERROR.
          
        IF itemfg.sell-uom BEGINS "L" THEN
            v-fg-value = itemfg.sell-price * IF w-fg-rctd.t-qty LT 0 THEN -1 ELSE 1.

        ELSE
            IF itemfg.sell-uom EQ "CS" THEN
                v-fg-value = itemfg.sell-price * w-fg-rctd.cases.

            ELSE
                IF AVAILABLE uom THEN
                    v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / uom.mult.

                ELSE
                    v-fg-value = itemfg.sell-price * w-fg-rctd.t-qty / 1000.

        IF w-fg-rctd.rita-code EQ "R" THEN 
        DO:
            IF v-msf[1] GT w-fg-rctd.t-qty * itemfg.t-sqft THEN
                v-msf[2] = v-msf[2] + (v-msf[1] - (w-fg-rctd.t-qty * itemfg.t-sqft)).

            v-msf[1] = w-fg-rctd.t-qty * itemfg.t-sqft.
        END.
    END. /* avail itemfg */

    ASSIGN
        v-msf[1] = v-msf[1] / 1000
        v-msf[2] = v-msf[2] / 1000
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

PROCEDURE pDefaultAsiValues:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes: Copied from persist.i since code is not available without dependencies
    ------------------------------------------------------------------------------*/

    FIND FIRST usr WHERE usr.uid EQ ipcUserID NO-LOCK NO-ERROR.
    IF AVAILABLE usr THEN
    DO:
        FIND FIRST company WHERE company.company EQ ipcCompany NO-LOCK NO-ERROR.
        IF NOT AVAILABLE company THEN
            FIND FIRST company NO-LOCK NO-ERROR.

        IF AVAILABLE usr AND AVAILABLE company THEN
        DO:
            FIND FIRST loc WHERE loc.company EQ company.company 
                AND loc.loc EQ usr.loc NO-LOCK NO-ERROR.
            IF NOT AVAILABLE loc THEN
                FIND FIRST loc WHERE loc.company EQ company.company NO-LOCK NO-ERROR.
            IF AVAILABLE loc THEN
            DO:
                ASSIGN
                    g_company = company.company
                    g_loc     = loc.loc
                    g_sysdate = TODAY.

                FIND FIRST period WHERE period.company EQ g_company 
                    AND period.pstat EQ TRUE   
                    AND period.pst LE g_sysdate   
                    AND period.pend GE g_sysdate NO-LOCK NO-ERROR.
                IF NOT AVAILABLE period THEN
                    FIND LAST period WHERE period.company EQ g_company AND
                        period.pstat EQ TRUE NO-LOCK NO-ERROR.
                IF AVAILABLE period THEN
                    g_period = period.pnum.
            END.
        END. /* avail user and company */
    END. /* avail usr */



END PROCEDURE.

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE print-and-post C-Win 
PROCEDURE print-and-post :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-r-no   LIKE rm-rctd.r-no NO-UNDO.
    DEFINE VARIABLE lContinue AS LOGICAL NO-UNDO.

    FOR EACH w-fg-rctd:
        DELETE w-fg-rctd.
    END.

    FOR EACH work-gl:
        DELETE work-gl.
    END.

    FOR EACH work-job:
        DELETE work-job.
    END.
    put stream sdebug "running run-report from print and post" skip. 
    /* Populate Temp-table records */
    RUN run-report.
   
    /*    IF fgpost-cha EQ "Before" OR fgpost-cha EQ "Both" THEN RUN show-report (1).*/

    choice = CAN-FIND(FIRST w-fg-rctd WHERE w-fg-rctd.has-rec).
 put stream sdebug "after run-report choice" choice skip. 
    IF choice THEN
        FOR EACH w-fg-rctd
            WHERE w-fg-rctd.has-rec
            AND NOT CAN-FIND(FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id):
            choice = NO.
            LEAVE.
        END.
 put stream sdebug "after run-report" skip. 
    /* TBD Notify user if not choice at this point? */   
    IF choice THEN 
    DO:
        FOR EACH w-fg-rctd
            WHERE w-fg-rctd.has-rec
            AND CAN-FIND(FIRST fg-rcpth WHERE fg-rcpth.r-no EQ w-fg-rctd.r-no),
            FIRST fg-rctd
            WHERE ROWID(fg-rctd)    EQ w-fg-rctd.row-id
            AND fg-rctd.rita-code NE "P":
            lv-r-no = fg-rctd.r-no.
            DO TRANSACTION:
                fg-rctd.r-no = 0.
            END.
            DO TRANSACTION:
                fg-rctd.r-no = lv-r-no.
                /* 06121406 - If r-no is changed by write trigger, must change linked records also */
                IF fg-rctd.r-no NE lv-r-no THEN 
                DO:        
                    FOR EACH fg-rcpts 
                        WHERE fg-rcpts.company EQ fg-rctd.company 
                        AND fg-rcpts.linker EQ "fg-rctd: " + STRING(lv-r-no,"9999999999") 
                        EXCLUSIVE-LOCK:
    
                        fg-rcpts.linker = "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999").
                        FOR EACH reftable 
                            WHERE reftable.reftable EQ "fg-rctd.user-id"
                            AND reftable.company  EQ fg-rcpts.company
                            AND reftable.loc      EQ STRING(fg-rcpts.r-no,"9999999999")        /* component */
                            AND (reftable.dscr EQ "fg-rctd: " + STRING(lv-r-no, "9999999999")  /* set header r-no */
                            AND reftable.dscr BEGINS "fg-rctd: ")  
                            USE-INDEX loc   EXCLUSIVE-LOCK:
                            reftable.dscr = "fg-rctd: " + STRING(fg-rctd.r-no, "9999999999").
                        END. /* each reftable */
                    END. /* each fg-rcpts */
                END. /* If r-no was changed by trigger */
            END. /* do trans */
            w-fg-rctd.r-no = fg-rctd.r-no.
        END. /* each w-fg-rctd */
 put stream sdebug "after  choice check1 " choice skip. 
        FOR EACH w-fg-rctd WHERE w-fg-rctd.has-rec,
            FIRST fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id,
            FIRST fg-rcpth NO-LOCK WHERE fg-rcpth.r-no EQ fg-rctd.r-no:
            choice = NO.
            LEAVE.
        END.
put stream sdebug "after  choice check2 " choice skip. 
        FOR EACH w-fg-rctd WHERE w-fg-rctd.has-rec,
            FIRST fg-rctd NO-LOCK WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id:

            FIND FIRST fg-bin 
                WHERE fg-bin.company = fg-rctd.company 
                AND fg-bin.loc = fg-rctd.loc 
                AND fg-bin.i-no = ""
                AND fg-bin.loc-bin = fg-rctd.loc-bin
                NO-LOCK NO-ERROR.
            IF NOT AVAILABLE fg-bin THEN 
            DO:      
                choice = NO.
                LEAVE.
            END.
        END. /* Each w-fg-rctd */

    END. /* If choice */
put stream sdebug "after  choice check3 " choice skip. 
    IF choice THEN 
    DO: 
    
        RUN fg-post. 
    
        IF NOT ERROR-STATUS:ERROR OR tgIssue THEN 
        DO:

            
            IF fgpost-cha EQ "After" OR fgpost-cha EQ "Both" THEN RUN show-report (2).
            IF CAN-FIND(FIRST w-fg-rctd WHERE w-fg-rctd.rita-code EQ "T")
                THEN RUN ReprintTag.

        END. /* If not error-status or tgIssue */

    END. /* If choice */

/*    RUN Send-FGemail-Purchased.*/

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE Process-FGemail-Data C-Win 
PROCEDURE Process-FGemail-Data :
    /*------------------------------------------------------------------------------
      Purpose:     Create FG email temp-table record for received item.
      Parameters:  item number, qty received
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER pc-i-no  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER pi-qty   AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER pc-po-no AS CHARACTER NO-UNDO.


    DEFINE BUFFER buf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER buf-oe-ord  FOR oe-ord.
    DEFINE BUFFER bf-po-ordl  FOR po-ordl.
    DEFINE VARIABLE cUserList AS CHARACTER NO-UNDO INIT "".
    DEFINE VARIABLE xOrdNo    LIKE oe-ordl.ord-no NO-UNDO.

 
    FIND FIRST bf-po-ordl 
        WHERE bf-po-ordl.company EQ g_company
        AND bf-po-ordl.po-no EQ INT(pc-po-no)
        AND bf-po-ordl.i-no EQ pc-i-no
        AND bf-po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAILABLE bf-po-ordl THEN 
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
                tt-fgemail.Recipient = TRIM(users.image_filename)
                .
    
        END.  /*each buf-oe-ordl*/
    END. /*Avail bf-po-ordl*/
    
    RETURN.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-rel-stat C-Win 
PROCEDURE process-rel-stat :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-ordl-row AS ROWID NO-UNDO.
    DEFINE OUTPUT PARAMETER opi-stat AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opi-rel-qty AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opr-release AS ROWID NO-UNDO.

    DEFINE VARIABLE v-highest-stat AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-tot-rqty     AS INTEGER NO-UNDO.
    DEFINE VARIABLE stat-type      AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-chosen-rel   AS ROWID   NO-UNDO.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    FIND FIRST bf-oe-ordl WHERE ROWID(bf-oe-ordl) EQ ipr-ordl-row NO-LOCK NO-ERROR.

    v-highest-stat = 0.
    FOR EACH oe-rel WHERE oe-rel.company EQ bf-oe-ordl.company
        AND oe-rel.ord-no  EQ bf-oe-ordl.ord-no
        AND oe-rel.i-no    EQ bf-oe-ordl.i-no
        AND oe-rel.LINE    EQ bf-oe-ordl.LINE
        NO-LOCK.
        /* Determine the status of each to know how to proceed */
        FIND FIRST oe-rell WHERE oe-rell.company  EQ oe-rel.company
            /* This doesn't work with normal release from ou1 */
            /* AND oe-rell.r-no     EQ oe-rel.link-no */
            AND oe-rell.ord-no   EQ oe-rel.ord-no
            /* AND oe-rell.rel-no   EQ oe-rel.rel-no */
            /* AND oe-rell.b-ord-no EQ oe-rel.b-ord-no */
            AND oe-rell.i-no     EQ oe-rel.i-no
            AND oe-rell.line     EQ oe-rel.line
            /* AND oe-rell.po-no    EQ oe-rel.po-no */
            AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
            USE-INDEX ord-no NO-LOCK NO-ERROR.
        IF AVAILABLE oe-rel THEN
            v-tot-rqty = v-tot-rqty + oe-rel.qty.
             
        IF LOOKUP(oe-rel.stat, "S,I,L") GT 0 THEN
            stat-type = 1.
        ELSE 
        DO:
            IF LOOKUP(oe-rel.stat, "A,B") GT 0 AND AVAILABLE oe-rell AND NOT oe-rell.printed THEN
                stat-type = 2.
            IF LOOKUP(oe-rel.stat, "A,B,P,Z,C") GT 0 AND AVAILABLE oe-rell AND oe-rell.printed THEN
                stat-type = 3.
        END.
        IF stat-type GT v-highest-stat THEN
            ASSIGN v-highest-stat = stat-type v-chosen-rel   = ROWID(oe-rel)
                .
    END.

    ASSIGN
        opi-stat    = v-highest-stat
        opi-rel-qty = v-tot-rqty
        opr-release = v-chosen-rel
        .

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE process-releases C-Win 
PROCEDURE process-releases :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-tot-rqty     AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-rel-qty      AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-found-order  LIKE oe-ord.ord-no NO-UNDO.
    DEFINE VARIABLE v-managed      AS LOG     NO-UNDO.
    DEFINE VARIABLE stat-type      AS INTEGER NO-UNDO.
    DEFINE VARIABLE new-qty        AS INTEGER NO-UNDO.
    DEFINE VARIABLE add-qty        AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-highest-stat AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-chosen-rel   AS ROWID   NO-UNDO.
    DEFINE VARIABLE v-tot-rcv-qty  AS INTEGER NO-UNDO.

    /* To Be Implemented */
    DEFINE VARIABLE nk-set         AS LOG     NO-UNDO.

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
            IF AVAILABLE job-hdr THEN
                FIND FIRST oe-ord WHERE oe-ord.company = job-hdr.company
                    AND oe-ord.ord-no  = integer(job-hdr.job-no)
                    NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ord THEN
                FIND FIRST oe-ordl WHERE oe-ordl.company = oe-ord.company
                    AND oe-ordl.ord-no  = oe-ord.ord-no
                    AND oe-ordl.i-no    = w-fg-rctd.i-no
                    NO-LOCK NO-ERROR.

            IF AVAILABLE oe-ordl THEN
                FIND FIRST oe-rel WHERE oe-rel.company = oe-ordl.company
                    AND oe-rel.ord-no  = oe-ordl.ord-no
                    AND oe-rel.LINE    = oe-ordl.LINE
                    NO-LOCK NO-ERROR.
      
            FIND FIRST itemfg WHERE itemfg.company EQ cocode 
                AND itemfg.i-no    EQ w-fg-rctd.i-no
                NO-LOCK NO-ERROR.
            IF AVAILABLE oe-ordl THEN
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
                                /* AND oe-rell.r-no     EQ oe-rel.link-no */
                                AND oe-rell.ord-no   EQ oe-rel.ord-no
                                /* AND oe-rell.rel-no   EQ oe-rel.rel-no */
                                /* AND oe-rell.b-ord-no EQ oe-rel.b-ord-no */
                                AND oe-rell.i-no     EQ oe-rel.i-no
                                AND oe-rell.line     EQ oe-rel.line
                                /* AND oe-rell.po-no    EQ oe-rel.po-no */
                                AND oe-rell.printed  EQ NO
                                AND CAN-FIND(FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no)
                                USE-INDEX r-no NO-LOCK NO-ERROR.

                            /* Replace release qty and actual release qty */
                            RUN replace-rel-qty (INPUT ROWID(oe-rel), INPUT new-qty).
                            IF AVAILABLE oe-rell THEN
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
                    v-found-order = 0
                    .
      
            END. /* all conditions met */
        END. /* last i-no */
    END. /* each w-fg-rctd */

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-actrel-qty C-Win 
PROCEDURE replace-actrel-qty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-rell-row AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipi-qty     AS INTEGER   NO-UNDO.
    /* wfk - 11/12 - did not find any extra logic that needs to be applied 
       when oe-rell.qty is changed */
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    FIND bf-oe-rell WHERE ROWID(bf-oe-rell) EQ ipr-rell-row
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-oe-rell THEN
        RETURN.
    bf-oe-rell.qty = ipi-qty.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE replace-rel-qty C-Win 
PROCEDURE replace-rel-qty :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipr-rel-row AS ROWID NO-UNDO.
    DEFINE INPUT PARAMETER ipi-qty     AS INTEGER   NO-UNDO.
    DEFINE BUFFER bf-orig-oe-rel FOR oe-rel.
    FIND bf-orig-oe-rel WHERE ROWID(bf-orig-oe-rel) EQ ipr-rel-row
        EXCLUSIVE-LOCK NO-ERROR.
    IF NOT AVAILABLE bf-orig-oe-rel THEN
        RETURN.

    IF bf-orig-oe-rel.tot-qty EQ bf-orig-oe-rel.qty THEN 
    DO:
        bf-orig-oe-rel.tot-qty = ipi-qty.

    END.
    bf-orig-oe-rel.qty = ipi-qty.
    RUN add-rel-assign-logic (INPUT ipr-rel-row, INPUT ipi-qty).
    RELEASE bf-orig-oe-rel.

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ReprintTag C-Win 
PROCEDURE ReprintTag :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE lv-found-recs AS LOG NO-UNDO.
    lv-found-recs = NO.
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "T"
        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.trans-time
        BY w-fg-rctd.r-no:



        FIND FIRST loadtag WHERE loadtag.company     EQ cocode
            AND loadtag.item-type   EQ NO
            AND loadtag.tag-no  EQ w-fg-rctd.tag2 NO-LOCK NO-ERROR.
        IF NOT AVAILABLE loadtag THEN NEXT.
    
        RUN create-w-ord.
        lv-found-recs = YES.
    END.

    SESSION:SET-WAIT-STATE ("general").
    FIND FIRST sys-ctrl NO-LOCK 
        WHERE sys-ctrl.company EQ gcompany
        AND sys-ctrl.name    EQ "BARDIR" NO-ERROR.
    IF NOT AVAILABLE sys-ctrl THEN
    DO TRANSACTION:
        CREATE sys-ctrl.
        ASSIGN
            sys-ctrl.company = gcompany
            sys-ctrl.name    = "BARDIR"
            sys-ctrl.descrip = "C:\BA\Label\"
            .
        FIND CURRENT sys-ctrl NO-LOCK.
    END.
    v-out = sys-ctrl.descrip.

    IF v-out = "" THEN v-out = "c:~\ba~\label~\loadtag.txt".
    ELSE 
    DO:
        IF SUBSTRING(v-out,LENGTH(v-out),1) = "/" OR
            SUBSTRING(v-out,LENGTH(v-out),1) = "\" THEN .
        ELSE v-out = v-out + "/".
        v-out = v-out + "loadtag.txt".
    END.
  
    RUN create-text-file.
  
    /*    IF (NOT is-from-addons() OR SSLoadTag-log = TRUE) AND lv-found-recs THEN  */
    /*        MESSAGE "Loadtag reprint is completed." VIEW-AS ALERT-BOX INFORMATION.*/
    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE run-report C-Win 
PROCEDURE run-report PRIVATE :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    /* Form include files removed */

    DEFINE VARIABLE ext-cost        AS DECIMAL   NO-UNDO.
    DEFINE VARIABLE type            AS ch        FORMAT "X" INITIAL "R".
    DEFINE VARIABLE type-prt        AS ch        FORMAT "X(11)" INIT "".
    DEFINE VARIABLE v-fg-qty        LIKE fg-rctd.t-qty.
    DEFINE VARIABLE v-fg-cost       AS DECIMAL   FORMAT "->,>>>,>>9.99<<".
    DEFINE VARIABLE v-tot-qty       AS INTEGER   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-tot-cost      AS DECIMAL   FORMAT "->>>,>>9.99<<".
    DEFINE VARIABLE v-grd-tot-qty   AS INTEGER   FORMAT "->>>,>>>,>>9".
    DEFINE VARIABLE v-grd-tot-cost  AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".                     
    DEFINE VARIABLE v-grd-tot-value AS DECIMAL   FORMAT "->>,>>>,>>9.99<<".                     
    DEFINE VARIABLE v-tot-value     AS DECIMAL   FORMAT "->>,>>>,>>9.99".
    DEFINE VARIABLE v-cum-tot       AS de.                                   
    DEFINE VARIABLE v-tran-type     AS CHARACTER FORMAT "x(1)".      
    DEFINE VARIABLE v-entrytype     AS CHARACTER INITIAL "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
    DEFINE VARIABLE v-on            LIKE eb.num-up.
    DEFINE VARIABLE v-qty-pallet    AS DECIMAL   FORMAT "->>,>>>,>>9" NO-UNDO.
    DEFINE VARIABLE v-whse          LIKE fg-rctd.loc.            
    DEFINE VARIABLE v-one           AS INTEGER   FORMAT "->>,>>9" INIT 1.
    DEFINE VARIABLE v-ftime         AS LOGICAL   INIT NO.
    DEFINE VARIABLE v-dscr          LIKE account.dscr.
    DEFINE VARIABLE v-disp-actnum   LIKE account.actnum.
    DEFINE VARIABLE v-disp-amt      AS DECIMAL   FORMAT ">>,>>>,>>9.99cr".
    DEFINE VARIABLE v-hdr           AS CHARACTER FORMAT "x(12)".
    
    DEFINE VARIABLE ll-wip          AS LOG       NO-UNDO.
    DEFINE VARIABLE li              AS INTEGER   NO-UNDO.
    DEFINE VARIABLE li-loop         AS INTEGER   NO-UNDO.
    DEFINE VARIABLE v-time          AS CHARACTER FORMAT "X(5)" NO-UNDO.

    DEFINE VARIABLE v-itm-lbl       AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE v-itm-dsh       AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE v-desc-lbl      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-Po-lbl        AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-vend-lbl      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-desc-dsh      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-Po-dsh        AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-vend-dsh      AS CHARACTER FORMAT "x(30)" NO-UNDO.
    DEFINE VARIABLE v-uom-lbl       AS CHARACTER FORMAT "x(10)" NO-UNDO.
    DEFINE VARIABLE v-uom-dsh       AS CHARACTER FORMAT "x(10)" NO-UNDO.
    DEFINE VARIABLE v-cstprt        AS CHARACTER FORMAT "x(15)" NO-UNDO.
    DEFINE VARIABLE v-pr-tots2      LIKE v-pr-tots NO-UNDO.
/* Form statements removed here for no UI */

    {ce/msfcalc.i}

    IF LENGTH(begin_job-no) < 6 THEN
        begin_job-no = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) + TRIM(begin_job-no).
    IF LENGTH(end_job-no) < 6 THEN
        end_job-no = FILL(" ",6 - LENGTH(TRIM(end_job-no))) + TRIM(end_job-no).

    ASSIGN
        /*        v-postlst   = (IF t-receipt THEN "R," ELSE "") +*/
        /*               (IF t-setup THEN "I," ELSE "") +         */
        /*               (IF t-ship THEN "S," ELSE "") +          */
        /*               (IF t-trans THEN "T," ELSE "") +         */
        /*               (IF t-adj THEN "A," ELSE "") +           */
        /*               (IF t-ret THEN "E," ELSE "") +           */
        /*               (IF tgIssue THEN "F," ELSE "")           */
        v-cost-sell = rd_print EQ "C"
        v-pr-tots2  = tb_totCstVal
        v-pr-tots   = tb_grndtotal
        .

    IF LENGTH(v-postlst) GT 0 AND
        SUBSTR(v-postlst,LENGTH(v-postlst),1) EQ "," THEN
        SUBSTR(v-postlst,LENGTH(v-postlst),1) = "".

    EMPTY TEMP-TABLE tt-set.
    /* If not running for all items, check these items for components that must */
    /* be included                                                              */
    IF NOT (begin_i-no EQ "" AND end_i-no BEGINS "zzzzzzzzzzz")
        THEN RUN createComponentList.
put stream sdebug unformatted "in run report gcom " gcompany  " vportlist " v-postlst 
  " begin_r "  begin_fg-r-no " end-r " end_fg-r-no " endino " end_i-no " ldt-to " ldt-to " end job " end_job-no
  " end whse " end_whs " end user "end_userid
skip.  
    DO li-loop = 1 TO NUM-ENTRIES(v-postlst):
        FOR EACH fg-rctd
            WHERE fg-rctd.company   EQ gcompany
            AND fg-rctd.rita-code EQ ENTRY(li-loop,v-postlst)
            AND fg-rctd.r-no      GE begin_fg-r-no
            AND fg-rctd.r-no      LE end_fg-r-no
            AND ((fg-rctd.i-no      GE begin_i-no
            AND fg-rctd.i-no      LE end_i-no)
            OR CAN-FIND(FIRST tt-set WHERE tt-set.part-no EQ fg-rctd.i-no))
            AND fg-rctd.rct-date  GE ldt-from
            AND fg-rctd.rct-date  LE ldt-to
            AND fg-rctd.job-no    GE begin_job-no
            AND fg-rctd.job-no    LE end_job-no
            AND fg-rctd.loc-bin   NE ""
            AND fg-rctd.loc       GE begin_whs
            AND fg-rctd.loc       LE end_whs
            AND ((begin_userid    LE "" AND
            end_userid      GE "") OR
            (fg-rctd.created-by GE begin_userid 
            AND fg-rctd.created-by LE end_userid))
            USE-INDEX rita-code:
put stream sdebug "run buildtab "  skip.
            RUN build-tables.
        END.
    END.

    RUN build-comp-tables.
    /* fg-post.i is just a report */
    /*    IF v-cost-sell THEN                                                       */
    /*    DO:                                                                       */
    /*        /* TBD - include called wiht 2 different buffers */                   */
    /*        IF rd-ItmPo EQ 1 THEN                                                 */
    /*        DO:                                                                   */
    /*         /*   RUN fgposti. */                                                 */
                {fg/rep/fg-postBatch.i "itemxA" "v-fg-cost" "itempxA" "v-tot-cost"}
    /*        END.                                                                  */
    /*        ELSE                                                                  */
    /*        DO:                                                                   */
    /*/*            RUN fgposti.*/                                                  */
                {fg/rep/fg-postBatch.i "itemx" "v-fg-cost" "itempx" "v-tot-cost"}
    /*        END.                                                                  */
    /*    END.                                                                      */
    /*    ELSE                                                                      */
    /*    DO:                                                                       */
    /*        v-hdr = "       VALUE".                                               */
    /*                                                                              */
    /*        IF rd-ItmPo EQ 1 THEN                                                 */
    /*        DO:                                                                   */
             {fg/rep/fg-postBatch.i "itemyA" "v-fg-value" "itempyA" "v-tot-value"}
    /*/*            RUN fgposti.*/                                                  */
    /*        END.                                                                  */
    /*        ELSE                                                                  */
    /*        DO:                                                                   */
              {fg/rep/fg-postBatch.i "itemy" "v-fg-value" "itempy" "v-tot-value"}
    /*/*            RUN fgposti.*/                                                  */
    /*        END.                                                                  */
    /*    END.                                                                      */

    SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME




&ANALYZE-SUSPEND _UIB-CODE-BLOCK _PROCEDURE ValidateFGItemRange C-Win 
PROCEDURE ValidateFGItemRange :
    /*------------------------------------------------------------------------------
      Purpose:  Determine if the FG ITem # Range includes Set header and    
      Parameters:  oplOK -> Yes/No if ok to continue
      Notes:       This could be replaced with code that automatically includes
                   the set component if the range only includes the set header
    ------------------------------------------------------------------------------*/
    DEFINE OUTPUT PARAMETER oplOK AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-fg-set FOR fg-set.
    
    oplOK = YES.
    IF begin_i-no NE "" AND NOT end_i-no BEGINS "zzzz" AND t-receipt THEN 
    DO:
        FOR EACH bf-itemfg 
            WHERE bf-itemfg.company EQ cocode
            AND bf-itemfg.i-no GE begin_i-no
            AND bf-itemfg.i-no LE end_i-no
            AND bf-itemfg.isaset
            NO-LOCK,
            EACH bf-fg-set 
            WHERE bf-fg-set.company EQ bf-itemfg.company
            AND bf-fg-set.set-no EQ bf-itemfg.i-no
            NO-LOCK:

            IF bf-fg-set.part-no GT end_i-no OR bf-fg-set.part-no LT begin_i-no THEN 
            DO:
                oplOK = NO.
                LEAVE.
            END.
        END.
    /*        IF NOT oplOK THEN                                                                                     */
    /*            MESSAGE "FG Item # Range includes a set header but does not include all of the set's components. "*/
    /*                "Please edit range to include components."                                                    */
    /*                VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.                                                     */
    END.


END PROCEDURE.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

/* ************************  Function Implementations ***************** */

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-act-rel-qty C-Win 
FUNCTION get-act-rel-qty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li      AS INTEGER   NO-UNDO.
    DEFINE VARIABLE lv-stat AS CHARACTER NO-UNDO.
  
    IF AVAILABLE oe-ordl THEN
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-prod C-Win 
FUNCTION get-prod RETURNS INTEGER
    (  ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE li     AS INTEGER NO-UNDO.
    DEFINE VARIABLE op-bal AS INTEGER NO-UNDO.
    IF AVAILABLE oe-ordl THEN
    DO:
        IF oe-ordl.job-no NE "" THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company   EQ cocode
                AND fg-rcpth.job-no    EQ oe-ordl.job-no
                AND fg-rcpth.job-no2   EQ oe-ordl.job-no2
                AND fg-rcpth.i-no      EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ "R"
                USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                li = li + fg-rdtlh.qty.
            END.
        ELSE
        DO:
            FOR EACH job-hdr FIELDS(job-no job-no2) WHERE
                job-hdr.company EQ cocode AND
                job-hdr.ord-no EQ oe-ordl.ord-no AND
                job-hdr.i-no EQ oe-ordl.i-no
                USE-INDEX ord-no
                NO-LOCK,
                EACH fg-rcpth FIELDS(r-no rita-code) NO-LOCK
                WHERE fg-rcpth.company   EQ cocode
                AND fg-rcpth.job-no    EQ job-hdr.job-no
                AND fg-rcpth.job-no2   EQ job-hdr.job-no2
                AND fg-rcpth.i-no      EQ oe-ordl.i-no
                AND fg-rcpth.rita-code EQ "R"
                USE-INDEX job,
                EACH fg-rdtlh FIELDS(qty) NO-LOCK
                WHERE fg-rdtlh.r-no      EQ fg-rcpth.r-no
                AND fg-rdtlh.rita-code EQ fg-rcpth.rita-code:
                li = li + fg-rdtlh.qty.
            END.
        END.
     
        IF oe-ordl.po-no-po NE 0 THEN
            FOR EACH fg-rcpth FIELDS(r-no rita-code) WHERE
                fg-rcpth.company   EQ cocode AND
                fg-rcpth.po-no     EQ STRING(oe-ordl.po-no-po) AND
                fg-rcpth.i-no      EQ oe-ordl.i-no AND
                fg-rcpth.rita-code EQ "R"
                NO-LOCK,
                EACH fg-rdtlh FIELDS(qty) WHERE
                fg-rdtlh.r-no EQ fg-rcpth.r-no AND
                fg-rdtlh.rita-code EQ fg-rcpth.rita-code
                NO-LOCK:
                li = li + fg-rdtlh.qty.
            END.
    END.

    op-bal = li.
    RETURN li.   /* Function return value. */

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION get-tot-rcv-qty C-Win 
FUNCTION get-tot-rcv-qty RETURNS INTEGER
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE v-tot-qty AS INTEGER NO-UNDO.           
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

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION is-from-addons C-Win 
FUNCTION is-from-addons RETURNS LOGICAL
    ( /* parameter-definitions */ ) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE hProc     AS HANDLE NO-UNDO.
    DEFINE VARIABLE lWasFound AS LOG    NO-UNDO.
    lWasFound = NO.
    hProc = SESSION:FIRST-PROCEDURE.
    DO WHILE VALID-HANDLE(hProc):
        IF INDEX(hProc:FILE-NAME, "addon") GT 0 THEN 
        DO:
            lWasFound = YES.
            LEAVE. /* found it. */
        END.
    
        hProc = hProc:NEXT-SIBLING.
    END.

    RETURN lWasFound.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME

&ANALYZE-SUSPEND _UIB-CODE-BLOCK _FUNCTION removeChars C-Win 
FUNCTION removeChars RETURNS CHARACTER
    (ipField AS CHARACTER) :
    /*------------------------------------------------------------------------------
      Purpose:  
        Notes:  
    ------------------------------------------------------------------------------*/
    DEFINE VARIABLE invalidChars AS CHARACTER NO-UNDO INITIAL "~"".
    DEFINE VARIABLE replaceChars AS CHARACTER NO-UNDO INITIAL "'',".
    DEFINE VARIABLE i            AS INTEGER   NO-UNDO.
    DEFINE VARIABLE k            AS INTEGER   NO-UNDO.

    /*k = NUM-ENTRIES(invalidChars).
    DO i = 1 TO k: */

    ipField = REPLACE(ipField,ENTRY(1,invalidChars),ENTRY(1,replaceChars)).
    /*END.*/
    RETURN ipField.

END FUNCTION.

/* _UIB-CODE-BLOCK-END */
&ANALYZE-RESUME


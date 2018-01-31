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
&IF DEFINED(head) = 0 &THEN 
&global-define LINE inv-line
&global-define head inv-head
&global-define bolno bol-no  
&global-define multiinvoice multi-invoice
&global-define soldno sold-no
&global-define rno r-no
&global-define bno b-no
&global-define miscrno r-no
&global-define vprgmname "r-invprt."
&ENDIF
DEFINE BUFFER b-{&head}1   FOR {&head}.
DEFINE BUFFER buf-{&head}  FOR {&head}.
DEFINE BUFFER b2-{&head}   FOR {&head}.
DEFINE BUFFER buf-{&line}  FOR {&line}.
DEFINE BUFFER buf-{&line}1 FOR {&line}.

DEFINE VARIABLE fi_depts-hidden            AS LOG       NO-UNDO.
DEFINE VARIABLE tb_print-dept-screen-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE fi_depts-screen-value      AS CHARACTER NO-UNDO.
DEFINE VARIABLE hFrame-Handle              AS HANDLE    NO-UNDO.
DEFINE VARIABLE rd-dest-screen-value       AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_batchMail-checked       AS LOGICAL   NO-UNDO. 
DEFINE VARIABLE fi_broker-bol-sensitive    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE fi_broker-bol-screen-value AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_collate-hidden          AS LOGICAL   NO-UNDO.
DEFINE VARIABLE tb_HideDialog-Checked      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE tbPostedAR                 AS LOGICAL   NO-UNDO.
DEFINE VARIABLE c-win-title                AS CHARACTER NO-UNDO.
DEFINE VARIABLE lCheckHoldStat             AS LOGICAL   NO-UNDO.
DEFINE VARIABLE list-name                  AS cha       NO-UNDO.
DEFINE VARIABLE init-dir                   AS CHARACTER NO-UNDO.
DEFINE VARIABLE cActualPdf                 AS CHARACTER NO-UNDO.
DEFINE VARIABLE hCallingProc               AS HANDLE    NO-UNDO.

DEFINE VARIABLE cAccumFileList             AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAccumPdfFileList          AS CHARACTER NO-UNDO.
DEFINE VARIABLE cAccumInvNums              AS CHARACTER NO-UNDO.
DEFINE VARIABLE lSupressEmail              AS LOGICAL   NO-UNDO.
DEFINE VARIABLE iFileCount                 AS INTEGER   NO-UNDO.
DEFINE VARIABLE cInvoiceType               AS CHARACTER NO-UNDO.

{methods/defines/hndldefs.i}
/*{methods/prgsecur.i} */ 
{methods/defines/globdefs.i}
DEFINE VARIABLE v-prgmname LIKE prgrms.prgmname NO-UNDO.
{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

{oe/rep/invoice.i "new"}

DEFINE VARIABLE v-program      AS CHARACTER NO-UNDO.
DEFINE VARIABLE is-xprint-form AS LOG       NO-UNDO.
DEFINE VARIABLE ls-fax-file    AS cha       NO-UNDO.
{custom/xprint.i}

DEFINE VARIABLE lv-multi-faxout AS LOG       NO-UNDO.  /*for faxing to multiple receipents */
DEFINE VARIABLE lv-fax-image    AS cha       NO-UNDO.  /* fax imge file */
DEFINE VARIABLE lv-prt-bypass   AS LOG       NO-UNDO.  /* bypass window's printer driver */

DEFINE VARIABLE v-ftp-done      AS LOG       NO-UNDO.
DEFINE VARIABLE vcInvNums       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-pdf-file     AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcDefaultForm   AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcBOLFiles      AS CHARACTER NO-UNDO.
DEFINE VARIABLE vcBOLSignDir    AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-rec-found     AS LOG       NO-UNDO.

DEFINE VARIABLE tb_splitPDF     AS LOG       NO-UNDO.
DEFINE VARIABLE rCurrentInvoice AS ROWID     NO-UNDO.

DEFINE BUFFER save-line    FOR reftable.
DEFINE BUFFER b1-cust      FOR cust.
DEFINE BUFFER b-ar-inv     FOR ar-inv.

DEFINE BUFFER b-cust       FOR cust.
DEFINE BUFFER b-broker-bol FOR reftable.
DEFINE BUFFER bf-cust      FOR cust.

/* gdm - 12080817 */
DEFINE NEW SHARED VARIABLE nsv_setcomp      AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE s-print-zero-qty AS LOG     NO-UNDO.

/* br Task 12081002 - to pass which item to print on invoice */
DEFINE NEW SHARED VARIABLE svi-print-item   AS INTEGER INITIAL 1 NO-UNDO.

{ar/rep/invoice2.i "new"}

DO WITH TRANSACTION:
    {sys\inc\invpass.i}
END.

RUN sys/ref/nk1look.p (cocode, "BOLSign", "C", NO, NO, "", "", 
    OUTPUT vcBOLSignDir, OUTPUT v-rec-found).

DEFINE VARIABLE retcode        AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar       AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL   NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.                       

/* Build a Table to keep sequence of pdf files */
DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr  AS INTEGER
    FIELD tt-FileName AS CHARACTER
    INDEX filelist IS PRIMARY TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter  AS CHARACTER NO-UNDO.

DEFINE            VARIABLE vcBegCustNo       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE vcEndCustNo       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE vlSkipRec         AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE lReportRecCreated AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE vcHoldStats       AS CHARACTER INIT "H,W" NO-UNDO.

DEFINE            VARIABLE glPaperless       AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE tt-list
    FIELD rec-row AS ROWID.

{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLInvoice &Company=cocode} /* rstark 05181205 */
{XMLOutput/XMLOutput.i &NEW=NEW &cXMLSysCtrl=cXMLInvoice &Company=cocode &c=c} /* rstark 05291402 */

DEFINE VARIABLE vSoldToNo         AS CHARACTER NO-UNDO.  /* to hold soldto# for email */
DEFINE VARIABLE vShipToNo         AS CHARACTER NO-UNDO.  /* to hold shipto# for email */


/* Input parameters */
DEFINE VARIABLE begin_bol         AS INTEGER   FORMAT ">>>>>>>>"      .
DEFINE VARIABLE begin_cust        AS CHARACTER FORMAT "X(8)"          .
DEFINE VARIABLE begin_date        AS DATE      FORMAT "99/99/9999":U  .
DEFINE VARIABLE begin_inv         AS INTEGER   FORMAT ">>>>>>>>"      .
DEFINE VARIABLE end_bol           AS INTEGER   FORMAT ">>>>>>>9"      .
DEFINE VARIABLE end_cust          AS CHARACTER FORMAT "X(8)"          .
DEFINE VARIABLE end_date          AS DATE      FORMAT "99/99/9999":U  .
DEFINE VARIABLE end_inv           AS INTEGER   FORMAT ">>>>>>>>"      .
DEFINE VARIABLE fi_broker-bol     AS INTEGER   FORMAT ">>>>>>>>"      .
DEFINE VARIABLE fi_depts          AS CHARACTER FORMAT "X(100)"        .
DEFINE VARIABLE lbl_sort          AS CHARACTER FORMAT "X(256)":U      .
DEFINE VARIABLE lines-per-page    AS INTEGER   FORMAT ">>":U          .
DEFINE VARIABLE lv-font-name      AS CHARACTER FORMAT "X(256)":U      .
DEFINE VARIABLE lv-font-no        AS CHARACTER FORMAT "X(256)":U      .
DEFINE VARIABLE lv-scr-num-copies AS INTEGER   FORMAT ">>9":U         .
DEFINE VARIABLE lv-ornt           AS CHARACTER INITIAL "P"            .
DEFINE VARIABLE rd-dest           AS INTEGER   INITIAL 1              .
DEFINE VARIABLE rd_sort           AS CHARACTER INITIAL "BOL"          .
DEFINE VARIABLE rs_no_PN          AS INTEGER   INITIAL 1              .
DEFINE VARIABLE tb_attachBOL      AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_BatchMail      AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_collate        AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_cust-copy      AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_email-orig     AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_HideDialog     AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_office-copy    AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_override-email AS LOGICAL   INITIAL YES              .
DEFINE VARIABLE tb_posted         AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_print-dept     AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_prt-inst       AS LOGICAL   INITIAL YES              .
DEFINE VARIABLE tb_prt-zero-qty   AS LOGICAL   INITIAL YES              .
DEFINE VARIABLE tb_reprint        AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_setcomp        AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE tb_sman-copy      AS LOGICAL   INITIAL NO               .
DEFINE VARIABLE td-show-parm      AS LOGICAL   INITIAL NO               .


PROCEDURE assignSelections:
    DEFINE INPUT PARAMETER ipbegin_bol          AS INTEGER   FORMAT ">>>>>>>>"      .
    DEFINE INPUT PARAMETER ipbegin_cust         AS CHARACTER FORMAT "X(8)"          .
    DEFINE INPUT PARAMETER ipbegin_date         AS DATE      FORMAT "99/99/9999":U  .
    DEFINE INPUT PARAMETER ipbegin_inv          AS INTEGER   FORMAT ">>>>>>>>"      .
    DEFINE INPUT PARAMETER ipend_bol            AS INTEGER   FORMAT ">>>>>>>9"      .
    DEFINE INPUT PARAMETER ipend_cust           AS CHARACTER FORMAT "X(8)"          .
    DEFINE INPUT PARAMETER ipend_date           AS DATE      FORMAT "99/99/9999":U  .
    DEFINE INPUT PARAMETER ipend_inv            AS INTEGER   FORMAT ">>>>>>>>"      .
    DEFINE INPUT PARAMETER ipfi_broker-bol      AS INTEGER   FORMAT ">>>>>>>>"      .
    DEFINE INPUT PARAMETER ipfi_depts           AS CHARACTER FORMAT "X(100)"        .
    DEFINE INPUT PARAMETER iplbl_sort           AS CHARACTER FORMAT "X(256)":U      .
    DEFINE INPUT PARAMETER iplines-per-page     AS INTEGER   FORMAT ">>":U          .
    DEFINE INPUT PARAMETER iplv-font-name       AS CHARACTER FORMAT "X(256)":U      .
    DEFINE INPUT PARAMETER iplv-font-no         AS CHARACTER FORMAT "X(256)":U      .
    DEFINE INPUT PARAMETER iplv-scr-num-copies  AS INTEGER   FORMAT ">>9":U         .
    DEFINE INPUT PARAMETER iplv-ornt            AS CHARACTER INITIAL "P"            .
    DEFINE INPUT PARAMETER iprd-dest            AS INTEGER   INITIAL 1              .
    DEFINE INPUT PARAMETER iprd_sort            AS CHARACTER INITIAL "BOL"          .
    DEFINE INPUT PARAMETER iprs_no_PN           AS INTEGER   INITIAL 1              .
    DEFINE INPUT PARAMETER iptb_attachBOL       AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_BatchMail       AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_collate         AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_cust-copy       AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_email-orig      AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_HideDialog      AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_office-copy     AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_override-email  AS LOGICAL INITIAL YES              .
    DEFINE INPUT PARAMETER iptb_posted          AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_print-dept      AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_prt-inst        AS LOGICAL INITIAL YES              .
    DEFINE INPUT PARAMETER iptb_prt-zero-qty    AS LOGICAL INITIAL YES              .
    DEFINE INPUT PARAMETER iptb_reprint         AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_setcomp         AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptb_sman-copy       AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptd-show-parm       AS LOGICAL INITIAL NO               .
    DEFINE INPUT PARAMETER iptbPostedAR         AS LOGICAL INITIAL NO               .

    DEFINE INPUT PARAMETER iptbSplitPDF         AS LOGICAL INITIAL NO               .
    
    ASSIGN
        begin_bol         = ipbegin_bol        
        begin_cust        = ipbegin_cust       
        begin_date        = ipbegin_date       
        begin_inv         = ipbegin_inv        
        end_bol           = ipend_bol          
        end_cust          = ipend_cust         
        end_date          = ipend_date         
        end_inv           = ipend_inv          
        fi_broker-bol     = ipfi_broker-bol    
        fi_depts          = ipfi_depts         
        lbl_sort          = iplbl_sort         
        lines-per-page    = iplines-per-page   
        lv-font-name      = iplv-font-name     
        lv-font-no        = iplv-font-no       
        lv-scr-num-copies = iplv-scr-num-copies
        lv-ornt           = iplv-ornt          
        rd-dest           = iprd-dest          
        rd_sort           = iprd_sort          
        rs_no_PN          = iprs_no_PN         
        tb_attachBOL      = iptb_attachBOL     
        tb_BatchMail      = iptb_BatchMail     
        tb_collate        = iptb_collate       
        tb_cust-copy      = iptb_cust-copy     
        tb_email-orig     = iptb_email-orig    
        tb_HideDialog     = iptb_HideDialog    
        tb_office-copy    = iptb_office-copy   
        tb_override-email = iptb_override-email
        tb_posted         = iptb_posted        
        tb_print-dept     = iptb_print-dept    
        tb_prt-inst       = iptb_prt-inst      
        tb_prt-zero-qty   = iptb_prt-zero-qty  
        tb_reprint        = iptb_reprint       
        tb_setcomp        = iptb_setcomp       
        tb_sman-copy      = iptb_sman-copy     
        td-show-parm      = iptd-show-parm  
        tbPostedAR        = iptbPostedAR

        tb_splitPDF       = iptbSplitPDF
        s-print-zero-qty = tb_prt-zero-qty
        svi-print-item   = rs_no_PN        
        nsv_setcomp      = tb_setcomp
        .
        
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
        

END. 
/* end input parameters */
/* ********************  Preprocessor Definitions  ******************** */
&Scoped-define FRAME-NAME FRAME-A

/* ***************************  Main Block  *************************** */



/* **********************  Internal Procedures  *********************** */


PROCEDURE assignScreenValues:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipfi_depts-hidden            AS LOG       NO-UNDO.
    DEFINE INPUT PARAMETER iptb_print-dept-screen-value AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipfi_depts-screen-value      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphFrame-Handle               AS HANDLE    NO-UNDO.
    DEFINE INPUT PARAMETER iprd-dest-screen-value       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iptb_batchMail-checked       AS LOGICAL   NO-UNDO. 
    DEFINE INPUT PARAMETER ipfi_broker-bol-sensitive    AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER ipfi_broker-bol-screen-value AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iptb_collate-hidden          AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iptb_HideDialog-Checked      AS LOGICAL   NO-UNDO.  
    DEFINE INPUT PARAMETER ipc-win-title                AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplCheckHoldStat             AS LOGICAL   NO-UNDO.
    DEFINE INPUT PARAMETER iplist-name                  AS cha       NO-UNDO.
    DEFINE INPUT PARAMETER ipinit-dir                   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcActualPdf                 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipVcDefaultForm                 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipPrgmName                 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipInvoiceType                 AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iphCallingProc               AS HANDLE NO-UNDO.
    
    ASSIGN 
        fi_depts-hidden            = ipfi_depts-hidden            
        tb_print-dept-screen-value = iptb_print-dept-screen-value 
        fi_depts-screen-value      = ipfi_depts-screen-value      
        hFrame-Handle              = iphFrame-Handle               
        rd-dest-screen-value       = iprd-dest-screen-value       
        tb_batchMail-checked       = iptb_batchMail-checked       
        fi_broker-bol-sensitive    = ipfi_broker-bol-sensitive    
        fi_broker-bol-screen-value = ipfi_broker-bol-screen-value 
        tb_collate-hidden          = iptb_collate-hidden          
        tb_HideDialog-Checked      = iptb_HideDialog-Checked      
        c-win-title                = ipc-win-title                
        lCheckHoldStat             = iplCheckHoldStat             
        list-name                  = iplist-name                  
        init-dir                   = ipinit-dir                   
        cActualPdf                 = ipcActualPdf
        hCallingProc               = iphCallingProc    
        vcDefaultForm              = ipVcDefaultForm
        v-prgmname                 = ipPrgmname
        cInvoiceType               = ipInvoiceType
        rCurrentInvoice            = ? 
        .
    EMPTY TEMP-TABLE tt-list.

END PROCEDURE.

PROCEDURE BatchMail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/

    DEFINE INPUT PARAMETER icBegCustNo  AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icEndCustNo  AS CHARACTER NO-UNDO.

    DEFINE BUFFER b1-{&head}2 FOR {&head}.
    DEFINE BUFFER b2-cust      FOR cust.
    DEFINE BUFFER b1-ar-inv    FOR ar-inv.
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

    IF fi_depts-HIDDEN  = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept-SCREEN-VALUE)
            v-depts      = fi_depts-SCREEN-VALUE.
            
    DO:  /* not tb_post */
            
             
    FOR EACH b1-{&head}2 NO-LOCK
        WHERE b1-{&head}2.company         EQ cocode
        AND b1-{&head}2.cust-no         GE icBegCustNo
        AND b1-{&head}2.cust-no         LE icEndCustNo  
        AND b1-{&head}2.inv-no GE finv
        AND b1-{&head}2.inv-no LE tinv 
        
        AND (INDEX(vcHoldStats, b1-{&head}2.stat) EQ 0 OR "{&head}" EQ "ar-inv")
        AND ("{&head}" NE "ar-inv" 
        OR (b1-{&head}2.posted = tb_posted AND cInvoiceType EQ "ar-inv")
        OR ((b1-{&head}2.posted = tbPostedAR or tbPostedAR eq ?) AND cInvoiceType EQ "inv-head")
        ) 
        
       
        AND (IF "{&head}" EQ "ar-inv" THEN b1-{&head}2.inv-date GE begin_date
        AND b1-{&head}2.inv-date LE end_date ELSE TRUE)        
        AND (    (v-reprint EQ NO
                     AND (b1-{&head}2.inv-no EQ 0 
                           OR ("{&head}" EQ "ar-inv" AND b1-{&head}2.printed EQ NO)))
                  OR (v-reprint EQ YES AND b1-{&head}2.inv-no NE 0 
                     AND b1-{&head}2.inv-no        GE finv 
                     AND b1-{&head}2.inv-no        LE tinv ))
   
        ,
        FIRST b2-cust
        WHERE b2-cust.company EQ cocode
        AND b2-cust.cust-no EQ b1-{&head}2.cust-no
        AND ((b2-cust.inv-meth EQ ? AND b1-{&head}2.{&multiinvoice}) OR
        (b2-cust.inv-meth NE ? AND NOT b1-{&head}2.{&multiinvoice}) OR
        "{&head}" EQ "ar-inv"
        ) AND ( (b2-cust.log-field[1]) OR tb_override-email)
        NO-LOCK break BY b2-cust.cust-no :
                    
            
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
                        FIND FIRST {&line}  NO-LOCK WHERE {&line}.{&rno} EQ b2-{&head}.{&rno} AND {&line}.ord-no NE 0 NO-ERROR.  
                END.  
                ELSE FIND FIRST {&line}  NO-LOCK WHERE {&line}.{&rno} EQ b1-{&head}2.{&rno} AND {&line}.ord-no NE 0 NO-ERROR.
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

   /* IF NOT lEmailed AND NOT tb_override-email THEN 
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

    END.*/

END PROCEDURE.

PROCEDURE bolValidate:
    /* Run from OK button trigger */
    DEFINE OUTPUT PARAMETER oplBadStatus AS LOGICAL NO-UNDO.
    DEFINE OUTPUT PARAMETER opcInvoice AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBol AS CHARACTER NO-UNDO.
    oplBadStatus = FALSE.         
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
            ASSIGN                         
            oplBadStatus = TRUE
            opcInvoice = STRING(buf-{&head}.inv-no)
            opcBol     = STRING(buf-{&head}.{&bolno})
            .

        END.
                 
END.
     
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

PROCEDURE GenerateEmail:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.

    DO WITH FRAME {&FRAME-NAME}:

        IF vlSkipRec THEN RETURN.

        IF v-print-fmt EQ "Southpak-xl" OR v-print-fmt EQ "PrystupExcel" THEN
        DO:
            OS-DELETE VALUE(init-dir + "\Invoice").
            OS-COPY VALUE(init-dir + "\Invoice.pdf") VALUE(init-dir + "\Invoice").
            ASSIGN 
                list-name = init-dir + "\Invoice".
        END.

        lv-pdf-file = lv-pdf-file + vcInvNums + '.pdf'.

        IF is-xprint-form THEN 
        DO:

            IF cActualPDF NE lv-pdf-file AND SEARCH(cActualPDF) NE ? THEN 
            DO:
                OS-COPY VALUE(cActualPDF) VALUE(lv-pdf-file).
                OS-DELETE VALUE(cActualPDF).           
            END.

            IF tb_HideDialog-Checked THEN RUN SendMail-1 (icCustNo, 'Customer1').
            ELSE RUN SendMail-1 (icCustNo, 'Customer').
        END.
        ELSE
            IF tb_HideDialog-Checked THEN RUN SendMail-1 (icCustNo, 'Customer1').
            ELSE RUN SendMail-1 (icCustNo, 'Customer').
    END.


END PROCEDURE.

PROCEDURE output-to-fax-prt:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    DEFINE VARIABLE lv-file-name AS cha FORM "x(60)" NO-UNDO.
    DEFINE VARIABLE lv-xpr-file  AS cha FORM "x(60)" NO-UNDO.

    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        INPUT FROM OS-DIR ("C:\temp\fax") NO-ECHO.
        REPEAT:
            SET lv-file-name.
            IF lv-file-name <> "." AND lv-file-name <> ".."  AND lv-file-name MATCHES "*xpr*" 
                THEN 
            DO:
                lv-xpr-file = "c:\temp\fax\" + lv-file-name.             
                RUN printfile (lv-xpr-file).
            END.
            lv-file-name = "".   
        END.

    END.

END PROCEDURE.

PROCEDURE output-to-file:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    {custom/out2file.i}


END PROCEDURE.

PROCEDURE output-to-port:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    RUN custom/d-print.w (list-name).

END PROCEDURE.

PROCEDURE output-to-printer:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE IF lv-prt-bypass THEN 
        DO:
            RUN custom/d-print.w (list-name).
        END.
        ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).


END PROCEDURE.

PROCEDURE output-to-screen:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    IF is-xprint-form THEN 
    DO:
        FILE-INFO:FILE-NAME = list-name.
        RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE
        RUN custom/scr-rpt2.w (list-name,c-win-TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).

END PROCEDURE.

PROCEDURE runReport5:
    DEFINE INPUT PARAMETER lv-fax-type AS CHARACTER.

    IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company = cocode AND
        sys-ctrl-shipto.NAME = "INVPRINT") THEN
    DO:
       
        FOR EACH buf-{&head} WHERE
            buf-{&head}.company EQ cocode AND
            buf-{&head}.cust-no GE begin_cust AND
            buf-{&head}.cust-no LE end_cust AND
            ("{&head}" NE "ar-inv" OR buf-{&head}.posted = tb_posted) AND 
            (INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 OR "{&head}" EQ "ar-inv") AND
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
            (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice}) OR "{&head}" EQ "ar-inv")
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
                IF lReportRecCreated THEN
                  RUN GenerateReport IN hCallingProc (INPUT lv-fax-type,
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
        IF lReportRecCreated THEN
          RUN GenerateReport IN hCallingProc (INPUT lv-fax-type,
              INPUT begin_cust,
              INPUT end_cust,
              INPUT lv-fax-image).
    END.
    
END.

PROCEDURE runReport1:
    DEFINE INPUT PARAMETER lv-fax-type AS CHARACTER.
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
    IF lReportRecCreated THEN
      RUN GenerateReport IN hCallingProc (INPUT lv-fax-type,
          INPUT begin_cust,
          INPUT end_cust,
          INPUT lv-fax-image).
END PROCEDURE.

PROCEDURE output-to-mail :
    /*------------------------------------------------------------------------------
      Purpose:     
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icCustNo AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-tt-list FOR tt-list.

      
    DO WITH FRAME {&FRAME-NAME}:
                 
        IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
            sys-ctrl-shipto.company = cocode AND
            sys-ctrl-shipto.NAME = "INVPRINT") THEN
            FOR EACH buf-{&head} FIELDS(company cust-no sold-no) WHERE
                buf-{&head}.company EQ cocode AND
                ((buf-{&head}.cust-no GE begin_cust AND buf-{&head}.cust-no LE end_cust AND NOT tb_BatchMail)
                OR
                (buf-{&head}.cust-no EQ icCustNo AND tb_BatchMail)) 
                AND  INDEX(vcHoldStats, buf-{&head}.stat) EQ 0 
                AND  (((NOT tb_reprint) AND buf-{&head}.inv-no EQ 0) OR
                (tb_reprint AND buf-{&head}.inv-no NE 0 AND
                (IF "{&head}" EQ "ar-inv" THEN buf-{&head}.inv-date GE begin_date
                AND buf-{&head}.inv-date LE end_date ELSE TRUE) AND                    
                buf-{&head}.inv-no GE begin_inv AND
                buf-{&head}.inv-no LE end_inv))
                NO-LOCK,
                FIRST b-cust WHERE
                b-cust.company EQ cocode AND
                b-cust.cust-no EQ buf-{&head}.cust-no  AND
                ((b-cust.inv-meth EQ ? AND buf-{&head}.{&multiinvoice}) OR
                (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice}) OR "{&head}" EQ "ar-inv") 
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

                    /* Use tt-list to select records so that run-report can be called one invoice at a time */
                    IF NOT AVAILABLE tt-list THEN
                        RUN build-list1 (buf-{&head}.cust-no,
                            "",
                            TRUE
                            ).
                        
                    ASSIGN 
                        rCurrentInvoice   = ?
                        cAccumFileList    = ""                       
                        cAccumPDFFileList = ""
                        cAccumInvNums     = ""
                        .
                    IF tb_splitPDF THEN
                        lSupressEmail = TRUE.

                    FOR EACH bf-tt-list 
                        BREAK BY bf-tt-list.rec-row:
                          
                        /* for lSplitPDF, call run-report for a speoific invoice */
                        IF tb_splitPDF THEN
                            rCurrentInvoice = bf-tt-list.rec-row.

                        ASSIGN                            
                            vcInvNums   = ""
                            lv-pdf-file = init-dir + "\Inv"
                            .

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

                        IF LAST(bf-tt-list.rec-row) THEN
                            lSupressEmail = FALSE.
                        RUN GenerateEmail(buf-{&head}.cust-no).

                        IF NOT tb_splitPDF THEN
                            LEAVE.
                    END. /* Each bf-tt-list */

                END.
            END.

        ELSE
        DO:
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
                    b-cust.cust-no EQ buf-{&head}.cust-no  AND
                    ((b-cust.inv-meth EQ ? AND buf-{&head}.{&multiinvoice}) OR
                    (b-cust.inv-meth NE ? AND NOT buf-{&head}.{&multiinvoice}) OR "{&head}" EQ "ar-inv") 
                NO-LOCK
                BREAK BY buf-{&head}.company
                BY buf-{&head}.cust-no:
   
                IF FIRST-OF(buf-{&head}.cust-no) THEN
                DO: 
                    IF "{&head}" EQ "ar-inv" THEN
                        RUN SetInvPostForm(vcDefaultForm). 
                    ELSE                   
                        RUN SetInvForm(vcDefaultForm).
                      
                    ASSIGN
                        v-print-fmt = vcDefaultForm
                        vcInvNums   = ""
                        lv-pdf-file = init-dir + "\Inv".
                               
                    /* Use tt-list to select records so that run-report can be called one invoice at a time */
                    IF NOT AVAILABLE tt-list THEN
                        RUN build-list1 (buf-{&head}.cust-no,
                            "",
                            TRUE
                            ).
           
                    ASSIGN 
                        rCurrentInvoice   = ?
                        cAccumFileList    = ""                       
                        cAccumPDFFileList = ""
                        cAccumInvNums     = ""
                        .
                             
                    IF tb_splitPDF THEN
                        lSupressEmail = TRUE.
           
                    FOR EACH bf-tt-list 
                        BREAK BY bf-tt-list.rec-row:
                             
                        /* for lSplitPDF, call run-report for a speoific invoice */
                        IF tb_splitPDF THEN
                            rCurrentInvoice = bf-tt-list.rec-row.
                        ASSIGN                            
                            vcInvNums   = ""
                            lv-pdf-file = init-dir + "\Inv"
                            .
                        RUN run-report("","", FALSE).
                             
                             
                        /* On last one, allow email to be sent */
                        IF LAST(bf-tt-list.rec-row) THEN
                            lSupressEmail = FALSE.
                                
                        RUN GenerateEmail(icCustNo).
                             
                        IF NOT tb_splitPDF THEN
                            LEAVE.                            
                    END. /* each tt-list */
                           
                END. /* if first-of cust */
                       
            END. /* each invoice */
            
        END. /* else...no sys-ship-ctrl */
        
    END. /* DO with frame */

END PROCEDURE.

PROCEDURE build-list1:

    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sold-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.
      
    DEFINE BUFFER bf-{&line} FOR {&line}.
      
    DEFINE VARIABLE ll-consolidate AS LOG     NO-UNDO.
    DEFINE VARIABLE lv-copy#       AS INTEGER NO-UNDO.      
    DEFINE VARIABLE dtl-ctr        AS INTEGER NO-UNDO.
      
    EMPTY TEMP-TABLE tt-list.

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
        IF rd-dest-screen-value = '5' AND 
            tb_BatchMail-CHECKED THEN
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
    IF fi_depts-hidden = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept-screen-value)
            v-depts      = fi_depts-screen-value.
    FOR EACH {&head} NO-LOCK
        WHERE {&head}.company         EQ cocode
        AND {&head}.cust-no         GE fcust
        AND {&head}.cust-no         LE tcust 
        AND {&head}.inv-no GE finv
        AND {&head}.inv-no LE tinv 
        AND (STRING({&head}.sold-no)         EQ ip-sold-no OR ip-sold-no = "")
        AND (INDEX(vcHoldStats, {&head}.stat) EQ 0 OR "{&head}" EQ "ar-inv")
        AND ("{&head}" NE "ar-inv" 
        OR ({&head}.posted = tb_posted AND cInvoiceType EQ "ar-inv")
        OR ({&head}.posted = tbPostedAR AND cInvoiceType EQ "inv-head")
        ) 
        AND (IF "{&head}" EQ "ar-inv" THEN {&head}.inv-date GE begin_date
        AND {&head}.inv-date LE end_date ELSE TRUE)        
        AND (    (v-reprint EQ NO
                     AND ({&head}.inv-no EQ 0 
                           OR ("{&head}" EQ "ar-inv" AND {&head}.printed EQ NO)))
                  OR (v-reprint EQ YES AND {&head}.inv-no NE 0 
                     AND {&head}.inv-no        GE finv 
                     AND {&head}.inv-no        LE tinv )),
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
            NO-ERROR.
            
        IF AVAIL buf-{&line} THEN 
            FIND FIRST oe-boll NO-LOCK WHERE oe-boll.b-no EQ buf-{&line}.{&bno}
            AND oe-boll.bol-no GE fbol
            AND oe-boll.bol-no LE tbol 
            NO-ERROR. 
            
        IF NOT ( ({&head}.{&multiinvoice} EQ NO AND AVAILABLE(oe-boll)) OR {&head}.{&multiinvoice} )  THEN 
        DO:
            IF "{&head}" EQ "inv-head" AND AVAIL(buf-{&line}) THEN
                NEXT.            
            END.
        
        CREATE tt-list.
        tt-list.rec-row = ROWID({&head}).

        DEFINE VARIABLE ti AS INTEGER.

    END. /* for each */
END PROCEDURE.

PROCEDURE run-report :
    /* ------------------------------------------------ oe/rep/invoice.p  9/94 RM */
    /* PRINT INVOICE - O/E MODULE                                                 */
    /* -------------------------------------------------------------------------- */
    DEFINE INPUT PARAMETER ip-cust-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sold-no AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.


    DEFINE BUFFER bf-{&line} FOR {&line}.

    DEFINE VARIABLE ll-consolidate AS LOG     NO-UNDO.
    DEFINE VARIABLE lv-copy#       AS INTEGER NO-UNDO.      
    DEFINE VARIABLE dtl-ctr        AS INTEGER NO-UNDO.
    DEFINE VARIABLE iBol           AS INTEGER NO-UNDO.

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
        IF rd-dest-screen-value = '5' AND 
            tb_BatchMail-CHECKED THEN
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
    IF fi_depts-hidden = NO THEN
        ASSIGN
            v-print-dept = LOGICAL(tb_print-dept-screen-value)
            v-depts      = fi_depts-screen-value.

    /* Make sure file is unique */
    iFileCount = iFileCount + 1. 
    {sys/inc/print1.i "+ string(iFileCount)"}

    {sys/inc/outprint.i value(lines-per-page)}

    IF td-show-parm THEN RUN show-param.

    {sa/sa-sls01.i}

    v-term-id = v-term + USERID("nosweat").

    SESSION:SET-WAIT-STATE ("general").

    FIND FIRST tt-list NO-ERROR.
    
    IF NOT AVAILABLE tt-list THEN
        RUN build-list1 (
            ip-cust-no,
            ip-sold-no,
            ip-sys-ctrl-shipto 

            ).
    lReportRecCreated = NO.
    vcBOLFiles = "".
    build-report:
    FOR EACH tt-list WHERE (IF rCurrentInvoice EQ ? THEN TRUE ELSE
        tt-list.rec-row = rCurrentInvoice),
        FIRST {&head} NO-LOCK
        WHERE ROWID({&head}) EQ tt-list.rec-row,
        FIRST cust
        WHERE cust.company EQ cocode
        AND cust.cust-no EQ {&head}.cust-no
        AND ((cust.inv-meth EQ ? AND {&head}.{&multiinvoice}) OR
        (cust.inv-meth NE ? AND NOT {&head}.{&multiinvoice}) OR
        "{&head}" EQ "ar-inv"
        )
        NO-LOCK BY {&head}.{&bolno} :
/*
        FIND FIRST buf-{&line} NO-LOCK 
            WHERE buf-{&line}.{&rno} EQ {&head}.{&rno}
            AND buf-{&line}.bol-no GE fbol
            AND buf-{&line}.bol-no LE tbol 
            NO-ERROR. 
*/      IF rd-dest EQ 1 THEN
           IF cust.log-field[1] AND NOT tb_override-email THEN NEXT .

        FIND FIRST buf-{&line} NO-LOCK 
            WHERE buf-{&line}.{&rno} EQ {&head}.{&rno}
            NO-ERROR.
            
        IF AVAIL buf-{&line} THEN 
            FIND FIRST oe-boll NO-LOCK WHERE oe-boll.b-no EQ buf-{&line}.{&bno}
              AND oe-boll.bol-no GE fbol
              AND oe-boll.bol-no LE tbol 
            NO-ERROR. 
    
                              
        IF AVAIL buf-{&line} AND NOT ( ({&head}.{&multiinvoice} EQ NO AND AVAILABLE(oe-boll)) OR {&head}.{&multiinvoice} OR "{&head}" EQ "AR-INV") THEN 
        DO:
            NEXT.            
        END.
            
                
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
    
                    /*
                     {oe/rep/bolcheck.i b-{&head}1 build-report} */
                    
                    &SCOPED-DEFINE bol-check                            ~
                           IF oe-bolh.bol-no   GE fbol  AND             ~
                              oe-bolh.bol-no   LE tbol  AND             ~
                              oe-bolh.bol-date GE fdate AND             ~
                              oe-bolh.bol-date LE tdate THEN DO:        ~
                             RELEASE {&line}.                          ~
                             RELEASE oe-bolh.                           ~
                             LEAVE.                                     ~
                           END.                                         ~
                           ELSE                                         ~
                             NEXT build-report.
                    

                RELEASE oe-bolh.
                IF AVAIL buf-{&line}1 AND "{&head}" EQ "inv-head" THEN DO:
                    IF buf-{&line}1.{&bolno} EQ 0 THEN                       
                        FOR EACH oe-bolh NO-LOCK 
                            WHERE  oe-bolh.b-no EQ buf-{&line}1.{&bno}:
    
                            {&bol-check}
                        END.                    
                        ELSE
                            FOR EACH oe-bolh NO-LOCK
                                WHERE  oe-bolh.b-no  EQ buf-{&line}1.{&bno}
                                BREAK BY oe-bolh.b-no:
                                                            
                                {&bol-check}
                            END.
                END.

                iBol = 0.
                IF AVAIL buf-{&line}1 AND buf-{&line}1.bol-no GT 0 THEN
                    iBol = buf-{&line}1.bol-no.
                ELSE 
                    IF AVAIL b-{&head}1 AND b-{&head}1.{&bolno} GT 0 THEN
                        iBol = b-{&head}1.{&bolno}.
            
            
                IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(iBol) + ".pdf") NE ? 
                  AND INDEX(vcBolFiles, SEARCH(vcBOLSignDir + "\" + string(iBol) + ".pdf")) EQ 0 THEN 
                    vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(iBol) + ".pdf").

                RUN create-save-line.

                ASSIGN 
                    dtl-ctr = dtl-ctr + 1.
            END. /* Each b-{&head}1 */
            IF dtl-ctr EQ 0 THEN DO: 
                /* Make sure invoices with no inv-lines are not missed */
                FOR EACH b-{&head}1 NO-LOCK
                    WHERE b-{&head}1.company       EQ {&head}.company
                    AND b-{&head}1.cust-no       EQ {&head}.cust-no
                    AND b-{&head}1.inv-no        EQ {&head}.inv-no
                    AND b-{&head}1.{&multiinvoice} EQ NO            
                    AND INDEX(vcHoldStats, b-{&head}1.stat) EQ 0:
                    FIND FIRST inv-misc NO-LOCK 
                       WHERE inv-misc.{&rno} EQ b-{&head}1.{&rno}
                       NO-ERROR.

                    IF AVAILABLE inv-misc THEN DO:
                        dtl-ctr = dtl-ctr + 1.
                      RUN create-save-line. 
                    END.
                END.             
            END.
        END. /* If multi-invoice */

        ELSE 
        DO:
  /* Not multi-invoice */
  /* Was bolcheck.i */
                                    
  RELEASE oe-bolh.
  IF AVAIL buf-{&line} AND "{&head}" EQ "inv-head" THEN DO:
      IF buf-{&line}.bol-no EQ 0 THEN
        FOR EACH oe-bolh NO-LOCK 
            WHERE oe-bolh.b-no EQ buf-{&line}.{&bno}:
                                    
          {&bol-check}
        END.
        ELSE
          FOR EACH oe-bolh NO-LOCK
                WHERE  oe-bolh.b-no  EQ buf-{&line}.{&bno}
                BREAK BY oe-bolh.b-no:
                          
              {&bol-check}
          END.
  END.
        
            
iBol = 0.
          IF AVAIL buf-{&line} AND buf-{&line}.bol-no GT 0 THEN
    iBol = buf-{&line}.bol-no.
ELSE 
    IF {&head}.{&bolno} GT 0 THEN
        iBol = {&head}.{&bolno}.


IF tb_attachBOL AND SEARCH(vcBOLSignDir + "\" + string(iBol) + ".pdf") NE ?  THEN 
    vcBOLFiles = vcBOLFiles + "," + SEARCH(vcBOLSignDir + "\" + string(iBol) + ".pdf").

END. /* else do: not for multi-invoice */
 

/* dont include {&head} on printing when it's a MASTER invoice (multi-inv) AND 
   no SLAVE invoices found which is NOT on HOLD Status ({&head}.stat NE "H") AH 07/08/10 */
IF cust.inv-meth EQ ? 
    AND {&head}.{&multiinvoice} 
    AND dtl-ctr LE 0 
    AND NOT "{&head}" EQ "ar-inv" THEN 
DO:
    NEXT. 


END.

vlSkipRec = NO.
lReportRecCreated = YES.
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
IF NOT {&head}.printed AND "{&head}" EQ "inv-head" THEN
    FOR EACH bf-{&line}  WHERE bf-{&line}.{&rno} EQ {&head}.{&rno} NO-LOCK:
        FOR EACH oe-boll WHERE oe-boll.company EQ bf-{&line}.company
            AND oe-boll.b-no    EQ bf-{&line}.{&bno}
            AND oe-boll.ord-no  EQ bf-{&line}.ord-no
            AND oe-boll.i-no    EQ bf-{&line}.i-no
            AND oe-boll.line    EQ bf-{&line}.line
            AND oe-boll.po-no   EQ bf-{&line}.po-no
            AND CAN-FIND(FIRST oe-bolh WHERE oe-bolh.b-no   EQ bf-{&line}.{&bno}
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
    
IF fi_broker-bol-SENSITIVE = YES AND
    LOOKUP(v-print-fmt,"Capitol,APC,ALLWEST,Bell,LoyLang,PrestigeLLB,RFCX,Soule,SouleMed,SoulePO,LoylangJIT,LoylangBSF,Printers,Protagon,Protagon2") GT 0 AND
    begin_inv EQ end_inv THEN
DO:
    FIND FIRST b-broker-bol WHERE
        b-broker-bol.reftable EQ "brokerbol" AND
        b-broker-bol.CODE EQ STRING(begin_inv)
        NO-ERROR.

    IF NOT AVAILABLE b-broker-bol AND
        fi_broker-bol-screen-value NE "" THEN
    DO:
        CREATE b-broker-bol.
        ASSIGN
            b-broker-bol.reftable = "brokerbol"
            b-broker-bol.CODE     = STRING(begin_inv).
    END.

    IF AVAILABLE b-broker-bol THEN
    DO:

        b-broker-bol.code2 = fi_broker-bol-screen-value.
        RELEASE b-broker-bol.
    END.
END.

IF is-xprint-form THEN 
DO:

    IF v-print-fmt EQ "Fibrex" AND
        tb_collate-hidden EQ NO AND tb_collate THEN
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

                    IF NOT tb_BatchMail-CHECKED THEN
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
                                IF "{&head}" EQ "ar-inv" THEN
                                  PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                ELSE
                                  PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                                cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                            END.  
            END.

    END CASE.

    PUT "</PROGRESS>".
END. /* Is Xprint form */

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

/* If it's split pdf, the tt-list is in use in 'output-to-mail' */
IF NOT tb_splitPDF THEN
EMPTY TEMP-TABLE tt-list.

RUN custom/usrprint.p (v-prgmname, hFrame-Handle).

SESSION:SET-WAIT-STATE ("").

END PROCEDURE.

PROCEDURE SendMail-1:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icIdxKey   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER icRecType  AS CHARACTER NO-UNDO.

    DEFINE VARIABLE vcSubject  AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcMailBody AS CHARACTER NO-UNDO.
    DEFINE VARIABLE vcErrorMsg AS CHARACTER NO-UNDO.

    ASSIGN  
        vcSubject  = "INVOICE:" + vcInvNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
        vcSubject  = IF tb_reprint AND NOT tb_email-orig THEN '[REPRINT] ' + vcSubject ELSE vcSubject
        vcMailBody = "Please review attached Invoice(s) for Invoice #: " + vcInvNums.

    IF NOT SEARCH (list-name) = ? THEN 
    DO: 
        IF "{&head}" EQ "ar-inv" AND v-print-fmt NE "Southpak-xl" AND v-print-fmt NE "PrystupExcel"  THEN 
        DO:
            IF NOT is-xprint-form THEN 
                RUN custom/scr-rpt2.w (list-name,c-win-TITLE,int(lv-font-no),lv-ornt,lv-prt-bypass).
        END.
  
        IF NOT is-xprint-form AND NOT v-print-fmt EQ "Southpak-xl" AND NOT v-print-fmt EQ "PrystupExcel" THEN 
        DO:

            OS-RENAME VALUE (SEARCH (list-name)) VALUE (SEARCH (list-name) + '.txt').

            IF OS-ERROR NE 0 THEN 
            DO:
                MESSAGE 'Failed to rename TEMP file.' SKIP
                    'OS-ERROR : ' OS-ERROR
                    VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
            END.

            ELSE
                list-name = list-name + '.txt'.
        END.

        ELSE
            IF v-print-fmt NE "Southpak-XL" AND v-print-fmt <> "PrystupExcel" THEN DO:
                RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                  
              /* Fix for incorrect file name during batch emailing */    
              IF cActualPDF NE lv-pdf-file AND SEARCH(cActualPDF) NE ? THEN 
              DO:
                  OS-COPY VALUE(cActualPDF) VALUE(lv-pdf-file).
                  OS-DELETE VALUE(cActualPDF).           
              END.
                  
                list-name = lv-pdf-file.
            END.
            ELSE
                list-name = list-name + '.pdf'.
    /* Process attached BOL form */

    END.

    ELSE 
    DO:
        MESSAGE 'File attachment is missing.' SKIP
            'FileName: ' list-name
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN.
    END.

    IF tb_attachBOL THEN
        list-name = list-name + "," + TRIM(vcBOLfiles, ",").


    cAccumFileList = cAccumFileList + "," + list-name .
    cAccumFileList = TRIM(cAccumFileList, ",").
    cAccumPDFFileList = cAccumPDFFileList + "," + lv-pdf-file.
    cAccumPDFFileList = TRIM(cAccumPDFFileList, ",").
    cAccumInvNums = TRIM(cAccumInvNums + "," + vcInvNums, ",").

    IF tb_splitPDF THEN
        ASSIGN  
            vcSubject  = "INVOICE:" + cAccumInvNums + '   ' + STRING (TODAY, '99/99/9999') + STRING (TIME, 'HH:MM:SS AM')
            vcSubject  = IF tb_reprint AND NOT tb_email-orig THEN '[REPRINT] ' + vcSubject ELSE vcSubject
            vcMailBody = "Please review attached Invoice(s) for Invoice #: " + cAccumInvNums
            list-name  = cAccumFileList.


    IF vSoldToNo <> "" THEN 
        ASSIGN icRecType = "SoldTo"     
            icIdxKey  = icIdxKey + "|" + (vSoldToNo) +
                       IF vShipToNo <> "" THEN "|" + vShipToNo ELSE "".
    IF list-name EQ ? OR icIdxKey EQ ? OR vcSubject = ? OR vcMailBody = ? THEN 
    DO:
        MESSAGE "Error Encountered: file not found for email." 
            VIEW-AS ALERT-BOX INFORMATION BUTTONS OK.
        RETURN. 
    END.

    IF NOT lSupressEmail THEN
        RUN custom/xpmail2.p   (INPUT   icRecType,
            INPUT   'R-INVPRT.',
            INPUT   list-name,
            INPUT   icIdxKey,
            INPUT   vcSubject,
            INPUT   vcMailBody,
            OUTPUT  vcErrorMsg).
/* for email by sold-to: need type "SoldTo" and icIdxKey(cust-no) and new key
   to have sold-no */


END PROCEDURE.

PROCEDURE setBolDates:
    DEFINE INPUT PARAMETER begin_inv-screen-value AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER end_inv-screen-value   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER begin_cust-screen-value AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER end_cust-screen-value   AS CHARACTER NO-UNDO.
    
    DEFINE OUTPUT PARAMETER opdBeginDate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdEndDate AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcBeginCust AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcEndCust AS CHARACTER NO-UNDO.    
    
    ASSIGN 
        opdBeginDate = ?
        opdEndDate   = ?.
    FOR EACH {&head} NO-LOCK WHERE {&head}.company = g_company
        AND {&head}.inv-no = integer(begin_inv-screen-value)
        AND {&head}.{&multiinvoice} EQ NO 
        :
        IF AVAILABLE {&head} THEN 
            ASSIGN begin_cust-SCREEN-VALUE = {&head}.cust-no
                end_cust-SCREEN-VALUE   = {&head}.cust-no
                opcBeginCust            = begin_cust-SCREEN-VALUE
                opcEndCust              = end_cust-SCREEN-VALUE
                .
        IF "{&head}" EQ "inv-head" THEN 
        DO:
            IF AVAILABLE {&head} AND {&head}.{&bolno} NE 0 THEN
                FIND FIRST oe-bolh
                    WHERE oe-bolh.company EQ cocode
                    AND oe-bolh.{&bolno}  EQ {&head}.{&bolno}
                    NO-LOCK NO-ERROR.
        
            IF AVAILABLE oe-bolh AND opdBeginDate EQ ? THEN
                ASSIGN opdBeginDate = STRING(oe-bolh.bol-date)
                       opdEndDate   = STRING(oe-bolh.bol-date).
      
            
                FOR EACH {&line} NO-LOCK WHERE {&line}.{&rno} EQ {&head}.{&rno},
                  FIRST oe-boll NO-LOCK WHERE oe-boll.{&bno} EQ {&line}.{&bno}:
                  FIND FIRST oe-bolh NO-LOCK WHERE oe-bolh.{&bno} EQ oe-boll.{&bno} NO-ERROR.
                  IF AVAILABLE oe-bolh THEN DO:
                      IF opdBeginDate EQ ? THEN 
                          ASSIGN opdBeginDate = STRING(oe-bolh.bol-date)
                                 opdEndDate   = STRING(oe-bolh.bol-date).
                      ELSE DO:
                          IF oe-bolh.bol-date LT DATE(opdBeginDate) THEN 
                            opdBeginDate = STRING(oe-bolh.bol-date).
                          IF oe-bolh.bol-date GT DATE(opdEndDate) THEN 
                            opdEndDate = STRING(oe-bolh.bol-date). 
                      END.
                  END.
                END.            
           
        END.
        ELSE 
        DO:
            /* ar-inv case */
            IF AVAILABLE {&head} THEN 
                ASSIGN opdBeginDate = STRING({&head}.inv-date)
                    opdEndDate   = STRING({&head}.inv-date).
        END.
    END. 
END.

PROCEDURE setBOLRange:
    DEFINE INPUT PARAMETER begin_bol-screen-value    AS CHARACTER NO-UNDO.  
    DEFINE INPUT PARAMETER end_bol-screen-value      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER begin_cust-screen-value   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER end_cust-screen-value     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER begin_inv-screen-value    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER end_inv-screen-value      AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER tb_reprint-screen-value   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER begin_date-screen-value   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER end_date-screen-value     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER tb_posted-screen-value    AS CHARACTER NO-UNDO.
        
    DEFINE OUTPUT PARAMETER opbegin_cust-screen-value  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opend_cust-screen-value    AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER optb_reprint-screen-value  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER optb_posted-screen-value   AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opbegin_bol-screen-value   AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opend_bol-screen-value     AS CHARACTER NO-UNDO.
    /* bol date */  
    DEFINE OUTPUT PARAMETER opbegin_date-screen-value AS CHARACTER NO-UNDO. 
    DEFINE OUTPUT PARAMETER opend_date-screen-value AS CHARACTER NO-UNDO. 
    
    ASSIGN 
        opbegin_cust-screen-value = ?
        opend_cust-screen-value   = ?
        optb_reprint-screen-value = ?
        optb_posted-screen-value  = ? 
        opbegin_bol-screen-value  = ?
        opend_bol-screen-value    = ?
        opbegin_date-screen-value = ? 
        opend_date-screen-value   = ? 
        .

          
    DO WITH FRAME frame-a: 
        
        IF INT(begin_bol-SCREEN-VALUE) NE 0                         AND
            INT(begin_bol-SCREEN-VALUE) EQ INT(end_bol-SCREEN-VALUE) THEN 
        DO :

            /* Multi Invoice Customer must print all of their Invoices */
            FOR FIRST oe-bolh NO-LOCK
                WHERE oe-bolh.company EQ cocode
                AND oe-bolh.{&bolno}  EQ INT(begin_bol-SCREEN-VALUE):

                ASSIGN
                    opbegin_cust-SCREEN-VALUE = oe-bolh.cust-no
                    opend_cust-SCREEN-VALUE   = oe-bolh.cust-no.

                FOR EACH {&head} NO-LOCK
                    WHERE {&head}.company EQ oe-bolh.company
                    AND {&head}.cust-no EQ oe-bolh.cust-no
                    AND {&head}.inv-no GE INT(begin_inv-SCREEN-VALUE)
                    AND {&head}.inv-no LE INT(end_inv-SCREEN-VALUE)
                    AND {&head}.{&multiinvoice}              
                    AND INDEX(vcHoldStats, {&head}.stat) EQ 0:

    
                    ASSIGN
                        optb_reprint-SCREEN-VALUE = STRING({&head}.printed)
                        optb_posted-SCREEN-VALUE  = STRING({&head}.posted).

                    FOR EACH b-{&head}1 NO-LOCK
                        WHERE b-{&head}1.company EQ {&head}.company
                        AND b-{&head}1.cust-no EQ {&head}.cust-no
                        AND b-{&head}1.inv-no  EQ {&head}.inv-no
                        AND b-{&head}1.{&multiinvoice} EQ NO                
                        AND INDEX(vcHoldStats, b-{&head}1.stat) EQ 0:

                        IF b-{&head}1.{&bolno} LT INT(begin_bol-SCREEN-VALUE) THEN
                            opbegin_bol-SCREEN-VALUE = STRING(b-{&head}1.{&bolno}).

                        IF b-{&head}1.{&bolno} GT INT(end_bol-SCREEN-VALUE) THEN
                            opend_bol-SCREEN-VALUE = STRING(b-{&head}1.{&bolno}).
                    END.
                    
                    IF int(begin_bol-SCREEN-VALUE) EQ 0 THEN opbegin_bol-SCREEN-VALUE = "0".
                    IF int(end_bol-SCREEN-VALUE) EQ 0 THEN opend_bol-SCREEN-VALUE = "99999999".
                    
                END.
            END.
        END. 
        IF "{&head}" eq "inv-head" THEN DO:
            IF INTEGER(begin_bol-screen-value) GT 0 THEN DO:
                FOR EACH oe-bolh NO-LOCK
                  WHERE oe-bolh.company EQ cocode
                    AND oe-bolh.posted EQ TRUE
                    AND oe-bolh.printed EQ TRUE
                    AND oe-bolh.{&bolno}  GE INT(begin_bol-SCREEN-VALUE)
                    AND oe-bolh.{&bolno}  LE INT(end_bol-SCREEN-VALUE):
           
                   IF oe-bolh.bol-date LT DATE(begin_date-SCREEN-VALUE) THEN
                      opbegin_date-SCREEN-VALUE = STRING(oe-bolh.bol-date).
    
                   IF oe-bolh.bol-date GT DATE(end_date-screen-value) THEN
                            opend_date-SCREEN-VALUE = STRING(oe-bolh.bol-date).
                    
                END.
            END. 
            ELSE 
            DO:
                IF INT(begin_inv-screen-value) GT 0 AND INT(end_inv-screen-value) GT 0 THEN DO:
                  IF int(begin_bol-SCREEN-VALUE) EQ 0 THEN opbegin_bol-SCREEN-VALUE = "0".
                  IF int(end_bol-SCREEN-VALUE) EQ 0 THEN opend_bol-SCREEN-VALUE = "99999999".
                END.
/*                FOR EACH oe-bolh NO-LOCK                                     */
/*                    WHERE oe-bolh.company EQ cocode                          */
/*                    AND oe-bolh.posted EQ TRUE                               */
/*                    AND oe-bolh.printed EQ TRUE                              */
/*                    AND oe-bolh.{&bolno}  GE INT(begin_bol-SCREEN-VALUE)     */
/*                    AND oe-bolh.{&bolno}  LE INT(end_bol-SCREEN-VALUE):      */
/*                                                                             */
/*                    IF oe-bolh.bol-date LT DATE(begin_date-SCREEN-VALUE) THEN*/
/*                        opbegin_date-SCREEN-VALUE = STRING(oe-bolh.bol-date).*/
/*                                                                             */
/*                    IF oe-bolh.bol-date GT DATE(end_date-screen-value) THEN  */
/*                      opend_date-SCREEN-VALUE = STRING(oe-bolh.bol-date).    */
/*                                                                             */
/*                 END.                                                        */
                
            END.
            
        END.
        ELSE DO:
            IF INT(begin_inv-screen-value) GT 0 THEN DO:
              FOR EACH ar-inv NO-LOCK
                 WHERE ar-inv.company eq cocode
                   AND ar-inv.inv-no GE INTEGER(begin_inv-screen-value)
                   AND ar-inv.inv-no LE INTEGER(end_inv-screen-value)
                   :
                IF ar-inv.inv-date LT DATE(begin_date-SCREEN-VALUE) THEN
                    opbegin_date-SCREEN-VALUE = STRING(ar-inv.inv-date).
    
                IF ar-inv.inv-date GT DATE(end_date-screen-value) THEN
                    opend_date-SCREEN-VALUE = STRING(ar-inv.inv-date).
                 
              END.
                
            END.
        END.
    END.     
    
END PROCEDURE.

PROCEDURE SetInvForm:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icFormName AS CHARACTER NO-UNDO.

    is-xprint-form = NO.

    CASE icFormName:
        WHEN "Allpkg" THEN
            ASSIGN
                v-program      = "oe/rep/invallpk.p"
                lines-per-page = 60.
        WHEN "Argrov" THEN
            ASSIGN
                v-program      = "oe/rep/invargrv.p"
                lines-per-page = 66.
        WHEN "1/2 page" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 44.
        WHEN "Livngstn" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 66.
        WHEN "TriState" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 41.
        WHEN "Clev 1/2" THEN
            ASSIGN
                v-program      = "oe/rep/invhalfp.p"
                lines-per-page = 42.
        WHEN "Phoenix" THEN
            ASSIGN
                v-program      = "oe/rep/invphx.p"
                lines-per-page = 62.
        WHEN "Color" THEN
            ASSIGN
                v-program      = "oe/rep/color.p"
                lines-per-page = 60.
        WHEN "Interpac" THEN
            ASSIGN
                v-program      = "oe/rep/invinter.p"
                lines-per-page = 60.
        WHEN "Royal" THEN
            ASSIGN
                v-program      = "oe/rep/invroyal.p"
                lines-per-page = 66.
        WHEN "ContSrvc" THEN
            ASSIGN
                v-program      = "oe/rep/inv-csc.p"
                lines-per-page = 66.
        WHEN "Blueridg" THEN
            ASSIGN
                v-program      = "oe/rep/invblue.p"
                lines-per-page = 66.
        WHEN "Brick" THEN
            ASSIGN
                v-program      = "oe/rep/invbrick.p"
                lines-per-page = 59.
        WHEN "Rudd" THEN
            ASSIGN
                v-program      = "oe/rep/invrudd.p"
                lines-per-page = 66.
        WHEN "Premier" THEN
            ASSIGN
                v-program      = "oe/rep/invprem.p"
                lines-per-page = 66.
        WHEN "PremierX" THEN
            ASSIGN
                v-program      = "oe/rep/invpremx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Coburn" THEN
            ASSIGN
                v-program      = "oe/rep/invcobrn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Axis" THEN
            ASSIGN
                v-program      = "oe/rep/invaxis.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PremierS" THEN
            ASSIGN
                v-program      = "oe/rep/invprems.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColoniaX" THEN
            ASSIGN
                v-program      = "oe/rep/invcolnx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGLot" THEN
            ASSIGN
                v-program      = "oe/rep/invcccfg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGL3" THEN
            ASSIGN
                v-program      = "oe/rep/invcfgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Carded" THEN
            ASSIGN
                v-program      = "oe/rep/invcardx.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "ABC" THEN
            ASSIGN
                v-program      = "oe/rep/invabcx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "BlueRX" THEN
            ASSIGN
                v-program      = "oe/rep/invbluex.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "MultiWll" THEN
            ASSIGN
                v-program      = "oe/rep/invmulti.p"
                lines-per-page = 57.
        WHEN "PAC 1/2" THEN
            ASSIGN
                v-program      = "oe/rep/invpack.p"
                lines-per-page = 44.
        WHEN "Triad" THEN
            ASSIGN
                v-program      = "oe/rep/invtriad.p"
                lines-per-page = 62.
        WHEN "Danbury" THEN
            ASSIGN
                v-program      = "oe/rep/invdnbry.p"
                lines-per-page = 41.
        WHEN "Sonoco" THEN
            ASSIGN
                v-program      = "oe/rep/invsono.p"
                lines-per-page = 62.
        WHEN "Empire" THEN
            ASSIGN
                v-program      = "oe/rep/invempir.p"
                lines-per-page = 60.
        WHEN "Acme" THEN
            ASSIGN
                v-program      = "oe/rep/invacme.p"
                lines-per-page = 66.
        WHEN "HOP" THEN
            ASSIGN
                v-program      = "oe/rep/invhop.p"
                lines-per-page = 42.
        WHEN "MaxPak" THEN
            ASSIGN
                v-program      = "oe/rep/invmaxpk.p"
                lines-per-page = 42.
        WHEN "Fibre" THEN
            ASSIGN
                v-program      = "oe/rep/invfibre.p"
                lines-per-page = 50.
        WHEN "Abox" THEN
            ASSIGN
                v-program      = "oe/rep/invabox.p"
                lines-per-page = 60.
        WHEN "ABOX-Xp" THEN
            ASSIGN
                v-program      = "oe/rep/invxabox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Harwell" THEN
            ASSIGN
                v-program      = "oe/rep/invharwl.p"
                lines-per-page = 63.
        WHEN "P&P" THEN
            ASSIGN
                v-program      = "oe/rep/invpnp.p"
                lines-per-page = 62.
        WHEN "CorrCart" THEN
            ASSIGN
                v-program      = "oe/rep/invcorrc.p"
                lines-per-page = 62.
        WHEN "Chillic" THEN
            ASSIGN
                v-program      = "oe/rep/invchill.p"
                lines-per-page = 45.
        WHEN "Pacific" THEN
            ASSIGN
                v-program      = "oe/rep/invpacif.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Xprint" OR 
        WHEN "invprint 1" OR 
        WHEN "invprint 2" THEN
            ASSIGN
                v-program      = "oe/rep/invxprnt.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Lovepac" THEN
            ASSIGN
                v-program      = "oe/rep/invlovepac.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "invprint10-CAN" THEN
            ASSIGN
                v-program      = "oe/rep/inv10can.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN  "invprint 10" OR 
        WHEN "invprint 20" THEN
            ASSIGN
                v-program      = "oe/rep/invxprnt10.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Shamrock" THEN
            ASSIGN
                v-program      = "oe/rep/invshamrock.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Boss" THEN
            ASSIGN
                v-program      = "oe/rep/invboss.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Keystone" THEN
            ASSIGN
                v-program      = "oe/rep/invkeystone.p"  /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Fibrex" THEN
            ASSIGN
                v-program      = "oe/rep/invfibrex.p"   /*Xprint format*/
                lines-per-page = 69
                is-xprint-form = YES.
        WHEN "ImperiaX" THEN
            ASSIGN
                v-program      = "oe/rep/invximp.p"   /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ConsBox" THEN
            ASSIGN
                v-program      = "oe/rep/invconsb.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "APC" THEN
            ASSIGN
                v-program      = "oe/rep/invxapc.p"   /*APC format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Allpkgx" THEN
            ASSIGN
                v-program      = "oe/rep/invalpkx.p"   /*Allpkgx Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCIN" THEN
            ASSIGN
                v-program      = "oe/rep/invcscin.p"   /*CSCIN  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCINStamp" THEN
            ASSIGN
                v-program      = "oe/rep/invcstmp.p"   /*CSCINSTAMP  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "RUDDX" THEN
            ASSIGN
                v-program      = "oe/rep/invruddx.p"   /*Rudd Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Sonocox" THEN
            ASSIGN
                v-program      = "oe/rep/invsonox.p"   /*Sonoco Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ASIXprnt" THEN
            ASSIGN
                v-program      = "oe/rep/invxasi.p"   /*ASIXprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "midwest" THEN
            ASSIGN
                v-program      = "oe/rep/invmidws.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Accord" THEN
            ASSIGN
                v-program      = "oe/rep/invaccrd.p"
                lines-per-page = 72
                is-xprint-form = YES.
        WHEN "mwbox" THEN
            ASSIGN
                v-program      = "oe/rep/invmwbox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak" THEN
            ASSIGN
                v-program      = "oe/rep/invsthpk.p" /*Southpak format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak-xl" THEN
            ASSIGN
                v-program      = "oe/rep/invsthpk-xl.p" /*Southpak excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PrystupExcel" THEN
            ASSIGN
                v-program      = "oe/rep/invpryst-xl.p" /*PrystupExcel excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes" THEN
            ASSIGN
                v-program      = "oe/rep/invhughs.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "NStock" THEN
            ASSIGN
                v-program      = "oe/rep/invnstok.p"  /*NStock format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes2" THEN
            ASSIGN
                v-program      = "oe/rep/invhugh2.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Concepts" THEN
            ASSIGN
                v-program      = "oe/rep/invxcorc.p"  /*Corrugate Concepts format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC" THEN
            ASSIGN
                v-program      = "oe/rep/invxcsc.p"  /*Container Service format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Elite" THEN
            ASSIGN
                v-program      = "oe/rep/invelite.p"  /*Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Adapt" THEN
            ASSIGN
                v-program      = "oe/rep/invadapt.p"  /*Adapt format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC-GA" THEN
            ASSIGN
                v-program      = "oe/rep/invcscga.p"  /*CSC GA format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "CSC-GASummary" THEN
            ASSIGN
                v-program      = "oe/rep/invcscgsm.p"  /*CSC GASummary format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "ARGROVX" THEN
            ASSIGN
                v-program      = "oe/rep/invxargv.p"  /*ArgrovX format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Indiana" THEN
            ASSIGN
                v-program      = "oe/rep/invindc.p"  /*Indiana <= Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Imperial" THEN
            ASSIGN
                v-program      = "oe/rep/invimper.p"
                lines-per-page = 62.
        WHEN "RFC" OR 
        WHEN "AgMach" THEN
            ASSIGN
                v-program      = "oe/rep/invrfc.p"
                lines-per-page = IF v-print-fmt EQ "RFC" THEN 62 ELSE 66.
        WHEN "Herman" THEN
            ASSIGN
                v-program      = "oe/rep/invhermn.p"
                lines-per-page = 60.
        WHEN "Century" THEN
            ASSIGN
                v-program      = "oe/rep/invcntry.p"
                lines-per-page = 60.
        WHEN "CENTBOX" THEN
            ASSIGN
                v-program      = "oe/rep/invcentx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Oracle" THEN
            ASSIGN
                v-program      = "oe/rep/invoracl.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakes" THEN
            ASSIGN
                v-program      = "oe/rep/invtri.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakesBroker" THEN
            ASSIGN
                v-program      = "oe/rep/invtribrk.p"
                lines-per-page = 66
                is-xprint-form = YES.   /* TriLakesBroker */
        WHEN "frankstn" OR 
        WHEN "Mirpkg" THEN
            ASSIGN
                v-program      = "oe/rep/invfrank.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "DEE" THEN
            ASSIGN
                v-program      = "oe/rep/invdee.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PPI" THEN
            ASSIGN
                v-program      = "oe/rep/invppi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Dayton" THEN
            ASSIGN
                v-program      = "oe/rep/invdaytn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Unipak" THEN
            ASSIGN
                v-program      = "oe/rep/invunipk.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Colonial" THEN
            ASSIGN
                v-program      = "oe/rep/invasi.p"
                lines-per-page = 60.
        WHEN "HPB" THEN
            ASSIGN
                v-program      = "oe/rep/invhpb.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Nosco" THEN
            ASSIGN
                v-program      = "oe/rep/invknight.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Nosco1" THEN
            ASSIGN
                v-program      = "oe/rep/invknight1.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Central" THEN                                  /*task# 12041303*/
            ASSIGN
                v-program      = "oe/rep/invcentral.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpakl" THEN
            ASSIGN
                v-program      = "oe/rep/invsthpklg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Androp" THEN
            ASSIGN
                v-program      = "oe/rep/invandrop.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Packrite" THEN
            ASSIGN
                v-program      = "oe/rep/invpkrt.p"  
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Rosmar" THEN
            ASSIGN
                v-program      = "oe/rep/invrosmr.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "Badger" THEN
            ASSIGN
                v-program      = "oe/rep/invbadger.p"   
                lines-per-page = 66
                is-xprint-form = YES.


        WHEN "Badger-Emailed" THEN
            ASSIGN
                v-program      = "oe/rep/invbadgereml.p"   
                lines-per-page = 66
                is-xprint-form = YES.

        /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
        WHEN "capitol" THEN
            ASSIGN
                v-program      = "oe/rep/invcapitol.p"
                lines-per-page = 71
                is-xprint-form = YES.

        WHEN "allwest" THEN
            ASSIGN
                v-program      = "oe/rep/invallws.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Bell" THEN
            ASSIGN
                v-program      = "oe/rep/invbell.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Simkins" THEN
            ASSIGN
                v-program      = "oe/rep/invsmkct.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CapCityIn" THEN 
            ASSIGN
                v-program      = "oe/rep/invcapcin.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ACPI" THEN
            ASSIGN
                v-program      = "oe/rep/invacpi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColorX" THEN
            ASSIGN
                v-program      = "oe/rep/invcolrx.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "Loylang" THEN /* LOYLANG gmd 11200902*/
            ASSIGN
                v-program      = "oe/rep/invloyln.p"
                lines-per-page = 78
                is-xprint-form = YES.
        WHEN "PrestigeLLB" THEN /* Task# 08271402*/
            ASSIGN
                v-program      = "oe/rep/invprstl.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "RFCX" THEN /*Task# 11061302*/
            ASSIGN
                v-program      = "oe/rep/invrfcx.p"
                lines-per-page = 71
                is-xprint-form = YES.  
        WHEN "LoylangBSF" THEN /* LoylangBSF - same report, just different price UOM*/
            ASSIGN
                v-program      = "oe/rep/invloyln.p"
                lines-per-page = 78
                is-xprint-form = YES.
        WHEN "Protagon" THEN /* Copied from LoyLangBSF*/
            ASSIGN
                v-program      = "oe/rep/invprot.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Protagon2" THEN /* Copied from Protagon */
            ASSIGN
                v-program      = "oe/rep/invprot2.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Soule" THEN       /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invsoule.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SouleMed" THEN   /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invsoulemed.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SoulePO" THEN   /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invsoulepo.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Printers" THEN       /* LOYLANG Format */
            ASSIGN
                v-program      = "oe/rep/invprnts.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "LoylangJIT" THEN
            ASSIGN
                v-program      = "oe/rep/invloyjit.p"
                lines-per-page = 76
                is-xprint-form = YES.

        WHEN "ColonialLot#" THEN
            ASSIGN
                v-program      = "oe/rep/invcolnx2.p"
                lines-per-page = 71   /* Task 10181309 */
                is-xprint-form = YES.
        WHEN "Peachtreefgl3" THEN
            ASSIGN
                v-program      = "oe/rep/invptreefgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Peachtree" THEN
            ASSIGN
                v-program      = "oe/rep/invptreelot.p"
                lines-per-page = 66
                is-xprint-form = YES.
        OTHERWISE
        ASSIGN
            v-program      = "oe/rep/invasi.p"
            lines-per-page = 55.

    END CASE.

END PROCEDURE.

PROCEDURE SetInvPostForm:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER icFormName AS CHARACTER NO-UNDO.

    ASSIGN
        lv-prt-bypass  = NO
        is-xprint-form = NO.
        
    CASE icFormName:
        WHEN "Allpkg" THEN
            ASSIGN
                v-program      = "ar/rep/invallpk.p"
                lines-per-page = 62.
        WHEN "1/2 page" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 44.
        WHEN "Livngstn" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 66.
        WHEN "TriState" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 41.
        WHEN "Clev 1/2" THEN
            ASSIGN
                v-program      = "ar/rep/invhalfp.p"
                lines-per-page = 42.
        WHEN "Phoenix" THEN
            ASSIGN
                v-program      = "ar/rep/invphx.p"
                lines-per-page = 62.
        WHEN "Color" THEN
            ASSIGN
                v-program      = "ar/rep/color.p"
                lines-per-page = 60.
        WHEN "Interpac" THEN
            ASSIGN
                v-program      = "ar/rep/invinter.p"
                lines-per-page = 60.
        WHEN "Brick" THEN
            ASSIGN
                v-program      = "ar/rep/invbrick.p"
                lines-per-page = 62.
        WHEN "Rudd" THEN
            ASSIGN
                v-program      = "ar/rep/invrudd.p"
                lines-per-page = 66.
        WHEN "Premier" THEN
            ASSIGN
                v-program      = "ar/rep/invprem.p"
                lines-per-page = 66.
        WHEN "PremierX" THEN
            ASSIGN
                v-program      = "ar/rep/invpremx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Coburn" THEN
            ASSIGN
                v-program      = "ar/rep/invcobrn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Axis" THEN
            ASSIGN
                v-program      = "ar/rep/invaxis.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PremierS" THEN
            ASSIGN
                v-program      = "ar/rep/invprems.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColoniaX" THEN
            ASSIGN
                v-program      = "ar/rep/invcolnx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGLot" THEN
            ASSIGN
                v-program      = "ar/rep/invcccfg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CCCFGL3" THEN
            ASSIGN
                v-program      = "ar/rep/invcfgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Carded" THEN
            ASSIGN
                v-program      = "ar/rep/invcardx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ABC" THEN
            ASSIGN
                v-program      = "ar/rep/invabcx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "BlueRX" THEN
            ASSIGN
                v-program      = "ar/rep/invbluex.p"
                lines-per-page = 66
                is-xprint-form = YES.

        WHEN "PAC 1/2" THEN
            ASSIGN
                v-program      = "ar/rep/invpack.p"
                lines-per-page = 44.
        WHEN "Triad" THEN
            ASSIGN
                v-program      = "ar/rep/invtriad.p"
                lines-per-page = 52.
        WHEN "Danbury" THEN
            ASSIGN
                v-program      = "ar/rep/invdnbry.p"
                lines-per-page = 41.
        WHEN "Sonoco" THEN
            ASSIGN
                v-program      = "ar/rep/invsono.p"
                lines-per-page = 62.
        WHEN "Empire" THEN
            ASSIGN
                v-program      = "ar/rep/invempir.p"
                lines-per-page = 62.
        WHEN "HOP" THEN
            ASSIGN
                v-program      = "ar/rep/invhop.p"
                lines-per-page = 42.
        WHEN "MaxPak" THEN
            ASSIGN
                v-program      = "ar/rep/invmaxpk.p"
                lines-per-page = 42.
        WHEN "Fibre" THEN
            ASSIGN
                v-program      = "ar/rep/invfibre.p"
                lines-per-page = 50.
        WHEN "Abox" THEN
            ASSIGN
                v-program      = "ar/rep/invabox.p"
                lines-per-page = 60.
        WHEN "ABOX-Xp" THEN
            ASSIGN
                v-program      = "ar/rep/invxabox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Harwell" THEN
            ASSIGN
                v-program      = "ar/rep/invharwl.p"
                lines-per-page = 63.
        WHEN "Chillic" THEN
            ASSIGN
                v-program      = "ar/rep/invchill.p"
                lines-per-page = 45.
        WHEN "Pacific" THEN
            ASSIGN
                v-program      = "ar/rep/invpacif.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Xprint" OR 
        WHEN "invprint 1" OR 
        WHEN "invprint 2" THEN
            ASSIGN
                v-program      = "ar/rep/invxprnt.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "lovepac" THEN
            ASSIGN
                v-program      = "ar/rep/invlovepac.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "invprint10-CAN" THEN
            ASSIGN
                v-program      = "ar/rep/inv10can.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "invprint 10" OR 
        WHEN "invprint 20" THEN
            ASSIGN
                v-program      = "ar/rep/invxprnt10.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Shamrock" THEN
            ASSIGN
                v-program      = "ar/rep/invshamrock.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Boss" THEN
            ASSIGN
                v-program      = "ar/rep/invboss.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Keystone" THEN
            ASSIGN
                v-program      = "ar/rep/invkeystone.p"  /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Fibrex" THEN
            ASSIGN
                v-program      = "ar/rep/invfibrex.p"   /*Xprint format*/
                lines-per-page = 69
                is-xprint-form = YES.
        WHEN "ImperiaX" THEN
            ASSIGN
                v-program      = "ar/rep/invximp.p"   /*Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ConsBox" THEN
            ASSIGN
                v-program      = "ar/rep/invconsb.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "APC" THEN
            ASSIGN
                v-program      = "ar/rep/invxapc.p"   /*APC format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSCIN" THEN
            ASSIGN
                v-program      = "ar/rep/invcscin.p"   /*CSCIN  format*/
                lines-per-page = 64
                is-xprint-form = YES.
        WHEN "CSCINStamp" THEN
            ASSIGN
                v-program      = "ar/rep/invcstmp.p"   /*CSCINSTAMP  format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "RUDDX" THEN
            ASSIGN
                v-program      = "ar/rep/invruddx.p"   /*Rudd Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Sonocox" THEN
            ASSIGN
                v-program      = "ar/rep/invsonox.p"   /*Sonoco Xprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ASIXprnt" THEN
            ASSIGN
                v-program      = "ar/rep/invxasi.p"   /*ASIXprint format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "midwest" THEN
            ASSIGN
                v-program      = "ar/rep/invmidws.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Accord" THEN
            ASSIGN
                v-program      = "ar/rep/invaccrd.p"
                lines-per-page = 72
                is-xprint-form = YES.
        WHEN "mwbox" THEN
            ASSIGN
                v-program      = "ar/rep/invmwbox.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak" THEN
            ASSIGN
                v-program      = "ar/rep/invsthpk.p" /*Southpak format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpak-xl" THEN
            ASSIGN
                v-program      = "ar/rep/invsthpk-xl.p" /*Southpak excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PrystupExcel" THEN
            ASSIGN
                v-program      = "ar/rep/invpryst-xl.p" /*PrystupExcel excel format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes" THEN
            ASSIGN
                v-program      = "ar/rep/invhughs.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "NStock" THEN
            ASSIGN
                v-program      = "ar/rep/invnstok.p"  /*NStock format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Hughes2" THEN
            ASSIGN
                v-program      = "ar/rep/invhugh2.p"  /*Hughes format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Concepts" THEN
            ASSIGN
                v-program      = "ar/rep/invxcorc.p"  /*Corrugate Concepts format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC" THEN
            ASSIGN
                v-program      = "ar/rep/invxcsc.p"  /*Container Service format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Elite" THEN
            ASSIGN
                v-program      = "ar/rep/invelite.p"  /*Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Adapt" THEN
            ASSIGN
                v-program      = "ar/rep/invadapt.p"  /*Adapt format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CSC-GA" THEN
            ASSIGN
                v-program      = "ar/rep/invcscga.p"  /*CSC-GA format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "CSC-GASummary" THEN
            ASSIGN
                v-program      = "ar/rep/invcscgsm.p"  /*CSC-GASummary format*/
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "ARGROVX" THEN
            ASSIGN
                v-program      = "ar/rep/invxargv.p"  /*ArgrovX format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Indiana" THEN
            ASSIGN
                v-program      = "ar/rep/invindc.p"  /*Indiana <= Elite format*/
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Imperial" THEN
            ASSIGN
                v-program      = "ar/rep/invimper.p"
                lines-per-page = 62.
        WHEN "RFC" OR 
        WHEN "AgMach" THEN
            ASSIGN
                v-program      = "ar/rep/invrfc.p"
                lines-per-page = IF v-print-fmt EQ "RFC" THEN 62 ELSE 66.
        WHEN "Herman" THEN
            ASSIGN
                v-program      = "ar/rep/invhermn.p"
                lines-per-page = 62.
        WHEN "CENTBOX" THEN
            ASSIGN
                v-program      = "ar/rep/invcentx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Oracle" THEN
            ASSIGN
                v-program      = "ar/rep/invoracl.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakes" THEN
            ASSIGN
                v-program      = "ar/rep/invtri.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "TriLakesBroker" THEN
            ASSIGN
                v-program      = "ar/rep/invtribrk.p"
                lines-per-page = 66
                is-xprint-form = YES.      /*  TriLakesBroker  */
        WHEN "frankstn" OR 
        WHEN "Mirpkg" THEN
            ASSIGN
                v-program      = "ar/rep/invfrank.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "DEE" THEN
            ASSIGN
                v-program      = "ar/rep/invdee.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "PPI" THEN
            ASSIGN
                v-program      = "ar/rep/invppi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Dayton" THEN
            ASSIGN
                v-program      = "ar/rep/invdaytn.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Unipak" THEN
            ASSIGN
                v-program      = "ar/rep/invunipk.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "HPB" THEN
            ASSIGN
                v-program      = "ar/rep/invhpb.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Allpkgx" THEN
            ASSIGN
                v-program      = "ar/rep/invalpkx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ILWALKER" THEN
            ASSIGN
                v-program      = "oe/rep/invilwalkp.p"
                lines-per-page = 55.
        WHEN "Nosco" THEN
            ASSIGN
                v-program      = "ar/rep/invknight.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Southpakl" THEN
            ASSIGN
                v-program      = "ar/rep/invsthpklg.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Androp" THEN
            ASSIGN
                v-program      = "ar/rep/invandrop.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Nosco1" THEN
            ASSIGN
                v-program      = "ar/rep/invknight1.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Central" THEN                                  /*task# 12041303*/
            ASSIGN
                v-program      = "ar/rep/invcentral.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Packrite" THEN
            ASSIGN
                v-program      = "ar/rep/invpkrt.p"  
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Rosmar" THEN
            ASSIGN
                v-program      = "ar/rep/invrosmr.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Badger" THEN
            ASSIGN
                v-program      = "ar/rep/invbadger.p"   
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Badger-Emailed" THEN
            ASSIGN
                v-program      = "ar/rep/invbadgereml.p"   
                lines-per-page = 66
                is-xprint-form = YES.

        /* 20130718 JAD Task 02071304 Add Capitol to v-print-fmt */
        WHEN "capitol" THEN
            ASSIGN
                v-program      = "ar/rep/invcapitol.p"
                lines-per-page = 71
                is-xprint-form = YES.

        WHEN "allwest" THEN
            ASSIGN
                v-program      = "ar/rep/invallws.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Bell" THEN
            ASSIGN
                v-program      = "ar/rep/invbell.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "Simkins" THEN
            ASSIGN
                v-program      = "ar/rep/invsmkct.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "CapCityIn" THEN 
            ASSIGN
                v-program      = "ar/rep/invcapcin.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ACPI" THEN
            ASSIGN
                v-program      = "ar/rep/invacpi.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "ColorX" THEN
            ASSIGN
                v-program      = "ar/rep/invcolrx.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "loylang" THEN /* LOYLANG gmd 11200902 */
            ASSIGN
                v-program      = "ar/rep/invloyln.p"
                lines-per-page = 71           
                is-xprint-form = YES.
        WHEN "PrestigeLLB" THEN /* Task# 08271402*/
            ASSIGN
                v-program      = "ar/rep/invprstl.p"
                lines-per-page = 71
                is-xprint-form = YES.
        WHEN "RFCX" THEN /*Task# 11061302*/
            ASSIGN
                v-program      = "ar/rep/invrfcx.p"
                lines-per-page = 71             
                is-xprint-form = YES.  
        WHEN "LoylangBSF" THEN /* small mod to Loylang with Price/BSF instead of price */
            ASSIGN
                v-program      = "ar/rep/invloyln.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Protagon" THEN /* Copied form LoyLangBSF */
            ASSIGN
                v-program      = "ar/rep/invprot.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Protagon2" THEN /* Copied from Protagon */
            ASSIGN
                v-program      = "ar/rep/invprot2.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Soule" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invsoule.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SouleMed" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invsoulemed.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "SoulePO" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invsoulepo.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "Printers" THEN /* LOYLANG Format */
            ASSIGN
                v-program      = "ar/rep/invprnts.p"
                lines-per-page = 71             
                is-xprint-form = YES.
        WHEN "loylangjit" THEN
            ASSIGN
                v-program      = "ar/rep/invloyjit.p"
                lines-per-page = 76
                is-xprint-form = YES.
        WHEN "ColonialLot#" THEN
            ASSIGN
                v-program      = "ar/rep/invcolnx2.p"
                lines-per-page = 71     /* Task 10181309   */
                is-xprint-form = YES.
        WHEN "Peachtreefgl3" THEN
            ASSIGN
                v-program      = "ar/rep/invptreefgl3.p"
                lines-per-page = 66
                is-xprint-form = YES.
        WHEN "Peachtree" THEN
            ASSIGN
                v-program      = "ar/rep/invptreelot.p"
                lines-per-page = 66
                is-xprint-form = YES.
        OTHERWISE
        ASSIGN
            v-program      = "ar/rep/invasi.p"
            lines-per-page = 66.
          
    END CASE.
    IF icFormName = "BOXTECH" THEN
        lv-prt-bypass = YES.


END PROCEDURE.

PROCEDURE validateCustPaper:
    
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
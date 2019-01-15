

/*------------------------------------------------------------------------
    File        : FgPostReport.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Aug 15 18:32:59 EDT 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd
    FIELD row-id      AS ROWID
    FIELD has-rec     AS LOG   INIT NO
    FIELD invoiced    AS LOG   INIT NO
    FIELD old-tag     AS CHAR
    FIELD ret-loc     AS CHAR
    FIELD ret-loc-bin AS CHAR    
    .

DEF INPUT PARAMETER ip-post-eom-date AS DATE NO-UNDO.
DEF INPUT PARAMETER ip-run-what AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER begin_fg-r-no  AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 0. 
DEFINE INPUT PARAMETER begin_i-no     AS CHARACTER FORMAT "X(15)":U. 
DEFINE INPUT PARAMETER begin_job-no   AS CHARACTER FORMAT "X(6)":U. 
DEFINE INPUT PARAMETER begin_userid   AS CHARACTER FORMAT "X(8)":U. 
DEFINE INPUT PARAMETER begin_whs      AS CHARACTER FORMAT "X(5)":U. 
DEFINE INPUT PARAMETER end_fg-r-no    AS INTEGER   FORMAT ">>>>>>>>":U INITIAL 99999999. 
DEFINE INPUT PARAMETER end_i-no       AS CHARACTER FORMAT "X(15)":U INITIAL "zzzzzzzzzzzzzzz". 
DEFINE INPUT PARAMETER end_job-no     AS CHARACTER FORMAT "X(6)":U INITIAL "zzzzzz" .
DEFINE INPUT PARAMETER end_userid     AS CHARACTER FORMAT "X(8)":U INITIAL "zzzzzzzz". 
DEFINE INPUT PARAMETER end_whs        AS CHARACTER FORMAT "X(5)":U INITIAL "zzzzz". 
DEFINE INPUT PARAMETER fi_file        AS CHARACTER FORMAT "X(30)" INITIAL "c:~\tmp~\r-fgpstall.csv". 
DEFINE INPUT PARAMETER ldt-from       AS DATE      FORMAT "99/99/9999":U INITIAL 01/01/001 .
DEFINE INPUT PARAMETER ldt-to         AS DATE      FORMAT "99/99/9999":U INITIAL 12/31/9999 .
DEFINE INPUT PARAMETER lines-per-page AS INTEGER   FORMAT ">>":U INITIAL 99 .
DEFINE INPUT PARAMETER lv-font-name   AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)". 
DEFINE INPUT PARAMETER lv-font-no     AS CHARACTER FORMAT "X(256)":U INITIAL "11" .
DEFINE INPUT PARAMETER v-post-date    AS DATE      FORMAT "99/99/9999":U .
DEFINE INPUT PARAMETER v-trans-lbl    AS CHARACTER FORMAT "X(256)":U INITIAL "Transaction Types". 
DEFINE INPUT PARAMETER lv-ornt        AS CHARACTER INITIAL "P" .
DEFINE INPUT PARAMETER rd-dest        AS INTEGER   INITIAL 2 .
DEFINE INPUT PARAMETER rd-Itm#Cst#    AS INTEGER .
DEFINE INPUT PARAMETER rd-ItmPo       AS INTEGER .
DEFINE INPUT PARAMETER rd-UOMJob      AS INTEGER .
DEFINE INPUT PARAMETER rd_print       AS CHARACTER. 
DEFINE INPUT PARAMETER t-adj          AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER t-receipt      AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER t-ret          AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER t-ship         AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER t-trans        AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER t-setup        AS LOGICAL   INITIAL NO.
DEFINE INPUT PARAMETER tb_excel       AS LOGICAL   INITIAL YES. 
DEFINE INPUT PARAMETER tb_glnum       AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER tb_grndtotal   AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER tb_runExcel    AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER tb_totCstVal   AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER td-show-parm   AS LOGICAL   INITIAL YES. 
DEFINE INPUT PARAMETER tg-recalc-cost AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER tgIssue        AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER tgl-itemCD     AS LOGICAL   INITIAL NO. 
DEFINE INPUT PARAMETER TABLE FOR w-fg-rctd. 
DEFINE OUTPUT PARAMETER lv-list-name AS CHARACTER EXTENT 2 NO-UNDO. 
/* Local Variable Definitions ---                                       */
DEF    VAR      list-name AS cha       NO-UNDO. 
DEFINE VARIABLE init-dir  AS CHARACTER NO-UNDO.
/*DEF VAR lv-list-name LIKE list-name EXTENT 2 NO-UNDO. */

{methods/defines/hndldefs.i}
/* {methods/prgsecur.i} */
/* temp prgsecur.i */
/* prgsecur.i {1} - WIN or blank */

{methods/defines/globdefs.i}

DEFINE BUFFER b-prgrms FOR prgrms.

DEFINE VARIABLE v-prgmname   LIKE b-prgrms.prgmname NO-UNDO.
DEFINE VARIABLE Audit_File   AS CHARACTER NO-UNDO.
DEFINE VARIABLE period_pos   AS INTEGER   NO-UNDO.
DEFINE VARIABLE num-groups   AS INTEGER   NO-UNDO.
DEFINE VARIABLE group-ok     AS LOGICAL   NO-UNDO.
DEFINE VARIABLE access-close AS LOGICAL   NO-UNDO.

v-prgmname = "fgpstall.".

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.
    
{fg/fullset.i NEW}
{fg/fg-post3.i NEW}
{jc/jcgl-sh.i }
{sys/inc/adjustgl.i}

DEF STREAM before.
DEF STREAM after.

DEFINE VARIABLE excelheader AS CHARACTER NO-UNDO.
DEFINE STREAM excel.
DEF            VAR v-fgpostgl     AS CHAR    NO-UNDO.

DO TRANSACTION:
    {sys/inc/fgpost.i}   
    {sys/inc/fgpostgl.i}
    assign v-fgpostgl = fgpostgl.
END.
DEFINE SHARED VARIABLE choice AS LOG NO-UNDO.

DEF VAR v-fg-value AS DEC FORMAT "->,>>>,>>9.99".

DEF            VAR v-msf          AS DEC     FORMAT ">,>>9.999" EXTENT 6.
DEF            VAR is-xprint-form AS LOG     NO-UNDO.
DEF {1} SHARED VAR v-print-fmt    AS CHAR    NO-UNDO.
DEF            VAR ls-fax-file    AS CHAR    NO-UNDO.
DEF            VAR fg-uom-list    AS CHAR    NO-UNDO.


DEF            VAR lInvFrt        AS LOG     NO-UNDO.
DEF            VAR dBillAmt       AS DECIMAL NO-UNDO.
DEF            VAR lEmailBol      AS LOG     NO-UNDO.
DEF            VAR ll             AS LOG     NO-UNDO.

{sys/form/r-top.i}

{sys/inc/ctrtext.i str-tit 112}.

{sys/form/r-top3w1.f "Before"}

{sys/form/r-top3w1.f "After"}

DEF TEMP-TABLE tt-set
    FIELD part-no LIKE fg-set.part-no
    INDEX i1 part-no.

{oerep/r-loadtg.i NEW}  /*w-ord for loadtag reprint */
    
DEF VAR ext-cost        AS DEC     NO-UNDO.
DEF VAR type            AS ch      FORMAT "X" INITIAL "R".
DEF VAR type-prt        AS ch      FORMAT "X(11)" INIT "".
DEF VAR v-fg-qty        LIKE fg-rctd.t-qty.
DEF VAR v-fg-cost       AS DEC     FORMAT "->,>>>,>>9.99<<".
DEF VAR v-tot-qty       AS INT     FORMAT "->>>,>>>,>>9".
DEF VAR v-tot-cost      AS DEC     FORMAT "->>>,>>9.99<<".
DEF VAR v-grd-tot-qty   AS INT     FORMAT "->>>,>>>,>>9".
DEF VAR v-grd-tot-cost  AS DEC     FORMAT "->>,>>>,>>9.99<<".                     
DEF VAR v-grd-tot-value AS DEC     FORMAT "->>,>>>,>>9.99<<".                     
DEF VAR v-tot-value     AS DEC     FORMAT "->>,>>>,>>9.99".
DEF VAR v-cum-tot       AS de.                                   
DEF VAR v-tran-type     AS CHAR    FORMAT "x(1)".      
DEF VAR v-entrytype     AS CHAR    INITIAL "REC ,TRAN,ADJ ,SHIP,RET ,INIT".
DEF VAR v-on            LIKE eb.num-up.
DEF VAR v-qty-pallet    AS DECIMAL FORMAT "->>,>>>,>>9" NO-UNDO.
DEF VAR v-whse          LIKE fg-rctd.loc.            
DEF VAR v-one           AS INTEGER FORMAT "->>,>>9" INIT 1.
DEF VAR v-ftime         AS LOGICAL INIT NO.
DEF VAR v-dscr          LIKE account.dscr.
DEF VAR v-disp-actnum   LIKE account.actnum.
DEF VAR v-disp-amt      AS DEC     FORMAT ">>,>>>,>>9.99cr".
DEF VAR v-hdr           AS CHAR    FORMAT "x(12)".
DEF VAR v-postlst       AS cha     NO-UNDO.
DEF VAR ll-wip          AS LOG     NO-UNDO.
DEF VAR li              AS INT     NO-UNDO.
DEF VAR li-loop         AS INT     NO-UNDO.
DEF VAR v-time          AS CHAR    FORMAT "X(5)" NO-UNDO.

DEF VAR v-itm-lbl       AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR v-itm-dsh       AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR v-desc-lbl      AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-lbl        AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-lbl      AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-desc-dsh      AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-Po-dsh        AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-vend-dsh      AS CHAR    FORMAT "x(30)" NO-UNDO.
DEF VAR v-uom-lbl       AS CHAR    FORMAT "x(10)" NO-UNDO.
DEF VAR v-uom-dsh       AS CHAR    FORMAT "x(10)" NO-UNDO.
DEF VAR v-cstprt        AS CHAR    FORMAT "x(15)" NO-UNDO.
DEF VAR v-pr-tots2      LIKE v-pr-tots NO-UNDO.


/* ********************  Preprocessor Definitions  ******************** */


/* ***************************  Main Block  *************************** */
IF rd-Itm#Cst# EQ 1 
    THEN ASSIGN v-itm-lbl = "ITEM"
        v-itm-dsh = "---------------".
ELSE ASSIGN v-itm-lbl = "CUSTOMER PART #"
        v-itm-dsh = "---------------".

IF rd-ItmPo EQ 1   
    THEN ASSIGN v-desc-lbl = "DESCRIPTION                           "
        v-Po-lbl   = ""
        v-vend-lbl = ""
        v-desc-dsh = "------------------------------".

ELSE ASSIGN v-desc-lbl = "DESCRIPTION"
        v-Po-lbl   = "P.O. #"
        v-vend-lbl = "VEND"
        v-desc-dsh = "-------------- --------- --------".

IF rd-UOMJob EQ 1 
    THEN ASSIGN v-uom-lbl = "UOM"
        v-uom-dsh = "----".
ELSE ASSIGN v-uom-lbl = "JOB #"
        v-uom-dsh = "----------".

FORM HEADER
    SPACE(56) "PRE POST AUDIT TRAIL"
    WITH FRAME before STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
    SPACE(57) "POSTED AUDIT TRAIL"
    WITH FRAME after STREAM-IO WIDTH 132 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

FORM HEADER
    "WHSE:"
    v-whse
    SKIP    
    "         TOTAL"   AT 128    
    "DATE"             AT 1
    "TIME"             AT 10
    TRIM(v-itm-lbl)  FORMAT "x(15)"  AT 16
    TRIM(v-desc-lbl) FORMAT "x(11)"  AT 32
    TRIM(v-Po-lbl)     AT 47
    TRIM(v-vend-lbl)  FORMAT "X(4)" AT 57
    "T"                AT 63
    "TAG #"            AT 65
    "UNITS"            AT 88  
    "COUNT"            AT 97
    "TOTAL"            AT 106
    "BIN"              AT 112    
    TRIM(v-uom-lbl) FORMAT "x(10)" AT 119
    v-hdr                  AT 130
    "--------"             AT 1                /*date*/
    "----"                 AT 10               /* time */                
    TRIM(v-itm-dsh)  FORMAT "x(15)" AT 16       /*item*/
    TRIM(v-desc-dsh) FORMAT "x(30)" AT 32      /*description p.o. # vendor*/
    "-"                    AT 63               /*t*/
    "--------------------" AT 65               /*tag # 8 -> 20*/
    "-------"              AT 86               /*units*/
    "--------"             AT 94               /*count*/
    "--------"             AT 103              /*total 11->8*/
    "------"               AT 112              /*bin  8 -> 6*/    
    TRIM(v-uom-dsh) FORMAT "x(10)" AT 119              /*uom*/
    "------------"         AT 130              /*total value 14 -> 12*/
    WITH FRAME r-top1 STREAM-IO WIDTH 170 NO-LABELS NO-BOX NO-UNDERLINE PAGE-TOP.

/*form #1 Print cases / qty case for TOTAL COST*/
FORM w-fg-rctd.rct-date             FORMAT "99/99/99" 
    v-time                                           
    w-fg-rctd.i-no                 FORMAT "x(15)"    
    w-fg-rctd.i-name               FORMAT "x(14)"    
    w-fg-rctd.po-no                                  
    po-ord.vend-no                 FORMAT "x(5)"                                  
    v-tran-type                                      
    w-fg-rctd.tag                  FORM "x(20)"      
    w-fg-rctd.cases                FORMAT "->>,>>9"  
    w-fg-rctd.qty-case             FORMAT "->>>,>>9" 
    v-fg-qty                       FORMAT "->>>,>>9" 
    w-fg-rctd.loc-bin              FORM "x(6)"       
    w-fg-rctd.pur-uom              FORMAT "x(9)"     
    v-fg-cost
    WITH FRAME itemx NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

FORM w-fg-rctd.rct-date             FORMAT "99/99/99" AT 1  
    v-time                                           AT 10 
    w-fg-rctd.i-no                 FORMAT "x(15)"    AT 16 
    w-fg-rctd.i-name               FORMAT "x(27)"    AT 32 
    v-tran-type                                      AT 63 
    w-fg-rctd.tag                  FORM "x(20)"      AT 65 
    w-fg-rctd.cases                FORMAT "->>,>>9"  AT 86 
    w-fg-rctd.qty-case             FORMAT "->>>,>>9" AT 94 
    v-fg-qty                       FORMAT "->>>,>>9" AT 103
    w-fg-rctd.loc-bin              FORM "x(6)"       AT 112
    w-fg-rctd.pur-uom              FORMAT "x(9)"     AT 119
    v-fg-cost 
    WITH FRAME itemxA NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

/*form #2 Print 1 / partial for TOTAL COST*/
FORM w-fg-rctd.rct-date             FORMAT "99/99/99"
    v-time                                          
    w-fg-rctd.i-no                 FORMAT "x(15)"   
    w-fg-rctd.i-name               FORMAT "x(14)"   
    w-fg-rctd.po-no                                 
    po-ord.vend-no                 FORMAT "x(5)"                   
    v-tran-type                                     
    w-fg-rctd.tag                FORM "x(20)"       
    v-one                          FORMAT "->>,>>9" 
    w-fg-rctd.partial              FORMAT "->>>,>>9"
    v-fg-qty                       FORMAT "->>>,>>9"
    w-fg-rctd.loc-bin              FORM "x(6)"      
    w-fg-rctd.pur-uom              FORMAT "x(9)"    
    v-fg-cost  
    WITH FRAME itempx NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

FORM w-fg-rctd.rct-date             FORMAT "99/99/99"   AT 1  
    v-time                                             AT 10 
    w-fg-rctd.i-no                 FORMAT "x(15)"      AT 16 
    w-fg-rctd.i-name               FORMAT "x(27)"      AT 32 
    v-tran-type                                        AT 63 
    w-fg-rctd.tag                  FORM "x(20)"        AT 65 
    v-one                          FORMAT "->>,>>9"    AT 86 
    w-fg-rctd.partial              FORMAT "->>>,>>9"   AT 94 
    v-fg-qty                       FORMAT "->>>,>>9"   AT 103
    w-fg-rctd.loc-bin              FORM "x(6)"         AT 112
    w-fg-rctd.pur-uom              FORMAT "x(9)"       AT 119
    v-fg-cost                                       
    WITH FRAME itempxA NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

/*form #3 Print cases / qty case for TOTAL VALUE*/
FORM w-fg-rctd.rct-date             FORMAT "99/99/99"   
    v-time                                             
    w-fg-rctd.i-no                 FORMAT "x(15)"      
    w-fg-rctd.i-name               FORMAT "x(14)"      
    w-fg-rctd.po-no                                    
    po-ord.vend-no                 FORMAT "x(5)"                                   
    v-tran-type                                        
    w-fg-rctd.tag                  FORM "x(20)"        
    w-fg-rctd.cases                FORMAT "->>,>>9"    
    w-fg-rctd.qty-case             FORMAT "->>>,>>9"   
    v-fg-qty                       FORMAT "->>>,>>9"   
    w-fg-rctd.loc-bin              FORM "x(6)"         
    w-fg-rctd.pur-uom              FORMAT "x(9)"       
    v-fg-value 
    WITH FRAME itemy NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

FORM w-fg-rctd.rct-date             FORMAT "99/99/99"  AT 1  
    v-time                                            AT 10 
    w-fg-rctd.i-no                 FORMAT "x(15)"     AT 16 
    w-fg-rctd.i-name               FORMAT "x(27)"     AT 32 
    v-tran-type                                       AT 63 
    w-fg-rctd.tag                  FORM "x(20)"       AT 65 
    w-fg-rctd.cases                FORMAT "->>,>>9"   AT 86 
    w-fg-rctd.qty-case             FORMAT "->>>,>>9"  AT 94 
    v-fg-qty                       FORMAT "->>>,>>9"  AT 103
    w-fg-rctd.loc-bin              FORM "x(6)"        AT 112
    w-fg-rctd.pur-uom              FORMAT "x(9)"      AT 119
    v-fg-value                                      
    WITH FRAME itemyA NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

/*form #4 Print 1 / partial for TOTAL VALUE*/
FORM w-fg-rctd.rct-date             FORMAT "99/99/99"    
    v-time                                              
    w-fg-rctd.i-no                 FORMAT "x(15)"       
    w-fg-rctd.i-name               FORMAT "x(14)"       
    w-fg-rctd.po-no                                     
    po-ord.vend-no                 FORMAT "x(5)"                                  
    v-tran-type                                         
    w-fg-rctd.tag                                       
    v-one                          FORMAT "->>,>>9"     
    w-fg-rctd.partial              FORMAT "->>>,>>9"    
    v-fg-qty                       FORMAT "->>,>>>,>>9" 
    w-fg-rctd.loc-bin              FORM "x(6)"          
    w-fg-rctd.pur-uom              FORMAT "x(9)"        
    v-fg-value 
    WITH FRAME itempy NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.

FORM w-fg-rctd.rct-date             FORMAT "99/99/99"    AT 1  
    v-time                                              AT 10 
    w-fg-rctd.i-no                 FORMAT "x(15)"       AT 16 
    w-fg-rctd.i-name               FORMAT "x(27)"       AT 32 
    v-tran-type                                         AT 63 
    w-fg-rctd.tag                                       AT 65 
    v-one                          FORMAT "->>,>>9"     AT 86 
    w-fg-rctd.partial              FORMAT "->>>,>>9"    AT 94 
    v-fg-qty                       FORMAT "->>,>>>,>>9" AT 103
    w-fg-rctd.loc-bin              FORM "x(6)"          AT 112
    w-fg-rctd.pur-uom              FORMAT "x(9)"        AT 119
    v-fg-value                                       
    WITH FRAME itempyA NO-BOX DOWN STREAM-IO WIDTH 170 NO-LABELS.


FORM v-disp-actnum LABEL "G/L ACCOUNT NUMBER"
    v-dscr        LABEL "DESCRIPTION"
    v-post-date   LABEL "DATE"   
    v-disp-amt    LABEL "AMOUNT" SKIP
    WITH DOWN STREAM-IO WIDTH 130 FRAME gldetail.    

{ce/msfcalc.i}

SESSION:SET-WAIT-STATE ("general").

IF LENGTH(begin_job-no) < 6 THEN
    begin_job-no = FILL(" ",6 - LENGTH(TRIM(begin_job-no))) + TRIM(begin_job-no).
IF LENGTH(end_job-no) < 6 THEN
    end_job-no = FILL(" ",6 - LENGTH(TRIM(end_job-no))) + TRIM(end_job-no).

/*    IF ip-run-what EQ "" THEN                                    */
/*        DISPLAY begin_job-no END_job-no WITH FRAME {&FRAME-NAME}.*/

ASSIGN
    str-tit2    = CURRENT-WINDOW:TITLE
    {sys/inc/ctrtext.i str-tit2 112}
    str-tit3    = "Period Date: " + string(v-post-date,"99/99/9999") + "             Posted by: " + USERID('nosweat') + "  As of " + string(TODAY,"99/99/9999")
    {sys/inc/ctrtext.i str-tit3 132}

    v-postlst   = (IF t-receipt THEN "R," ELSE "") +
               (IF t-setup THEN "I," ELSE "") +
               (IF t-ship THEN "S," ELSE "") +
               (IF t-trans THEN "T," ELSE "") +
               (IF t-adj THEN "A," ELSE "") +
               (IF t-ret THEN "E," ELSE "") +
               (IF tgIssue THEN "F," ELSE "")
    v-cost-sell = rd_print EQ "C"
    v-pr-tots2  = tb_totCstVal
    v-pr-tots   = tb_grndtotal.

IF LENGTH(v-postlst) GT 0 AND
    SUBSTR(v-postlst,LENGTH(v-postlst),1) EQ "," THEN
    SUBSTR(v-postlst,LENGTH(v-postlst),1) = "".

DO li = 1 TO 2:
{sys/inc/print1.i}
    lv-list-name[li] = list-name.
    PAUSE 1 NO-MESSAGE.
END.

OUTPUT STREAM before TO VALUE(lv-list-name[1]) PAGE-SIZE VALUE(lines-per-page).
OUTPUT STREAM after  TO VALUE(lv-list-name[2]) PAGE-SIZE VALUE(lines-per-page).

IF td-show-parm THEN RUN show-param.

EMPTY TEMP-TABLE tt-set.
/* If not running for all items, check these items for components that must */
/* be included                                                              */
IF NOT (begin_i-no EQ "" AND end_i-no BEGINS "zzzzzzzzzzz")
    THEN RUN createComponentList.

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
        (fg-rctd.updated-by GE begin_userid 
        AND fg-rctd.updated-by LE end_userid))
        USE-INDEX rita-code:

        RUN build-tables.

    END.
END.
    
RUN build-comp-tables.

IF v-cost-sell THEN 
DO:
    v-hdr = "        COST".

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(fi_file).

        IF rd-ItmPo EQ 1
            THEN ASSIGN excelheader = "Date,Time,Item,Description,".
        ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

        ASSIGN 
            excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".
        IF rd-UOMJob EQ 1 
            THEN ASSIGN excelheader = excelheader + "UOM,Total Cost".
        ELSE ASSIGN excelheader = excelheader + "Job #,Total Cost".

        PUT STREAM excel UNFORMATTED excelheader SKIP.

    END.

    IF rd-ItmPo EQ 1 THEN 
    DO:
    {fg/rep/fg-post.i "itemxA" "v-fg-cost" "itempxA" "v-tot-cost"}
    END.
    ELSE 
    DO:
    {fg/rep/fg-post.i "itemx" "v-fg-cost" "itempx" "v-tot-cost"}
    END.
END.
ELSE 
DO:
    v-hdr = "       VALUE".

    IF tb_excel THEN 
    DO:
        OUTPUT STREAM excel TO VALUE(fi_file).

        IF rd-ItmPo EQ 1
            THEN ASSIGN excelheader = "Date,Time,Item,Description,".
        ELSE ASSIGN excelheader = "Date,Time,Item,Description,Po No,Vendor,".

        ASSIGN 
            excelheader = excelheader + 
                         "T,Tag No,Units,Count,Total,Bin,".

        IF rd-UOMJob EQ 1 
            THEN ASSIGN excelheader = excelheader + "UOM,Total Value".
        ELSE ASSIGN excelheader = excelheader + "Job #,Total Value".

        PUT STREAM excel UNFORMATTED excelheader SKIP.
    END.

    IF rd-ItmPo EQ 1 THEN 
    DO:
    {fg/rep/fg-post.i "itemyA" "v-fg-value" "itempyA" "v-tot-value"}
    END.
    ELSE 
    DO:
    {fg/rep/fg-post.i "itemy" "v-fg-value" "itempy" "v-tot-value"}
    END.
END.

IF v-pr-tots THEN 
DO:
    IF v-cost-sell THEN 
    DO:                   
        PUT STREAM before
            " " TO 124 SKIP       
            "MSF->  FG: " + trim(STRING(v-msf[5],"->>,>>9.9<<")) +
            "  Wst: " + trim(STRING(v-msf[6],"->>,>>9.9<<"))    +
            "  Tot: " + trim(STRING(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
            FORMAT "x(63)" AT 15
            "GRAND TOTALS:" TO 97
            v-grd-tot-qty TO 110 v-grd-tot-cost TO 141 SKIP. 

        PUT STREAM after
            " " TO 124 SKIP       
            "MSF->  FG: " + trim(STRING(v-msf[5],"->>,>>9.9<<")) +
            "  Wst: " + trim(STRING(v-msf[6],"->>,>>9.9<<"))    +
            "  Tot: " + trim(STRING(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
            FORMAT "x(63)" AT 15 
            "GRAND TOTALS:" TO 97
            v-grd-tot-qty TO 110 v-grd-tot-cost TO 141 SKIP.     
    END.
    ELSE 
    DO:
        PUT STREAM before
            " " TO 124 SKIP       
            "MSF->  FG: " + trim(STRING(v-msf[5],"->>,>>9.9<<")) +
            "  Wst: " + trim(STRING(v-msf[6],"->>,>>9.9<<"))    +
            "  Tot: " + trim(STRING(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
            FORMAT "x(63)" AT 15 
            "GRAND TOTALS:" TO 100
            v-grd-tot-qty TO 113 v-grd-tot-value TO 144 SKIP.

        PUT STREAM after
            " " TO 124 SKIP       
            "MSF->  FG: " + trim(STRING(v-msf[5],"->>,>>9.9<<")) +
            "  Wst: " + trim(STRING(v-msf[6],"->>,>>9.9<<"))    +
            "  Tot: " + trim(STRING(v-msf[5] + v-msf[6],"->>,>>9.9<<"))
            FORMAT "x(63)" AT 15 
            "GRAND TOTALS:" TO 97
            v-grd-tot-qty TO 110 v-grd-tot-value TO 141 SKIP.
    END.
END. /* if v-pr-tots */

HIDE FRAME r-top1.

IF tb_glnum THEN 
DO:
    PAGE STREAM before.
    PAGE STREAM after.

    FOR EACH work-gl BREAK BY work-gl.actnum:

        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ work-gl.actnum
            NO-LOCK NO-ERROR.

        ASSIGN
            v-dscr        = IF AVAIL account THEN account.dscr
                     ELSE "ACCOUNT NOT FOUND - " + work-gl.actnum
            v-disp-actnum = work-gl.actnum
            v-disp-amt    = work-gl.debits - work-gl.credits.

        DISPLAY STREAM before
            v-disp-actnum v-dscr v-post-date v-disp-amt
            WITH FRAME gldetail.
        DOWN STREAM before WITH FRAME gldetail.

        DISPLAY STREAM after
            v-disp-actnum v-dscr v-post-date v-disp-amt
            WITH FRAME gldetail.
        DOWN STREAM after WITH FRAME gldetail.
    END. /* each work-job */

    FOR EACH work-job BREAK BY work-job.actnum:

        FIND FIRST account
            WHERE account.company EQ cocode
            AND account.actnum  EQ work-job.actnum
            NO-LOCK NO-ERROR.

        ASSIGN
            v-dscr        = IF AVAIL account THEN account.dscr
                     ELSE "ACCOUNT NOT FOUND - " + work-job.actnum
            v-disp-actnum = work-job.actnum.

        IF work-job.fg THEN
            v-disp-amt = - work-job.amt.
        ELSE
            v-disp-amt = work-job.amt.

        DISPLAY STREAM before
            v-disp-actnum v-dscr v-post-date v-disp-amt
            WITH FRAME gldetail.
        DOWN STREAM before WITH FRAME gldetail.

        DISPLAY STREAM after
            v-disp-actnum v-dscr v-post-date v-disp-amt
            WITH FRAME gldetail.
        DOWN STREAM after WITH FRAME gldetail.
    END. /* each work-job */
END.

OUTPUT STREAM before CLOSE.
OUTPUT STREAM after  CLOSE.


/* **********************  Internal Procedures  *********************** */

PROCEDURE build-comp-tables:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEF BUFFER bf-fg-rctd   FOR fg-rctd.
    DEF BUFFER bf-w-fg-rctd FOR w-fg-rctd.

    /* Make sure all components are included in w-fg-rctd */
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "R":

        /* Check using fg-rcpts, then reftable since either could be used */
        FOR EACH fg-rcpts 
            WHERE fg-rcpts.company EQ w-fg-rctd.company 
            AND fg-rcpts.linker EQ "fg-rctd: " + STRING(w-fg-rctd.r-no,"9999999999") 
            NO-LOCK.
            FIND FIRST fg-set WHERE fg-set.part-no = fg-rcpts.i-no 
                AND fg-set.set-no EQ w-fg-rctd.i-no
                AND fg-set.company = w-fg-rctd.company NO-LOCK NO-ERROR.
            FIND fg-rctd WHERE fg-rctd.r-no EQ fg-rcpts.r-no
                NO-LOCK NO-ERROR.
            IF AVAIL fg-rctd /* AND AVAIL fg-set */ THEN 
            DO:
                FIND FIRST bf-w-fg-rctd WHERE bf-w-fg-rctd.row-id EQ ROWID(fg-rctd)
                    NO-LOCK NO-ERROR.
                IF NOT AVAIL bf-w-fg-rctd AND fg-rctd.rita-code EQ w-fg-rctd.rita-code THEN
                    RUN build-tables.
            END.
        END.
    END.
    FOR EACH w-fg-rctd WHERE w-fg-rctd.rita-code EQ "R":
        /* Checking a second time using reftable */
        FOR EACH fg-set 
            WHERE fg-set.company EQ w-fg-rctd.company
            AND fg-set.set-no  EQ w-fg-rctd.i-no
            NO-LOCK:


            IF w-fg-rctd.SetHeaderRno GT 0 THEN 
            DO:
                /* Each fg-rctd of a set part number */
                FOR EACH bf-fg-rctd 
                    WHERE bf-fg-rctd.company EQ w-fg-rctd.company
                    AND bf-fg-rctd.i-no EQ fg-set.part-no
                    AND bf-fg-rctd.rita-code EQ "R" 
                    AND bf-fg-rctd.SetHeaderRno GT 0
                    NO-LOCK:

                    FIND fg-rctd WHERE ROWID(fg-rctd) EQ ROWID(bf-fg-rctd)
                          NO-LOCK NO-ERROR.
                    IF AVAIL fg-rctd AND fg-rctd.rita-code NE "P" THEN 
                    DO:
                        FIND FIRST bf-w-fg-rctd WHERE bf-w-fg-rctd.row-id EQ ROWID(fg-rctd)
                             NO-LOCK NO-ERROR.
                        IF NOT AVAIL bf-w-fg-rctd AND fg-rctd.rita-code EQ w-fg-rctd.rita-code THEN
                             RUN build-tables.
                    END.
                  
                END. /* each bf-fg-rctd */
            END. /* avail reftable for header item */
        END. /* each fg-set */

    END. /* each w-fg-rctd, check for set components */


END PROCEDURE.

PROCEDURE build-tables:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR li-max-qty AS INT NO-UNDO.
    DEF VAR v-part-qty AS DEC NO-UNDO.
    DEF VAR v-set-qty  AS DEC NO-UNDO.
    DEF VAR v-cost     AS DEC NO-UNDO.

    DEF BUFFER b-fg-rctd FOR fg-rctd.
    DEF BUFFER b-itemfg  FOR itemfg.
    DEF BUFFER use-job   FOR reftable.

    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ fg-rctd.i-no
        NO-LOCK NO-ERROR.
    FIND FIRST loc WHERE loc.company EQ itemfg.company
        AND loc.loc = itemfg.loc
        NO-LOCK NO-ERROR.
    IF AVAIL itemfg /*AND AVAIL loc*/ THEN 
    DO TRANSACTION:
        li-max-qty = fg-rctd.t-qty.

        IF li-max-qty GE fg-rctd.t-qty THEN 
        DO:

            CREATE w-fg-rctd.
            BUFFER-COPY fg-rctd TO w-fg-rctd
                ASSIGN
                w-fg-rctd.row-id  = ROWID(fg-rctd)                
                w-fg-rctd.has-rec = YES.

            IF ip-run-what EQ "SETUP" THEN
                ASSIGN
                    w-fg-rctd.old-tag     = fg-rctd.tag
                    w-fg-rctd.ret-loc     = fg-rctd.loc
                    w-fg-rctd.ret-loc-bin = fg-rctd.loc-bin.
        END.
    END.


END PROCEDURE.

PROCEDURE calc-partial:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN 
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
                IF AVAIL uom THEN
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

PROCEDURE calc-total:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN 
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
                IF AVAIL uom THEN
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

PROCEDURE createComponentList:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-fg-set FOR fg-set.

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

        FIND FIRST tt-set WHERE tt-set.part-no = bf-fg-set.part-no NO-ERROR.
        IF NOT AVAIL tt-set THEN 
        DO:
            CREATE tt-set.
            ASSIGN 
                tt-set.part-no = bf-fg-set.part-no.
        END.

    END.


END PROCEDURE.

PROCEDURE orig:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    /*find first item finished goods based on the item number*/
    FIND FIRST itemfg
        WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
        USE-INDEX i-no NO-LOCK NO-ERROR.

    IF AVAIL itemfg THEN 
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
                IF AVAIL uom THEN
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
        v-msf[2] = v-msf[2] / 1000.


END PROCEDURE.

PROCEDURE show-param:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEF VAR lv-frame-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-group-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-field-hdl  AS HANDLE NO-UNDO.
    DEF VAR lv-field2-hdl AS HANDLE NO-UNDO.
    DEF VAR parm-fld-list AS cha    NO-UNDO.
    DEF VAR parm-lbl-list AS cha    NO-UNDO.
    DEF VAR i             AS INT    NO-UNDO.
    DEF VAR lv-label      AS cha.
/*                                                                                         */
/*    lv-frame-hdl = FRAME {&frame-name}:handle.                                           */
/*    lv-group-hdl = lv-frame-hdl:FIRST-CHILD.                                             */
/*    lv-field-hdl = lv-group-hdl:FIRST-CHILD .                                            */
/*                                                                                         */
/*    DO WHILE TRUE:                                                                       */
/*        IF NOT VALID-HANDLE(lv-field-hdl) THEN LEAVE.                                    */
/*        IF LOOKUP(lv-field-hdl:PRIVATE-DATA,"parm") > 0                                  */
/*            THEN                                                                         */
/*        DO:                                                                              */
/*            IF lv-field-hdl:LABEL <> ? THEN                                              */
/*                ASSIGN parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","   */
/*                    parm-lbl-list = parm-lbl-list + lv-field-hdl:LABEL + ","             */
/*                    .                                                                    */
/*            ELSE                                                                         */
/*            DO:  /* radio set */                                                         */
/*                ASSIGN                                                                   */
/*                    parm-fld-list = parm-fld-list + lv-field-hdl:SCREEN-VALUE + ","      */
/*                    .                                                                    */
/*                lv-field2-hdl = lv-group-hdl:FIRST-CHILD.                                */
/*                REPEAT:                                                                  */
/*                    IF NOT VALID-HANDLE(lv-field2-hdl) THEN LEAVE.                       */
/*                    IF lv-field2-hdl:PRIVATE-DATA = lv-field-hdl:NAME THEN               */
/*                    DO:                                                                  */
/*                        parm-lbl-list = parm-lbl-list + lv-field2-hdl:SCREEN-VALUE + ",".*/
/*                    END.                                                                 */
/*                    lv-field2-hdl = lv-field2-hdl:NEXT-SIBLING.                          */
/*                END.                                                                     */
/*            END.                                                                         */
/*        END.                                                                             */
/*        lv-field-hdl = lv-field-hdl:NEXT-SIBLING.                                        */
/*    END.                                                                                 */
/*                                                                                         */
/*    PUT STREAM before SPACE(28)                                                          */
/*        "< Selection Parameters >"                                                       */
/*        SKIP(1).                                                                         */
/*                                                                                         */
/*    DO i = 1 TO NUM-ENTRIES(parm-fld-list,","):                                          */
/*        IF ENTRY(i,parm-fld-list) NE "" OR                                               */
/*            entry(i,parm-lbl-list) NE "" THEN                                            */
/*        DO:                                                                              */
/*                                                                                         */
/*            lv-label = FILL(" ",34 - length(TRIM(ENTRY(i,parm-lbl-list)))) +             */
/*                trim(ENTRY(i,parm-lbl-list)) + ":".                                      */
/*                                                                                         */
/*            PUT STREAM before lv-label FORMAT "x(35)" AT 5                               */
/*                SPACE(1)                                                                 */
/*                TRIM(ENTRY(i,parm-fld-list)) FORMAT "x(40)"                              */
/*                SKIP.                                                                    */
/*        END.                                                                             */
/*    END.                                                                                 */
/*                                                                                         */
/*    PUT STREAM before FILL("-",80) FORMAT "x(80)" SKIP.                                  */


END PROCEDURE.

    

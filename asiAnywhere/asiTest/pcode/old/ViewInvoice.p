



/*------------------------------------------------------------------------
    File        : ViewInvoice.p
    Purpose     : ViewInvoice

    Syntax      :

    Description : Return a Dataset of all Order Inquiry

    Author(s)   : Jyoti Bajaj
    Created     : dec 19 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */



MESSAGE "aa".
{ViewInvoice.i}
    MESSAGE "cc".
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmOrderNum  as Character  NO-UNDO.
DEFINE INPUT PARAMETER vRowid  as RECID  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewInv.
{ar/rep/invoice.i "new"}
/*{custom/globdefs.i}  
{methods/defines/globdefs.i}*/
/*{methods/prgsecur.i}*/
DEFINE VARIABLE begin_inv  AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "x(8)"NO-UNDO.
DEFINE VARIABLE end_inv    AS INTEGER FORMAT ">>>>>9" NO-UNDO.
DEFINE VARIABLE end_cust   AS  CHARACTER FORMAT "x(8)"NO-UNDO.
DEFINE VARIABLE end_date AS DATE FORMAT "99/99/9999" INITIAL 12/31/9999 NO-UNDO.
DEFINE VARIABLE begin_date AS DATE FORMAT "99/99/9999" INITIAL 01/01/001 NO-UNDO.
DEFINE VARIABLE tb_export AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_reprint AS  LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE tb_posted AS LOGICAL INITIAL NO NO-UNDO.
DEFINE VARIABLE gcompany AS CHARACTER NO-UNDO.
DEFINE VARIABLE gloc AS CHARACTER NO-UNDO.
DEF VAR lv-prt-bypass AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-multi-faxout AS LOG NO-UNDO. 
DEF VAR lv-fax-image AS cha NO-UNDO.  
DEFINE VARIABLE tb_cust-copy AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_office-copy AS LOGICAL INITIAL NO NO-UNDO. 
DEFINE VARIABLE tb_sman-copy AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL NO NO-UNDO. 
DEF VAR v-ftp-done AS LOG NO-UNDO.
DEF VAR v-org-lines-per-page AS INT NO-UNDO.
DEF VAR lines-per-page AS INT NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR v-program AS CHAR NO-UNDO.
def var list-name as cha no-undo.


DEFINE VARIABLE v-prgmname AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE cocode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE locode AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_company AS CHAR NO-UNDO.





DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO .

DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.


IF prmOrderNum = ? THEN ASSIGN prmOrderNum = "".
IF prmAction = ? THEN ASSIGN prmAction = "".
/* ********************  Preprocessor Definitions  ******************** */
MESSAGE "prmAction" prmAction prmOrderNum vRowid.
    FOR EACH oe-ordl WHERE oe-ordl.ord-no =  int(prmOrderNum) NO-LOCK:
        FOR EACH ar-invl where ar-invl.ord-no  = oe-ordl.ord-no
                           AND ar-invl.cust-no = oe-ordl.cust-no
                           AND ar-invl.i-no    = oe-ordl.i-no
                           AND ar-invl.est-no  = oe-ordl.est-no NO-LOCK BY ar-invl.LINE :
            FOR EACH ar-inv WHERE ar-inv.x-no = ar-invl.x-no 
                              AND ar-inv.ord-no = ar-invl.ord-no  NO-LOCK:
                create ttViewInv.
                assign 
                    ttViewInv.cust-no      = ar-inv.cust-no
                    ttViewInv.cust-name    = ar-inv.cust-name
                    ttViewInv.ship-id      = ar-inv.ship-id
                    ttViewInv.inv-no       = ar-inv.inv-no
                    ttViewInv.tax-code     = ar-inv.tax-code
                    ttViewInv.terms        = ar-inv.terms
                    ttViewInv.terms-d      = ar-inv.terms-d
                    ttViewInv.inv-date     = ar-inv.inv-date
                    ttViewInv.due-date     = ar-inv.due-date
                    ttViewInv.ord-no       = ar-invl.ord-no
                    ttViewInv.disc-%       = ar-inv.disc-% 
                    ttViewInv.disc-days    = ar-inv.disc-days
                    ttViewInv.carrier      = ar-inv.carrier
                    ttViewInv.cost         = ar-invl.cost
                    ttViewInv.gross        = ar-inv.gross
                    ttViewInv.freight      = ar-inv.freight
                    ttViewInv.tax-amt      = ar-inv.tax-amt
                    ttViewInv.disc-taken   = ar-inv.disc-taken
                    ttViewInv.paid         = ar-inv.paid
                    ttViewInv.due          = ar-inv.due
                    ttViewInv.po-no        = ar-inv.po-no
                    ttViewInv.LINE         = ar-invl.LINE
                    ttViewInv.actnum       = ar-invl.actnum
                    ttViewInv.act-dscr     = get-actdscr()
                    ttViewInv.i-name       = ar-invl.i-name
                    ttViewInv.i-dscr       = get-i-dscr()
                    ttViewInv.inv-qty      = ar-invl.inv-qty
                    ttViewInv.cons-uom     = ar-invl.cons-uom
                    ttViewInv.sf-sht       = ar-invl.sf-sht
                    ttViewInv.unit-pr      = ar-invl.unit-pr
                    ttViewInv.pr-qty-uom   = ar-invl.pr-qty-uom
                    ttViewInv.amt          = ar-invl.amt
                    ttViewInv.amt-msf      = ar-invl.amt-msf 
                    ttViewInv.vRowid       = recid(ar-invl)
                    .
            END.  /*FOR EACH ar-invl */
        END.  /*FOR EACH ar-invl */
    END.   /*FOR EACH oe-ordl*/

/*********************************************************************************/


IF prmAction = "PrintInvoice" THEN DO:
    FIND ar-invl WHERE RECID(ar-invl) = vRowid NO-LOCK NO-ERROR.
    ASSIGN 
        cocode = ar-invl.company
        locode = ar-invl.loc
        g_company = ar-invl.company.
    MESSAGE "jy1" ar-invl.company ar-invl.loc ar-invl.company.
    find first sys-ctrl WHERE sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "INVPRINT"
                          no-lock no-error.
    if not avail sys-ctrl THEN
        do transaction:
        create sys-ctrl.
        assign
            sys-ctrl.company = cocode
            sys-ctrl.name    = "INVPRINT"
            sys-ctrl.descrip = "Print Invoice Headers on Invoice Form?".
        MESSAGE "jyo2" sys-ctrl.NAME.
            /* message sys-ctrl.descrip
                 VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                 UPDATE sys-ctrl.log-fld.*/
        end.  /*do transaction:*/        
        ASSIGN
            v-print-head = sys-ctrl.log-fld
            v-print-fmt  = sys-ctrl.char-fld.
        do transaction:
            {sys/inc/inexport.i}
        end. /*do transaction:*/ 
    IF v-print-fmt EQ "Clev 1/2" THEN
        ASSIGN
        v-program      = "ar/rep/invhalfp.p"
        lines-per-page = 42.
    ELSE
        IF v-print-fmt EQ "TriState" THEN
            ASSIGN
            v-program      = "ar/rep/invhalfp.p"
            lines-per-page = 41.
    ELSE
        IF v-print-fmt EQ "1/2 Page" THEN
            ASSIGN
            v-program      = "ar/rep/invhalfp.p"
            lines-per-page = 44.
        ELSE
        IF v-print-fmt EQ "Livngstn" THEN
            ASSIGN
            v-program      = "ar/rep/invhalfp.p"
            lines-per-page = 66.
        ELSE
        IF v-print-fmt eq "PAC 1/2" THEN
            ASSIGN
            v-program      = "ar/rep/invpack.p"
            lines-per-page = 44.
        ELSE
            IF v-print-fmt EQ "Color" THEN
                ASSIGN
                v-program      = "ar/rep/color.p"
                lines-per-page = 60.
        ELSE
            IF v-print-fmt EQ "Phoenix" THEN
                ASSIGN
                v-program      = "ar/rep/invphx.p"
                lines-per-page = 62.
        ELSE
        IF v-print-fmt EQ "Rudd" THEN
            ASSIGN
            v-program      = "ar/rep/invrudd.p"
            lines-per-page = 66.
        ELSE
        IF v-print-fmt EQ "Premier" THEN
            ASSIGN
            v-program      = "ar/rep/invprem.p"
            lines-per-page = 66.
        ELSE
        IF v-print-fmt EQ "Triad" THEN
            ASSIGN
            v-program      = "ar/rep/invtriad.p"
            lines-per-page = 62.
        ELSE
        IF v-print-fmt EQ "Brick" THEN
            ASSIGN
            v-program      = "ar/rep/invbrick.p"
            lines-per-page = 62.
        ELSE
            IF v-print-fmt EQ "Danbury" THEN
                ASSIGN
                v-program      = "ar/rep/invdnbry.p"
                lines-per-page = 41.
        ELSE
        IF v-print-fmt EQ "Sonoco" THEN
            ASSIGN
            v-program      = "ar/rep/invsono.p"
            lines-per-page = 62.
        ELSE
        IF v-print-fmt EQ "Empire" THEN
            ASSIGN
            v-program      = "ar/rep/invempir.p"
            lines-per-page = 62.
        ELSE
            IF v-print-fmt EQ "HOP" THEN
                ASSIGN
                v-program      = "ar/rep/invhop.p"
                lines-per-page = 42.
        ELSE
            IF v-print-fmt EQ "Allpkg" THEN
                ASSIGN
                v-program      = "ar/rep/invallpk.p"
                lines-per-page = 62.
        ELSE
            IF v-print-fmt EQ "MaxPak" THEN
                ASSIGN
                v-program      = "ar/rep/invmaxpk.p"
                lines-per-page = 42. 
        ELSE
            IF v-print-fmt EQ "Fibre" THEN
                ASSIGN
                v-program      = "ar/rep/invfibre.p"
                lines-per-page = 50.  
        ELSE
            IF v-print-fmt EQ "Abox" THEN
                ASSIGN
                v-program      = "ar/rep/invabox.p"
                lines-per-page = 60.
        ELSE
            IF v-print-fmt EQ "Harwell" THEN
                ASSIGN
                v-program      = "ar/rep/invharwl.p"
                lines-per-page = 63.
        ELSE
            IF v-print-fmt EQ "Chillic" THEN
                ASSIGN
                v-program      = "ar/rep/invchill.p"
                lines-per-page = 45.
        ELSE IF v-print-fmt EQ "Pacific" THEN
            ASSIGN
            v-program      = "ar/rep/invpacif.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "PremierX" THEN
            ASSIGN
            v-program      = "ar/rep/invpremx.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "ColoniaX" THEN
            ASSIGN
            v-program      = "ar/rep/invcolnx.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "BlueRX" THEN
            ASSIGN
            v-program      = "ar/rep/invbluex.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE
            IF v-print-fmt EQ "Interpac" THEN
                ASSIGN
                v-program      = "ar/rep/invinter.p"
                lines-per-page = 60.
        ELSE IF v-print-fmt EQ "Oracle" THEN
            ASSIGN
            v-program      = "ar/rep/invoracl.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt = "frankstn" OR v-print-fmt = "Mirpkg" THEN
            ASSIGN
            v-program      = "ar/rep/invfrank.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt = "ppi" THEN
            ASSIGN
            v-program      = "ar/rep/invppi.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "Xprint" THEN
            ASSIGN
            v-program      = "ar/rep/invxprnt.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "ImperiaX" THEN
            ASSIGN
            v-program      = "ar/rep/invximp.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "ConsBox" THEN
            ASSIGN
            v-program      = "ar/rep/invconsb.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "apc" THEN
            ASSIGN
            v-program      = "ar/rep/invxapc.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "Allpkgx" THEN
            ASSIGN
            v-program      = "ar/rep/invalpkx.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "CSCIN" THEN
            ASSIGN
            v-program      = "ar/rep/invcscin.p"
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "RUDDX" THEN
            ASSIGN
            v-program      = "ar/rep/invruddx.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "Sonocox" THEN
            ASSIGN
            v-program      = "ar/rep/invsonox.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "ASIXprnt" THEN
            ASSIGN
            v-program      = "ar/rep/invxasi.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "midwest" THEN
            ASSIGN
            v-program      = "ar/rep/invmidws.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "mwbox" THEN
            ASSIGN
            v-program      = "ar/rep/invmwbox.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "Southpak" THEN
            ASSIGN
            v-program      = "ar/rep/invsthpk.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "Southpak-xl" THEN
            ASSIGN
            v-program      = "ar/rep/invsthpk-xl.p"
        
          lines-per-page = 66
          is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "Hughes" THEN
         ASSIGN
          v-program      = "ar/rep/invhughs.p"
          lines-per-page = 66
          is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "Concepts" THEN
           ASSIGN
            v-program      = "ar/rep/invxcorc.p"  /*Corrugate Concepts format*/
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "CSC" THEN
           ASSIGN
            v-program      = "ar/rep/invxcsc.p"  /*Container Services format*/
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "elite" THEN
           ASSIGN
            v-program      = "ar/rep/invelite.p"  /*Elite Packaging format*/
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "ArgrovX" THEN
           ASSIGN
            v-program      = "ar/rep/invxargv.p"  /*Argrov Packaging format*/
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "Indiana" THEN
           ASSIGN
            v-program      = "ar/rep/invindc.p"  /*Indiana <= Elite Packaging format*/
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt = "Imperial" THEN
         ASSIGN
          v-program      = "ar/rep/invimper.p"
          lines-per-page = 62.
        
        ELSE
        IF CAN-DO("RFC,AgMach",v-print-fmt) THEN
         ASSIGN
          v-program      = "ar/rep/invrfc.p"
          lines-per-page = IF v-print-fmt EQ "RFC" THEN 62 ELSE 66.
        
        ELSE IF v-print-fmt = "Herman" THEN
         ASSIGN
          v-program      = "ar/rep/invhermn.p"
          lines-per-page = 62.
        
        ELSE
        IF v-print-fmt EQ "Dayton" THEN
         ASSIGN
          v-program      = "ar/rep/invdaytn.p"
          lines-per-page = 66
          is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "Unipak" THEN
         ASSIGN
          v-program      = "ar/rep/invunipk.p"
          lines-per-page = 66
          is-xprint-form = YES.
        
        ELSE IF v-print-fmt = "CENTBOX" THEN
             ASSIGN
              v-program      = "ar/rep/invcentx.p"
              lines-per-page = 66
              is-xprint-form = YES.
        
        ELSE IF v-print-fmt = "ABOX-Xp" THEN
             ASSIGN
              v-program      = "ar/rep/invxabox.p"
              lines-per-page = 66
              is-xprint-form = YES.
        
        ELSE IF v-print-fmt = "HPB" THEN
          ASSIGN
           v-program      = "ar/rep/invhpb.p"
           lines-per-page = 66
           is-xprint-form = YES.
        ELSE
         ASSIGN
          v-program      = "ar/rep/invasi.p"
          is-xprint-form = YES /* rdb 02/02/07 01290705 */
          lines-per-page = 66.
         IF v-print-fmt = "BOXTECH" THEN lv-prt-bypass = YES.
         IF LOOKUP(v-print-fmt,"PremierX,BlueRx,ColoniaX") > 0 THEN DO:
     ASSIGN 
            /*tb_cust-copy:HIDDEN IN FRAME {&FRAME-NAME} = NO*/
            tb_cust-copy = YES
            /*tb_office-copy:HIDDEN = NO*/
            tb_office-copy = YES
           /* tb_sman-copy:HIDDEN = NO*/
            tb_sman-copy = YES.
           
  END.
MESSAGE "EMAIL3" .
  /*{methods/nowait.i}*/
  v-org-lines-per-page = lines-per-page .
    
    /*{custom/usrprint.i}      */
    
/*    lines-per-page = string(v-org-lines-per-page).*/
    IF LOOKUP(v-print-fmt,"frankstn,Mirpkg") > 0 THEN 
       ASSIGN /*tb_export:HIDDEN = NO*/
              tb_export = NO.
              /*tb_export:SCREEN-VALUE = "NO"*/
              IF tb_posted = yes THEN 
                  ASSIGN tb_export = YES.
                   ELSE 
                       ASSIGN tb_export = NO .
    /*ELSE ASSIGN tb_export = no.*/
                /*tb_export:SCREEN-VALUE = "NO"
                tb_export:HIDDEN = YES.*/
   
    MESSAGE "tb_export" tb_export VIEW-AS ALERT-BOX.
    IF tb_posted = yes THEN tb_reprint =  YES.

  
MESSAGE "PrintInvoice" prmAction vRowid.
IF AVAIL ar-invl THEN DO:
    FIND FIRST ar-inv WHERE ar-inv.x-no = ar-invl.x-no   NO-LOCK NO-ERROR.
    RUN custom/setUserPrint.p (ar-inv.company,'ar-inv_.',
                               'begin_inv,end_inv,begin_cust,end_cust,tb_reprint,tb_posted',
                               STRING(ar-inv.inv-no) + ',' + STRING(ar-inv.inv-no) + ',' +
                               ar-inv.cust-no + ',' + ar-inv.cust-no + ',' +
                               STRING(ar-inv.printed) + ',' + STRING(ar-inv.posted)).
 /*   RUN listobjs/ar-inv_.w.*/
 
    ASSIGN
        begin_inv    =  ar-inv.inv-no
        begin_cust   =  ar-inv.cust-no
        end_inv      =  ar-inv.inv-no
        end_cust     =  ar-inv.cust-no
        tb_reprint   =  ar-inv.printed
        tb_posted    =  ar-inv.posted
        begin_date   =  01/01/001
        end_date     =  12/31/9999
        .
    MESSAGE "ar-inv" ar-inv.cust-no ar-inv.inv-no ar-inv.printed ar-inv.posted.
    END.
    
    run run-report. 
    IF is-xprint-form THEN  DO:
        IF v-print-fmt EQ "Southpak-xl" THEN
            DO:
            OS-DELETE c:\tmp\Invoice.
            OS-COPY c:\tmp\Invoice.pdf c:\tmp\Invoice.
            ASSIGN list-name = "c:\tmp\Invoice".
        END. /*IF v-print-fmt EQ "Southpak-xl" THEN*/
    
    IF v-print-fmt EQ "Southpak-xl" THEN .
    ELSE
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
        {custom/asimail.i &TYPE="CUSTOMER"
            &begin_cust=begin_cust
            &END_cust=END_cust
            &mail-subject= "Invoice"
            &mail-body="Invoice"
            &mail-file=list-name }
    END.  /*IF is-xprint-form THEN  DO:*/
        ELSE DO:
            {custom/asimailR.i &TYPE="CUSTOMER"
                &begin_cust=begin_cust
                &END_cust=END_cust
                &mail-subject="Invoice"
                &mail-body="Invoice"
                &mail-file=list-name }
        END.   /*ELSE DO:*/
            
END.  /*if prmAction*/

PROCEDURE run-report :
/* ------------------------------------------------ ar/rep/invoice.p  9/94 RM */
/* PRINT INVOICE - A/R MODULE                                                 */
/* -------------------------------------------------------------------------- */
    DEF VAR lv-copy# AS INT NO-UNDO.
    MESSAGE "run-report" VIEW-AS ALERT-BOX.
    {sys/form/r-top.i}
        ASSIGN
        finv       = begin_inv
        tinv       = end_inv
        fcust      = begin_cust
        tcust      = end_cust
        v-print    = tb_reprint
        v-posted   = tb_posted.
    {sys/inc/print1.i}
    {sys/inc/outprint.i VALUE(lines-per-page)}
        IF td-show-parm THEN RUN show-param.
        {sa/sa-sls01.i}
            v-term-id = v-term.
        
        FOR EACH ar-inv WHERE ar-inv.company  EQ cocode
                          AND ar-inv.inv-no   GE finv
                          AND ar-inv.inv-no   LE tinv
                          AND ar-inv.cust-no  GE fcust
                          AND ar-inv.cust-no  LE tcust
                          AND (ar-inv.posted  EQ NO OR ar-inv.posted  EQ v-posted) 
                          AND ar-inv.printed  EQ v-print
                          AND ar-inv.inv-date GE begin_date
                          AND ar-inv.inv-date LE end_date
                          AND CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no EQ ar-inv.x-no)
                          USE-INDEX inv-no NO-LOCK:
            MESSAGE "abc" ar-inv.company.
            CREATE report.
            ASSIGN
                report.term-id = v-term-id
                report.key-01  = STRING(ar-inv.inv-no,"9999999999")
                report.rec-id  = RECID(ar-inv).
            MESSAGE "report" report.term-id  report.key-01 report.rec-id.
        END.   /*FOR EACH ar-inv */
MESSAGE "report.rec-id" VIEW-AS ALERT-BOX.
v-lines-per-page = lines-per-page.

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "INVCOPYS" NO-LOCK NO-ERROR.
lv-copy# = IF AVAIL sys-ctrl AND sys-ctrl.int-fld <> 0 THEN sys-ctrl.int-fld ELSE 1.

IF is-xprint-form THEN DO:
   
            MESSAGE "is-xprint-form" VIEW-AS ALERT-BOX.
            IF v-print-fmt = "CentBox" THEN
                 PUT "<PREVIEW><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=3mm><PDF-TOP=4mm><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(120)".
            ELSE PUT "<PREVIEW><PDF-OUTPUT=" + list-name + ".pdf>" FORM "x(60)".  
      
END.

IF LOOKUP(v-print-fmt,"SOUTHPAK,Southpak-xl,ASIXprnt") > 0 THEN DO: 
    
    RUN value(v-program) (lv-multi-faxout,lines-per-page).     
    
END.
ELSE IF lookup(v-print-fmt,"PremierX,BlueRX,ColoniaX") > 0 THEN do:
    RUN value(v-program) ("").
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
END.
ELSE RUN value(v-program). 
OUTPUT CLOSE.

v-ftp-done = NO.
inexport-log  = YES.
MESSAGE "tb_export" tb_export.
IF tb_export AND inexport-log THEN DO:    
   DEF VAR v-exp-file AS cha NO-UNDO.
   v-exp-file = inexport-desc +  
                "ARINV_" + trim(v-print-fmt) + 
                    substr(string(year(today),"9999"),3,2) +
                    string(month(today),"99") +
                    string(day(today),"99") +
                    substr(string(time,"HH:MM:SS"),1,2) +
                    substr(string(time,"HH:MM:SS"),4,2) +
                    substr(string(time,"HH:MM:SS"),7,2) + ".dat".
   OUTPUT TO VALUE(v-exp-file).
   MESSAGE "out".
   IF inexport-cha EQ "CIT" THEN DO:
      RUN ar/rep/expfrank.p .
      OUTPUT CLOSE.
      OUTPUT TO VALUE(".\ar\ftpcmd2.txt").     /* ftp text file */
      PUT UNFORMATTED 
       "open cs.ftp.citonline.com" SKIP  /* ftp server ip address */
       "ftpa1526" SKIP  /* userid*/
       "none" SKIP  /* password */
       "put " v-exp-file " " '"' "$$ ID=EP003F BID='DI1526' PASSWORD=NARF" '"' SKIP         /* file to transfer */
       "quit" .
      OUTPUT CLOSE.
      OS-COMMAND SILENT value("ftp -v -i -s:.\oe\ftpcmd2.txt"). 
      v-ftp-done = YES.
   END.
END.

FOR EACH report WHERE report.term-id EQ v-term-id: 
    MESSAGE "report1"  report.term-id .
  DELETE report.
END.

/*RUN custom/usrprint.p (v-prgmname).*/
                                                                    
END PROCEDURE.


PROCEDURE printPDF EXTERNAL "C:\Program Files\xPrint79\xPrint.dll"            PERSISTENT :
    DEF INPUT PARAMETER A AS CHAR.
    DEF INPUT PARAMETER B AS CHAR.
    DEF INPUT PARAMETER C AS CHAR.
END.


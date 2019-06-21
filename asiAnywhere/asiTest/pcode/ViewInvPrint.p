
/*------------------------------------------------------------------------
    File        : ViewInvoicePrint.p
    Purpose     : ViewInvoicePrint
    Main File   : arrep/r-ariprt.w
    Syntax      :

    Description : Return a Dataset of all View INvoice Print

    Author(s)   : 
    Created     : dec 19 2007
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */




{ViewInvPrint.i}
    
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vRowid  as RECID  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewInvPrint.


{sys/inc/var.i new shared}  
{ar/rep/invoice.i "new"}
{custom/xprint.i}

DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
DEF VAR vcBOLNums       AS CHAR NO-UNDO.
DEFINE VARIABLE v-title AS CHARACTER INITIAL "Invoicing".
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
DEF VAR vPdfFile AS CHAR.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO .
DEF VAR vcDefaultForm AS CHAR NO-UNDO.
DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.
DEF BUFFER b-ar-inv FOR ar-inv.
DEFINE VAR v-webrootpath AS CHAR NO-UNDO.
DEF VAR tmp-path AS CHAR NO-UNDO .
DEF VAR v-VERSION AS CHAR NO-UNDO.
/* Output selection for the report */
DEFINE NEW SHARED VARIABLE LvOutputSelection AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE CallingParameter AS CHAR NO-UNDO.
DEF NEW SHARED VAR nsv_setcomp AS LOGICAL NO-UNDO.
DEF NEW SHARED VAR s-print-zero-qty AS LOG NO-UNDO.

/* br Task 12081002 - to pass which item to print on invoice */
DEFINE NEW SHARED VARIABLE svi-print-item AS INTEGER INITIAL 1 NO-UNDO.
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmAction = "PrintInvoice" THEN DO:

ASSIGN LvOutputSelection = "Email".
    FIND ar-invl WHERE RECID(ar-invl) = vRowid NO-LOCK NO-ERROR.
    ASSIGN 
        cocode = ar-invl.company
        locode = ar-invl.loc
        .
    
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "X-VERSION" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "X-VERSION"
         sys-ctrl.descrip  = "Server Name"
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "Server 2003".
   END.
   IF AVAIL sys-ctrl  THEN
        v-VERSION = sys-ctrl.char-fld .
  RELEASE sys-ctrl.


FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
     sys-ctrl.NAME = "Xspool" NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
   DO:
      CREATE sys-ctrl.
      ASSIGN
         sys-ctrl.company  = cocode
         sys-ctrl.name     = "Xspool"
         sys-ctrl.descrip  = "Default path To Create temp File for Web pdf "
         sys-ctrl.log-fld = YES
         sys-ctrl.char-fld = "c:\spool\".
   END.
   IF AVAIL sys-ctrl  THEN
        tmp-path = sys-ctrl.char-fld .
  RELEASE sys-ctrl.
    
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
        end.  /*do transaction:*/        
        ASSIGN
            v-print-head = sys-ctrl.log-fld
            v-print-fmt  = sys-ctrl.char-fld
            vcDefaultForm = v-print-fmt.
        do transaction:
            {sys/inc/inexport.i}
        end. /*do transaction:*/ 
       RUN SetInvForm(v-print-fmt).
/******************************************************************************************************/  

IF AVAIL ar-invl THEN DO:
    FIND FIRST ar-inv WHERE ar-inv.x-no = ar-invl.x-no   NO-LOCK NO-ERROR.
    ASSIGN
        begin_inv    =  ar-inv.inv-no
        begin_cust   =  ar-inv.cust-no
        end_inv      =  ar-inv.inv-no
        end_cust     =  ar-inv.cust-no
        tb_reprint   =  ar-inv.printed
        tb_posted    =  ar-inv.posted
        begin_date   =  01/01/001
        end_date     =  12/31/9999
        vcBOLNums       =  STRING (ar-inv.inv-no)
        .
    
    END.
FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

     init-dir    = v-webrootpath .
    lv-pdf-file = init-dir + "INVOICE" + vcBOLNums + STRING(TIME) .
    ASSIGN vPdfFile = 'INVOICE' + vcBOLNums + STRING(TIME) + '.pdf'.
    
    IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
    sys-ctrl-shipto.company = cocode AND
    sys-ctrl-shipto.NAME = "INVPRINT") THEN
    DO:
       FOR EACH b-ar-inv FIELDS(cust-no) WHERE
           b-ar-inv.company  EQ cocode AND
           b-ar-inv.inv-no   GE begin_inv AND
           b-ar-inv.inv-no   LE end_inv AND
           b-ar-inv.cust-no  GE begin_cust AND
           b-ar-inv.cust-no  LE end_cust AND
           (b-ar-inv.posted  EQ NO OR
            b-ar-inv.posted  EQ tb_posted) AND
           b-ar-inv.printed  EQ tb_reprint AND
           b-ar-inv.inv-date GE begin_date AND
           b-ar-inv.inv-date LE end_date AND
           CAN-FIND(FIRST ar-invl WHERE ar-invl.x-no EQ b-ar-inv.x-no)
           NO-LOCK
           BREAK BY b-ar-inv.cust-no:

           IF FIRST-OF(b-ar-inv.cust-no) THEN
           DO:
              FIND FIRST sys-ctrl-shipto WHERE
                   sys-ctrl-shipto.company = cocode AND
                   sys-ctrl-shipto.NAME = "INVPRINT" AND
                   sys-ctrl-shipto.cust-vend = YES AND
                   sys-ctrl-shipto.cust-vend-no = b-ar-inv.cust-no AND
                   sys-ctrl-shipto.char-fld > ''
                   NO-LOCK NO-ERROR.

              IF AVAIL sys-ctrl-shipto THEN
              DO:
                 v-print-fmt = sys-ctrl-shipto.char-fld.
                 RUN SetInvForm(sys-ctrl-shipto.char-fld).
              END.
              ELSE
              DO:
                  v-print-fmt = vcDefaultForm.
                  RUN SetInvForm(vcDefaultForm).
              END.

              RUN run-report(b-ar-inv.cust-no, TRUE).

           END.

       END.
    END.
 ELSE /* not find sys-ctrl-shipto*/
 DO:
    v-print-fmt = vcDefaultForm.

    RUN run-report("", FALSE).
   END.
   
    IF v-VERSION = "Server 2008" THEN do:
        OS-COPY VALUE(list-name) VALUE (tmp-path).
        PAUSE 15.
    END.
    ELSE
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

       
        CREATE ttViewInvPrint.
     ASSIGN ttViewInvPrint.vInvFile = vPdfFile.

END.  /*if prmAction*/
/**********************************************************************************************/

PROCEDURE run-report :
DEFINE INPUT PARAMETER ip-cust-no AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER ip-sys-ctrl-shipto AS LOG NO-UNDO.

   DEF VAR lv-copy# AS INT NO-UNDO.
   {sys/form/r-top.i}
       ASSIGN
       finv       = begin_inv
       tinv       = end_inv
       fcust      = begin_cust
       tcust      = end_cust
       v-print    = tb_reprint
       v-posted   = tb_posted.

   /*{sys/inc/print1.i}*/

if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmp" + string(time)
       init-dir = tmp-dir.


   {sys/inc/outprint.i VALUE(lines-per-page)}

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

           CREATE report.
           ASSIGN
               report.term-id = v-term-id
               report.key-01  = STRING(ar-inv.inv-no,"9999999999")
               report.rec-id  = RECID(ar-inv)
               vcBOLNums       = vcBOLNums + '-' + STRING (ar-inv.inv-no)
       vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

   IF vcBOLNums MATCHES '*-*' THEN DO:
       vcBOLNums       = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).
   END.
END.   /*FOR EACH ar-inv */
v-lines-per-page = lines-per-page.

PUT "<PDF=DIRECT><PRINT=NO><PDF-EXCLUDE=MS Mincho,Courier new><PDF-LEFT=5mm><PDF-TOP=5mm><PDF-OUTPUT=" + lv-pdf-file  + ".pdf>" FORM "x(180)".
DISPLAY WITH FRAME r-top.


IF LOOKUP(v-print-fmt,"SOUTHPAK,southpak-xl,ASIXprnt,Badger,Badger-Emailed,Southpakl") > 0 THEN DO:
    RUN value(v-program) (lv-multi-faxout,lines-per-page).
END.
ELSE IF v-print-fmt EQ "1/2 Page" /*AND rd-dest = 6*/ THEN DO:
    PUT CONTROL CHR(27) CHR(67) CHR(44). 
    RUN value(v-program). 
    PUT CONTROL CHR(18).
END.

ELSE IF lookup(v-print-fmt,"BlueRX,ColoniaX,ABC,Knight,Knight1,Rosmar,ACPI,ColorX,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN do:  
    
    RUN value(v-program) ("").
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
END.
ELSE IF lookup(v-print-fmt,"ColorX") > 0 THEN DO:
    /*v-reprint = YES.*/
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy").
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy").
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy").
END.
ELSE IF lookup(v-print-fmt,"PremierX") > 0 THEN do: 
    
    RUN value(v-program) ("",NO). 
    /*v-reprint = YES.*/
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",NO).
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy",NO).
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",NO).
END.
ELSE IF lookup(v-print-fmt,"PremierS") > 0 THEN do:    
    RUN value(v-program) ("",YES). 
    /*-v-reprint = YES.*/
    IF tb_cust-copy THEN RUN value(v-program) ("Customer Copy",YES).
    IF tb_office-copy THEN RUN value(v-program) ("Office Copy",YES).
    IF tb_sman-copy  THEN RUN value(v-program) ("Salesman Copy",YES).
END.
ELSE RUN value(v-program). 
OUTPUT CLOSE.

FOR EACH report WHERE report.term-id EQ v-term-id: 

 DELETE report.
END.
END PROCEDURE.
/**********************************************************************************************/
PROCEDURE SetInvForm :
  DEFINE INPUT PARAMETER icFormName AS CHAR NO-UNDO.  
 
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
       IF v-print-fmt EQ "PremierS" THEN
          ASSIGN
             v-program      =  "ar/rep/invpremx.p"
             lines-per-page = 66
             is-xprint-form = YES.
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
       ELSE IF (v-print-fmt = "Xprint"  OR v-print-fmt = "invprint 1" OR v-print-fmt = "invprint 2") THEN
           ASSIGN
           v-program      = "ar/rep/invxprnt.p"
           lines-per-page = 75
           is-xprint-form = YES.

       ELSE IF v-print-fmt = "invprint10-CAN" THEN
            ASSIGN
                v-program      = "oe/rep/inv10can.p"
                lines-per-page = 66
                is-xprint-form = YES.
        ELSE IF v-print-fmt = "invprint 10" OR v-print-fmt = "invprint 20" THEN
            ASSIGN
                v-program      = "oe/rep/invxprnt10.p"
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
        ELSE IF v-print-fmt EQ "Soule" THEN
            ASSIGN
            v-program      = "ar/rep/invsoule.p"
            lines-per-page = 66
            is-xprint-form = YES.
        ELSE IF v-print-fmt EQ "SouleMed" THEN
            ASSIGN
            v-program      = "ar/rep/invsoulemed.p"
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
                lines-per-page = 70
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "CSC" THEN
           ASSIGN
            v-program      = "ar/rep/invxcsc.p"  /*Container Services format*/
            lines-per-page = 66
            is-xprint-form = YES.
        
        ELSE IF v-print-fmt EQ "elite" THEN
           ASSIGN
            v-program      = "ar/rep/invelite.p"  /*Elite Packaging format*/
            lines-per-page = 70
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

        ELSE IF v-print-fmt = "TriLakes" THEN
             ASSIGN
              v-program      = "ar/rep/invtri.p"
              lines-per-page = 66
              is-xprint-form = YES.

        ELSE IF v-print-fmt = "TriLakesBroker" THEN
          ASSIGN
             v-program = "ar/rep/invtribrk.p"
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

        ELSE IF v-print-fmt = "ILWALKER" THEN
          ASSIGN
             v-program = "oe/rep/invilwalkp.p"
             lines-per-page = 55.

        ELSE IF v-print-fmt = "DEE"  THEN
          ASSIGN
             v-program = "ar/rep/invdee.p"
             lines-per-page = 66
             is-xprint-form = YES.

        ELSE IF v-print-fmt = "Badger"  THEN
          ASSIGN
            v-program = "ar/rep/invBadger.p"
            lines-per-page = 66
            is-xprint-form = YES.

        ELSE IF v-print-fmt = "Carded"  THEN
          ASSIGN
            v-program = "ar/rep/invcardx.p"
            lines-per-page = 66
            is-xprint-form = YES.

        ELSE IF v-print-fmt = "CCCFGLot" THEN
          ASSIGN
             v-program =  "ar/rep/invcccfg.p"
             lines-per-page = 66
             is-xprint-form = YES. 
        ELSE IF v-print-fmt = "CCCFGL3" THEN
          ASSIGN
             v-program =  "ar/rep/invcfgl3.p"
             lines-per-page = 66
             is-xprint-form = YES. 
        ELSE IF v-print-fmt = "ABC" THEN
          ASSIGN
             v-program =  "ar/rep/invabcx.p"
             lines-per-page = 66
             is-xprint-form = YES. 
        ELSE IF v-print-fmt = "Keystone" THEN
          ASSIGN
             v-program = "ar/rep/invkeystone.p"  /*Xprint format*/
             lines-per-page = 66
             is-xprint-form = YES.

         ELSE IF v-print-fmt = "Fibrex" THEN
          ASSIGN
             v-program = "ar/rep/invfibrex.p"   /*Xprint format*/
             lines-per-page = 66
             is-xprint-form = YES.
         ELSE IF v-print-fmt = "Accord" THEN
          ASSIGN
             v-program      = "ar/rep/invaccrd.p"
             lines-per-page = 72
             is-xprint-form = YES.

          ELSE IF v-print-fmt = "NStock" THEN
          ASSIGN
             v-program = "ar/rep/invnstok.p"  /*NStock format*/
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Hughes2" THEN
          ASSIGN
             v-program = "ar/rep/invhugh2.p"  /*Hughes format*/
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Adapt" THEN
          ASSIGN
            v-program      = "ar/rep/invadapt.p"  /*Adapt format*/
            lines-per-page = 66
            is-xprint-form = YES.
          ELSE IF v-print-fmt = "CSC-GA" THEN
          ASSIGN
            v-program      = "ar/rep/invcscga.p"  /*CSC-GA format*/
            lines-per-page = 71
            is-xprint-form = YES.
          ELSE IF v-print-fmt = "CSC-GASummary" THEN
          ASSIGN
            v-program      = "ar/rep/invcscgsm.p"  /*CSC-GASummary format*/
            lines-per-page = 71
            is-xprint-form = YES.
          ELSE IF v-print-fmt = "Knight" THEN
          ASSIGN
             v-program      =  "ar/rep/invknight.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Southpakl" THEN
          ASSIGN
             v-program      =  "ar/rep/invsthpklg.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Androp" THEN
          ASSIGN
             v-program = "ar/rep/invandrop.p"   
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Knight1" THEN
          ASSIGN
             v-program      =  "ar/rep/invknight1.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Packrite" THEN
          ASSIGN
             v-program = "ar/rep/invpkrt.p"  
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Rosmar" THEN
          ASSIGN
             v-program =  "ar/rep/invrosmr.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Badger-Emailed" THEN
          ASSIGN
             v-program = "ar/rep/invbadgereml.p"   
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "allwest" THEN
          ASSIGN
             v-program      = "ar/rep/invallws.p"
             lines-per-page = 71
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Simkins" THEN
          ASSIGN
             v-program = "ar/rep/invsmkct.p"
             lines-per-page = 66
             is-xprint-form = YES.

          ELSE IF v-print-fmt = "CapCityIn" THEN 
          ASSIGN
             v-program = "ar/rep/invcapcin.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "ACPI" THEN
          ASSIGN
             v-program =  "ar/rep/invacpi.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "ColorX" THEN
          ASSIGN
             v-program      =  "ar/rep/invcolrx.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "loylang" THEN /* LOYLANG gmd 11200902 */
          ASSIGN
             v-program      = "ar/rep/invloyln.p"
             lines-per-page = 71             
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "LoylangBSF" THEN /* small mod to Loylang with Price/BSF instead of price */
          ASSIGN
             v-program      = "ar/rep/invloyln.p"
             lines-per-page = 71             
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Protagon" THEN /* Copied form LoyLangBSF */
          ASSIGN
             v-program      = "ar/rep/invprot.p"
             lines-per-page = 71             
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Protagon2" THEN /* Copied from Protagon */
          ASSIGN
             v-program      = "ar/rep/invprot2.p"
             lines-per-page = 71             
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Printers" THEN /* LOYLANG Format */
          ASSIGN
             v-program      = "ar/rep/invprnts.p"
             lines-per-page = 71             
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "loylangjit" THEN
          ASSIGN
             v-program      = "ar/rep/invloyjit.p"
             lines-per-page = 76
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "ColonialLot#" THEN
          ASSIGN
             v-program =  "ar/rep/invcolnx2.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Peachtreefgl3" THEN
          ASSIGN
             v-program =  "ar/rep/invptreefgl3.p"
             lines-per-page = 66
             is-xprint-form = YES.
          ELSE IF v-print-fmt = "Peachtree" THEN
          ASSIGN
             v-program =  "ar/rep/invptreelot.p"
             lines-per-page = 66
             is-xprint-form = YES.

        ELSE
         ASSIGN
          v-program      = "ar/rep/invasi.p"
          is-xprint-form = YES /* rdb 02/02/07 01290705 */
          lines-per-page = 66.
         IF v-print-fmt = "BOXTECH" THEN lv-prt-bypass = YES.



END PROCEDURE.

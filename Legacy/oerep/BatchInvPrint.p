/* -----------------------------------------------------
    File        : BatchInvprint.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Mon April 29  2019
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
{methods/defines/hndldefs.i}

{methods/defines/globdefs.i}

{custom/gcompany.i}
{custom/gloc.i}
{custom/getcmpny.i}
{custom/getloc.i}

{sys/inc/var.i new shared}

ASSIGN
    cocode = gcompany
    locode = gloc.

{oe/rep/invoice.i "new"}
{custom/xprint.i}

DEFINE            VARIABLE vcDefaultForm    AS CHARACTER NO-UNDO.
DEFINE            VARIABLE list-name        AS cha       NO-UNDO.
DEFINE            VARIABLE init-dir         AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lines-per-page   AS INTEGER   INITIAL 99 NO-UNDO .
DEFINE            VARIABLE vcInvNums        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-program        AS CHARACTER NO-UNDO.
DEFINE            VARIABLE is-xprint-form   AS LOG       NO-UNDO.
DEFINE            VARIABLE lv-pdf-file      AS CHARACTER NO-UNDO.
DEFINE            VARIABLE cActualPdf       AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-multi-faxout  AS LOG       INIT NO NO-UNDO.  /*for faxing to multiple receipents */
DEFINE            VARIABLE lv-prt-bypass    AS LOG       NO-UNDO.  /* bypass window's printer driver */
DEFINE            VARIABLE cRtnChar         AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lRecFound        AS LOGICAL   NO-UNDO.
DEFINE            VARIABLE cPathPdfFile     AS CHARACTER NO-UNDO .
DEFINE            VARIABLE lCopyPdfFile     AS LOGICAL   NO-UNDO.


DEFINE NEW SHARED VARIABLE nsv_setcomp      AS LOGICAL   NO-UNDO.
DEFINE NEW SHARED VARIABLE s-print-zero-qty AS LOG       NO-UNDO.
DEFINE NEW SHARED VARIABLE svi-print-item   AS INTEGER   INITIAL 1 NO-UNDO.

{ar/rep/invoice2.i "new"}

DEFINE BUFFER b-cust FOR cust.

RUN sys/ref/nk1look.p (INPUT cocode, "InvoiceSavePDF", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    cPathPdfFile = cRtnChar . 

RUN sys/ref/nk1look.p (INPUT cocode, "InvoiceSavePDF", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lCopyPdfFile = LOGICAL(cRtnChar) NO-ERROR .

FIND FIRST sys-ctrl
    WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "INVPRINT"
    NO-LOCK NO-ERROR.

ASSIGN
    v-print-head  = sys-ctrl.log-fld
    v-print-fmt   = sys-ctrl.char-fld
    vcDefaultForm = v-print-fmt.

FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.
 
IF AVAILABLE users AND users.user_program[2] NE "" THEN
    init-dir = users.user_program[2].
ELSE
    init-dir = "c:\tmp".


IF lCopyPdfFile THEN
    RUN runReportForm .

PROCEDURE runReportForm:
    
    IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
        sys-ctrl-shipto.company = cocode AND
        sys-ctrl-shipto.NAME = "INVPRINT" AND
        sys-ctrl-shipto.cust-vend = YES 
        ) THEN
    DO:
       
        FOR EACH ar-inv WHERE
            ar-inv.company EQ cocode AND
            ar-inv.inv-date EQ TODAY AND
            ar-inv.inv-no NE 0 
            NO-LOCK,
            FIRST b-cust WHERE
            b-cust.company EQ cocode AND
            b-cust.cust-no EQ ar-inv.cust-no 
            NO-LOCK
            BREAK BY ar-inv.inv-no
            BY ar-inv.cust-no
            BY ar-inv.sold-no:

            IF FIRST-OF(ar-inv.inv-no) THEN
            DO:
                /* Find INVPRINT shipto for customer, ship location and a form name. */
                FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "INVPRINT" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = ar-inv.cust-no AND
                    sys-ctrl-shipto.ship-id = STRING(ar-inv.sold-no) AND
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.

                /* If not found, then find INVPRINT shipto for customer and a form name. */
                IF NOT AVAILABLE sys-ctrl-shipto THEN
                    FIND FIRST sys-ctrl-shipto WHERE
                        sys-ctrl-shipto.company = cocode AND
                        sys-ctrl-shipto.NAME = "INVPRINT" AND
                        sys-ctrl-shipto.cust-vend = YES AND
                        sys-ctrl-shipto.cust-vend-no = ar-inv.cust-no AND
                        sys-ctrl-shipto.ship-id = '' AND /* stacey */
                        sys-ctrl-shipto.char-fld > ''
                        NO-LOCK NO-ERROR.

                IF AVAILABLE sys-ctrl-shipto THEN
                DO:

                    RUN SetInvPostForm(sys-ctrl-shipto.char-fld). 
                    
                    v-print-fmt = sys-ctrl-shipto.char-fld.
                    dPrintFmtDec = sys-ctrl-shipto.dec-fld.
                END.
                ELSE
                DO:
                    RUN SetInvPostForm(vcDefaultForm). 
                    v-print-fmt = vcDefaultForm.
                END.
                RUN run-report(ar-inv.cust-no,ar-inv.sold-no, TRUE).
                
                RUN GenerateReport .
            END.
        END.
    END. /*can-find sys-ctrl and not posted*/
    ELSE
    DO:
        FOR EACH ar-inv WHERE
            ar-inv.company EQ cocode AND
            ar-inv.inv-date EQ TODAY AND
            ar-inv.inv-no NE 0 
            NO-LOCK,
            FIRST b-cust WHERE
            b-cust.company EQ cocode AND
            b-cust.cust-no EQ ar-inv.cust-no 
            NO-LOCK
            BREAK BY ar-inv.inv-no
            BY ar-inv.cust-no
            BY ar-inv.sold-no:

            RUN SetInvPostForm(vcDefaultForm). 
            v-print-fmt = vcDefaultForm.
             
            RUN run-report("","", FALSE).
            RUN GenerateReport  .
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


   
    {sys/form/r-top.i}
    
    {sys/inc/print1.i }

    {sys/inc/outprint.i value(lines-per-page)}
    
    {sa/sa-sls01.i}

    v-term-id = v-term + USERID("nosweat") . 
    /*list-name =   .*/
    lv-pdf-file = cPathPdfFile + "\Inv_"  .

    SESSION:SET-WAIT-STATE ("general").


    CREATE report.
    ASSIGN
        report.term-id = v-term-id
        report.rec-id  = RECID(ar-inv)
        vcInvNums      = STRING (ar-inv.inv-no) .

    ASSIGN 
        v-lines-per-page = lines-per-page.

    FIND FIRST report NO-LOCK WHERE report.term-id  = v-term-id NO-ERROR.
    
    IF is-xprint-form THEN 
    DO: 

        IF v-print-fmt EQ "CentBox" THEN
        DO:
            PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-EXCLUDE=MS Mincho><PDF-LEFT=" + trim(STRING(3 + dPrintFmtDec)) + "mm><PDF-TOP=4mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
            cActualPDF = lv-pdf-file + vcInvNums  + ".pdf".
        END.

        ELSE IF v-print-fmt EQ "Southpak-XL" OR v-print-fmt EQ "PrystupExcel" THEN 
            DO:
                IF dPrintFmtDec > 0 THEN
                    PUT "<PDF=DIRECT><PDF-LEFT=" + trim(STRING(dPrintFmtDec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums +  ".pdf>" FORM "x(180)".
                ELSE
                    PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcInvNums +  ".pdf>" FORM "x(180)".
                cActualPDF = list-name + ".pdf".
            END.

            ELSE IF v-print-fmt EQ "Protagon" OR v-print-fmt = "Protagon2" THEN 
                DO:
                    PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=" + trim(STRING(0.5 + dPrintFmtDec)) + "mm><PDF-TOP=-0.5mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                    cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                END.

                ELSE IF v-print-fmt EQ "PremierX" OR v-print-fmt EQ "Coburn" OR v-print-fmt = "PremierS" OR v-print-fmt = "Axis" THEN  
                    DO:
                        PUT "<PDF=DIRECT><FORMAT=LETTER><PDF-LEFT=" + trim(STRING(5 + dPrintFmtDec)) + "mm><PDF-TOP=7mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
                    END.

                    ELSE 
                    DO:
                        IF dPrintFmtDec > 0 THEN
                            PUT "<PDF=DIRECT><PDF-LEFT=" + trim(STRING(dPrintFmtDec)) + "mm><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                        ELSE
                            PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcInvNums + ".pdf>" FORM "x(180)".
                 
                    
                        cActualPDF = lv-pdf-file + vcInvNums + ".pdf".
               
                    END.

        PUT "</PROGRESS>".

    END. /* Is Xprint form */
    

    IF LOOKUP(v-print-fmt,"SOUTHPAK,southpak-xl,PrystupExcel,ASIXprnt,Southpakl,Badger,Badger-Emailed") > 0 THEN 
    DO: 
        RUN value(v-program) (lv-multi-faxout,lines-per-page). 
    END.
    ELSE IF LOOKUP(v-print-fmt,"BlueRX,ColoniaX,ABC,Nosco,Nosco1,Central,Rosmar,ACPI,ColonialLot#,Carded,CCCFGLot,CCCFGL3,Peachtreefgl3,Peachtree") > 0 THEN 
        DO:
            RUN value(v-program) (""). 
           
            
        END.
        ELSE IF LOOKUP(v-print-fmt,"ColorX") > 0 THEN 
            DO:
                /* v-reprint = YES.*/
                RUN  value(v-program) ("Office Copy").
                
            END.
            ELSE IF LOOKUP(v-print-fmt,"PremierX,Coburn,Axis") > 0 THEN 
                DO: 
                    RUN value(v-program) ("",NO). 
                /*v-reprint = YES.*/
                END.
                ELSE IF LOOKUP(v-print-fmt,"PremierS") > 0 THEN 
                    DO:    
                        RUN value(v-program) ("",YES). 
                    /*v-reprint = YES.*/
                        
                    END.
                    ELSE IF LOOKUP(v-print-fmt,"nStock,nStockLogo") > 0 THEN 
                        DO:    
                            RUN value(v-program) (v-print-fmt). 
                        END.
                        ELSE RUN value(v-program). 
    

    SESSION:SET-WAIT-STATE ("").

    FOR EACH report WHERE report.term-id EQ v-term-id: 
        DELETE report.
    END.


END PROCEDURE.



PROCEDURE GenerateReport:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/

    IF v-print-fmt NE "Southpak-XL" AND v-print-fmt <> "PrystupExcel" THEN 
    DO:
        RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

    END.


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
        WHEN "LancoYork" THEN
            ASSIGN
                v-program      = "ar/rep/invlanyork.p"
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
        WHEN "NStock" OR 
        WHEN "NStockLogo" THEN
            ASSIGN
                v-program      = "ar/rep/invnstok.p"  /*NStock nStockLogo format*/
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

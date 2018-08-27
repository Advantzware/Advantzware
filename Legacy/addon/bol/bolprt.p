/* addon/bol/bolprt.p   */
DEF INPUT PARAM ip-beg-cust AS cha NO-UNDO.
DEF INPUT PARAM ip-end-cust AS cha NO-UNDO.
DEF INPUT PARAM ip-beg-bol AS INT NO-UNDO.
DEF INPUT PARAM ip-end-bol AS INT NO-UNDO.
DEF INPUT PARAM ip-beg-ord AS INT NO-UNDO.
DEF INPUT PARAM ip-end-ord AS INT NO-UNDO.
DEF INPUT PARAM ip-release# AS INT NO-UNDO.
DEF INPUT PARAM ip-reprint AS LOG NO-UNDO.
DEF INPUT PARAM ip-term AS cha NO-UNDO.

DEF VAR rd_bolcert AS cha INIT "BOL" NO-UNDO.
DEF VAR tb_reprint AS LOG NO-UNDO.
DEF VAR tb_pallet AS LOG INIT NO NO-UNDO.
DEF VAR tmp-dir AS cha NO-UNDO.
DEF VAR list-name AS cha NO-UNDO.
DEF VAR init-dir AS cha NO-UNDO.

DEF VAR rd-dest AS INT INIT 1 NO-UNDO.
DEF VAR lv-ornt AS cha NO-UNDO.
DEF VAR lv-lines-per-page AS INT NO-UNDO.
DEF VAR lv-font AS INT NO-UNDO.
DEF VAR lv-show-parm AS LOG NO-UNDO.

DEF BUFFER b-boll FOR oe-boll.
DEF BUFFER b-report FOR report.

{custom/globdefs.i}
{sys/inc/var.i new shared}

assign
 cocode = g_company
 locode = g_loc.

{oe/rep/oe-lad.i NEW}
{XMLOutput/XMLOutput.i &NEW=NEW &XMLSysCtrl=XMLBOL &Company=cocode}

def var v-print-fmt as char no-undo format 'x'.
def var v-headers   as log no-undo.
def var v-print-coc as log no-undo.

DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS CHAR NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEF VAR begin_cust-no AS cha NO-UNDO.
DEF VAR begin_bol# AS INT NO-UNDO.

DEFINE VARIABLE retcode AS INTEGER   NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.

 RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.


{custom/xprint.i}

DEF VAR lv-prt-bypass AS LOG NO-UNDO.  /* bypass window's printer driver */

DEF VAR lines-per-page AS INT NO-UNDO.

tb_reprint = ip-reprint.


RUN get-bolform.
RUN custom/d-prtdes.w (OUTPUT rd-dest,OUTPUT lv-ornt,OUTPUT lv-lines-per-page,OUTPUT lv-font, OUTPUT lv-show-parm).

FIND FIRST users WHERE
     users.user_id EQ USERID("NOSWEAT")
     NO-LOCK NO-ERROR.

IF AVAIL users AND users.user_program[2] NE "" THEN
   init-dir = users.user_program[2] + "\".
ELSE
   init-dir = "c:\tmp\".

RUN run-report.

case rd-dest:
       when 1 then run output-to-printer.
       when 2 then run output-to-screen.
       when 3 then run output-to-file.
/*       when 4 then do:
           /*run output-to-fax.*/
           {custom/asifax.i &begin_cust=begin_cust-no
                            &END_cust=begin_cust-no
                            &fax-subject="BOL"
                            &fax-body="BOL"
                            &fax-file=list-name }
       END. */
       when 5 then do:
           IF is-xprint-form THEN DO:
              RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
              {custom/asimail1.i &TYPE = "CUSTOMER"
                             &begin_cust= begin_cust-no
                             &END_cust=begin_cust-no
                             &mail-subject="BOL"
                             &mail-body="BOL"
                             &mail-file=lv-pdf-file + ".pdf" }
           END.
           ELSE DO:
               {custom/asimail3.i &TYPE = "CUSTOMER"
                                  &begin_cust= begin_cust-no
                                  &END_cust=begin_cust-no
                                  &mail-subject='"BOL"'
                                  &mail-body='"BOL"'
                                  &mail-file=list-name }

           END.
 
       END.       
       WHEN 6 THEN run output-to-port.
end case. 

/*RUN OUTPUT-to-printer.*/
RETURN.

PROCEDURE get-bolform:
    find first company where company.company eq cocode no-lock no-error.
    find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
    find first sys-ctrl where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLPRINT" no-lock no-error.
    if not avail sys-ctrl then do transaction:
       create sys-ctrl.
       ASSIGN sys-ctrl.company = cocode
              sys-ctrl.name    = "BOLPRINT"
              sys-ctrl.descrip = "Print Bill of Lading Headers on Bill of Lading Form".
       MESSAGE "System control record not found.  Would you like to print headers?"
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE sys-ctrl.log-fld.
    end.
    v-print-hdgs = sys-ctrl.log-fld.

    find first sys-ctrl where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLFMT" NO-LOCK no-error.
    if not avail sys-ctrl then do transaction:
       create sys-ctrl.
       ASSIGN sys-ctrl.company  = cocode
              sys-ctrl.name     = "BOLFMT"
              sys-ctrl.descrip  = "Bill of lading format"
              sys-ctrl.char-fld = "ASI".
       message "System control record not found. Update BOL Print format"
       update sys-ctrl.char-fld.
    end.
    ASSIGN v-print-fmt = sys-ctrl.char-fld
           v-headers   = sys-ctrl.log-fld.

    find first sys-ctrl where sys-ctrl.company eq cocode
         and sys-ctrl.name    eq "BOLCERT" no-lock no-error.
    if not avail sys-ctrl then do transaction:
       create sys-ctrl.
       ASSIGN sys-ctrl.company  = cocode
              sys-ctrl.name     = "BOLCERT"
              sys-ctrl.descrip  = "Print Certificate of Compliance forms?"
              sys-ctrl.log-fld  = no.
       message sys-ctrl.descrip update sys-ctrl.log-fld.
    end.
    ASSIGN v-print-coc = sys-ctrl.log-fld
           v-coc-fmt   = sys-ctrl.char-fld. 

    IF v-print-fmt EQ "1/2 page" THEN
       ASSIGN
       v-program      = "oe/rep/bolhalfp.p"
       lines-per-page = 44.

    ELSE IF v-print-fmt EQ "Royal" THEN
       ASSIGN v-program      = "oe/rep/bolroyal.p"
              lines-per-page = 56.
    ELSE IF v-print-fmt EQ "ContSrvc" THEN
         ASSIGN v-program      = "oe/rep/bol-csc.p"
                lines-per-page = 62.

    ELSE IF v-print-fmt EQ "HOP" THEN
         ASSIGN v-program      = "oe/rep/bolhop.p"
                lines-per-page = 39.

    ELSE IF v-print-fmt EQ "Superior" THEN
         ASSIGN v-program      = "oe/rep/bolsuper.p"
                lines-per-page = 55.

    ELSE IF v-print-fmt EQ "Premier" THEN
         ASSIGN v-program      = "oe/rep/bolprem.p"
                lines-per-page = 55.

    ELSE IF v-print-fmt EQ "PremierX" THEN
      ASSIGN
       v-program      = "oe/rep/bolpremx.p"
       is-xprint-form = YES
       lines-per-page = 80.

    ELSE IF v-print-fmt EQ "RFC" THEN
         ASSIGN v-program      = "oe/rep/bolrfc.p"
                lines-per-page = 55.

    ELSE IF v-print-fmt EQ "Sonoco" THEN
         ASSIGN v-program      = "oe/rep/bolsonoc.p"
                lines-per-page = 51.

    ELSE IF v-print-fmt EQ "Warren" THEN
         ASSIGN v-program      = "oe/rep/bolwarrn.p"
                lines-per-page = 59.

    ELSE IF v-print-fmt EQ "PAC 1/2" THEN
         ASSIGN v-program      = "oe/rep/bolpack.p"
                lines-per-page = 44.

    ELSE IF v-print-fmt EQ "Imperial" THEN
         ASSIGN v-program      = "oe/rep/bolimp.p"
                lines-per-page = 60.

    ELSE IF v-print-fmt EQ "P&P" THEN
         ASSIGN v-program      = "oe/rep/bolpnp.p"
                lines-per-page = 62.

    ELSE IF v-print-fmt EQ "Triad" THEN
         ASSIGN v-program      = "oe/rep/boltriad.p"
                lines-per-page = 62.

    ELSE IF v-print-fmt EQ "TriState" THEN
         ASSIGN v-program      = "oe/rep/boltrist.p"
                lines-per-page = 41.

    ELSE IF v-print-fmt EQ "BlueRidg" THEN
         ASSIGN v-program      = "oe/rep/bolbluer.p"
                lines-per-page = 65.

    ELSE IF v-print-fmt EQ "Danbury" THEN
         ASSIGN v-program      = "oe/rep/boldnbry.p"
                lines-per-page = 42.

    ELSE IF v-print-fmt EQ "Boxtech" THEN
         ASSIGN v-program      = "oe/rep/bolboxt.p"
                lines-per-page = 55
                lv-prt-bypass = YES .

    ELSE IF v-print-fmt EQ "Empire" THEN
         ASSIGN v-program      = "oe/rep/bolempir.p"
                lines-per-page = 54.

    ELSE IF v-print-fmt EQ "pacific" THEN
         ASSIGN v-program      = "oe/rep/bolpacif.p"
                is-xprint-form = YES
                lines-per-page = 64.

    ELSE IF v-print-fmt EQ "century" THEN
         ASSIGN v-program      = "oe/rep/bolcentx.p"
                is-xprint-form = YES
                lines-per-page = 64.

    ELSE IF v-print-fmt EQ "Xprint" THEN
         ASSIGN v-program      = "oe/rep/bolxprnt.p"
                is-xprint-form = YES
                lines-per-page = 66.

    ELSE IF v-print-fmt EQ "APC" THEN
         ASSIGN v-program      = "oe/rep/bolxapc.p"
                is-xprint-form = YES
                lines-per-page = 66.

    ELSE IF v-print-fmt EQ "P&Px" THEN
         ASSIGN v-program      = "oe/rep/bolpnpx.p"
                is-xprint-form = YES
                lines-per-page = 66.
    ELSE IF v-print-fmt EQ "Xprint2" THEN
      ASSIGN
       v-program      = "oe/rep/bolxprt2.p"
       is-xprint-form = YES
       lines-per-page = 66.
    ELSE IF v-print-fmt EQ "CSCIN" THEN
      ASSIGN
       v-program      = "oe/rep/bolcscin.p"
       is-xprint-form = YES
       lines-per-page = 66.

    ELSE IF v-print-fmt EQ "SouthPak" THEN
      ASSIGN
       v-program      = "oe/rep/bolsouth.p"
       is-xprint-form = YES
       lines-per-page = 66.

    ELSE IF v-print-fmt EQ "MWBox" THEN
         ASSIGN v-program      = "oe/rep/bolmwbox.p"
                is-xprint-form = YES
                lines-per-page = 66.
    ELSE IF v-print-fmt EQ "Hughes" THEN
         ASSIGN v-program      = "oe/rep/bolhughs.p"
                is-xprint-form = YES
                lines-per-page = 64.
    ELSE IF v-print-fmt EQ "Inland" THEN
         ASSIGN v-program      = "oe/rep/bolxinld.p"
                is-xprint-form = YES
                lines-per-page = 80.
    ELSE IF v-print-fmt EQ "concepts" THEN
         ASSIGN v-program      = "oe/rep/bolxcorc.p"
                is-xprint-form = YES
                lines-per-page = 80.
    ELSE IF v-print-fmt EQ "Brick" THEN
         IF v-headers THEN
            ASSIGN v-program      = "oe/rep/bolbrck1.p"
                   lines-per-page = 64.
         ELSE ASSIGN v-program      = "oe/rep/bolbrick.p"
                     lines-per-page = 60.

    ELSE IF v-print-fmt EQ "AllPkg" THEN
         ASSIGN v-program      = "oe/rep/bolallpk.p"
                lines-per-page = 60.

    ELSE IF v-print-fmt EQ "Fibre" THEN
         ASSIGN v-program      = "oe/rep/bolfibre.p"
                lines-per-page = 57.

    ELSE IF v-print-fmt EQ "MaxPak" THEN
         ASSIGN v-program      = "oe/rep/bolmaxpk.p"
                lines-per-page = 42.
/*
    ELSE IF v-print-fmt EQ "Oracle" THEN
         ASSIGN v-program      = "oe/rep/boloracl.p"
                lines-per-page = 64.
*/  ELSE IF v-print-fmt EQ "Oracle" THEN
    ASSIGN
     v-program      = "oe/rep/bolora2.p"
     is-xprint-form = YES
     lines-per-page = 64.

    ELSE
    IF v-print-fmt EQ "Frankstn" OR v-print-fmt = "Mirpkg" THEN
    ASSIGN
     v-program      = "oe/rep/bolfrank.p"
     is-xprint-form = YES
     lines-per-page = 64.

    ELSE IF v-print-fmt EQ "Ottpkg" THEN
    ASSIGN
     v-program      = "oe/rep/bolottpk.p"
     is-xprint-form = YES
     lines-per-page = 64.

    ELSE IF v-print-fmt EQ "ConsBox" THEN
    ASSIGN
     v-program      = "oe/rep/bolcnsbx.p"
     is-xprint-form = YES
     lines-per-page = 64.

    ELSE IF v-print-fmt EQ "Harwell" THEN
         ASSIGN v-program      = "oe/rep/bolharwl.p"
                lines-per-page = 62.

    ELSE IF v-print-fmt EQ "Chillic" THEN
         ASSIGN v-program      = "oe/rep/bolchill.p"
                lines-per-page = 60.

    ELSE IF v-print-fmt EQ "Midwest" THEN
         ASSIGN  v-program      = "oe/rep/bolmdwst.p"
                 lines-per-page = 52. /* 47*/

    ELSE
  IF v-print-fmt EQ "Intrpack" THEN
    ASSIGN 
     v-program      = "oe/rep/bolinter.p"
     lines-per-page = 60.

  ELSE
  IF v-print-fmt EQ "Dayton" THEN
    ASSIGN
     v-program      = "oe/rep/boldaytn.p"
     lines-per-page = 57.

  ELSE
  IF v-print-fmt EQ "Elite" THEN
    ASSIGN
     v-program      = "oe/rep/bolelite.p"
     is-xprint-form = YES
     lines-per-page = 66.

  ELSE
  IF v-print-fmt EQ "Express" THEN
    ASSIGN
     v-program      = "oe/rep/bolexprs.p"
     lines-per-page = 62.

    ELSE ASSIGN v-print-mode   = "PROD"
                v-program      = "oe/rep/oe-lad" +
                     (if v-print-fmt eq "c" then "c" else "s") + ".p"
                lines-per-page = 62.

END.

PROCEDURE run-report:
/* ------------------------------------------------- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */

{sys/form/r-top.i}

assign
 v-s-cust    = ip-beg-cust
 v-e-cust    = ip-end-cust
 v-s-bol     = ip-beg-bol
 v-e-bol     = ip-end-bol
 v-s-ord     = ip-beg-ord
 v-e-ord     = ip-end-ord
 v-printed   = tb_reprint
 v-print-pal = tb_pallet
 v-print-bol = rd_bolcert EQ "BOL".

IF NOT v-print-bol THEN
  ASSIGN
   v-program      = "oe/rep/cocbrick.p".
   
{sys/inc/print1.i}


{sys/inc/outprint.i value(lines-per-page)}

/*if td-show-parm then run show-param.*/


{sa/sa-sls01.i}
v-term-id = v-term.

build-work:
FOR EACH b-report
    WHERE b-report.term-id EQ ip-term
    NO-LOCK,

    FIRST b-boll
    WHERE RECID(b-boll) EQ b-report.rec-id
    NO-LOCK,
        
    FIRST oe-bolh
    WHERE oe-bolh.b-no    EQ b-boll.b-no
      AND oe-bolh.deleted EQ NO
      AND oe-bolh.printed EQ v-printed
      AND oe-bolh.posted  EQ NO
    
    BREAK BY oe-bolh.bol-no:

  IF LAST-OF(oe-bolh.bol-no) THEN DO:
    IF NOT oe-ctrl.p-bol THEN
    FOR EACH oe-boll
        WHERE oe-boll.company EQ oe-bolh.company
          AND oe-boll.bol-no  EQ oe-bolh.bol-no
          AND CAN-FIND(FIRST oe-ord
                       WHERE oe-ord.company EQ oe-boll.company
                         AND oe-ord.ord-no  EQ oe-boll.ord-no
                         AND (oe-ord.stat    EQ "H" OR oe-ord.priceHold))
        NO-LOCK:
      NEXT build-work.
    END.

    CREATE report.
    ASSIGN
     report.term-id  = v-term-id
     report.key-01   = oe-bolh.cust-no
     report.key-02   = oe-bolh.ship-id
     report.rec-id   = RECID(oe-bolh)
     report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")
     oe-bolh.printed = YES.
   begin_cust-no = oe-bolh.cust-no.
   begin_bol# = oe-bolh.bol-no.
  END.
  
END.
lv-pdf-file = init-dir + "\BOL" + string(begin_bol#).    
v-lines-per-page = lines-per-page.
/*
IF rd-dest = 2 AND is-xprint-form THEN PUT "<PREVIEW>".   
ELSE IF is-xprint-form AND rd-dest = 1 THEN PUT "<PRINTER?>".
*/



IF IS-xprint-form THEN DO:
    CASE rd-dest:
        WHEN 1 THEN PUT  "<PRINTER?>".
        WHEN 2 THEN do:
            IF NOT lBussFormModle THEN
              PUT "<PREVIEW><MODAL=NO>". 
            ELSE
              PUT "<PREVIEW>".        
        END.         
        WHEN  4 THEN do:
              ls-fax-file = "c:\tmp\fax" + STRING(TIME) + ".tif".
                  /*(IF is-xprint-form THEN ".xpr" ELSE ".txt").*/
              PUT UNFORMATTED "<PRINTER?><EXPORT=" Ls-fax-file ",BW>".
        END.
        WHEN 5 THEN DO:
            IF v-print-fmt = "Century" THEN
                 PUT "<PREVIEW><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
            ELSE PUT "<PREVIEW><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(180)".
        END.
        
    END CASE.
END.


RUN value(v-program).

for each report where report.term-id eq v-term-id:
  delete report.
end.

/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.


PROCEDURE output-to-printer:
  DEF VAR lv-font-no AS INT INIT 15 NO-UNDO.
  DEF VAR lv-ornt AS cha INIT "P" NO-UNDO.

  IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
  END.
  ELSE IF lv-prt-bypass THEN RUN custom/d-print.w (list-name).
  ELSE RUN custom/prntproc.p (list-name,INT(lv-font-no),lv-ornt).
END PROCEDURE.
PROCEDURE output-to-port:
    RUN custom/d-print.w (list-name).
END PROCEDURE.

PROCEDURE output-to-screen:
    IF is-xprint-form THEN DO:
     FILE-INFO:FILE-NAME = list-name.
     RUN printfile (FILE-INFO:FILE-NAME).
    END.
    ELSE OS-COMMAND SILENT VALUE('notepad.exe ' + list-name ).
END PROCEDURE.

PROCEDURE output-to-file:
       {custom/out2file.i}
END PROCEDURE.

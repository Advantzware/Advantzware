
/*------------------------------------------------------------------------
    File        : SignBolPrint.p
    Purpose     : SignBolPrint

    Syntax      :

    Description : Return a Dataset of all View SignBolPrint

    Author(s)   : Kuldeep
    Created     : Feb 01 2008
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttSignBolPrint NO-UNDO
        FIELD vSignBolFile        AS CHAR  .
DEFINE DATASET dsSignBolPrint FOR ttSignBolPrint .
DEFINE QUERY q-SignBolPrintQuery FOR ttSignBolPrint.
DEFINE DATA-SOURCE src-SignBolPrint  FOR QUERY q-SignBolPrintQuery.
BUFFER ttSignBolPrint :ATTACH-DATA-SOURCE(DATA-SOURCE src-SignBolPrint  :HANDLE).


   
DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER vRowid  as RECID  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsSignBolPrint.
{sys/inc/var.i new shared}  
 {oe/rep/oe-lad.i NEW}

{custom/xprint.i}


def var v-print-fmt     as char no-undo format 'x'.
DEF VAR lv-pdf-file     AS CHAR NO-UNDO.
DEF VAR vcBOLNums       AS CHAR NO-UNDO.
DEFINE VARIABLE v-title AS CHARACTER INITIAL "Print Signed Bills of Lading".
DEFINE VARIABLE begin_bol# AS INTEGER FORMAT ">>>>>>>>"  NO-UNDO.
DEFINE VARIABLE begin_cust AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_ord# AS INTEGER FORMAT ">>>>>>>>" NO-UNDO.
DEFINE VARIABLE end_bol# AS INTEGER FORMAT ">>>>>>>9"  NO-UNDO.
DEFINE VARIABLE end_cust AS CHARACTER FORMAT "X(8)" NO-UNDO.
DEFINE VARIABLE end_ord# AS INTEGER FORMAT ">>>>>>>>" NO-UNDO.
DEFINE VARIABLE tb_posted AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL NO-UNDO. 
DEFINE VARIABLE tb_export AS LOGICAL INITIAL no NO-UNDO.
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
def var list-name as cha format "X(100)" no-undo.
DEF VAR vPdfFile AS CHAR.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEF VAR tmp-path AS CHAR NO-UNDO .
DEF VAR v-VERSION AS CHAR NO-UNDO.

DEFINE NEW SHARED VARIABLE g_company AS CHAR NO-UNDO.
FOR EACH ttSignBolPrint :
    DELETE ttSignBolPrint.
END.

DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO .

DEFINE NEW SHARED TEMP-TABLE tt-filelist
    FIELD tt-FileCtr AS INT
    FIELD tt-FileName AS CHAR
INDEX filelist IS PRIMARY TT-FILECTR.

IF prmAction = ? THEN ASSIGN prmAction = "".


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
  

IF prmAction = "SignBol" THEN DO:
  FIND oe-boll WHERE RECID(oe-boll) = vRowid NO-LOCK.
  IF AVAILABLE oe-boll THEN DO:
      ASSIGN cocode = oe-boll.company
     locode = oe-boll.loc
     g_company = oe-boll.company
     .
    FIND FIRST oe-bolh OF oe-boll NO-LOCK NO-ERROR.
    IF AVAILABLE oe-bolh THEN DO:
       find first sys-ctrl where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "BOLSIGN"
                           no-lock no-error.
       if not avail sys-ctrl then do transaction:
          create sys-ctrl.
          ASSIGN sys-ctrl.company = cocode
                 sys-ctrl.name    = "BOLSIGN"
                 sys-ctrl.descrip = "Signed Bill of Lading Path"
                 sys-ctrl.char-fld = "C:\tmp\".
          MESSAGE sys-ctrl.descrip
                  VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                  UPDATE sys-ctrl.log-fld.
       end.

       DEF VAR v-bolimage AS cha NO-UNDO.
       DEF VAR v-bolpdf AS CHAR NO-UNDO.
       DEFINE VARIABLE v-webrootcopy AS CHARACTER NO-UNDO.

     ASSIGN   v-bolimage =  sys-ctrl.char-fld 
               v-bolpdf  =  sys-ctrl.char-fld .
     IF INDEX(v-bolimage ,'P',1) > 0 THEN ASSIGN
         v-bolimage  = REPLACE(v-bolimage ,'P',"D").
     IF INDEX(v-bolpdf ,'P',1) > 0 THEN ASSIGN
         v-bolpdf  = REPLACE(v-bolpdf ,'P',"D").

     ASSIGN
         v-bolpdf =  v-bolpdf + STRING(oe-bolh.bol-no) + ".pdf"
          v-bolimage =  v-bolimage + STRING(oe-bolh.bol-no) + ".jpg" .

     FIND FIRST sys-ctrl WHERE sys-ctrl.company = g_company AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootcopy = sys-ctrl.char-fld.
    ASSIGN v-webrootcopy =  v-webrootcopy + STRING(oe-bolh.bol-no) + ".pdf" .

     IF SEARCH(v-bolpdf) <> ? THEN DO:
         OS-COPY VALUE(v-bolpdf) VALUE (v-webrootcopy) .

             CREATE   ttSignBolPrint.
            ASSIGN ttSignBolPrint.vSignBolFile =STRING(oe-bolh.bol-no) + ".pdf"  .
            RETURN.

     END.

      
       IF SEARCH(v-bolimage) = ? THEN DO:
          MESSAGE "Signed receipt is not available. " .
         RETURN. 
       END.
      
       ASSIGN 
        begin_cust =  oe-bolh.cust-no
        end_cust   =  oe-bolh.cust-no
        begin_bol# =  oe-boll.bol-no
        end_bol#   =  oe-boll.bol-no
        tb_reprint =  oe-bolh.printed
        tb_posted  =  oe-bolh.posted
        begin_ord# = 0
        end_ord#   = 99999999.
         
        IF tb_posted THEN 
           ASSIGN tb_reprint = YES.
         
     FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
      IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.
         
         assign
            init-dir = v-webrootpath
            vcBOLNums       = vcBOLNums + '-' + string(begin_bol#)
             vcBOLNums       = LEFT-TRIM (vcBOLNums, '-').

         IF vcBOLNums MATCHES '*-*' THEN 
            vcBOLNums = RIGHT-TRIM (SUBSTRING (vcBOLNums, 1, INDEX (vcBOLNums,'-')), '-') + SUBSTRING (vcBOLNums, R-INDEX (vcBOLNums, '-')).
         
         
         assign
           lv-pdf-file = init-dir + "SIGNBOL"
           vPdfFile = 'SIGNBOL' + string(begin_bol#)+ '.pdf'.
          
         run run-report. 

         IF is-xprint-form THEN DO:
           IF v-VERSION = "Server 2008" THEN do:
               OS-COPY VALUE(list-name) VALUE (tmp-path).
               PAUSE 1.
           END.
           ELSE
               RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").                            
            
                                          
            CREATE   ttSignBolPrint.
            ASSIGN ttSignBolPrint.vSignBolFile = vPdfFile.
         END.
        END.
  END. /*IF AVAILABLE oe-boll THEN DO:*/

            
END.  /*IF prmAction = "SignBol" THEN DO:*/

PROCEDURE run-report :
   
/* --------------------------------------------- oe/rep/oe-lad.p 3/94 RM ---- */
/* print bill of ladings                                                      */
/* -------------------------------------------------------------------------- */
DEF BUFFER bf-oe-boll FOR oe-boll.
    
def var v-format as char no-undo.

{sys/form/r-top.i}

assign
 v-s-cust    = begin_cust
 v-e-cust    = end_cust
 v-s-bol     = begin_bol#
 v-e-bol     = end_bol#
 v-s-ord     = begin_ord#
 v-e-ord     = end_ord#
 v-printed   = tb_reprint
 v-print-bol = YES.

ASSIGN v-program      = "oe/rep/bolsign.p"
       is-xprint-form = YES
       lines-per-page = 66.

/*{sys/inc/print1.i} */

list-name = init-dir + "tmp" + string(time).

{sys/inc/outprint.i value(lines-per-page)}

{sa/sa-sls01.i}

v-term-id = v-term.

build-work:
for each oe-bolh NO-LOCK
    where oe-bolh.company eq cocode
      and oe-bolh.bol-no  ge v-s-bol
      and oe-bolh.bol-no  le v-e-bol
      and oe-bolh.cust-no ge v-s-cust
      and oe-bolh.cust-no le v-e-cust 
     /* and oe-bolh.printed eq v-printed
      and oe-bolh.posted  eq tb_posted
      AND can-find(FIRST oe-boll
                   WHERE oe-boll.company EQ oe-bolh.company
                     AND oe-boll.b-no    EQ oe-bolh.b-no
                     AND oe-boll.ord-no  GE v-s-ord
                     AND oe-boll.ord-no  LE v-e-ord)
    use-index post*/ .

  create report.
  assign
   
   report.term-id  = v-term-id
   report.key-01   = oe-bolh.cust-no
   report.key-02   = oe-bolh.ship-id
   report.rec-id   = recid(oe-bolh)
   report.key-09   = STRING(oe-bolh.printed,"REVISED/ORIGINAL")   
   .
end.

v-lines-per-page = lines-per-page.
IF IS-xprint-form THEN DO:
      IF v-print-fmt = "Century"
        THEN
        DO:
           v-format = "X(" + STRING(LENGTH("<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>"))
                    + ")".
           PUT "<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM v-format.
           DISPLAY WITH FRAME r-top.
        END.
        ELSE
        DO:
           v-format = "X(" + STRING(LENGTH("<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>"))
                    + ")".
           PUT "<PDF=DIRECT><PDF-OUTPUT=" + lv-pdf-file + vcBOLNums + ".pdf>" FORM v-format. 
           DISPLAY WITH FRAME r-top.
        end.
    END.

RUN value(v-program).

for each report where report.term-id eq v-term-id:
    delete report.
end.

OUTPUT CLOSE.

end procedure.


/*------------------------------------------------------------------------
    File        : TopPrintOrderAck_Report.p
    Purpose     :  Print Order Acknowledgement Report

    Syntax      :

    Description : Return a Dataset of Request For Print Order Acknowledgements

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttTopPrintOrderAckReport NO-UNDO
    FIELD vFile AS CHAR
    FIELD vvsdss AS CHAR.
DEFINE DATASET dsTopPrintOrderAckReport FOR ttTopPrintOrderAckReport.

    DEFINE INPUT PARAMETER prmUser              AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginOrder        AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrder          AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginCust         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndCust           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginOrdDate      AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmEndOrdDate        AS DATE NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginRel          AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmEndRel            AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmReprintAck        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmConsForm          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmWareHouse         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmMonths            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSchRel            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSpecNotes         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmShipAddr          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmActRel            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmPrintRevised      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBillMat           AS CHAR NO-UNDO.
    
    IF prmUser          = ?  THEN ASSIGN prmUser          = "".
    IF prmAction        = ?  THEN ASSIGN prmAction        = "".
    IF prmOut           = ?  THEN ASSIGN prmOut           = "".
    IF prmBeginOrder    = ?  THEN ASSIGN prmBeginOrder    = 0.
    IF prmEndOrder      = ?  THEN ASSIGN prmEndOrder      = 0.
    IF prmBeginCust     = ?  THEN ASSIGN prmBeginCust     = "".
    IF prmEndCust       = ?  THEN ASSIGN prmEndCust       = "".
    IF prmBeginRel      = ?  THEN ASSIGN prmBeginRel      = 0.
    IF prmEndRel        = ?  THEN ASSIGN prmEndRel        = 0.
    IF prmReprintAck    = ?  THEN ASSIGN prmReprintAck    = "".
    IF prmConsForm      = ?  THEN ASSIGN prmConsForm      = "".
    IF prmWareHouse     = ?  THEN ASSIGN prmWareHouse     = "".
    IF prmMonths        = ?  THEN ASSIGN prmMonths        = "".
    IF prmSchRel        = ?  THEN ASSIGN prmSchRel        = "".
    IF prmSpecNotes     = ?  THEN ASSIGN prmSpecNotes     = "".
    IF prmShipAddr      = ?  THEN ASSIGN prmShipAddr      = "".
    IF prmActRel        = ?  THEN ASSIGN prmActRel        = "".
    IF prmPrintRevised  = ?  THEN ASSIGN prmPrintRevised  = "".
    IF prmBillMat       = ?  THEN ASSIGN prmBillMat       = "".

  DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopPrintOrderAckReport.
 DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

 def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.
DEF VAR vcDefaultForm AS CHAR NO-UNDO.

DEF NEW SHARED VAR lv-qty AS INT NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

DEF NEW SHARED VAR s-committed-board-only AS LOG NO-UNDO.
DEF NEW SHARED VAR s-print-revised AS LOG NO-UNDO.
DEF NEW SHARED VAR LvOutputSelection AS CHAR NO-UNDO.
DEF NEW SHARED VAR v-rs-whs-mths AS CHAR NO-UNDO.
DEF NEW SHARED VAR v-tg-whs-mths AS LOG NO-UNDO.
DEF NEW SHARED VAR v-dept-codes AS CHAR NO-UNDO. 
DEF NEW SHARED VAR v-print-components AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-po AS LOG NO-UNDO.
DEF NEW SHARED VAR v-UntCnt AS LOG NO-UNDO.
DEF NEW SHARED VAR v-Shpnot AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-due AS LOG NO-UNDO.
DEF NEW SHARED VAR v-print-tot AS LOG NO-UNDO.
DEFINE VARIABLE d-print-fmt-dec  AS DECIMAL NO-UNDO.

DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.

{custom/xprint.i}
{sys/inc/var.i new shared}
{oe/rep/acknowl.i new}
{jcrep/r-ticket.i "new shared"}

DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>" INITIAL 44.


DEF STREAM excel.

DEFINE VARIABLE begin_cust-no AS CHARACTER FORMAT "X(8)"  NO-UNDO.
DEFINE VARIABLE begin_due-date AS DATE FORMAT "99/99/9999":U INITIAL 01/01/001  NO-UNDO.
DEFINE VARIABLE begin_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0  NO-UNDO.
DEFINE VARIABLE begin_relnum AS INTEGER FORMAT ">>>>>>>>" INITIAL 0  NO-UNDO.
DEFINE VARIABLE end_cust-no AS CHARACTER FORMAT "X(8)" INITIAL "zzzzzzzz"  NO-UNDO.
DEFINE VARIABLE end_due-date AS DATE FORMAT "99/99/9999":U INITIAL 12/31/9999  NO-UNDO.
DEFINE VARIABLE end_ord-no AS INTEGER FORMAT ">>>>>>>>":U INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE end_relnum AS INTEGER FORMAT ">>>>>>>>" INITIAL 99999999  NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=12 (10 cpi for 132 column Report)"  NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "15"  NO-UNDO.
DEFINE VARIABLE RS_whs-mths AS CHARACTER  NO-UNDO.
DEFINE VARIABLE tb_act-rel AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_inst AS LOGICAL INITIAL yes  no-undo.
DEFINE VARIABLE tb_prt-bom AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_prt-revise AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_sch-rel AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_ship-to AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE TG_cons_form AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE TG_whs-mths AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE rd-dest AS INTEGER INITIAL 2  NO-UNDO.
DEFINE VARIABLE tb_terms AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE lv-termFile AS CHARACTER FORMAT "X(256)":U  NO-UNDO.
DEFINE VARIABLE TG_print-pen-notes AS LOGICAL INITIAL no  NO-UNDO.

DEF VAR tmp-path AS CHAR NO-UNDO .
DEF VAR v-program AS CHAR NO-UNDO.
DEF VAR v-VERSION AS CHAR NO-UNDO.

DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.

DEF BUFFER b-oe-ord FOR oe-ord.
    DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND usercust.cust-no = prmBeginCust  NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Cust No should be same for Begin Cust and End Cust".
    RETURN.
END.
FIND FIRST usercust NO-LOCK WHERE usercust.company EQ prmComp
    AND usercust.user_id = prmUser
    AND (usercust.cust-no = prmEndCust OR prmEndCust = "zzzzzzzz") NO-ERROR.
IF NOT AVAIL usercust THEN DO:
    ASSIGN cError = "Cust No should be same for Begin Cust and End Cust".
    RETURN.
END. 


FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .
END.

assign
 cocode = prmComp
 g_company = prmComp
 locode = usercomp.loc
 g_loc     = "Main"
 v-today    = TODAY   .  

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

 ASSIGN
     begin_cust-no = prmBeginCust
     begin_due-date =  prmBeginOrdDate
     begin_ord-no = prmBeginOrder
     begin_relnum = prmBeginRel
     end_cust-no = prmEndCust
     end_due-date = prmEndOrdDate
     end_ord-no = prmEndOrder
     end_relnum = prmEndRel
     RS_whs-mths = prmMonths
     tb_act-rel = IF prmActRel = "Yes" THEN TRUE ELSE FALSE
     tb_inst = IF prmSpecNotes = "Yes" THEN TRUE ELSE FALSE
     tb_prt-bom = IF prmBillMat = "Yes" THEN TRUE ELSE FALSE
     tb_prt-revise = IF prmPrintRevised = "Yes" THEN TRUE ELSE FALSE
     tb_reprint = IF prmReprintAck = "Yes" THEN TRUE ELSE FALSE
     tb_sch-rel = IF prmSchRel = "Yes" THEN TRUE ELSE FALSE
     tb_ship-to = IF prmShipAddr = "Yes" THEN TRUE ELSE FALSE
     TG_cons_form = IF prmConsForm = "Yes" THEN TRUE ELSE FALSE
     TG_whs-mths = IF prmWareHouse = "Yes" THEN TRUE ELSE FALSE
        .
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    
ASSIGN
     ford-no   = begin_ord-no
     tord-no   = end_ord-no
     fdate     = begin_due-date
     tdate     = END_due-date
     v-schrel  = tb_sch-rel
     v-actrel  = tb_act-rel
     v-shipto  = tb_ship-to
     v-reprint = tb_reprint
     v-prntinst = tb_inst
     v-terms    = tb_terms
     v-termfile = lv-termFile.

/********************************main block*****************************/

find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "ACKHEAD"
      no-lock no-error.
IF AVAIL sys-ctrl THEN
        ASSIGN
            v-print-fmt  =  sys-ctrl.char-fld  /*"Frankstn"*/
            v-print-head = sys-ctrl.log-fld
            vcDefaultForm = v-print-fmt.

      RUN SetOEAckForm(v-print-fmt).
    
           ASSIGN
            TG_whs-mths  = NO
            RS_whs-mths     = "NO" .
            

            IF LOOKUP(v-print-fmt,"Century,Fibrex,Allwest") = 0 THEN 
                ASSIGN
                tb_prt-bom = NO  .

            IF v-print-fmt EQ "Allwest" THEN ASSIGN TG_cons_form  = YES.

            IF v-print-fmt EQ "Indiana" THEN
                ASSIGN
                tb_sch-rel = NO
                tb_act-rel = NO .

            IF v-print-fmt = "Dee" THEN
                ASSIGN
                TG_whs-mths = YES. 

            IF v-print-fmt EQ "Albert" THEN
                ASSIGN
                tb_prt-revise = NO  .

            
            IF v-print-fmt NE "Simkins"   THEN 
                ASSIGN 
                tb_terms    = NO
                lv-termFile = "NO".  

/***********************end main******************/
        
IF prmAction = "OrderAck" THEN DO:
    
  IF prmOut = "No" THEN DO:

      
      ASSIGN  
          init-dir    = v-webrootpath
          lv-pdf-file = init-dir + 'OrderAck' 
          lv-pdf-file = lv-pdf-file + prmBeginCust + STRING(TIME)
          vPdfFile   = 'OrderAck' + prmBeginCust + STRING(TIME) + '.pdf'.
      
      
      IF CAN-FIND(FIRST sys-ctrl-shipto WHERE
                  sys-ctrl-shipto.company = prmComp AND
                  sys-ctrl-shipto.NAME = "ACKHEAD") THEN
          DO:
         
          FIND  FIRST b-oe-ord
                      WHERE b-oe-ord.company  EQ prmComp
                      AND b-oe-ord.ord-no   GE ford-no
                      AND b-oe-ord.ord-no   LE tord-no
                      AND b-oe-ord.cust-no  GE begin_cust-no
                      AND b-oe-ord.cust-no  LE end_cust-no
                      AND b-oe-ord.ord-date GE fdate
                      AND b-oe-ord.ord-date LE tdate
                      AND b-oe-ord.ack-prnt EQ v-reprint NO-LOCK NO-ERROR .
              IF AVAIL b-oe-ord THEN DO:
                  
              FOR EACH b-oe-ord FIELDS(company cust-no ord-no) WHERE
              b-oe-ord.company  EQ prmComp AND
              b-oe-ord.ord-no   GE ford-no AND
              b-oe-ord.ord-no   LE tord-no AND
              b-oe-ord.cust-no  GE begin_cust-no AND
              b-oe-ord.cust-no  LE end_cust-no AND
              b-oe-ord.ord-date GE fdate AND
              b-oe-ord.ord-date LE tdate AND
              b-oe-ord.ack-prnt EQ v-reprint
              NO-LOCK
              BREAK BY b-oe-ord.company
              BY b-oe-ord.cust-no:
                 IF LOOKUP(b-oe-ord.cust-no, custcount) = 0 THEN NEXT.

              IF FIRST-OF(b-oe-ord.cust-no) THEN DO:
                  
                  FIND FIRST sys-ctrl-shipto WHERE
                      sys-ctrl-shipto.company = prmComp AND
                      sys-ctrl-shipto.NAME = "ACKHEAD" AND
                      sys-ctrl-shipto.cust-vend = YES AND
                      sys-ctrl-shipto.cust-vend-no = b-oe-ord.cust-no AND
                      sys-ctrl-shipto.char-fld > ''
                      NO-LOCK NO-ERROR.

                  IF AVAIL sys-ctrl-shipto THEN
                      DO:
                      RUN SetOEAckForm (sys-ctrl-shipto.char-fld).
                      v-print-fmt = sys-ctrl-shipto.char-fld.
                      
                      END.

                      ELSE  DO:
                          RUN SetOEAckForm (vcDefaultForm).
                          v-print-fmt = vcDefaultForm . 
                          
                          END.

                    RUN run-report(b-oe-ord.cust-no, TRUE).  
               
                      IF v-VERSION = "Server 2008" THEN do:
                          OS-COPY VALUE(list-name) VALUE (tmp-path).
                          PAUSE 15.
                      END.
                      ELSE do: 
                         RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
                      END.

                    CREATE ttTopPrintOrderAckReport.
                    ASSIGN ttTopPrintOrderAckReport.vFile = vPdfFile.
                    
                 END. /* end of cust*/
                 
           END. /* FOR EACH*/   
       END.  /* find first buffer */
       
       IF NOT AVAIL b-oe-ord THEN DO:
           ASSIGN
                 cError = "No Order Acknowledgements Were Printed." .
                 RETURN.
                 END.

               
     END. /*if can-find sys-ctrl-shipto*/ 
     ELSE DO:
        v-print-fmt = vcDefaultForm.
       
        /*RUN SetGlobalVariables(INPUT begin_ord-no).*/
        RUN run-report("", FALSE).
         IF v-VERSION = "Server 2008" THEN do:
             OS-COPY VALUE(list-name) VALUE (tmp-path).
             PAUSE 15.
         END.
         ELSE
             RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").
          
             
                    CREATE ttTopPrintOrderAckReport.
                    ASSIGN ttTopPrintOrderAckReport.vFile = vPdfFile.
        
     END.
     
    END. /*rnd of prmOut*/

END. /* end of prmAction */

PROCEDURE run-report :
/* --------------------------------------------------- po/po-print.p 10/94 rd */
/* Purchase Order Print Program - P/O Module                                  */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAM icCustNo AS CHAR NO-UNDO.
DEFINE INPUT PARAM ip-sys-ctrl-shipto AS LOG NO-UNDO.



{sys/form/r-top.i}


ASSIGN
 ford-no   = begin_ord-no
 tord-no   = end_ord-no
 fdate     = begin_due-date
 tdate     = END_due-date
 v-schrel  = tb_sch-rel
 v-actrel  = tb_act-rel
 v-shipto  = tb_ship-to
 v-reprint = tb_reprint
 v-prntinst = tb_inst
 v-rs-whs-mths = RS_whs-mths
 v-tg-whs-mths = TG_whs-mths
 v-print-pen-notes = TG_print-pen-notes
 .

IF ip-sys-ctrl-shipto THEN
   ASSIGN
      fcust = icCustNo
      tcust = icCustNo.
ELSE
   ASSIGN
      fcust = begin_cust-no
      tcust = end_cust-no.

{sa/sa-sls01.i}

IF v-print-fmt EQ "Allwest" AND
   TG_cons_form AND
   ford-no NE 0 AND
   tord-no NE 0 AND
   TRIM(tcust) EQ "" 
  THEN tcust = "zzzzzzzz".

FOR EACH oe-ord
    WHERE oe-ord.company  EQ cocode
      AND oe-ord.ord-no   GE ford-no
      AND oe-ord.ord-no   LE tord-no
      AND oe-ord.cust-no  GE fcust
      AND oe-ord.cust-no  LE tcust
      AND oe-ord.ord-date GE fdate
      AND oe-ord.ord-date LE tdate
      AND oe-ord.ack-prnt EQ v-reprint
    NO-LOCK:    

  CREATE report.
  ASSIGN
   report.term-id = v-term
   report.rec-id  = RECID(oe-ord).

END.


/*{sys/inc/print1.i}*/
if tmp-dir = "" then tmp-dir = v-webrootpath .
assign list-name = tmp-dir + "\tmpAck" + string(time)
       init-dir = tmp-dir.

{sys/inc/outprint.i VALUE(lines-per-page)}

/*IF td-show-parm THEN RUN show-param.*/

ASSIGN
 v-lines-per-page = lines-per-page
 v-term-id        = v-term.

list-name = init-dir + "tmpAck" + string(time).

            IF lookup(v-print-fmt,"Century,Unipak,APC,Fibrex,Allwest,Simkins") > 0 THEN
                PUT "<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=2mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(150)" "</PROGRESS>". 
            ELSE PUT "<PDF=DIRECT><PDF-EXCLUDE=MS Mincho><PDF-LEFT=5mm><PDF-TOP=10mm><PDF-OUTPUT=" + lv-pdf-file + ".pdf>" FORM "x(150)" "</PROGRESS>".
      
  /* PUT "</PROGRESS>".*/

   

IF CAN-DO("Frankstn,3CPack,Mirpkg,PPI,Indiana,ContSvc,HPB,Packrite",v-print-fmt) THEN RUN VALUE(v-program) (v-print-fmt).
ELSE IF LOOKUP(v-print-fmt,"Century,Unipak,Axis,Soule,SouleUOM,APC,Fibrex,Dee,Allwest,Accord") > 0 THEN RUN VALUE(v-program) (tb_prt-revise).
ELSE RUN VALUE(v-program).


OUTPUT CLOSE.

FOR EACH report WHERE report.term-id EQ v-term-id: 
  DELETE report.
END.


/*IF tb_prt-bom THEN DO:
   {sys/inc/outprt2.i 60 APPEND}
   RUN run-report-bom.
   OUTPUT CLOSE.
END.*/

/*RUN custom/usrprint.p (v-prgmname, FRAME {&FRAME-NAME}:HANDLE).*/


/* end ---------------------------------- copr. 2001 Advanced Software, Inc. */

end procedure.

PROCEDURE run-report-bom :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
     ASSIGN   fjob-no   = FILL(" ",6 - LENGTH(STRING(begin_ord-no))) +
                          STRING(begin_ord-no)
     tjob-no   = FILL(" ",6 - LENGTH(string(end_ord-no))) +
                 STRING(end_ord-no)
     fjob-no2  = 0
     tjob-no2  = 99
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99")
     s-print-revised = tb_prt-revise.
     RUN cerep/bomcbox.p .

END PROCEDURE.



PROCEDURE SetOEAckForm :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAM icPrintFormat  AS CHAR NO-UNDO.
   
v-print-fmt = icPrintFormat.

   CASE icPrintFormat:
       WHEN "WesInd" THEN ASSIGN v-program = "oe/rep/wiackn.p" is-xprint-form = NO lines-per-page = 60.
       WHEN "HOP" THEN ASSIGN tb_ship-to = NO v-program = "oe/rep/acknhop.p" is-xprint-form = NO lines-per-page = 45.
       WHEN "Brick" THEN ASSIGN v-program = "oe/rep/ackbrick.p" is-xprint-form = NO lines-per-page = 60.  
       WHEN "Gulf" THEN ASSIGN v-program = "oe/rep/ackgulf.p" is-xprint-form = NO lines-per-page = 55.
       WHEN "Pacific" THEN ASSIGN v-program = "oe/rep/ackpacif.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Hopx" THEN ASSIGN v-program = "oe/rep/ackhopx.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Carded" THEN ASSIGN v-program = "oe/rep/ackcared.p" is-xprint-form = YES lines-per-page = 68.  /* Task 09181303  */
       WHEN "XPrint" OR WHEN "ackhead 1" OR WHEN "ackhead 2" THEN ASSIGN v-program = "oe/rep/ackxprnt.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "ackhead 10" OR WHEN "ackhead 20" THEN ASSIGN v-program = "oe/rep/ackxprnt10.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Shamrock-Ack" THEN ASSIGN v-program = "oe/rep/ackshamrock.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "ackhead10-CAN" THEN ASSIGN v-program = "oe/rep/ackcan.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Badger" THEN ASSIGN v-program = "oe/rep/ackbager.p" is-xprint-form = YES lines-per-page = 67. /* task 04021401 */
       WHEN "Hughes" THEN ASSIGN v-program = "oe/rep/ackhughes.p" is-xprint-form = YES lines-per-page = 65. 
       WHEN "Accord" THEN ASSIGN v-program = "oe/rep/ackacord.p" is-xprint-form = YES lines-per-page = 72.
       WHEN "Imperial" THEN ASSIGN v-program = "oe/rep/ackimprl.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Ruddx" THEN ASSIGN v-program = "oe/rep/ackruddx.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Shelby" THEN ASSIGN v-program = "oe/rep/ackshlby.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "PremierX" THEN ASSIGN v-program = "oe/rep/ackxprem.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "ACPI" THEN ASSIGN v-program = "oe/rep/ackacpi.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "PremierCX" THEN ASSIGN v-program = "oe/rep/ackcxprem.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Century" THEN ASSIGN v-program = "oe/rep/ackcentx.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "APC" THEN ASSIGN v-program = "oe/rep/ackxapc.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Perform" THEN ASSIGN v-program = "oe/rep/ackprfrm.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Unipak" THEN ASSIGN v-program = "oe/rep/ackunipk.p" is-xprint-form = YES lines-per-page = 70.
       WHEN "Axis" THEN ASSIGN v-program = "oe/rep/ackaxis.p" is-xprint-form = YES lines-per-page = 68.
       WHEN "Soule" THEN ASSIGN v-program = "oe/rep/acksoule.p" is-xprint-form = YES lines-per-page = 69. /*Soule */
       WHEN "SouleUOM" THEN ASSIGN v-program = "oe/rep/acksolUom.p" is-xprint-form = YES lines-per-page = 65. /*SouleUOM */ /*12031306*/
       WHEN "Oracle" THEN ASSIGN v-program = "oe/rep/ackoracl.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "OTTPkg" THEN ASSIGN v-program = "oe/rep/ackottpk.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Frankstn" OR WHEN "MirPkg" THEN ASSIGN v-program = "oe/rep/ackfrank.p" is-xprint-form = YES
                                                    lines-per-page = 65.
       WHEN "3CPack" THEN ASSIGN v-program = "oe/rep/ack3cpak.p" is-xprint-form = YES lines-per-page = 75.
       WHEN "3CPackSD" THEN ASSIGN v-program = "oe/rep/ack3cpaksd.p" is-xprint-form = YES lines-per-page = 75.
       WHEN "HPB" THEN ASSIGN v-program = "oe/rep/ackhpb.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "PPI" THEN ASSIGN v-program = "oe/rep/ackppi.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Southpak" THEN ASSIGN v-program = "oe/rep/acksthpk.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Indiana"  THEN ASSIGN v-program = "oe/rep/ackindc.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Fibrex" THEN ASSIGN v-program = "oe/rep/ackfibrex.p" is-xprint-form = YES lines-per-page = 69.
       WHEN "Albert" THEN ASSIGN v-program = "oe/rep/ackalbert.p" is-xprint-form = NO.
       WHEN "ContSvc" THEN ASSIGN v-program = "oe/rep/ackcontsvc.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Triad" THEN ASSIGN v-program = "oe/rep/acktriad.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Packrite" THEN ASSIGN v-program = "oe/rep/ackpkrit.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Dee" THEN ASSIGN v-program = "oe/rep/ackdee.p" is-xprint-form = YES lines-per-page = 69.
       WHEN "Allwest" THEN ASSIGN v-program = "oe/rep/ackallws.p" is-xprint-form = YES lines-per-page = 65.
       WHEN "Simkins" THEN ASSIGN v-program = "oe/rep/acksimkn.p" is-xprint-form = YES lines-per-page = 65.
       OTHERWISE ASSIGN v-program = "oe/rep/ackasi.p" is-xprint-form = NO lines-per-page = 55.
   END.
   
END PROCEDURE.


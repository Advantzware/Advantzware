/*------------------------------------------------------------------------
    File        : AgeinvRep.p
    Purpose     : Inventory Report
    Main File   : fgrep/r-ageinv.w

    Syntax      :

    Description : Return a Dataset of Inventory Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


DEFINE TEMP-TABLE ttRticketRep NO-UNDO
    FIELD ticketfile AS CHAR
    FIELD vCorr      AS CHAR
    FIELD vFold      AS CHAR
    FIELD vForcor    AS CHAR
    FIELD vForfold   AS CHAR
    FIELD vTicket    AS CHAR .

DEFINE DATASET dsRticketRep FOR ttRticketRep.
    
    DEFINE INPUT PARAMETER prmUser              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAction            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmbeginJob1         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmbeginJob2         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmendJob1           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmendJob2           AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmtbFold            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbRS              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbCorr            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPR              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbReprint         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbDC              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbBox             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbGL              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbSW              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbApprove         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmspecCodes         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmrevsnNo           AS INT  NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPrtLabel        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbCommitted       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPrtSetHeader    AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPromptShip      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmdeptCodes         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbFreezeNote      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbDeptNote        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmTBSampleReq       AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmflJobord          AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmrdPrintSpeed      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbFgimage         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbMakeHold        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPrtMch          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPrtSellprc      AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbTray2           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbAppUunprinted   AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPrtRev          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmtbPrtShipto       AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER cError              AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsRticketRep.
     
    IF prmUser           = ? THEN ASSIGN prmUser            = "".
    IF prmAction         = ? THEN ASSIGN prmAction          = "".
    IF prmbeginJob1      = ? THEN ASSIGN prmbeginJob1       = "".
    IF prmbeginJob2      = ? THEN ASSIGN prmbeginJob2       = 0.
    IF prmendJob1        = ? THEN ASSIGN prmendJob1         = "".
    IF prmendJob2        = ? THEN ASSIGN prmendJob2         = 0.
    IF prmtbFold         = ? THEN ASSIGN prmtbFold          = "".
    IF prmtbRS           = ? THEN ASSIGN prmtbRS            = "".
    IF prmtbCorr         = ? THEN ASSIGN prmtbCorr          = "".
    IF prmtbPR           = ? THEN ASSIGN prmtbPR            = "".
    IF prmtbReprint      = ? THEN ASSIGN prmtbReprint       = "".
    IF prmtbDC           = ? THEN ASSIGN prmtbDC            = "".
    IF prmtbBox          = ? THEN ASSIGN prmtbBox           = "". 
    IF prmtbGL           = ? THEN ASSIGN prmtbGL            = "". 
    IF prmtbSW           = ? THEN ASSIGN prmtbSW            = "". 
    IF prmtbApprove      = ? THEN ASSIGN prmtbApprove       = "".  
    IF prmspecCodes      = ? THEN ASSIGN prmspecCodes       = "". 
    IF prmrevsnNo        = ? THEN ASSIGN prmrevsnNo         = 0. 
    IF prmtbPrtLabel     = ? THEN ASSIGN prmtbPrtLabel      = "". 
    IF prmtbCommitted    = ? THEN ASSIGN prmtbCommitted     = "". 
    IF prmtbPrtSetHeader = ? THEN ASSIGN prmtbPrtSetHeader  = "". 
    IF prmtbPromptShip   = ? THEN ASSIGN prmtbPromptShip    = "". 
    IF prmdeptCodes      = ? THEN ASSIGN prmdeptCodes       = "". 
    IF prmtbFreezeNote   = ? THEN ASSIGN prmtbFreezeNote    = "". 
    IF prmtbDeptNote     = ? THEN ASSIGN prmtbDeptNote      = "". 
    IF prmTBSampleReq    = ? THEN ASSIGN prmTBSampleReq     = "". 
    IF prmflJobord       = ? THEN ASSIGN prmflJobord        = 0.
    IF prmrdPrintSpeed       = ? THEN ASSIGN prmrdPrintSpeed = "".
    IF prmtbAppUunprinted = ? THEN ASSIGN prmtbAppUunprinted = "".
    IF prmtbFgimage      = ? THEN ASSIGN prmtbFgimage = "".
    IF prmtbDeptNote = ? THEN ASSIGN prmtbDeptNote = "".
    IF prmtbMakeHold = ? THEN ASSIGN prmtbMakeHold = "".
    IF prmtbPrtMch = ? THEN ASSIGN prmtbPrtMch = "".
    IF prmtbPrtRev = ? THEN ASSIGN prmtbPrtRev = "".
    IF prmtbPrtSellprc = ? THEN ASSIGN prmtbPrtSellprc = "".
    IF prmtbTray2 = ? THEN ASSIGN prmtbTray2 = "".
    IF prmtbPrtShipto = ? THEN ASSIGN prmtbPrtShipto = "".

DEFINE VARIABLE begin_job1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE begin_job2 AS INTEGER NO-UNDO.
DEFINE VARIABLE dept_codes AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_job1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE end_job2 AS INTEGER NO-UNDO.
DEFINE VARIABLE fl-jobord AS INTEGER NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER NO-UNDO.
DEFINE VARIABLE revsn_no AS INTEGER NO-UNDO.
DEFINE VARIABLE spec_codes AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-ornt AS CHARACTER NO-UNDO.
DEFINE VARIABLE rd-dest AS INTEGER NO-UNDO.
DEFINE VARIABLE rd_print-speed AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_app-unprinted AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_approve AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_box AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_committed AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_corr AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_DC AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_fgimage AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_fold AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_freeze-note AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_dept-note AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_GL AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_make_hold AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_PR AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prompt-ship AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prt-label AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prt-mch AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prt-rev AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prt-sellprc AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prt-set-header AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_prt-shipto AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_RS AS LOGICAL NO-UNDO.
DEFINE VARIABLE TB_sample_req AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_SW AS LOGICAL NO-UNDO.
DEFINE VARIABLE tb_tray-2 AS LOGICAL NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF STREAM excel.

{custom/xprint.i}
{sys/inc/var.i new shared}

def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS CHAR NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.


DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
ASSIGN 
    cocode = prmComp
    g_company = prmComp .

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser  AND
     usercomp.loc <> "" AND
     usercomp.company = prmComp
     NO-LOCK NO-ERROR.

 locode   = IF AVAIL usercomp THEN usercomp.loc ELSE "MAIN" .
ASSIGN
    g_loc  = locode .

DEF NEW SHARED VAR v-dept-log AS LOG NO-UNDO.
DEF NEW SHARED VAR v-dept-codes AS CHAR NO-UNDO.
DEF NEW SHARED VAR lv-qty AS int NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR lv-format-c AS CHAR NO-UNDO.
DEF VAR lv-default-f AS CHAR NO-UNDO.
DEF VAR lv-default-c AS CHAR NO-UNDO.
DEF VAR lv-int-f AS INT NO-UNDO.
DEF VAR lv-int-c AS INT NO-UNDO.

{jcrep/r-ticket.i "new shared"}

DEF NEW SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-revno AS LOG NO-UNDO.
DEF NEW SHARED VAR revision-no AS CHAR NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF NEW SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF NEW SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF NEW SHARED VAR s-committed-board-only AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF NEW SHARED VAR s-sample-required AS LOG NO-UNDO.

DEF VAR lv-save-spec AS CHAR NO-UNDO.
DEF VAR v-freezenotes-log AS LOG NO-UNDO.
DEF VAR v-freezenote-log AS LOG NO-UNDO.
DEF VAR v-freezenotes-pass AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE fi_file AS CHARACTER FORMAT "X(30)" NO-UNDO.
DEFINE VARIABLE v-today AS DATETIME FORMAT "9999/99/99" NO-UNDO.

{cerep/jc-keyst.i "NEW"}
{cerep/jc-keys2.i "NEW"}
{cecrep/jc-prem.i "NEW"}
{cecrep/jc-fibre.i "NEW"}
{cecrep/jc-pallet.i "NEW"}
/*{cecrep/tt-artios.i "NEW"}*/
{cerep/tt-samp-ctn.i "NEW"}

DEF TEMP-TABLE t-ef-form
   FIELD form-no LIKE ef.form-no.

DEF NEW SHARED VAR s-prt-ship-split AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-label AS LOG NO-UNDO.

{ cerep/tt-wrk-ink.i "NEW SHARED" }

/* gdm - 10010805 */
DEF TEMP-TABLE tt-specCd NO-UNDO
    FIELD tt-char-val AS CHAR
    INDEX chr-1 tt-char-val.
/* gdm - 11030807 */
DEF NEW SHARED VAR v-newdie   AS LOG NO-UNDO INIT FALSE.
DEF NEW SHARED VAR v-newfilm  AS LOG NO-UNDO INIT FALSE.
DEF NEW SHARED VAR v-newcombo AS LOG NO-UNDO INIT FALSE.
DEF BUFFER b-reftable-freeze FOR reftable.
DEF BUFFER b-reftable-split FOR reftable.
DEF VAR v-VERSION AS CHAR NO-UNDO. 
DEF VAR  tmp-path AS CHAR NO-UNDO. 
DEF VAR Ticket_check AS CHAR NO-UNDO .

ASSIGN Ticket_check = "no" .

IF prmAction = "validatejobticket" THEN DO:


  assign
    begin_job1      = prmbeginJob1     
    begin_job2      = prmbeginJob2
    end_job1        = prmendJob1       
    end_job2        = prmendJob2  .
  
    FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTES"
      NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.NAME     = "FREEZENOTES"
     sys-ctrl.module   = "OU5"
     sys-ctrl.descrip = "Default Toggle to User's Last Action?".
  end.

  v-freezenotes-log = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTE"
      NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.NAME     = "FREEZENOTE"
     sys-ctrl.module   = "OU5"
     sys-ctrl.descrip = "Prompt for password on Freeze Notes?".
  end.

  v-freezenote-log = sys-ctrl.log-fld.

  IF v-freezenote-log THEN
    v-freezenotes-pass = sys-ctrl.char-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  ASSIGN
   tb_fold = NOT AVAIL sys-ctrl           OR
             sys-ctrl.char-fld EQ "Both"  OR
             sys-ctrl.char-fld EQ "Foldware"
   tb_corr = AVAIL sys-ctrl AND
             (sys-ctrl.char-fld EQ "Both" OR sys-ctrl.char-fld EQ "Corrware").
  
   {sys/inc/jobcard.i "F"}
   ASSIGN
    lv-format-f = sys-ctrl.char-fld
    lv-int-f    = sys-ctrl.int-fld
    lv-default-f = sys-ctrl.char-fld.
   IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0*/
       lookup(lv-format-f,"Interpac,FibreFC,HPB,metro,Dayton,Livngstn,CentBox,Frankstn,Colonial,Unipak,Ottpkg,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Dee,Carded") > 0 THEN lines-per-page = 55.
  
   {sys/inc/jobcard.i "C"}
   ASSIGN
    lv-format-c = sys-ctrl.char-fld
    lv-int-c    = sys-ctrl.int-fld
    lv-default-c = sys-ctrl.char-fld .   

    IF TRIM(begin_job1) NE ""                          AND
       TRIM(begin_job1) EQ TRIM(end_job1) /*AND
       INT(begin_job2)  EQ INT(end_job2)*/  THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1)) +
                                   TRIM(begin_job1)
            AND job-hdr.job-no2 EQ INT(begin_job2)
          NO-ERROR.
    IF AVAIL job-hdr THEN do:
     FIND FIRST job
        WHERE job.company                   EQ cocode
          AND job.job                       EQ job-hdr.job
          AND job.job-no                    EQ job-hdr.job-no
          AND job.job-no2                   EQ job-hdr.job-no2
          AND job.stat                      NE "H"
        NO-LOCK NO-ERROR.
     ASSIGN Ticket_check = string(job-hdr.ftick-prn) .
    END.

    IF AVAIL job THEN 
    FIND FIRST est
        WHERE est.company = job.company
          AND est.est-no                    EQ job.est-no
        NO-LOCK NO-ERROR.
     
     
   
    
    ASSIGN
         tb_fold = NO
         tb_corr = NO.
    IF AVAIL est THEN
        IF est.est-type LE 4 THEN tb_fold = YES.
                           ELSE tb_corr = YES.
        
      IF AVAIL job-hdr THEN DO:
             FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = "JOBCARDC" AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                           /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-c = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-c = lv-default-c .

              FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "JOBCARDF" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                    /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.
                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-f = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-f = lv-default-f .
      END.
    END.

    CREATE ttRticketRep .
    ASSIGN
        ttRticketRep.ticketfile  = ""
        ttRticketRep.vCorr       = string(tb_corr)
        ttRticketRep.vFold       = string(tb_fold)
        ttRticketRep.vForcor     = lv-format-c 
        ttRticketRep.vForfold    = lv-format-f  
        ttRticketRep.vTicket     = Ticket_check
                    .





END.

IF prmAction = "jobticket" THEN DO:

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



FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

    assign
    begin_job1      = prmbeginJob1     
    begin_job2      = prmbeginJob2
    dept_codes      = prmdeptCodes     
    end_job1        = prmendJob1       
    end_job2        = prmendJob2       
    fl-jobord       = prmflJobord      
   
    revsn_no        = prmrevsnNo       
    spec_codes      = prmspecCodes     
    lv-ornt         = "L"        
          
    rd_print-speed  = prmrdPrintSpeed  .
      
   
   
   tb_app-unprinted = IF prmtbAppUunprinted = "True" THEN TRUE ELSE FALSE.
   tb_approve = IF prmtbApprove = "True" THEN TRUE ELSE FALSE.
   tb_box = IF prmtbbox = "True" THEN TRUE ELSE FALSE.
   tb_committed = IF prmtbCommitted = "True" THEN TRUE ELSE FALSE.
   tb_corr = IF prmtbCorr = "True" THEN TRUE ELSE FALSE.
   tb_DC = IF prmtbDC = "True" THEN TRUE ELSE FALSE.
   tb_fgimage = IF prmtbFgimage = "True" THEN TRUE ELSE FALSE.
   tb_Fold = IF prmtbFold = "True" THEN TRUE ELSE FALSE.
   tb_freeze-note = IF prmtbFreezeNote = "True" THEN TRUE ELSE FALSE.
   tb_dept-note = IF prmtbDeptNote = "True" THEN TRUE ELSE FALSE.
   tb_GL = IF prmtbGL = "True" THEN TRUE ELSE FALSE.
   tb_make_hold = IF prmtbMakeHold = "True" THEN TRUE ELSE FALSE.  
   tb_PR = IF prmtbPr = "True" THEN TRUE ELSE FALSE.  
   tb_prompt-ship = IF prmtbPromptShip = "True" THEN TRUE ELSE FALSE.  
   tb_prt-label = IF prmtbPrtLabel = "True" THEN TRUE ELSE FALSE.  
   tb_prt-mch = IF prmtbPrtMch = "True" THEN TRUE ELSE FALSE.
   tb_prt-rev = IF prmtbPrtRev = "True" THEN TRUE ELSE FALSE.  
   tb_prt-sellprc = IF prmtbPrtSellprc = "True" THEN TRUE ELSE FALSE.  
   tb_prt-set-header = IF prmtbPrtSetHeader = "True" THEN TRUE ELSE FALSE.  
   tb_prt-shipto = IF prmtbPrtShipto = "True" THEN TRUE ELSE FALSE.  
   tb_reprint = IF prmtbReprint = "True" THEN TRUE ELSE FALSE.
   tb_RS = IF prmtbRS = "True" THEN TRUE ELSE FALSE.  
   TB_sample_req = IF prmTBSampleReq = "True" THEN TRUE ELSE FALSE.  
   tb_SW = IF prmtbSW = "True" THEN TRUE ELSE FALSE.  
   tb_tray-2 = IF prmtbTray2 = "True" THEN TRUE ELSE FALSE.  
   
    assign
       init-dir    = v-webrootpath .
        
    ASSIGN lv-pdf-file = v-webrootpath + "ticket" + begin_job1 + STRING(TIME) .
           
       ASSIGN
           vPdfFile =  "ticket" + begin_job1 + STRING(TIME) + ".pdf".
       



  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTES"
      NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.NAME     = "FREEZENOTES"
     sys-ctrl.module   = "OU5"
     sys-ctrl.descrip = "Default Toggle to User's Last Action?".
  end.

  v-freezenotes-log = sys-ctrl.log-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "FREEZENOTE"
      NO-LOCK NO-ERROR.

  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
     create sys-ctrl.
     assign
     sys-ctrl.company  = cocode
     sys-ctrl.NAME     = "FREEZENOTE"
     sys-ctrl.module   = "OU5"
     sys-ctrl.descrip = "Prompt for password on Freeze Notes?".
  end.

  v-freezenote-log = sys-ctrl.log-fld.

  IF v-freezenote-log THEN
    v-freezenotes-pass = sys-ctrl.char-fld.

  FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "CEMENU"
      NO-LOCK NO-ERROR.
  
   {sys/inc/jobcard.i "F"}
   ASSIGN
    lv-format-f = sys-ctrl.char-fld
    lv-int-f    = sys-ctrl.int-fld
    lv-default-f = sys-ctrl.char-fld.
   IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0*/
       lookup(lv-format-f,"Interpac,FibreFC,HPB,metro,Dayton,Livngstn,CentBox,Frankstn,Colonial,Unipak,Ottpkg,Shelby,CCC,Indiana-XL,PPI,PackRite,Rosmar,Accord,Knight,MidYork,Dee,Carded") > 0 THEN lines-per-page = 55.
  
   {sys/inc/jobcard.i "C"}
   ASSIGN
    lv-format-c = sys-ctrl.char-fld
    lv-int-c    = sys-ctrl.int-fld
    lv-default-c = sys-ctrl.char-fld .   

    IF TRIM(begin_job1) NE ""                          AND
       TRIM(begin_job1) EQ TRIM(end_job1) /*AND
       INT(begin_job2)  EQ INT(end_job2)*/  THEN DO:
      FIND FIRST job-hdr NO-LOCK
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1)) +
                                   TRIM(begin_job1)
            AND job-hdr.job-no2 EQ INT(begin_job2)
          NO-ERROR.
       IF AVAIL job-hdr THEN
           FIND FIRST job
           WHERE job.company                   EQ cocode
           AND job.job                       EQ job-hdr.job
           AND job.job-no                    EQ job-hdr.job-no
           AND job.job-no2                   EQ job-hdr.job-no2
           AND job.stat                      NE "H"
           NO-LOCK NO-ERROR.
       IF AVAIL job THEN
           FIND FIRST est
           WHERE est.company = job.company
           AND est.est-no                    EQ job.est-no
           NO-LOCK NO-ERROR.
       ASSIGN
           tb_fold = NO
           tb_corr = NO.
       
       IF AVAIL est THEN
           IF est.est-type LE 4 THEN tb_fold = YES.
           ELSE tb_corr = YES.
     
      IF AVAIL job-hdr THEN DO:
             FIND FIRST sys-ctrl-shipto WHERE
                           sys-ctrl-shipto.company = cocode AND
                           sys-ctrl-shipto.NAME = "JOBCARDC" AND
                           sys-ctrl-shipto.cust-vend = YES AND
                           sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                           /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                           sys-ctrl-shipto.char-fld > ''
                           NO-LOCK NO-ERROR.

                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-c = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-c = lv-default-c .

              FIND FIRST sys-ctrl-shipto WHERE
                    sys-ctrl-shipto.company = cocode AND
                    sys-ctrl-shipto.NAME = "JOBCARDF" AND
                    sys-ctrl-shipto.cust-vend = YES AND
                    sys-ctrl-shipto.cust-vend-no = job-hdr.cust-no AND
                    /*sys-ctrl-shipto.ship-id = job-hdr.ship-id AND*/
                    sys-ctrl-shipto.char-fld > ''
                    NO-LOCK NO-ERROR.
                IF AVAIL sys-ctrl-shipto THEN 
                    lv-format-f = sys-ctrl-shipto.char-fld .
                ELSE 
                    lv-format-f = lv-default-f .
      END.
    END.




  IF tb_fold THEN DO:
    /*lines-per-page = IF lv-format-f EQ "HOP" THEN 64 ELSE 58. */

    RUN run-report ("Fold").

      END.

  IF tb_corr THEN DO:
    /*lines-per-page = 0. ??? */

    RUN run-report ("Corr").

 END.
  
   IF v-VERSION = "Server 2008" THEN do:
            OS-COPY VALUE(list-name) VALUE (tmp-path). 
            PAUSE 1.
        END.
        ELSE 
             RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

    CREATE ttRticketRep.
     ASSIGN ttRticketRep.ticketfile = vPdfFile.
   
END.


PROCEDURE run-report :
/* ---------------------------------------------- fg/rep/fg-aging.p 12/96 JLF */
/* finished goods aged inventory report                                       */
/* -------------------------------------------------------------------------- */

DEF INPUT PARAM ip-industry AS CHAR NO-UNDO.

  DEF BUFFER b-reftable FOR reftable.
  DEF BUFFER b-eb FOR eb.
  DEF VAR li AS INT NO-UNDO.
  DEF VAR v-prgmname AS CHAR NO-UNDO.

  
  {sys/form/r-top.i}
  
  RUN set-job-vars.
  
  ASSIGN 
    print-box               = tb_box
    reprint                 = tb_reprint
    s-prt-fgimage           = tb_fgimage
    s-prt-label             = tb_prt-label
    s-committed-board-only  = tb_committed
    s-prt-set-header        = tb_prt-set-header
    spec-list               = spec_codes
    s-prt-ship-split        = tb_prompt-ship
    approve                 = tb_approve    
    s-prt-revno             = tb_prt-rev 
    v-dept-log             = tb_dept-note
    v-dept-codes            = dept_codes. 

  IF s-prt-revno THEN
      ASSIGN revision-no             = string(revsn_no).
  ELSE
      ASSIGN revision-no             = "".
    
  FOR EACH wrk-ink:
      DELETE wrk-ink.
  END.

  {jcrep/tickrrpt.i}

  /*RUN custom/usrprint.p (v-prgmname).*/

  /*share settings between the different ways this program is called*/
  
  SESSION:SET-WAIT-STATE ("").

end procedure.

PROCEDURE set-job-vars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  
    ASSIGN
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(begin_job1))) + TRIM(begin_job1)
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(end_job1))) +
                 trim(end_job1)
     fjob-no2  = INT(begin_job2)
     tjob-no2  = INT(end_job2)
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99").
  

END PROCEDURE.

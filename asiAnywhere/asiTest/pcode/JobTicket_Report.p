/*------------------------------------------------------------------------
    File        : JobTicket_Report.p
    Purpose     :  Print Order Acknowledgement Report

    Syntax      :

    Description : Return a Dataset of Request For Print Order Acknowledgements

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
MESSAGE "enter" .
DEFINE TEMP-TABLE ttTopJobTicketReport NO-UNDO
    FIELD vFile AS CHAR
    FIELD iuyiu  AS CHAR.
DEFINE DATASET dsTopJobTicketReport FOR ttTopJobTicketReport.

    DEFINE INPUT PARAMETER prmUser              AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction            AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmOut               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginJob          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmEndJob            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBeginJob2         AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmEndJob2           AS INT NO-UNDO.
    DEFINE INPUT PARAMETER prmFoldCat           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCorrugated        AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmReTicket          AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBoxDes            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmFGImage           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmAppJobs           AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCopy              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmCode              AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmMachineSt         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmMachSpeed         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmShipto            AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmUpc               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmLabelinfo         AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmBoard             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSet               AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmSplit             AS CHAR NO-UNDO.
    DEFINE INPUT PARAMETER prmJobNot            AS CHAR NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsTopJobTicketReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.
    
    IF prmUser         = ?  THEN ASSIGN prmUser        = "".
    IF prmAction       = ?  THEN ASSIGN prmAction      = "".
    IF prmOut          = ?  THEN ASSIGN prmOut         = "".
    IF prmBeginJob     = ?  THEN ASSIGN prmBeginJob    = "".
    IF prmEndJob       = ?  THEN ASSIGN prmEndJob      = "".
    IF prmBeginJob2    = ?  THEN ASSIGN prmBeginJob2   = 0.
    IF prmEndJob2      = ?  THEN ASSIGN prmEndJob2     = 0.
    IF prmFoldCat      = ?  THEN ASSIGN prmFoldCat     = "".
    IF prmCorrugated   = ?  THEN ASSIGN prmCorrugated  = "".
    IF prmReTicket     = ?  THEN ASSIGN prmReTicket    = "".
    IF prmBoxDes       = ?  THEN ASSIGN prmBoxDes      = "".
    IF prmFGImage      = ?  THEN ASSIGN prmFGImage     = "".
    IF prmAppJobs      = ?  THEN ASSIGN prmAppJobs     = "".
    IF prmCopy         = ?  THEN ASSIGN prmCopy        = "".
    IF prmCode         = ?  THEN ASSIGN prmCode        = "".
    IF prmMachineSt    = ?  THEN ASSIGN prmMachineSt   = "".
    IF prmMachSpeed    = ?  THEN ASSIGN prmMachSpeed   = "".
    IF prmShipto       = ?  THEN ASSIGN prmShipto      = "".
    IF prmUpc          = ?  THEN ASSIGN prmUpc         = "".
    IF prmLabelinfo    = ?  THEN ASSIGN prmLabelinfo   = "".
    IF prmBoard        = ?  THEN ASSIGN prmBoard       = "".
    IF prmSet          = ?  THEN ASSIGN prmSet         = "".
    IF prmSplit        = ?  THEN ASSIGN prmSplit       = "".
    IF prmJobNot       = ?  THEN ASSIGN prmJobNot      = "".
   
  
 def var list-name as cha no-undo.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEF VAR lv-pdf-file AS cha NO-UNDO.
DEFINE VAR vPdfFile AS CHAR NO-UNDO.

DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-excel-file    AS CHARACTER FORMAT "X(100)":U      NO-UNDO.
DEF VAR v-dir AS CHAR FORMAT "X(80)" NO-UNDO.
DEFINE VAR li AS INT NO-UNDO.

{sys/inc/var.i new shared}

DEF NEW SHARED VAR lv-qty AS int NO-UNDO.
DEF NEW SHARED VAR qty AS INT NO-UNDO.
DEF VAR lv-format-f AS CHAR NO-UNDO.
DEF VAR lv-format-c AS CHAR NO-UNDO.
DEF VAR lv-int-f AS INT NO-UNDO.
DEF VAR lv-int-c AS INT NO-UNDO.

{jcrep/r-ticket.i "new shared"}

{custom/xprint.i}

DEF NEW SHARED VAR s-prt-fgimage AS LOG NO-UNDO.
DEF VAR is-xprint-form AS LOG NO-UNDO.
DEF VAR ls-fax-file AS cha NO-UNDO.

DEF NEW SHARED VAR s-prt-mstandard AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-shipto AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-sellprc AS LOG NO-UNDO.
DEF NEW SHARED VAR s-run-speed AS LOG NO-UNDO.
DEF NEW SHARED VAR s-committed-board-only AS LOG NO-UNDO.
DEF NEW SHARED VAR s-prt-set-header AS LOG NO-UNDO.
DEF NEW SHARED VAR s-sample-required AS LOG NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.


DEF VAR lv-save-spec AS CHAR NO-UNDO.

{cerep/jc-keyst.i "NEW"}
{cerep/jc-keys2.i "NEW"}
{cecrep/jc-prem.i "NEW"}
{cecrep/jc-fibre.i "NEW"}
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
DEF VAR plev AS INT NO-UNDO.

DEFINE VARIABLE begin_job1 AS CHARACTER FORMAT "x(6)" NO-UNDO.
DEFINE VARIABLE begin_job2 AS INTEGER FORMAT ">9" INITIAL 0  NO-UNDO.
DEFINE VARIABLE end_job1 AS CHARACTER FORMAT "x(6)" INITIAL "zzzzzz"  NO-UNDO.
DEFINE VARIABLE end_job2 AS INTEGER FORMAT ">9" INITIAL 99  NO-UNDO.
DEFINE VARIABLE lines-per-page AS INTEGER FORMAT ">>":U INITIAL 99  NO-UNDO.
DEFINE VARIABLE spec_codes AS CHARACTER FORMAT "X(256)":U INITIAL "QA"  NO-UNDO.
DEFINE VARIABLE rd_print-speed AS CHARACTER NO-UNDO.
DEFINE VARIABLE tb_approve AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_box AS LOGICAL INITIAL yes  NO-UNDO.
DEFINE VARIABLE tb_committed AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_corr AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_DC AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_fgimage AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_fold AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_freeze-note AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_GL AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_PR AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_prompt-ship AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_prt-label AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_prt-mch AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_prt-sellprc AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_prt-set-header AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_prt-shipto AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_reprint AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE tb_RS AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE TB_sample_req AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_SW AS LOGICAL INITIAL no NO-UNDO.
DEFINE VARIABLE tb_tray-2 AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE td-show-parm AS LOGICAL INITIAL no  NO-UNDO.
DEFINE VARIABLE lv-ornt AS CHARACTER INITIAL "P"  NO-UNDO.
DEFINE VARIABLE lv-font-no AS CHARACTER FORMAT "X(256)":U INITIAL "11"  NO-UNDO.
DEFINE VARIABLE lv-font-name AS CHARACTER FORMAT "X(256)":U INITIAL "Courier New Size=7 (17 cpi for 132 column Report)" NO-UNDO.
DEFINE VARIABLE fl-jobord AS INTEGER FORMAT ">>>>>>>>":U INITIAL 0 NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

assign
 cocode = prmComp
 g_company = prmComp
 locode = usercomp.loc
 g_loc  = usercomp.loc
 v-today    = TODAY   . 

 ASSIGN
     begin_job1            =  prmBeginJob  
     end_job1              =  prmEndJob  
     begin_job2            =  prmBeginJob2
     end_job2              =  prmEndJob2  
     tb_fold               = IF prmFoldCat      = "Yes" THEN TRUE ELSE FALSE
     tb_corr               = IF prmCorrugated   = "Yes" THEN TRUE ELSE FALSE
     tb_reprint            = IF prmReTicket     = "Yes" THEN TRUE ELSE FALSE
     tb_box                = IF prmBoxDes       = "Yes" THEN TRUE ELSE FALSE
     tb_fgimage            = IF prmFGImage      = "Yes" THEN TRUE ELSE FALSE
     tb_approve            = IF prmAppJobs      = "Yes" THEN TRUE ELSE FALSE  
     tb_tray-2             = IF prmCopy         = "Yes" THEN TRUE ELSE FALSE  
     spec_codes            =  prmCode 
     tb_prt-mch            = IF prmMachineSt    = "Yes" THEN TRUE ELSE FALSE  
     rd_print-speed        = IF prmMachSpeed    = "Speed" THEN "S" ELSE "H"
     tb_prt-shipto         = IF prmShipto       = "Yes" THEN TRUE ELSE FALSE  
     tb_prt-sellprc        = IF prmUpc          = "Yes" THEN TRUE ELSE FALSE  
     tb_prt-label          = IF prmLabelinfo    = "Yes" THEN TRUE ELSE FALSE  
     tb_committed          = IF prmBoard        = "Yes" THEN TRUE ELSE FALSE  
     tb_prt-set-header     = IF prmSet          = "Yes" THEN TRUE ELSE FALSE  
     tb_prompt-ship        = IF prmSplit        = "Yes" THEN TRUE ELSE FALSE  
     tb_freeze-note        = IF prmJobNot       = "Yes" THEN TRUE ELSE FALSE 
                                                        

        .
FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.

   

/********************************main block*****************************/

 production = NO.

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
            lv-int-f    = sys-ctrl.int-fld.
        IF /*index("Interpac,Dayton,FibreFC,Livngstn",lv-format-f) > 0*/
            lookup(lv-format-f,"Interpac,FibreFC,HPB,metro,Dayton,Livngstn,CentBox,Frankstn,Colonial,Unipak,Ottpkg,Shelby,CCC,Indiana-XL,PPI,Accord,Knight,Dee") > 0 THEN lines-per-page = 55.

        {sys/inc/jobcard.i "C"}
            ASSIGN
            lv-format-c = sys-ctrl.char-fld
            lv-int-c    = sys-ctrl.int-fld.

        /* gdm - 11120805*/

        MESSAGE "sys" lv-format-f lv-format-c .

        ASSIGN tb_fgimage = NO.
        IF LOOKUP(lv-format-c,"Artios,CapCity,Trilakes2,Suthrlnd") > 0 
            THEN ASSIGN tb_fgimage = YES.

        IF lv-format-c = "Artios" THEN
            ASSIGN
            tb_tray-2 = YES.

       /* FIND FIRST users WHERE
            users.user_id EQ USERID("NOSWEAT")
            NO-LOCK NO-ERROR.

        IF AVAIL users AND users.user_program[2] NE "" THEN
            init-dir = users.user_program[2].
        ELSE
            init-dir = "c:\tmp".

            RUN enable_UI.
            {methods/nowait.i}

                DO WITH FRAME {&frame-name}:
                    {custom/usrprint.i}
                        IF INDEX(PROGRAM-NAME(4),"mainmenu") GT 0 
                        THEN ASSIGN fl-jobord = 0
                            fl-jobord:SCREEN-VALUE = "0".
                        /* gdm - 07130906 */
                        IF lv-format-f = "FibreFC" 
                            THEN ASSIGN fl-jobord = INT(fl-jobord:SCREEN-VALUE).
                        ELSE ASSIGN fl-jobord = 0.*/
                        
                        ASSIGN
                            tb_approve = NO
                            TB_sample_req  = TRUE
                            tb_freeze-note  = NO
                            tb_prompt-ship = NO .
                        IF lv-format-c = "pacific" OR lv-format-c = "Allwest" THEN
                            ASSIGN
                            lv-ornt = "L"
                            lv-font-no = "13"
                            lv-font-name = "Courier New Size=9 (13CPI)"
                            lines-per-page = 48.

                        /*IF tb_corr:SCREEN-VALUE EQ "No" AND lv-format-f NE "ASI" THEN spec_codes:HIDDEN = YES.*/

                        IF tb_fold AND (lv-format-f = "Interpac"  OR lv-format-f = "Dayton" 
                                        OR lv-format-f = "Livngstn"  OR lv-format-f = "FibreFC"  OR lv-format-f = "HPB"
                                        OR lv-format-f = "metro"     or lv-format-f = "Indiana-XL"
                                        OR lv-format-f = "CentBox"   OR lv-format-f = "Keystone" OR lv-format-f = "Frankstn" 
                                        OR lv-format-f = "Colonial"  OR lv-format-f = "Unipak"   OR lv-format-f = "Ottpkg"
                                        OR lv-format-f = "MWFIbre"   OR lv-format-f = "Shelby"   OR lv-format-f = "CCC"
                                        OR lv-format-f = "PPI"       OR lv-format-f = "Accord"   OR lv-format-f = "Knight" 
                                        OR lv-format-f = "Dee")
                            THEN
                            assign 
                                tb_prt-mch     = YES
                                tb_prt-shipto  = YES
                                tb_prt-sellprc = YES
                                rd_print-speed = "S"  .            

                        ELSE do:
                            ASSIGN tb_prt-mch = NO
                                tb_prt-shipto = NO
                                tb_prt-sellprc = NO
                                rd_print-speed = "S".
                            END.

                            IF lv-format-c EQ "Artios" AND lv-format-f EQ "FibreFC" THEN DO:
                                plev = 1.
                                REPEAT WHILE PROGRAM-NAME(plev) NE ?:
                                    IF PROGRAM-NAME(plev) MATCHES "*w-jobapp*" THEN DO:
                                        production = YES.
                                        LEAVE.
                                        END.
                                        plev = plev + 1.
                                        END.
                                        IF NOT production THEN
                                            DO:
                                            
                                            ASSIGN
                                                
                                                tb_approve = YES.
                                            END.
                                            RUN split-ship-proc. /*only for Fibre*/
                                            END.

                                            IF tb_corr = TRUE AND (lv-format-c = "Hughes"  OR lv-format-c = "Allwest") THEN
                                               
                                                IF TRIM(prmBeginJob) NE ""                          AND
                                                    TRIM(prmBeginJob) EQ TRIM(prmEndJob) AND
                                                    INT(prmBeginJob2)  EQ INT(prmEndJob2)  THEN DO:
                                                    FIND FIRST job-hdr NO-LOCK
                                                        WHERE job-hdr.company EQ prmComp

                                                        AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(prmBeginJob)) +
                                                        TRIM(prmBeginJob)
                                                        AND job-hdr.job-no2 EQ INT(prmBeginJob2)
                                                        NO-ERROR.

                                                    IF AVAIL job-hdr THEN DO:
                                                        FIND FIRST job NO-LOCK
                                                            WHERE job.company EQ job-hdr.company
                                                            AND job.job     EQ job-hdr.job
                                                            AND job.job-no  EQ job-hdr.job-no
                                                            AND job.job-no2 EQ job-hdr.job-no2
                                                            NO-ERROR.
                                                        IF AVAIL job THEN  do:
                                                            IF job.cs-to-pr = TRUE THEN 
                                                                assign
                                                                tb_approve   = NO .
                                                            ELSE
                                                                assign
                                                                  
                                                                    tb_approve =  YES .
                                                                
                                                                END.

                                                                IF can-find(FIRST b-reftable-freeze WHERE
                                                                            b-reftable-freeze.reftable EQ "FREEZENOTE" AND
                                                                            b-reftable-freeze.company  EQ cocode AND
                                                                            b-reftable-freeze.loc      EQ job-hdr.job-no AND
                                                                            b-reftable-freeze.CODE     EQ STRING(job-hdr.job-no2,"99")) THEN
                                                                    tb_freeze-note = YES.
                                                                
                                                                IF CAN-FIND(FIRST b-reftable-split WHERE
                                                                            b-reftable-split.reftable EQ "splitshp" AND
                                                                            b-reftable-split.company  EQ cocode AND
                                                                            b-reftable-split.loc      EQ TRIM(job-hdr.job-no) AND
                                                                            b-reftable-split.code     EQ STRING(job-hdr.job-no2,"9999999999")) THEN
                                                                    tb_prompt-ship = YES.
                                                                END.  /* avail job-hdr */
                                                                END.
                                                                /* Task #: 02160708 - dgd 04/04/2007 - START */
                                                               /* if can-do ("Indiana-XL", lv-format-f) 
                                                                    then run HideDeptBoxes (no).
                                                                else run HideDeptBoxes (yes).*/
                                                                /* Task #: 02160708 - dgd 04/04/2007 - END */

                                                                tb_prt-set-header = CAN-DO("Artios,Premier,Xprint,Suthrlnd,United,Hughes,Spectrum,CapCity,Allwest,LoyLang",lv-format-c).
                                                                IF NOT tb_prt-set-header THEN
                                                                    tb_prt-set-header= NO.
                                                                
                                                                IF TRIM(begin_job1) NE ""                          AND
                                                                    TRIM(begin_job1) EQ TRIM(end_job1) AND
                                                                    INT(begin_job2)  EQ INT(end_job2)  THEN DO:
                                                                    FIND FIRST job-hdr NO-LOCK
                                                                        WHERE job-hdr.company EQ cocode
                                                                        AND job-hdr.job-no  EQ FILL(" ",6 - LENGTH(begin_job1)) +
                                                                        TRIM(begin_job1)
                                                                        AND job-hdr.job-no2 EQ INT(begin_job2)
                                                                        NO-ERROR.
                                                                    IF AVAIL job-hdr THEN DO:
                                                                        tb_reprint = (job-hdr.ftick-prnt).
                                                                        IF production THEN DO:
                                                                            FIND FIRST job NO-LOCK
                                                                                WHERE job.company EQ job-hdr.company
                                                                                AND job.job     EQ job-hdr.job
                                                                                AND job.job-no  EQ job-hdr.job-no
                                                                                AND job.job-no2 EQ job-hdr.job-no2
                                                                                NO-ERROR.
                                                                            IF AVAIL job THEN tb_reprint = (job.pr-printed). 
                                                                            
                                                                            END.
                                                                            END.
                                                                                

                     
  END.  

  
 
  /*RUN new-job-no.*/


/***********************************end main ****************************/


IF prmAction = "JobTicket" THEN DO:
    
  IF prmOut = "No" THEN DO:

      MESSAGE "report2" .
      ASSIGN  
          init-dir    = v-webrootpath
          lv-pdf-file = init-dir + 'JobTicket' 
          lv-pdf-file = lv-pdf-file + prmBeginJob
          vPdfFile   = 'JobTicket' + prmBeginJob + '.pdf'.
      
    
          IF tb_fold THEN DO:
              RUN run-report ("Fold").
          END.
          IF tb_corr THEN DO:
              RUN run-report ("Corr").
          END.

          RUN printPDF (list-name, "ADVANCED SOFTWARE","A1g9f84aaq7479de4m22").

          CREATE ttTopJobTicketReport.
          ASSIGN ttTopJobTicketReport.vFile = vPdfFile.
         MESSAGE "testre" ttTopJobTicketReport.vFile.
    END. /*rnd of prmOut*/

END. /* end of prmAction */


PROCEDURE run-report :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  /* Parameters */
  DEF INPUT PARAM ip-industry AS CHAR NO-UNDO.

  DEF BUFFER b-reftable FOR reftable.
  DEF BUFFER b-eb FOR eb.
  
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
    approve                 = tb_approve.
    
  FOR EACH wrk-ink:
      DELETE wrk-ink.
  END.

  {jcrep/tickrrpt.i}

 /* RUN custom/usrprint.p (v-prgmname, ).

  /*share settings between the different ways this program is called*/
  IF INDEX(PROGRAM-NAME(1),"job_") NE 0 THEN
     RUN custom/usrprint.p ("r-ticket.", FRAME {&FRAME-NAME}:HANDLE).
  ELSE IF INDEX(PROGRAM-NAME(1),"r-ticket") NE 0 THEN
     RUN custom/usrprint.p ("job_.", FRAME {&FRAME-NAME}:HANDLE).
  
  SESSION:SET-WAIT-STATE ("").*/

end procedure.

PROCEDURE set-job-vars :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/


    ASSIGN
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(prmBeginJob))) +
                 TRIM(prmBeginJob)
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(prmEndJob))) +
                 trim(prmEndJob)
     fjob-no2  = INT(prmBeginJob2)
     tjob-no2  = INT(prmEndJob)
     fjob-no   = FILL(" ",6 - LENGTH(TRIM(fjob-no))) + TRIM(fjob-no) +
                 STRING(fjob-no2,"99")
     tjob-no   = FILL(" ",6 - LENGTH(TRIM(tjob-no))) + TRIM(tjob-no) +
                 STRING(tjob-no2,"99").
  END.

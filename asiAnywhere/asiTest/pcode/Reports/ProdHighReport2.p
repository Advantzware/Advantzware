

/*------------------------------------------------------------------------
    File        : ProdHighReport.p
    Purpose     :  Production Highlights

    Syntax      :

    Description : Return a Dataset of Request Report

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */


    DEFINE TEMP-TABLE ttProdHighlightsReport NO-UNDO
          FIELD vDashFile AS CHAR
          FIELD  vMachine AS CHAR
          FIELD  vMachDesc    AS CHAR  .
        

    DEFINE DATASET dsProdHighlightsReport FOR ttProdHighlightsReport .

    DEFINE INPUT PARAMETER prmUser            AS CHARACTER  NO-UNDO.
    DEFINE INPUT PARAMETER prmAction          AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDate            AS CHARACTER NO-UNDO. 
    DEFINE INPUT PARAMETER prmComp            AS CHAR  NO-UNDO.
    DEFINE INPUT PARAMETER prmMachine1        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine2        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine3        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine4        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine5        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine6        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine7        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine8        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine9        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine10        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine11        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine12        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine13        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine14        AS CHAR  NO-UNDO. 
    DEFINE INPUT PARAMETER prmMachine15        AS CHAR  NO-UNDO. 
    
    DEFINE INPUT PARAMETER prmOutexcel        AS CHARACTER NO-UNDO.
    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsProdHighlightsReport.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.

  
       IF  prmUser        = ?        THEN ASSIGN     prmUser       = "".
       IF  prmAction      = ?        THEN ASSIGN     prmAction     = "".
       IF  prmDate        = ?        THEN ASSIGN     prmDate       = "".
       IF  prmComp        = ?        THEN ASSIGN     prmComp       = "".
       IF  prmMachine1    = ?        THEN ASSIGN     prmMachine1   = "". 
       IF  prmMachine2    = ?        THEN ASSIGN     prmMachine2   = "". 
       IF  prmMachine3    = ?        THEN ASSIGN     prmMachine3   = "". 
       IF  prmMachine4    = ?        THEN ASSIGN     prmMachine4   = "". 
       IF  prmMachine5    = ?        THEN ASSIGN     prmMachine5   = "". 
       IF  prmMachine6    = ?        THEN ASSIGN     prmMachine6   = "". 
       IF  prmMachine7    = ?        THEN ASSIGN     prmMachine7   = "". 
       IF  prmMachine8    = ?        THEN ASSIGN     prmMachine8   = "". 
       IF  prmMachine9    = ?        THEN ASSIGN     prmMachine9   = "". 
       IF  prmMachine10   = ?        THEN ASSIGN     prmMachine10  = "". 
       IF  prmMachine11   = ?        THEN ASSIGN     prmMachine11  = "". 
       IF  prmMachine12   = ?        THEN ASSIGN     prmMachine12  = "". 
       IF  prmMachine13   = ?        THEN ASSIGN     prmMachine13  = "". 
       IF  prmMachine14   = ?        THEN ASSIGN     prmMachine14  = "". 
       IF  prmMachine15   = ?        THEN ASSIGN     prmMachine15  = "". 
                                                                   

 
   DEFINE VARIABLE Machine AS CHAR EXTENT 15 NO-UNDO.

   ASSIGN
        Machine[1] = prmMachine1  
        Machine[2] = prmMachine2 
        Machine[3] = prmMachine3 
        Machine[4] = prmMachine4 
        Machine[5] = prmMachine5 
        Machine[6] = prmMachine6 
        Machine[7] = prmMachine7 
        Machine[8] = prmMachine8 
        Machine[9] = prmMachine9 
        Machine[10] = prmMachine10 
        Machine[11] = prmMachine11 
        Machine[12] = prmMachine12
        Machine[13] = prmMachine13
        Machine[14] = prmMachine14 
        Machine[15] = prmMachine15 
         .


   DEFINE VARIABLE fi_as-of-date AS DATE FORMAT "99/99/9999"   NO-UNDO.  
   DEFINE VARIABLE fi_company AS CHARACTER FORMAT "X(3)"    NO-UNDO.
   DEFINE VARIABLE v-today AS DATE FORMAT "9999/99/99" NO-UNDO.
   DEFINE VARIABLE v-VERSION AS CHARACTER NO-UNDO.
{custom/xprint.i}
    
{sys/inc/var.i new shared}
{salrep/dashprod.i NEW}
    
DEFINE NEW SHARED VAR v-webrootpath AS CHAR NO-UNDO.
DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
DEFINE VARIABLE vFileName AS CHAR NO-UNDO.
DEFINE VARIABLE init-dir AS CHAR NO-UNDO.
DEFINE VARIABLE  v-excel-file    AS CHARACTER   NO-UNDO.
DEF TEMP-TABLE tt-report NO-UNDO LIKE report
    FIELD DATE AS DATE
    FIELD row-id AS ROWID
    FIELD qty AS DEC
    FIELD amt       LIKE ar-invl.amt        FORMAT "->>>>>>>9.99"
    FIELD cash-date LIKE ar-inv.inv-date
    FIELD misc AS LOG
    FIELD cost AS DEC
    FIELD msf AS DEC.

DEF TEMP-TABLE work-tmp NO-UNDO
   field job like job.job
   field frm like job-mch.frm
   field blank-no like job-mch.blank-no
   FIELD sort-field AS CHAR
   field dept as char format 'xx'
   field m-code like mach.m-code
   field pass like job-mch.pass
   field r-act-hrs as dec format '>>>>9.99'
   field m-act-hrs as dec format '>>>>9.99'
   field dt-chg-hrs as dec format '>>>>9.99'
   field dt-nochg-hrs as dec format '>>>>9.99'
   field qty as dec format '>>>>>>>>9'
   field msf as dec format '>>>>>.999'
   INDEX work-tmp job frm blank-no dept m-code pass sort-field.

def TEMP-TABLE w-data no-undo
  field w-sman-no   AS CHAR
  field w-sqft      LIKE itemfg.t-sqft format "->>>9.999"    extent 4
  field w-amt       like ar-inv.gross  format "->>>,>>9.99"  extent 4
  field w-cost      like ar-inv.t-cost format "->>,>>9.99"   extent 3
  FIELD w-msf       AS DEC EXTENT 3.

DEF BUFFER b-mach FOR mach.
DEFINE NEW SHARED VARIABLE ttoutputfile AS CHARACTER.
DEF VAR counter AS INT NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

/*prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".*/

    assign
 cocode = prmComp
 g_company = prmComp 
 locode = usercomp.loc
 g_loc  = usercomp.loc
 v-today = TODAY . 

 IF prmAction = "Select" THEN DO:

     FOR EACH mach WHERE mach.company = prmComp  NO-LOCK BY mach.m-code :
       IF AVAIL mach THEN DO:
         CREATE  ttProdHighlightsReport.
           ASSIGN
                ttProdHighlightsReport.vMachine   = mach.m-code
                ttProdHighlightsReport.vMachDesc  = mach.m-dscr .
       END.
     END.
 END.




  FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
    IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


IF prmAction = "RunReport" THEN DO:

ASSIGN
            fi_as-of-date = DATE(prmDate)
            fi_company    = prmComp 
                        .  
        

        FOR EACH reftable WHERE
        reftable.reftable EQ "HM5" AND
        reftable.company EQ USERID("NOSWEAT") AND
        reftable.loc = fi_company
        EXCLUSIVE-LOCK:

        DELETE reftable.
        END.
  do counter = 1 TO 15 :
      IF Machine[counter] = ""  THEN LEAVE.

        CREATE tt-raw-prod.
        ASSIGN tt-raw-prod.m-code = Machine[counter]
            tt-raw-prod.DATE   = fi_as-of-date.
        RELEASE tt-raw-prod.

        CREATE reftable.
        ASSIGN reftable.reftable = "HM5"
            reftable.company = USERID("NOSWEAT")
            reftable.loc = fi_company
            reftable.CODE = Machine[counter].
        
        RELEASE reftable.
     
              
     
  END.
     run run-report.
        /*ASSIGN ttoutputfile =  "prod_high_rpt_" 
             + STRING(YEAR(TODAY), "9999") + STRING(MONTH(TODAY), "99")
             + STRING(DAY(TODAY), "99") + REPLACE(STRING(TIME, "HH:MM:SS"), ":", "") + ".xlsx".*/
        
        CREATE ttProdHighlightsReport.
        ASSIGN ttProdHighlightsReport.vDashFile = ttoutputfile .
 
 
 
 END.
  
    /*****************************************PROCEDURE run-report :*****************************************************/
 
  PROCEDURE run-report :
    
      RUN raw-prod-proc. /*Raw Production*/
      RUN salrep\dashprod.p(INPUT prmComp,
                         INPUT fi_as-of-date).

       OS-DELETE VALUE("d:\webapps\prodhigh.csv") .

end procedure.

PROCEDURE raw-prod-proc :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   /*DR1*/
   def var v-on as int no-undo.
   def var v-out as int no-undo.
   DEF VAR v-up AS INT NO-UNDO.
   DEF VAR std-hrs-var AS DEC NO-UNDO.
   DEF VAR tot-hrs-var AS DEC NO-UNDO.
   DEF VAR v-start-of-year AS DATE NO-UNDO.
   DEF VAR v-month-as-of-date AS INT NO-UNDO.

   EMPTY TEMP-TABLE tt-report.

   FIND FIRST period WHERE
        period.company EQ fi_company AND
        period.yr EQ YEAR(fi_as-of-date)
        NO-LOCK NO-ERROR.

   IF AVAIL period THEN
      v-start-of-year = period.pst.
   ELSE
      v-start-of-year = DATE(1,1,YEAR(fi_as-of-date)).

   v-month-as-of-date = MONTH(fi_as-of-date).

   FOR EACH tt-raw-prod,
       EACH mch-act FIELDS(company m-code op-date job frm blank-no dept pass
            CODE qty op-date hours) WHERE
            mch-act.company EQ fi_company AND
            mch-act.m-code  EQ tt-raw-prod.m-code AND
            mch-act.op-date GE v-start-of-year AND
            mch-act.op-date LE fi_as-of-date
            NO-LOCK:

       find first work-tmp where
            work-tmp.job = mch-act.job and
            work-tmp.frm = mch-act.frm and
            work-tmp.blank-no = mch-act.blank-no and
            work-tmp.dept = mch-act.dept and
            work-tmp.m-code = mch-act.m-code and
            work-tmp.pass = mch-act.pass
            no-error.

       find first b-mach WHERE
            b-mach.company eq fi_company AND
            b-mach.m-code  eq mch-act.m-code
            no-lock no-error.

       IF NOT AVAIL work-tmp THEN
       DO:
          CREATE work-tmp.
          ASSIGN
             work-tmp.job      = mch-act.job
             work-tmp.frm      = mch-act.frm
             work-tmp.blank-no = mch-act.blank-no
             work-tmp.dept     = IF mch-act.dept NE "" THEN mch-act.dept
                                 ELSE IF AVAIL b-mach THEN b-mach.dept[1] ELSE ""
             work-tmp.m-code   = mch-act.m-code
             work-tmp.pass     = mch-act.pass.
       END.

       FIND job-code WHERE
            job-code.code EQ mch-act.code
            NO-LOCK NO-ERROR.

       IF AVAIL job-code THEN
       DO:
           IF job-code.cat eq "RUN" THEN DO:
              
              work-tmp.qty = work-tmp.qty
                           + IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty.

              IF work-tmp.qty EQ ? THEN work-tmp.qty = 0.

              IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                 ASSIGN
                    tt-raw-prod.date-qty = tt-raw-prod.date-qty
                                         + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                    tt-raw-prod.date-run-hrs = tt-raw-prod.date-run-hrs
                                             + mch-act.hours.

              IF mch-act.op-date LE fi_as-of-date THEN
              DO:
                 IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                    ASSIGN
                       tt-raw-prod.mtd-qty = tt-raw-prod.mtd-qty
                                           + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                       tt-raw-prod.mtd-run-hrs = tt-raw-prod.mtd-run-hrs
                                               + mch-act.hours.
                 
                 ASSIGN
                    tt-raw-prod.ytd-qty = tt-raw-prod.ytd-qty
                                        + (IF mch-act.qty EQ ? THEN 0 ELSE mch-act.qty)
                    tt-raw-prod.ytd-run-hrs = tt-raw-prod.ytd-run-hrs
                                            + mch-act.hours.
              END.

              for each job-hdr FIELDS(company job frm i-no est-no frm blank-no n-on) WHERE
                  job-hdr.company   eq fi_company AND
                  job-hdr.job       eq work-tmp.job AND
                  job-hdr.frm       eq work-tmp.frm AND
                 (job-hdr.blank-no eq mch-act.blank-no or mch-act.blank-no eq 0)
                 no-lock,
                 first itemfg FIELDS(company i-no t-sqft) WHERE
                       itemfg.company eq fi_company AND
                       itemfg.i-no    eq job-hdr.i-no
                       no-lock:

                 assign
                    v-on  = 1
                    v-out = 1.
         
                 if avail b-mach and index("APB",b-mach.p-type) le 0 then do:
                    find first eb WHERE
                         eb.company   eq job-hdr.company AND
                         eb.est-no    EQ job-hdr.est-no AND
                         eb.form-no   eq job-hdr.frm AND
                         (eb.blank-no eq job-hdr.blank-no or job-hdr.blank-no eq 0)
                         no-lock no-error.

                     if avail eb then v-up = eb.num-up.
                    
                     if job-hdr.n-on ne 0 then v-up = job-hdr.n-on.
                    
                     find first ef
                         where ef.company eq job-hdr.company
                           and ef.est-no  EQ job-hdr.est-no
                           and ef.form-no eq job-hdr.frm
                         no-lock no-error.
                    
                     IF AVAIL ef THEN RUN est/ef-#out.p (ROWID(ef), OUTPUT v-out).
                    
                     v-on = v-up * v-out.
                      
                     find first est-op
                         where est-op.company eq job-hdr.company
                           AND est-op.est-no  EQ job-hdr.est-no
                           and est-op.s-num   eq mch-act.frm
                           and (est-op.b-num  eq mch-act.blank-no or
                                mch-act.blank-no eq 0)
                           and est-op.m-code  eq mch-act.m-code
                           and est-op.op-pass eq mch-act.pass
                           and est-op.dept    eq mch-act.dept
                           and est-op.line    lt 500
                         no-lock no-error.
                     if not avail est-op then
                     find first est-op
                         where est-op.company eq job-hdr.company
                           AND est-op.est-no  EQ job-hdr.est-no
                           and est-op.s-num   eq mch-act.frm
                           and (est-op.b-num  eq mch-act.blank-no or
                                mch-act.blank-no eq 0)
                           and est-op.op-pass eq mch-act.pass
                           and est-op.dept    eq mch-act.dept
                           and est-op.line    lt 500
                         no-lock no-error.
                    
                     if avail est-op then
                       run sys/inc/numout.p (recid(est-op), output v-out).
                    
                     else v-out = 1.
                    
                     v-on = v-on / v-out.
                 end.

                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-qty-msf = tt-raw-prod.date-qty-msf
                                             + (mch-act.qty * itemfg.t-sqft * v-on / 1000).

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-qty-msf = tt-raw-prod.mtd-qty-msf
                                               + (mch-act.qty * itemfg.t-sqft * v-on / 1000).
                    
                    tt-raw-prod.ytd-qty-msf = tt-raw-prod.ytd-qty-msf
                                            + (mch-act.qty * itemfg.t-sqft * v-on / 1000).
                 END.
              END.
           END.
           ELSE
              IF job-code.cat EQ "MR" THEN
              DO:
                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-mr-hrs = tt-raw-prod.date-mr-hrs
                                            + mch-act.hours.

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-mr-hrs = tt-raw-prod.mtd-mr-hrs
                                              + mch-act.hours.
                   
                    tt-raw-prod.ytd-mr-hrs = tt-raw-prod.ytd-mr-hrs
                                           + mch-act.hours.
                 END.
              END.
           ELSE
              IF job-code.cat EQ "DT" THEN
              DO:
                 IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                    tt-raw-prod.date-dt-charge = tt-raw-prod.date-dt-charge
                                               + mch-act.hours.

                 IF mch-act.op-date LE fi_as-of-date THEN
                 DO:
                    IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                       tt-raw-prod.mtd-dt-charge = tt-raw-prod.mtd-dt-charge
                                                 + mch-act.hours.
                    
                    
                    tt-raw-prod.ytd-dt-charge = tt-raw-prod.ytd-dt-charge
                                              + mch-act.hours.
                 END.
              END.
           ELSE
           DO:
              IF mch-act.op-date EQ tt-raw-prod.DATE THEN
                 tt-raw-prod.date-dt-nc = tt-raw-prod.date-dt-nc
                                        + mch-act.hours.

              IF mch-act.op-date LE fi_as-of-date THEN
              DO:
                 IF MONTH(mch-act.op-date) EQ v-month-as-of-date THEN
                    tt-raw-prod.mtd-dt-nc = tt-raw-prod.mtd-dt-nc
                                          + mch-act.hours.
                 
                 tt-raw-prod.ytd-dt-nc = tt-raw-prod.ytd-dt-nc
                                       + mch-act.hours.
              END.
           END.
       END. /*avail job-code */

   END. /*each tt-raw-prod*/

   FOR EACH work-tmp,
       FIRST tt-raw-prod WHERE
             tt-raw-prod.m-code EQ work-tmp.m-code:

       find first job-mch where job-mch.company  = fi_company and
                                job-mch.job      eq work-tmp.job and
                                job-mch.frm      = work-tmp.frm and
                                (job-mch.blank-no = work-tmp.blank-no or
                                 work-tmp.blank-no = 0) and
                                job-mch.m-code   = work-tmp.m-code and
                                job-mch.pass     = work-tmp.pass
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job      eq work-tmp.job and
                                job-mch.frm      eq work-tmp.frm and
                                (job-mch.blank-no = work-tmp.blank-no or
                                 work-tmp.blank-no = 0) and
                                job-mch.m-code   eq work-tmp.m-code
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job     eq work-tmp.job and
                                job-mch.frm     eq work-tmp.frm and
                                job-mch.m-code  eq work-tmp.m-code and
                                job-mch.speed   ne 0
                                no-lock no-error.
       if not avail job-mch then
       find first job-mch where job-mch.company eq fi_company and
                                job-mch.job     eq work-tmp.job and
                                job-mch.frm     eq work-tmp.frm and
                                job-mch.m-code  eq work-tmp.m-code
                                no-lock no-error.

       if available job-mch then
       DO:
          std-hrs-var = (IF work-tmp.qty NE 0 AND job-mch.speed NE 0 THEN
                            work-tmp.qty / job-mch.speed ELSE job-mch.run-hr)
                      + job-mch.mr-hr.

          IF job-mch.start-date EQ tt-raw-prod.DATE THEN
             tt-raw-prod.date-std-hrs = tt-raw-prod.date-std-hrs
                                      + std-hrs-var.

          IF job-mch.start-date LE fi_as-of-date THEN
          DO:
             IF MONTH(job-mch.start-date) EQ v-month-as-of-date THEN
                tt-raw-prod.mtd-std-hrs = tt-raw-prod.mtd-std-hrs
                                        + std-hrs-var.
             
             tt-raw-prod.ytd-std-hrs = tt-raw-prod.ytd-std-hrs
                                     + std-hrs-var.
          END.
       end.

   END. /*each work-tmp*/

   FOR EACH tt-raw-prod:

       ASSIGN
       tt-raw-prod.date-eff = (IF (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs +
                               tt-raw-prod.date-dt-charge) NE 0 THEN
                               (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                               tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge) * 100)
                               ELSE 0)
       tt-raw-prod.date-util = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-std-hrs / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)
       tt-raw-prod.date-dt-perc = (IF (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs +
                                tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) NE 0 THEN
                                (tt-raw-prod.date-dt-nc / (tt-raw-prod.date-run-hrs +
                                tt-raw-prod.date-mr-hrs + tt-raw-prod.date-dt-charge +
                                tt-raw-prod.date-dt-nc) * 100)
                                ELSE 0)
       
       tt-raw-prod.mtd-eff = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-util = (IF (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs +
                              tt-raw-prod.mtd-dt-charge + tt-raw-prod.mtd-dt-nc) NE 0 THEN
                              (tt-raw-prod.mtd-std-hrs / (tt-raw-prod.mtd-run-hrs +
                              tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                              tt-raw-prod.mtd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.mtd-dt-perc = (IF (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs +
                                 tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.mtd-dt-nc / (tt-raw-prod.mtd-run-hrs +
                                 tt-raw-prod.mtd-mr-hrs + tt-raw-prod.mtd-dt-charge +
                                 tt-raw-prod.mtd-dt-nc) * 100)
                                 ELSE 0)
       tt-raw-prod.ytd-eff = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-util = (IF (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs +
                              tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) NE 0 THEN
                              (tt-raw-prod.ytd-std-hrs / (tt-raw-prod.ytd-run-hrs +
                              tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                              tt-raw-prod.ytd-dt-nc) * 100)
                              ELSE 0)
       tt-raw-prod.ytd-dt-perc = (IF (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs +
                                 tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) NE 0 THEN
                                 (tt-raw-prod.ytd-dt-nc / (tt-raw-prod.ytd-run-hrs +
                                 tt-raw-prod.ytd-mr-hrs + tt-raw-prod.ytd-dt-charge +
                                 tt-raw-prod.ytd-dt-nc) * 100)
                                 ELSE 0).
   END.
   

END PROCEDURE.

/* ----------------------------------------------- cerep/jobSLdee.p 01/09 gdm */
/*  Schedule Labels for DEE                                                   */
/* -------------------------------------------------------------------------- */

/************* Parameters ******************/
DEF INPUT PARAM ip-multi-faxout   AS LOG  NO-UNDO.
DEF INPUT PARAM ip-lines-per-page AS INT  NO-UNDO.
DEF INPUT PARAM icBegJobNo        AS CHAR NO-UNDO.
DEF INPUT PARAM iiBegJobNo2       AS INT  NO-UNDO.
DEF INPUT PARAM icEndJobNo        AS CHAR NO-UNDO.
DEF INPUT PARAM iiEndJobNo2       AS INT  NO-UNDO.
DEF INPUT PARAM icBegMach         AS CHAR NO-UNDO.
DEF INPUT PARAM icEndMach         AS CHAR NO-UNDO.
DEF INPUT PARAM icBegForm         AS CHAR NO-UNDO.
DEF INPUT PARAM icEndForm         AS CHAR NO-UNDO.
DEF INPUT PARAM icBegBlnk         AS CHAR NO-UNDO.
DEF INPUT PARAM icEndBlnk         AS CHAR NO-UNDO.

DEF VAR v_due          AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR v_due2         AS CHAR FORMAT "x(8)" NO-UNDO.
DEF VAR tmp-dir        AS char NO-UNDO.
DEF SHARED VAR  list-name      AS char NO-UNDO.
DEF VAR init-dir       AS char NO-UNDO.
DEF VAR lines-per-page AS INT  NO-UNDO.
DEF VAR v_shtsize      AS CHAR FORMAT "x(20)" NO-UNDO.
DEF VAR v-job-qty      AS INT  NO-UNDO.
DEF VAR v-job-itm      AS INT  NO-UNDO.

DEF VAR v_lblcnt AS INT NO-UNDO.
DEF VAR v_lpcnt AS INT NO-UNDO.

DEF TEMP-TABLE wrk-op NO-UNDO
    FIELD m-dscr   LIKE mach.m-dscr 
    FIELD m-code   LIKE job-mch.m-code
    FIELD d-seq    LIKE mach.d-seq
    FIELD dept     LIKE job-mch.dept
    FIELD b-num    LIKE job-mch.blank-no
    FIELD s-num    LIKE job-mch.frm
    FIELD pass     LIKE job-mch.pass
    FIELD mr       LIKE job-mch.mr-hr    EXTENT 100
    FIELD speed    LIKE job-mch.speed    EXTENT 100
    FIELD run-hr   LIKE job-mch.run-hr   EXTENT 100
    FIELD num-sh   AS INT FORMAT ">>>,>>9"  EXTENT 100
    FIELD spoil    LIKE job-mch.wst-prct EXTENT 100
    FIELD mr-waste LIKE job-mch.mr-waste EXTENT 100
    FIELD t-jobno  AS CHAR FORMAT "x(6)"
    FIELD t-custnm AS CHAR FORMAT "x(30)"
    FIELD t-machcd AS CHAR FORMAT "x(6)" 
    FIELD t-machnm AS CHAR FORMAT "x(20)"
    FIELD t-itemno AS CHAR FORMAT "x(15)"
    FIELD t-itemnm AS CHAR FORMAT "x(30)"
    FIELD t-dieno  AS CHAR FORMAT "x(15)"
    FIELD t-dept   AS CHAR
    FIELD t-shtSz  AS CHAR FORMAT "x(20)"
    FIELD t-deptsr AS INT
    FIELD t-jqty   AS INT 
    FIELD t-jitm   AS INT.

DEF BUFFER bf-wrk-op  FOR wrk-op.
DEF BUFFER bf-wrk-op2 FOR wrk-op.

{sys/inc/var.i shared}
/* {sys/form/s-top.f}     */

{jcrep/r-ticket.i "shared"}


/* {custom/formtext.i NEW} */

{cec/msfcalc.i}

ASSIGN v_due  = STRING(TODAY,"99/99/99")
       v_due2 = STRING(TODAY,"99/99/99").

RUN gather_data.

FORM SKIP (3)
    WITH NO-BOX FRAME lbl-SPC NO-LABELS STREAM-IO WIDTH  132.

FORM
    "Job#:" AT 2 wrk-op.t-jobno AT 7 "Due:" AT 16 v_due AT 20 "MID:" AT 29 wrk-op.m-code    FORMAT "x(6)" AT 34 
    "Job#:" AT 44 bf-wrk-op.t-jobno AT 49 "Due:" AT 58 v_due2 AT 62 "MID:" AT 71 bf-wrk-op.m-code FORMAT "x(6)" AT 76 
    SKIP
    "Cust:"  AT 2  wrk-op.t-custnm AT 7 "Cust:"  AT 44 bf-wrk-op.t-custnm AT 49
   SKIP
    "Item #:" AT 2 wrk-op.t-itemno FORMAT "x(15)" AT 9 
    "Item #:" AT 44 bf-wrk-op.t-itemno FORMAT "x(15)" AT 51
   SKIP
    "Item Name:" AT 2  wrk-op.t-itemnm    AT 12
    "Item Name:" AT 44 bf-wrk-op.t-itemnm AT 54
   SKIP
    "Sheet:" AT 2  wrk-op.t-shtSz AT 8 "Qty:" AT 29 wrk-op.num-sh[1] AT 33 
    "Sheet:" AT 44 bf-wrk-op.t-shtSz AT 51 "Qty:" AT 71 bf-wrk-op.num-sh[1] AT 75
   SKIP 
    "Die #:" AT 2  wrk-op.t-dieno AT 8
    "Die #:" AT 44 bf-wrk-op.t-dieno AT 50
   WITH NO-BOX FRAME lbl-PR NO-LABELS STREAM-IO WIDTH  132.

FORM
    "Job#:" AT 2 wrk-op.t-jobno AT 7 "Due:" AT 16 v_due AT 20 "MID:" AT 29 wrk-op.m-code    FORMAT "x(6)" AT 34 
    "Job#:" AT 44 bf-wrk-op.t-jobno AT 49 "Due:" AT 58 v_due2 AT 62 "MID:" AT 71 bf-wrk-op.m-code FORMAT "x(6)" AT 76 
    SKIP
    "Cust:"  AT 2  wrk-op.t-custnm AT 7 "Cust:"  AT 44 bf-wrk-op.t-custnm AT 49
   SKIP
    "Item #:" AT 2 wrk-op.t-itemno FORMAT "x(15)" AT 9 
    "Item #:" AT 44 bf-wrk-op.t-itemno FORMAT "x(15)" AT 51
   SKIP
    "Item Name:" AT 2  wrk-op.t-itemnm    AT 12
    "Item Name:" AT 44 bf-wrk-op.t-itemnm AT 54
   SKIP
    "Sheet:" AT 2  wrk-op.t-shtSz AT 8 "Qty:" AT 29 wrk-op.num-sh[1] AT 33 
    "Sheet::" AT 44 bf-wrk-op.t-shtSz AT 51 "Qty:" AT 71 bf-wrk-op.num-sh[1] AT 75   
   SKIP 
    "Die #:" AT 2  wrk-op.t-dieno AT 8
    "Die #:" AT 44 bf-wrk-op.t-dieno AT 50
   WITH NO-BOX FRAME lbl-DC NO-LABELS STREAM-IO WIDTH  132.

FORM
    "Job#:" AT 2 wrk-op.t-jobno AT 7 "Due:" AT 16 v_due AT 20 "MID:" AT 29 wrk-op.m-code    FORMAT "x(6)" AT 34 
    "Job#:" AT 44 bf-wrk-op2.t-jobno AT 49 "Due:" AT 58 v_due2 AT 62 "MID:" AT 71 bf-wrk-op2.m-code FORMAT "x(6)" AT 76 
    SKIP
    "Cust:"  AT 2  wrk-op.t-custnm AT 7 "Cust:"  AT 44 bf-wrk-op2.t-custnm AT 49
   SKIP
    "Number of Item:" AT 2 wrk-op.t-jitm AT 17  "Number of Item:" AT 44 bf-wrk-op2.t-jitm AT 60
   SKIP
    "Total Item Qty:" AT 2 wrk-op.t-jqty AT 17
    "Total Item Qty:" AT 44 bf-wrk-op2.t-jqty AT 60
   WITH NO-BOX FRAME lbl-GLWN NO-LABELS STREAM-IO WIDTH  132.

{sys/inc/print1.i}

{sys/inc/outprint.i value(lines-per-page)}

PUT "<PREVIEW>" "</PROGRESS>".

FOR EACH wrk-op EXCLUSIVE-LOCK   
    BY wrk-op.t-jobno
    BY wrk-op.t-deptsr
    by wrk-op.m-code  
    BY wrk-op.s-num 
    BY wrk-op.b-num  
    BY wrk-op.pass:      

    FIND FIRST bf-wrk-op EXCLUSIVE-LOCK
        WHERE RECID(bf-wrk-op) EQ RECID(wrk-op) NO-ERROR.
        

    /* Process Each Dept. */      
    CASE wrk-op.t-dept:

        WHEN 'PR' THEN RUN Dept_PR. 
        WHEN 'DC' THEN RUN Dept_DC.
        WHEN 'GL' OR
        WHEN 'WN' 
             THEN RUN Dept_GLWN. 

    END CASE.        

        
END.

/* MESSAGE list-name                      */
/*     VIEW-AS ALERT-BOX INFO BUTTONS OK. */


PROCEDURE gather_data:

    DEF VAR v_itmcnt AS INT NO-UNDO.
    DEF VAR v_itmcntT AS INT NO-UNDO.

    DEF BUFFER xjob-hdr FOR job-hdr.

/* Go through each jobs selected by the user.*/ 
FOR EACH job-hdr NO-LOCK 
    WHERE job-hdr.company EQ cocode
      AND job-hdr.job-no  GE icBegJobNo
      AND job-hdr.job-no2 GE iiBegJobNo2
      AND job-hdr.job-no  LE icEndJobNo
      AND job-hdr.job-no2 LE iiEndJobNo2,
    FIRST eb NO-LOCK 
    WHERE eb.company  EQ job-hdr.company
      AND eb.est-no   EQ job-hdr.est-no
      AND eb.form-no  EQ job-hdr.frm
      AND eb.blank-no EQ job-hdr.blank-no,
    FIRST ef NO-LOCK 
    WHERE ef.company  EQ job-hdr.company
      AND ef.est-no   EQ job-hdr.est-no,
    FIRST itemfg OF job-hdr NO-LOCK 
    BREAK BY job-hdr.job-no
          BY job-hdr.job-no2
          BY job-hdr.frm:

    FIND FIRST cust NO-LOCK
        WHERE cust.company EQ job-hdr.company 
          AND cust.cust-no EQ job-hdr.cust-no NO-ERROR.
    FIND FIRST est NO-LOCK
        WHERE est.company EQ job-hdr.company 
         AND est.est-no EQ job-hdr.est-no NO-ERROR.

    IF FIRST-OF(job-hdr.frm) THEN DO:
        
        FIND FIRST job NO-LOCK
            WHERE job.company eq job-hdr.company
              AND job.job     eq job-hdr.job
              AND job.job-no  eq job-hdr.job-no
              AND job.job-no2 eq job-hdr.job-no2 NO-ERROR.
        IF AVAIL job THEN 
            FOR EACH job-mch NO-LOCK
               WHERE job-mch.company EQ job.company
                 AND job-mch.job     EQ job.job
                 AND job-mch.job-no  EQ job.job-no
                 AND job-mch.job-no2 EQ job.job-no2
                 AND job-mch.frm     EQ job-hdr.frm ,
               FIRST mach NO-LOCK
               {sys/ref/machW.i}
                 AND mach.m-code EQ job-mch.m-code
                BY mach.d-seq
                BY job-mch.frm
                BY job-mch.blank-no
                BY job-mch.pass
                BY job-mch.run-qty DESC:

              FOR EACH job-mat NO-LOCK
                  WHERE job-mat.company = job-mch.company
                    AND job-mat.job-no  = job-mch.job-no
                    AND job-mat.job-no2 = job-mch.job-no2
                    AND job-mat.frm     = job-mch.frm,
                  FIRST item NO-LOCK 
                      WHERE item.company  = job-mat.company
                        AND item.i-no     = job-mat.rm-i-no
                        AND item.mat-type = 'B'
                  BREAK BY job-mat.i-no:

                   IF FIRST-OF(job-mat.i-no) 
                     THEN 
                       ASSIGN v_shtsize = String(job-mat.wid) + "x"+ String(job-mat.len).                   
                   
              END.

              ASSIGN
                  v-job-qty = 0 v-job-itm = 0.

              FOR EACH xjob-hdr NO-LOCK
                  WHERE xjob-hdr.company EQ cocode
                    AND xjob-hdr.job     EQ job-hdr.job
                    AND xjob-hdr.job-no  EQ job-hdr.job-no
                    AND xjob-hdr.job-no2 EQ job-hdr.job-no2
                    AND xjob-hdr.i-no    EQ job-hdr.i-no:

                  ASSIGN
                      v-job-qty = v-job-qty + xjob-hdr.qty
                      v-job-itm = v-job-itm + 1.
              END.
              
              FIND FIRST wrk-op
                   WHERE wrk-op.t-jobno EQ job-hdr.job-no
                     AND wrk-op.t-dept  EQ job-mch.dept
                     AND wrk-op.m-code  EQ job-mch.m-code
                     AND wrk-op.s-num   EQ job-mch.frm
                     AND wrk-op.b-num   EQ job-mch.blank-no
                     AND wrk-op.pass    EQ job-mch.pass NO-ERROR.
               IF NOT AVAIL wrk-op THEN DO:
                   CREATE wrk-op.
                   ASSIGN 
                       wrk-op.t-dept   = job-mch.dept
                       wrk-op.m-code   = job-mch.m-code
                       wrk-op.m-dscr   = mach.m-dscr
                       wrk-op.d-seq    = mach.d-seq
                       wrk-op.dept     = job-mch.dept
                       wrk-op.s-num    = job-mch.frm
                       wrk-op.b-num    = job-mch.blank-no
                       wrk-op.pass     = job-mch.pass
                       wrk-op.t-jobno  = job-hdr.job-no
                       wrk-op.t-custnm = cust.NAME
                       wrk-op.t-itemno = job-hdr.i-no
                       wrk-op.t-itemnm = itemfg.i-name
                       wrk-op.t-dieno  = eb.die-no
                       wrk-op.t-shtSz  = v_shtsize 
                       .
               END.

               IF job-mch.dept EQ "PR"
                 THEN ASSIGN wrk-op.t-deptsr = 1.
               IF job-mch.dept EQ "DC"
                 THEN ASSIGN wrk-op.t-deptsr = 2.
               IF job-mch.dept EQ "GL"
                 THEN ASSIGN wrk-op.t-deptsr = 3.
               IF job-mch.dept EQ "WN"
                 THEN ASSIGN wrk-op.t-deptsr = 4.

               ASSIGN 
                 wrk-op.mr[job-mch.frm]       = job-mch.mr-hr
                 wrk-op.speed[job-mch.frm]    = job-mch.speed
                 wrk-op.num-sh[job-mch.frm]   = int(job-mch.run-qty)
                 wrk-op.spoil[job-mch.frm]    = job-mch.wst-prct   
                 wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste   
                 wrk-op.run-hr[job-mch.frm]   = job-mch.run-hr
                 wrk-op.t-jqty                = v-job-qty
                 wrk-op.t-jitm                = v-job-itm.

        END.

    END.

END.

END PROCEDURE.


PROCEDURE Dept_PR:

    IF v_lblcnt LT 1 
      THEN
        DISP " " 
          WITH NO-BOX FRAME lbl-SPC NO-LABELS STREAM-IO WIDTH  132.

    DISP 
        wrk-op.t-jobno    v_due  wrk-op.m-code     
        bf-wrk-op.t-jobno v_due2 bf-wrk-op.m-code  
      SKIP
        wrk-op.t-custnm   bf-wrk-op.t-custnm 
      SKIP
        wrk-op.t-itemno   bf-wrk-op.t-itemno
      SKIP
        wrk-op.t-itemnm   bf-wrk-op.t-itemnm
      SKIP
        wrk-op.t-shtSz
        wrk-op.num-sh[1] 
        bf-wrk-op.t-shtSz 
        bf-wrk-op.num-sh[1] 
      SKIP 
        wrk-op.t-dieno 
        bf-wrk-op.t-dieno 
    WITH NO-BOX FRAME lbl-PR NO-LABELS NO-ATTR-SPACE  STREAM-IO WIDTH  132.

    DISP 
        SKIP
         WITH NO-BOX FRAME lbl-SPC1 NO-LABELS STREAM-IO WIDTH  132.

    DISP 
        wrk-op.t-jobno    v_due  wrk-op.m-code     
        bf-wrk-op.t-jobno v_due2 bf-wrk-op.m-code  
      SKIP
        wrk-op.t-custnm   bf-wrk-op.t-custnm 
      SKIP
        wrk-op.t-itemno   bf-wrk-op.t-itemno
      SKIP
        wrk-op.t-itemnm   bf-wrk-op.t-itemnm
      SKIP
        wrk-op.t-shtSz
        wrk-op.num-sh[1] 
        bf-wrk-op.t-shtSz 
        bf-wrk-op.num-sh[1] 
      SKIP 
        wrk-op.t-dieno 
        bf-wrk-op.t-dieno 
    WITH NO-BOX FRAME lbl-DC NO-LABELS NO-ATTR-SPACE STREAM-IO WIDTH  132.

    DISP 
        SKIP(2)
         WITH NO-BOX FRAME lbl-SPC1 NO-LABELS STREAM-IO WIDTH  132.

    ASSIGN 
        v_lblcnt = v_lblcnt + 2.

    DELETE wrk-op.
    FIND CURRENT bf-wrk-op NO-ERROR.
    IF AVAIL bf-wrk-op THEN DELETE bf-wrk-op.

END PROCEDURE.


PROCEDURE Dept_DC:

    IF v_lblcnt LT 1 
      THEN
        DISP " "
          WITH NO-BOX FRAME lbl-SPC NO-LABELS STREAM-IO WIDTH  132.
          
    DISP 
        wrk-op.t-jobno    v_due  wrk-op.m-code     
        bf-wrk-op.t-jobno v_due2 bf-wrk-op.m-code  
      SKIP
        wrk-op.t-custnm   bf-wrk-op.t-custnm 
      SKIP
        wrk-op.t-itemno   bf-wrk-op.t-itemno
      SKIP
        wrk-op.t-itemnm   bf-wrk-op.t-itemnm
      SKIP
        wrk-op.t-shtSz
        wrk-op.num-sh[1] 
        bf-wrk-op.t-shtSz 
        bf-wrk-op.num-sh[1] 
      SKIP 
        wrk-op.t-dieno 
        bf-wrk-op.t-dieno 
    WITH NO-BOX FRAME lbl-DC NO-LABELS NO-ATTR-SPACE  STREAM-IO WIDTH  132.

    DISP 
        SKIP(2)
         WITH NO-BOX FRAME lbl-SPC1 NO-LABELS STREAM-IO WIDTH  132.

    ASSIGN 
        v_lblcnt = v_lblcnt + 1.

    DELETE wrk-op.
    FIND CURRENT bf-wrk-op NO-ERROR.
    IF AVAIL bf-wrk-op THEN DELETE bf-wrk-op.

END PROCEDURE.


PROCEDURE Dept_GLWN:

    DEF VAR v_dept   LIKE wrk-op.t-dept   NO-UNDO.
    DEF VAR v-jobno  LIKE wrk-op.t-jobno  NO-UNDO.
    DEF VAR v-m-code LIKE wrk-op.m-code   NO-UNDO.
    DEF VAR v-custnm LIKE wrk-op.t-custnm NO-UNDO.
    DEF VAR v-jitm   LIKE wrk-op.t-jitm   NO-UNDO.
    DEF VAR v-jqty   LIKE wrk-op.t-jqty   NO-UNDO.
    
    IF v_lblcnt LT 1 
      THEN
        DISP " "
          WITH NO-BOX FRAME lbl-SPC NO-LABELS STREAM-IO WIDTH  132.
    
    ASSIGN v_dept = IF wrk-op.t-dept = "GL" 
                      THEN "WN" ELSE wrk-op.t-dept.

    FIND FIRST bf-wrk-op2 EXCLUSIVE-LOCK
        WHERE bf-wrk-op2.t-jobno EQ wrk-op.t-jobno
        AND bf-wrk-op2.t-dept   EQ v_dept
        AND bf-wrk-op2.s-num    EQ wrk-op.s-num   
        AND bf-wrk-op2.b-num    EQ wrk-op.b-num NO-ERROR.
    IF AVAIL bf-wrk-op2 THEN
        ASSIGN 
         v-jobno  = bf-wrk-op2.t-jobno   
         v-m-code = bf-wrk-op2.m-code   
         v-custnm = bf-wrk-op2.t-custnm 
         v-jitm   = bf-wrk-op2.t-jitm   
         v-jqty   = bf-wrk-op2.t-jqty. 


    DISP
        wrk-op.t-jobno 
        v_due
        wrk-op.m-code
        v-jobno @ bf-wrk-op2.t-jobno   
        v_due2
        v-m-code @ bf-wrk-op2.m-code
      SKIP
        wrk-op.t-custnm 
        v-custnm @ bf-wrk-op2.t-custnm 
      SKIP
        wrk-op.t-jitm
        v-jitm  @ bf-wrk-op2.t-jitm   
      SKIP
        wrk-op.t-jqty
        v-jqty  @ bf-wrk-op2.t-jqty
    WITH NO-BOX FRAME lbl-GLWN NO-LABELS STREAM-IO WIDTH  132.


    DISP 
        SKIP(2)
         WITH NO-BOX FRAME lbl-SPC1 NO-LABELS STREAM-IO WIDTH  132.

    ASSIGN 
        v_lblcnt = v_lblcnt + 1.

    DELETE wrk-op.
    FIND CURRENT bf-wrk-op2 NO-ERROR.
    IF AVAIL bf-wrk-op2 THEN DELETE bf-wrk-op2.

END PROCEDURE.

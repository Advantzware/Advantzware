/*------------------------------------------------------------------------
  File:         r-wipstd.p
  Description:  Business Logic
  Author:       Ron Stark
  Date Created: 8.20.2021
------------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

/* Temp-Table Definitions ---                                           */

&Scoped-define ttTempTable ttWIPStandards
DEFINE TEMP-TABLE ttWIPStandards NO-UNDO
    FIELD machine     AS CHARACTER FORMAT "x(8)"                 LABEL "Machine"
    FIELD deprt       AS CHARACTER FORMAT "x(3)"                 LABEL "DP"
    FIELD frm         AS INTEGER   FORMAT ">>9"                  LABEL "Form"
    FIELD blank-no    AS INTEGER   FORMAT ">>9"                  LABEL "Blank"
    FIELD pass        AS INTEGER   FORMAT ">>9"                  LABEL "Pass"
    FIELD code        AS CHARACTER FORMAT "x(10)"                LABEL "Charge Code"
    FIELD job-code    AS CHARACTER FORMAT "x(5)"                 LABEL "Charge Cat"
    FIELD op-date     AS DATE      FORMAT "99/99/9999"           LABEL "Date"
    FIELD job-no      AS CHARACTER FORMAT "x(10)"                LABEL "Job"
    FIELD shift       AS INTEGER   FORMAT ">9"                   LABEL "Shift"
    FIELD hours       AS DECIMAL   FORMAT "->>>9.9<<"            LABEL "Hours"
    FIELD start       AS CHARACTER FORMAT "x(5)"                 LABEL "Start"
    FIELD stop        AS CHARACTER FORMAT "x(5)"                 LABEL "Stop"
    FIELD crew        AS INTEGER   FORMAT ">9"                   LABEL "Crew"
    FIELD qty         AS INTEGER   FORMAT "->,>>>,>>9"           LABEL "Quantity"
    FIELD waste       AS INTEGER   FORMAT "->,>>>,>>9"           LABEL "Waste"
    FIELD comp        AS LOGICAL   FORMAT "Yes/No"               LABEL "Complete"
    FIELD stock-no    AS CHARACTER FORMAT "x(15)"                LABEL "FG Item"
    FIELD style       AS CHARACTER FORMAT "x(6)"                 LABEL "Style"
    FIELD len         AS DECIMAL   FORMAT ">>9.9<<"              LABEL "Length"
    FIELD wid         AS DECIMAL   FORMAT ">>9.9<<"              LABEL "Width"
    FIELD dep         AS DECIMAL   FORMAT ">>9.9<<"              LABEL "Depth"
    FIELD t-len       AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Blank Length"
    FIELD t-wid       AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Blank Width"
    FIELD t-sqin      AS DECIMAL   FORMAT ">>>>>>>9.9<<"         LABEL "Blank Sq Inches"
    FIELD board       AS CHARACTER FORMAT "x(8)"                 LABEL "Board"
    FIELD cal         AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Caliper"
    FIELD ld-msf      AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "MSF"
    FIELD weight      AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Wgt/MSF"
    FIELD roll-wid    AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Roll Width"
    FIELD gsh-wid     AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Gross Sheet Width"
    FIELD gsh-len     AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Gross Sheet Length"
    FIELD nsh-wid     AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Net Sheet Width"
    FIELD nsh-len     AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Net Sheet Length"
    FIELD flm-len     AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Film Length"
    FIELD flm-wid     AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Film Width"
    FIELD inkc        AS INTEGER   FORMAT ">9"                   LABEL "Colors"
    FIELD die-in      AS INTEGER   FORMAT ">>>>9"                LABEL "Die Inches"
    FIELD li-up       AS INTEGER   FORMAT ">>9"                  LABEL "Up"
    FIELD n-out       AS INTEGER   FORMAT ">>>>9"                LABEL "Out"
    FIELD lin-in      AS DECIMAL   FORMAT ">>>>9.9<<"            LABEL "Glue Inches"
    FIELD tot-job-qty AS DECIMAL   FORMAT "->>,>>>,>>>,>>9"      LABEL "Total Run Qty"
    FIELD cust-no     AS CHARACTER FORMAT "x(8)"                 LABEL "Customer"
    FIELD name        AS CHARACTER FORMAT "x(30)"                LABEL "Customer Name"
    FIELD price       AS DECIMAL   FORMAT ">>,>>>,>>9.99<<<<"    LABEL "Price"
    FIELD uom         AS CHARACTER FORMAT "x(3)"                 LABEL "UOM"
    FIELD sale-value  AS DECIMAL   FORMAT ">,>>>,>>>,>>9.99<<<<" LABEL "Sales Value"
    FIELD user-id     AS CHARACTER FORMAT "x(10)"                LABEL "User ID"
    FIELD cuts        AS INTEGER   FORMAT ">>>,>>9"              LABEL "Cuts"
    .
{ce/mach-ink.i NEW}

DEFINE NEW SHARED BUFFER xest FOR est.

/* Parameters Definitions ---                                           */

&Scoped-define subjectID 178
{AOA/includes/subjectID{&subjectID}Defs.i}

/* Local Variable Definitions ---                                       */

FUNCTION cvt-time-to-string RETURNS CHARACTER
   (ipcType AS CHAR, ipiTime AS INT, ipdHour AS DECIMAL):
    DEFINE VARIABLE iEndTime AS INTEGER NO-UNDO.

    IF ipcType EQ "END" THEN DO:
       iEndTime = ipiTime + ipdHour * 3600.
       RETURN STRING(iEndTime,"HH:MM").
    END.
    ELSE
    RETURN STRING(ipiTime,"HH:MM").
END FUNCTION.

/* **********************  Internal Procedures  *********************** */

PROCEDURE pBusinessLogic:
    DEFINE VARIABLE dSaleValue  AS DECIMAL NO-UNDO.
    DEFINE VARIABLE i           AS INTEGER NO-UNDO.
    DEFINE VARIABLE iCount      AS INTEGER NO-UNDO.
    DEFINE VARIABLE iStartShift AS INTEGER NO-UNDO.
    DEFINE VARIABLE iEndShift   AS INTEGER NO-UNDO.
    DEFINE VARIABLE hDynInitializeProc AS HANDLE NO-UNDO.
    DEFINE VARIABLE ld-msf      AS DECIMAL NO-UNDO.
    DEFINE VARIABLE li-up       AS INTEGER NO-UNDO.
    DEFINE VARIABLE lv-rc-seq LIKE dept.fc NO-UNDO.
    DEFINE VARIABLE tot-job-qty AS INTEGER NO-UNDO.
    DEFINE VARIABLE v-corr      AS LOGICAL NO-UNDO.

    DEFINE BUFFER bf-mch-act FOR mch-act.
    DEFINE BUFFER b-mch-act  FOR mch-act.

    RUN AOA/spDynInitializeProc.p PERSISTENT SET hDynInitializeProc.
    RUN dynInitNK1AutoPDC IN hDynInitializeProc.
    ASSIGN
        iStartShift = INTEGER(cStartShift)
        iEndShift   = IF cEndShift EQ CHR(254) THEN 999 ELSE INTEGER(cEndShift)
        cExcludeMachines = RETURN-VALUE
        .
    RUN est/rc-seq.p (OUTPUT lv-rc-seq).
    FOR EACH mch-act NO-LOCK
        WHERE mch-act.company   EQ cCompany
          AND mch-act.m-code    GE cStartMachine
          AND mch-act.m-code    LE cEndMachine
          AND mch-act.op-date   GE dtStartDate
          AND mch-act.op-date   LE dtEndDate
          AND mch-act.shift     GE iStartShift
          AND mch-act.shift     LE iEndShift
          AND (mch-act.complete EQ YES
           OR NOT lCompletedMachinesOnly)
          AND LOOKUP(mch-act.m-code,cExcludeMachines) EQ 0
        USE-INDEX dly-idx,
        FIRST mach NO-LOCK
        WHERE mach.company EQ mch-act.company
          AND mach.m-code  EQ mch-act.m-code,
        FIRST job NO-LOCK
        WHERE job.company EQ mch-act.company
          AND job.job     EQ mch-act.job
          AND job.job-no  EQ mch-act.job-no
          AND job.job-no2 EQ mch-act.job-no2,
        FIRST job-hdr OF job NO-LOCK
        :
        FIND FIRST job-code NO-LOCK
             WHERE job-code.code EQ mch-act.code
             NO-ERROR.
        FIND FIRST job-mch NO-LOCK
             WHERE job-mch.company  EQ mch-act.company
               AND job-mch.job      EQ mch-act.job
               AND job-mch.job-no   EQ mch-act.job-no
               AND job-mch.job-no2  EQ mch-act.job-no2
               AND job-mch.frm      EQ mch-act.frm
               AND job-mch.blank-no EQ mch-act.blank-no
               AND job-mch.m-code   EQ mch-act.m-code
               AND job-mch.pass     EQ mch-act.pass
             NO-ERROR.    
        FIND FIRST cust NO-LOCK
             WHERE cust.company EQ mch-act.company
               AND cust.cust-no EQ job-hdr.cust-no
             NO-ERROR.
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ mch-act.company
               AND oe-ordl.i-no    EQ mch-act.i-no
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
             NO-ERROR.
        FIND FIRST itemfg NO-LOCK
             WHERE itemfg.company EQ mch-act.company
               AND itemfg.i-no    EQ mch-act.i-no
             NO-ERROR.
        RELEASE est.
        RELEASE ef.
        RELEASE eb.
        IF TRIM(job.est-no) NE "" THEN
        FIND FIRST est NO-LOCK
             WHERE est.company EQ job.company
               AND est.est-no  EQ job.est-no
             NO-ERROR.
        IF AVAILABLE est THEN
        FIND FIRST ef NO-LOCK
             WHERE ef.company EQ est.company
               AND ef.est-no  EQ est.est-no
               AND ef.form-no EQ mch-act.frm
             NO-ERROR.
        IF AVAILABLE ef THEN
        FIND FIRST eb NO-LOCK
             WHERE eb.company   EQ ef.company
               AND eb.est-no    EQ ef.est-no
               AND eb.form-no   EQ ef.form-no
               AND (eb.blank-no EQ mch-act.blank-no
                OR mch-act.blank-no EQ 0)
             NO-ERROR.
        IF AVAILABLE ef THEN
        FIND FIRST est-flm NO-LOCK
             WHERE est-flm.company EQ ef.company
               AND est-flm.est-no  EQ ef.est-no
               AND est-flm.snum    EQ ef.form-no
               AND est-flm.bnum    EQ mch-act.blank-no
             NO-ERROR.
        IF AVAILABLE est THEN
        FIND FIRST est-op NO-LOCK
             WHERE est-op.company EQ est.company
               AND est-op.est-no  EQ est.est-no
               AND est-op.s-num   EQ mch-act.frm
               AND (est-op.b-num  EQ mch-act.blank-no
                OR mch-act.blank-no EQ 0)
               AND est-op.m-code  EQ mch-act.m-code
               AND est-op.op-pass EQ mch-act.pass
               AND est-op.dept    EQ mch-act.dept
               AND est-op.line    LT 500
             NO-ERROR.
        ld-msf = 0.
        IF AVAILABLE ef THEN DO:
            IF mach.d-seq LT lv-rc-seq THEN
            ld-msf = ef.nsh-len * ef.nsh-wid.
            ELSE
            ld-msf = ef.gsh-len * ef.gsh-wid.
            IF v-corr THEN ld-msf = ld-msf * .007.
            ELSE ld-msf = ld-msf / 144.
            ld-msf = ld-msf / 1000.
        END.
        RELEASE w-ink.
        IF AVAILABLE est THEN DO:
            FIND FIRST xest NO-LOCK
                 WHERE ROWID(xest) EQ ROWID(est)
                 NO-ERROR.
            RUN ce/mach-ink.p.
            FIND FIRST w-ink
                 WHERE w-ink.form-no EQ mch-act.frm
                   AND w-ink.pass    EQ mch-act.pass
                 NO-ERROR.
        END.
        li-up = 1.
        IF AVAILABLE job-mch THEN DO:
            IF job-mch.n-out GT 0 THEN li-up = job-mch.n-out.
            IF job-mch.n-on  GT 0 THEN li-up = job-mch.n-on / li-up.
            IF li-up EQ ? THEN li-up = 1.
        END.
        tot-job-qty = 0.
        FOR EACH bf-mch-act NO-LOCK
            WHERE bf-mch-act.company EQ cCompany
              AND bf-mch-act.m-code  GE mch-act.m-code
              AND bf-mch-act.shift   EQ mch-act.shift
              AND bf-mch-act.job-no  EQ mch-act.job-no
              AND bf-mch-act.job-no2  EQ mch-act.job-no2
            :
            tot-job-qty =  tot-job-qty +  bf-mch-act.qty .
        END.
        IF AVAILABLE mch-act THEN
        BUFFER b-mch-act:FIND-BY-ROWID(ROWID(mch-act), NO-LOCK).
        CREATE ttWIPStandards.
        ASSIGN
            ttWIPStandards.machine     = mch-act.m-code
            ttWIPStandards.deprt       = IF AVAILABLE job-mch THEN mch-act.dep ELSE ""
            ttWIPStandards.frm         = mch-act.frm
            ttWIPStandards.blank-no    = mch-act.blank-no
            ttWIPStandards.pass        = IF AVAILABLE job-mch THEN mch-act.pass ELSE 0
            ttWIPStandards.code        = mch-act.code
            ttWIPStandards.job-code    = IF AVAILABLE job-code THEN job-code.cat ELSE ""
            ttWIPStandards.op-date     = mch-act.op-date
            ttWIPStandards.job-no      = TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job.job-no, job.job-no2)))
            ttWIPStandards.shift       = mch-act.shift
            ttWIPStandards.hours       = mch-act.hours
            ttWIPStandards.start       = IF AVAILABLE mch-act THEN cvt-time-to-string('',mch-act.start,0.00) ELSE ""
            ttWIPStandards.stop        = IF AVAILABLE mch-act THEN cvt-time-to-string('',mch-act.stopp,0.00) ELSE ""
            ttWIPStandards.crew        = mch-act.crew
            ttWIPStandards.qty         = mch-act.qty
            ttWIPStandards.waste       = mch-act.waste
            ttWIPStandards.comp        = mch-act.complete
            ttWIPStandards.stock-no    = IF AVAILABLE eb THEN eb.stock-no ELSE ""
            ttWIPStandards.style       = IF AVAILABLE eb THEN eb.style ELSE ""
            ttWIPStandards.len         = IF AVAILABLE eb THEN eb.len ELSE 0
            ttWIPStandards.wid         = IF AVAILABLE eb THEN eb.wid ELSE 0
            ttWIPStandards.dep         = IF AVAILABLE eb THEN eb.dep ELSE 0
            ttWIPStandards.t-len       = IF AVAILABLE eb THEN eb.t-len ELSE 0
            ttWIPStandards.t-wid       = IF AVAILABLE eb THEN eb.t-wid ELSE 0
            ttWIPStandards.t-sqin      = IF AVAILABLE eb THEN eb.t-sqin ELSE 0
            ttWIPStandards.board       = IF AVAILABLE eb THEN ef.board ELSE ""
            ttWIPStandards.cal         = IF AVAILABLE ef THEN ef.cal ELSE 0
            ttWIPStandards.ld-msf      = ld-msf
            ttWIPStandards.weight      = IF AVAILABLE ef THEN ef.weight ELSE 0
            ttWIPStandards.roll-wid    = IF AVAILABLE ef THEN ef.roll-wid ELSE 0
            ttWIPStandards.gsh-wid     = IF AVAILABLE ef THEN ef.gsh-wid ELSE 0
            ttWIPStandards.gsh-len     = IF AVAILABLE ef THEN ef.gsh-len ELSE 0
            ttWIPStandards.nsh-wid     = IF AVAILABLE ef THEN ef.nsh-wid ELSE 0
            ttWIPStandards.nsh-len     = IF AVAILABLE ef THEN ef.nsh-len ELSE 0
            ttWIPStandards.flm-len     = IF AVAILABLE est-flm THEN est-flm.len ELSE 0
            ttWIPStandards.flm-wid     = IF AVAILABLE est-flm THEN est-flm.wid ELSE 0
            ttWIPStandards.inkc        = IF AVAILABLE w-ink THEN w-ink.inks + w-ink.varn ELSE 0
            ttWIPStandards.die-in      = IF AVAILABLE ef THEN ef.die-in ELSE 0
            ttWIPStandards.li-up       = li-up
            ttWIPStandards.n-out       = IF AVAILABLE job-mch THEN job-mch.n-out ELSE 0
            ttWIPStandards.lin-in      = IF AVAILABLE eb THEN eb.lin-in ELSE 0
            ttWIPStandards.tot-job-qty = tot-job-qty
            ttWIPStandards.cust-no     = job-hdr.cust-no
            ttWIPStandards.name        = IF AVAILABLE cust THEN cust.name ELSE ""
            ttWIPStandards.price       = IF AVAILABLE oe-ordl THEN oe-ordl.price
                                    ELSE IF AVAILABLE itemfg  THEN itemfg.sell-price
                                    ELSE 0
            ttWIPStandards.uom         = IF AVAILABLE oe-ordl THEN oe-ordl.pr-uom
                                    ELSE IF AVAILABLE itemfg  THEN itemfg.pur-uom
                                    ELSE ""
            ttWIPStandards.user-id     = mch-act.user-id
            ttWIPStandards.cuts        = IF AVAILABLE ef THEN ef.n-cuts ELSE 0
            dSaleValue                 = ttWIPStandards.price * (mch-act.qty / IF ttWIPStandards.uom EQ "M" THEN 1000 ELSE 1)
            .
        IF CAN-DO("A,R,S",mach.p-type) THEN
        dSaleValue = dSaleValue * li-up * (IF AVAILABLE job-mch AND job-mch.n-out GT 0 THEN job-mch.n-out ELSE 1).
        ASSIGN
            ttWIPStandards.sale-value = dSaleValue
            iCount                    = iCount + 1 
            .
        IF lProgressBar THEN
        RUN spProgressBar (cProgressBar, iCount, ?).
    END.
    DELETE PROCEDURE hDynInitializeProc.
END PROCEDURE.

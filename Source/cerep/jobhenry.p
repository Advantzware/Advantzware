/* ------------------------------------------------- jc/rep/ticket.p 10/94 gb */
/*  factory ticket                                                            */
/* -------------------------------------------------------------------------- */

DEFINE INPUT PARAMETER v-format LIKE sys-ctrl.char-fld.

{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}

DEFINE NEW SHARED VARIABLE save_id        AS RECID.
DEFINE NEW SHARED VARIABLE v-today        AS DATE      INIT TODAY FORMAT 99/99/9999.
DEFINE NEW SHARED VARIABLE v-job          AS CHARACTER FORMAT "x(9)" EXTENT 2 INIT [" ","zzzzzzzzz"].
DEFINE NEW SHARED VARIABLE v-job2         AS INTEGER   FORMAT "999" EXTENT 2 INIT [000,999].
DEFINE NEW SHARED VARIABLE v-stypart      LIKE style.dscr.
DEFINE NEW SHARED VARIABLE v-dsc          LIKE oe-ordl.part-dscr1 EXTENT 2.
DEFINE NEW SHARED VARIABLE v-size         AS CHARACTER FORMAT "x(26)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-bld-job      LIKE oe-ord.job-no.
DEFINE NEW SHARED VARIABLE v-bld-job2     LIKE oe-ord.job-no2.
DEFINE NEW SHARED VARIABLE v-fill         AS CHARACTER FORMAT "x(132)".
DEFINE NEW SHARED VARIABLE v-frst         AS LOG.
DEFINE NEW SHARED VARIABLE v-ok           AS LOG.
DEFINE NEW SHARED VARIABLE v-skip         AS LOG.
DEFINE NEW SHARED VARIABLE v-est-qty      AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-job-qty      AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-fac          AS DECIMAL.
DEFINE NEW SHARED VARIABLE v-job-no       LIKE oe-ordl.job-no.
DEFINE NEW SHARED VARIABLE v-job-no2      LIKE oe-ordl.job-no2.
DEFINE NEW SHARED VARIABLE v-due-date     LIKE oe-ord.due-date.
DEFINE NEW SHARED VARIABLE v-reprint      AS LOG.
DEFINE NEW SHARED VARIABLE v-up           LIKE eb.num-up.
DEFINE NEW SHARED VARIABLE v-tandem       AS LOG.
DEFINE NEW SHARED VARIABLE v-form-no      LIKE eb.form-no.
DEFINE NEW SHARED VARIABLE v-fup          AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-layout       AS CHARACTER FORMAT "x(27)".
DEFINE NEW SHARED VARIABLE v-out1-id      AS RECID     NO-UNDO.  /* YSK 06/08/01  was~ local var */
DEFINE NEW SHARED VARIABLE v-out2-id      AS RECID     NO-UNDO.  /* YSK 06/08/01  was~ local var */

DEFINE            VARIABLE v-line         AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE v-gsh-qty      AS INTEGER   NO-UNDO.
DEFINE            VARIABLE cnt            AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE v-frm-blk      AS CHARACTER FORMAT "x(6)" NO-UNDO.
DEFINE            VARIABLE v-dec          AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-ovund        AS CHARACTER FORMAT "x(34)" NO-UNDO.
DEFINE            VARIABLE v-mrhr         AS CHARACTER FORMAT "x(5)".
DEFINE            VARIABLE v-cas-dscr     LIKE item.est-dscr.
DEFINE            VARIABLE v-first        AS LOG       NO-UNDO.
DEFINE            VARIABLE v-spec-list    AS CHARACTER FORMAT "x(20)"INIT "QA" NO-UNDO.
DEFINE            VARIABLE lv-form-note   AS cha       NO-UNDO.
DEFINE            VARIABLE v-prev-ext-gap AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-po-no        LIKE oe-ordl.po-no NO-UNDO.
DEFINE            VARIABLE cBoardDscr     AS CHARACTER NO-UNDO.
DEFINE            VARIABLE iItemCount     AS INTEGER   NO-UNDO.
DEFINE            VARIABLE dExpectedPallets     AS DECIMAL   NO-UNDO.

DEFINE WORKFILE w-lo
    FIELD layout LIKE v-layout.

DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEFINE            BUFFER b-eb     FOR eb.
DEFINE            BUFFER b-ef     FOR ef.

DEFINE NEW SHARED WORKFILE wrk-op
    FIELD m-dscr LIKE est-op.m-dscr
    FIELD m-code LIKE est-op.m-code
    FIELD d-seq LIKE est-op.d-seq
    FIELD dept LIKE est-op.dept
    FIELD b-num LIKE est-op.b-num
    FIELD s-num LIKE est-op.s-num
    FIELD pass LIKE est-op.op-pass
    FIELD mr LIKE est-op.op-mr EXTENT 100
    FIELD speed LIKE est-op.op-speed EXTENT 100
    FIELD num-sh LIKE est-op.num-sh EXTENT 100.

DEFINE NEW SHARED WORKFILE wrk-die
    FIELD die-no LIKE eb.die-no
    FIELD form-no LIKE eb.form-no
    FIELD die-size AS CHARACTER FORMAT "x(17)".

DEFINE NEW SHARED WORKFILE wrk-sheet
    FIELD gsh-qty LIKE ef.gsh-qty
    FIELD cal LIKE ef.cal
    FIELD i-no LIKE ef.board
    FIELD brd-dscr LIKE ef.brd-dscr
    FIELD form-no LIKE ef.form-no
    FIELD sh-wid LIKE ef.nsh-len
    FIELD sh-len LIKE ef.nsh-wid.

DEFINE NEW SHARED WORKFILE wrk-film
    FIELD form-no LIKE ef.form-no
    FIELD snum AS INTEGER FORMAT "99"
    FIELD bnum AS INTEGER FORMAT "99"
    FIELD leaf AS CHARACTER FORMAT "x(10)"
    FIELD leaf-l AS DECIMAL FORMAT ">9.9999"
    FIELD leaf-w AS DECIMAL FORMAT ">9.9999".

DEFINE NEW SHARED WORKFILE wrk-ink
    FIELD i-code AS CHARACTER FORMAT "x(10)"
    FIELD form-no LIKE eb.form-no
    FIELD blank-no LIKE eb.blank-no
    FIELD i-dscr AS CHARACTER FORMAT "x(20)"
    FIELD i-qty AS DECIMAL FORMAT ">,>>9.9<"
    FIELD i-pass AS DECIMAL.

DEFINE NEW SHARED WORKFILE wrk-prep
    FIELD code LIKE est-prep.code
    FIELD dscr LIKE est-prep.dscr
    FIELD s-num AS INTEGER FORMAT "99"
    FIELD b-num AS INTEGER FORMAT "99"
    FIELD ml LIKE est-prep.ml.

DEFINE NEW SHARED WORKFILE wrk-spec
    FIELD form-no LIKE ef.form-no
    FIELD spec-no AS CHARACTER FORMAT "x(10)"
    FIELD dscr AS CHARACTER FORMAT "x(20)"
    FIELD qty AS DECIMAL FORMAT ">>>9.9<<<"
    FIELD uom AS CHARACTER FORMAT "x(3)".

DEFINE NEW SHARED WORKFILE wrk-inst
    FIELD d-seq LIKE dept.fc
    FIELD dscr LIKE est-inst.dscr
    FIELD line LIKE est-inst.line-no
    FIELD rec-id AS RECID.

DEFINE NEW SHARED WORKFILE wrk-misc
    FIELD form-no LIKE ef.form-no
    FIELD snum AS INTEGER FORMAT "99"
    FIELD bnum AS INTEGER FORMAT "99"
    FIELD cost AS CHARACTER FORMAT "x(20)".
  
DEFINE TEMP-TABLE tt-fgitem 
    FIELD i-no AS cha.
DEFINE TEMP-TABLE tt-eb 
    FIELD eb-recid AS RECID.

FORM HEADER
    SKIP(1)
    "07/22/02 Job Ticket QF-130"   TO 132
    WITH NO-BOX NO-ATTR-SPACE FRAME bott PAGE-BOTTOM STREAM-IO WIDTH 132.

{custom/formtext.i NEW}
DEFINE        VARIABLE lv-text       AS CHARACTER NO-UNDO.
DEFINE        VARIABLE li            AS INTEGER   NO-UNDO.
DEFINE        VARIABLE lv-is-set     AS LOG       NO-UNDO.

DEFINE        VARIABLE ld-yld        AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ld-sqin       AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ld-msf        AS DECIMAL   NO-UNDO.
DEFINE        VARIABLE ls-fgitem-img AS CHARACTER FORM "x(150)" NO-UNDO.
DEFINE        VARIABLE v-lines       AS INTEGER   NO-UNDO .
DEFINE SHARED VARIABLE s-prt-fgimage AS LOGICAL   NO-UNDO.
DEFINE        VARIABLE v-printline   AS INTEGER   NO-UNDO.
DEFINE        VARIABLE cJobLabel     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMachineLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCycles       AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cFurnish      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cConsistency  AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMoldTime     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cAgitation    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cDelay        AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cOverTemp     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cBeltSpeed    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cDryTime      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cItemList     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cItemID       AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cItemName     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMoldCount    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cGeneralNotes AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cPreAgitate   AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cUpAgitate    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cDownAgitate  AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cOvenTemp1    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cOvenTemp2    AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cOvenTemp3    AS CHARACTER NO-UNDO.

DEFINE        VARIABLE cItemSpecLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cFGItemLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cKeyItemLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMoldsLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cWetWeightLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cFirstDryLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cDscrLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMoldIDsLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cBoneDryLabel  AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMoistureLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cSizeLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cJigAvailableLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cMinWeightLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cFiberContentLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cPackingLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cPalletCountLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cPalletSizeLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCartonCodeLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cPalletLabel AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cInstructionsLabel AS CHARACTER NO-UNDO.

DEFINE        VARIABLE cJobMachCode     AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cJobMachRunQty   AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cCycleValue      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cTotalCount      AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cExpectedPallets AS CHARACTER NO-UNDO.
DEFINE        VARIABLE cEstRecKey       AS CHARACTER NO-UNDO.
DEFINE        VARIABLE lPrintSetHeader  AS LOGICAL NO-UNDO.
DEFINE        VARIABLE cBarCode         AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.

DEFINE BUFFER bff-job-hdr FOR job-hdr.    

{cec/msfcalc.i}
DEFINE BUFFER bf-eb     FOR eb.
DEFINE BUFFER bf-jobhdr FOR job-hdr.
v-fill = FILL("=",132).



/* ************************  Function Prototypes ********************** */

FUNCTION fGetKeyItemEstimateRecKey RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER,
	 ipcJobID AS CHARACTER,
	 ipcJobID2 AS INTEGER) FORWARD.

FUNCTION fGetMiscFields RETURNS CHARACTER
  (iRecKey AS CHARACTER,iId AS CHARACTER)  FORWARD.

FUNCTION fHasMiscFields RETURNS LOGICAL PRIVATE
	(ipcRecKey AS CHARACTER) FORWARD.
            
DEFINE NEW SHARED FRAME head.

FORMAT HEADER
         "<C45>HENRY MOLDED PRODUCTS,INC."   SKIP
         "<C47>Job/Head Especificaci�n"  
         "<C84>Fecha:"  v-today  SKIP
         "<P16><C22>N�mero de orden:<B>" STRING(v-job-no)"</B>" 
         "<C50>M�quina: " "<B>" cJobMachCode "</B>"
         "<C68>Ciclos: " "<B>" cCycleValue "</B>"
         "<P10><C84>Fecha De Vencimiento:"  v-due-date SKIP(1)
         /*v-fill*/
         WITH NO-BOX FRAME headSpanish NO-LABELS STREAM-IO WIDTH 162.

FORMAT HEADER
    "<C45>HENRY MOLDED PRODUCTS,INC."   SKIP
    "<C45>Job/Head Specification" 
    "<C84>Date:"  v-today  SKIP
    "<P16><C29>Head ID:"  "<B>" STRING(v-job-no) "</B>" 
    "<C47>Machine: " "<B>" cJobMachCode "</B>"
    "<C68>Cycles: " "<B>" cCycleValue "</B>"
    "<P10><C84>Due Date:"  v-due-date   SKIP(1)
    
    /*v-fill*/
    WITH NO-BOX FRAME head NO-LABELS STREAM-IO WIDTH 142.

FORMAT "Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
    "Salesman:" AT 68 oe-ord.sname[1] "Order#:" AT 113 oe-ord.ord-no
    WITH NO-BOX FRAME line-head NO-LABELS STREAM-IO WIDTH 132.
   
{sys/inc/notes.i}

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
    v-job[1]    = fjob-no
    v-job[2]    = tjob-no
    v-job2[1]   = fjob-no2
    v-job2[2]   = tjob-no2
    v-reprint   = reprint
    v-spec-list = spec-list.  

FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company               EQ cocode
    AND fill(" ",9 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"999")  GE fjob-no
    AND fill(" ",9 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"999")  LE tjob-no
    AND job-hdr.job-no2 GE fjob-no2
    AND job-hdr.job-no2 LE tjob-no2
    AND (job-hdr.ftick-prnt            EQ v-reprint OR
    PROGRAM-NAME(2) MATCHES "*r-tickt2*")
    AND CAN-FIND(FIRST job WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    AND job.stat    NE "H")
    USE-INDEX job-no,

    FIRST est
    WHERE est.company  EQ job-hdr.company
    AND est.est-no   EQ job-hdr.est-no
    AND est.est-type LE 4  
    NO-LOCK

    BREAK BY job-hdr.job
    BY job-hdr.job-no
    BY job-hdr.job-no2
    BY job-hdr.frm
    BY job-hdr.blank-no:
    
    IF est.est-type = 2 THEN
    FOR EACH eb NO-LOCK 
                  WHERE eb.company EQ est.company
                    AND eb.est-no EQ est.est-no 
                    AND eb.form-no NE 0:          
        CREATE tt-reftable.
        ASSIGN
        tt-reftable.reftable = "jc/jc-calc.p"
        tt-reftable.company  = job-hdr.company
        tt-reftable.loc      = ""
        tt-reftable.CODE     = STRING(job-hdr.job,"999999999")
        tt-reftable.code2    = job-hdr.i-no
        tt-reftable.val[12]  = eb.form-no
        tt-reftable.val[13]  = eb.blank-no
        tt-reftable.est-type = est.est-type .
        
    END.
    FIND FIRST tt-reftable WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
        AND tt-reftable.company  EQ job-hdr.company
        AND tt-reftable.loc      EQ ""
        AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
        AND tt-reftable.val[12] = job-hdr.frm   
        NO-LOCK NO-ERROR.
    IF NOT AVAILABLE tt-reftable THEN 
    DO:
        CREATE tt-reftable.
        ASSIGN 
            tt-reftable.reftable = "jc/jc-calc.p"
            tt-reftable.company  = job-hdr.company
            tt-reftable.loc      = ""
            tt-reftable.CODE     = STRING(job-hdr.job,"999999999")
            tt-reftable.code2    = job-hdr.i-no
            tt-reftable.val[12]  = job-hdr.frm
            tt-reftable.val[13]  = job-hdr.blank-no
            tt-reftable.est-type = est.est-type.

    END.
END.
    

FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company               EQ cocode
    AND fill(" ",9 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"999")  GE fjob-no
    AND fill(" ",9 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"999")  LE tjob-no
    AND job-hdr.job-no2 GE fjob-no2
    AND job-hdr.job-no2 LE tjob-no2
    AND (job-hdr.ftick-prnt            EQ v-reprint OR
    PROGRAM-NAME(2) MATCHES "*r-tickt2*")
    AND CAN-FIND(FIRST job WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    AND job.stat    NE "H")
    USE-INDEX job-no,

    FIRST est
    WHERE est.company  EQ job-hdr.company
    AND est.est-no   EQ job-hdr.est-no
    AND est.est-type LE 4  
    NO-LOCK,
    EACH tt-reftable WHERE (tt-reftable.val[12] EQ job-hdr.frm OR est.est-type EQ 2 ) NO-LOCK

    BREAK BY job-hdr.job
    BY job-hdr.job-no
    BY job-hdr.job-no2
    BY tt-reftable.val[12]
    BY tt-reftable.val[13]:

    IF NOT job-hdr.ftick-prnt THEN 
    DO WHILE TRUE:
        li = li + 1.
        FIND xjob-hdr EXCLUSIVE-LOCK
            WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
            NO-ERROR NO-WAIT.
        IF AVAILABLE xjob-hdr THEN xjob-hdr.ftick-prnt = YES.
        IF li GE 1000 OR xjob-hdr.ftick-prnt THEN LEAVE.
    END.
      
    v-est-qty = IF AVAILABLE est THEN est.est-qty[1] ELSE 0.

    FIND FIRST tt-fgitem WHERE tt-fgitem.i-no EQ job-hdr.i-no NO-ERROR.
    IF NOT AVAILABLE tt-fgitem THEN 
    DO:
        CREATE tt-fgitem.
        tt-fgitem.i-no = job-hdr.i-no.
    END.
    
    IF FIRST-OF(tt-reftable.val[12]) THEN v-first = YES.
    IF FIRST-OF(job-hdr.job-no2) THEN
    ASSIGN        
        lPrintSetHeader = NO. 
      
    /** PRINT JOB HEADER **/
    IF v-first THEN 
    DO:       
        ASSIGN
            v-job-no  = job-hdr.job-no
            v-job-no2 = job-hdr.job-no2.

        FIND FIRST oe-ord
            WHERE oe-ord.company EQ job-hdr.company
            AND oe-ord.ord-no  EQ job-hdr.ord-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE oe-ord THEN
            IF NOT oe-ctrl.p-fact AND (oe-ord.stat EQ "H" OR oe-ord.priceHold) THEN NEXT.
          
        /** SUM UP NUMBER OF SHEETS **/
        FIND FIRST job
            WHERE job.company EQ cocode
            AND job.job     EQ job-hdr.job
            AND job.job-no  EQ v-job-no
            AND job.job-no2 EQ v-job-no2
            NO-LOCK NO-ERROR.
        FIND FIRST job-mch
                WHERE job-mch.company EQ cocode
                AND job-mch.job     EQ job.job
                AND job-mch.job-no  EQ job.job-no
                AND job-mch.job-no2 EQ job.job-no2
                NO-LOCK NO-ERROR.
                
        ASSIGN 
            cJobMachCode = IF AVAIL job-mch THEN TRIM(job-mch.m-code) ELSE ""
            cCycleValue  = IF AVAIL job-mch THEN TRIM(STRING(job-mch.run-qty)) ELSE "" .
        v-due-date = IF AVAILABLE oe-ord THEN oe-ord.due-date ELSE job.due-date.
        
                
        PUT "<FCalibri>" .
               
        v-printline = 5 .
        IF v-format EQ "Fibre" THEN VIEW FRAME bott.

        v-line = IF AVAILABLE est                            AND
            est.est-type GT 2 AND est.est-type LT 5 THEN 500 ELSE 50.
                            
        IF AVAILABLE job THEN
            FOR EACH job-mch
                WHERE job-mch.company EQ cocode
                AND job-mch.job     EQ job.job
                AND job-mch.job-no  EQ job.job-no
                AND job-mch.job-no2 EQ job.job-no2
                AND job-mch.frm = int(tt-reftable.val[12])
                NO-LOCK,

                FIRST mach
                {sys/ref/machW.i}
              and mach.m-code eq job-mch.m-code
            no-lock

            by mach.d-seq
            by job-mch.frm
            by job-mch.blank-no
            by job-mch.pass
            by job-mch.run-qty desc:
            
        FIND FIRST wrk-op
            WHERE wrk-op.m-code EQ job-mch.m-code
            AND wrk-op.s-num  EQ job-mch.frm
            AND wrk-op.b-num  EQ job-mch.blank-no
            AND wrk-op.pass   EQ job-mch.pass
            NO-ERROR.
        IF NOT AVAILABLE wrk-op THEN 
        DO:
            CREATE wrk-op.
            ASSIGN
                wrk-op.m-code = job-mch.m-code
                wrk-op.m-dscr = mach.m-dscr
                wrk-op.d-seq  = mach.d-seq
                wrk-op.dept   = job-mch.dept
                wrk-op.s-num  = job-mch.frm
                wrk-op.b-num  = job-mch.blank-no
                wrk-op.pass   = job-mch.pass.
        END.
        ASSIGN
            wrk-op.mr[job-mch.frm]     = job-mch.mr-hr
            wrk-op.speed[job-mch.frm]  = job-mch.speed
            wrk-op.num-sh[job-mch.frm] = job-mch.run-qty.
    END.

    /** BUILD PREP WORK FILE **/
    FOR EACH job-prep
        WHERE job-prep.company EQ cocode
        AND job-prep.job     EQ job-hdr.job
        AND job-prep.job-no  EQ job-hdr.job-no
        AND job-prep.job-no2 EQ job-hdr.job-no2
        NO-LOCK:
        FIND FIRST prep
            WHERE prep.company EQ cocode
            AND prep.code    EQ job-prep.code
            NO-LOCK NO-ERROR.
        CREATE wrk-prep.
        ASSIGN
            wrk-prep.code  = job-prep.code
            wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
            wrk-prep.s-num = job-prep.frm
            wrk-prep.b-num = job-prep.blank-no
            wrk-prep.ml    = job-prep.ml.
    END. /* each job-prep */

    IF AVAILABLE est THEN
        FOR EACH est-prep
            WHERE est-prep.company EQ est.company
            AND est-prep.est-no  EQ est.est-no
            AND index("SON",est-prep.simon) GT 0
            NO-LOCK:
            FIND FIRST prep
                WHERE prep.company EQ cocode
                AND prep.code    EQ est-prep.code
                NO-LOCK NO-ERROR.
            CREATE wrk-prep.
            ASSIGN
                wrk-prep.code  = est-prep.code
                wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
                wrk-prep.s-num = est-prep.s-num
                wrk-prep.b-num = est-prep.b-num
                wrk-prep.ml    = est-prep.ml.
        END.

    IF AVAILABLE oe-ord THEN
        FOR EACH oe-ordm 
            WHERE oe-ordm.company EQ cocode
            AND oe-ordm.ord-no  EQ oe-ord.ord-no
            NO-LOCK:
            FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.
            IF NOT AVAILABLE wrk-prep THEN 
            DO:
                FIND FIRST prep
                    WHERE prep.company EQ cocode
                    AND prep.code    EQ oe-ordm.charge
                    NO-LOCK NO-ERROR.
                CREATE wrk-prep.
                ASSIGN
                    wrk-prep.code  = oe-ordm.charge
                    wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
                    wrk-prep.s-num = 9
                    wrk-prep.b-num = 99
                    wrk-prep.ml    = IF AVAILABLE prep THEN prep.ml ELSE ?.
            END.
        END.


FOR EACH ef
    NO-LOCK WHERE ef.company EQ job-hdr.company
    AND ef.est-no  EQ job-hdr.est-no
    AND ef.form-no EQ tt-reftable.val[12]
    BREAK BY ef.est-no BY ef.form-no:
          
    v-job-qty = 0.
    FOR EACH xjob-hdr FIELDS(qty)
        WHERE xjob-hdr.company EQ cocode
        AND xjob-hdr.job     EQ job-hdr.job
        AND xjob-hdr.job-no  EQ job-hdr.job-no
        AND xjob-hdr.job-no2 EQ job-hdr.job-no2
        AND (xjob-hdr.i-no    EQ job-hdr.i-no OR est.est-type EQ 2)
        NO-LOCK:
        v-job-qty = v-job-qty + xjob-hdr.qty.
    END.
          
    v-est-qty = 0.
    IF est.est-type EQ 4 THEN
        FOR EACH eb
            WHERE eb.company  EQ ef.company
            AND eb.est-no   EQ ef.est-no
            AND eb.stock-no EQ job-hdr.i-no
            NO-LOCK:
            v-est-qty = v-est-qty + eb.yld-qty.
        END.

    ELSE v-fac = 1.    

    FOR EACH tt-eb:
        DELETE tt-eb.
    END.
    IF ef.form-no EQ tt-reftable.val[12] THEN  
    DO:
                         
        FIND FIRST wrk-op NO-LOCK NO-ERROR .
        i = 0.
        IF NOT lPrintSetHeader THEN 
        DO:  
            lPrintSetHeader = YES.
            iItemCount = 0.
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.frm     EQ job-hdr.frm
                NO-LOCK,
                FIRST item
                WHERE item.company EQ cocode
                AND item.i-no    EQ job-mat.i-no
                AND index("BPR",item.mat-type) GT 0
                NO-LOCK:
                cBoardDscr =  ITEM.i-no + " - " + ITEM.i-name .
                LEAVE.
            END.
            
            IF lSpanish THEN
            DO:
               VIEW FRAME headSpanish.
            END.
            ELSE DO:
               VIEW FRAME head.
            END.
            
            RUN pGetPrintLabel1(INPUT lSpanish, OUTPUT cJobLabel, OUTPUT cMachineLabel, OUTPUT cCycles, OUTPUT cFurnish,
                                 OUTPUT cConsistency, OUTPUT cMoldTime , OUTPUT cAgitation, OUTPUT cDelay , OUTPUT cOverTemp,
                                 OUTPUT cBeltSpeed, OUTPUT cDryTime, OUTPUT cItemList, OUTPUT cItemID, OUTPUT cItemName , 
                                 OUTPUT cMoldCount, OUTPUT cGeneralNotes, OUTPUT cPreAgitate, OUTPUT cUpAgitate, OUTPUT cDownAgitate, OUTPUT cOvenTemp1, OUTPUT cOvenTemp2, OUTPUT cOvenTemp3).
        
            PUT "<C2><#2><R+10><C+39><RECT#2><|3>"
                "<#3><R-10><C+23><RECT#3><|3>"
                "<#4><R+10><C+44><RECT#4><|3>" SKIP.
            
            IF fHasMiscFields(est.rec_key) THEN 
                cEstRecKey = est.rec_key.
            ELSE DO:
                cEstRecKey = fGetKeyItemEstimateRecKey(job-hdr.company, job-hdr.job-no, job-hdr.job-no2).
                IF cEstRecKey EQ "" THEN 
                    cEstRecKey = est.rec_key.
            END.
            
            PUT "<=#2> <C3>" cFurnish FORMAT "x(18)"  cBoardDscr  FORMAT "x(40)" SKIP
                "<C3>" cMoldTime    FORMAT "x(20)"    "<C15><B>" STRING(fGetMiscFields(cEstRecKey,"00008")) "</B>"    "<C21>" cConsistency FORMAT "x(25)" "<C37><B>" STRING(fGetMiscFields(cEstRecKey,"00007")) "</B>" SKIP
                "<C3>" cPreAgitate  FORMAT "x(20)"    "<C15><B>" STRING(fGetMiscFields(cEstRecKey,"00009")) "</B>"    "<C21>" cDryTime     FORMAT "x(25)" "<C37><B>" STRING(fGetMiscFields(cEstRecKey,"00014")) "</B>" SKIP
                "<C3>" cUpAgitate   FORMAT "x(20)"    "<C15><B>" STRING(fGetMiscFields(cEstRecKey,"00010")) "</B>"    "<C21>" cBeltSpeed   FORMAT "x(25)" "<C37><B>" STRING(fGetMiscFields(cEstRecKey,"00015")) "</B>" SKIP
                "<C3>" cDownAgitate FORMAT "x(20)"    "<C15><B>" STRING(fGetMiscFields(cEstRecKey,"00011")) "</B>"    "<C21>" cOvenTemp1   FORMAT "x(25)" "<C37><B>" STRING(fGetMiscFields(cEstRecKey,"00016")) "</B>" SKIP
                "<C3>" cAgitation   FORMAT "x(20)"    "<C15><B>" STRING(fGetMiscFields(cEstRecKey,"00012")) "</B>"    "<C21>" cOvenTemp2   FORMAT "x(25)" "<C37><B>" STRING(fGetMiscFields(cEstRecKey,"00017")) "</B>" SKIP
                "<C3>" cDelay       FORMAT "x(20)"    "<C15><B>" STRING(fGetMiscFields(cEstRecKey,"00013")) "</B>"    "<C21>" cOvenTemp3   FORMAT "x(25)" "<C37><B>" STRING(fGetMiscFields(cEstRecKey,"00018")) "</B>" SKIP
                .
   
            PUT "<=#3><R-10> <C41.2><B>" cItemList FORMAT "x(30)"  "</b> "  SKIP
                "<C42>  " cItemID FORMAT "x(27)"       "<C55>" cMoldCount FORMAT "x(25)" SKIP    .
            j = 9.     
            
            FOR EACH b-eb NO-LOCK
                WHERE b-eb.company EQ cocode
                  AND b-eb.est-no    EQ job-hdr.est-no
                  AND (b-eb.form-no  GT 0 OR b-eb.est-type EQ 2)
                  BY b-eb.form-no 
                  BY b-eb.blank-no:
                  
                  FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ job-hdr.company
                    AND itemfg.i-no    EQ b-eb.stock-no
                    NO-ERROR .
                    
                  i = i + 1.
                  PUT "<=#3><C41.2><R-" + STRING(j - i) + ">" FORMAT "x(18)" i FORMAT "9"  "<C42>  " b-eb.stock-no  FORMAT "x(15)" 
                      "<C55>" (IF AVAILABLE b-eb THEN b-eb.num-up ELSE 0)  SKIP   .  
            END.
            PUT SKIP(j - i) .
         
            PUT "<=#4><P10> <C64.2><B> " cGeneralNotes FORMAT "x(18)" "</b> "  SKIP .
         
            FOR EACH notes
                WHERE notes.rec_key   EQ job.rec_key
                AND notes.note_code NE ""
                AND LOOKUP(notes.note_code,v-exc-depts) EQ 0
                NO-LOCK:
                /*IF lv-text = "" THEN lv-text = notes.note_title + CHR(10).*/
                lv-text = lv-text + " " + notes.note_title + CHR(10) + 
                    TRIM(notes.note_text) + CHR(10).
            END.
            i = 0.
            IF lv-text NE "" THEN 
            DO:
                FOR EACH tt-formtext:
                    DELETE tt-formtext.
                END.

                DO li = 1 TO 20:
                    CREATE tt-formtext.
                    ASSIGN
                        tt-line-no = li
                        tt-length  = 75.
                END.

                RUN custom/formtext.p (lv-text).
                i = 0.
                FOR EACH tt-formtext WHERE tt-text NE "" BREAK BY tt-line-no:
                    i = i + 1 .
                    PUT "<=#4><P8><C64><R+" + STRING(i) + ">" FORMAT "x(22)" tt-formtext.tt-text FORMAT "x(80)"  SKIP.
                    
                    IF i GE 9 THEN LEAVE.
                END.
            END.
         
            PUT "<P10>"  SKIP(9 - i)    .           
            
        END.
        
        /*==========*/
        IF ef.form-no EQ tt-reftable.val[12] THEN  
        ebloop:
        FOR EACH eb
            WHERE eb.company     EQ ef.company
            AND eb.est-no      EQ ef.est-no
            AND (eb.form-no     EQ ef.form-no OR (ef.form-no EQ 1 AND eb.form-no EQ 0))
            NO-LOCK
            
            BREAK BY eb.form-no BY eb.blank-no.
                             
            IF iItemCount GT 1 AND iItemCount MOD 2 EQ 0 THEN 
            DO:           
                PAGE.
                IF lSpanish THEN
                DO:
                    VIEW FRAME headSpanish.
                END.
                ELSE 
                DO:
                    VIEW FRAME head.
                END.
            END.   
            iItemCount = iItemCount + 1.     
            CREATE w-lo.
            FOR EACH b-eb
                WHERE b-eb.company EQ eb.company
                AND b-eb.est-no  EQ eb.est-no
                AND b-eb.part-no EQ eb.part-no
                NO-LOCK BREAK BY b-eb.est-no:
                v-fup = "F" + trim(STRING(b-eb.form-no,">>9")) + "-" +
                    trim(STRING(b-eb.blank-no,"99")) + "/" +
                    trim(STRING(b-eb.num-up,">>9")) + "up".
                IF LENGTH(TRIM(v-fup)) + length(TRIM(w-lo.layout)) GT 30 THEN 
                DO:
                    substr(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                    CREATE w-lo.
                END.
                w-lo.layout = TRIM(w-lo.layout + " " + trim(v-fup) + ",").
                IF LAST(b-eb.est-no) THEN
                    substr(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
            END.
          
            FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
            IF NOT AVAILABLE wrk-die AND eb.die-no GT "" THEN 
            DO:
                CREATE wrk-die.
                ASSIGN 
                    wrk-die.die-no   = eb.die-no
                    wrk-die.form-no  = eb.form-no
                    wrk-die.die-size = STRING(ef.trim-w) + "x" +
              string(ef.trim-l).
            END.

            /** BUILD INK WORK FILE **/
            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ job-hdr.job
                AND job-mat.frm     EQ eb.form-no
                AND job-mat.blank-no EQ eb.blank-no
                NO-LOCK,
                FIRST item
                {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no
              no-lock:

            DO i = 1 TO 12:
                IF eb.i-code2[i] EQ job-mat.i-no THEN 
                DO:

                    FIND FIRST wrk-ink
                        WHERE wrk-ink.i-code   EQ eb.i-code2[i]
                        AND wrk-ink.form-no  EQ eb.form-no
                        AND wrk-ink.blank-no EQ eb.blank-no
                        NO-ERROR.

                    IF NOT AVAILABLE wrk-ink THEN 
                    DO:
                        CREATE wrk-ink.
                        ASSIGN
                            wrk-ink.i-code   = eb.i-code2[i]
                            wrk-ink.form-no  = eb.form-no
                            wrk-ink.blank-no = eb.blank-no
                            wrk-ink.i-dscr   = eb.i-dscr2[i]
                            wrk-ink.i-pass   = eb.i-ps2[i].
                    END.
                END.
            END.

            FIND FIRST wrk-ink
                WHERE wrk-ink.i-code    EQ job-mat.i-no
                AND wrk-ink.form-no   EQ job-mat.frm
                AND (wrk-ink.blank-no EQ job-mat.blank-no OR
                est.est-type     EQ 4)
                NO-ERROR.
                
            IF NOT AVAILABLE wrk-ink                              AND
                (job-mat.blank-no  EQ eb.blank-no OR
                (job-mat.blank-no EQ 0 AND eb.blank-no EQ 1)) THEN 
            DO:
                CREATE wrk-ink.
                ASSIGN
                    wrk-ink.i-code   = job-mat.i-no
                    wrk-ink.form-no  = eb.form-no
                    wrk-ink.blank-no = eb.blank-no
                    wrk-ink.i-dscr   = item.est-dscr
                    wrk-ink.i-pass   = 1.
            END.

            IF AVAILABLE wrk-ink THEN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.
        END. /* JOB-MAT */

        IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
          
        /*if last-of(eb.form-no) then do:ysk*/
        FIND FIRST style
            WHERE style.company EQ eb.company
            AND style.style   EQ eb.style
            NO-LOCK NO-ERROR.
        IF AVAILABLE style THEN v-stypart = style.dscr.
        ASSIGN
            v-dsc[1]  = eb.part-dscr1
            v-dsc[2]  = eb.part-dscr2
            v-size[1] = STRING(eb.len) + "x" + string(eb.wid) + "x" +
                         string(eb.dep)
            v-size[2] = eb.i-coldscr.             
            
        FIND FIRST bff-job-hdr NO-LOCK
             WHERE bff-job-hdr.company  EQ job-hdr.company
               AND bff-job-hdr.job-no   EQ job-hdr.job-no
               AND bff-job-hdr.job-no2  EQ job-hdr.job-no2
               AND bff-job-hdr.frm      EQ job-hdr.frm
               AND bff-job-hdr.blank-no EQ eb.blank-no NO-ERROR.
        
        RUN pGetPrintLabel2(INPUT lSpanish, OUTPUT cItemSpecLabel, OUTPUT cFGItemLabel, OUTPUT cKeyItemLabel, OUTPUT cMoldsLabel, OUTPUT cWetWeightLabel,
                             OUTPUT cFirstDryLabel, OUTPUT cDscrLabel , OUTPUT cMoldIDsLabel, OUTPUT cBoneDryLabel , OUTPUT cMoistureLabel,
                             OUTPUT cSizeLabel, OUTPUT cJigAvailableLabel, OUTPUT cMinWeightLabel, OUTPUT cFiberContentLabel, OUTPUT cPackingLabel , 
                             OUTPUT cPalletCountLabel, OUTPUT cPalletSizeLabel, OUTPUT cCartonCodeLabel, OUTPUT cPalletLabel, OUTPUT cInstructionsLabel, OUTPUT cTotalCount, OUTPUT cExpectedPallets).
                             
        PUT "<R-0.5><C45>" cItemSpecLabel FORMAT "x(23)" SKIP .
        
            
        PUT "<C2><#5><R+3><C+108><RECT#5><|3>" SKIP
            "<C2><#6><R+12><C+45><RECT#6><|3>"
            "<#7><R-12><C+33><RECT#7><|3>"
            "<#8><R+12><C+28.5><RECT#8><|3>" SKIP.

        IF lv-is-set THEN v-first = NO.

        /** PRINT ITEM **/
        FIND FIRST oe-ordl
            WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ job-hdr.ord-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND oe-ordl.i-no    EQ eb.stock-no
            NO-LOCK NO-ERROR.

        IF AVAILABLE oe-ordl THEN 
        DO:
            v-est-qty = oe-ordl.qty.
            FIND FIRST oe-ord OF oe-ordl NO-LOCK.
            v-ovund = STRING("Overrun/Underrun %:  " +
                trim(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
                trim(STRING(oe-ordl.under-pct,">>9.99"))).
        END.
        ELSE v-est-qty = v-job-qty.
            
        RELEASE w-lo.
        FIND FIRST w-lo NO-ERROR.
        
        FIND FIRST itemfg NO-LOCK
            WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ eb.stock-no
            NO-ERROR .          
        
            IF eb.tr-cnt NE 0 THEN
            DO:
               ASSIGN dExpectedPallets =  ( (IF AVAILABLE bff-job-hdr THEN bff-job-hdr.qty ELSE job-hdr.qty) / eb.tr-cnt ) .
                {sys/inc/roundup.i dExpectedPallets}
            END.
            ELSE dExpectedPallets = 0 .                  
                    
        PUT "<=#5> <C3>" cFGItemLabel FORMAT "x(10)" "<C8><B>" eb.stock-no FORMAT "x(15)"  "</B><C20>" cKeyItemLabel FORMAT "x(15)" "<B>" (IF AVAIL bff-job-hdr THEN bff-job-hdr.keyItem ELSE job-hdr.keyItem) "</B> "  
            "<C35>" cMoldsLabel FORMAT "x(8)" "<C44><B>" TRIM(STRING(eb.num-up)) "</B>"   
            "<C50>" cWetWeightLabel FORMAT "x(13)" "<C59><B>" STRING(fGetMiscFields(itemfg.rec_key,"00001")) "</B>" 
            "<C65>" cFirstDryLabel FORMAT "x(14)" "<C74><B>" STRING(fGetMiscFields(itemfg.rec_key,"00003")) "</B>" SKIP
            "<C3>" cDscrLabel FORMAT "x(13)" "<C8>" TRIM(IF AVAILABLE itemfg THEN itemfg.part-dscr1 ELSE "") FORMAT "x(30)"    
            "<C35>" cMoldIDsLabel FORMAT "x(25)"  
            "<C50>" cBoneDryLabel FORMAT "x(10)" "<C59><B>"( IF AVAILABLE itemfg THEN TRIM(STRING(itemfg.weightPerEA,">>>>9.99")) ELSE "") FORMAT "x(30)" "</B>" 
            "<C65>" cMoistureLabel FORMAT "X(9)" "<C74><B>" STRING(fGetMiscFields(itemfg.rec_key,"00005")) "</B>" SKIP
            "<C3>" cSizeLabel FORMAT "x(8)" "<C8>" TRIM(STRING(eb.len) + " x " + STRING(eb.wid) + " x " + STRING(eb.dep))  
            "<C35>" cJigAvailableLabel FORMAT "x(21)" "<C44><B>" STRING(fGetMiscFields(itemfg.rec_key,"00004")) "</B>"  
            "<C50>" cMinWeightLabel FORMAT "x(12)" "<C59><B>" STRING(fGetMiscFields(itemfg.rec_key,"00002")) "</B>" 
            "<C65>" cFiberContentLabel FORMAT "x(20)" "<C74><B>" STRING(fGetMiscFields(itemfg.rec_key,"00006")) "</B>" SKIP
            .
        IF AVAILABLE bff-job-hdr THEN
        cBarCode = STRING(TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', bff-job-hdr.job-no, bff-job-hdr.job-no2))) + "-" + STRING(bff-job-hdr.frm,"99") + "-" + STRING(bff-job-hdr.blank-no,"99")).
        ELSE
        cBarCode = STRING(TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', job-hdr.job-no, job-hdr.job-no2))) + "-" + STRING(job-hdr.frm,"99") + "-" + STRING(job-hdr.blank-no,"99")).
               
        PUT "<=#5><R+0.5><UNITS=INCHES><C80><FROM><C105><r+2><BARCODE,TYPE=128B,CHECKSUM=NONE,VALUE=" 
        cBarCode FORMAT "x(19)" "><R-3>" . 
               
        PUT "<=#6><R-1> <C3><B>" cPackingLabel FORMAT "x(8)"  "</B>" SKIP
            "<C3>" cTotalCount                 FORMAT "x(23)" "<B><C15>" TRIM(STRING((IF AVAILABLE bff-job-hdr THEN bff-job-hdr.qty ELSE job-hdr.qty), ">>>,>>>,>>9")) "</B>"
            "<C25>" cCartonCodeLabel           FORMAT "x(20)" "<C35>" TRIM(eb.cas-no) SKIP
            "<C3>" cPalletCountLabel           FORMAT "x(23)" "<C15>" TRIM(STRING(eb.tr-cnt,">,>>>,>>9"))    
            "<C25>" cPalletSizeLabel           FORMAT "x(23)" "<C35>" TRIM(IF AVAILABLE eb THEN (STRING(eb.tr-len) + " x " +  STRING(eb.tr-wid) + " x " +  STRING(eb.tr-dep)) ELSE "") FORMAT "x(12)" SKIP
            "<C3>" cExpectedPallets            FORMAT "x(23)"  "<B><C15>" TRIM(STRING(dExpectedPallets )) "</B>"
            "<C25>" cPalletLabel               FORMAT "x(25)"  "<C35>" TRIM(IF AVAILABLE eb THEN eb.tr-no ELSE "") SKIP
            "<C2><FROM><C47><LINE>" .
           
                
        PUT "<=#7><R-9><C3><B>" cInstructionsLabel FORMAT "x(13)" "</B>" SKIP . 
        lv-text = "".
        i = 0.
        IF AVAILABLE itemfg THEN
            FOR  EACH notes
                WHERE notes.rec_key   EQ itemfg.rec_key
                AND notes.note_type EQ "S"
                AND LOOKUP(notes.note_code,v-spec-list) GT 0
                NO-LOCK:
                lv-text = lv-text + " " + TRIM(notes.note_text) + CHR(10).
            END.
            
        IF lv-text NE "" THEN 
        DO:
            FOR EACH tt-formtext:
                DELETE tt-formtext.
            END.

            DO li = 1 TO 9:
                CREATE tt-formtext.
                ASSIGN
                    tt-line-no = li
                    tt-length  = 80.
            END.

            RUN custom/formtext.p (lv-text).
            j = 9. 
            i = 0.
            FOR EACH tt-formtext WHERE tt-text NE "" BREAK BY tt-line-no:
                i = i + 1.
                PUT "<=#7><R-" + STRING(j - i) + "><C3><P8>" FORMAT "x(25)" tt-formtext.tt-text FORMAT "x(80)"  SKIP.
                IF i GE 9 THEN LEAVE.    
            END.
           
        END.
        PUT "<P10>"  SKIP(8 - i).     
        RUN pBoxDesign .

    END. /* each eb , ebloop*/
END. /* do: */ 
       
END. /* each ef */

END. /* first job-no */

      
IF LAST(job-hdr.job-no2) THEN 
DO:

    FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
        AND bf-jobhdr.job-no = job-hdr.job-no
        AND bf-jobhdr.job-no2 = job-hdr.job-no2
        BREAK BY bf-jobhdr.frm
        BY bf-jobhdr.blank-no:
        IF FIRST-OF(bf-jobhdr.blank-no) THEN 
        DO:
            IF s-prt-fgimage THEN 
            DO:            
                FIND FIRST itemfg NO-LOCK
                    WHERE itemfg.company EQ job-hdr.company 
                    AND itemfg.i-no    EQ bf-jobhdr.i-no NO-ERROR.
                    
                IF avail itemfg THEN    
                FOR EACH ATTACH  NO-LOCK
                    WHERE attach.company EQ cocode
                    AND trim(attach.est-no) EQ trim(bf-jobhdr.est-no)
                    AND  attach.i-no EQ itemfg.i-no
                    AND ATTACH.spare-int-1 EQ 1 BREAK BY attach.i-no :
                    
                    IF FIRST(attach.i-no) THEN
                    PAGE. 
                    
                   PUT UNFORMATTED "<#12><C1><FROM><C106><R+47><RECT><||3><C80>" /*v-qa-text*/ SKIP
                           "<=12><C30><FROM><R+4><C30><LINE><|3>"
                           "<=12><C60><FROM><R+4><C60><LINE><|3>"
                          "<=12><R+1><C5>Job # <C30> Estimate #" "<C60> FG Item:" itemfg.i-no
                          "<=12><R+2><C8>" TRIM(STRING(DYNAMIC-FUNCTION('sfFormat_JobFormatWithHyphen', bf-jobhdr.job-no, bf-jobhdr.job-no2))) FORM "x(13)"   "<C35>"  bf-jobhdr.est-no  
                          "<C60> File Name: " STRING( SUBSTR(attach.attach-file,r-INDEX(attach.attach-file,'\') + 1)) FORMAT "x(50)"
                          "<=12><R+4><C1><FROM><C106><LINE><||3>"
                          "<=12><R+5><C5><#21><R+42><C+90><IMAGE#21=" attach.attach-file ">" SKIP.  
                      PAGE.
                END.
                              
            END.
        END. /* FIRST-OF(bf-jobhdr.frm) */
    END. /* bf-jobhdr */

END. /* last(job-hdr.job-no2)*/

/** PRINT MULT COPIES OF TICKETS **/
save_id = RECID(job-hdr).
IF LAST-OF(job-hdr.job-no2) THEN 
DO:
    FOR EACH wrk-op:
        DELETE wrk-op.
    END.
    FOR EACH wrk-prep:
        DELETE wrk-prep.
    END.
END.
FOR EACH wrk-spec:
    DELETE wrk-spec.
END.
FOR EACH wrk-film:
    DELETE wrk-film.
END.
FOR EACH wrk-die:
    DELETE wrk-die.
END.
FOR EACH wrk-sheet:
    DELETE wrk-sheet.
END.
FOR EACH wrk-misc:
    DELETE wrk-misc.
END.
FOR EACH wrk-inst:
    DELETE wrk-inst.
END.
      
v-first = NO.
END. /* each job-hdr */
    
PROCEDURE pBoxDesign:
    FIND box-design-hdr
            WHERE box-design-hdr.design-no EQ 0
            AND box-design-hdr.company     EQ eb.company 
            AND box-design-hdr.est-no      EQ eb.est-no
            AND box-design-hdr.form-no     EQ eb.form-no
            AND box-design-hdr.blank-no    EQ eb.blank-no
            NO-LOCK NO-ERROR.
        IF AVAILABLE box-design-hdr THEN
            ASSIGN
                FILE-INFO:FILE-NAME = box-design-hdr.box-image.
     
        PUT UNFORMATTED "<=#6><R+3><C110>"
                        "<#71><C47.5><R+6>"
                        "<IMAGE#71=" FILE-INFO:FULL-PATHNAME ">"
                        "<R+3>" 
                        .
        ls-fgitem-img = IF AVAILABLE itemfg THEN itemfg.box-image ELSE "" .
                
                PUT UNFORMATTED                     
                    "<=#6><R+2><C110>"
                    "<#21><C83><R+8>"
                    "<IMAGE#21=" ls-fgitem-img ">"
                    "<R+3>"
                    .   
END PROCEDURE.


PROCEDURE pGetPrintLabel1:
  DEFINE INPUT PARAMETER iplSpanish AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opcJobLabel AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcMachineLabel AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcCycles AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcFurnish AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcConsistency AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcMoldTime  AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcAgitation AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDelay AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcOverTemp AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcBeltSpeed AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDryTime AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcItemList AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcItemID AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcItemName AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcMoldCount AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcGeneralNotes AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcPreAgitate AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcUpAgitate AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcDownAgitate AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcOvenTemp1 AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcOvenTemp2 AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcOvenTemp3 AS CHARACTER NO-UNDO.
   
    IF iplSpanish THEN DO:
     ASSIGN
         opcJobLabel      = "N�mero de orden: "
         opcMachineLabel  = "M�quina: "
         opcCycles        = "Ciclos: "
         opcFurnish       = "Material de pulpa: "
         opcConsistency   = "Consistencia de pulpa: "
         opcMoldTime      = "Tiempo de molde: "
         opcAgitation     = "Agitaci�n: "
         opcDelay         = "Retrasar: "
         opcOverTemp      = "Temperatura del horno:"
         opcBeltSpeed     = "Velocidad de la Correa: "
         opcDryTime       = "Tiempo seco:"
         opcItemList      = "Lista de articulos "
         opcItemID        = "Identificaci�n del art�culo"
         opcItemName      = "Nombre del �rticulo "
         opcMoldCount     = "Cantidad de moldes "
         opcGeneralNotes  = "Notas generales"
         opcPreAgitate    = "Pre-agitar: "
         opcUpAgitate     = "Agitar: "
         opcDownAgitate   = "Agitar hacia abajo: "
         opcOvenTemp1     = "Temperatura del horno 1: "
         opcOvenTemp2     = "Temperatura del horno 2: "
         opcOvenTemp3     = "Temperatura del horno 3: "
         .
    END.
    ELSE DO:
       ASSIGN
         opcJobLabel      = "Head ID: "
         opcMachineLabel  = "Machine: "
         opcCycles        = "Cycles: "
         opcFurnish       = "Furnish: "
         opcConsistency   = "Consistency: "
         opcMoldTime      = "Mold Time: "
         opcAgitation     = "Agitation: "
         opcDelay         = "Delay: "
         opcOverTemp      = "Over Temp:"
         opcBeltSpeed     = "Belt Speed: "
         opcDryTime       = "Dry Time:"
         opcItemList      = " Item List "
         opcItemID        = "Item ID"
         opcItemName      = "Item Name"
         opcMoldCount     = "Mold Count"
         opcGeneralNotes  = "General Notes"
         opcPreAgitate    = "Pre-Agitate: " /* 13 */
         opcUpAgitate     = "Up-Agitate: "   /* 12 */
         opcDownAgitate   = "Down-Agitate: "  /* 14 */
         opcOvenTemp1     = "Oven Temp 1: "  /* 13 */
         opcOvenTemp2     = "Oven Temp 2: "  /* 13 */
         opcOvenTemp3     = "Oven Temp 3: "  /* 13 */
         . 
    
    END.
END PROCEDURE.

PROCEDURE pGetPrintLabel2:
  DEFINE INPUT PARAMETER iplSpanish AS LOGICAL NO-UNDO.
  DEFINE OUTPUT PARAMETER opcItemSpecLabel AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcFGItemLabel AS CHARACTER NO-UNDO.      
  DEFINE OUTPUT PARAMETER opcKeyItemLabel AS CHARACTER NO-UNDO. 
  DEFINE OUTPUT PARAMETER opcMoldsLabel AS CHARACTER NO-UNDO.      
  DEFINE OUTPUT PARAMETER opcWetWeightLabel AS CHARACTER NO-UNDO.   
  DEFINE OUTPUT PARAMETER opcFirstDryLabel AS CHARACTER NO-UNDO.   
  DEFINE OUTPUT PARAMETER opcDscrLabel AS CHARACTER NO-UNDO.     
  DEFINE OUTPUT PARAMETER opcMoldIDsLabel AS CHARACTER NO-UNDO.    
  DEFINE OUTPUT PARAMETER opcBoneDryLabel  AS CHARACTER NO-UNDO.    
  DEFINE OUTPUT PARAMETER opMoistureLabel AS CHARACTER NO-UNDO.    
  DEFINE OUTPUT PARAMETER opcSizeLabel AS CHARACTER NO-UNDO.       
  DEFINE OUTPUT PARAMETER opcJigAvailableLabel AS CHARACTER NO-UNDO.    
  DEFINE OUTPUT PARAMETER opcMinWeightLabel AS CHARACTER NO-UNDO.       
  DEFINE OUTPUT PARAMETER opcFiberContentLabel AS CHARACTER NO-UNDO.    
  DEFINE OUTPUT PARAMETER opcPackingLabel AS CHARACTER NO-UNDO.        
  DEFINE OUTPUT PARAMETER opcPalletCountLabel AS CHARACTER NO-UNDO.     
  DEFINE OUTPUT PARAMETER opcPalletSizeLabel AS CHARACTER NO-UNDO.      
  DEFINE OUTPUT PARAMETER opcCartonCodeLabel AS CHARACTER NO-UNDO.      
  DEFINE OUTPUT PARAMETER opcPalletLabel AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcInstructionsLabel AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcTotalCount AS CHARACTER NO-UNDO.
  DEFINE OUTPUT PARAMETER opcExpectedPallets AS CHARACTER NO-UNDO.
  
      
    IF iplSpanish THEN DO:
     ASSIGN
      opcItemSpecLabel     = "Espec�fico del art�culo" /* 23*/
      opcFGItemLabel       = "Articulo: "    /* 10*/
      opcKeyItemLabel      = "Art�culo clave: "   /* 15*/
      opcMoldsLabel        = "Moldes: "     /*8*/
      opcWetWeightLabel    = "Peso Mojado: "   /*13*/
      opcFirstDryLabel     = "Primero seco: " /* 14*/
      opcDscrLabel         = "Descripci�n: "  /*13*/
      opcMoldIDsLabel      = "Identificaci�n del molde:"  /*25*/
      opcBoneDryLabel      = "Peso seco:"  /*10*/
      opMoistureLabel      = "Humedad:" /*8*/
      opcSizeLabel         = "Talla: "  /*7*/
      opcJigAvailableLabel = "Plantilla disponible:"  /*21*/
      opcMinWeightLabel    = "Peso m�nimo:"    /*12*/
      opcFiberContentLabel = "Contenido de fibra: " /*20*/
      opcPackingLabel      = "Embalaje"  /*8*/
      opcPalletCountLabel  = "Cantidad en la paleta: " /*23*/
      opcPalletSizeLabel   = "Tama�o de la paleta: "  /*21*/
      opcCartonCodeLabel   = "C�digo de cart�n:"  /*18*/
      opcPalletLabel       = "Paleta:"     /*7*/
      opcInstructionsLabel = "Instrucciones"  /*13*/
      opcTotalCount        = "Cuenta total:"  /*14*/
      opcExpectedPallets   = "# Palets esperados:"  /*20*/
      .
    END.
    ELSE DO:
      ASSIGN
      opcItemSpecLabel     =  "ITEM SPECIFICATIONS"
      opcFGItemLabel       =  "Item: "
      opcKeyItemLabel      =  "Key Item: "
      opcMoldsLabel        =  "Molds: "
      opcWetWeightLabel    =  "Wet Weight: "
      opcFirstDryLabel     =  "First Dry: "
      opcDscrLabel         =  "Dscr: "
      opcMoldIDsLabel      =  "Mold IDs:"
      opcBoneDryLabel      =  "Bone Dry:"
      opMoistureLabel      =  "Moisture:"
      opcSizeLabel         =  "Size: "
      opcJigAvailableLabel =  "Jig Available:"
      opcMinWeightLabel    =  "Min Weight:"
      opcFiberContentLabel =  "Fiber Content:"
      opcPackingLabel      =  "Packing"
      opcPalletCountLabel  =  "Pallet Count: "
      opcPalletSizeLabel   =  "Pallet Size: "
      opcCartonCodeLabel   =  "Carton Code: "
      opcPalletLabel       =  "Pallet #: "
      opcInstructionsLabel =  "Instructions"
      opcTotalCount        =  "Total Count:"
      opcExpectedPallets   =  "Expected # Pallets:"
      .
    
    END.
END PROCEDURE.
    
IF v-format EQ "Fibre" THEN PAGE.



/* ************************  Function Implementations ***************** */

FUNCTION fGetKeyItemEstimateRecKey RETURNS CHARACTER PRIVATE
	(ipcCompany AS CHARACTER, ipcJobID AS CHARACTER, ipcJobID2 AS INTEGER) :
/*------------------------------------------------------------------------------
 Purpose:  Given a job, return the rec_key for the key item's estimate
 Notes:
------------------------------------------------------------------------------*/	
    DEFINE VARIABLE cKeyItemEstimateRecKey AS CHARACTER NO-UNDO.
    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-itemfg FOR itemfg.
    DEFINE BUFFER bf-est FOR est.
    
    FIND FIRST bf-job-hdr NO-LOCK
        WHERE bf-job-hdr.company EQ ipcCompany
        AND bf-job-hdr.job-no EQ ipcJobID
        AND bf-job-hdr.job-no2 EQ ipcJobID2
        AND bf-job-hdr.keyItem EQ YES
        NO-ERROR.
    IF NOT AVAILABLE bf-job-hdr THEN 
        FIND FIRST bf-job-hdr NO-LOCK 
            WHERE bf-job-hdr.company EQ ipcCompany
            AND bf-job-hdr.job-no EQ ipcJobID
            AND bf-job-hdr.job-no2 EQ ipcJobID2
            NO-ERROR.     
    IF AVAILABLE bf-job-hdr THEN 
        FIND FIRST bf-itemfg NO-LOCK 
            WHERE bf-itemfg.company EQ bf-job-hdr.company
            AND bf-itemfg.i-no EQ bf-job-hdr.i-no
            NO-ERROR.
    IF AVAILABLE bf-itemfg AND bf-itemfg.est-no NE "" THEN 
        FIND FIRST bf-est NO-LOCK 
            WHERE bf-est.company EQ ipcCompany
            AND bf-est.est-no EQ bf-itemfg.est-no
            NO-ERROR.
    IF AVAILABLE est THEN 
        cKeyItemEstimateRecKey = bf-est.rec_key.
     
    RETURN cKeyItemEstimateRecKey.
    	
END FUNCTION.

FUNCTION fGetMiscFields RETURNS CHARACTER
  (iRecKey AS CHARACTER,iId AS CHARACTER) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
    
    FIND FIRST mfvalues NO-LOCK
        WHERE mfvalues.rec_key EQ iRecKey
        AND mfvalues.mf_id EQ iId
        NO-ERROR.
    IF AVAILABLE mfvalues 
        THEN
            RETURN  mfvalues.mf_value.
        ELSE 
            RETURN "".
   
END FUNCTION.

FUNCTION fHasMiscFields RETURNS LOGICAL PRIVATE
	(ipcRecKey AS CHARACTER):
/*------------------------------------------------------------------------------
 Purpose:  Given a rec_key - determine if there are misc fields assigned to it
 Notes:
------------------------------------------------------------------------------*/	

    RETURN CAN-FIND(FIRST mfvalues WHERE mfvalues.rec_key EQ ipcRecKey).

		
END FUNCTION.

/* end ---------------------------------- copr. 1994  advanced software, inc. */

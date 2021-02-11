/* copy of cerep/jobcarded.p   Xprint FC Factory  Ticket for Mclean */
/*------------------------------------------------------------------------
    File        : cerep/jobmclean.p
    Purpose     : 

    Syntax      :

    Description : print folding job ticket  

    Author(s)   : Sewa Singh 
    Created     : fri aug 9 19:29:35 EST 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE INPUT PARAMETER v-format LIKE sys-ctrl.char-fld.
DEFINE STREAM st-st.

DEFINE VARIABLE v-dir AS CHARACTER FORMAT "X(80)" NO-UNDO.

FIND FIRST users WHERE
    users.user_id EQ USERID("NOSWEAT")
    NO-LOCK NO-ERROR.

IF AVAILABLE users AND users.user_program[2] NE "" THEN
    v-dir = users.user_program[2] + "\".
ELSE
    v-dir = "c:\tmp\".



{sys/inc/var.i shared}
{sys/form/s-top.f}
{jcrep/r-ticket.i "shared"}
{cecrep/jc-soule.i}
DEFINE NEW SHARED VARIABLE save_id       AS RECID.
DEFINE NEW SHARED VARIABLE v-today       AS DATE      INIT TODAY.
DEFINE NEW SHARED VARIABLE v-job         AS CHARACTER FORMAT "x(6)" EXTENT 2 INIT [" ","zzzzzz"].
DEFINE NEW SHARED VARIABLE v-job2        AS INTEGER   FORMAT "99" EXTENT 2 INIT [00,99].
DEFINE NEW SHARED VARIABLE v-stypart     LIKE style.dscr.
DEFINE NEW SHARED VARIABLE v-dsc         LIKE oe-ordl.part-dscr1 EXTENT 2.
DEFINE NEW SHARED VARIABLE v-size        AS CHARACTER FORMAT "x(26)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-bld-job     LIKE oe-ord.job-no.
DEFINE NEW SHARED VARIABLE v-bld-job2    LIKE oe-ord.job-no2.
DEFINE NEW SHARED VARIABLE v-fill        AS CHARACTER FORMAT "x(100)".
DEFINE NEW SHARED VARIABLE v-frst        AS LOG.
DEFINE NEW SHARED VARIABLE v-ok          AS LOG.
DEFINE NEW SHARED VARIABLE v-est-qty     AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-job-qty     AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-fac         AS DECIMAL .
DEFINE NEW SHARED VARIABLE v-job-no      LIKE oe-ordl.job-no.
DEFINE NEW SHARED VARIABLE v-job-no2     LIKE oe-ordl.job-no2.
DEFINE NEW SHARED VARIABLE v-due-date    LIKE oe-ord.due-date.
DEFINE NEW SHARED VARIABLE v-reprint     AS LOG.
DEFINE NEW SHARED VARIABLE v-up          LIKE eb.num-up.
DEFINE NEW SHARED VARIABLE v-tandem      AS LOG.
DEFINE NEW SHARED VARIABLE v-form-no     LIKE eb.form-no.
DEFINE NEW SHARED VARIABLE v-fup         AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-layout      AS CHARACTER FORMAT "x(30)".

DEFINE            VARIABLE v-line        AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE cnt           AS INTEGER   INIT 1 NO-UNDO.
DEFINE            VARIABLE v-first       AS LOG       NO-UNDO.
DEFINE            VARIABLE v-spec-list   AS CHARACTER FORMAT "x(20)"INIT "QA" NO-UNDO.
DEFINE            VARIABLE lv-form-note  AS cha       NO-UNDO.
DEFINE            VARIABLE v-itm-printed AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-alloc       AS cha       NO-UNDO.
DEFINE            VARIABLE v-skip        AS LOG       NO-UNDO.
DEFINE            VARIABLE v-fill2       AS cha       INIT "-" FORM "x(100)" NO-UNDO.
DEFINE            VARIABLE v-fill3       AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE            VARIABLE li            AS INTEGER   NO-UNDO.

DEFINE TEMP-TABLE w-lo NO-UNDO
    FIELD layout LIKE v-layout.

DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.
DEFINE VARIABLE v-dept-note AS cha FORM "x(48)" EXTENT 50 NO-UNDO.
DEFINE BUFFER b-eb       FOR eb.
DEFINE BUFFER b-ef       FOR ef.
DEFINE BUFFER bf-item    FOR ITEM.
DEFINE BUFFER bx-job-hdr FOR job-hdr.
DEFINE BUFFER bff-eb FOR eb .
DEFINE VARIABLE v-ord-no AS INTEGER NO-UNDO.

DEFINE NEW SHARED WORKFILE wrk-op
    FIELD m-dscr LIKE est-op.m-dscr
    FIELD m-code LIKE est-op.m-code
    FIELD i-line LIKE job-mch.LINE
    FIELD dept LIKE est-op.dept
    FIELD b-num LIKE est-op.b-num
    FIELD s-num LIKE est-op.s-num
    FIELD pass LIKE est-op.op-pass
    FIELD mr LIKE est-op.op-mr EXTENT 100
    FIELD speed LIKE est-op.op-speed EXTENT 100
    FIELD run-hr LIKE job-mch.run-hr EXTENT 100
    FIELD num-sh LIKE est-op.num-sh EXTENT 100
    FIELD spoil LIKE job-mch.wst-prct EXTENT 100
    FIELD mr-waste LIKE job-mch.mr-waste EXTENT 100 
    FIELD waste-per LIKE job-mch.wst-prct EXTENT 100 
    FIELD iRecSeq AS INTEGER  
    FIELD cMachType AS CHARACTER  .
DEFINE BUFFER bf-wrk-op FOR wrk-op .
DEFINE BUFFER bff-wrk-op FOR wrk-op .

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
    FIELD i-pass AS DECIMAL
    FIELD i-seq AS INTEGER  .

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

DEFINE SHARED VARIABLE s-prt-set-header AS LOG       NO-UNDO.  
DEFINE SHARED VARIABLE s-prt-label      AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE lIncludeLastPage AS LOGICAL   NO-UNDO .
DEFINE SHARED VARIABLE cRdOptionMclean  AS CHARACTER NO-UNDO .
DEFINE SHARED VARIABLE cJobType AS CHARACTER NO-UNDO .
DEFINE SHARED VARIABLE lFSC AS LOGICAL NO-UNDO .
    
{custom/notesdef.i}
DEFINE VARIABLE v-inst2          AS cha  EXTENT 70 NO-UNDO.    
DEFINE VARIABLE v-start-date     AS DATE NO-UNDO.
DEFINE VARIABLE v-req-date       AS DATE NO-UNDO.
DEFINE VARIABLE v-shipto         AS cha  FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-case-size      AS cha  NO-UNDO.
DEFINE VARIABLE cVendor          LIKE po-ord.vend-no NO-UNDO.
DEFINE VARIABLE v-po-no          LIKE oe-ordl.po-no NO-UNDO.
DEFINE VARIABLE lv-mat-dept-list AS cha  INIT "FB,FS,WN,WS,GL" NO-UNDO.
DEFINE VARIABLE v-mat-for-mach   AS cha  NO-UNDO.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE VARIABLE v-fgdsc LIKE eb.part-dscr1 EXTENT 30 NO-UNDO.
DEFINE TEMP-TABLE tt-fgitm NO-UNDO 
    FIELD i-no      AS cha     FORM "x(15)"
    FIELD seq       AS INTEGER
    FIELD qty       AS INTEGER 
    FIELD i-dscr    AS cha
    FIELD po-no     AS cha
    FIELD ord-no    AS INTEGER
    FIELD cust-name AS cha
    FIELD shipto    AS cha     EXTENT 4.
DEFINE VARIABLE iBoardPO LIKE oe-ordl.po-no-po NO-UNDO.

DEFINE BUFFER xoe-ordl  FOR oe-ordl.
DEFINE BUFFER bf-fg-bin FOR fg-bin.
DEFINE VARIABLE v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-last-j    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-po-no2    LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-po-no3    LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-spc-no    AS cha     FORM "x(15)" NO-UNDO.
DEFINE VARIABLE v-ord-qty   AS INTEGER NO-UNDO.
DEFINE VARIABLE v-stock-no  LIKE eb.stock-no NO-UNDO.
DEFINE VARIABLE v-have-note AS LOG     NO-UNDO.
DEFINE VARIABLE v-shipvia   LIKE carrier.dscr NO-UNDO.
DEFINE TEMP-TABLE tt-size NO-UNDO 
    FIELD frm       LIKE job-hdr.frm
    FIELD blank-no  LIKE eb.blank-no
    FIELD cad#      LIKE eb.cad-no FORM "x(10)"
    FIELD cad-size  AS cha     FORM "x(25)"
    FIELD COUNT     LIKE eb.cas-cnt
    FIELD vend-part AS cha     FORM "x(30)"
    FIELD seq       AS INTEGER
    INDEX tt-size frm seq.
DEFINE VARIABLE v-tt-seq    AS INTEGER NO-UNDO.
DEFINE VARIABLE v-item-name LIKE itemfg.i-name NO-UNDO.
DEFINE VARIABLE v-cat       AS cha     NO-UNDO.
{custom/formtext.i NEW}
DEFINE VARIABLE lv-text     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-note-cnt AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-cas-no    AS cha       NO-UNDO.
DEFINE BUFFER bf-jobmat FOR job-mat.
ASSIGN
    v-fill  = "<||3><C1><FROM><C83><LINE><||3>"
    v-fill2 = "<||3><C5><FROM><C83><LINE><||3>"
    v-fill3 = "<C2><FROM><C83><LINE><||3><R-1>".

DEFINE NEW SHARED FRAME head.

DEFINE SHARED VARIABLE s-prt-mstandard     AS LOG     NO-UNDO.
DEFINE SHARED VARIABLE s-prt-shipto        AS LOG     NO-UNDO.
DEFINE SHARED VARIABLE s-prt-sellprc       AS LOG     NO-UNDO.
DEFINE        VARIABLE dtPoDueDate         LIKE po-ordl.due-date NO-UNDO.
DEFINE        VARIABLE v-upc-lbl           AS cha     FORM "x(10)" NO-UNDO.
DEFINE SHARED VARIABLE s-run-speed         AS LOG     NO-UNDO.
DEFINE        VARIABLE v-ink-seq           AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-ink-list          AS cha     NO-UNDO.
DEFINE        VARIABLE v-ink-use-per-blank AS INTEGER NO-UNDO.  
DEFINE        VARIABLE vs-len              AS cha     NO-UNDO.
DEFINE        VARIABLE vs-wid              AS cha     NO-UNDO.
DEFINE        VARIABLE vs-dep              AS cha     NO-UNDO.
DEFINE BUFFER bf-ink    FOR wrk-ink.
DEFINE BUFFER bf-jobhdr FOR job-hdr.
DEFIN VARIABLE iJobQty AS INTEGER NO-UNDO.
DEFINE VARIABLE v-job-cnt  AS INTEGER NO-UNDO.
DEFINE VARIABLE v-prev-job AS cha     NO-UNDO.

DEFINE TEMP-TABLE tt-ink NO-UNDO
    FIELD i-code LIKE wrk-ink.i-code
    FIELD i-seq  LIKE wrk-ink.i-seq.

DEFINE TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.

DEFINE        VARIABLE lv-cad-image      AS cha       NO-UNDO.
DEFINE        VARIABLE lv-cad-image-list AS cha       NO-UNDO.
DEFINE        VARIABLE v-case-due-date   AS DATE      NO-UNDO.
DEFINE        VARIABLE v-hg              AS cha       NO-UNDO.
DEFINE        VARIABLE v-net-size        AS cha       NO-UNDO.
DEFINE        VARIABLE cNewOrderValue    AS CHARACTER NO-UNDO .
DEFINE        VARIABLE cLabelSetItem     AS CHARACTER NO-UNDO .

DEFINE        VARIABLE cLabelSetPart     AS CHARACTER NO-UNDO .
DEFINE        VARIABLE cSetItemName      AS CHARACTER NO-UNDO .
DEFINE        VARIABLE cSetPartNo        AS CHARACTER NO-UNDO .
DEFINE        VARIABLE iOrderNo          AS INTEGER   NO-UNDO .
DEFINE        VARIABLE cRelStat          AS CHARACTER NO-UNDO .
DEFINE        VARIABLE dtRelDate         AS DATE      NO-UNDO .
DEFINE        VARIABLE cImageBoxDesign   AS CHARACTER NO-UNDO .
DEFINE        VARIABLE clsFGitemImg      AS CHARACTER NO-UNDO .
DEFINE        VARIABLE dBoardSheet       AS DECIMAL   NO-UNDO .
DEFINE        VARIABLE iSetRelQty        AS INTEGER   EXTENT 10 NO-UNDO .
DEFINE        VARIABLE cRelDate          AS CHARACTER EXTENT 10 NO-UNDO .
DEFINE        VARIABLE dMRWaste          AS DECIMAL   NO-UNDO .
DEFINE        VARIABLE dRunWaste         AS DECIMAL   NO-UNDO .
DEFINE        VARIABLE dMRCrew           AS DECIMAL   NO-UNDO .
DEFINE        VARIABLE dRunCrew          AS DECIMAL   NO-UNDO .
DEFINE        VARIABLE dBeginQty         AS INTEGER   NO-UNDO .
DEFINE VARIABLE iYieldQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iDisYieldQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iEbTotalYldQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iEbTotalblQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iEbTotalUpQty AS INTEGER NO-UNDO .
DEFINE SHARED VARIABLE s-prt-fgimage     AS LOG       NO-UNDO.
DEFINE BUFFER bf-ttSoule FOR ttSoule .
DEFINE VARIABLE lv-pg-num AS INT NO-UNDO.
DEFINE VARIABLE lAssembled AS LOGICAL NO-UNDO . 
DEFINE VARIABLE cSetFGItem AS CHARACTER NO-UNDO . 
DEFINE VARIABLE dPerSetQty AS DECIMAL NO-UNDO .
DEFINE VARIABLE iEbTotalOverQty AS INTEGER NO-UNDO .
DEFINE VARIABLE cCaseItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCaseSize AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCaseCount AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCasePerPallet AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintSetHeader AS LOGICAL NO-UNDO.

cNewOrderValue = CAPS(cJobType) .



FORMAT "  Customer:" oe-ord.cust-name "Sold To:" oe-ord.sold-id
    "Salesman:" AT 90 oe-ord.sname[1] "Order#:" AT 138 oe-ord.ord-no
    WITH NO-BOX FRAME line-head NO-LABELS STREAM-IO WIDTH 162.
    
{sys/inc/notes.i}

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
    v-job[1]    = fjob-no
    v-job[2]    = tjob-no
    v-job2[1]   = fjob-no2
    v-job2[2]   = tjob-no2
    v-reprint   = reprint
    v-spec-list = spec-list.

/* build tt-reftable */
FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company               EQ cocode
    AND job-hdr.job-no                GE substr(fjob-no,1,6)
    AND job-hdr.job-no                LE substr(tjob-no,1,6)
    AND fill(" ",6 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"99")  GE fjob-no
    AND fill(" ",6 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"99")  LE tjob-no
    AND (production OR
    job-hdr.ftick-prnt           EQ v-reprint OR
    PROGRAM-NAME(2) MATCHES "*r-tickt2*")
    AND CAN-FIND(FIRST job WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    AND job.stat    NE "H"
    AND (job.pr-printed EQ reprint OR
    NOT production))
    USE-INDEX job-no,

    FIRST est
    WHERE est.company  EQ job-hdr.company
    AND est.est-no   EQ job-hdr.est-no
    AND est.est-type LE 4  
    NO-LOCK

    BREAK BY job-hdr.job
    BY job-hdr.job-no
    BY job-hdr.job-no2
    BY job-hdr.frm:

/*    FOR EACH reftable NO-LOCK WHERE reftable.reftable EQ "jc/jc-calc.p"*/
/*        AND reftable.company  EQ job-hdr.company                       */
/*        AND reftable.loc      EQ ""                                    */
/*        AND reftable.code     EQ STRING(job-hdr.job,"999999999"):      */
/*        CREATE tt-reftable.                                            */
/*        BUFFER-COPY reftable TO tt-reftable.                           */
/*        tt-reftable.est-type = est.est-type.                           */
/*    END.                                                               */
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
            tt-reftable.val[12]  = job-hdr.frm
            tt-reftable.val[13]  = job-hdr.blank-no
            tt-reftable.est-type = est.est-type.
    END.

    IF est.est-type EQ 1 THEN DO:
            CREATE ttSoule .
                ASSIGN
                ttSoule.job-no = job-hdr.job-no
                ttSoule.job-no2 = job-hdr.job-no2
                ttSoule.frm = job-hdr.frm
                ttSoule.blank-no = job-hdr.blank-no
                ttSoule.qty = job-hdr.qty
                ttSoule.i-no = job-hdr.i-no
                ttSoule.runForm = YES.
    END.

END.
/* end of building tt-reftable */

FOR EACH job-hdr NO-LOCK
    WHERE job-hdr.company               EQ cocode
    AND job-hdr.job-no                GE substr(fjob-no,1,6)
    AND job-hdr.job-no                LE substr(tjob-no,1,6)
    AND fill(" ",6 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"99")  GE fjob-no
    AND fill(" ",6 - length(TRIM(job-hdr.job-no))) +
    trim(job-hdr.job-no) +
    string(job-hdr.job-no2,"99")  LE tjob-no
    AND (production OR
    job-hdr.ftick-prnt           EQ v-reprint OR
    PROGRAM-NAME(2) MATCHES "*r-tickt2*")
    AND CAN-FIND(FIRST job WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    AND job.stat    NE "H"
    AND (job.pr-printed EQ reprint OR
    NOT production))
    USE-INDEX job-no,

    FIRST est
    WHERE est.company  EQ job-hdr.company
    AND est.est-no   EQ job-hdr.est-no
    AND est.est-type LE 4  
    NO-LOCK

    BREAK BY job-hdr.job
    BY job-hdr.job-no
    BY job-hdr.job-no2
    BY job-hdr.frm:
    
    IF FIRST-OF(job-hdr.job) THEN
    lPrintSetHeader = TRUE .

    FIND FIRST job
        WHERE job.company EQ cocode
        AND job.job     EQ job-hdr.job
        AND job.job-no  EQ job-hdr.job-no
        AND job.job-no2 EQ job-hdr.job-no2
        NO-LOCK NO-ERROR.

    IF production AND
        job.cs-trans-date NE ? THEN 
    DO:
        li = 0.
        DO WHILE li LT 1000:
            li = li + 1.
            FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE job THEN
                ASSIGN
                    job.pr-printed    = YES
                    job.pr-user-id-p  = USERID("nosweat")
                    job.pr-print-date = TODAY
                    job.pr-print-time = TIME
                    li                = 1000.
        END.
    END.

    ELSE 
    DO:
        li = 0.
        IF NOT job-hdr.ftick-prnt THEN 
        DO WHILE li LT 1000:
            li = li + 1.
            FIND xjob-hdr EXCLUSIVE-LOCK
                WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
                NO-ERROR NO-WAIT.
            IF AVAILABLE xjob-hdr THEN 
            DO:
                ASSIGN
                    xjob-hdr.ftick-prnt = YES
                    li                  = 1000.
                FIND xjob-hdr NO-LOCK
                    WHERE ROWID(xjob-hdr) EQ ROWID(job-hdr)
                    NO-ERROR NO-WAIT.

            END.
        END.

        li = 0.
        DO WHILE li LT 1000:
            li = li + 1.
            FIND CURRENT job EXCLUSIVE-LOCK NO-ERROR NO-WAIT.
            IF AVAILABLE job THEN 
            DO:
                li = 1000.

                IF NOT job.cs-printed THEN
                    ASSIGN
                        job.cs-printed    = YES
                        job.cs-user-id-p  = USERID("nosweat")
                        job.cs-print-date = TODAY
                        job.cs-print-time = TIME.

                IF approve THEN
                    ASSIGN
                        job.cs-to-pr      = YES
                        job.cs-user-id-t  = USERID("nosweat")
                        job.cs-trans-date = TODAY
                        job.cs-trans-time = TIME.
            END.
        END.
    END.

    FIND CURRENT job NO-LOCK NO-ERROR.
      
    v-est-qty = IF AVAILABLE est THEN est.est-qty[1] ELSE 0.
    FIND FIRST oe-ord WHERE oe-ord.company EQ job-hdr.company
        AND oe-ord.ord-no  EQ job-hdr.ord-no NO-LOCK NO-ERROR.
    IF FIRST-OF(job-hdr.job-no2) THEN v-first = YES.
    /** PRINT JOB HEADER **/
    IF v-first THEN 
    DO:   
        ASSIGN
            v-job-no  = job-hdr.job-no
            v-job-no2 = job-hdr.job-no2.

        IF AVAILABLE oe-ord AND NOT oe-ctrl.p-fact AND (oe-ord.stat EQ "H" OR oe-ord.priceHold) THEN NEXT.
        FIND FIRST cust WHERE cust.company = job-hdr.company AND
            cust.cust-no = job-hdr.cust-no NO-LOCK NO-ERROR.
        dtRelDate = ? .
        FIND FIRST oe-rel NO-LOCK
            WHERE oe-rel.company EQ cocode
            AND oe-rel.ord-no  EQ oe-ord.ord-no
            AND oe-rel.link-no EQ 0
            AND oe-rel.i-no    EQ job-hdr.i-no
            /*and oe-rel.line    eq oe-ordl.line*/ NO-ERROR.
           
        IF AVAILABLE oe-rel THEN 
        DO:

            {oe/rel-stat.i cRelStat}
            IF AVAILABLE oe-rell THEN
                FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
           /* dtRelDate = IF AVAILABLE oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date. */
               dtRelDate = oe-ord.due-date .

        END.
        ELSE DO: 
               dtRelDate = job-hdr.due-date.
        END.
        
        ASSIGN
            cLabelSetItem = "Ident:" 
            cLabelSetPart = "Cust Style:" 
            v-due-date    = IF AVAILABLE oe-ord THEN oe-ord.due-date ELSE ?
            v-start-date  = IF AVAILABLE oe-ord THEN oe-ord.ord-date ELSE job-hdr.start-date.
        v-cust-name = IF AVAILABLE oe-ord THEN oe-ord.cust-name 
        ELSE IF AVAILABLE cust THEN cust.name
        ELSE job-hdr.cust-no.
        iOrderNo = IF AVAILABLE oe-ord THEN oe-ord.ord-no ELSE 0 .
        IF est.est-type EQ 2 THEN 
        DO:
            ASSIGN
                cLabelSetItem = "Set FG Item #: " 
                cLabelSetPart = "Set Customer Part: " .
            FIND FIRST b-eb NO-LOCK
                WHERE b-eb.company EQ est.company
                AND b-eb.est-no EQ est.est-no 
                AND b-eb.form-no EQ 0 NO-ERROR .
            IF AVAILABLE b-eb THEN
                ASSIGN 
                    cSetItemName = b-eb.stock-no
                    cSetPartNo   = b-eb.part-no .
        END.
        ELSE do:
            ASSIGN
                cLabelSetItem = "" 
                cLabelSetPart = "" 
                cSetItemName = ""
                cSetPartNo   = "" .
        END.

        IF NOT FIRST(job-hdr.job-no) THEN do:
             PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
             PAGE.
        END.
        ELSE PUT SKIP
                "<FCalibri><B><C2>MCLEAN PACKAGING INC. </B><P10>"
                "<C30><B><P11>Factory Order: <P11>" (IF cRdOptionMclean EQ "M" THEN "Moorestown" ELSE "Nazareth") FORMAT "x(12)"  "<C65>Report Date: " TODAY "</B><P10>" SKIP
                v-fill SKIP.

        PUT "<C2><B>Customer:<P10>" v-cust-name  "<P10>"
            "<B><C39>Delivery Date: " ( IF dtRelDate NE ? THEN STRING(dtRelDate) ELSE "")  "<C68>Order # :" TRIM(STRING(iOrderNo,">>>>>>9")) FORMAT "x(8)" SKIP
            "<C2>" cLabelSetItem FORMAT "x(14)" cSetItemName FORMAT "x(15)"  .
       
        PUT "<C68>Job #: </B>" v-job-no SPACE(0) "-" SPACE(0) v-job-no2 FORMAT "99" SKIP
            "<C2><b>" cLabelSetPart FORMAT "x(18)" cSetPartNo FORMAT "x(15)" SKIP
            v-fill SKIP .
        PUT "<R4.8><P20><C24>" IF lFSC THEN "FSC" ELSE "" "<C40>" cNewOrderValue FORMAT "x(13)" "<P10><R6></b>" SKIP  .

        /*view frame head.*/

        /* print all customeres if it's more than one*/
        FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
            AND bf-jobhdr.job-no = job-hdr.job-no
            AND bf-jobhdr.job-no2 = job-hdr.job-no2
            BREAK BY bf-jobhdr.ord-no:
            IF FIRST-OF(bf-jobhdr.ord-no) THEN 
            DO:
    
                v-shipto = "".
                FIND FIRST oe-ordl
                    WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
                    AND oe-ordl.job-no  EQ job-hdr.job-no
                    AND oe-ordl.job-no2 EQ job-hdr.job-no2
                    AND oe-ordl.i-no    EQ bf-jobhdr.i-no
                    NO-LOCK NO-ERROR.

                IF bf-jobhdr.ord-no NE 0 AND NOT AVAILABLE oe-ordl THEN
                    FIND FIRST oe-ordl
                        WHERE oe-ordl.company EQ job-hdr.company
                        AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
                        AND oe-ordl.i-no    EQ bf-jobhdr.i-no
                        NO-ERROR.

                IF bf-jobhdr.ord-no NE 0 AND NOT AVAILABLE oe-ordl THEN
                    FIND FIRST oe-ordl
                        WHERE oe-ordl.company EQ job-hdr.company
                        AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
                        NO-ERROR.

                IF AVAILABLE oe-ordl THEN
                    FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
                        AND oe-rel.ord-no  EQ oe-ordl.ord-no
                        AND oe-rel.i-no    EQ oe-ordl.i-no
                        AND oe-rel.line    EQ oe-ordl.line
                        NO-LOCK NO-ERROR.
                IF AVAILABLE oe-rel THEN 
                DO:
                    FIND FIRST shipto WHERE shipto.company EQ cocode
                        AND shipto.cust-no EQ oe-rel.cust-no
                        AND shipto.ship-id EQ oe-rel.ship-id NO-LOCK NO-ERROR.  
                    IF AVAILABLE shipto THEN
                        ASSIGN v-shipto[1] = shipto.ship-name
                            v-shipto[2] = shipto.ship-addr[1]
                            v-shipto[3] = shipto.ship-addr[2]
                            v-shipto[4] = TRIM(oe-rel.ship-city) + ", " +
                                   oe-rel.ship-state + "  " + oe-rel.ship-zip.          
                END.
       
                IF AVAILABLE oe-ordl THEN FIND FIRST oe-ord OF oe-ordl NO-LOCK .
                ASSIGN
                    v-req-date = IF AVAILABLE oe-ordl THEN oe-ordl.req-date ELSE ?.
                FIND FIRST cust WHERE cust.company = job-hdr.company AND
                    cust.cust-no = bf-jobhdr.cust-no NO-LOCK NO-ERROR.
                v-cust-name = IF AVAILABLE oe-ord THEN oe-ord.cust-name 
                ELSE IF AVAILABLE cust THEN cust.name
                ELSE bf-jobhdr.cust-no.
                FIND FIRST eb WHERE eb.company = est.company
                    AND eb.est-no = est.est-no
                    AND eb.form-no <> 0
                    AND eb.blank-no <> 0 NO-LOCK NO-ERROR.
                v-spc-no = IF AVAILABLE eb THEN eb.spc-no ELSE "".
                FIND FIRST oe-rel WHERE oe-rel.company EQ cocode
                    AND oe-rel.ord-no  EQ oe-ordl.ord-no
                    NO-LOCK NO-ERROR.
           
                FIND FIRST carrier WHERE carrier.company = oe-rel.company
                    AND carrier.carrier  EQ oe-rel.carrier NO-LOCK NO-ERROR.
                IF AVAILABLE carrier 
                    THEN ASSIGN v-shipvia = carrier.dscr.
                ELSE ASSIGN v-shipvia = "".
        
     
            END. /* first-of(bf-jobhdr.ord-no)*/
        END. /* for each bf-jobhdr*/

        v-line = IF AVAILABLE est                            AND
            est.est-type GT 2 AND est.est-type LT 5 THEN 500 ELSE 50.


        /* new */

        FOR EACH wrk-ink:
            DELETE wrk-ink.
        END.

        FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
            AND bf-jobhdr.job-no = job-hdr.job-no
            AND bf-jobhdr.job-no2 = job-hdr.job-no2
            BREAK BY bf-jobhdr.frm:
            IF FIRST-OF(bf-jobhdr.frm) THEN 
            DO:
           
            /*PUT "<||><R+0><C2><#5>" SKIP.    */
            /* IF AVAIL ttSoule AND ttSoule.runForm EQ YES THEN
             PUT "<||><R+0><C2><#5><FROM><R+1><C10><RECT>" SKIP.*/
            
            END.

            IF LAST-OF(bf-jobhdr.frm) THEN 
            DO:        
                MAIN-FORM:
                FOR  EACH tt-reftable NO-LOCK WHERE tt-reftable.reftable EQ "jc/jc-calc.p"
                    AND tt-reftable.company  EQ job-hdr.company
                    AND tt-reftable.loc      EQ ""
                    AND tt-reftable.code     EQ STRING(job-hdr.job,"999999999")
                    AND (tt-reftable.est-type <> 4 OR
                    tt-reftable.val[12] = bf-jobhdr.frm)
                    AND tt-reftable.spare-int-1 EQ 0
                    BREAK BY tt-reftable.val[12].
                    tt-reftable.spare-int-1 = 1.
                    IF est.est-type NE 1 AND NOT CAN-FIND(FIRST ttSoule NO-LOCK
                        WHERE  ttSoule.frm EQ tt-reftable.val[12]
                         AND   ttSoule.runForm EQ YES ) THEN NEXT MAIN-FORM.
                    
                    IF FIRST-OF(tt-reftable.val[12]) THEN 
                    DO:
        
                        /* build wrk-ink per form  BUILD INK WORK FILE **/
                        FOR EACH job-mat
                            WHERE job-mat.company EQ cocode
                            AND job-mat.job     EQ bf-jobhdr.job
                            AND job-mat.frm     EQ int(tt-reftable.val[12])
                            NO-LOCK,
                            FIRST item
                            {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no 
                AND lookup(item.mat-type,"I,V") > 0 no-lock:

                        FIND FIRST wrk-ink WHERE wrk-ink.i-code    EQ job-mat.i-no
                            NO-ERROR.                
                        IF NOT AVAILABLE wrk-ink THEN 
                        DO:
                            CREATE wrk-ink.
                            ASSIGN
                                wrk-ink.i-code   = job-mat.i-no
                                wrk-ink.form-no  = job-mat.frm
                                wrk-ink.blank-no = job-mat.blank-no
                                wrk-ink.i-dscr   = item.i-name /*est-dscr*/
                                wrk-ink.i-pass   = 1.
                        END.
                        wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.

                    END. /* JOB-MAT */
                    /* get color info for the board */
                    v-ink-seq = 0.            
                    FOR EACH wrk-ink WHERE wrk-ink.form-no = tt-reftable.val[12] /*bf-jobhdr.frm*/
                        BREAK BY wrk-ink.i-pass
                        BY wrk-ink.i-code
                        BY wrk-ink.blank-no :

                        IF FIRST-OF(wrk-ink.blank-no) THEN v-ink-use-per-blank = 0.
                        v-ink-use-per-blank = v-ink-use-per-blank + 1.
                        IF FIRST-OF(wrk-ink.i-code) THEN v-ink-seq = v-ink-seq + 1.
                        wrk-ink.i-seq = v-ink-seq.
                    END.
                    /* end of building wrk-ink*/

                    /** SUM UP NUMBER OF SHEETS **/
                    FIND FIRST job
                        WHERE job.company EQ cocode
                        AND job.job     EQ bf-jobhdr.job
                        AND job.job-no  EQ v-job-no
                        AND job.job-no2 EQ v-job-no2
                        NO-LOCK NO-ERROR.
                    j = 0 .
                    IF AVAILABLE job THEN
                        FOR EACH job-mch
                            WHERE job-mch.company EQ cocode
                            AND job-mch.job     EQ job.job
                            AND job-mch.job-no  EQ job.job-no
                            AND job-mch.job-no2 EQ job.job-no2
                            AND job-mch.frm = int(tt-reftable.val[12]) /*bf-jobhdr.frm*/
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
                            wrk-op.i-line  = job-mch.LINE
                            wrk-op.dept   = job-mch.dept
                            wrk-op.s-num  = job-mch.frm
                            wrk-op.b-num  = job-mch.blank-no
                            wrk-op.pass   = job-mch.pass
                            wrk-op.cMachType = mach.p-type 
                            wrk-op.iRecSeq = j 
                            j = j + 1 .
                    END.
                    ASSIGN
                        wrk-op.mr[job-mch.frm]       = job-mch.mr-hr
                        wrk-op.speed[job-mch.frm]    = job-mch.speed
                        wrk-op.num-sh[job-mch.frm]   = job-mch.run-qty
                        wrk-op.spoil[job-mch.frm]    = job-mch.wst-prct   
                        wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste   
                        wrk-op.run-hr[job-mch.frm]   = job-mch.run-hr 
                        wrk-op.waste-per[job-mch.frm] = job-mch.wst-prct  .
                END.

                /** BUILD PREP WORK FILE **/
                FOR EACH job-prep
                    WHERE job-prep.company EQ cocode
                    AND job-prep.job     EQ bf-jobhdr.job
                    AND job-prep.job-no  EQ bf-jobhdr.job-no
                    AND job-prep.job-no2 EQ bf-jobhdr.job-no2
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
                    WHERE ef.company EQ job-hdr.company
                    AND ef.est-no  EQ bf-jobhdr.est-no
                    AND ef.form-no = tt-reftable.val[12] /*bf-jobhdr.frm*/
                    BREAK BY ef.est-no BY ef.form-no:
     
                    v-est-qty = 0.
                    IF est.est-type EQ 4 THEN
                        FOR EACH eb FIELDS(yld-qty)
                            WHERE eb.company  EQ ef.company
                            AND eb.est-no   EQ ef.est-no
                            AND eb.stock-no EQ bf-jobhdr.i-no
                            NO-LOCK:
                            v-est-qty = v-est-qty + eb.yld-qty.
                        END.

                    ELSE v-fac = 1.
                    v-itm-printed = 0.
                    FOR EACH tt-ink:
                        DELETE tt-ink.
                    END.
                    IF ef.form-no EQ tt-reftable.val[12] THEN 
                        ebloop:
                        FOR EACH eb
                            WHERE eb.company     EQ ef.company
                            AND eb.est-no      EQ ef.est-no
                            AND eb.form-no     EQ ef.form-no
                            NO-LOCK,
                            FIRST ttSoule WHERE ttSoule.frm EQ eb.form-no 
                                AND ttSoule.blank-no EQ eb.blank-no 
                                AND ttSoule.runForm EQ YES NO-LOCK                            
                            BREAK BY eb.form-no BY eb.blank-no.
                            
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
                            FIND FIRST itemfg WHERE itemfg.company = eb.company
                                AND itemfg.i-no = eb.stock-no NO-LOCK NO-ERROR.
                            v-item-name = IF AVAILABLE itemfg THEN ITEMfg.i-name ELSE v-dsc[1].
                            IF eb.blank-no > 0 AND eb.blank-no < 11 THEN 
                                ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                            ASSIGN
                                v-upc-lbl  = IF s-prt-sellprc THEN "Sell Price" ELSE "   UPC#"
                                v-job-qty  = 0
                                v-stock-no = IF est.est-type >= 2 AND est.est-type <= 3 THEN bf-jobhdr.i-no ELSE eb.stock.
                            FOR EACH xjob-hdr WHERE xjob-hdr.company EQ cocode
                                AND xjob-hdr.job     EQ job-hdr.job
                                AND xjob-hdr.job-no  EQ job-hdr.job-no
                                AND xjob-hdr.job-no2 EQ job-hdr.job-no2
                                AND xjob-hdr.i-no    EQ v-stock-no NO-LOCK:
                                v-job-qty = v-job-qty + xjob-hdr.qty.
                            END.
                            /** PRINT ITEM **/
                            IF CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                                AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
                                AND oe-ordl.job-no  EQ job-hdr.job-no
                                AND oe-ordl.job-no2 EQ job-hdr.job-no2
                                AND oe-ordl.i-no    EQ v-stock-no) 
                                THEN v-ord-no = bf-jobhdr.ord-no.
                            ELSE IF CAN-FIND(FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                                    AND oe-ordl.job-no  EQ job-hdr.job-no
                                    AND oe-ordl.job-no2 EQ job-hdr.job-no2
                                    AND oe-ordl.i-no    EQ v-stock-no) THEN 
                                DO:
                                    FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                                        AND oe-ordl.job-no  EQ job-hdr.job-no
                                        AND oe-ordl.job-no2 EQ job-hdr.job-no2
                                        AND oe-ordl.i-no    EQ v-stock-no NO-LOCK NO-ERROR.
                                    v-ord-no = oe-ordl.ord-no.
                                END.
                                ELSE v-ord-no = bf-jobhdr.ord-no.

                            FIND FIRST oe-ordl
                                WHERE oe-ordl.company EQ job-hdr.company
                                AND oe-ordl.ord-no  EQ v-ord-no
                                AND oe-ordl.job-no  EQ job-hdr.job-no
                                AND oe-ordl.job-no2 EQ job-hdr.job-no2
                                AND oe-ordl.i-no    EQ v-stock-no /*job-hdr.i-no*/
                                NO-LOCK NO-ERROR.
                            IF bf-jobhdr.ord-no NE 0 AND NOT AVAILABLE oe-ordl THEN
                                FIND FIRST oe-ordl
                                    WHERE oe-ordl.company EQ job-hdr.company
                                    AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
                                    AND oe-ordl.i-no    EQ v-stock-no /*job-hdr.i-no*/
                                    NO-LOCK NO-ERROR.
                            IF bf-jobhdr.ord-no NE 0 AND NOT AVAILABLE oe-ordl THEN
                                FIND FIRST oe-ordl
                                    WHERE oe-ordl.company EQ job-hdr.company
                                    AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
                                    NO-LOCK NO-ERROR.
                            IF AVAILABLE oe-ordl THEN 
                            DO:
                                IF oe-ordl.i-no EQ v-stock-no THEN v-est-qty = oe-ordl.qty.
                                FIND FIRST oe-ord OF oe-ordl NO-LOCK.
                            END.
                            ELSE v-est-qty = v-job-qty.
            
                            RELEASE w-lo.
                            FIND FIRST w-lo NO-ERROR.
                            v-case-size = STRING(eb.cas-len) + "x" + string(eb.cas-wid) + "x" +
                                string(eb.cas-dep).

                            ASSIGN 
                                vs-len = ""
                                vs-wid = ""
                                vs-dep = "".
                            IF eb.cas-len <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-len,32,OUTPUT vs-len).
                            IF eb.cas-wid <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-wid,32,OUTPUT vs-wid).
                            IF eb.cas-dep <> 0 THEN RUN sys/inc/dec-frac.p (eb.cas-dep,32,OUTPUT vs-dep).
                            v-case-size = (IF vs-len <> "" THEN TRIM(vs-len) + "x" ELSE "") +
                                (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                                trim(vs-dep).

                            ASSIGN 
                                vs-len = ""
                                vs-wid = ""
                                vs-dep = "".
                            IF eb.len <> 0 THEN RUN sys/inc/dec-frac.p (eb.len,32,OUTPUT vs-len).
                            IF eb.wid <> 0 THEN RUN sys/inc/dec-frac.p (eb.wid,32,OUTPUT vs-wid).
                            IF eb.dep <> 0 THEN RUN sys/inc/dec-frac.p (eb.dep,32,OUTPUT vs-dep).
                            v-size[1] = (IF vs-len <> "" THEN TRIM(vs-len) + "x" ELSE "") +
                                (IF vs-wid <> "" THEN TRIM(vs-wid) + "x" ELSE "") +
                                trim(vs-dep).
                            IF ef.trim-l <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-l,32,OUTPUT vs-len).
                            IF ef.trim-w <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-w,32,OUTPUT vs-wid).
                            IF ef.trim-d <> 0 THEN RUN sys/inc/dec-frac.p (ef.trim-d,32,OUTPUT vs-dep).
            
                            ASSIGN
                                v-up       = eb.num-up
                                v-po-no    = IF AVAILABLE oe-ordl THEN oe-ordl.po-no ELSE ""
                                v-spc-no   = eb.spc-no
                                v-ink-list = "".
                            FOR EACH tt-ink:
                                DELETE tt-ink.
                            END.
            
                            DO i = 1 TO 12:
                                IF eb.i-code2[i] <> "" THEN 
                                DO:
                
                                    FOR EACH wrk-ink WHERE wrk-ink.form-no = eb.form-no
                                        AND wrk-ink.blank-no = eb.blank-no
                                        AND wrk-ink.i-code = eb.i-code2[i]
                                        AND wrk-ink.i-pass = eb.i-ps2[i]
                                        NO-LOCK BREAK BY wrk-ink.i-code:
                                        IF FIRST-OF(wrk-ink.i-code) AND
                                            NOT CAN-FIND( FIRST tt-ink WHERE tt-ink.i-code = wrk-ink.i-code) 
                                            THEN 
                                        DO:
                                            CREATE tt-ink.
                                            ASSIGN 
                                                tt-ink.i-code = eb.i-code2[i]
                                                tt-ink.i-seq  = wrk-ink.i-seq.
                                        END.
                                    END.
                                END.
                            END.
                            FOR EACH tt-ink BREAK BY tt-ink.i-seq:
                                v-ink-list =  IF LOOKUP(STRING(tt-ink.i-seq),v-ink-list) > 0 
                                    THEN v-ink-list
                                    ELSE v-ink-list + string(tt-ink.i-seq) + ",".
                            END.
                            IF LENGTH(v-ink-list) > 1 AND
                                SUBSTRING(v-ink-list,LENGTH(v-ink-list),1) = "," 
                                THEN v-ink-list = SUBSTRING(v-ink-list,1,LENGTH(v-ink-list) - 1).                    
                            ELSE IF v-ink-list = "," THEN v-ink-list = "".

                            v-alloc = v-ink-list.            
            
                            IF NUM-ENTRIES(v-ink-list) GT 1 THEN 
                            DO:
                                v-alloc = "".
                                DO j = 1 TO NUM-ENTRIES(v-ink-list):
                                    IF j EQ 1 OR j EQ num-entries(v-ink-list) THEN v-alloc = v-alloc + entry(j,v-ink-list) + ",".
                                    ELSE 
                                    DO:
                                        IF int(ENTRY(j,v-ink-list)) - int(ENTRY(j - 1,v-ink-list)) LE 1 THEN
                                            substr(v-alloc,LENGTH(TRIM(v-alloc)),1) = "-".
                                        ELSE 
                                        DO:
                                            IF substr(v-alloc,LENGTH(TRIM(v-alloc)),1) EQ "-" THEN
                                                v-alloc = v-alloc + entry(j - 1,v-ink-list) + ",".
                                            v-alloc = v-alloc + entry(j,v-ink-list) + ",".
                                        END.
                                    END.
                  
                                END.                    
                                IF v-alloc NE "" THEN substr(v-alloc,LENGTH(TRIM(v-alloc)),1) = "".
                            END.
                            /* ==============*/
                            ASSIGN
                                v-ord-qty   = IF AVAILABLE oe-ordl AND oe-ordl.i-no EQ v-stock-no THEN
                          (IF eb.cust-% > 0 THEN (oe-ordl.qty * eb.cust-%)
                           ELSE oe-ordl.qty)
                        ELSE v-est-qty
                                v-job-qty   = v-job-qty * (IF eb.cust-% > 0 THEN eb.cust-% ELSE 1)
                                v-item-name = IF AVAILABLE oe-ordl THEN oe-ordl.i-name ELSE v-dsc[1]
                                v-dsc[1]    = IF AVAILABLE oe-ordl THEN oe-ordl.part-dscr1 ELSE v-dsc[1].

                            FIND FIRST po-ordl WHERE po-ordl.company = job-hdr.company
                                AND po-ordl.job-no = job-hdr.job-no
                                AND po-ordl.job-no2 = job-hdr.job-no2
                                AND po-ordl.i-no = eb.cas-no NO-LOCK NO-ERROR.
                            v-case-due-date = IF AVAILABLE po-ordl THEN po-ordl.due-date ELSE ?.
                            IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
                            ASSIGN
                                v-cas-no  = ""
                                v-job-cnt = 0.
                            FOR EACH bf-jobmat NO-LOCK WHERE bf-jobmat.company = eb.company
                                AND bf-jobmat.job-no = job-hdr.job-no
                                AND bf-jobmat.job-no2 = job-hdr.job-no2
                                AND bf-jobmat.frm = eb.form-no
                                AND (bf-jobmat.blank-no = eb.blank-no OR bf-jobmat.blank-no = 0)
                                AND CAN-FIND(FIRST ITEM WHERE item.company = bf-jobmat.company
                                AND ITEM.i-no = bf-jobmat.i-no 
                                AND ITEM.mat-type = "C")
                                BY bf-jobmat.j-no DESCENDING BY bf-jobmat.blank-no DESCENDING:
                                ASSIGN
                                    v-cas-no  = bf-jobmat.i-no
                                    v-job-cnt = bf-jobmat.qty.
                                LEAVE.
                            END.            
                            v-prev-job = "".
                            FOR EACH bf-fg-bin NO-LOCK WHERE bf-fg-bin.company EQ eb.company
                                AND bf-fg-bin.i-no    EQ eb.stock-no
                                BY bf-fg-bin.job-no BY bf-fg-bin.job-no2 :
                                IF bf-fg-bin.job-no = job-hdr.job-no AND
                                    bf-fg-bin.job-no2 = job-hdr.job-no2 THEN .
                                ELSE v-prev-job = v-prev-job + trim(bf-fg-bin.job-no) + ",".
                            END.
               
                            IF AVAILABLE oe-ordl THEN
                                FIND FIRST po-ord WHERE po-ord.company = oe-ordl.company
                                    AND po-ord.po-no = int(oe-ordl.po-no-po) NO-LOCK NO-ERROR.
                            ASSIGN
                                cVendor  = IF AVAILABLE oe-ordl THEN oe-ordl.vend-no ELSE ""
                                iBoardPO = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po ELSE 0.
                            IF AVAILABLE po-ord THEN
                                FIND FIRST po-ordl WHERE
                                    po-ordl.company EQ po-ord.company AND
                                    po-ordl.po-no   EQ po-ord.po-no AND
                                    po-ordl.i-no = ef.board
                                    NO-LOCK NO-ERROR.

                            dtPoDueDate = IF AVAILABLE po-ordl THEN po-ordl.due-date ELSE ?. 
                            IF cVendor NE "" THEN 
                            DO:
                                FIND FIRST vend NO-LOCK 
                                    WHERE vend.company EQ eb.company
                                    AND vend.vend-no EQ cVendor
                                    USE-INDEX vend NO-ERROR.
                                IF AVAILABLE vend THEN ASSIGN cVendor = vend.NAME .
                            END.

                            FIND FIRST ITEM NO-LOCK 
                                WHERE ITEM.company EQ ef.company
                                AND ITEM.i-no EQ ef.board NO-ERROR .

                            IF lPrintSetHeader THEN DO: 
                                lPrintSetHeader = FALSE.
                                FOR EACH bff-eb NO-LOCK
                                    WHERE bff-eb.company EQ eb.company
                                    AND bff-eb.est-no EQ eb.est-no
                                    AND bff-eb.form-no EQ 0
                                    AND bff-eb.est-type EQ 2 :
                                    
                                    FIND FIRST bf-ttSoule NO-LOCK
                                         WHERE bf-ttSoule.frm EQ bff-eb.form-no
                                         AND bf-ttSoule.blank-no EQ bff-eb.blank-no 
                                         AND bf-ttSoule.runForm EQ YES NO-ERROR.

                                    lAssembled = IF bff-eb.set-is-assembled EQ YES THEN YES ELSE NO .
                                    cSetFGItem = bff-eb.stock-no  .
                                    IF AVAIL bf-ttSoule THEN DO:
                                     IF LINE-COUNTER > 70 THEN DO: 
                                       PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                                       PAGE.
                                       RUN pPrintHeader .
                                     END.
                                    
                                     PUT 
                                         "<BGCOLOR=255,255,0><C1.5><FROM><R+1><C12><FILLRECT><R-1><BGCOLOR=WHITE>"
                                         "<C1.5><FROM><R+1><C12><RECT><R-1>"
                                         "<P10><B><C2>Form " TRIM(STRING(bff-eb.form-no,"99")) "</B>" 
                                         "<P10><C20><b>Customer Part: </B>" bff-eb.part-no FORMAT "x(15)"   
                                         "<C45><b>FG#: </b>" bff-eb.stock-no  FORMAT "x(15)"    skip 
                                         
                                         "<P10><C20><b>Descr.: </B>" bff-eb.part-dscr1 FORMAT "x(30)"  
                                         "<C45><b>Cad#: </b>" bff-eb.cad-no FORMAT "x(20)" skip
                                         
                                         "<P10><C20><b>Adhesive: </B>" bff-eb.adhesive FORMAT "x(15)"  
                                         "<C45><b>Art#: </b>" bff-eb.Plate-no FORMAT "x(30)" SKIP
                                         
                                         "<C2><B>Blank | </B>" STRING(bff-eb.blank-no,"99")  
                                         "<P10><C20><b>Size: </B>" (string(bff-eb.len,">9.9999") + " x " + STRING(bff-eb.wid,">9.9999") + " x " + STRING(bff-eb.dep,">9.9999")) FORMAT "x(40)" 
                                          "<C45>" bff-eb.spc-no FORMAT "x(30)" SKIP .
                                         IF LINE-COUNTER > 70 THEN DO: 
                                             PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                                             PAGE.
                                             RUN pPrintHeader .
                                         END.
                                       RUN pGetCaseItem(BUFFER bff-eb, BUFFER job-hdr, OUTPUT cCaseItem, OUTPUT cCaseSize, OUTPUT cCaseCount, OUTPUT cCasePerPallet).
                                       PUT  
                                         "<C19.5><FROM><R+3><C65><RECT><R-3>"
                                         "<P10><C20><b>Packing: </B>" cCaseItem FORMAT "x(15)"  
                                         "<C45><b>Pallet: </b>" bff-eb.tr-no FORMAT "x(15)" SKIP

                                         "<P10><C20><b>Case Size: </B>" cCaseSize FORMAT "x(40)"  
                                         "<C45><b>Ctn/Bdl.Per: </b>" cCasePerPallet SKIP

                                         "<P10><C20><b>Count: </B>" cCaseCount  SKIP  .
                                         RUN pPrintMiscItems(bff-eb.est-no,bff-eb.form-no,bff-eb.blank-no,"5,6,M").
                                        PUT v-fill SKIP .
                                        PUT "<R-1>" .
                                    END.    
                                END.
                            END.

                            IF FIRST-OF(eb.form-no) THEN 
                            DO:
                                RUN pGetFormQtys(eb.company, bf-jobhdr.job-no, bf-jobhdr.job-no2, eb.form-no, OUTPUT iEbTotalYldQty, OUTPUT iEBTotalBlQty, OUTPUT iEbTotalUpQty).
/*                                ASSIGN iEbTotalYldQty = 0                                                                                                */
/*                                       iEbTotalblQty  = 0                                                                                                */
/*                                       iEbTotalUpQty  = 0 .                                                                                              */
/*                                                                                                                                                         */
/*                                FOR EACH bff-eb NO-LOCK                                                                                                  */
/*                                    WHERE bff-eb.company EQ eb.company                                                                                   */
/*                                      AND bff-eb.est-no EQ eb.est-no                                                                                     */
/*                                      AND bff-eb.form-no EQ eb.form-no ,                                                                                 */
/*                                    FIRST bf-ttSoule WHERE  bf-ttSoule.frm EQ bff-eb.form-no                                                             */
/*                                     AND  bf-ttSoule.blank-no EQ bff-eb.blank-no                                                                         */
/*                                    AND  bf-ttSoule.runForm EQ YES NO-LOCK :                                                                             */
/*                                    dPerSetQty = 1 .                                                                                                     */
/*                                    IF cSetFGItem NE "" THEN do:                                                                                         */
/*                                         FIND FIRST fg-set WHERE fg-set.company = eb.company                                                             */
/*                                             AND fg-set.set-no = cSetFGItem                                                                              */
/*                                             AND fg-set.part-no = bff-eb.stock-no NO-ERROR.                                                              */
/*                                         IF AVAIL fg-set THEN                                                                                            */
/*                                            dPerSetQty = fg-set.qtyPerSet .                                                                              */
/*                                     END.                                                                                                                */
/*                                                                                                                                                         */
/*                                    ASSIGN                                                                                                               */
/*                                        iEbTotalYldQty = iEbTotalYldQty + ((IF bff-eb.yld-qty EQ 0 THEN bff-eb.bl-qty ELSE bff-eb.yld-qty) * dPerSetQty )*/
/*                                        iEbTotalblQty  = iEbTotalblQty + ( bff-eb.bl-qty * dPerSetQty )                                                  */
/*                                        iEbTotalUpQty  = iEbTotalUpQty + bff-eb.num-up  .                                                                */
/*                                END.                                                                                                                     */

                                k = 1 .
                                PUT 
                                    "<BGCOLOR=255,255,0><C1.5><FROM><R+1><C12><FILLRECT><R-1><BGCOLOR=WHITE>"
                                    "<C1.5><FROM><R+1><C12><RECT><R-1>"
                                    "<P10><B><C2>Form " TRIM(STRING(eb.form-no,"99")) "</B>"
                                    "<C34><b>Sheet: </b>" ef.gsh-wid  SPACE(3) ef.gsh-len 
                                    "<C54><b># Out:</b>" ef.n-out  "<C66><b>Total Yield Qty: </b>" STRING(iEbTotalYldQty) SKIP
                                    "<C2><b>Material: </b>" (IF AVAILABLE ITEM THEN ITEM.i-no ELSE "") FORMAT "x(10)"
                                    "<C20><FROM><C+13><R+2><BARCODE,TYPE=128A,CHECKSUM=NONE,VALUE=" + string((job-hdr.job-no) + "-" + STRING(job-hdr.job-no2) + "-" + STRING( eb.form-no)) + "><R-2>" FORMAT "x(250)"
                                    "<C34><b>Press: </b>" ef.nsh-wid  SPACE(3) ef.nsh-len  "<C66><b>Total Req. Qty: </b>" STRING(iEbTotalblQty)  SKIP
                                    "<C2>" (IF AVAILABLE ITEM THEN ITEM.i-dscr ELSE "") FORMAT "x(30)"
                                    "<C34><b>Die:    </b>" ef.trim-w FORMAT ">>9.9999" SPACE(3) ef.trim-l FORMAT ">>9.9999"  "<C54><b>Total # Up: </b>" STRING(iEbTotalUpQty)  
                                     "<C65>   <b>Die#: </b>" eb.die-no FORMAT "x(20)" SKIP(1) .
                                
                                RUN pPrintOperationsForForm(ef.company, job-hdr.job-no, job-hdr.job-no2,ef.form-no).
                                RUN pGetBlankQtys(ef.company, job-hdr.job-no, job-hdr.job-no2, eb.form-no, eb.blank-no, OUTPUT iEbTotalYldQty, OUTPUT iEbTotalBlQty).
                                
                              PUT v-fill SKIP  .
/*                              iEbTotalYldQty = IF eb.yld-qty EQ 0 THEN eb.bl-qty ELSE eb.yld-qty .*/
/*                              iEbTotalblQty  = eb.bl-qty  .                                       */
                              RUN pGetJobQty(bf-jobhdr.job-no,bf-jobhdr.job-no2,bf-jobhdr.frm,eb.blank-no, OUTPUT iJobQty ) .        
                              iEbTotalOverQty = IF AVAIL oe-ordl THEN( iEbTotalBlQty * ( 1 + oe-ordl.over-pct / 100 )) ELSE IF iJobQty NE 0 THEN iJobQty ELSE bf-jobhdr.qty .
                              IF cSetFGItem NE "" THEN do:
                                  FIND FIRST fg-set WHERE fg-set.company = eb.company
                                      AND fg-set.set-no = cSetFGItem
                                      AND fg-set.part-no = eb.stock-no NO-ERROR.
                                  IF AVAIL fg-set THEN
                                      ASSIGN
                                      iEbTotalYldQty = iEbTotalYldQty * fg-set.qtyPerSet 
                                      iEbTotalblQty  = iEbTotalblQty  * fg-set.qtyPerSet 
                                      iEbTotalOverQty = iEbTotalOverQty * fg-set.qtyPerSet .
                              END.
                               
                              PUT 
                                  "<R-1><P10><C20><b>Customer Part: </B>" eb.part-no FORMAT "x(15)"   
                                  "<C45><b>FG#: </b>" eb.stock-no FORMAT "x(15)"     
                                  "<C70><B><P10>Yield Qty: </B>" TRIM(STRING(iEbTotalYldQty)) FORMAT "x(12)"  skip 

                                  "<P10><C20><b>Descr.: </B>" eb.part-dscr1 FORMAT "x(30)"  
                                  "<C45><b>Cad#: </b>" eb.cad-no FORMAT "x(20)"
                                  "<C70><B><P10>Req Qty: </B>" TRIM(STRING(iEbTotalblQty)) FORMAT "x(12)"  skip

                                  "<P10><C20><b>Adhesive: </B>" eb.adhesive FORMAT "x(15)"  
                                  "<C45><b>Art#: </b>" eb.Plate-no FORMAT "x(30)"
                                  "<C70><B><P10>Over Qty: </B>" TRIM(STRING(iEbTotalOverQty)) FORMAT "x(12)" SKIP 
                                  
                                  "<C2><B>Blank | </B>" STRING(eb.blank-no,"99")  "<C10><B># Up: </b>" string(eb.num-up)
                                  "<P10><C20><b>Size: </B>" (string(eb.len,">9.9999") + " x " + STRING(eb.wid,">9.9999") + " x " + STRING(eb.dep,">9.9999")) FORMAT "x(40)" 
                                  "<C45>" eb.spc-no FORMAT "x(30)"
                                  SKIP .
                        
                                  IF LINE-COUNTER > 70 THEN 
                                    DO:
                                      PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                                        PAGE.
                                        RUN pPrintHeader .
                                    END.
                              IF NOT lAssembled THEN do:
                               RUN pGetCaseItem(BUFFER eb, BUFFER job-hdr, OUTPUT cCaseItem, OUTPUT cCaseSize, OUTPUT cCaseCount, OUTPUT cCasePerPallet).
                               PUT   
                                  "<C19.5><FROM><R+3><C65><RECT><R-3>"
                                  "<P10><C20><b>Packing: </B>" cCaseItem FORMAT "x(15)"  
                                  "<C45><b>Pallet: </b>" eb.tr-no FORMAT "x(15)" SKIP

                                  "<P10><C20><b>Case Size: </B>" cCaseSize FORMAT "x(40)"
                                  "<C45><b>Ctn/Bdl.Per: </b>" cCasePerPallet SKIP

                                  "<P10><C20><b>Count: </B>" cCaseCount 
                                                /*"<C45><b>Label: </b>" (IF eb.layer-pad NE "" OR eb.divider NE "" THEN "Y" ELSE "N" )*/ SKIP  .
                             
                                   RUN pPrintMiscItems(eb.est-no,eb.form-no,eb.blank-no,"5,6,M").
                              END.
                              ELSE 
                                   RUN pPrintMiscItems(eb.est-no,eb.form-no,eb.blank-no,"M").

                                PUT v-fill SKIP .

                             FOR EACH bff-eb NO-LOCK
                                    WHERE bff-eb.est-no EQ eb.est-no
                                      AND bff-eb.form-no EQ eb.form-no 
                                      AND bff-eb.blank-no NE eb.blank-no ,
                                    FIRST bf-ttSoule WHERE  bf-ttSoule.frm EQ bff-eb.form-no
                                     AND  bf-ttSoule.blank-no EQ bff-eb.blank-no 
                                    AND  bf-ttSoule.runForm EQ YES NO-LOCK :
                                    bf-ttSoule.runForm = NO.
                                     IF LINE-COUNTER > 70 THEN DO: 
                                        PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                                         PAGE.
                                         RUN pPrintHeader .
                                     END.
                                     k = K + 1 .
                                     RUN pGetBlankQtys(ef.company, job-hdr.job-no, job-hdr.job-no2, bff-eb.form-no, bff-eb.blank-no, OUTPUT iEbTotalYldQty, OUTPUT iEbTotalBlQty).
/*                                     iEbTotalYldQty = IF bff-eb.yld-qty EQ 0 THEN bff-eb.bl-qty ELSE bff-eb.yld-qty .*/
/*                                     iEbTotalblQty  = bff-eb.bl-qty  .                                               */
                                     RUN pGetJobQty(bf-jobhdr.job-no,bf-jobhdr.job-no2,bf-jobhdr.frm,bff-eb.blank-no, OUTPUT iJobQty ) .
                                     iEbTotalOverQty = IF AVAIL oe-ordl THEN (iEbTotalBlQty * (1 + oe-ordl.over-pct / 100 )) ELSE IF iJobQty NE 0 THEN iJobQty ELSE bf-jobhdr.qty .
                                     IF cSetFGItem NE "" THEN 
                                     do:
                                         FIND FIRST fg-set WHERE fg-set.company = eb.company
                                             AND fg-set.set-no = cSetFGItem
                                             AND fg-set.part-no = bff-eb.stock-no NO-ERROR.
                                         IF AVAIL fg-set THEN
                                             ASSIGN
                                             iEbTotalYldQty = iEbTotalYldQty * fg-set.qtyPerSet 
                                             iEbTotalblQty  = iEbTotalblQty * fg-set.qtyPerSet 
                                             iEbTotalOverQty = iEbTotalOverQty * fg-set.qtyPerSet.
                                     END.

                                     PUT 
                                         "<R-1><P10><C20><b>Customer Part: </B>" bff-eb.part-no FORMAT "x(15)"   
                                         "<C45><b>FG#: </b>" bff-eb.stock-no  FORMAT "x(15)"    
                                         "<C70><B><P10>Yield Qty: </B>" TRIM(STRING(iEbTotalYldQty)) FORMAT "x(12)"  skip 
                                         
                                         "<P10><C20><b>Descr.: </B>" bff-eb.part-dscr1 FORMAT "x(30)"  
                                         "<C45><b>Cad#: </b>" bff-eb.cad-no FORMAT "x(20)"
                                         "<C70><B><P10>Req Qty: </B>" TRIM(STRING(iEbTotalblQty)) FORMAT "x(12)"  skip
                                         
                                         "<P10><C20><b>Adhesive: </B>" bff-eb.adhesive FORMAT "x(15)"  
                                         "<C45><b>Art#: </b>" bff-eb.Plate-no FORMAT "x(30)"
                                         "<C70><B><P10>Over Qty: </B>" TRIM(STRING(iEbTotalOverQty)) FORMAT "x(12)" SKIP
                                         
                                         "<C2><B>Blank | </B>" STRING(bff-eb.blank-no,"99")  "<C10><B># Up: </B>" string(bff-eb.num-up)
                                         "<P10><C20><b>Size: </B>" (string(bff-eb.len,">9.9999") + " x " + STRING(bff-eb.wid,">9.9999") + " x " + STRING(bff-eb.dep,">9.9999")) FORMAT "x(40)" 
                                         "<C45>" bff-eb.spc-no FORMAT "x(30)" SKIP .

                                        IF LINE-COUNTER > 70 THEN DO:
                                            PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                                            PAGE.
                                            RUN pPrintHeader .
                                        END.
                                      IF NOT lAssembled THEN do:
                                       RUN pGetCaseItem(BUFFER bff-eb, BUFFER job-hdr, OUTPUT cCaseItem, OUTPUT cCaseSize, OUTPUT cCaseCount, OUTPUT cCasePerPallet).
                                       PUT  
                                         "<C19.5><FROM><R+3><C65><RECT><R-3>"
                                         "<P10><C20><b>Packing: </B>" cCaseItem FORMAT "x(15)"  
                                         "<C45><b>Pallet: </b>" bff-eb.tr-no FORMAT "x(15)" SKIP

                                         "<P10><C20><b>Case Size: </B>" cCaseSize FORMAT "x(40)"  
                                         "<C45><b>Ctn/Bdl.Per: </b>" cCasePerPallet SKIP

                                         "<P10><C20><b>Count: </B>" cCaseCount  
                                         /*"<C45><b>Label: </b>" (IF bff-eb.layer-pad NE "" OR bff-eb.divider NE "" THEN "Y" ELSE "N" )*/ SKIP  .
                                         RUN pPrintMiscItems(bff-eb.est-no,bff-eb.form-no,bff-eb.blank-no,"5,6,M").
                                      END.
                                      ELSE 
                                         RUN pPrintMiscItems(bff-eb.est-no,bff-eb.form-no,bff-eb.blank-no,"M").
                                        PUT v-fill SKIP .
                                END.
                                PUT "<R-1>" .
                            IF LINE-COUNTER > 70 THEN 
                            DO: 
                                PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                                PAGE.
                                RUN pPrintHeader .
                            END.

                        END.  /* first-of eb form*/

                    v-itm-printed = v-itm-printed + 1.    
                END. /* eb */
            END.   /*ef */
            /*IF NOT LAST(tt-reftable.val[12]) 
                OR tt-reftable.est-type = 4  THEN  PUT v-fill3  SKIP .*/
        END. /*first-of(tt-reftable.val[12]*/
    END. /*tt-reftable*/
        
END. /* last-of(bf-jobhdr.frm) */
END. /* each bf-jobhdr*/

     
    
FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
    AND bf-jobhdr.job-no = job-hdr.job-no
    AND bf-jobhdr.job-no2 = job-hdr.job-no2
    BREAK BY bf-jobhdr.frm:
    lv-text = "".

    IF FIRST(bf-jobhdr.frm) THEN 
    DO:

        lv-line-chars = 60.
        FIND FIRST job OF bf-jobhdr NO-LOCK NO-ERROR.       
        ASSIGN 
            v-inst2       = ""
            v-tmp-lines   = 0
            j             = 0
            K             = 0
            lv-got-return = 0
            lv-note-cnt   = 0.
        IF FIRST(bf-jobhdr.frm) THEN 
        DO: 
            FIND FIRST oe-ordl WHERE oe-ordl.company EQ job-hdr.company
                AND oe-ordl.ord-no  EQ job-hdr.ord-no 
                AND oe-ordl.i-no EQ job-hdr.i-no 
                NO-LOCK NO-ERROR.

            PUT "<C5><B>Special instructions - "  (IF cRdOptionMclean EQ "M" THEN "Moorestown" ELSE "Nazareth") FORMAT "x(12)" SKIP
                "<C5>Over:" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.over-pct,">>9.99%") ELSE "0") 
                "<C28>Under:" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.under-pct,">>9.99%") ELSE "0")  SKIP .
            PUT "<B><C5>FORM  <C9>DEPARTMENT      INSTRUCTION NOTES</B>" SKIP.

        END.
        
        
        FOR EACH notes WHERE notes.rec_key = job.rec_key 
             AND notes.note_code <> ''  NO-LOCK               
              BY notes.note_form_no:
          FIND FIRST bf-ttSoule WHERE  bf-ttSoule.frm EQ notes.note_form_no
              AND  bf-ttSoule.runForm EQ YES NO-LOCK NO-ERROR.
          IF NOT AVAILABLE bf-ttSoule AND NOT notes.note_form_no EQ 0 THEN NEXT.
            v-inst2 = "".
            IF v-prev-note-rec <> ? AND
                v-prev-note-rec <> RECID(notes) THEN v-prev-extent = lv-note-cnt.
            
            lv-form-note = /*notes.note_title + " " +*/ notes.note_text .

            DO i = 1 TO LENGTH(lv-form-note) :        
                IF i - j >= lv-line-chars THEN ASSIGN j             = i
                        lv-got-return = lv-got-return + 1.
                v-tmp-lines = ( i - j ) / lv-line-chars.
                {SYS/INC/ROUNDUP.I v-tmp-lines}
                k = v-tmp-lines + lv-got-return.

                IF k > 0 AND k <= 70 THEN 
                    v-inst2[k] = v-inst2[k] +
                        IF SUBSTRING(lv-form-note,i,1) <> CHR(10) AND SUBSTRING(notes.note_text,i,1) <> CHR(13)
                        THEN SUBSTRING(lv-form-note,i,1)
                        ELSE "" .              

                IF SUBSTRING(lv-form-note,i,1) = CHR(10) OR SUBSTRING(lv-form-note,i,1) = CHR(13) THEN
                    ASSIGN
                        lv-got-return = lv-got-return + 1
                        j             = i.
            END.

            ASSIGN 
                v-prev-note-rec = RECID(notes)
                j               = 0
                lv-got-return   = 0
                lv-note-cnt     = lv-note-cnt + k.
            IF lv-note-cnt > 70 THEN LEAVE.
            /* PRINT*/
            /*<ADJUST=LPI>*/
            
            IF LINE-COUNTER > 70 THEN 
            DO:
                PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                PAGE.
                RUN pPrintHeader .
            END.
            PUT "<P10><C5>" notes.note_form_no "<C9>" "<B>" + caps(TRIM(SUBSTRING(notes.note_title,1,13))) + "</B>" FORM "x(22)"
                "<C20>" v-inst2[1] FORM "x(60)" "<P10>" SKIP.
            DO i = 2 TO k /*cnt*/ :
                PUT "<P10><C20>"  v-inst2[i] FORM "x(60)" "<P10>"  SKIP.
               
                IF LINE-COUNTER > 70 THEN 
                DO:
                    PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                    PAGE.
                    RUN pPrintHeader .
                    PUT "<B><C5>FORM  <C9>DEPARTMENT     INSTRUCTION NOTES</B>" SKIP.
                END.
            END.
            PUT "<LPI=6>" SKIP. 
            ASSIGN 
                k           = 0
                v-tmp-lines = 0
                .
        END.        
    END.
END.

    
/* print die# image */
IF print-box THEN 
DO:    
    MAIN-PRINT-BOX:
    FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
        AND bf-jobhdr.job-no = job-hdr.job-no
        AND bf-jobhdr.job-no2 = job-hdr.job-no2
        BREAK BY bf-jobhdr.frm:
       
        IF est.est-type NE 1 AND NOT CAN-FIND(FIRST ttSoule NO-LOCK
                        WHERE  ttSoule.frm EQ bf-jobhdr.frm
                         AND   ttSoule.runForm EQ YES
                         AND   ttSoule.lPrintDieImage ) THEN NEXT MAIN-PRINT-BOX.

        IF FIRST-OF(bf-jobhdr.frm) THEN 
        DO:
            FIND FIRST b-ef WHERE b-ef.company = bf-jobhdr.company
                AND b-ef.est-no = bf-jobhdr.est-no
                AND b-ef.form-no = bf-jobhdr.frm NO-LOCK NO-ERROR.
            FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
            IF AVAILABLE b-ef AND b-ef.cad-image <> "" 
                AND LOOKUP(b-ef.cad-image,lv-cad-image-list) <= 0
                THEN 
            DO:    
                DO li = LENGTH(TRIM(b-ef.cad-image)) TO 1 BY -1:
                    IF SUBSTR(b-ef.cad-image,li,1) EQ "/" OR
                        SUBSTR(b-ef.cad-image,li,1) EQ "\" OR
                        SUBSTR(b-ef.cad-image,li,1) EQ ":" THEN LEAVE.
                    cImageBoxDesign = SUBSTR(b-ef.cad-image,li,1) + TRIM(cImageBoxDesign).
                END.
               
                lv-cad-image =  (IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "") + "\" +
                    cImageBoxDesign  .
                lv-cad-image-list = lv-cad-image-list + b-ef.cad-image + ",". 
                PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                PAGE.
                RUN pPrintHeader .
            
                PUT UNFORMATTED 
                    "<#12><C2><R8><FROM><C80><R+52><RECT><||3><C80>" /*v-qa-text*/ SKIP
                    "<=12><R+1><C5>Image: Die        "    "Form No:" bf-jobhdr.frm FORMAT ">99"  
                    "<=12><R+2><C2><FROM><C80><LINE><||3>"
                    "<=12><R+3><C3><#21><R+46><C+76><IMAGE#21=" lv-cad-image ">" SKIP. 
            END.
        END.
    END. /*each bf-jobhdr*/
END.  /* print-box*/

RUN pPrintCadImage(job-hdr.company,job-hdr.job-no,job-hdr.job-no2).

RUN pPrintPlateImage(job-hdr.company,job-hdr.job-no,job-hdr.job-no2).

/* print fgitem image */
IF s-prt-fgimage THEN 
DO:      
    MAIN-PRINT-IMAGE:
    FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
        AND bf-jobhdr.job-no = job-hdr.job-no
        AND bf-jobhdr.job-no2 = job-hdr.job-no2
        BREAK BY bf-jobhdr.frm:

        IF est.est-type NE 1 AND NOT CAN-FIND(FIRST ttSoule NO-LOCK
                        WHERE  ttSoule.frm EQ bf-jobhdr.frm
                         AND   ttSoule.i-no EQ bf-jobhdr.i-no
                         AND   ttSoule.runForm EQ YES
                         AND   ttSoule.lPrintFGImage) THEN NEXT MAIN-PRINT-IMAGE.
        
        IF FIRST-OF(bf-jobhdr.frm) THEN 
        DO:
            FIND FIRST itemfg NO-LOCK
                WHERE itemfg.company EQ job-hdr.company 
                AND itemfg.i-no    EQ bf-jobhdr.i-no NO-ERROR.
            clsFGitemImg = IF AVAILABLE itemfg THEN itemfg.box-image ELSE "" .
            IF clsFGitemImg NE "" THEN 
            DO:
                PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                PAGE.
                RUN pPrintHeader .
                PUT UNFORMATTED 
                    "<#12><C2><R8><FROM><C80><R+52><RECT><||3><C80>" /*v-qa-text*/ SKIP
                    "<=12><R+1><C5>FG Item: " itemfg.i-no " " itemfg.i-name
                    "<=12><R+2><C2><FROM><C80><LINE><||3>"
                    "<=12><R+3><C5><#21><R+46><C+80><IMAGE#21=" clsFGitemImg ">" SKIP. 
            END.
        END.
    END. /*each bf-jobhdr*/
END.  /* print fg item*/

/* print include page */
IF lIncludeLastPage THEN 
DO:          
    FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = job-hdr.company
        AND bf-jobhdr.job-no = job-hdr.job-no
        AND bf-jobhdr.job-no2 = job-hdr.job-no2,
        FIRST itemfg NO-LOCK 
        WHERE itemfg.company EQ bf-jobhdr.company
        AND itemfg.i-no EQ bf-jobhdr.i-no
        BREAK BY bf-jobhdr.i-no:

        FIND FIRST oe-ord WHERE oe-ord.company EQ job-hdr.company
            AND oe-ord.ord-no  EQ job-hdr.ord-no NO-LOCK NO-ERROR.
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ job-hdr.company
            AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no
            AND oe-ordl.job-no  EQ job-hdr.job-no
            AND oe-ordl.job-no2 EQ job-hdr.job-no2
            AND oe-ordl.i-no    EQ bf-jobhdr.i-no
            NO-ERROR.

        IF FIRST(bf-jobhdr.i-no) THEN 
        DO:
            PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
            PAGE.
            FIND FIRST cust NO-LOCK
                WHERE cust.company EQ job-hdr.company
                AND cust.cust-no EQ bf-jobhdr.cust-no
                NO-ERROR .

            PUT UNFORMATTED 
                "<C1.5><b>Account ID:</b> <C10>" bf-jobhdr.cust-no FORMAT "x(8)" 
                "<C37> <b>Order#:</b><C47>" (IF AVAILABLE oe-ord THEN STRING(oe-ord.ord-no) ELSE "") SKIP
                "<C1.5><b>Name:</b> <C10>"  (IF AVAILABLE cust THEN cust.NAME ELSE "") FORMAT "x(30)"   
                "<C37> <b>Order Date:</b><C47>" (IF AVAILABLE oe-ord THEN STRING(oe-ord.ord-date,"99/99/9999") ELSE "") SKIP
                "<C1> <C10>" (IF AVAILABLE cust THEN cust.addr[1] ELSE "") FORMAT "x(30)"   
                "<C37> <b>Est.#(s):</b><C47>" (IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ord.est-no)) ELSE "") SKIP

                "<C10>" (IF AVAILABLE cust THEN cust.addr[2] ELSE "") FORMAT "x(30)"  "<C37><b> Ident.:</b><C47>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.i-name) ELSE "") FORMAT "x(30)"  SKIP
                "<C10>" (IF AVAILABLE cust THEN STRING(cust.city + " " + cust.state + "   " + cust.zip) ELSE "") FORMAT "x(33)"    
                "<C37> <b>Repeat:</b><C47>" (IF AVAILABLE oe-ord AND oe-ord.TYPE EQ "R" THEN "Y" ELSE "N") SKIP
                "<C37> <b>Cust Part#:</b><C47>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.part-no) ELSE "") FORMAT "x(15)" SKIP
                "<C1.5><b>Salesperson:</b> <C10>"  (IF AVAILABLE oe-ord THEN STRING(oe-ord.sman[1] + "  " + oe-ord.sname[1]) ELSE "") FORMAT "x(33)"   
                "<C37><b> Completion:</b><C47>" (IF AVAILABLE oe-ord THEN STRING(oe-ord.due-date,"99/99/9999") ELSE "") SKIP 
                .
            PUT "<#12><C1.5><R9.5><FROM><C84><R+54><RECT><||3><C80>" "<R9.5>" .

             
            IF AVAILABLE est AND est.est-type EQ 2 THEN 
            DO:
                iSetRelQty = 0 .
                cRelDate = "" .
                i = 0 .
                FOR EACH oe-rel NO-LOCK
                    WHERE oe-rel.company EQ job-hdr.company
                    AND oe-rel.ord-no EQ job-hdr.ord-no :
                    i = i + 1 .
                    {oe/rel-stat.i cRelStat}
                    IF AVAILABLE oe-rell THEN
                        FIND FIRST oe-relh WHERE oe-relh.r-no EQ oe-rell.r-no NO-LOCK NO-ERROR.
                    cRelDate[i] = STRING(IF AVAILABLE oe-relh THEN oe-relh.rel-date ELSE oe-rel.rel-date).
                    iSetRelQty[i] = oe-rel.tot-qty .
                    IF i GE 8 THEN LEAVE .
                END.

                PUT "<#15><C66><R1><FROM><C84><R+8><RECT><||3>"
                    "<=15><C66><R2><FROM><C84><LINE><||3>"
                    "<=15><C66><R3><FROM><C84><LINE><||3>"
                    "<=15><C75><R2><FROM><C75><R+7><LINE><||3>"
                    "<R1><C67>  Delivery Schedule"
                    "<R2><C69>      Qty              Date   "
                    "<R3><C69>" iSetRelQty[1] FORMAT ">>>>>>>>>" SPACE(9) cRelDate[1]
                    "<R4><C69>" iSetRelQty[2] FORMAT ">>>>>>>>>" SPACE(9) cRelDate[2]
                    "<R5><C69>" iSetRelQty[3] FORMAT ">>>>>>>>>" SPACE(9) cRelDate[3]
                    "<R6><C69>" iSetRelQty[4] FORMAT ">>>>>>>>>" SPACE(9) cRelDate[4]
                    "<R7><C69>" iSetRelQty[5] FORMAT ">>>>>>>>>" SPACE(9) cRelDate[5]
                    "<R8><C69>" iSetRelQty[6] FORMAT ">>>>>>>>>" SPACE(9) cRelDate[6] "<R9.5>" .
            END.
             


        END.
        IF FIRST-OF(bf-jobhdr.i-no) THEN 
        DO:

            FOR EACH job-mat
                WHERE job-mat.company EQ cocode
                AND job-mat.job     EQ bf-jobhdr.job
                AND job-mat.frm     EQ bf-jobhdr.frm
                NO-LOCK,
                FIRST item
                {sys/look/itemivW.i}
                and item.i-no eq job-mat.i-no 
                AND item.mat-type NE "B"  NO-LOCK BREAK BY job-mat.i-no :
            dBoardSheet = job-mat.qty .
        END.

        FIND FIRST b-eb WHERE b-eb.company = bf-jobhdr.company
                AND b-eb.est-no = bf-jobhdr.est-no
                AND b-eb.form-no = bf-jobhdr.frm
                AND b-eb.stock-no = bf-jobhdr.i-no
                AND (b-eb.blank-no = bf-jobhdr.blank-no OR bf-jobhdr.blank-no EQ 0 ) NO-LOCK NO-ERROR.

        PUT UNFORMATTED 
            "<C2><b>Order Qty: </b><C11>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.qty) ELSE "0") FORMAT "x(10)"
            "<C25><b>FG #: </b>"  (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.i-no) ELSE itemfg.i-no) FORMAT "x(15)" 
            "<C60><b> Price:</b>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.price) ELSE "0") SKIP
            "<C2><b>Material: </b><C11>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.i-name) ELSE itemfg.i-name) FORMAT "x(30)" SKIP
            "<C11>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.part-dscr1) ELSE "") FORMAT "x(30)" SKIP
            "<C2><b>Estimate:</b> <C11>" (IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.est-no)) ELSE "") FORMAT "x(10)" 
            "<C25><b>Quantity:</b>" (IF AVAIL b-eb THEN b-eb.bl-qty ELSE 0) FORMAT ">>>>>>9.99<<"
            "<C60> <b>UOM:</b>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.pr-uom) ELSE "") SKIP 
            "<C1.5><FROM><C84><LINE><||3>" SKIP.
                       
        IF LINE-COUNTER > 58 THEN do:
            PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" . 
            PAGE.
        END.

/*       IF LAST(bf-jobhdr.i-no) AND AVAILABLE est AND est.est-type EQ 2 THEN DO:                                         */
/*        FIND FIRST bff-eb NO-LOCK                                                                                       */
/*             WHERE bff-eb.company = bf-jobhdr.company                                                                   */
/*                AND bff-eb.est-no = bf-jobhdr.est-no                                                                    */
/*                AND bff-eb.blank-no EQ 0   NO-ERROR.                                                                    */
/*                                                                                                                        */
/*        FOR EACH b-eb WHERE b-eb.company = bf-jobhdr.company                                                            */
/*                AND b-eb.est-no = bf-jobhdr.est-no                                                                      */
/*                AND b-eb.form-no NE 0 NO-LOCK :                                                                         */
/*                                                                                                                        */
/*            IF AVAIL bff-eb AND  bff-eb.set-is-assembled EQ NO THEN                                                     */
/*                FIND FIRST oe-ordl NO-LOCK                                                                              */
/*                WHERE oe-ordl.company EQ job-hdr.company                                                                */
/*                AND oe-ordl.ord-no  EQ bf-jobhdr.ord-no                                                                 */
/*                AND oe-ordl.job-no  EQ bf-jobhdr.job-no                                                                 */
/*                AND oe-ordl.job-no2 EQ bf-jobhdr.job-no2                                                                */
/*                AND oe-ordl.est-no  EQ b-eb.est-no                                                                      */
/*                AND oe-ordl.i-no    EQ b-eb.stock-no                                                                    */
/*                NO-ERROR.                                                                                               */
/*                                                                                                                        */
/*            PUT UNFORMATTED                                                                                             */
/*            "<C2><b>Order Qty: </b><C11>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.qty) ELSE "0") FORMAT "x(10)"       */
/*            "<C25><b>FG #: </b>"  b-eb.stock-no FORMAT "x(15)"                                                          */
/*            /*"<C60><b>xPrice:</b>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.price) ELSE "0")*/ SKIP                   */
/*            "<C2><b>Material: </b><C11>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.i-name) ELSE "") FORMAT "x(30)" SKIP */
/*            "<C11>" b-eb.part-dscr1 FORMAT "x(30)" SKIP                                                                 */
/*            "<C2><b>Estimate:</b> <C11>" (IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.est-no)) ELSE "") FORMAT "x(10)"*/
/*            "<C25><b>Quantity:</b>" (IF AVAIL b-eb THEN b-eb.bl-qty ELSE 0) FORMAT ">>>>>>9.99<<"                       */
/*            /*"<C60> <b>UOM:</b>" (IF AVAILABLE oe-ordl THEN STRING(oe-ordl.pr-uom) ELSE "")*/ SKIP                     */
/*            "<C1.5><FROM><C84><LINE><||3>" SKIP.                                                                        */
/*                                                                                                                        */
/*            IF LINE-COUNTER > 68 THEN do:                                                                               */
/*                PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .              */
/*                PAGE.                                                                                                   */
/*            END.                                                                                                        */
/*        END. /* FOR EACH b-eb*/                                                                                         */
/*       END. /* last bf-jobhdr.i-no*/                                                                                    */
        IF LAST(bf-jobhdr.i-no) THEN
            PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" . 
    END.
         
END. /*each bf-jobhdr*/
END.  /* print include page*/
      

END. /* first job-no */    
      
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

PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" . 



RELEASE xjob-hdr NO-ERROR.    


/* **********************  Internal Procedures  *********************** */


PROCEDURE pGetBlankQtys PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form and blank, return the yield and request quantity
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQtyYield AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQtyRequest AS INTEGER NO-UNDO.
    
    FOR FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ ipcCompany
        AND estCostHeader.jobID EQ ipcJobID
        AND estCostHeader.jobID2 EQ ipiJobID2,
        FIRST estCostBlank NO-LOCK
        WHERE estCostBlank.estCostHeaderID EQ estCostHeader.estCostHeaderID
        AND estCostBlank.formNo EQ ipiFormNo
        AND estCostBlank.blankNO EQ ipiBlankNo:
        ASSIGN 
            opiQtyYield   = estCostBlank.quantityYielded
            opiQtyRequest = estCostBlank.quantityRequired
            .
    END.

END PROCEDURE.

PROCEDURE pGetCaseItem PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE PARAMETER BUFFER ipbf-eb      FOR eb.
    DEFINE PARAMETER BUFFER ipbf-job-hdr FOR job-hdr.
    DEFINE OUTPUT PARAMETER opcSubUnitItemID AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubUnitSize AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubUnitCount AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcSubUnitsPerUnit AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job-mat FOR job-mat.
    DEFINE BUFFER bf-item FOR ITEM.
    
    IF AVAILABLE ipbf-eb AND AVAILABLE ipbf-job-hdr THEN 
    DO:
        ASSIGN 
            opcSubUnitItemID = ipbf-eb.cas-no
            opcSubUnitSize = STRING(ipbf-eb.cas-len,">9.9999") + " x " + STRING(ipbf-eb.cas-wid,">9.9999") + " x " + STRING(ipbf-eb.cas-dep,">9.9999")
            opcSubUnitCount = STRING(ipbf-eb.cas-cnt)
            opcSubUnitsPerUnit = STRING(ipbf-eb.cas-pal)
            .
        FIND FIRST bf-job-mat NO-LOCK 
            WHERE bf-job-mat.company EQ ipbf-job-hdr.company
            AND bf-job-mat.job EQ ipbf-job-hdr.job
            AND bf-job-mat.job-no EQ ipbf-job-hdr.job-no
            AND bf-job-mat.job-no2 EQ ipbf-job-hdr.job-no2
            AND bf-job-mat.frm EQ ipbf-eb.form-no
            AND bf-job-mat.blank-no EQ ipbf-eb.blank-no
            AND bf-job-mat.i-no EQ opcSubUnitItemID
            NO-ERROR.
        IF NOT AVAILABLE bf-job-mat THEN 
        DO:
            opcSubUnitItemID = "FPNC".
            FIND FIRST bf-item NO-LOCK 
                WHERE bf-item.company EQ ipbf-eb.company
                AND bf-item.i-no EQ opcSubUnitItemID
                NO-ERROR.
            IF AVAILABLE bf-item THEN 
                ASSIGN 
                    opcSubUnitSize = STRING(bf-item.case-l,">9.9999") + " x " + STRING(bf-item.case-w,">9.9999") + " x " + STRING(bf-item.case-d,">9.9999")
                    opcSubUnitCount = STRING(bf-item.box-case)
                    opcSubUnitsPerUnit = STRING(bf-item.case-pall)
                    .
        END.
    END.

    
END PROCEDURE.

PROCEDURE pGetFormQtys PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose: Given a form, return the total yield and request quantity for the job
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQtyTotalYield AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiQtyTotalRequest AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOut AS INTEGER NO-UNDO.
     
    FOR FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ ipcCompany
        AND estCostHeader.jobID EQ ipcJobID
        AND estCostHeader.jobID2 EQ ipiJobID2,
        EACH estCostBlank NO-LOCK
        WHERE estCostBlank.estCostHeaderID EQ estCostHeader.estCostHeaderID
        AND estCostBlank.formNo EQ ipiFormNo:
            ASSIGN 
            opiQtyTotalYield = opiQtyTotalYield + estCostBlank.quantityYielded
            opiQtyTotalRequest = opiQtyTotalRequest + estCostBlank.quantityRequired
            opiOut = opiOut + estCostBlank.numOut
            .
    END.

END PROCEDURE.

PROCEDURE pPrintHeader :
    /*------------------------------------------------------------------------------
      Purpose:     Print header
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    PUT
        "<FCalibri><B><C2>MCLEAN PACKAGING INC.<P10>"
        "<C30><B><P11>Factory Order: <P11>" (IF cRdOptionMclean EQ "M" THEN "Moorestown" ELSE "Nazareth") FORMAT "x(12)"
        "<C65>Report Date: "  TODAY /*"  PRINTED DATE:" TODAY*/  "</B><P10>" SKIP
        v-fill SKIP
        "<C2><B>Customer: <P10>" v-cust-name  "<P10>"
        "<B><C39>Delivery Date: "  (IF dtRelDate NE ? THEN STRING(dtRelDate) ELSE "")  " <C68>Order #: " TRIM(STRING(iOrderNo,">>>>>>9"))  SKIP
        "  " cLabelSetItem FORMAT "x(14)" cSetItemName FORMAT "x(15)"
        "<C68>Job #: " v-job-no SPACE(0) "-" SPACE(0) v-job-no2 FORMAT "99" SKIP
        "  " cLabelSetPart FORMAT "x(18)" cSetPartNo FORMAT "x(15)" SKIP
        v-fill SKIP
        "<R4><C40><P20>" cNewOrderValue FORMAT "x(13)" "</B><P10><R6.5>" SKIP .

END PROCEDURE.


PROCEDURE pPrintMiscItems :
    /*------------------------------------------------------------------------------
      Purpose:     Print header
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcEstimate AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipiForm AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipcMatTypes AS CHARACTER NO-UNDO.
        
    DEFINE BUFFER bf-eb FOR eb.
    DEFINE BUFFER bf-job-mat FOR job-mat.
    
    FOR EACH xjob-mat NO-LOCK
        WHERE xjob-mat.company EQ job-hdr.company
        AND xjob-mat.job     EQ job-hdr.job
        AND xjob-mat.job-no  EQ job-hdr.job-no
        AND xjob-mat.job-no2 EQ job-hdr.job-no2
        AND xjob-mat.frm     EQ ipiForm
        AND (xjob-mat.blank-no EQ ipiBlank OR xjob-mat.blank-no EQ 0) ,
        FIRST bf-item NO-LOCK
        WHERE bf-item.company EQ xjob-mat.company
        AND bf-item.i-no    EQ xjob-mat.rm-i-no  
        AND CAN-DO(ipcMatTypes, bf-item.mat-type)
        BREAK BY bf-item.i-no :
        
        IF LAST-OF(bf-item.i-no) THEN do:
            PUT 
                "<P10><C20><b>Code: </B>" STRING(bf-item.i-no)  
                "<C45><b>Desc: </b>" bf-item.i-name FORMAT "x(20)" SKIP.
            
        END.
    END.
       
        
        FIND FIRST bf-eb NO-LOCK 
            WHERE bf-eb.company EQ cocode
            AND bf-eb.est-no EQ ipcEstimate
            AND bf-eb.form-no EQ 0
            AND bf-eb.blank-no EQ 0
            NO-ERROR.
        IF AVAILABLE bf-eb AND bf-eb.divider NE "" THEN DO:
            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.company  EQ bf-eb.company
                AND bf-item.i-no     EQ bf-eb.divider
                NO-ERROR.
            FIND FIRST bf-job-mat NO-LOCK 
                WHERE bf-job-mat.company EQ bf-eb.company
                AND bf-job-mat.job EQ job-hdr.job
                AND bf-job-mat.job-no EQ job-hdr.job-no
                AND bf-job-mat.job-no2 EQ job-hdr.job-no2
                AND bf-job-mat.frm EQ 0
                AND bf-job-mat.blank-no EQ 0
                AND bf-job-mat.i-no EQ bf-eb.divider
                NO-ERROR.
            IF AVAILABLE bf-item AND NOT AVAILABLE bf-job-mat THEN  
                PUT
                    "<P10><C20><b>Code: </B>" STRING(bf-item.i-no)
                    "<C45><b>Desc: </b>" bf-item.i-name FORMAT "x(20)" SKIP.
        END.
        IF AVAILABLE bf-eb AND bf-eb.layer-pad NE "" THEN DO:
            FIND FIRST bf-item NO-LOCK
                WHERE bf-item.company  EQ bf-eb.company
                AND bf-item.i-no     EQ bf-eb.layer-pad
                NO-ERROR.
            FIND FIRST bf-job-mat NO-LOCK 
                WHERE bf-job-mat.company EQ bf-eb.company
                AND bf-job-mat.job EQ job-hdr.job
                AND bf-job-mat.job-no EQ job-hdr.job-no
                AND bf-job-mat.job-no2 EQ job-hdr.job-no2
                AND bf-job-mat.frm EQ 0
                AND bf-job-mat.blank-no EQ 0
                AND bf-job-mat.i-no EQ bf-eb.layer-pad
                NO-ERROR.    
            IF AVAILABLE bf-item AND NOT AVAILABLE bf-job-mat THEN 
                PUT
                    "<P10><C20><b>Code: </B>" STRING(bf-item.i-no)
                    "<C45><b>Desc: </b>" bf-item.i-name FORMAT "x(20)" SKIP.        
        END.
                                 
/*    IF ipiBlank EQ 1  THEN do:                                                                         */
/*        DO i = 1 TO 5: /*no room for all 6*/                                                           */
/*            IF ef.mis-cost[i] NE "" THEN DO:                                                           */
/*                FIND FIRST ITEM NO-LOCK                                                                */
/*                    WHERE item.company  EQ cocode                                                      */
/*                    AND item.i-no     EQ ef.mis-cost[i]                                                */
/*                    NO-ERROR.                                                                          */
/*                PUT   "<P10><C20><b>Code: </B>" STRING(ef.mis-cost[i])                                 */
/*                    "<C45><b>Desc: </b>" ( IF AVAIL ITEM THEN ITEM.i-name ELSE "") FORMAT "x(20)" SKIP.*/
/*            END.                                                                                       */
/*        END.                                                                                           */
/*        DO i = 1 TO 8:                                                                                 */
/*            IF ef.spec-no[i] <> "" THEN do:                                                            */
/*                PUT   "<P10><C20><b>Code: </B>" STRING(ef.spec-no[i])                                  */
/*                       "<C45><b>Desc: </b>" ef.spec-dscr FORMAT "x(20)" SKIP.                          */
/*            END.                                                                                       */
/*        END.                                                                                           */
/*    END.                                                                                               */
                

END PROCEDURE.


PROCEDURE pGetJobQty :
    /*------------------------------------------------------------------------------
      Purpose:     Print header
      Parameters:  <none>
      Notes:       
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcJobNo AS CHARACTER NO-UNDO .
    DEFINE INPUT PARAMETER ipiJobNo2 AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipiFornNo AS INTEGER NO-UNDO .
    DEFINE INPUT PARAMETER ipiBlankNo AS INTEGER NO-UNDO .
    DEFINE OUTPUT PARAMETER opiReturnQty AS INTEGER NO-UNDO .
    DEFINE BUFFER bff-job-hdr FOR job-hdr.
     
     FIND FIRST bff-job-hdr NO-LOCK
          WHERE bff-job-hdr.company EQ cocode 
            AND bff-job-hdr.job-no EQ ipcJobNo 
            AND bff-job-hdr.job-no2 EQ ipiJobNo2
            AND bff-job-hdr.frm EQ ipiFornNo
            AND bff-job-hdr.blank-no EQ ipiBlankNo NO-ERROR .
      opiReturnQty = IF AVAIL bff-job-hdr THEN bff-job-hdr.qty ELSE 0 .

END PROCEDURE.

PROCEDURE pPrintOperationsForForm PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiFormNo AS INTEGER NO-UNDO.

    
    PUT "<b><C2>Operation             <c20>R Crw.  <c27>R Hrs.    <c33>MR Crw.   <c40>MR Hrs.   <c48>Speed    <c53.5>Mr Wst.   <c61.5>R Wst.  <c68>Beginning   <c77>Yield </b>" SKIP
    "<||3><C1><FROM><C83><LINE><||3><R-1>" SKIP .

    IF LINE-COUNTER > 70 THEN 
    DO:
        PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
        PAGE.
        RUN pPrintHeader .
    END.

    FOR FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ ipcCompany
        AND estCostHeader.jobID EQ ipcJobID
        AND estCostHeader.jobID2 EQ ipiJobID2,
        EACH estCostOperation NO-LOCK
        WHERE estCostOperation.estCostHeaderID EQ estCostHeader.estCostHeaderID
        AND estCostOperation.formNo EQ ipiFormNo,
        FIRST job-mch NO-LOCK 
        WHERE job-mch.company EQ estCostOperation.company
        AND job-mch.job-no EQ estCostHeader.jobID
        AND job-mch.job-no2 EQ estCostHeader.jobID2
        AND job-mch.m-code EQ estCostOperation.operationID
        AND job-mch.frm EQ estCostOperation.formNo 
            BY estCostOperation.sequence:
        IF s-prt-mstandard THEN
            PUT "<C2>" estCostOperation.operationName   SPACE(1)
                "<C20>" estCostOperation.crewSizeRun FORMAT ">>>9.99"   
                "<C27>" estCostOperation.hoursRun FORMAT ">>>9.99"   
                "<C34>" estCostOperation.crewSizeSetup FORMAT ">>>9.99"   
                "<C41>" estCostOperation.hoursSetup FORMAT ">>>9.99"         
                "<C48>" estCostOperation.speed FORMAT ">>>>>>9"
                "<C55>" estCostOperation.quantityInSetupWaste  FORMAT ">>>>>>9" 
                "<C62>" estCostOperation.quantityInRunWaste FORMAT ">>>>>>9" 
                "<C69>" estCostOperation.quantityIn FORMAT ">>>>>>>9"   
                "<C76>" estCostOperation.quantityOut FORMAT ">>>>>>>9" SKIP.
        ELSE 
            PUT "<C2>" estCostOperation.operationName SPACE(3) SKIP .      
                                                 
        IF LINE-COUNTER > 70 THEN 
        DO:
            PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
            PAGE.
            RUN pPrintHeader .
        END.
    END.
    

END PROCEDURE.


PROCEDURE pPrintPlateImage PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE cFileExt AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cPlateImage AS CHARACTER NO-UNDO.
    
    /* print Plate image */
    IF print-box THEN 
    DO: 
        lv-cad-image-list = "".
        cImageBoxDesign = "".
        MAIN-PRINT-BOX:
        FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = ipcCompany
            AND bf-jobhdr.job-no = ipcJobID
            AND bf-jobhdr.job-no2 = ipiJobID2
            BREAK BY bf-jobhdr.frm
                  BY bf-jobhdr.blank-no:
           
            IF est.est-type NE 1 AND NOT CAN-FIND(FIRST ttSoule NO-LOCK
                            WHERE  ttSoule.frm EQ bf-jobhdr.frm
                             AND   ttSoule.runForm EQ YES 
                             AND   ttSoule.lPrintPlateImage) THEN NEXT MAIN-PRINT-BOX.

            IF FIRST-OF(bf-jobhdr.blank-no) THEN 
            DO:
                cImageBoxDesign = "".
                FIND FIRST b-eb WHERE b-eb.company = bf-jobhdr.company
                    AND b-eb.est-no = bf-jobhdr.est-no
                    AND b-eb.form-no = bf-jobhdr.frm 
                    AND b-eb.blank-no = bf-jobhdr.blank-no NO-LOCK NO-ERROR.
                FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                    AND sys-ctrl.NAME = "PLATEFILE" NO-LOCK NO-ERROR. 
                IF AVAILABLE b-eb AND b-eb.plate-no <> "" 
                    AND LOOKUP(b-eb.plate-no,lv-cad-image-list) <= 0
                    THEN 
                DO:                                         
                    IF AVAILABLE sys-ctrl AND sys-ctrl.int-fld EQ 0 THEN 
                    cFileExt = ".jpg".
                    ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.int-fld EQ 1 THEN 
                    cFileExt = ".bmp".
                    ELSE IF AVAILABLE sys-ctrl AND sys-ctrl.int-fld EQ 2 THEN 
                    cFileExt = ".pdf".
                    ELSE cFileExt = ".ard".
                    cPlateImage =  (IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "") + "\" +
                        b-eb.plate-no + cFileExt  .   
                    lv-cad-image-list = lv-cad-image-list + b-eb.cad-no + ",". 
                    PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                    PAGE.
                    RUN pPrintHeader .
                
                    PUT UNFORMATTED 
                        "<#12><C2><R8><FROM><C80><R+52><RECT><||3><C80>" /*v-qa-text*/ SKIP
                        "<=12><R+1><C5>Image: Plate        "    "Form No:" bf-jobhdr.frm FORMAT ">99"  
                        "<=12><R+2><C2><FROM><C80><LINE><||3>"
                        "<=12><R+3><C3><#21><R+46><C+76><IMAGE#21=" cPlateImage ">" SKIP. 
                END.
            END.
        END. /*each bf-jobhdr*/
    END.  /* print-box*/       

END PROCEDURE.

PROCEDURE pPrintCadImage PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcJobID AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiJobID2 AS INTEGER NO-UNDO.
    DEFINE VARIABLE cFileExt AS CHARACTER NO-UNDO.
        
    /* print Cad# image */
    IF print-box THEN 
    DO: 
        lv-cad-image-list = "".
        cImageBoxDesign = "".
        MAIN-PRINT-BOX:
        FOR EACH bf-jobhdr NO-LOCK WHERE bf-jobhdr.company = ipcCompany
            AND bf-jobhdr.job-no = ipcJobID
            AND bf-jobhdr.job-no2 = ipiJobID2
            BREAK BY bf-jobhdr.frm
                  BY bf-jobhdr.blank-no:
           
            IF est.est-type NE 1 AND NOT CAN-FIND(FIRST ttSoule NO-LOCK
                            WHERE  ttSoule.frm EQ bf-jobhdr.frm
                             AND   ttSoule.runForm EQ YES 
                             AND   ttSoule.lPrintCadImage) THEN NEXT MAIN-PRINT-BOX.

            IF FIRST-OF(bf-jobhdr.blank-no) THEN 
            DO:
                cImageBoxDesign = "".
                FIND FIRST b-eb WHERE b-eb.company = bf-jobhdr.company
                    AND b-eb.est-no = bf-jobhdr.est-no
                    AND b-eb.form-no = bf-jobhdr.frm 
                    AND b-eb.blank-no = bf-jobhdr.blank-no NO-LOCK NO-ERROR.
                FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                    AND sys-ctrl.NAME = "CADFILE" NO-LOCK NO-ERROR. 
                IF AVAILABLE b-eb AND b-eb.cad-no <> "" 
                    AND LOOKUP(b-eb.cad-no,lv-cad-image-list) <= 0
                    THEN 
                DO:                        
                    lv-cad-image =  (IF AVAILABLE sys-ctrl THEN sys-ctrl.char-fld ELSE "") + "\" +
                        b-eb.cad-no + ".jpg"  .    
                    lv-cad-image-list = lv-cad-image-list + b-eb.cad-no + ",". 
                    PUT "<C74><R64>Page: " string(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)" .
                    PAGE.
                    RUN pPrintHeader .
                
                    PUT UNFORMATTED 
                        "<#12><C2><R8><FROM><C80><R+52><RECT><||3><C80>" /*v-qa-text*/ SKIP
                        "<=12><R+1><C5>Image: Cad        "    "Form No:" bf-jobhdr.frm FORMAT ">99"  
                        "<=12><R+2><C2><FROM><C80><LINE><||3>"
                        "<=12><R+3><C3><#21><R+46><C+76><IMAGE#21=" lv-cad-image ">" SKIP. 
                END.
            END.
        END. /*each bf-jobhdr*/
    END.  /* print-box*/       

END PROCEDURE.

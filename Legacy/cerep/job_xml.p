/* ----------------------------------------------- cerep/jobcolnlp2.p   */
/*  factory ticket  for  corrugated ColonialPL                                 */
/* -------------------------------------------------------------------------- */
DEFINE VARIABLE cSide AS CHARACTER NO-UNDO.
{sys/inc/var.i shared}
{sys/form/s-top.f}

{jcrep/r-ticket.i "shared"}
{cerep\tt-samp-ctn.i}

DEFINE NEW SHARED VARIABLE save_id                  AS RECID.
DEFINE NEW SHARED VARIABLE v-today                  AS DATE      INITIAL TODAY.
DEFINE NEW SHARED VARIABLE v-job                    AS CHARACTER FORMAT "x(6)" EXTENT 2 INITIAL [" ","zzzzzz"].
DEFINE NEW SHARED VARIABLE v-job2                   AS INTEGER   FORMAT "99" EXTENT 2 INITIAL [00,99].
DEFINE NEW SHARED VARIABLE v-stypart                LIKE style.dscr.
DEFINE NEW SHARED VARIABLE v-dsc                    LIKE oe-ordl.part-dscr1 EXTENT 2.
DEFINE NEW SHARED VARIABLE v-size                   AS CHARACTER FORMAT "x(26)" EXTENT 2.
DEFINE NEW SHARED VARIABLE v-bld-job                LIKE oe-ord.job-no.
DEFINE NEW SHARED VARIABLE v-bld-job2               LIKE oe-ord.job-no2.
DEFINE NEW SHARED VARIABLE v-fill                   AS CHARACTER FORMAT "x(128)".
DEFINE NEW SHARED VARIABLE v-frst                   AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-ok                     AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-est-qty                AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-job-qty                AS INTEGER   FORMAT "->>,>>>,>>9".
DEFINE NEW SHARED VARIABLE v-fac                    AS DECIMAL .
DEFINE NEW SHARED VARIABLE v-job-no                 LIKE oe-ordl.job-no.
DEFINE NEW SHARED VARIABLE v-job-no2                LIKE oe-ordl.job-no2.
DEFINE NEW SHARED VARIABLE v-due-date               LIKE oe-ord.due-date.
DEFINE NEW SHARED VARIABLE v-reprint                AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-up                     LIKE eb.num-up.
DEFINE NEW SHARED VARIABLE v-tandem                 AS LOGICAL.
DEFINE NEW SHARED VARIABLE v-form-no                LIKE eb.form-no.
DEFINE NEW SHARED VARIABLE v-fup                    AS CHARACTER.
DEFINE NEW SHARED VARIABLE v-layout                 AS CHARACTER FORMAT "x(30)".
DEFINE            VARIABLE v-case-count             LIKE eb.cas-cnt NO-UNDO.
DEFINE            VARIABLE v-case-qty               AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-spc-no                 LIKE eb.spc-no NO-UNDO.
DEFINE            VARIABLE v-gsh-qty                AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-frm-blk                AS CHARACTER FORMAT "x(6)" NO-UNDO.
DEFINE            VARIABLE v-dec                    AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-ovund                  AS CHARACTER FORMAT "x(34)" NO-UNDO.
DEFINE            VARIABLE v-mrhr                   AS CHARACTER FORMAT "x(5)".
DEFINE            VARIABLE v-cas-dscr               LIKE item.est-dscr.
DEFINE            VARIABLE v-first                  AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE v-spec-list              AS CHARACTER FORMAT "x(20)"INITIAL "QA" NO-UNDO.
DEFINE            VARIABLE lv-form-note             AS CHARACTER       NO-UNDO.
DEFINE            VARIABLE v-itm-printed            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-prep                   AS CHARACTER       EXTENT 8 NO-UNDO.
DEFINE            VARIABLE v-misc                   AS CHARACTER       EXTENT 6 NO-UNDO.
DEFINE            VARIABLE v-spec-no                AS CHARACTER       EXTENT 8 NO-UNDO.
DEFINE            VARIABLE v-skip                   AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE v-fill2                  AS CHARACTER       INITIAL "-" FORM "x(125)" NO-UNDO.
DEFINE            VARIABLE lv-text                  AS CHARACTER NO-UNDO.
DEFINE            VARIABLE li                       AS INTEGER   NO-UNDO.
DEFINE            VARIABLE lv-under-run             AS CHARACTER NO-UNDO.
DEFINE            VARIABLE lv-over-run              AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-cust-name-extent       AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-ship1-extent           AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-ship2-extent           AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-ship4-extent           AS CHARACTER EXTENT 10 NO-UNDO.
DEFINE            VARIABLE v-po-no                  LIKE oe-ordl.po-no EXTENT 4 NO-UNDO.
DEFINE            VARIABLE v-unit-per-int           LIKE eb.cas-cnt NO-UNDO.
DEFINE            VARIABLE v-unit-per-dec           AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-job-qty-unit-per-int   AS INTEGER   FORMAT ">>>>>9" NO-UNDO.
DEFINE            VARIABLE v-job-qty-unit-per-dec   AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-dc-gl-speed            AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-job-qty-boxes-code-int AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-job-qty-boxes-code-dec AS DECIMAL   NO-UNDO.
DEFINE            VARIABLE v-dc-out                 LIKE est-op.n-out NO-UNDO.
DEFINE            VARIABLE v-dc-only-out            LIKE est-op.n-out NO-UNDO.
DEFINE            VARIABLE v-shink-wrap             AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE v-sample-on-cnt          AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE v-shrink-wrap            AS LOGICAL       NO-UNDO.
DEFINE            VARIABLE v-cas-wt                 AS DECIMAL   FORMAT ">>>>9.99" NO-UNDO.
DEFINE            VARIABLE v-cust-lot#              AS CHARACTER EXTENT 4 NO-UNDO.
DEFINE            VARIABLE v-per-ord                AS CHARACTER NO-UNDO.
DEFINE            VARIABLE v-upc-no                 LIKE eb.upc-no NO-UNDO.
DEFINE            VARIABLE v-pricnt-id              AS CHARACTER NO-UNDO .

DEFINE BUFFER b-est     FOR est.
DEFINE BUFFER b-oe-ordl FOR oe-ordl.
DEFINE BUFFER b-oe-rel  FOR oe-rel.
DEFINE BUFFER b-shipto  FOR shipto.
DEFINE BUFFER b-cust    FOR cust.
DEFINE BUFFER b-rt      FOR reftable.
DEFINE BUFFER ref-side  FOR reftable.

DEFINE TEMP-TABLE w-lo NO-UNDO
    FIELD layout LIKE v-layout.

DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.

DEFINE            BUFFER b-eb     FOR eb.

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
    FIELD run-hr LIKE job-mch.run-hr EXTENT 100
    FIELD num-sh LIKE est-op.num-sh EXTENT 100
    FIELD spoil LIKE job-mch.wst-prct EXTENT 20
    FIELD mr-waste LIKE job-mch.mr-waste EXTENT 20    .

DEFINE NEW SHARED WORKFILE wrk-die
    FIELD die-no LIKE eb.die-no
    FIELD cad-no LIKE eb.cad-no
    FIELD form-no LIKE eb.form-no
    FIELD die-size AS CHARACTER FORMAT "x(17)".

DEFINE NEW SHARED WORKFILE wrk-sheet
    FIELD gsh-qty AS INTEGER  FORMAT "->>>,>>>,>>9" /* gdm - 12180809*/
    FIELD cal LIKE ef.cal
    FIELD i-no LIKE ITEM.i-no
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
    FIELD i-unit AS INTEGER
    FIELD i-side AS CHARACTER
    FIELD i-code2 AS CHARACTER FORMAT "x(10)".

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
  
{custom/formtext.i NEW}     
{custom/notesdef.i}
DEFINE VARIABLE v-inst2          AS CHARACTER     EXTENT 25 NO-UNDO.    
DEFINE VARIABLE v-dept-inst      AS CHARACTER     FORMAT "x(80)" EXTENT 20 NO-UNDO.
DEFINE VARIABLE v-note-length    AS INTEGER INITIAL 80 NO-UNDO.

DEFINE VARIABLE v-start-date     AS DATE    NO-UNDO.
DEFINE VARIABLE v-req-date       AS DATE    NO-UNDO.
DEFINE VARIABLE v-shipto         AS CHARACTER     FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-case-size      AS CHARACTER     NO-UNDO.
DEFINE VARIABLE v-vend           LIKE po-ord.vend-no NO-UNDO.
DEFINE VARIABLE v-item           AS CHARACTER     EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-i-qty          AS DECIMAL       EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-ink1           AS CHARACTER     EXTENT 100 NO-UNDO.
DEFINE VARIABLE v-ink2           AS CHARACTER     EXTENT 100 NO-UNDO.
DEFINE VARIABLE lv-mat-dept-list AS CHARACTER     INITIAL "FB,FS,WN,WS,GL" NO-UNDO.
DEFINE VARIABLE v-mat-for-mach   AS CHARACTER     NO-UNDO.
DEFINE BUFFER xjob-mat   FOR job-mat. 
DEFINE BUFFER bf-job-mat FOR job-mat. 
DEFINE VARIABLE v-layer-qty    AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-cases-qty    AS DECIMAL NO-UNDO .
DEFINE VARIABLE v-fgitm        AS CHARACTER       FORMAT "x(15)" EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgdsc        LIKE eb.part-dscr1 EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-fgqty        LIKE job-hdr.qty   EXTENT 10 FORM ">>,>>>,>>>" NO-UNDO.
DEFINE VARIABLE v-pono         LIKE oe-ordl.po-no EXTENT 10 NO-UNDO.
DEFINE VARIABLE v-num-of-fgitm AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tt-fgitm 
    FIELD i-no      AS CHARACTER       FORMAT "x(15)"
    FIELD seq       AS INTEGER
    FIELD qty       AS INTEGER 
    FIELD i-dscr    AS CHARACTER
    FIELD po-no     AS CHARACTER
    FIELD cust-name AS CHARACTER
    FIELD shipto1   AS CHARACTER
    FIELD shipto2   AS CHARACTER
    FIELD shipto4   AS CHARACTER
    FIELD dOverPrct AS DECIMAL
    FIELD dUnderPrct AS DECIMAL
    FIELD cCustNo    AS CHARACTER
    FIELD cPart      AS CHARACTER
    FIELD iName      AS CHARACTER
    FIELD cEstNo     AS CHARACTER
    FIELD cLenWid    AS CHARACTER
    FIELD cStyle    AS CHARACTER
    FIELD cMisCost  AS CHARACTER
    FIELD cShipId  AS CHARACTER
    FIELD cCarrier  AS CHARACTER
    FIELD cChgMthd  AS CHARACTER
    FIELD cFrtCls  AS CHARACTER
    FIELD cClsDesc  AS CHARACTER
    FIELD cDefLoc  AS CHARACTER
    FIELD cContact  AS CHARACTER
    FIELD cAreaCode AS CHARACTER
    FIELD cDockHr AS CHARACTER
    FIELD dDueDt AS DATE
    FIELD dLstDt AS DATE.
    
DEFINE TEMP-TABLE ttTempJob
    FIELD company      AS CHARACTER
    FIELD jobID        AS CHARACTER
    FIELD newProject   AS INTEGER
    FIELD customerID   AS CHARACTER
    FIELD customerName AS CHARACTER 
    FIELD FGItemCode   AS CHARACTER
    FIELD FGName       AS CHARACTER
    FIELD CustPart     AS CHARACTER 
    FIELD ItemStatus   AS CHARACTER  
    FIELD itemRecKey   AS CHARACTER
    FIELD DateIssued   AS CHARACTER   
    FIELD ebWIDTH      AS CHARACTER 
    FIELD ebLENGTH     AS CHARACTER
    FIELD ebDepth      AS CHARACTER  
    FIELD FlatWidth    AS CHARACTER
    FIELD FlatLength   AS CHARACTER
    FIELD ColorsCoat   AS CHARACTER
    FIELD CCNumber     AS CHARACTER
    FIELD Weight       AS CHARACTER
    FIELD Caliper      AS CHARACTER
    FIELD Structure    AS CHARACTER
    FIELD Board        AS CHARACTER
    FIELD FgCategory   AS CHARACTER
    .
     
DEFINE VARIABLE v-board-po      LIKE oe-ordl.po-no-po NO-UNDO.
DEFINE VARIABLE v-plate-printed AS LOGICAL NO-UNDO.
DEFINE BUFFER xoe-ordl FOR oe-ordl.
DEFINE VARIABLE v-cust-name         LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name2        LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-cust-name3        LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-last-j            AS INTEGER   NO-UNDO.
DEFINE VARIABLE v-po-no2            LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-po-no3            LIKE v-po-no NO-UNDO.
DEFINE VARIABLE v-lbs               AS DECIMAL   FORM ">>,>>>,>>9" NO-UNDO.
DEFINE VARIABLE v-dept-title        AS cha       NO-UNDO.
DEFINE VARIABLE v-dept-note-printed AS LOGICAL.
/* aj */
DEFINE VARIABLE v-ship-date         AS DATE      EXTENT 4 NO-UNDO.
DEFINE VARIABLE v-due-qty           LIKE oe-rel.tot-qty EXTENT 4 NO-UNDO.
DEFINE VARIABLE icount              AS INTEGER   INITIAL 0 NO-UNDO.
DEFINE VARIABLE v-max-qty           AS INTEGER   NO-UNDO .
DEFINE VARIABLE v-min-qty           AS INTEGER   NO-UNDO .
DEFINE VARIABLE v-reprun            AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-brd-code          AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-item-desc         AS CHARACTER NO-UNDO.
DEFINE VARIABLE v-weight            LIKE job-mat.rm-i-no NO-UNDO.
DEFINE VARIABLE v-width             LIKE job-mat.basis-w NO-UNDO.
DEFINE VARIABLE v-lenght            AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-print-qty         AS DECIMAL   NO-UNDO.
DEFINE VARIABLE v-print-feet        AS DECIMAL   FORMAT "->>>,>>>,>>9.99" NO-UNDO.

DEFINE VARIABLE v-fill3                   AS CHARACTER FORMAT "x(128)".

v-fill = "<||3><C2><FROM><C108><LINE><||3>".
v-fill3 = "<||3><C2><FROM><C62><LINE><||3>".

DEFINE NEW SHARED FRAME head.

DEFINE SHARED VARIABLE s-prt-mstandard AS LOGICAL       NO-UNDO.
DEFINE SHARED VARIABLE s-prt-shipto    AS LOGICAL       NO-UNDO.
DEFINE SHARED VARIABLE s-prt-sellprc   AS LOGICAL       NO-UNDO.
DEFINE        VARIABLE v-po-duedate    LIKE po-ordl.due-date NO-UNDO.
DEFINE        VARIABLE v-upc-lbl       AS CHARACTER       FORMAT "x(10)" NO-UNDO.
DEFINE        VARIABLE v-shipto1       AS CHARACTER       FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE        VARIABLE v-shipto2       AS CHARACTER       FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE SHARED VARIABLE s-run-speed     AS LOGICAL       NO-UNDO.
DEFINE        VARIABLE v-pass-count    AS INTEGER       NO-UNDO.
DEFINE SHARED VARIABLE s-prt-label     AS LOGICAL       NO-UNDO.
DEFINE        VARIABLE v-boardcode     LIKE job-mat.rm-i-no NO-UNDO.  
DEFINE        VARIABLE v-length        LIKE job-mat.len NO-UNDO.
DEFINE        VARIABLE v-upnew         AS CHARACTER      NO-UNDO.
DEFINE        VARIABLE v-lp-dep        AS DECIMAL       NO-UNDO.
DEFINE        VARIABLE v-lp-qty        AS INTEGER       NO-UNDO.
DEFINE        VARIABLE v-mr-hours      AS DECIMAL       NO-UNDO.
DEFINE        VARIABLE iSpeed     AS INTEGER       NO-UNDO.

DEFINE        VARIABLE cDraftImage     AS CHARACTER     NO-UNDO.
DEFINE        VARIABLE cDraftImageFull AS CHARACTER     FORMAT "x(50)" NO-UNDO.
DEFINE        VARIABLE cJobNo          AS CHARACTER     NO-UNDO.

DEFINE        VARIABLE iOrder          AS INTEGER NO-UNDO.
DEFINE VARIABLE cProCat LIKE itemfg.procat NO-UNDO.
DEFINE VARIABLE cTypeCode AS CHARACTER NO-UNDO.
DEFINE VARIABLE iMrWaste AS INTEGER NO-UNDO.
DEFINE VARIABLE dMrHour LIKE job-mch.mr-hr NO-UNDO.
DEFINE VARIABLE cPoItemName AS CHARACTER NO-UNDO.
DEFINE VARIABLE dWstPrct LIKE job-mch.wst-prct NO-UNDO.
DEFINE VARIABLE cShpDoc AS CHARACTER NO-UNDO.
DEFINE VARIABLE dQtyTray AS DECIMAL NO-UNDO.
DEFINE VARIABLE cSize    AS CHARACTER NO-UNDO.
DEFINE VARIABLE dDueDate    LIKE oe-ord.due-date NO-UNDO.
DEFINE VARIABLE dLastDate LIKE oe-ord.last-date NO-UNDO.
DEFINE VARIABLE cFrtCls LIKE itemfg.frt-class NO-UNDO.
DEFINE VARIABLE cFrtClsDscr LIKE itemfg.frt-class-dscr NO-UNDO.
DEFINE VARIABLE cDefLoc LIKE itemfg.def-loc NO-UNDO.
DEFINE VARIABLE cShipDocHour LIKE shipto.dock-hour NO-UNDO.
DEFINE VARIABLE cDockAptmnt AS CHARACTER NO-UNDO.
DEFINE VARIABLE cContact LIKE cust.contact NO-UNDO.
DEFINE VARIABLE iCountLine AS INTEGER INITIAL 0 NO-UNDO.
DEFINE BUFFER b-ef FOR ef.
DEF VAR lv-cad-image AS cha NO-UNDO.
DEF VAR lv-cad-image-list AS cha NO-UNDO.
DEFINE VARIABLE lRecFound AS LOG NO-UNDO.
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE cXMlFinalDest AS CHARACTER NO-UNDO.

RUN sys/ref/nk1look.p (INPUT cocode, "XMLJobTicket", "C" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
    OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
   cXMLFinalDest  = cRtnChar NO-ERROR.
 /* temp table populated with UDF data */   
{UDF/ttUDF.i}                

/* function to get UDF Group */
{UDF/fUDFGroup.i "itemfg."}                         
/* ********************  Preprocessor Definitions  ******************** */

/* ************************  Function Prototypes ********************** */


FUNCTION fnStripInvalidChar RETURNS CHARACTER 
    (ipcInput AS CHARACTER  ) FORWARD.




/* ************************  Function Implementations ***************** */


FUNCTION fnStripInvalidChar RETURNS CHARACTER 
    (ipcInput AS CHARACTER  ):
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/    

    DEFINE VARIABLE cResult       AS CHARACTER NO-UNDO.
    DEFINE VARIABLE iPos          AS INTEGER   NO-UNDO. 
    DEFINE VARIABLE lBadCharFound AS LOGICAL   NO-UNDO.  
    DEFINE VARIABLE iNumTries     AS INTEGER   NO-UNDO.  

    iNumTries = 0.   

    DO WHILE TRUE:   
        lBadCharFound = FALSE.
        
        iPos = INDEX(ipcInput, '"'). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, '"', '&quot;').            
        END.  
        
        iPos = INDEX(ipcInput, "'"). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, "'", '&apos;').            
        END.  
        
        iPos = INDEX(ipcInput, "<"). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, "<", '&lt;').            
        END. 
        
        iPos = INDEX(ipcInput, ">"). 
        IF iPos GT 0 THEN 
        DO:  
            lBadCharFound = TRUE. 
            ipcInput = REPLACE(ipcInput, ">", '&gt;').            
        END. 
        iNumTries = iNumTries + 1.
        IF lBadCharFound EQ FALSE OR iNumTries GT 200 THEN
            LEAVE.  
    END. 
    cResult = ipcInput.
    RETURN cResult.
        
END FUNCTION.
   
   
/* ***************************  Main Block  *************************** */   
{XMLOutput/XMLOutput.i &XMLOutput=XMLJobTicket &Company=cocode} /* rstark 05181205 */

ASSIGN
             lXMLOutput = YES
            XMLFile = "C:\Temp\JobTicketxml" + STRING(TIME,'99999') + '.xml'
            XMLTemp =  XMLFile
            .

OUTPUT STREAM XMLOutput TO VALUE(XMLTemp).
RUN XMLOutput (lXMLOutput,'','','Header'). /* rstark 05181205 */

ASSIGN 
    cDraftImage         = "images\draft.jpg"

    FILE-INFO:FILE-NAME = cDraftImage.
    cDraftImageFull = IF lDraft 
    THEN  "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">" 
    ELSE "".


    iCountLine = iCountLine + 3.

{sys/inc/notes.i}

FIND FIRST oe-ctrl WHERE oe-ctrl.company EQ cocode NO-LOCK NO-ERROR.

ASSIGN
    v-reprint   = reprint
    v-spec-list = spec-list.

FOR EACH job-hdr NO-LOCK
        WHERE job-hdr.company               EQ cocode
          AND job-hdr.job-no                GE SUBSTRING(fjob-no,1,6)
          AND job-hdr.job-no                LE SUBSTRING(tjob-no,1,6)
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  GE fjob-no
          AND FILL(" ",6 - LENGTH(TRIM(job-hdr.job-no))) +
              TRIM(job-hdr.job-no) +
              STRING(job-hdr.job-no2,"99")  LE tjob-no
          AND (production OR
               job-hdr.ftick-prnt           EQ v-reprint OR
               PROGRAM-NAME(2) MATCHES "*r-tickt2*")
    AND CAN-FIND(FIRST job WHERE job.company EQ cocode
    AND job.job     EQ job-hdr.job
    AND job.job-no  EQ job-hdr.job-no
    AND job.job-no2 EQ job-hdr.job-no2
    /*and job.stat    ne "H"*/
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

   
    FIND FIRST job NO-LOCK
        WHERE job.company EQ cocode
        AND job.job       EQ job-hdr.job
        AND job.job-no    EQ job-hdr.job-no
        AND job.job-no2   EQ job-hdr.job-no2
        NO-ERROR.

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
                ASSIGN
                    xjob-hdr.ftick-prnt = YES
                    li                  = 1000.
        END.
        IF AVAILABLE job AND job.stat EQ "H" THEN 
        DO:
            ASSIGN 
                cDraftImage         = "images\on-hold.jpg"
                FILE-INFO:FILE-NAME = cDraftImage.
            cDraftImageFull = "<C25><#1><R+80><C+50><IMAGE#1=" + FILE-INFO:FULL-PATHNAME + ">"  .
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
    FIND FIRST oe-ord NO-LOCK WHERE oe-ord.company EQ job-hdr.company
                                AND oe-ord.ord-no  EQ job-hdr.ord-no NO-ERROR.

    IF FIRST-OF(job-hdr.frm) THEN v-first = YES.

    /** PRINT JOB HEADER **/
    IF v-first THEN 
    DO:
        ASSIGN
            v-job-no  = job-hdr.job-no
            v-job-no2 = job-hdr.job-no2
            cJobNo    = v-job-no + "-" + STRING(v-job-no2,"99")
            iOrder = job-hdr.ord-no.

        IF AVAILABLE oe-ord THEN
            IF NOT oe-ctrl.p-fact AND (oe-ord.stat EQ "H" OR oe-ord.priceHold) THEN NEXT.

        FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company AND
                                      cust.cust-no EQ job-hdr.cust-no NO-ERROR.

        IF AVAILABLE cust THEN 
        DO:
            ASSIGN 
                v-pricnt-id = "" . 
            FOR EACH empalert NO-LOCK WHERE empalert.table_rec_key = cust.rec_key,
                FIRST users NO-LOCK WHERE users.user_id = empalert.USER-ID:

                IF empalert.spare-char-1 EQ "YES" THEN 
                DO:
                    ASSIGN 
                        v-pricnt-id = users.USER_id .
                    LEAVE.
                END.
            END.
        END.   

        ASSIGN
            dDueDate = if avail oe-ord then oe-ord.due-date else ?
            v-start-date = job-hdr.start-date
            dLastDate = if avail oe-ord then oe-ord.last-date else ?.

        IF NOT FIRST(job-hdr.job-no) THEN PAGE.
        
        v-shipto = "".
        FIND FIRST oe-ordl NO-LOCK
             WHERE oe-ordl.company EQ job-hdr.company
               AND oe-ordl.ord-no  EQ job-hdr.ord-no
               AND oe-ordl.job-no  EQ job-hdr.job-no
               AND oe-ordl.job-no2 EQ job-hdr.job-no2
               AND oe-ordl.i-no    EQ job-hdr.i-no
             NO-ERROR.
        IF AVAILABLE oe-ordl THEN 
            FIND FIRST oe-rel NO-LOCK
                WHERE oe-rel.company EQ cocode
                  AND oe-rel.ord-no  EQ oe-ordl.ord-no
                  AND oe-rel.i-no    EQ oe-ordl.i-no
                  AND oe-rel.line    EQ oe-ordl.line
                NO-ERROR.
        ASSIGN 
            v-reprun = IF AVAILABLE oe-ordl AND oe-ordl.type-code = "R" THEN "RETURN" 
                          ELSE "NEW" .
        FIND FIRST eb NO-LOCK WHERE eb.company     EQ job-hdr.company
            AND eb.est-no      EQ job-hdr.est-no
            AND eb.form-no     EQ job-hdr.frm
            AND eb.stock-no = job-hdr.i-no  NO-ERROR.
        IF NOT AVAILABLE eb THEN FIND FIRST eb NO-LOCK WHERE eb.company     EQ job-hdr.company
            AND eb.est-no      EQ job-hdr.est-no
            AND eb.form-no     EQ job-hdr.frm
            AND eb.blank-no    GT 0 NO-ERROR.
        v-spc-no = IF AVAILABLE eb THEN eb.spc-no ELSE "".
        v-upc-no = IF AVAILABLE eb THEN eb.upc-no ELSE "".
        cSize = STRING(eb.len) + "       x       " + STRING(eb.wid) + "       x       " +
                         STRING(eb.dep) .

       FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
           AND itemfg.i-no    EQ job-hdr.i-no NO-ERROR.
       ASSIGN cProCat = itemfg.procat
              cTypeCode = itemfg.i-no
              cFrtCls = itemfg.frt-class
               cFrtClsDscr = itemfg.frt-class-dscr
               cDefLoc = itemfg.def-loc.

       FIND FIRST e-itemfg-vend NO-LOCK
            WHERE e-itemfg-vend.company EQ itemfg.company
              AND e-itemfg-vend.i-no    EQ itemfg.i-no NO-ERROR.
       
       FIND FIRST ef NO-LOCK
           WHERE ef.company    EQ job-hdr.company
            AND ef.est-no      EQ job-hdr.est-no
            AND ef.form-no     EQ job-hdr.frm NO-ERROR.
       IF AVAILABLE ef THEN DO:
           j = 1.
           DO i = 1 TO 6:
               IF ef.mis-cost[i] <> "" THEN
                   ASSIGN  v-misc[j] = ef.mis-cost[i].
                   j = j + 1.                          
           END.
           j = 1.
       END.
      
      RUN ipGenJobTempTable (INPUT ROWID(job)). 
      RUN ipOutputJDFTop. /* start with JDF tag */ 
      
      /* This section falls within the JDF tag */
      /* VIEW FRAME head.*/
      RUN XMLOutput (lXMLOutput,'JobTicketHeader','','Row').
      RUN XMLOutput (lXMLOutput,'Job',v-job-no,'Col').
      RUN XMLOutput (lXMLOutput,'Job2',v-job-no2,'Col').
      RUN XMLOutput (lXMLOutput,'Order',iOrder,'Col').
      RUN XMLOutput (lXMLOutput,'NUMBER_UP',v-upnew,'Col').
      RUN XMLOutput (lXMLOutput,'QC_SPC',v-spc-no,'Col').
      RUN XMLOutput (lXMLOutput,'Category',cProCat,'Col').
      RUN XMLOutput (lXMLOutput,'/JobTicketHeader','','Row').
      
      
        IF AVAILABLE oe-rel THEN 
        DO:
            v-po-no = oe-rel.po-no .
            v-cust-lot# = oe-rel.lot-no.

            FIND FIRST shipto NO-LOCK
                WHERE shipto.company EQ cocode
                  AND shipto.cust-no EQ oe-rel.cust-no
                  AND shipto.ship-id EQ oe-rel.ship-id
                NO-ERROR.  
            IF AVAILABLE shipto THEN
                ASSIGN v-shipto[1] = shipto.ship-name
                    v-shipto[2] = shipto.ship-addr[1]
                    v-shipto[3] = shipto.ship-addr[2]
                    v-shipto[4] = TRIM(oe-rel.ship-city) + ", " +
                                   oe-rel.ship-state + "  " + oe-rel.ship-zip
                    cShpDoc = shipto.dock-loc
                    cShipDocHour = shipto.dock-hour.

        END.
        /* aj */
        icount = 0.
        FOR EACH b-oe-rel NO-LOCK WHERE  b-oe-rel.company EQ cocode
                                    AND b-oe-rel.ord-no  EQ oe-rel.ord-no
                                    AND b-oe-rel.i-no    EQ oe-rel.i-no
                                    AND b-oe-rel.line    EQ oe-rel.LINE :

            FIND FIRST reftable NO-LOCK WHERE
                       reftable.reftable EQ "oe-rel.lot-no" AND
                       reftable.company  EQ STRING(b-oe-rel.r-no,"9999999999")
                       NO-ERROR.
           
            icount =  icount + 1 .
            IF icount = 1 THEN  
                ASSIGN 
                    v-ship-date[1] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[1]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[1]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE ""
                    v-cust-lot#[1] = IF AVAILABLE reftable THEN reftable.CODE ELSE "" .
                      
            IF icount = 2 THEN  
                ASSIGN 
                    v-ship-date[2] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[2]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[2]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE "" 
                    v-cust-lot#[2] = IF AVAILABLE reftable THEN reftable.CODE ELSE ""   .

            IF icount = 3 THEN  
                ASSIGN 
                    v-ship-date[3] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ? 
                    v-due-qty[3]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[3]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE ""   
                    v-cust-lot#[3] = IF AVAILABLE reftable THEN reftable.CODE ELSE "" .

            IF icount = 4 THEN  
                ASSIGN 
                    v-ship-date[4] = IF b-oe-rel.rel-date NE ?  THEN  b-oe-rel.rel-date ELSE ?  
                    v-due-qty[4]   = IF b-oe-rel.tot-qty NE 0 THEN b-oe-rel.tot-qty ELSE 0 
                    v-po-no[4]     = IF b-oe-rel.po-no NE "" THEN  b-oe-rel.po-no ELSE ""   
                    v-cust-lot#[4] = IF AVAILABLE reftable THEN reftable.CODE ELSE "" . 
            
        END. /* FOR EACH */
        
        FIND FIRST cust NO-LOCK WHERE cust.company EQ job-hdr.company AND
                                      cust.cust-no EQ job-hdr.cust-no  NO-ERROR.
        FIND FIRST bank WHERE bank.company EQ cust.company NO-LOCK NO-ERROR.
        
        

        ASSIGN
            v-req-date   = IF AVAILABLE oe-ordl THEN oe-ordl.req-date ELSE ?
            v-cust-name  = IF AVAILABLE oe-ord THEN oe-ord.cust-name 
                         ELSE IF AVAILABLE cust THEN cust.name
                         ELSE job-hdr.cust-no
            lv-over-run  = IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.over-pct,">>9.99")) ELSE
                         IF AVAILABLE oe-ord  THEN TRIM(STRING(oe-ord.over-pct,">>9.99"))  ELSE ""
            lv-under-run = IF AVAILABLE oe-ordl THEN TRIM(STRING(oe-ordl.under-pct,">>9.99")) ELSE
                          IF AVAILABLE oe-ord  THEN TRIM(STRING(oe-ord.under-pct,">>9.99"))  ELSE ""
            v-due-date   = IF AVAILABLE oe-ordl THEN oe-ordl.prom-date ELSE ? 
            cDockAptmnt = STRING(cust.area-code,"(999)") + "        " + STRING(bank.phone,"999-9999")
            cContact = cust.contact  
            v-max-qty =  INTEGER ( oe-ordl.qty + oe-ordl.qty * (DECIMAL(lv-over-run) / 100) )
            v-min-qty =  INTEGER ( oe-ordl.qty - oe-ordl.qty * (DECIMAL(lv-under-run) / 100)) .
        IF AVAILABLE oe-ord THEN
            v-per-ord   = IF oe-ord.po-no2 NE "" THEN oe-ord.po-no2 ELSE STRING(oe-ord.pord-no) .
        IF AVAILABLE oe-ord AND oe-ord.TYPE EQ "T" AND oe-ord.pord-no GT 0 THEN
            v-per-ord = STRING(oe-ord.pord-no).
       iCountLine = iCountLine + 7.
       RUN XMLOutput (lXMLOutput,'TicketPrinting','','Row'). 
       RUN XMLOutput (lXMLOutput,'Job_Item',job-hdr.i-no,'Col').
       RUN XMLOutput (lXMLOutput,'max_qty',v-max-qty,'Col').
       RUN XMLOutput (lXMLOutput,'job_qty',job-hdr.qty,'Col').
       RUN XMLOutput (lXMLOutput,'min_qty',v-min-qty,'Col').
       RUN XMLOutput (lXMLOutput,'OverRun',lv-over-run,'Col').
       RUN XMLOutput (lXMLOutput,'UnderRun',lv-under-run,'Col').
       RUN XMLOutput (lXMLOutput,'Account_code',job-hdr.cust-no,'Col').
       RUN XMLOutput (lXMLOutput,'Cust_item',IF AVAILABLE eb THEN eb.part-no ELSE "",'Col').

       RUN XMLOutput (lXMLOutput,'Description',itemfg.i-name,'Col').
       RUN XMLOutput (lXMLOutput,'Customer',job-hdr.cust-no,'Col').
       RUN XMLOutput (lXMLOutput,'Purchase_order',IF AVAILABLE oe-ordl THEN oe-ordl.po-no ELSE "",'Col').
       RUN XMLOutput (lXMLOutput,'Estimate',job.est-no,'Col').
       RUN XMLOutput (lXMLOutput,'Finished_size',cSize,'Col').
       RUN XMLOutput (lXMLOutput,'Style',IF AVAILABLE eb THEN eb.style ELSE "",'Col').
       RUN XMLOutput (lXMLOutput,'vend_no',IF AVAILABLE e-itemfg-vend THEN e-itemfg-vend.vend-no ELSE "",'Col').
       RUN XMLOutput (lXMLOutput,'vendor_item',IF AVAILABLE e-itemfg-vend THEN e-itemfg-vend.vend-item ELSE "",'Col').

       DO i = 1 TO 4:
        IF i = 1 THEN
        RUN XMLOutput (lXMLOutput,'cost',v-misc[1],'Col').
        IF i = 2 THEN
        RUN XMLOutput (lXMLOutput,'cost',v-misc[2],'Col').
        IF i = 3 THEN
        RUN XMLOutput (lXMLOutput,'cost',v-misc[3],'Col').
        IF i = 4 THEN
        RUN XMLOutput (lXMLOutput,'cost',v-misc[4],'Col').
       END.
       RUN XMLOutput (lXMLOutput,'/TicketPrinting','','Row').

       RUN XMLOutput (lXMLOutput,'Graphics','','Row').
       RUN XMLOutput (lXMLOutput,'FGItem',cTypeCode,'Col').
       RUN XMLOutput (lXMLOutput,'Structure_Number',eb.cad-no,'Col').
       RUN XMLOutput (lXMLOutput,'Artwork',eb.plate-no,'Col').
       RUN XMLOutput (lXMLOutput,'BarCode',eb.upc-no,'Col').
       RUN XMLOutput (lXMLOutput,'Lastship_order',v-per-ord,'Col').
       RUN XMLOutput (lXMLOutput,'/Graphics','','Row').

       RUN XMLOutput (lXMLOutput,'StructuralDesign','','Row').
       RUN XMLOutput (lXMLOutput,'FG_Item',cTypeCode,'Col').
       RUN XMLOutput (lXMLOutput,'Die_Size',STRING(ef.trim-w) + "x" + STRING(ef.trim-l),'Col').
       RUN XMLOutput (lXMLOutput,'Style',eb.style,'Col').
       RUN XMLOutput (lXMLOutput,'Die',eb.die-no,'Col').
       RUN XMLOutput (lXMLOutput,'Blank_size',string(eb.t-wid) + "   X   " + string(eb.t-len),'Col').
       RUN XMLOutput (lXMLOutput,'/StructuralDesign','','Row').
       
        /** SUM UP NUMBER OF SHEETS **/
        FIND FIRST job NO-LOCK
            WHERE job.company EQ cocode
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ v-job-no
              AND job.job-no2 EQ v-job-no2
            NO-ERROR.
            
        IF AVAILABLE job THEN
            FOR EACH job-mch NO-LOCK
               WHERE job-mch.company EQ cocode
                 AND job-mch.job     EQ job.job
                 AND job-mch.job-no  EQ job.job-no
                 AND job-mch.job-no2 EQ job.job-no2
                 AND job-mch.frm     EQ job-hdr.frm ,

                FIRST mach
                {sys/ref/machW.i}
              AND mach.m-code EQ job-mch.m-code
            NO-LOCK

            BY mach.d-seq
            BY job-mch.frm
            BY job-mch.blank-no
            BY job-mch.pass
            BY job-mch.run-qty DESC:

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
            wrk-op.mr[job-mch.frm]       = job-mch.mr-hr
            wrk-op.speed[job-mch.frm]    = job-mch.speed
            wrk-op.num-sh[job-mch.frm]   = job-mch.run-qty
            wrk-op.spoil[job-mch.frm]    = job-mch.wst-prct   
            wrk-op.mr-waste[job-mch.frm] = job-mch.mr-waste  
            wrk-op.run-hr[job-mch.frm]   = job-mch.run-hr    .
    END.

    /** BUILD PREP WORK FILE **/
    FOR EACH job-prep NO-LOCK
       WHERE job-prep.company EQ cocode
         AND job-prep.job     EQ job-hdr.job
         AND job-prep.job-no  EQ job-hdr.job-no
         AND job-prep.job-no2 EQ job-hdr.job-no2:
        FIND FIRST prep NO-LOCK
            WHERE prep.company EQ cocode
            AND prep.code    EQ job-prep.code
            NO-ERROR.
        CREATE wrk-prep.
        ASSIGN
            wrk-prep.code  = job-prep.code
            wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
            wrk-prep.s-num = job-prep.frm
            wrk-prep.b-num = job-prep.blank-no
            wrk-prep.ml    = job-prep.ml.
    END. /* each job-prep */

    IF AVAILABLE est THEN
        FOR EACH est-prep NO-LOCK
            WHERE est-prep.company EQ est.company
              AND est-prep.est-no  EQ est.est-no
              AND index("SON",est-prep.simon) GT 0 :
            FIND FIRST prep NO-LOCK
                WHERE prep.company EQ cocode
                  AND prep.code    EQ est-prep.code
                NO-ERROR.
            CREATE wrk-prep.
            ASSIGN
                wrk-prep.code  = est-prep.code
                wrk-prep.dscr  = IF AVAILABLE prep THEN prep.dscr ELSE ""
                wrk-prep.s-num = est-prep.s-num
                wrk-prep.b-num = est-prep.b-num
                wrk-prep.ml    = est-prep.ml.
        END.

    IF AVAILABLE oe-ord THEN
        FOR EACH oe-ordm NO-LOCK 
            WHERE oe-ordm.company EQ cocode
              AND oe-ordm.ord-no  EQ oe-ord.ord-no :
            FIND FIRST wrk-prep WHERE wrk-prep.code EQ oe-ordm.charge NO-ERROR.
            IF NOT AVAILABLE wrk-prep THEN 
            DO:
                FIND FIRST prep NO-LOCK
                    WHERE prep.company EQ cocode
                      AND prep.code    EQ oe-ordm.charge
                    NO-ERROR.
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
          AND ef.est-no  EQ job-hdr.est-no
          AND ef.form-no EQ job-hdr.frm
          BREAK BY ef.est-no BY ef.form-no:
          

        v-job-qty = 0.
        FOR EACH xjob-hdr FIELDS(qty) NO-LOCK
            WHERE xjob-hdr.company EQ cocode
              AND xjob-hdr.job     EQ job-hdr.job
              AND xjob-hdr.job-no  EQ job-hdr.job-no
              AND xjob-hdr.job-no2 EQ job-hdr.job-no2
              AND xjob-hdr.i-no    EQ job-hdr.i-no :
              v-job-qty = v-job-qty + xjob-hdr.qty.
        END.
          
        v-est-qty = 0.
        IF est.est-type EQ 4 THEN
            FOR EACH eb NO-LOCK
                WHERE eb.company  EQ ef.company
                  AND eb.est-no   EQ ef.est-no
                  AND eb.stock-no EQ job-hdr.i-no:
                  v-est-qty = v-est-qty + eb.yld-qty.
            END.

        ELSE v-fac = 1.
        v-itm-printed = 0.

        IF ef.form-no EQ job-hdr.frm THEN 
            ebloop:
            FOR EACH eb NO-LOCK
                WHERE eb.company     EQ ef.company
                  AND eb.est-no      EQ ef.est-no
                  AND eb.form-no     EQ ef.form-no
                
                  BREAK BY eb.form-no BY eb.blank-no.

                CREATE w-lo.
                FOR EACH b-eb NO-LOCK
                    WHERE b-eb.company EQ eb.company
                      AND b-eb.est-no  EQ eb.est-no
                      AND b-eb.part-no EQ eb.part-no
                      BREAK BY b-eb.est-no:
                      v-fup = "F" + TRIM(STRING(b-eb.form-no,">>9")) + "-" +
                        TRIM(STRING(b-eb.blank-no,"99")) + "/" +
                        TRIM(STRING(b-eb.num-up,">>9")) + "up".
                    IF LENGTH(TRIM(v-fup)) + LENGTH(TRIM(w-lo.layout)) GT 30 THEN 
                    DO:
                        SUBSTRING(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                        CREATE w-lo.
                    END.
                    w-lo.layout = TRIM(w-lo.layout + " " + trim(v-fup) + ",").
                    IF LAST(b-eb.est-no) THEN
                        SUBSTRING(w-lo.layout,LENGTH(TRIM(w-lo.layout)),1) = "".
                END.
          
                FIND FIRST wrk-die WHERE wrk-die.die-no EQ eb.die-no NO-ERROR.
                IF NOT AVAILABLE wrk-die AND eb.die-no GT "" THEN 
                DO:
                    CREATE wrk-die.
                    ASSIGN 
                        wrk-die.die-no   = eb.die-no
                        wrk-die.cad-no   = eb.cad-no
                        wrk-die.form-no  = eb.form-no
                        wrk-die.die-size = STRING(ef.trim-w) + "x" +
              STRING(ef.trim-l).
                END.

                /** BUILD INK WORK FILE **/
                FIND FIRST reftable NO-LOCK WHERE 
                           reftable.reftable EQ "ce/v-est3.w Unit#" AND
                           reftable.company EQ eb.company AND
                           reftable.loc     EQ eb.est-no AND
                           reftable.code    EQ STRING(eb.form-no,"9999999999") AND
                           reftable.code2   EQ STRING(eb.blank-no,"9999999999")
                           NO-ERROR.

                FIND FIRST b-rt NO-LOCK WHERE
                           b-rt.reftable EQ "ce/v-est3.w Unit#1" AND
                           b-rt.company  EQ b-eb.company AND
                           b-rt.loc      EQ eb.est-no AND
                           b-rt.code     EQ STRING(eb.form-no,"9999999999") AND
                           b-rt.code2    EQ STRING(eb.blank-no,"9999999999")
                           NO-ERROR.

                FOR EACH job-mat NO-LOCK
                    WHERE job-mat.company EQ cocode
                      AND job-mat.job     EQ job-hdr.job
                      AND job-mat.frm     EQ eb.form-no,
                      FIRST item
                      {sys/look/itemivW.i}
                      AND item.i-no EQ job-mat.i-no
                      NO-LOCK:

                DO i = 1 TO 20:
                    IF eb.i-code2[i] EQ job-mat.i-no THEN 
                    DO:

                        cSide = "".
                        IF AVAIL(reftable) THEN
                            cSide = FILL(" ",5) + SUBSTRING(reftable.dscr,i,1).
                        FIND FIRST wrk-ink WHERE wrk-ink.i-code EQ eb.i-code2[i]
                               AND wrk-ink.form-no  EQ eb.form-no
                               AND wrk-ink.blank-no EQ eb.blank-no
                               AND (wrk-ink.i-side EQ cSide OR cSide EQ "")
                            NO-ERROR.
                        IF NOT AVAILABLE wrk-ink THEN 
                        DO:
                  
                            CREATE wrk-ink.
                            ASSIGN
                                wrk-ink.i-code   = eb.i-code2[i]
                                wrk-ink.form-no  = eb.form-no
                                wrk-ink.blank-no = eb.blank-no
                                wrk-ink.i-dscr   = eb.i-dscr2[i]
                                wrk-ink.i-pass   = eb.i-ps2[i]
                                wrk-ink.i-unit   = IF i LE 12 AND AVAILABLE reftable THEN reftable.val[i]
                                        ELSE IF i > 12 AND AVAILABLE b-rt THEN b-rt.val[i - 12]
                                        ELSE 1.
  
  
                            IF i LE 12 THEN 
                            DO:
                                FIND FIRST ref-side NO-LOCK WHERE
                                           ref-side.reftable EQ "ce/v-est3.w Unit#"  AND
                                           ref-side.company  EQ eb.company AND
                                           ref-side.loc      EQ eb.est-no AND
                                           ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                                           ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                                           NO-ERROR.
                                IF AVAILABLE ref-side THEN
                                    wrk-ink.i-side = FILL(" ",5) + SUBSTRING(ref-side.dscr,i,1).
                            END.
                            ELSE 
                            DO:
                                FIND FIRST ref-side WHERE
                                           ref-side.reftable EQ "ce/v-est3.w Unit#1"  AND
                                           ref-side.company  EQ eb.company AND
                                           ref-side.loc      EQ eb.est-no AND
                                           ref-side.code     EQ STRING(eb.form-no,"9999999999") AND
                                           ref-side.code2    EQ STRING(eb.blank-no,"9999999999")
                                           NO-ERROR.
                                IF AVAILABLE ref-side THEN
                                    wrk-ink.i-side = FILL(" ",5) + SUBSTRING(ref-side.dscr,i - 12,1).
                            END.          
                        
                            IF wrk-ink.i-unit EQ 0 THEN
                                wrk-ink.i-unit = 1.
                        END.
                    END.
                END. /* loop i */
            
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
                        wrk-ink.i-pass   = 1
                        wrk-ink.i-unit   = 1
                        .
                END.
                IF AVAILABLE wrk-ink AND
                    ((est.est-type EQ 4 AND eb.form-no = job-mat.frm AND eb.blank-no EQ job-mat.blank-no) OR
                    est.est-type NE 4 ) 
                    THEN wrk-ink.i-qty = wrk-ink.i-qty + job-mat.qty.            
            
            END. /* JOB-MAT */

        IF eb.est-type EQ 4 THEN v-fac = eb.yld-qty / v-est-qty.
          
        FIND FIRST style NO-LOCK
            WHERE style.company EQ eb.company
              AND style.style   EQ eb.style
            NO-ERROR.
        IF AVAILABLE style THEN v-stypart = style.dscr.
        ASSIGN
            v-dsc[1]  = eb.part-dscr1
            v-dsc[2]  = eb.part-dscr2
            v-size[1] = STRING(eb.len) + "x" + STRING(eb.wid) + "x" +
                         STRING(eb.dep)
            v-size[2] = eb.i-coldscr.

        IF eb.blank-no GT 0 AND eb.blank-no LE 11 THEN 
            ASSIGN v-fgdsc[eb.blank-no] = eb.part-dscr1.
                                                             
        /*if v-first then*/
        v-upc-lbl = "   CAD#".
        v-job-qty = 0.
        FOR EACH xjob-hdr FIELDS(qty) NO-LOCK 
             WHERE xjob-hdr.company EQ cocode
               AND xjob-hdr.job     EQ job-hdr.job
               AND xjob-hdr.job-no  EQ job-hdr.job-no
               AND xjob-hdr.job-no2 EQ job-hdr.job-no2
               AND xjob-hdr.i-no    EQ eb.stock :
               v-job-qty = v-job-qty + xjob-hdr.qty.
        END.

        /** PRINT ITEM **/
        FIND FIRST oe-ordl NO-LOCK
            WHERE oe-ordl.company EQ job-hdr.company
              AND oe-ordl.ord-no  EQ job-hdr.ord-no
              AND oe-ordl.job-no  EQ job-hdr.job-no
              AND oe-ordl.job-no2 EQ job-hdr.job-no2
              AND oe-ordl.i-no    EQ eb.stock-no /*job-hdr.i-no*/
              NO-ERROR.
 
        IF AVAILABLE oe-ordl THEN 
        DO:
            v-est-qty = oe-ordl.qty.
            FIND FIRST oe-ord OF oe-ordl NO-LOCK.
            v-ovund = STRING("Overrun/Underrun %:  " +
                TRIM(STRING(oe-ordl.over-pct,">>9.99")) + "/" +
                TRIM(STRING(oe-ordl.under-pct,">>9.99"))).
        END.
        ELSE v-est-qty = v-job-qty.
           
        RELEASE w-lo.
        FIND FIRST w-lo NO-ERROR.
        ASSIGN
            v-case-size   = STRING(eb.cas-len) + "x" + STRING(eb.cas-wid) + "x" +
                         STRING(eb.cas-dep)
            v-up          = eb.num-up
            v-case-count  = IF AVAILABLE oe-ordl AND oe-ordl.cas-cnt NE 0 THEN oe-ordl.cas-cnt
                           ELSE eb.cas-cnt
            v-case-qty    = ROUND(v-job-qty / v-case-count,0)
            v-itm-printed = v-itm-printed + 1.

        FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
            AND itemfg.i-no    EQ job-hdr.i-no NO-ERROR.
       
        FOR EACH wrk-op WHERE
            wrk-op.s-num EQ job-hdr.frm 
            BREAK BY wrk-op.d-seq BY wrk-op.b-num:
            
            ASSIGN
                iMrWaste = iMrWaste + wrk-op.mr-waste[job-hdr.frm]
                dMrHour = dMrHour + wrk-op.mr[job-hdr.frm]
                iSpeed = iSpeed + wrk-op.speed[job-hdr.frm]
                v-mr-hours = wrk-op.mr[job-hdr.frm]
                 dWstPrct = dWstPrct + wrk-op.spoil[job-hdr.frm].
        END.
        iCountLine = iCountLine + 4.
        RUN XMLOutput (lXMLOutput,'PlateMaking','','Row').
        RUN XMLOutput (lXMLOutput,'MR_Waste',iMrWaste,'Col').
        RUN XMLOutput (lXMLOutput,'Artwork',eb.plate-no,'Col').
        RUN XMLOutput (lXMLOutput,'MR_Hours',dMrHour,'Col').
        RUN XMLOutput (lXMLOutput,'fg_item',itemfg.i-no,'Col').
        RUN XMLOutput (lXMLOutput,'Item_code',itemfg.cc-code,'Col').
        RUN XMLOutput (lXMLOutput,'/PlateMaking','','Row').
        
        FIND FIRST ITEM NO-LOCK
            WHERE item.company EQ cocode
              AND item.i-no    EQ eb.cas-no
            NO-ERROR.
        v-item-desc = IF AVAILABLE ITEM THEN ITEM.i-name ELSE  "" .  
        /* end. /* last-of(eb.form-no) */      */
        IF LAST-OF(eb.form-no) THEN 
        DO:
            RUN oe/rep/ticket2.p (RECID(ef), RECID(job-hdr)).
            IF AVAILABLE oe-ordl THEN
                FIND FIRST po-ord NO-LOCK WHERE po-ord.company EQ oe-ordl.company
                    AND po-ord.po-no EQ INTEGER(oe-ordl.po-no-po) NO-ERROR.
            ASSIGN
                v-vend     = IF AVAILABLE oe-ordl THEN oe-ordl.vend-no ELSE ""
                v-board-po = IF AVAILABLE oe-ordl THEN oe-ordl.po-no-po ELSE 0.

            IF AVAILABLE po-ord THEN
                FIND FIRST po-ordl NO-LOCK WHERE
                           po-ordl.company EQ po-ord.company AND
                           po-ordl.po-no   EQ po-ord.po-no AND
                           po-ordl.i-no EQ ef.board
                           NO-ERROR.

            v-po-duedate = IF AVAILABLE po-ordl THEN po-ordl.due-date ELSE ?.
            cPoItemName = IF AVAILABLE po-ordl THEN po-ordl.i-name ELSE "".
            x = 2.
             
            FOR EACH wrk-sheet WHERE wrk-sheet.form-no = ef.form-no  
                        /*break by wrk-sheet.form-no*/ :  
                FIND FIRST ITEM WHERE item.company EQ cocode
                    AND item.i-no    EQ wrk-sheet.i-no NO-LOCK NO-ERROR.
                FIND FIRST job-mat NO-LOCK 
                    WHERE job-mat.company EQ cocode 
                      AND job-mat.job-no  EQ v-job-no
                      AND job-mat.job-no2 EQ v-job-no2
                      AND job-mat.frm     EQ wrk-sheet.form-no
                      AND job-mat.i-no    EQ wrk-sheet.i-no
                      NO-ERROR.

                FIND FIRST notes NO-LOCK WHERE notes.rec_key EQ job.rec_key AND
                                               notes.note_code EQ "BS" AND
                                               notes.note_form_no EQ wrk-sheet.form-no NO-ERROR.

                ASSIGN
                    v-lbs        = wrk-sheet.gsh-qty * (wrk-sheet.sh-wid * wrk-sheet.sh-len / 144) / 1000 * ITEM.basis-w
                    v-dept-title = IF AVAILABLE notes THEN notes.note_title ELSE ""
                    v-print-feet = (wrk-sheet.gsh-qty * ef.gsh-len) / 12 .

                  iCountLine = iCountLine + 5.
                  RUN XMLOutput (lXMLOutput,'Printing','','Row').
                  RUN XMLOutput (lXMLOutput,'Code',ef.m-code,'Col').
                  RUN XMLOutput (lXMLOutput,'MRWaste',iMrWaste,'Col').
                  RUN XMLOutput (lXMLOutput,'Stock_Code',IF AVAIL job-mat THEN  job-mat.rm-i-no ELSE "",'Col').
                  RUN XMLOutput (lXMLOutput,'Sheet_Size',string(ef.gsh-wid) + "  X  " + string(ef.gsh-len),'Col').
                  RUN XMLOutput (lXMLOutput,'MRHours',dMrHour,'Col').
                  RUN XMLOutput (lXMLOutput,'Board_Paper',cPoItemName,'Col').
                  RUN XMLOutput (lXMLOutput,'NetSheet_Size',string(ef.lsh-wid) + "  X  " + string(ef.lsh-len),'Col').
                  RUN XMLOutput (lXMLOutput,'UNIT_SIZE',STRING(eb.t-len) + " x " + STRING(eb.t-wid),'Col').
                  RUN XMLOutput (lXMLOutput,'FTM',iSpeed,'Col').
                  RUN XMLOutput (lXMLOutput,'LBS_Stock',IF AVAIL job-mat THEN  job-mat.qty ELSE 0,'Col').
                  RUN XMLOutput (lXMLOutput,'Flexo_Cylinder',ef.gsh-len,'Col').
                  RUN XMLOutput (lXMLOutput,'Spoilage',dWstPrct,'Col').
                  RUN XMLOutput (lXMLOutput,'Number_Out',ef.n-out,'Col').
                  RUN XMLOutput (lXMLOutput,'Caliper',ef.cal,'Col').
                  RUN XMLOutput (lXMLOutput,'/Printing','','Row').

                x = 1.
            END. /* each wrk-sheet */  

            FOR EACH wrk-film NO-LOCK WHERE wrk-film.form-no EQ ef.form-no
                        /*break by wrk-sheet.form-no*/ BREAK BY wrk-film.leaf :
                FIND FIRST ITEM NO-LOCK WHERE item.company EQ cocode
                                          AND item.i-no    EQ wrk-film.leaf NO-ERROR.
                FIND FIRST job-mch NO-LOCK
                     WHERE job-mch.company EQ cocode
                       AND job-mch.job EQ job.job
                       AND job-mch.job-no EQ job.job-no
                       AND job-mch.job-no2 EQ job.job-no2
                       AND job-mch.m-code  EQ ef.m-code 
                     NO-ERROR .
            END.
            ASSIGN
                x            = 2
                i            = 1
                v-ink1       = ""
                v-ink2       = ""
                v-pass-count = 0.

            FOR EACH wrk-ink WHERE wrk-ink.form-no EQ eb.form-no
                BREAK BY wrk-ink.i-pass:
                IF FIRST-OF(wrk-ink.i-pass) THEN v-pass-count = v-pass-count + 1.
            END.
            FOR EACH wrk-ink WHERE wrk-ink.form-no EQ eb.form-no
                BREAK BY wrk-ink.i-pass
                BY wrk-ink.i-code
                BY wrk-ink.blank-no:

                IF wrk-ink.i-pass LE 2 THEN
                    IF FIRST-OF(wrk-ink.i-pass) THEN i = 1.

                IF FIRST-OF(wrk-ink.i-code) THEN ASSIGN v-item[i]  = ""
                        v-i-qty[i] = 0.
                ASSIGN
                    v-item[i]  = IF LOOKUP(STRING(wrk-ink.blank-no),v-item[i]) GT 0 THEN v-item[i] ELSE v-item[i] + string(wrk-ink.blank-no) + ","
                    v-i-qty[i] = v-i-qty[i] + wrk-ink.i-qty.

                IF SUBSTRING(v-item[i],LENGTH(v-item[i]),1) EQ "," THEN v-item[i] = SUBSTRING(v-item[i],1,LENGTH(v-item[i]) - 1).                    
                IF wrk-ink.i-side NE "" THEN
                DO: 
                    
                    IF wrk-ink.i-pass EQ 1 THEN
                        ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                           STRING(v-i-qty[i],"->>,>>9.99") + "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                           STRING(wrk-ink.i-dscr,"x(25)") 
                               i         = i + 1. 
                    ELSE IF wrk-ink.i-pass EQ 2 THEN
                            ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                               STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                               STRING(wrk-ink.i-dscr,"x(25)")
                                i         = i + 1.
                        ELSE IF wrk-ink.i-pass EQ 3 THEN
                                ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                                   STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                                   STRING(wrk-ink.i-dscr,"x(25)")
                                    i         = i + 1.
                            ELSE IF wrk-ink.i-pass EQ 4 THEN
                                    ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                                       STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                                       STRING(wrk-ink.i-dscr,"x(25)")
                                        i         = i + 1.
                                ELSE IF wrk-ink.i-pass EQ 5 THEN
                                        ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                                           STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                                           STRING(wrk-ink.i-dscr,"x(25)")
                                            i         = i + 1.
                                    ELSE IF wrk-ink.i-pass EQ 6 THEN
                                            ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                                               STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                                               STRING(wrk-ink.i-dscr,"x(25)")
                                                i         = i + 1.
                                        ELSE IF wrk-ink.i-pass EQ 7 THEN
                                                ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                                                   STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                                                   STRING(wrk-ink.i-dscr,"x(25)")
                                                    i         = i + 1.
                                            ELSE IF wrk-ink.i-pass EQ 8 THEN
                                                    ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + string(wrk-ink.i-side,"x(7)") +
                                                                       STRING(v-i-qty[i],"->>,>>9.99") +  "    " + STRING(wrk-ink.i-code,"x(10)") + "  " +
                                                                       STRING(wrk-ink.i-dscr,"x(25)")
                                                        i         = i + 1.
                END.
                ELSE
                DO:
                    
                    
                    IF wrk-ink.i-pass EQ 1 THEN
                        ASSIGN v-ink1[i] =  STRING(wrk-ink.i-unit,">>>>>9") + "       456" + "  " + STRING(wrk-ink.i-pass,">>>>>9") + "  " + (IF v-pass-count EQ 1 THEN "F   " ELSE "B    ") +
                                            STRING(v-i-qty[i],"->,>>9.99") +  "    " + STRING(wrk-ink.i-code)  + "  " +
                                            STRING(wrk-ink.i-dscr,"x(25)") 
                            i         = i + 1. 
                    ELSE IF wrk-ink.i-pass EQ 2 THEN
                            ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "    " +
                                               STRING(wrk-ink.i-code)  + "  " + STRING(wrk-ink.i-dscr,"x(25)") 
                                i         = i + 1.
                        ELSE IF wrk-ink.i-pass = 3 THEN
                                ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "    " +
                                                   STRING(wrk-ink.i-code)  + "  " + STRING(wrk-ink.i-dscr,"x(25)") 
                                    i         = i + 1.
                            ELSE IF wrk-ink.i-pass EQ 4 THEN
                                    ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "    " +
                                                       STRING(wrk-ink.i-code)  + "  " + STRING(wrk-ink.i-dscr,"x(25)") 
                                        i         = i + 1.
                                ELSE IF wrk-ink.i-pass EQ 5 THEN
                                        ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "   " +
                                                           STRING(wrk-ink.i-code)  + "   " + STRING(wrk-ink.i-dscr,"x(25)") 
                                            i         = i + 1.
                                    ELSE IF wrk-ink.i-pass EQ 6 THEN
                                            ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "    " +
                                                               STRING(wrk-ink.i-code)  + "  " + STRING(wrk-ink.i-dscr,"x(25)")  
                                                i         = i + 1.
                                        ELSE IF wrk-ink.i-pass EQ 7 THEN
                                                ASSIGN v-ink1[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "    " +
                                                                   STRING(wrk-ink.i-code)  + "  " + STRING(wrk-ink.i-dscr,"x(25)") 
                                                    i         = i + 1.
                                            ELSE IF wrk-ink.i-pass = 8 THEN
                                                    ASSIGN v-ink2[i] = STRING(wrk-ink.i-unit,">>>>9") + "       456" + "  " +  STRING(wrk-ink.i-pass,">>>9")  + "F   " + STRING(v-i-qty[i],"->,>>9.99") +  "    " +
                                                                       STRING(wrk-ink.i-code)  + "  " + STRING(wrk-ink.i-dscr,"x(25)") 
                                                        i         = i + 1.
                END.

                /*END.*/
              
                DELETE wrk-ink.
            END. /* each wrk-ink */

            ASSIGN
                v-skip          = NO
                v-plate-printed = NO.

            ASSIGN
                v-upnew         = string(eb.num-wid) + " X " + STRING(eb.num-len)
                /*iSpeed      = 0*/
                v-mr-hours      = 0
                v-dc-gl-speed   = 0
                v-dc-out        = 0
                v-dc-only-out   = 0
                v-cas-wt        = 0
                v-sample-on-cnt = NO
                v-shrink-wrap   = CAN-FIND(FIRST est-op WHERE
                                est-op.company EQ job-hdr.company AND
                                est-op.est-no EQ est.est-no AND
                                est-op.dept = "SW").

            iCountLine = iCountLine + 3.

            RUN XMLOutput (lXMLOutput,'UP',STRING(v-upnew),'Col').
            RUN XMLOutput (lXMLOutput,'AC',eb.num-wid,'Col').
            RUN XMLOutput (lXMLOutput,'Print_Feet',v-print-feet,'Col').
            RUN XMLOutput (lXMLOutput,'AR',eb.num-len ,'Col').
            RUN XMLOutput (lXMLOutput,'FB',v-job-no + "-" + TRIM(STRING(eb.form-no,">>9")) + TRIM(STRING(eb.blank-no,">>9")),'Col').
            RUN XMLOutput (lXMLOutput,'passes',eb.i-pass,'Col').
            
            DO j = 1 TO EXTENT(v-ink1):
                IF TRIM(v-ink1[j]) EQ "-" THEN v-ink1[j] = "".               
                IF v-ink1[j] NE "" THEN 
                DO:
                        RUN XMLOutput (lXMLOutput,'ink',v-ink1[j],'Col').
                      
                        IF j EQ 2 THEN 
                        DO:
                            
                            v-plate-printed = YES.
                        END.
                        
                        iCountLine = iCountLine + 1.
                                                                            
                    v-skip = NOT v-skip.             
                END.
            END. 

            
            DO j = 1 TO EXTENT(v-ink2):
                IF TRIM(v-ink2[j]) EQ "-" THEN v-ink2[j] = "".                 
                IF v-ink2[j] NE "" THEN 
                DO:
                   RUN XMLOutput (lXMLOutput,'ink',v-ink2[j],'Col').
                    v-skip = NOT v-skip.
                    iCountLine = iCountLine + 1.
                END.                
            END.
            
            iCountLine = iCountLine + 1.
            iCountLine = iCountLine + 8.
      /*      RUN XMLOutput (lXMLOutput,'/Printing','','Row'). */

            RUN XMLOutput (lXMLOutput,'DieCutting','','Row').
            RUN XMLOutput (lXMLOutput,'MR_Waste',iMrWaste,'Col').
            RUN XMLOutput (lXMLOutput,'Die_Size',STRING(ef.trim-w) + "  x  " + STRING(ef.trim-l),'Col').
            RUN XMLOutput (lXMLOutput,'MR_Hours',dMrHour,'Col').
            RUN XMLOutput (lXMLOutput,'Die',eb.cad-no,'Col').
            RUN XMLOutput (lXMLOutput,'Run_Speed',iSpeed,'Col').
            RUN XMLOutput (lXMLOutput,'Spoilage',dWstPrct,'Col').
            RUN XMLOutput (lXMLOutput,'/DieCutting','','Row').
            
            FOR FIRST wrk-op WHERE
                      wrk-op.s-num EQ job-hdr.frm AND
                      INDEX("DC,GL",wrk-op.dept) > 0
                      BREAK BY wrk-op.d-seq BY wrk-op.b-num:

                      v-dc-gl-speed = wrk-op.speed[job-hdr.frm].
            END.

            FOR FIRST wrk-op WHERE
                      wrk-op.s-num EQ job-hdr.frm AND
                      wrk-op.dept EQ "DC"
                      BREAK BY wrk-op.d-seq BY wrk-op.b-num:
                     
                      v-dc-gl-speed = wrk-op.speed[job-hdr.frm].
            END.

            FOR EACH est-op FIELDS(n-out m-code d-seq b-num) NO-LOCK WHERE
                     est-op.company EQ job-hdr.company AND
                     est-op.est-no EQ est.est-no AND
                     est-op.line LT 500,
                     FIRST mach FIELDS(dept) NO-LOCK
                     {sys/ref/machW.i}
                     AND mach.m-code EQ est-op.m-code
                     BY est-op.d-seq BY est-op.b-num:

            IF mach.dept[1] EQ "DC" OR
                mach.dept[2] EQ "DC" THEN
            DO:
                v-dc-only-out = est-op.n-out.
                LEAVE.
            END.
                
        END.

        FOR EACH est-op FIELDS(n-out m-code d-seq b-num) NO-LOCK WHERE
                 est-op.company EQ job-hdr.company AND
                 est-op.est-no EQ est.est-no AND
                 est-op.line LT 500 ,
                 FIRST mach NO-LOCK
                 {sys/ref/machW.i}
                  AND mach.m-code EQ est-op.m-code
                  BY est-op.d-seq BY est-op.b-num:
                    
        IF INDEX("GL,DC",mach.dept[1]) GT 0 OR
                   INDEX("GL,DC",mach.dept[2]) GT 0 THEN
        DO:
            v-dc-out = est-op.n-out.
            LEAVE.
        END.
    END.

    FIND FIRST bf-job-mat NO-LOCK 
         WHERE bf-job-mat.company EQ job.company
           AND bf-job-mat.job     EQ job.job
           AND bf-job-mat.job-no  EQ job.job-no
           AND bf-job-mat.job-no2 EQ job.job-no2
           AND bf-job-mat.frm     EQ eb.form-no
           AND bf-job-mat.rm-i-no EQ eb.layer-pad 
         NO-ERROR .
                                         
    ASSIGN
        v-layer-qty = IF AVAILABLE bf-job-mat THEN bf-job-mat.qty ELSE 0 .
           
    FIND item WHERE
        item.company eq eb.company AND
        item.i-no eq eb.layer-pad  AND
        item.mat-type eq "5" 
        NO-LOCK NO-ERROR.

    ASSIGN 
        v-lp-dep = IF AVAILABLE item THEN ITEM.case-d ELSE 0 
        v-lp-qty = IF AVAILABLE item THEN ITEM.box-case ELSE 0 .

    IF eb.lp-up NE 0 THEN
    DO:
        v-unit-per-dec = eb.cas-cnt / eb.lp-up.
        {sys/inc/roundup.i v-unit-per-dec}
        v-unit-per-int = INTEGER(v-unit-per-dec).
    END.
    ELSE
        v-unit-per-int = 0.

    IF v-unit-per-int NE 0 THEN
    DO:
        v-job-qty-unit-per-dec = v-job-qty / v-unit-per-int.
        {sys/inc/roundup.i v-job-qty-unit-per-dec}
        v-job-qty-unit-per-int = v-job-qty-unit-per-dec.
    END.
    ELSE
        v-job-qty-unit-per-int = 0.

    IF eb.cas-cnt NE 0 THEN
    DO:
        v-job-qty-boxes-code-dec = v-job-qty / eb.cas-cnt.
        {sys/inc/roundup.i v-job-qty-boxes-code-dec}
        v-job-qty-boxes-code-int = v-job-qty-boxes-code-dec.
    END.
    ELSE
        v-job-qty-boxes-code-int = 0.

    FIND FIRST itemfg NO-LOCK WHERE
               itemfg.company EQ eb.company AND
               itemfg.i-no EQ eb.stock-no
               NO-ERROR.
             
    IF AVAILABLE itemfg THEN
    DO:
        v-cas-wt = (itemfg.weight-100 / 100) * eb.cas-cnt.             
    END.

    FIND FIRST tt-sample-ctn WHERE
               tt-sample-ctn.tt-job-no EQ job-hdr.job-no AND
               tt-sample-ctn.tt-job-no2 EQ job-hdr.job-no2 AND
               tt-sample-ctn.tt-frm EQ eb.form-no
               NO-ERROR.

    IF AVAILABLE tt-sample-ctn THEN
        v-sample-on-cnt = tt-sample-ctn.tt-samp-on-cnt.

    IF v-dc-only-out EQ 0 THEN
        v-dc-only-out = 1.

    IF v-dc-out EQ 0 THEN
        v-dc-out = 1.

    FIND FIRST ITEM NO-LOCK
        WHERE item.company EQ cocode
          AND item.i-no    EQ eb.cas-no
        NO-ERROR.

    FIND FIRST bf-job-mat NO-LOCK
         WHERE bf-job-mat.company EQ job.company 
           AND bf-job-mat.job EQ job.job 
           AND bf-job-mat.job-no EQ job.job-no 
           AND bf-job-mat.job-no2 EQ job.job-no2
           AND bf-job-mat.frm EQ eb.form-no
           AND bf-job-mat.rm-i-no EQ eb.cas-no 
         NO-ERROR .
     ASSIGN
        v-cases-qty = IF AVAILABLE bf-job-mat THEN bf-job-mat.qty ELSE 0 .
        dQtyTray =  IF v-lp-qty GT 0 THEN eb.cas-cnt / v-lp-qty ELSE 0 .

       iCountLine = iCountLine + 10.
           IF iCountLine >= 41 THEN do:
             PAGE.
             iCountLine = 0 .
           END.
           
           iCountLine = iCountLine + 4.
           RUN XMLOutput (lXMLOutput,'RollExamining','','Row').
           RUN XMLOutput (lXMLOutput,'code',ef.m-code,'Col').
           RUN XMLOutput (lXMLOutput,'MR_Waste',iMrWaste,'Col').
           RUN XMLOutput (lXMLOutput,'case_no',eb.cas-no,'Col').
           RUN XMLOutput (lXMLOutput,'Size',string(item.case-w) + "  x  " + STRING(item.case-l) + "  x  " + STRING(item.case-d),'Col').
           RUN XMLOutput (lXMLOutput,'MR_Hours',dMrHour,'Col').
           RUN XMLOutput (lXMLOutput,'Units_perCase',eb.cas-cnt,'Col').
           RUN XMLOutput (lXMLOutput,'QtyofCases',v-cases-qty,'Col').
           RUN XMLOutput (lXMLOutput,'case_weight',v-cas-wt,'Col').
           RUN XMLOutput (lXMLOutput,'Flat',STRING(eb.t-len) + " x " + STRING(eb.t-wid),'Col').
           RUN XMLOutput (lXMLOutput,'Run_Speed',iSpeed,'Col').
           RUN XMLOutput (lXMLOutput,'Qtyof_Cases',v-cases-qty,'Col').
           RUN XMLOutput (lXMLOutput,'Spoilage',dWstPrct,'Col').
           RUN XMLOutput (lXMLOutput,'Divider',string(eb.div-up) + "   " + string(eb.div-len) + " x " + STRING(eb.div-wid) + "   " + STRING(eb.divider),'Col').
           RUN XMLOutput (lXMLOutput,'Dividerper_Case',eb.div-up,'Col').
           RUN XMLOutput (lXMLOutput,'Shrink_Wrap',v-shrink-wrap,'Col').
           RUN XMLOutput (lXMLOutput,'Wind_Direction',itemfg.prod-code,'Col').
           RUN XMLOutput (lXMLOutput,'Size',string(eb.lp-len) + "     x     " + STRING(eb.lp-wid),'Col').
           RUN XMLOutput (lXMLOutput,'stock',IF AVAIL job-mat THEN  job-mat.rm-i-no ELSE "",'Col').
           RUN XMLOutput (lXMLOutput,'Units_per_TrayLP',v-layer-qty,'Col').
           RUN XMLOutput (lXMLOutput,'LBS_Stock',IF AVAIL job-mat THEN job-mat.qty ELSE 0,'Col').
           RUN XMLOutput (lXMLOutput,'Qty_of_TraysLPs',STRING(dQtyTray) + "      " + STRING(eb.lp-up),'Col').
           RUN XMLOutput (lXMLOutput,'Web_Width',string(ef.gsh-wid) + "  X  " + string(ef.gsh-len),'Col').
           RUN XMLOutput (lXMLOutput,'Size',string(eb.tr-len) + "     x     " + STRING(eb.tr-wid) + "       " + STRING(eb.tr-no),'Col').
           RUN XMLOutput (lXMLOutput,'Quantity',job-hdr.qty,'Col').
           RUN XMLOutput (lXMLOutput,'Max_Ht',cShpDoc,'Col').
           RUN XMLOutput (lXMLOutput,'class',itemfg.class,'Col').
           RUN XMLOutput (lXMLOutput,'/RollExamining','','Row').
           
           RUN XMLOutput (lXMLOutput,'TicketPrint','','Row').
           RUN XMLOutput (lXMLOutput,'Requested_Date',dDueDate,'Col').
           RUN XMLOutput (lXMLOutput,'Ship_Due_Date',dLastDate,'Col').
           RUN XMLOutput (lXMLOutput,'Ship_id',eb.ship-id,'Col').
           RUN XMLOutput (lXMLOutput,'Fright_class',cFrtCls,'Col').
           RUN XMLOutput (lXMLOutput,'Fright_class_description',cFrtClsDscr,'Col').
           RUN XMLOutput (lXMLOutput,'Carrier',eb.carrier,'Col').
           RUN XMLOutput (lXMLOutput,'Freigth_Charge',eb.chg-method,'Col').
           RUN XMLOutput (lXMLOutput,'Warehouse',cDefLoc,'Col').
           RUN XMLOutput (lXMLOutput,'Available_Deliverly_Hours',cShipDocHour,'Col').
           RUN XMLOutput (lXMLOutput,'Dock_Appointment_Number',cDockAptmnt,'Col').
           RUN XMLOutput (lXMLOutput,'Dock_Appointment_Contact',cContact,'Col').
           RUN XMLOutput (lXMLOutput,'/TicketPrint','','Row').
             
   END. /* last-of(eb.form-no) */
          
  END. /* each eb */

  FIND FIRST b-ef WHERE b-ef.company = job-hdr.company
                        AND b-ef.est-no = job-hdr.est-no
                        AND b-ef.form-no = job-hdr.frm NO-LOCK NO-ERROR.
          
          IF AVAIL b-ef AND print-box AND b-ef.cad-image <> "" THEN DO:
             FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                            AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
             IF LOOKUP(b-ef.cad-image,lv-cad-image-list) <= 0
             THEN DO:             
               lv-cad-image = IF index(b-ef.cad-image,":") > 0 OR index(b-ef.cad-image,"\\") > 0 
                              THEN b-ef.cad-image
                              ELSE IF AVAIL sys-ctrl THEN trim(sys-ctrl.char-fld) + b-ef.cad-image + ".JPG"
                              ELSE b-ef.cad-image + ".JPG".
               lv-cad-image-list = lv-cad-image-list + b-ef.cad-image + ",".              
/*                RUN prPage(8). */
               PAGE.
               
             END.
          END.
          ELSE DO:
            FIND FIRST eb WHERE eb.company = job-hdr.company
                          AND eb.est-no = job-hdr.est-no
                          AND eb.form-no = job-hdr.frm NO-LOCK NO-ERROR.
            FIND FIRST sys-ctrl WHERE sys-ctrl.company = job-hdr.company
                                AND sys-ctrl.NAME = "DIEFILE" NO-LOCK NO-ERROR.
            IF AVAIL eb AND print-box AND eb.die-no <> "" AND LOOKUP(eb.die-no,lv-cad-image-list) <= 0
            THEN DO:             
             lv-cad-image =  (IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "") +
                          eb.die-no + ".JPG".
             lv-cad-image-list = lv-cad-image-list + eb.die-no + ",".

/*              RUN prPage(8). */
             PAGE.
             
            END.
        END. /* i > 0 */


 END. /* each ef */
END. /* first job-no */

IF LAST-OF(job-hdr.frm) THEN 
DO:
    
    iCountLine = iCountLine + 10.
    lv-line-chars = 128.
    FIND FIRST job OF job-hdr NO-LOCK NO-ERROR.       
        
    /*=change */
    FOR EACH tt-formtext:
        DELETE tt-formtext.
    END.
    ASSIGN
        lv-text             = ""
        v-dept-inst         = ""
        v-exc-depts         = v-exc-depts + (IF v-exc-depts NE "" THEN ",BS" ELSE "BS")
        v-dept-note-printed = NO.
    ASSIGN
        lv-line-chars = 95
        v-inst2       = "".

    FIND FIRST itemfg NO-LOCK WHERE itemfg.company EQ job-hdr.company
                                AND itemfg.i-no EQ job-hdr.i-no NO-ERROR.

    {custom/notespr8.i itemfg v-inst2 25 "notes.rec_key EQ itemfg.rec_key AND
                           notes.note_type EQ 'S' AND CAN-DO(v-spec-list,notes.note_code) "}
   
    
    ASSIGN
        i       = 1
        v-fgitm = "".
                                
    /* Close JDF section */
    RUN ipOutputJDFBottom. 
    
    PAGE.
END. /* last-of job-hdr.frm */

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

END. /* for first job-hdr */  

{XMLOutput/XMLOutput.i &XMLClose} /* rstark 05181205 */

OS-RENAME VALUE(XMLTemp) VALUE(cXMlFinalDest).
/* Don't show user view xml file */
/*  os-command silent value(XMLTemp). */
  /* READ-XML( XMLTemp).*/

PROCEDURE ipGenJobTempTable:
DEFINE INPUT  PARAMETER iprJobRow AS ROWID NO-UNDO.

DEFINE VARIABLE cItemOnOrder AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCustPart    AS CHARACTER NO-UNDO.
DEFINE VARIABLE rItemfgRow   AS ROWID NO-UNDO.
DEFINE VARIABLE cSheetBlank  AS CHARACTER NO-UNDO.
DEFINE BUFFER bf-job FOR job.
DEFINE BUFFER bf-job-hdr FOR job-hdr.
DEFINE BUFFER bf-itemfg FOR itemfg.
DEFINE BUFFER bf-eb FOR eb.
DEFINE BUFFER bf-ef FOR ef.     

    EMPTY TEMP-TABLE ttTempjob.
   
    FIND FIRST bf-job NO-LOCK 
        WHERE ROWID(bf-job) EQ iprJobRow
        NO-ERROR.
    IF NOT AVAILABLE bf-job THEN 
      RETURN.
        
    FOR EACH bf-job-hdr NO-LOCK WHERE bf-job-hdr.company EQ bf-job.company
        AND bf-job-hdr.job-no  EQ bf-job.job-no
        AND bf-job-hdr.job-no2 EQ bf-job.job-no2,
        FIRST bf-itemfg NO-LOCK WHERE bf-itemfg.company EQ bf-job-hdr.company
        AND bf-itemfg.i-no    EQ bf-job-hdr.i-no
        :
        cItemOnOrder = bf-itemfg.i-no.
        FIND FIRST bf-eb NO-LOCK 
            WHERE bf-eb.company EQ bf-job-hdr.company
            AND bf-eb.est-no  EQ bf-job-hdr.est-no
            AND bf-eb.stock-no = bf-job-hdr.i-no
            NO-ERROR.
        IF NOT AVAIL bf-eb THEN 
            FIND FIRST bf-eb NO-LOCK 
                WHERE bf-eb.company     EQ bf-job-hdr.company
                AND bf-eb.est-no      EQ bf-job-hdr.est-no
                AND bf-eb.form-no     EQ bf-job-hdr.frm
                AND bf-eb.blank-no    GT 0 NO-ERROR.              
        IF AVAILABLE bf-eb THEN 
        DO:        
            FIND FIRST oe-ord NO-LOCK
                WHERE oe-ord.company EQ bf-eb.company
                AND oe-ord.est-no  EQ bf-eb.est-no                  
                NO-ERROR.             
            FIND FIRST bf-ef NO-LOCK 
                WHERE bf-ef.company EQ bf-eb.company
                AND bf-ef.est-no EQ bf-eb.est-no
                AND bf-ef.form-no = bf-eb.form-no
                NO-ERROR.
            IF NOT AVAILABLE bf-ef THEN 
                FIND FIRST bf-ef NO-LOCK 
                    WHERE bf-ef.company EQ bf-eb.company
                    AND bf-ef.est-no EQ bf-eb.est-no
                    NO-ERROR.
            
            FIND style NO-LOCK 
                WHERE style.company = bf-eb.company 
                AND style.style = bf-eb.style
                NO-ERROR.
            /* stock-no overrides item number for sales order lookup as jobcard does */
            IF bf-eb.stock-no GT "" THEN 
                cItemOnOrder = bf-eb.stock-no.
        END.
        
        cocode = bf-job-hdr.company.
        FIND FIRST sys-ctrl NO-LOCK WHERE sys-ctrl.company EQ bf-job-hdr.company
            AND sys-ctrl.name EQ "xmljob"
            NO-ERROR.
        IF NOT AVAILABLE sys-ctrl THEN RETURN.
   
        FIND FIRST cust NO-LOCK WHERE cust.company EQ bf-job-hdr.company
            AND cust.cust-no EQ bf-job-hdr.cust-no
            NO-ERROR.
        
        rItemfgRow = ROWID(bf-itemfg).
        cCustPart = bf-itemfg.part-no.
        RUN custom/getcpart.p (INPUT cust.company, INPUT cust.cust-no, 
            INPUT-OUTPUT cCustPart, INPUT-OUTPUT rItemfgRow).

        cSheetBlank = STRING(bf-job-hdr.frm, "9") + string(bf-job-hdr.blank-no, "9") .

        FIND FIRST ttTempJob EXCLUSIVE-LOCK WHERE ttTempJob.company EQ bf-job-hdr.company
            AND ttTempJob.jobID = trim(bf-job-hdr.job-no) + "-" + STRING(bf-job-hdr.job-no2, "99") + "-" + cSheetBlank
            AND ttTempJob.newProject EQ 1
            NO-ERROR.

        IF NOT AVAILABLE ttTempJob THEN 
        DO:

            CREATE ttTempJob.
            ASSIGN
                ttTempjob.company    = bf-job-hdr.company
                ttTempjob.jobID      = TRIM(bf-job-hdr.job-no) + "-" + STRING(bf-job-hdr.job-no2, "99") + "-" + cSheetBlank
                ttTempjob.itemRecKey = bf-itemfg.rec_key
                ttTempjob.FGItemCode = cItemOnOrder
                ttTempjob.FGName     = bf-itemfg.i-name
                ttTempjob.CustPart   = cCustPart
                ttTempjob.ItemStatus = (IF bf-itemfg.stat EQ "A" THEN "Active" ELSE "Inactive") 
                ttTempjob.FgCategory = bf-itemfg.procat-desc
                .

            /* Override of bf-itemfg.procat-desc to match logic in viewers/itemfg.w */    
            FIND FIRST fgcat NO-LOCK WHERE fgcat.company = bf-job-hdr.company 
                AND fgcat.procat = bf-itemfg.procat
                NO-ERROR.
            ttTempjob.FgCategory = IF AVAIL fgcat THEN fgcat.dscr ELSE ttTempjob.FgCategory.
                                
            IF AVAILABLE cust THEN 
                ASSIGN ttTempjob.customerID   = cust.cust-no
                    ttTempJob.customerName = cust.name
                    .
            IF AVAILABLE eb THEN 
                ASSIGN                   
                    ttTempJob.ebWIDTH    = STRING(bf-eb.wid)
                    ttTempJob.ebLENGTH   = STRING(bf-eb.len)
                    ttTempJob.ebDepth    = STRING(bf-eb.dep)
                    ttTempJob.FlatWidth  = STRING(bf-eb.t-len)
                    ttTempJob.FlatLength = STRING(bf-eb.t-wid)
                    ttTempJob.ColorsCoat = bf-eb.i-dscr2[1]
                    ttTempJob.CCNumber   = bf-eb.cad-no                  
                    .
            IF AVAILABLE ef THEN 
                ASSIGN  ttTempJob.Weight  = STRING(bf-ef.weight)
                    ttTempJob.Caliper = STRING(bf-ef.cal)                    
                    ttTempJob.Board   = fnStripInvalidChar(bf-ef.brd-dscr)
                    .
            IF AVAILABLE oe-ord THEN 
                ASSIGN 
                    ttTempJob.DateIssued = STRING(oe-ord.ord-date)
                    .
            IF AVAILABLE style THEN 
                ASSIGN 
                    ttTempJob.Structure = style.dscr
                    .      
        END.  /* create ttTempJob */
    END. /* Each bf-job-hdr of job */         
          
    
END PROCEDURE. /* ipGenJobTempTable */

PROCEDURE ipOutputJDFTop:
   DEFINE VARIABLE cUDFString AS CHARACTER NO-UNDO.        
    
    FOR EACH ttTempJob BREAK BY ttTempJob.jobID:
        
        IF FIRST-OF(ttTempJob.jobID) THEN 
        DO:                        
            RUN XMLOutput (lXMLOutput,'JDF','','Row').
            RUN XMLOutput (lXMLOutput,'Company',ttTempJob.company,'Col').
            RUN XMLOutput (lXMLOutput,'JobID',ttTempJob.jobID,'Col').
            RUN XMLOutput (lXMLOutput,'NewProject',0,'Col').
            RUN XMLOutput (lXMLOutput,'CustomerID',ttTempJob.customerID,'Col').
            RUN XMLOutput (lXMLOutput,'CustomerName',ttTempJob.customerName,'Col').
        END.
        
        RUN XMLOutput (lXMLOutput,'ResourcePool','','Row').
        
        /* Process Itemfg Level */
        RUN XMLOutput (lXMLOutput,'Product','','Row').
        RUN XMLOutput (lXMLOutput,'FGItemCode', ttTempjob.FGItemCode,'Col').
        RUN XMLOutput (lXMLOutput,'FGName'    , ttTempjob.FGName,'Col').
        RUN XMLOutput (lXMLOutput,'CustPart'  , ttTempjob.CustPart,'Col').    
        RUN XMLOutput (lXMLOutput,'FGCategory', ttTempjob.FgCategory,'Col').
        RUN XMLOutput (lXMLOutput,'ItemStatus', ttTempjob.ItemStatus,'Col').  
                
        EMPTY TEMP-TABLE ttUDF.
        IF CAN-FIND(FIRST mfvalues
            WHERE mfvalues.rec_key EQ ttTempjob.itemRecKey) THEN 
        DO:
            /* get UDF records for this record */
            RUN UDF/UDF.p (cUDFGroup, ttTempjob.itemRecKey, OUTPUT TABLE ttUDF).
                     
                     
            /* process UDF data found */
            FOR EACH ttUDF NO-LOCK 
                WHERE ttUDF.udfEsko EQ YES
                :                            
                cUDFSTring = "SmartName " + 'Name="' + ttUDF.udfLabel + '" Value="' + ttUDF.udfValue + '"/'.
                RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
            END.  /* each ttudf */     
        END.        
                
        /* Estimate Values */
        cUDFSTring = "SmartName " + 'Name="Date Issued" Value="' + ttTempJob.DateIssued + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').   
        cUDFSTring = "SmartName " + 'Name="Width" Value="' + ttTempJob.ebWIDTH    + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row'). 
        cUDFSTring = "SmartName " + 'Name="Length" Value="' + ttTempJob.ebLENGTH   + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Depth" Value="' + ttTempJob.ebDepth    + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').  
        cUDFSTring = "SmartName " + 'Name="Flat Width" Value="' + ttTempJob.FlatWidth  + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Flat Length" Value="' + ttTempJob.FlatLength + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Colors/Coating" Value="' + ttTempJob.ColorsCoat + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="CC#" Value="' + ttTempJob.CCNumber   + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Weight" Value="' + ttTempJob.Weight     + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Caliper" Value="' + ttTempJob.Caliper    + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Style Code" Value="' + ttTempJob.Structure  + '"/'.
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
        cUDFSTring = "SmartName " + 'Name="Board" Value=' + "'" + ttTempJob.Board      + "'/".
        RUN XMLOutput (lXMLOutput,cUDFString,'','Row').
       
    END.
                               
  
END PROCEDURE. /* ipOutputJDFTop */

PROCEDURE ipOutputJDFBottom:
    
    RUN XMLOutput (lXMLOutput,'/Product','','Row'). 
    RUN XMLOutput (lXMLOutput,'/ResourcePool','','Row').
            
   /* IF LAST-OF(ttTempJob.jobID) THEN */
        RUN XMLOutput (lXMLOutput,'/JDF','','Row').      
END PROCEDURE. /* ipOutputJDFBottom */
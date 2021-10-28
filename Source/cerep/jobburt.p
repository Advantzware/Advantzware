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
DEFINE OUTPUT PARAMETER oplcRequestData AS LONGCHAR NO-UNDO.

{api/ttAPIOutboundEvent.i}

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
DEFINE            VARIABLE v-first       AS LOG       NO-UNDO.
DEFINE            VARIABLE v-spec-list   AS CHARACTER FORMAT "x(20)"INIT "QA" NO-UNDO.
DEFINE            VARIABLE lv-form-note  AS cha       NO-UNDO.
DEFINE            VARIABLE v-itm-printed AS INTEGER   NO-UNDO.
DEFINE            VARIABLE v-alloc       AS cha       NO-UNDO.
DEFINE            VARIABLE v-fill2       AS cha       INIT "-" FORM "x(100)" NO-UNDO.
DEFINE            VARIABLE v-fill3       AS CHARACTER FORMAT "x(100)" NO-UNDO.
DEFINE            VARIABLE li            AS INTEGER   NO-UNDO.


DEFINE NEW SHARED BUFFER xjob-hdr FOR job-hdr.
DEFINE BUFFER b-eb       FOR eb.
DEFINE BUFFER b-ef       FOR ef.
DEFINE BUFFER bf-item    FOR ITEM.
DEFINE BUFFER bx-job-hdr FOR job-hdr.
DEFINE BUFFER bff-eb FOR eb .
DEFINE VARIABLE v-ord-no AS INTEGER NO-UNDO.

DEFINE SHARED VARIABLE s-prt-set-header AS LOG       NO-UNDO.  
DEFINE SHARED VARIABLE s-prt-label      AS LOG       NO-UNDO.
DEFINE SHARED VARIABLE lIncludeLastPage AS LOGICAL   NO-UNDO .
DEFINE SHARED VARIABLE cRdOptionMclean  AS CHARACTER NO-UNDO .
DEFINE SHARED VARIABLE cJobType AS CHARACTER NO-UNDO .
DEFINE SHARED VARIABLE lFSC AS LOGICAL NO-UNDO .
    
{custom/notesdef.i}
DEFINE VARIABLE v-inst2          AS cha  EXTENT 70 NO-UNDO.    
DEFINE VARIABLE v-start-date     AS DATE NO-UNDO.
DEFINE VARIABLE v-shipto         AS cha  FORMAT "x(30)" EXTENT 4 NO-UNDO.
DEFINE BUFFER xjob-mat FOR job-mat.
DEFINE VARIABLE v-cust-name LIKE oe-ord.cust-name NO-UNDO.
DEFINE VARIABLE v-spc-no    AS cha     FORM "x(15)" NO-UNDO.
DEFINE VARIABLE v-stock-no  LIKE eb.stock-no NO-UNDO.

{custom/formtext.i NEW}
DEFINE VARIABLE lv-text     AS CHARACTER NO-UNDO.
DEFINE VARIABLE lv-note-cnt AS INTEGER   NO-UNDO.

ASSIGN
    v-fill  = "<||3><C1><FROM><C83><LINE><||3>"
    v-fill2 = "<||3><C5><FROM><C83><LINE><||3>"
    v-fill3 = "<C2><FROM><C83><LINE><||3><R-1>".

DEFINE NEW SHARED FRAME head.

DEFINE SHARED VARIABLE s-prt-mstandard     AS LOG     NO-UNDO.
DEFINE SHARED VARIABLE s-prt-sellprc       AS LOG     NO-UNDO.
DEFINE        VARIABLE v-ink-seq           AS INTEGER NO-UNDO.
DEFINE        VARIABLE v-ink-use-per-blank AS INTEGER NO-UNDO.  

DEFINE BUFFER bf-jobhdr FOR job-hdr.

DEFINE TEMP-TABLE tt-reftable NO-UNDO LIKE reftable
    FIELD est-type LIKE est.est-type.

DEFINE        VARIABLE lv-cad-image      AS cha       NO-UNDO.
DEFINE        VARIABLE lv-cad-image-list AS cha       NO-UNDO.
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
DEFINE VARIABLE iEbTotalUpQty AS INTEGER NO-UNDO .
DEFINE SHARED VARIABLE s-prt-fgimage     AS LOG       NO-UNDO.
DEFINE BUFFER bf-ttSoule FOR ttSoule .
DEFINE VARIABLE lv-pg-num AS INT NO-UNDO.
DEFINE VARIABLE lAssembled AS LOGICAL NO-UNDO . 
DEFINE VARIABLE cSetFGItem AS CHARACTER NO-UNDO . 
DEFINE VARIABLE cCaseItem AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCaseSize AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCaseCount AS CHARACTER NO-UNDO.
DEFINE VARIABLE cCasePerPallet AS CHARACTER NO-UNDO.
DEFINE VARIABLE lPrintSetHeader AS LOGICAL NO-UNDO.
DEFINE VARIABLE iTotalJobQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iTotalReqQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iJobQty AS INTEGER NO-UNDO .
DEFINE VARIABLE iReqQty AS INTEGER NO-UNDO .

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


DEFINE VARIABLE lSuccess         AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage         AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdOutboundProcs  AS HANDLE    NO-UNDO.

DEFINE TEMP-TABLE ttEstCostHeaderID NO-UNDO
    FIELD estCostHeaderID AS INT64
    FIELD riJobHeader     AS ROWID
    .


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
    
    FIND FIRST estCostHeader NO-LOCK
         WHERE estCostHeader.company    = job-hdr.company
           AND estCostHeader.estimateNo = job-hdr.est-no
           AND estCostHeader.jobID      = job-hdr.job-no
           AND estCostHeader.jobid2     = job-hdr.job-no2
         NO-ERROR.
    IF NOT AVAILABLE estCostHeader THEN
        NEXT.
    
    FIND FIRST ttEstCostHeaderID NO-LOCK
         WHERE ttEstCostHeaderID.estCostHeaderID EQ estCostHeader.estCostHeaderID
         NO-ERROR.
    IF NOT AVAILABLE ttEstCostHeaderID THEN DO:
        CREATE ttEstCostHeaderID.
        ASSIGN
            ttEstCostHeaderID.estCostHeaderID = estCostHeader.estCostHeaderID
            ttEstCostHeaderID.riJobHeader     = ROWID(job-hdr)
            .    
    END.
END.
/* end of building tt-reftable */

RUN api/OutboundProcs.p PERSISTENT SET hdOutboundProcs.

RUN Outbound_PrepareAndExecuteForScope IN hdOutboundProcs (
    INPUT  cocode,                             /* Company Code (Mandatory) */
    INPUT  "",                                /* Location Code (Mandatory) */
    INPUT  "JobTicket",                    /* API ID (Mandatory) */
    INPUT  "",                                     /* Scope ID */
    INPUT  "",                                     /* Scope Type */
    INPUT  "PrintJob",                         /* Trigger ID (Mandatory) */
    INPUT  "TTEstCostHeaderIDHandle",                           /* Comma separated list of table names for which data being sent (Mandatory) */
    INPUT  STRING(TEMP-TABLE ttEstCostHeaderID:HANDLE),                   /* Comma separated list of ROWIDs for the respective table's record from the table list (Mandatory) */ 
    INPUT  "Job Print",                          /* Primary ID for which API is called for (Mandatory) */   
    INPUT  "Job Card print",    /* Event's description (Optional) */
    OUTPUT lSuccess,                               /* Success/Failure flag */
    OUTPUT cMessage                                /* Status message */
    ).
                    
RUN Outbound_GetEvents IN hdOutboundProcs (OUTPUT TABLE ttAPIOutboundEvent).

FIND FIRST ttAPIOutboundEvent NO-LOCK NO-ERROR.
IF AVAILABLE ttAPIOutboundEvent THEN DO:
    FIND FIRST apiOutboundEvent NO-LOCK
         WHERE apiOutboundEvent.apiOutboundEventID EQ ttAPIOutboundEvent.APIOutboundEventID
         NO-ERROR.
    IF AVAILABLE apiOutboundEvent THEN
        oplcRequestData = apiOutboundEvent.requestData.
END.    
                                
DELETE PROCEDURE hdOutboundProcs.


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

PROCEDURE pGetFilePath PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcFilePath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcFilePath AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER oplValid AS LOGICAL NO-UNDO.
    
   

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
    DEFINE OUTPUT PARAMETER opiReturnJobQty AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiReturnReqQty AS INTEGER NO-UNDO.
    DEFINE OUTPUT PARAMETER opiOut AS INTEGER NO-UNDO.
   
    DEFINE BUFFER bff-job-hdr FOR job-hdr.
    DEFINE BUFFER bff-oe-ordl FOR oe-ordl.
     
    FOR EACH bff-job-hdr NO-LOCK
          WHERE bff-job-hdr.company EQ ipcCompany 
            AND bff-job-hdr.job-no EQ ipcJobID 
            AND bff-job-hdr.job-no2 EQ ipiJobID2
            AND bff-job-hdr.frm EQ ipiFormNo  :
            
      opiReturnJobQty = opiReturnJobQty +  bff-job-hdr.qty .
      
      FIND FIRST bff-oe-ordl
            WHERE bff-oe-ordl.company EQ bff-job-hdr.company
            AND bff-oe-ordl.ord-no  EQ bff-job-hdr.ord-no
            AND bff-oe-ordl.job-no  EQ bff-job-hdr.job-no
            AND bff-oe-ordl.job-no2 EQ bff-job-hdr.job-no2
            AND bff-oe-ordl.i-no    EQ bff-job-hdr.i-no
            NO-LOCK NO-ERROR.

      IF bff-job-hdr.ord-no NE 0 AND NOT AVAILABLE bff-oe-ordl THEN
      FIND FIRST bff-oe-ordl
           WHERE bff-oe-ordl.company EQ bff-job-hdr.company
           AND bff-oe-ordl.ord-no  EQ bff-job-hdr.ord-no
           AND bff-oe-ordl.i-no    EQ bff-job-hdr.i-no
          NO-ERROR.
      IF bff-job-hdr.ord-no NE 0 AND NOT AVAILABLE bff-oe-ordl THEN
      FIND FIRST bff-oe-ordl
           WHERE bff-oe-ordl.company EQ bff-job-hdr.company
           AND bff-oe-ordl.ord-no  EQ bff-job-hdr.ord-no
           NO-ERROR.    
      
      
      opiReturnReqQty = opiReturnReqQty + (IF AVAIL bff-oe-ordl THEN bff-oe-ordl.qty ELSE bff-job-hdr.qty ) .
      
    END. 
    
    FOR FIRST estCostHeader NO-LOCK
        WHERE estCostHeader.company EQ ipcCompany
        AND estCostHeader.jobID EQ ipcJobID
        AND estCostHeader.jobID2 EQ ipiJobID2,
        EACH estCostBlank NO-LOCK
        WHERE estCostBlank.estCostHeaderID EQ estCostHeader.estCostHeaderID
        AND estCostBlank.formNo EQ ipiFormNo:
            ASSIGN             
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


PROCEDURE pPrintImage PRIVATE:
    /*------------------------------------------------------------------------------
     Purpose:
     Notes:
    ------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcImageType AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipiForm AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipiBlank AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER ipcImageFile AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER iplAddExtension AS LOGICAL NO-UNDO.
    
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValid AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    IF iplAddExtension THEN DO:
        cImageFile = ipcImageFile + ".pdf".
        RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
        IF NOT lValid THEN DO:
            cImageFile = ipcImageFile + ".jpg".
            RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
        END.
        IF NOT lValid THEN 
        DO:
            cImageFile = ipcImageFile + ".png".
            RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
        END.
    END.
    ELSE DO:     
        cImageFile = ipcImageFile.
        RUN FileSys_ValidateFile(cImageFile, OUTPUT lValid, OUTPUT cMessage).
    END.
    PAGE.
    RUN pPrintHeader .
                
    PUT UNFORMATTED 
        "<#12><C2><R8><FROM><C80><R+52><RECT><||3><C80>" SKIP
        "<=12><R+1><C5>Image: " ipcImageType         "          Form No:" ipiForm FORMAT ">99"  " Blank No:" ipiBlank FORMAT ">99"
        "<=12><R+2><C2><FROM><C80><LINE><||3>".
    IF lValid THEN 
        PUT UNFORMATTED
        "<=12><R+3><C3><#21><R+46><C+76><IMAGE#21=" cImageFile ">" SKIP.
    ELSE 
        PUT UNFORMATTED "<C5>Invalid File: " ipcImageFile SKIP.
    PUT "<C74><R64>Page: " STRING(PAGE-NUM - lv-pg-num,">>9") + " of <#PAGES>"  FORM "x(20)".  
    
END PROCEDURE.

PROCEDURE pPrintImages PRIVATE:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT PARAMETER ipcCompany AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER ipcEstNo AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cPlateFolder AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cDieFolder   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE cCadFolder   AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lValidPlateFolder AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValidDieFolder AS LOGICAL NO-UNDO.
    DEFINE VARIABLE lValidCadFolder AS LOGICAL NO-UNDO.
    DEFINE VARIABLE cImageFile AS CHARACTER NO-UNDO.
    DEFINE VARIABLE lRecFound  AS LOGICAL   NO-UNDO. 
    DEFINE VARIABLE cMessage AS CHARACTER NO-UNDO.
    
    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "PlateFile",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cPlateFolder,
        OUTPUT lRecFound
        ).       
    RUN FileSys_GetFilePath(cPlateFolder, OUTPUT cPlateFolder, OUTPUT lValidPlateFolder, OUTPUT cMessage).
    IF SUBSTRING(cPlateFolder, LENGTH(cPlateFolder), 1) NE "\" THEN
        cPlateFolder = cPlateFolder + "\".                         

    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "DieFile",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cDieFolder,
        OUTPUT lRecFound
        ).
    RUN FileSys_GetFilePath(cDieFolder, OUTPUT cDieFolder, OUTPUT lValidDieFolder, OUTPUT cMessage).
    IF SUBSTRING(cDieFolder, LENGTH(cDieFolder), 1) NE "\" THEN
        cDieFolder = cDieFolder + "\".                         

    RUN sys/ref/nk1look.p(
        INPUT ipcCompany,
        INPUT "CadFile",
        INPUT "C",
        INPUT NO,
        INPUT NO,
        INPUT "",
        INPUT "",
        OUTPUT cCadFolder,
        OUTPUT lRecFound
        ).
    RUN FileSys_GetFilePath(cCADFolder, OUTPUT cCADFolder, OUTPUT lValidCADFolder, OUTPUT cMessage).    
    IF SUBSTRING(cCadFolder, LENGTH(cCadFolder), 1) NE "\" THEN
        cCadFolder = cCadFolder + "\".                         
    
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
                "<P10><C20><b>Code: </B>" STRING(bf-item.i-no) FORMAT "x(20)" 
                "<C45><b>Desc: </b>" bf-item.i-name FORMAT "x(30)" SKIP.
            
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
                    "<P10><C20><b>Code: </B>" STRING(bf-item.i-no)  FORMAT "x(20)" 
                    "<C45><b>Desc: </b>" bf-item.i-name FORMAT "x(30)" SKIP.
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
                    "<P10><C20><b>Code: </B>" STRING(bf-item.i-no)  FORMAT "x(20)" 
                    "<C45><b>Desc: </b>" bf-item.i-name FORMAT "x(30)" SKIP.        
        END.                  

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
    DEFINE OUTPUT PARAMETER opiReturnReqQty AS INTEGER NO-UNDO .
    DEFINE BUFFER bff-job-hdr FOR job-hdr.
    DEFINE BUFFER bff-oe-ordl FOR oe-ordl.
         
     FIND FIRST bff-job-hdr NO-LOCK
          WHERE bff-job-hdr.company EQ cocode 
            AND bff-job-hdr.job-no EQ ipcJobNo 
            AND bff-job-hdr.job-no2 EQ ipiJobNo2
            AND bff-job-hdr.frm EQ ipiFornNo
            AND bff-job-hdr.blank-no EQ ipiBlankNo NO-ERROR .
      opiReturnQty = IF AVAIL bff-job-hdr THEN bff-job-hdr.qty ELSE 0 .
      
      IF AVAIL bff-job-hdr THEN 
      FIND FIRST bff-oe-ordl
            WHERE bff-oe-ordl.company EQ bff-job-hdr.company
            AND bff-oe-ordl.ord-no  EQ bff-job-hdr.ord-no
            AND bff-oe-ordl.job-no  EQ bff-job-hdr.job-no
            AND bff-oe-ordl.job-no2 EQ bff-job-hdr.job-no2
            AND bff-oe-ordl.i-no    EQ bff-job-hdr.i-no
            NO-LOCK NO-ERROR.

      IF AVAIL bff-job-hdr AND bff-job-hdr.ord-no NE 0 AND NOT AVAILABLE bff-oe-ordl THEN
      FIND FIRST bff-oe-ordl
           WHERE bff-oe-ordl.company EQ bff-job-hdr.company
           AND bff-oe-ordl.ord-no  EQ bff-job-hdr.ord-no
           AND bff-oe-ordl.i-no    EQ bff-job-hdr.i-no
          NO-ERROR.
      IF AVAIL bff-job-hdr AND bff-job-hdr.ord-no NE 0 AND NOT AVAILABLE bff-oe-ordl THEN
      FIND FIRST bff-oe-ordl
           WHERE bff-oe-ordl.company EQ bff-job-hdr.company
           AND bff-oe-ordl.ord-no  EQ bff-job-hdr.ord-no
           NO-ERROR.          
      
      opiReturnReqQty = IF AVAIL bff-oe-ordl THEN bff-oe-ordl.qty ELSE IF AVAIL bff-job-hdr THEN bff-job-hdr.qty ELSE 0  . 

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



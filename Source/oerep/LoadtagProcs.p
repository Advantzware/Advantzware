
/* **********************  Internal Procedures  *********************** */
DEFINE VARIABLE cRtnChar AS CHARACTER NO-UNDO.
DEFINE VARIABLE lRecFound AS LOGICAL NO-UNDO.
DEFINE VARIABLE lBussFormModle AS LOGICAL NO-UNDO.
DEFINE VARIABLE list-name AS cha NO-UNDO.
DEFINE VARIABLE init-dir AS CHARACTER NO-UNDO.
DEFINE VARIABLE ls-full-img1 AS cha FORM "x(200)" NO-UNDO.
DEFINE VARIABLE ls-image1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFound AS LOGICAL     NO-UNDO.
DEFINE VARIABLE iCountPallet AS INTEGER NO-UNDO .

{oerep/ttLoadTag.i SHARED}
{oerep/r-loadtg.i }
{custom/xprint.i}

{sys/inc/var.i shared}
{sys/form/r-top3.f}

 DEFINE SHARED TEMP-TABLE tt-word-print LIKE w-ord 
   FIELD tag-no AS CHARACTER .
  
RUN sys/ref/nk1look.p (INPUT cocode, "BusinessFormModal", "L" /* Logical */, NO /* check by cust */, 
    INPUT YES /* use cust not vendor */, "" /* cust */, "" /* ship-to*/,
OUTPUT cRtnChar, OUTPUT lRecFound).
IF lRecFound THEN
    lBussFormModle = LOGICAL(cRtnChar) NO-ERROR.  
RUN sys/ref/nk1look.p (INPUT cocode,
                       INPUT "LoadTagXprintImage",
                       INPUT "C",
                       INPUT NO,
                       INPUT NO,
                       INPUT "",
                       INPUT "",
                       OUTPUT ls-image1,
                       OUTPUT lFound).

FILE-INFO:FILE-NAME = ls-image1.
ls-full-img1 = FILE-INFO:FULL-PATHNAME + ">".


PROCEDURE pPrintView:
DEFINE INPUT PARAMETER ipcCasLabel AS CHARACTER .
DEFINE INPUT PARAMETER iplPrintView AS LOGICAL .

DEFINE VARIABLE cEmail AS CHARACTER NO-UNDO.
DEFINE VARIABLE cPhone AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFax   AS CHARACTER NO-UNDO.

    {sys/inc/print1.i}
    {sys/inc/outprint.i value(85)}

    SESSION:SET-WAIT-STATE ("general").
   
    IF iplPrintView THEN DO:
        IF NOT lBussFormModle THEN
           PUT "<PREVIEW><MODAL=NO></PROGRESS>" FORM "x(50)".
         ELSE
           PUT "<PREVIEW></PROGRESS>" FORM "x(50)".
    END.
    ELSE DO:
       PUT "<PRINTER?><FORMAT=LEGAL></PROGRESS>" FORM "x(50)".
    END.

        iCountPallet = 0 .
        FOR EACH tt-word-print NO-LOCK BREAK
                                BY tt-word-print.ord-no 
                                BY tt-word-print.i-no:
            FIND FIRST loadtag NO-LOCK
             WHERE loadtag.company   EQ cocode
             AND loadtag.item-type EQ NO
                AND loadtag.tag-no EQ TRIM(tt-word-print.tag-no)
                USE-INDEX tag NO-ERROR.
            iCountPallet = iCountPallet + 1 .
                                
           IF ipcCasLabel EQ "loadtag.xpr" THEN DO:
               {oe/rep/lodxprntstd.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag1.xpr" THEN DO:
               {oe/rep/lodxprnt.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag2.xpr" THEN DO:
               {oe/rep/lodxprnt2.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag3.xpr" THEN DO:
               {oe/rep/lodxprnt3.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag4.xpr" THEN DO:
               {oe/rep/lodxprnt4.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag5.xpr" THEN DO:
               {oe/rep/lodxprnt5.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag6.xpr" THEN DO:
               {oe/rep/lodxprnt6.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag7.xpr" THEN DO:
               {oe/rep/lodxprnt7.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag8.xpr" THEN DO:
               {oe/rep/lodxprnt8.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag9.xpr" THEN DO:
               {oe/rep/lodxprnt9.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag10.xpr" THEN DO:
               {oe/rep/lodxprnt10.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag11.xpr" THEN DO:
               {oe/rep/lodxprnt11.i}
           END.
           ELSE IF ipcCasLabel EQ "loadtag12.xpr" THEN DO:
               {oe/rep/lodxprnt12.i} 
           END.
           ELSE IF ipcCasLabel EQ "loadtag13.xpr" THEN DO:
               {oe/rep/lodxprnt13.i}
           END.
           ELSE IF ipcCasLabel EQ "Logo.xpr" THEN DO:
               {oe/rep/lodxprntLogo.i} 
           END.
           ELSE IF ipcCasLabel EQ "NoLogo.xpr" THEN DO:
               {oe/rep/lodxprntNoLogo.i} 
           END.
    
         IF NOT LAST(tt-word-print.i-no) THEN PAGE .
        END.
   
    OUTPUT CLOSE.
    SESSION:SET-WAIT-STATE ("").

    FILE-INFO:FILE-NAME = list-name.
    RUN printfile (FILE-INFO:FILE-NAME).
 
        
END PROCEDURE.

PROCEDURE BuildLoadTagsFromJob:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipcCompany           AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcJobno             AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiJobno2            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiFormNo            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiBlankNo           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcItemID            AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantity          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnits          AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiSubUnitsPerUnit   AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiQuantityPerPallet AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPartial           AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiPalletTags        AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipiCopies            AS INTEGER   NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField1        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField2        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserField3        AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue1   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue2   AS CHARACTER NO-UNDO.
    DEFINE INPUT  PARAMETER ipcUserFieldValue3   AS CHARACTER NO-UNDO.

    DEFINE BUFFER bf-job       FOR job.
    DEFINE BUFFER bf-job-hdr   FOR job-hdr.
    DEFINE BUFFER bf-cust      FOR cust.
    DEFINE BUFFER bf-itemfg    FOR itemfg.
    DEFINE BUFFER bf-cust-part FOR cust-part.
    DEFINE BUFFER bf-style     FOR style.
    DEFINE BUFFER bf-oe-ordl   FOR oe-ordl.
    DEFINE BUFFER bf-oe-ord    FOR oe-ord.
    DEFINE BUFFER bf-shipto    FOR shipto.
    DEFINE BUFFER bf-est       FOR est.
    DEFINE BUFFER bf-eb        FOR eb.
    
    FIND FIRST bf-job NO-LOCK
         WHERE bf-job.company EQ ipcCompany
           AND bf-job.job-no  EQ ipcJobNo
           AND bf-job.job-no2 EQ ipiJobNo2
         NO-ERROR.
    IF AVAILABLE bf-job THEN
        FIND FIRST bf-job-hdr NO-LOCK
             WHERE bf-job-hdr.company  EQ ipcCompany
               AND bf-job-hdr.job      EQ bf-job.job
               AND bf-job-hdr.job-no   EQ ipcJobno
               AND bf-job-hdr.job-no2  EQ ipiJobNo2
               AND bf-job-hdr.frm      EQ ipiFormNo
               AND bf-job-hdr.blank-no EQ ipiBlankNo
               AND bf-job-hdr.i-no     EQ ipcItemID
               AND bf-job-hdr.opened   EQ TRUE
             NO-ERROR.
    
    IF NOT AVAILABLE bf-job-hdr THEN
        RETURN.             
    
    FIND FIRST bf-itemfg NO-LOCK
         WHERE bf-itemfg.company EQ bf-job-hdr.company
           AND bf-itemfg.i-no    EQ bf-job-hdr.i-no
         NO-ERROR.
    IF NOT AVAILABLE bf-itemfg THEN
        RETURN.

    FIND FIRST bf-cust NO-LOCK
         WHERE bf-cust.company EQ bf-job-hdr.company
           AND bf-cust.cust-no EQ bf-job-hdr.cust-no
         NO-ERROR.
    IF NOT AVAILABLE bf-cust THEN
        RETURN.

    IF bf-job-hdr.ord-no EQ 0 THEN DO:
        CREATE ttLoadTag.
        ASSIGN
            ttLoadTag.orderID       = bf-job-hdr.ord-no
            ttLoadTag.jobID         = bf-job-hdr.job-no
            ttLoadTag.jobID2        = bf-job-hdr.job-no2
            ttLoadTag.custID        = bf-cust.cust-no
            ttLoadTag.custName      = bf-cust.name
            ttLoadTag.itemID        = bf-job-hdr.i-no
            ttLoadTag.custPartNo    = bf-itemfg.part-no
            ttLoadTag.overPct       = 0
            ttLoadTag.qtyBefore     = bf-job-hdr.qty
            ttLoadTag.ordQuantity   = ttLoadTag.qtyBefore * (1 + (ttLoadTag.overPct / 100))
            ttLoadTag.itemName      = bf-itemfg.i-name
            ttLoadTag.upcNo         = bf-itemfg.upc-no
            ttLoadTag.dueDate       = bf-job.start-date
            ttLoadTag.estID         = bf-job.est-no
            ttLoadTag.formNo        = bf-job-hdr.frm
            ttLoadTag.boxLen        = bf-itemfg.l-score[50]
            ttLoadTag.boxWid        = bf-itemfg.w-score[50]
            ttLoadTag.boxDep        = bf-itemfg.d-score[50]
            ttLoadTag.style         = bf-itemfg.style
/*            ttLoadTag.vendor        = company.name*/
            ttLoadTag.tareWeight    = 10
            ttLoadTag.uom           = "EA"
            ttLoadTag.mult          = ipiCopies
            ttLoadTag.dueDateJob    = IF bf-job.due-date <> ? THEN STRING(bf-job.due-date, "99/99/9999") ELSE ""
            ttLoadTag.dueDateJobHdr = IF bf-job-hdr.due-date <> ? THEN STRING(bf-job-hdr.due-date, "99/99/9999") ELSE ""
            ttLoadTag.jobQuantity   = bf-job-hdr.qty
            ttLoadTag.zoneID        = bf-itemfg.spare-char-4. 
            ttLoadTag.ipReturn      = NO
            .

        FOR EACH bf-cust-part NO-LOCK 
            WHERE bf-cust-part.company EQ ipcCompany   
              AND bf-cust-part.i-no    EQ bf-itemfg.i-no 
              AND bf-cust-part.cust-no EQ bf-cust.cust-no
              AND bf-cust-part.part-no NE "":
            ttLoadTag.custPartNo = bf-cust-part.part-no.
            LEAVE.
        END.

        IF ttLoadTag.style NE "" THEN DO:
            FIND FIRST bf-style NO-LOCK
                 WHERE bf-style.company EQ ipcCompany
                   AND bf-style.style   EQ ttLoadTag.style
                 NO-ERROR.
            IF AVAILABLE bf-style THEN
                ttLoadTag.styleDesc = bf-style.dscr.
        END.

        FOR EACH bf-shipto NO-LOCK
            WHERE bf-shipto.company EQ ipcCompany
              AND bf-shipto.cust-no EQ bf-job-hdr.cust-no
            USE-INDEX ship-id
            BREAK BY bf-shipto.ship-no DESCENDING:
            IF bf-shipto.ship-id EQ bf-job-hdr.cust-no OR LAST(bf-shipto.ship-no) THEN DO:
                ASSIGN
                    ttLoadTag.shipID        = bf-shipto.ship-id
                    ttLoadTag.shipName      = bf-shipto.ship-name
                    ttLoadTag.shipAddress1  = bf-shipto.ship-add[1]
                    ttLoadTag.shipAddress2  = bf-shipto.ship-add[2]
                    ttLoadTag.shipCity      = bf-shipto.ship-city
                    ttLoadTag.shipState     = bf-shipto.ship-state
                    ttLoadTag.shipCountry   = bf-shipto.country
                    ttLoadTag.shipZip       = bf-shipto.ship-zip
                    ttLoadTag.broker        = bf-shipto.broker
                    .
                LEAVE.
            END.
        END.
   
        FIND FIRST bf-est NO-LOCK
             WHERE bf-est.company EQ bf-job.company
               AND bf-est.est-no  EQ bf-job.est-no
             NO-ERROR.

        IF AVAILABLE bf-est THEN
            FIND FIRST bf-eb NO-LOCK
                 WHERE bf-eb.company   EQ bf-est.company
                   AND bf-eb.est-no    EQ bf-est.est-no
                   AND bf-eb.form-no   EQ bf-job-hdr.frm
                   AND (bf-eb.blank-no EQ bf-job-hdr.blank-no OR bf-job-hdr.blank-no EQ 0)
                 NO-ERROR.

        IF AVAILABLE bf-eb THEN
            ASSIGN
                ttLoadTag.flute      = bf-eb.flute
                ttLoadTag.test       = bf-eb.test
                ttLoadTag.caseNo     = bf-eb.cas-no
                ttLoadTag.palletID   = bf-eb.tr-no
                ttLoadTag.partDscr2  = bf-eb.part-dscr2
                .

/*        IF AVAILABLE bf-eb AND bf-eb.est-type EQ 2 THEN                                                              */
/*            ttLoadTag.jobQuantity = bf-job-hdr.qty * (IF bf-eb.cust-% GT 0 THEN bf-eb.cust-% ELSE 1)  .              */
/*        ELSE IF AVAILABLE bf-eb AND bf-eb.est-type EQ 6  THEN                                                        */
/*            ttLoadTag.jobQuantity = bf-job-hdr.qty * (IF bf-eb.quantityPerSet GT 0 THEN bf-eb.quantityPerSet ELSE 1).*/

        ASSIGN
            ttLoadTag.jobQuantity = ipiQuantity
            ttLoadTag.pcs         = ipiQuantityPerPallet
            ttLoadTag.bundle      = TRUNC(ipiQuantity / ipiQuantityPerPallet, 0)
            ttLoadTag.partial     = ipiPartial
            ttLoadTag.totalUnit   = ipiQuantity
            ttLoadTag.totalTags   = ipiPalletTags
            .

        IF ipcUserField1 EQ "Lot Number" THEN
            ttLoadTag.lotID = ipcUserFieldValue1.
        ELSE IF ipcUserField2 EQ "Lot Number" THEN
            ttLoadTag.lotID = ipcUserFieldValue2.
        ELSE IF ipcUserField3 EQ "Lot Number" THEN
            ttLoadTag.lotID = ipcUserFieldValue3.
                
        IF ipcUserField1 EQ "Weight" THEN
            ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue1).
        ELSE IF ipcUserField2 EQ "Weight" THEN
            ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue2).
        ELSE IF ipcUserField3 EQ "Weight" THEN
            ttLoadTag.unitWeight = DECIMAL(ipcUserFieldValue3).

        IF ipcUserField1 EQ "CustPo" THEN
            ttLoadTag.custPONo = ipcUserFieldValue1.
        ELSE IF ipcUserField2 EQ "CustPo" THEN
            ttLoadTag.custPONo = ipcUserFieldValue2.
        ELSE IF ipcUserField3 EQ "CustPo" THEN
            ttLoadTag.custPONo = ipcUserFieldValue3.
             
        IF ttLoadTag.partial EQ ? THEN 
            ttLoadTag.partial = 0. 
    END. 
END PROCEDURE.

PROCEDURE pGetReleaseInfo:
/*------------------------------------------------------------------------------
      Purpose: Given a job-hdr returns the release details
      Parameters:  <none>
      Notes: This is a replacement procedure for get-rel-info     
    ------------------------------------------------------------------------------*/
    DEFINE INPUT  PARAMETER ipriJobHdr   AS ROWID     NO-UNDO.
    DEFINE OUTPUT PARAMETER opcCustPONO  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opdtRelDate  AS DATE      NO-UNDO.
    DEFINE OUTPUT PARAMETER opcRelLotID  AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER opcShipNotes AS CHARACTER NO-UNDO EXTENT 4.
    DEFINE OUTPUT PARAMETER opiRelQty    AS INTEGER   NO-UNDO.

    DEFINE BUFFER bf-job-hdr FOR job-hdr.
    DEFINE BUFFER bf-oe-rell FOR oe-rell.
    DEFINE BUFFER bf-oe-ordl FOR oe-ordl.
    DEFINE BUFFER bf-oe-rel  FOR oe-rel.
    DEFINE BUFFER bf-oe-relh FOR oe-relh.
    
    FIND FIRST bf-job-hdr NO-LOCK
         WHERE ROWID(bf-job-hdr) EQ ipriJobHdr
         NO-ERROR .
    IF NOT AVAILABLE bf-job-hdr THEN
        RETURN.
    
    IF bf-job-hdr.ord-no EQ 0 THEN
        RETURN.

    FIND FIRST bf-oe-ordl NO-LOCK
         WHERE bf-oe-ordl.company EQ bf-job-hdr.company
           AND bf-oe-ordl.ord-no  EQ bf-job-hdr.ord-no
           AND bf-oe-ordl.i-no    EQ bf-job-hdr.i-no
         NO-ERROR.
    
    IF NOT AVAILABLE bf-oe-ordl THEN
        RETURN.

    FOR EACH bf-oe-rell NO-LOCK
        WHERE bf-oe-rell.company  EQ bf-oe-ordl.company
          AND bf-oe-rell.ord-no   EQ bf-oe-ordl.ord-no
          AND bf-oe-rell.i-no     EQ bf-oe-ordl.i-no
          AND bf-oe-rell.line     EQ bf-oe-ordl.line,
        FIRST bf-oe-relh NO-LOCK
        WHERE bf-oe-relh.r-no     EQ bf-oe-rell.r-no
           BY bf-oe-relh.rel-date
           BY bf-oe-relh.r-no:

        ASSIGN
            opcCustPONo     = bf-oe-rell.po-no
            opdtRelDate     = bf-oe-relh.rel-date
            opcShipNotes[1] = bf-oe-relh.ship-i[1]
            opcShipNotes[2] = bf-oe-relh.ship-i[2]
            opcShipNotes[3] = bf-oe-relh.ship-i[3]
            opcShipNotes[4] = bf-oe-relh.ship-i[4]
            opiRelQty       = bf-oe-rell.qty 
            .
        LEAVE.
    END.

    IF AVAILABLE bf-oe-rell THEN
        FIND FIRST bf-oe-rel NO-LOCK
             WHERE bf-oe-rel.r-no EQ bf-oe-rell.link-no
             NO-ERROR.
    ELSE DO:
        FOR EACH bf-oe-rel NO-LOCK
            WHERE bf-oe-rel.company  EQ bf-oe-ordl.company
              AND bf-oe-rel.ord-no   EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.i-no     EQ bf-oe-ordl.i-no
              AND bf-oe-rel.line     EQ bf-oe-ordl.line
              AND bf-oe-rel.rel-no   EQ 0
            BY bf-oe-rel.rel-date
            BY bf-oe-rel.r-no:
            ASSIGN
                opcCustPONo     = (IF bf-oe-rel.po-no GT "" THEN bf-oe-rel.po-no ELSE bf-oe-ordl.po-no)
                opdtRelDate     = bf-oe-rel.rel-date
                opcShipNotes[1] = bf-oe-rel.ship-i[1]
                opcShipNotes[2] = bf-oe-rel.ship-i[2]
                opcShipNotes[3] = bf-oe-rel.ship-i[3]
                opcShipNotes[4] = bf-oe-rel.ship-i[4]
                .
            LEAVE.
        END.
    END.  

    IF NOT AVAILABLE bf-oe-rel THEN DO:
        FOR EACH bf-oe-rel NO-LOCK
            WHERE bf-oe-rel.company  EQ bf-oe-ordl.company
              AND bf-oe-rel.ord-no   EQ bf-oe-ordl.ord-no
              AND bf-oe-rel.i-no     EQ bf-oe-ordl.i-no
              AND bf-oe-rel.line     EQ bf-oe-ordl.line
               BY bf-oe-rel.rel-date
               BY bf-oe-rel.r-no:

            ASSIGN 
                opcCustPONo     = (IF bf-oe-rel.po-no GT "" THEN bf-oe-rel.po-no ELSE bf-oe-ordl.po-no)
                opdtRelDate     = bf-oe-rel.rel-date
                opcShipNotes[1] = bf-oe-rel.ship-i[1]
                opcShipNotes[2] = bf-oe-rel.ship-i[2]
                opcShipNotes[3] = bf-oe-rel.ship-i[3]
                opcShipNotes[4] = bf-oe-rel.ship-i[4]
                .
            LEAVE.
        END.
    END.
    
    IF AVAILABLE bf-oe-rel THEN 
        ASSIGN opcRelLotID = bf-oe-rel.lot-no.

    opcCustPONo = bf-job-hdr.po-no.
END PROCEDURE.
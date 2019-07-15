

/*------------------------------------------------------------------------
    File        : BrwsCEstimate.p
    Purpose     : Corrugated Box
    Syntax      :

    Description : Return a Dataset of Estimate Corrugated box
    Author(s)   : 
    Created     : 14 Jan 2009 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttViewFGrcpt NO-UNDO
        FIELD vRno            AS INT 
        FIELD vDate           AS CHAR  
        FIELD vTransTime      AS CHAR   
        FIELD vTag            AS CHAR FORMAT "x(20)"
        FIELD vPo_no          AS CHAR FORMAT "x(9)"
        FIELD vJob_no         AS CHAR FORMAT "x(6)"  
        FIELD vJob_no2        AS INT
        FIELD vItem           AS CHAR FORMAT "x(15)"
        FIELD vItemName       AS CHAR FORMAT "x(30)"
        FIELD vLoc            AS CHAR  FORMAT "x(5)" 
        FIELD vLocBin         AS CHAR  FORMAT "x(8)"  
        FIELD vCases          AS INT 
        FIELD vQtyCas         AS INT 
        FIELD vCasUnit        AS INT    
        FIELD vPartial        AS INT    
        FIELD vStdCost        AS DEC
        FIELD vCostUom        AS CHAR FORMAT "x(3)"
        FIELD vT_Qty          AS DEC
        FIELD vFrtCost        AS DEC
        FIELD vExtCost        AS DEC
        FIELD vStackCode      AS CHAR FORMAT "x(20)"   
        FIELD vCreatedBy      AS CHAR   FORMAT "x(9)"
        FIELD vCreate2        AS CHAR  FORMAT "x(9)"
        FIELD vTot_Wt         AS DEC
        FIELD vRecKey         AS CHAR 
        .
DEF VAR lv-frst-rno AS INT NO-UNDO.
DEF VAR lv-linker LIKE fg-rcpts.linker NO-UNDO.
DEF VAR lv-setprt AS LOG NO-UNDO.
DEF BUFFER b-fg-rctd FOR fg-rctd.  /* for tag validation */
DEF BUFFER b-fg-rdtlh FOR fg-rdtlh. /* for tag validation */
DEF BUFFER b2-fg-rdtlh FOR fg-rdtlh. /* for tag validation */
DEF BUFFER reftable-job FOR reftable.
DEF BUFFER b-po-ord FOR po-ord.
DEF BUFFER b-company FOR company.
DEF VAR ll-set-parts AS LOG NO-UNDO.
def var fg-uom-list  as char NO-UNDO.

DEFINE DATASET dsViewFGrcpt FOR ttViewFGrcpt .

DEFINE INPUT PARAMETER prmUser           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmAction         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFgItem         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJobno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPono           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmSeqno          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRcptDate       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTagno          AS CHAR        NO-UNDO. 
DEFINE INPUT PARAMETER prmTransTime      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmJob_no2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmName           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLoc            AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmLocBin         AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCases          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmQty_Cas        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCasUnit        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmPartial        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmStdCost        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCost_Uom       AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTQty           AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmFrtCost        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmExtCost        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmStackCode      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCreatedBy      AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmCreate2        AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmTotWt          AS CHAR        NO-UNDO.
DEFINE INPUT PARAMETER prmRecKey         AS CHAR        NO-UNDO.
DEFINE OUTPUT PARAMETER cError            AS CHAR        NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewFGrcpt.

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
/*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.*/
DEF NEW SHARED VAR vUser AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-next-tag AS cha NO-UNDO.
{sys/inc/var.i "new shared"}
 {fg/invrecpt.i NEW}

IF prmUser          = ? THEN ASSIGN prmUser        = "".
IF prmAction        = ? THEN ASSIGN prmAction      = "Select".
IF prmFgItem        = ? THEN ASSIGN prmFgItem      = "".
IF prmJobno         = ? THEN ASSIGN prmJobno       = "".  
IF prmPono          = ? THEN ASSIGN prmPono        = "".
IF prmSeqno         = ? THEN ASSIGN prmSeqno       = "".
IF prmRcptDate      = ? THEN ASSIGN prmRcptDate    = "".
IF prmTagno         = ? THEN ASSIGN prmTagno       = "".
IF prmTransTime     = ? THEN ASSIGN prmTransTime   = "".
IF prmJob_no2       = ? THEN ASSIGN prmJob_no2     = "0".
IF prmName          = ? THEN ASSIGN prmName    = "".
IF prmLoc           = ? THEN ASSIGN prmLoc         = "".
IF prmLocBin        = ? THEN ASSIGN prmLocBin      = "".
IF prmCases         = ? THEN ASSIGN prmCases       = "0".
IF prmQty_Cas        = ? THEN ASSIGN prmQty_Cas      = "0".
IF prmCasUnit       = ? THEN ASSIGN prmCasUnit     = "0".
IF prmPartial       = ? THEN ASSIGN prmPartial     = "0".
IF prmStdCost       = ? THEN ASSIGN prmStdCost     = "0".
IF prmCost_Uom       = ? THEN ASSIGN prmCost_Uom     = "".
IF prmTQty          = ? THEN ASSIGN prmTQty        = "0".
IF prmFrtCost       = ? THEN ASSIGN prmFrtCost     = "0".
IF prmExtCost       = ? THEN ASSIGN prmExtCost     = "0".
IF prmStackCode     = ? THEN ASSIGN prmStackCode   = "".
IF prmCreatedBy     = ? THEN ASSIGN prmCreatedBy   = "".
IF prmCreate2       = ? THEN ASSIGN prmCreate2     = "".
IF prmTotWt         = ? THEN ASSIGN prmTotWt       = "0".
IF prmRecKey        = ? THEN ASSIGN prmRecKey      = "".
IF cError           = ? THEN ASSIGN cError         = "".



FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN
    cocode = prmComp
    g_company = prmComp 
    vUser = prmUser .


DO TRANSACTION:
  {sys/inc/fgpofrt.i}
  {sys/inc/fgrecpt.i}
  {sys/inc/autopost.i}
  {sys/inc/fgsetrec.i}
  {sys/inc/oeship.i}
  {sys/inc/fgsecur.i}
  {sys/inc/rfidtag.i}
END.
  
RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).

find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOTAG#"
    no-lock no-error.
v-auto-add-tag = NO.
IF AVAIL sys-ctrl THEN
  v-auto-add-tag = sys-ctrl.log-fld.

DEF TEMP-TABLE tt-fg-rctd LIKE fg-rctd
    FIELD tt-rowid AS ROWID
    FIELD po-rowid AS ROWID.


   FOR EACH usercust WHERE usercust.user_id = prmUser AND 
            usercust.company = prmComp  NO-LOCK:
       ASSIGN 
         custcount = custcount + "," + usercust.cust-no .

       END. /*FOR EACH usercust*/ 

RUN get-first-r-no.
RUN get-linker.   

IF prmAction = "Update" THEN DO: 
   
  FIND FIRST fg-rctd WHERE fg-rctd.company = cocode AND fg-rctd.r-no EQ int(prmSeqno) NO-LOCK NO-ERROR.

  FIND FIRST loc WHERE loc.company = cocode
      AND loc.loc = prmLoc NO-LOCK NO-ERROR.
  IF NOT AVAIL loc THEN DO:
      cError =  "Invalid Warehouse. Try Help. " .
      RETURN .
  END.

  FIND FIRST fg-bin WHERE fg-bin.company = cocode
      AND fg-bin.i-no = ""
      AND fg-bin.loc = prmLoc
      AND fg-bin.loc-bin = prmLocBin
      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
      cError = "Invalid Bin#. Try Help. " .
      RETURN .
  END.

    IF prmLoc     EQ ""      OR prmLocBin  EQ ""    OR
        INT(prmQty_Cas) EQ 0 OR prmCost_Uom = ""    OR
        DEC(prmStdCost) EQ 0 THEN
        RUN get-values.

    IF INT(prmPono) NE 0 AND prmFgItem NE ""      THEN DO:

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(prmPono)
            AND po-ordl.item-type EQ NO NO-LOCK NO-ERROR.
        IF AVAIL po-ordl THEN do:
            FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(prmPono)
            AND po-ordl.item-type EQ NO
            AND (po-ordl.i-no     EQ prmFgItem OR
                 prmFgItem EQ "") NO-LOCK NO-ERROR.

                IF NOT  AVAIL po-ordl THEN DO:
                    cError = "FG does not exist on PO..." .
                    RETURN .
                END.
        END.
     END.

     find first itemfg where (itemfg.company  = cocode ) and 
         (itemfg.i-no = prmFgItem ) no-lock no-error.
     IF NOT AVAIL itemfg THEN DO:
         cError =  "Invalid Item. Try help. " .
         RETURN .
     END.

     IF oeship-log                   AND
         CAN-FIND(FIRST itemfg
                  WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ prmFgItem
                  AND itemfg.isaset
                  AND itemfg.alloc) THEN DO:
         cError = "Item may not be an unassembled set header...".
         RETURN .
     END.

     FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
         AND reftable.company  EQ cocode
         AND reftable.loc      EQ ""
         AND reftable.code     EQ prmFgItem
         NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
         cError =  prmFgItem + " has InActive Status. Receipt cannot be created for the Inactive Item." .
         RETURN .
     END. 
    
     IF date(prmRcptDate) > DATE("12/31/2099") THEN DO:
         ASSIGN 
             cError ="Date lies between 01/01/1990 & 12/31/2099".
         RETURN.
     END.

      IF date(prmRcptDate) < DATE("01/01/1990") THEN DO:
          ASSIGN 
              cError ="Date lies between 01/01/1990 & 12/31/2099".
          RETURN.
      END.

      /*RUN get-matrix (NO).     */
      /*RUN valid-tag.*/

      ASSIGN lv-msg = "" .
      prmTagno = CAPS(prmTagno).
      IF prmTagno NE "" THEN DO:
          
          IF lv-msg EQ ""                                           AND
              (CAN-FIND(FIRST b-fg-rctd
                        WHERE b-fg-rctd.company EQ cocode
                        AND b-fg-rctd.tag     EQ prmTagno
                        AND RECID(b-fg-rctd)  NE RECID(fg-rctd)) OR
               CAN-FIND(FIRST b-fg-rdtlh
                        WHERE b-fg-rdtlh.company   EQ cocode
                        AND b-fg-rdtlh.tag       EQ prmTagno
                        AND b-fg-rdtlh.qty       GT 0
                        AND b-fg-rdtlh.rita-code NE "S"))          AND
              (INTE(prmCases) > 0 OR
               CAN-FIND(FIRST b2-fg-rdtlh
                        WHERE b2-fg-rdtlh.company   EQ cocode
                        AND b2-fg-rdtlh.tag       EQ prmTagno
                        AND b2-fg-rdtlh.qty       LT ABS(INTE(prmCases))
                        AND b2-fg-rdtlh.rita-code NE "S") ) THEN
              lv-msg = "Tag# has already been used, please re-enter".
          
          /* IF lv-msg EQ "" AND v-copy-mode AND prmTagno NE "" AND
          CAN-FIND(FIRST b-fg-rctd
          WHERE b-fg-rctd.company EQ cocode
          AND b-fg-rctd.tag     EQ prmTagno
          AND b-fg-rctd.r-no    NE 0) THEN
          lv-msg = "Tag# has already been used, please re-enter".*/
          
          IF lv-msg EQ ""                                                   AND
              /*fgrecpt-int EQ 1                                               AND*/
              NOT CAN-FIND(FIRST loadtag
                           WHERE loadtag.company   EQ cocode
                           AND loadtag.item-type EQ NO
                           AND loadtag.tag-no    EQ prmTagno) THEN
              lv-msg = "Invalid Tag#, try help or scan valid tag#".
          IF lv-msg NE "" THEN DO:
              ASSIGN cError = lv-msg .
              RETURN .
          END.
       END.

       /*RUN valid-lot#.*/
       IF prmStackCode NE "" THEN DO:
           IF prmTagno = ""  THEN DO:
               cError =  TRIM(prmStackCode) + " may not be entered when tag# is blank".
               RETURN .
           END.
      END.

      /*RUN valid-po-no (1).          */
      IF (prmPono) NE ""              AND
          NOT CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN DO:
          IF prmJobno NE "" THEN DO:
              cError =  "You may only enter a Job or a PO, PO will be erased..." .
              RETURN.
          END.

          FIND FIRST po-ordl
              WHERE po-ordl.company   EQ cocode
              AND po-ordl.po-no     EQ INT(prmPono)
              AND po-ordl.item-type EQ NO
              AND (po-ordl.i-no     EQ prmFgItem OR prmFgItem EQ "")
              NO-LOCK NO-ERROR.
          IF NOT AVAIL po-ordl THEN DO:
              cError = "Invalid PO#, try help..." .
              RETURN .
          END.

          /*ASSIGN fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.i-name.*/

          FIND FIRST po-ord
              WHERE po-ord.company EQ po-ordl.company
              AND po-ord.po-no   EQ po-ordl.po-no
              NO-LOCK NO-ERROR.
          
          /* IF ip-type EQ 1                   AND
          AVAIL po-ord                   AND
          adm-adding-record              AND
          NOT CAN-FIND(FIRST tt-fg-rctd) THEN DO:
          
          RUN fg/d-selpos.w (ROWID(po-ord), NO).
          
          RUN create-from-po.       
          
          RUN update-ttt.
          END.*/
    END.  


    /* RUN valid-job-no.*/
    prmJobno = FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno).

    IF prmJobno EQ "" THEN DO:
        IF fgrecpt                                                AND
            prmPono EQ "" AND
            fg-rctd.rita-code NE "E"                                  THEN DO:
            cError =  "You must enter a Job or a PO..." .
            RETURN .
         END.
     END.

     ELSE DO:
         IF (prmPono) NE "" THEN DO:
             prmJobno = "".
             cError = "You may only enter a Job or a PO, Job No will be erased..." .
             RETURN.
         END.

         FIND FIRST job-hdr
             WHERE job-hdr.company EQ cocode
             AND job-hdr.job-no  EQ prmJobno
             NO-LOCK NO-ERROR.
         IF NOT AVAIL job-hdr THEN DO:
             cError =  "Invalid Job#. Try Help..." .
             RETURN .
         END.
      END.

      /* RUN valid-job-no2.*/
      IF prmJobno NE "" THEN DO:
          FOR EACH job-hdr
              WHERE job-hdr.company EQ cocode
              AND job-hdr.job-no  EQ prmJobno
              AND job-hdr.job-no2 EQ INT(prmJob_no2)
              NO-LOCK,
              FIRST job
              WHERE job.company EQ job-hdr.company
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ job-hdr.job-no
              AND job.job-no2 EQ job-hdr.job-no2
              NO-LOCK:
              LEAVE.
         END.

         IF NOT AVAIL job-hdr THEN
             FOR EACH job
             WHERE job.company EQ fg-rctd.company
             AND job.job-no  EQ prmJobno
             AND job.job-no2 EQ INT(prmJob_no2)
             NO-LOCK,
             FIRST job-hdr
             WHERE job-hdr.company EQ job.company
             AND job-hdr.job     EQ job.job
             AND job-hdr.job-no  EQ job.job-no
             AND job-hdr.job-no2 EQ job.job-no2
             NO-LOCK:
         LEAVE.
       END.


       IF NOT AVAIL job-hdr THEN DO:
           cError =  "Invalid Job#. Try Help..." .
           RETURN.
       END. 
    END.

  /*RUN validate-record.*/
            
END.  /*** update*/  

IF prmAction = "Update" THEN DO:
  ASSIGN
      ll-set-parts = NO.
    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and 
            fg-rctd.r-no EQ int(prmSeqno) AND
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL fg-rctd AND AVAIL reftable THEN
          ASSIGN
            fg-rctd.rct-date    = date(prmRcptDate)
            fg-rctd.trans-time  = TIME          
            fg-rctd.tag         = prmTagno                  
            fg-rctd.po-no       = prmPono                
            fg-rctd.job-no      = prmJobno                  
            fg-rctd.job-no2     = int(prmJob_no2)
            fg-rctd.i-no        = prmFgItem                  
            fg-rctd.i-name      = prmName    
            fg-rctd.loc         = prmLoc           
            fg-rctd.loc-bin     = prmLocBin        
            fg-rctd.cases       = int(prmCases)
            fg-rctd.qty-case    = int(prmQty_Cas)
            fg-rctd.cases-unit  = int(prmCasUnit)
            fg-rctd.partial     = int(prmPartial)
            fg-rctd.std-cost    = decimal(prmStdCost)
            fg-rctd.cost-uom    = prmCost_Uom     
            fg-rctd.t-qty       = dec(prmTQty)
            fg-rctd.frt-cost    = dec(prmFrtCost)
            fg-rctd.ext-cost    = dec(prmExtCost)
            fg-rctd.stack-code  = prmStackCode     
            reftable.code2      = prmUser    
            fg-rctd.tot-wt      = decimal(prmTotWt)     .

            IF (fg-rctd.cases * fg-rctd.qty-case) NE 0 OR
                fg-rctd.partial NE 0                         THEN
                fg-rctd.t-qty = (fg-rctd.cases * fg-rctd.qty-case) + fg-rctd.partial.


      RUN get-matrix (NO).

      

      IF ll-set-parts THEN DO:
          FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ fg-rctd.r-no EXCLUSIVE-LOCK NO-ERROR.
          IF NOT AVAIL fg-rcpts THEN DO:
              CREATE fg-rcpts.
              fg-rcpts.r-no       = fg-rctd.r-no.
          END.
      ASSIGN
          fg-rcpts.company    = cocode
          fg-rcpts.i-no       = fg-rctd.i-no
          fg-rcpts.i-name     = fg-rctd.i-name
          fg-rcpts.trans-date = fg-rctd.rct-date
          fg-rcpts.linker     = lv-linker.
      END.
      ELSE RUN fg/comprcpt.p (ROWID(fg-rctd)).
      
      /*IF v-auto-add-tag AND fg-rctd.tag EQ "" THEN DO:
          RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag).
          RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
          fg-rctd.tag = v-next-tag.
      END.*/

      RUN fg/invrecpt.p (ROWID(fg-rctd), 1).
      
      
      ASSIGN prmAction = "Select" .


END.  /**** update  ***/ 

IF prmAction = "Addrcpt" THEN DO:

    FIND FIRST loc WHERE loc.company = cocode
      AND loc.loc = prmLoc NO-LOCK NO-ERROR.
  IF NOT AVAIL loc THEN DO:
      cError =  "Invalid Warehouse. Try Help. " .
      RETURN .
  END.

  FIND FIRST fg-bin WHERE fg-bin.company = cocode
      AND fg-bin.i-no = ""
      AND fg-bin.loc = prmLoc
      AND fg-bin.loc-bin = prmLocBin
      USE-INDEX co-ino NO-LOCK NO-ERROR.
  IF NOT AVAIL fg-bin THEN DO:
      cError = "Invalid Bin#. Try Help. " .
      RETURN .
  END.

    IF prmLoc     EQ ""      OR prmLocBin  EQ ""    OR
        INT(prmQty_Cas) EQ 0 OR prmCost_Uom = ""    OR
        DEC(prmStdCost) EQ 0 THEN
        RUN get-values.

    IF INT(prmPono) NE 0 AND prmFgItem NE ""      THEN DO:

        FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(prmPono)
            AND po-ordl.item-type EQ NO NO-LOCK NO-ERROR.
        IF AVAIL po-ordl THEN do:
            FIND FIRST po-ordl
            WHERE po-ordl.company   EQ cocode
            AND po-ordl.po-no     EQ INT(prmPono)
            AND po-ordl.item-type EQ NO
            AND (po-ordl.i-no     EQ prmFgItem OR
                 prmFgItem EQ "") NO-LOCK NO-ERROR.

                IF NOT  AVAIL po-ordl THEN DO:
                    cError = "FG does not exist on PO..." .
                    RETURN .
                END.
        END.
     END.

     find first itemfg where (itemfg.company  = cocode ) and 
         (itemfg.i-no = prmFgItem ) no-lock no-error.
     IF NOT AVAIL itemfg THEN DO:
         cError =  "Invalid Item. Try help. " .
         RETURN .
     END.

     IF oeship-log                   AND
         CAN-FIND(FIRST itemfg
                  WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ prmFgItem
                  AND itemfg.isaset
                  AND itemfg.alloc) THEN DO:
         cError = "Item may not be an unassembled set header...".
         RETURN .
     END.

     FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
         AND reftable.company  EQ cocode
         AND reftable.loc      EQ ""
         AND reftable.code     EQ prmFgItem
         NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
         cError =  prmFgItem + " has InActive Status. Receipt cannot be created for the Inactive Item." .
         RETURN .
     END. 
    
     IF date(prmRcptDate) > DATE("12/31/2099") THEN DO:
         ASSIGN 
             cError ="Date lies between 01/01/1990 & 12/31/2099".
         RETURN.
     END.

      IF date(prmRcptDate) < DATE("01/01/1990") THEN DO:
          ASSIGN 
              cError ="Date lies between 01/01/1990 & 12/31/2099".
          RETURN.
      END.

      /*RUN get-matrix (NO).     */
      /*RUN valid-tag.*/

      ASSIGN lv-msg = "" .
      prmTagno = CAPS(prmTagno).
      IF prmTagno NE "" THEN DO:
          
          IF lv-msg EQ ""                                           AND
              (CAN-FIND(FIRST b-fg-rctd
                        WHERE b-fg-rctd.company EQ cocode
                        AND b-fg-rctd.tag     EQ prmTagno
                        AND RECID(b-fg-rctd)  NE RECID(fg-rctd)) OR
               CAN-FIND(FIRST b-fg-rdtlh
                        WHERE b-fg-rdtlh.company   EQ cocode
                        AND b-fg-rdtlh.tag       EQ prmTagno
                        AND b-fg-rdtlh.qty       GT 0
                        AND b-fg-rdtlh.rita-code NE "S"))          AND
              (INTE(prmCases) > 0 OR
               CAN-FIND(FIRST b2-fg-rdtlh
                        WHERE b2-fg-rdtlh.company   EQ cocode
                        AND b2-fg-rdtlh.tag       EQ prmTagno
                        AND b2-fg-rdtlh.qty       LT ABS(INTE(prmCases))
                        AND b2-fg-rdtlh.rita-code NE "S") ) THEN
              lv-msg = "Tag# has already been used, please re-enter".
          
          /* IF lv-msg EQ "" AND v-copy-mode AND prmTagno NE "" AND
          CAN-FIND(FIRST b-fg-rctd
          WHERE b-fg-rctd.company EQ cocode
          AND b-fg-rctd.tag     EQ prmTagno
          AND b-fg-rctd.r-no    NE 0) THEN
          lv-msg = "Tag# has already been used, please re-enter".*/
          
          IF lv-msg EQ ""                                                   AND
              /*fgrecpt-int EQ 1                                               AND*/
              NOT CAN-FIND(FIRST loadtag
                           WHERE loadtag.company   EQ cocode
                           AND loadtag.item-type EQ NO
                           AND loadtag.tag-no    EQ prmTagno) THEN
              lv-msg = "Invalid Tag#, try help or scan valid tag#".
          IF lv-msg NE "" THEN DO:
              ASSIGN cError = lv-msg .
              RETURN .
          END.
       END.

       /*RUN valid-lot#.*/
       IF prmStackCode NE "" THEN DO:
           IF prmTagno = ""  THEN DO:
               cError =  TRIM(prmStackCode) + " may not be entered when tag# is blank".
               RETURN .
           END.
      END.

      /*RUN valid-po-no (1).          */
      IF (prmPono) NE ""              AND
          NOT CAN-FIND(FIRST tt-fg-rctd WHERE tt-fg-rctd.tt-rowid EQ ROWID(fg-rctd)) THEN DO:
          IF prmJobno NE "" THEN DO:
              cError =  "You may only enter a Job or a PO, PO will be erased..." .
              RETURN.
          END.

          FIND FIRST po-ordl
              WHERE po-ordl.company   EQ cocode
              AND po-ordl.po-no     EQ INT(prmPono)
              AND po-ordl.item-type EQ NO
              AND (po-ordl.i-no     EQ prmFgItem OR prmFgItem EQ "")
              NO-LOCK NO-ERROR.
          IF NOT AVAIL po-ordl THEN DO:
              cError = "Invalid PO#, try help..." .
              RETURN .
          END.

          /*ASSIGN fg-rctd.i-name:SCREEN-VALUE IN BROWSE {&browse-name} = po-ordl.i-name.*/

          FIND FIRST po-ord
              WHERE po-ord.company EQ po-ordl.company
              AND po-ord.po-no   EQ po-ordl.po-no
              NO-LOCK NO-ERROR.
          
          /* IF ip-type EQ 1                   AND
          AVAIL po-ord                   AND
          adm-adding-record              AND
          NOT CAN-FIND(FIRST tt-fg-rctd) THEN DO:
          
          RUN fg/d-selpos.w (ROWID(po-ord), NO).
          
          RUN create-from-po.       
          
          RUN update-ttt.
          END.*/
    END.  


    /* RUN valid-job-no.*/
    prmJobno = FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno).

    IF prmJobno EQ "" THEN DO:
        IF fgrecpt                                                AND
            prmPono EQ "" AND
            fg-rctd.rita-code NE "E"                                  THEN DO:
            cError =  "You must enter a Job or a PO..." .
            RETURN .
         END.
     END.

     ELSE DO:
         IF (prmPono) NE "" THEN DO:
             prmJobno = "".
             cError = "You may only enter a Job or a PO, Job No will be erased..." .
             RETURN.
         END.

         FIND FIRST job-hdr
             WHERE job-hdr.company EQ cocode
             AND job-hdr.job-no  EQ prmJobno
             NO-LOCK NO-ERROR.
         IF NOT AVAIL job-hdr THEN DO:
             cError =  "Invalid Job#. Try Help..." .
             RETURN .
         END.
      END.

      /* RUN valid-job-no2.*/
      IF prmJobno NE "" THEN DO:
          FOR EACH job-hdr
              WHERE job-hdr.company EQ cocode
              AND job-hdr.job-no  EQ prmJobno
              AND job-hdr.job-no2 EQ INT(prmJob_no2)
              NO-LOCK,
              FIRST job
              WHERE job.company EQ job-hdr.company
              AND job.job     EQ job-hdr.job
              AND job.job-no  EQ job-hdr.job-no
              AND job.job-no2 EQ job-hdr.job-no2
              NO-LOCK:
              LEAVE.
         END.

         IF NOT AVAIL job-hdr THEN
             FOR EACH job
             WHERE job.company EQ fg-rctd.company
             AND job.job-no  EQ prmJobno
             AND job.job-no2 EQ INT(prmJob_no2)
             NO-LOCK,
             FIRST job-hdr
             WHERE job-hdr.company EQ job.company
             AND job-hdr.job     EQ job.job
             AND job-hdr.job-no  EQ job.job-no
             AND job-hdr.job-no2 EQ job.job-no2
             NO-LOCK:
         LEAVE.
       END.


       IF NOT AVAIL job-hdr THEN DO:
           cError =  "Invalid Job#. Try Help..." .
           RETURN.
       END. 
    END.


END.   /* validate of add***/

IF prmAction = "Addnewrcpt" THEN DO:
 DEF VAR lv-rno LIKE fg-rctd.r-no NO-UNDO.
  /*DEF BUFFER b-fg-rctd FOR fg-rctd.*/
  ASSIGN
      ll-set-parts = NO.
  
  FIND LAST b-fg-rctd USE-INDEX fg-rctd NO-LOCK NO-ERROR.
  IF AVAIL b-fg-rctd AND b-fg-rctd.r-no GT lv-rno THEN lv-rno = b-fg-rctd.r-no.

  FIND LAST fg-rcpth USE-INDEX r-no NO-LOCK NO-ERROR.
  IF AVAIL fg-rcpth AND fg-rcpth.r-no GT lv-rno THEN lv-rno = fg-rcpth.r-no.

  /* Code placed here will execute AFTER standard behavior.    */
  DO WHILE TRUE:
    lv-rno = lv-rno + 1.
    FIND FIRST fg-rcpth WHERE fg-rcpth.r-no EQ lv-rno USE-INDEX r-no NO-LOCK NO-ERROR.
    IF AVAIL fg-rcpth THEN NEXT.
    FIND FIRST b-fg-rctd WHERE b-fg-rctd.r-no EQ lv-rno USE-INDEX fg-rctd NO-LOCK NO-ERROR.
    IF AVAIL b-fg-rctd THEN NEXT.
    LEAVE.
  END.

  CREATE fg-rctd .
  
    ASSIGN 
     fg-rctd.rct-date    = TODAY
     fg-rctd.trans-time   = TIME
     fg-rctd.units-pallet = 1
     fg-rctd.cases-unit   = 1
     fg-rctd.ext-cost     = 0
     fg-rctd.partial      = 0
     fg-rctd.qty          = 0
     fg-rctd.qty-case     = 0.

  ASSIGN
   fg-rctd.company   = cocode
   fg-rctd.r-no      = lv-rno
   fg-rctd.rita-code = "R"
   /* gdm - */
   fg-rctd.trans-time   = TIME .

      FIND FIRST reftable
         WHERE reftable.reftable EQ "fg-rctd.user-id"
           AND reftable.company  EQ fg-rctd.company
           AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
         NO-ERROR.
     IF NOT AVAIL reftable THEN DO:
       CREATE reftable.
       ASSIGN
        reftable.reftable = "fg-rctd.user-id"
        reftable.company  = fg-rctd.company
        reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
        reftable.code     = prmUser.
     END.
     ASSIGN
      reftable.code2        = prmUser
      fg-rctd.upd-date = TODAY
      fg-rctd.upd-time = TIME.

     ASSIGN prmAction = "Select"
         prmSeqno  = string(lv-rno) .

RUN get-first-r-no.
END.



IF prmAction = "Addrcpt" THEN DO:

 ASSIGN
      ll-set-parts = NO.
    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and 
            fg-rctd.r-no EQ int(prmSeqno) AND
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.
      IF AVAIL fg-rctd AND AVAIL reftable THEN
        ASSIGN
            fg-rctd.rct-date    = date(prmRcptDate)
            fg-rctd.trans-time  = TIME          
            fg-rctd.tag         = prmTagno                  
            fg-rctd.po-no       = prmPono                
            fg-rctd.job-no      = prmJobno                  
            fg-rctd.job-no2     = int(prmJob_no2)
            fg-rctd.i-no        = prmFgItem                  
            fg-rctd.i-name      = prmName    
            fg-rctd.loc         = prmLoc           
            fg-rctd.loc-bin     = prmLocBin        
            fg-rctd.cases       = int(prmCases)
            fg-rctd.qty-case    = int(prmQty_Cas)
            fg-rctd.cases-unit  = int(prmCasUnit)
            fg-rctd.partial     = int(prmPartial)
            fg-rctd.std-cost    = decimal(prmStdCost)
            fg-rctd.cost-uom    = prmCost_Uom     
            fg-rctd.t-qty       = dec(prmTQty)
            fg-rctd.frt-cost    = dec(prmFrtCost)
            fg-rctd.ext-cost    = dec(prmExtCost)
            fg-rctd.stack-code  = prmStackCode     
            /*reftable.code       = prmUser   
            reftable.code2      = prmUser  */   
            fg-rctd.tot-wt      = decimal(prmTotWt)  .


     FIND FIRST reftable
      WHERE reftable.reftable EQ "fg-rctd.user-id"
        AND reftable.company  EQ fg-rctd.company
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
      NO-ERROR.
  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "fg-rctd.user-id"
     reftable.company  = fg-rctd.company
     reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
     reftable.code     = prmUser.
  END.
  ASSIGN
   reftable.code2        = prmUser
   fg-rctd.upd-date = TODAY
   fg-rctd.upd-time = TIME.

  RUN get-matrix (NO).
  RUN get-linker.   
  IF ll-set-parts THEN DO:
      FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ fg-rctd.r-no EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAIL fg-rcpts THEN DO:
          CREATE fg-rcpts.
          fg-rcpts.r-no       = fg-rctd.r-no.
      END.
    ASSIGN
        fg-rcpts.company    = cocode
        fg-rcpts.i-no       = fg-rctd.i-no
        fg-rcpts.i-name     = fg-rctd.i-name
        fg-rcpts.trans-date = fg-rctd.rct-date
        fg-rcpts.linker     = lv-linker.
  END.
  ELSE RUN fg/comprcpt.p (ROWID(fg-rctd)).

     
      /*IF v-auto-add-tag AND fg-rctd.tag EQ "" THEN DO:
          RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag).
          RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
          fg-rctd.tag = v-next-tag.
      END.*/

      RUN fg/invrecpt.p (ROWID(fg-rctd), 1).


  ASSIGN prmAction = "Select"
          .

END.

IF prmAction = "Deletercpt" THEN DO:
    
    ASSIGN lv-setprt = NO.
    FIND FIRST fg-rctd WHERE fg-rctd.company eq cocode and 
            fg-rctd.r-no EQ int(prmRecKey) AND
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") EXCLUSIVE-LOCK NO-ERROR.
      
      IF AVAIL fg-rctd THEN 
          DELETE fg-rctd .


FIND LAST fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.r-no ge lv-frst-rno and 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd NO-LOCK NO-ERROR.
       FIND FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND 
             ((reftable.dscr    EQ lv-linker AND reftable.dscr begins "fg-rctd: ") OR 
             (not lv-setprt and not reftable.dscr begins "fg-rctd: ")) NO-LOCK NO-ERROR.
       IF AVAIL fg-rctd AND AVAIL reftable THEN
           ASSIGN
           prmAction = "Select" 
           prmSeqno  = string(fg-rctd.r-no).
  END.


 IF prmAction = "Select" THEN DO:
  
    FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.r-no ge lv-frst-rno and 
           fg-rctd.r-no EQ int(prmSeqno) AND
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") 
           use-index fg-rctd NO-LOCK, 
       FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND 
             ((reftable.dscr    EQ lv-linker AND reftable.dscr begins "fg-rctd: ") OR 
             (not lv-setprt and not reftable.dscr begins "fg-rctd: ")) NO-LOCK: 
             
    
            create ttViewFGrcpt.
            assign
                ttViewFGrcpt.vRno             = fg-rctd.r-no
                ttViewFGrcpt.vDate            = string(fg-rctd.rct-date)
                ttViewFGrcpt.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
                ttViewFGrcpt.vTag             = fg-rctd.tag
                ttViewFGrcpt.vPo_no           = fg-rctd.po-no
                ttViewFGrcpt.vJob_no          = fg-rctd.job-no
                ttViewFGrcpt.vJob_no2         = fg-rctd.job-no2
                ttViewFGrcpt.vItem            = fg-rctd.i-no
                ttViewFGrcpt.vItemName        = fg-rctd.i-name
                ttViewFGrcpt.vLoc             = fg-rctd.loc 
                ttViewFGrcpt.vLocBin          = fg-rctd.loc-bin
                ttViewFGrcpt.vCases           = fg-rctd.cases 
                ttViewFGrcpt.vQtyCas          = fg-rctd.qty-case 
                ttViewFGrcpt.vCasUnit         = fg-rctd.cases-unit  
                ttViewFGrcpt.vPartial         = fg-rctd.partial
                ttViewFGrcpt.vStdCost         = fg-rctd.std-cost
                ttViewFGrcpt.vCostUom         = fg-rctd.cost-uom
                ttViewFGrcpt.vT_Qty           = fg-rctd.t-qty
                ttViewFGrcpt.vFrtCost         = fg-rctd.frt-cost
                ttViewFGrcpt.vExtCost         = fg-rctd.ext-cost
                ttViewFGrcpt.vStackCode       = fg-rctd.stack-code 
                ttViewFGrcpt.vCreatedBy       = reftable.code
                ttViewFGrcpt.vCreate2         = reftable.code2  
                ttViewFGrcpt.vTot_Wt          = fg-rctd.tot-wt 
                ttViewFGrcpt.vRecKey          = fg-rctd.rec_key .


               

                      /* v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.*/
                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/
/*********************************************************************************/



  

PROCEDURE get-first-r-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF BUFFER bq-fg-rctd FOR fg-rctd.

  lv-frst-rno = 999999999.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "R"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  RELEASE bq-fg-rctd.

  FOR EACH bq-fg-rctd FIELDS(r-no)
      WHERE bq-fg-rctd.company   EQ cocode
        AND bq-fg-rctd.rita-code EQ "E"
        AND bq-fg-rctd.r-no      LT lv-frst-rno
      USE-INDEX rita-code NO-LOCK
      BY bq-fg-rctd.r-no:
    lv-frst-rno = bq-fg-rctd.r-no.
    LEAVE.
  END.
  RELEASE bq-fg-rctd.

END PROCEDURE.


PROCEDURE get-linker :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  /*DEF OUTPUT PARAM op-linker LIKE lv-linker NO-UNDO.*/


  lv-linker = IF AVAIL fg-rctd                 AND
                 CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ fg-rctd.company
                            AND itemfg.i-no    EQ fg-rctd.i-no
                            AND itemfg.isaset) THEN
                "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
              ELSE "".

END PROCEDURE.


PROCEDURE valid-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
 
  
END PROCEDURE.

PROCEDURE valid-lot# :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   
END PROCEDURE.


PROCEDURE valid-po-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-type AS INT NO-UNDO.


  
   
  

END PROCEDURE.

PROCEDURE valid-job-no :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
    
 

END PROCEDURE.



PROCEDURE valid-job-no2 :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-ans AS LOG NO-UNDO.
  DEF VAR lv-err AS LOG INIT NO NO-UNDO.

 

   

END PROCEDURE.


PROCEDURE get-matrix :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def input parameter ip-first-disp as log no-undo.

  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  DEF VAR v-tot-msf AS DEC /* int wfk */ NO-UNDO.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-over-cost AS DEC NO-UNDO.
  def var lv-ext-cost as dec no-undo.
  DEF VAR lv-cost-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR lv-from-uom LIKE rm-rctd.cost-uom NO-UNDO.
  DEF VAR lv-out-ea AS DEC NO-UNDO.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR v-job-qty AS DEC NO-UNDO.
  DEF VAR v-ord-qty AS DEC NO-UNDO.
  DEF VAR v-ord-cost AS DEC NO-UNDO .
  DEF VAR v-ord-uom  AS CHAR NO-UNDO.
  DEF VAR v-setup-qty AS DEC NO-UNDO.
  DEF VAR v-ord-po-uom AS CHAR NO-UNDO.
  DEF VAR v-cost-per-ea AS DEC NO-UNDO .
  DEF VAR v-cost-setup AS DEC NO-UNDO .
  DEF VAR v-cost-with-setup AS DEC NO-UNDO .
  DEF VAR v-corr AS LOG NO-UNDO.
  DEF VAR v-basis-w AS DEC NO-UNDO. 
  DEF VAR v-out-qty AS DEC NO-UNDO.
  DEF VAR v-qty-per-msf AS DEC NO-UNDO.
  DEF VAR v-tot-cost AS DEC NO-UNDO.
  DEF BUFFER b-job-hdr FOR job-hdr.
  if not avail fg-rctd then return.  /* no records */


find itemfg where itemfg.company eq cocode
              and itemfg.i-no  eq fg-rctd.i-no
            use-index i-no no-lock no-error.

ASSIGN
 lv-cost-uom = itemfg.prod-uom
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0
 v-ord-qty   = 0
 v-ord-cost  = 0
 v-ord-uom   = ""
 v-ord-po-uom  = ""
 v-rec-qty   = 0
 v-job-qty   = 0
 v-ord-cost  = 0
 v-ord-uom   = ""
 v-ord-po-uom      = ""
 v-cost-per-ea     = 0
 v-cost-setup      = 0
 v-cost-with-setup = 0
 v-cost-setup      = 0.

/* Always find just to get quantity */
find first po-ordl where po-ordl.company = cocode
                     and po-ordl.po-no = int(fg-rctd.po-no)
                     and po-ordl.i-no  = fg-rctd.i-no
                     and po-ordl.job-no = fg-rctd.job-no
                     and po-ordl.job-no2 = fg-rctd.job-no2
                     and po-ordl.item-type = no
                     no-lock no-error.
IF NOT AVAIL po-ordl THEN
    find first po-ordl where po-ordl.company = cocode
                         and po-ordl.po-no 
                             = int(fg-rctd.po-no)
                         and po-ordl.i-no  = fg-rctd.i-no
                         and po-ordl.item-type = no
                         no-lock no-error.

IF AVAIL(po-ordl) THEN DO:

  ASSIGN v-ord-qty  = po-ordl.ord-qty
         v-ord-cost = po-ordl.cons-cost
         v-ord-uom  = po-ordl.cons-uom
         v-ord-po-uom = po-ordl.pr-qty-uom.
END.

if ip-first-disp  and avail fg-rctd and fg-rctd.i-no <> "" then do: /* for row-display */  
  IF AVAIL po-ordl THEN
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid.

  ASSIGN
   lv-out-qty  = fg-rctd.t-qty
   lv-from-uom = fg-rctd.cost-uom
   lv-out-cost = fg-rctd.std-cost.
END. /* avail fg-rctd */
/* ======================================================================= */
ELSE
if avail fg-rctd and fg-rctd.i-no <> "" then do: /* in update mode - use screen-value */
  find first po-ordl where po-ordl.company = fg-rctd.company
                       and po-ordl.po-no = INT(fg-rctd.po-no) 
                       and po-ordl.i-no  = fg-rctd.i-no
                       and po-ordl.job-no = (fg-rctd.job-no)
                       and po-ordl.job-no2 = integer(fg-rctd.job-no2)
                       and po-ordl.item-type = no
                       no-lock no-error.
  v-rec-qty = INT(fg-rctd.t-qty).

  FOR EACH b-fg-rctd
      WHERE b-fg-rctd.company    EQ fg-rctd.company
        AND b-fg-rctd.rita-code  EQ "R"
        AND b-fg-rctd.i-no       EQ fg-rctd.i-no
        AND INT(b-fg-rctd.po-no) EQ INT(fg-rctd.po-no)
        AND b-fg-rctd.job-no     EQ fg-rctd.job-no
        AND b-fg-rctd.job-no2    EQ INT(fg-rctd.job-no2)
        AND ROWID(b-fg-rctd)     NE ROWID(fg-rctd)
      NO-LOCK:
    v-rec-qty = v-rec-qty + b-fg-rctd.t-qty.
  END.

  /* If available PO orderline */
  IF AVAIL po-ordl THEN DO:
    ASSIGN
     v-len = po-ordl.s-len
     v-wid = po-ordl.s-wid
     v-rec-qty = v-rec-qty + po-ordl.t-rec-qty.

    IF LOOKUP(po-ordl.pr-qty-uom,fg-uom-list) EQ 0 THEN DO:
       RUN sys/ref/convquom.p("EA", po-ordl.pr-qty-uom, 0, 0, 0, 0,
                              v-rec-qty, OUTPUT v-rec-qty).

    END.
    
  END. /* IF AVAIL po-ordl */
  /* Else if not available PO orderline and job number is entered... */
  ELSE IF fg-rctd.job-no <> "" THEN DO:
       find first job-hdr where job-hdr.company = fg-rctd.company                       
                       and job-hdr.i-no  = fg-rctd.i-no
                       and job-hdr.job-no = (fg-rctd.job-no)
                       and job-hdr.job-no2 = integer(fg-rctd.job-no2)
                       no-lock no-error.
       IF AVAIL job-hdr THEN DO:

/*           FOR EACH fg-act                                                                         */
/*               WHERE fg-act.company EQ cocode                                                      */
/*                 AND fg-act.job-no  EQ fg-rctd.job-no:SCREEN-VALUE IN BROWSE {&browse-name}        */
/*                 AND fg-act.job-no2 EQ INT(fg-rctd.job-no2:SCREEN-VALUE IN BROWSE {&browse-name})  */
/*                 AND fg-act.i-no    EQ fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}          */
/*               NO-LOCK:                                                                            */
/*             v-rec-qty = v-rec-qty + fg-act.qty.                                                   */
/*           END.                                                                                    */

          FIND FIRST sys-ctrl WHERE sys-ctrl.company = cocode AND
                                    sys-ctrl.name = "JOB QTY" 
                                    NO-LOCK NO-ERROR.
          IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-job-qty = job-hdr.qty                          .
          ELSE DO:
              FIND FIRST oe-ordl NO-LOCK
                  WHERE oe-ordl.company EQ job-hdr.company
                    AND oe-ordl.ord-no  EQ job-hdr.ord-no
                    AND oe-ordl.i-no    EQ job-hdr.i-no
                  NO-ERROR.
              FIND FIRST oe-ord NO-LOCK
                  WHERE oe-ord.company EQ job-hdr.company
                    AND oe-ord.ord-no  EQ job-hdr.ord-no
                  NO-ERROR.
              
              v-job-qty = (job-hdr.qty * (1 + ((IF AVAIL oe-ordl THEN oe-ordl.over-pct ELSE
                                                IF AVAIL oe-ord  THEN oe-ord.over-pct  ELSE 0) / 100))).
          END.


          {sys/inc/roundup.i v-job-qty}

          
       END.
  END.


  ASSIGN
   lv-out-qty  = DEC(fg-rctd.t-qty)
   lv-from-uom = fg-rctd.cost-uom
   lv-out-cost = DEC(fg-rctd.std-cost).
END.


IF lv-from-uom EQ "L" THEN 
    ASSIGN
     lv-from-uom = "EA"
     lv-out-cost = lv-out-cost / lv-out-qty.



/* Calculate for quantity comparison purposes */
RUN rm/convquom.p(v-ord-po-uom, 'EA',                   
          v-bwt, v-len, v-wid, v-dep,
          v-ord-qty, OUTPUT v-ord-qty).

RUN rm/convquom.p(v-ord-po-uom, 'EA',            
          v-bwt, v-len, v-wid, v-dep,
          lv-out-qty, OUTPUT lv-out-ea).

/* Per Joe, if quantity is less than PO qty, use PO price */
IF v-ord-qty > 0 AND v-ord-qty > lv-out-qty 
                 AND v-ord-cost > 0 
                 AND v-ord-uom > "" THEN DO:

    ASSIGN lv-out-cost = v-ord-cost
           lv-from-uom = v-ord-uom.

END.

/* convert cost pr-uom*/
IF lv-from-uom EQ lv-cost-uom               OR
   (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
    LOOKUP(lv-cost-uom,fg-uom-list) GT 0)   THEN.
ELSE DO:

        RUN rm/convcuom.p(lv-from-uom, lv-cost-uom,                   
                          v-bwt, v-len, v-wid, v-dep,
                          lv-out-cost, OUTPUT lv-out-cost).

    END.
  
IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
  RUN rm/convquom.p("EA", lv-cost-uom,                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-qty).

 /* get quantity in eaches */
  RUN rm/convquom.p(lv-cost-uom, "EA",                   
                    v-bwt, v-len, v-wid, v-dep,
                    lv-out-qty, OUTPUT lv-out-ea).


 ASSIGN v-basis-w = 0
        v-corr = NO.

 IF AVAIL(po-ordl) AND v-ord-qty <= lv-out-ea AND po-ordl.setup > 0 THEN DO:

   lv-over-cost = po-ordl.cost.
   IF po-ordl.PR-uom EQ lv-cost-uom               OR
     (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
      LOOKUP(lv-cost-uom,fg-uom-list) GT 0)   THEN.
   ELSE
     RUN rm/convcuom.p(po-ordl.PR-uom, lv-cost-uom, 
                       v-bwt, v-len, v-wid, v-dep,
                       po-ordl.cost, OUTPUT lv-over-cost).
   v-setup-qty = lv-out-ea.
   IF lv-cost-uom NE 'EA' THEN
       RUN rm/convquom.p("EA", lv-cost-uom,                   
                         v-bwt, v-len, v-wid, v-dep,
                         lv-out-ea, OUTPUT v-setup-qty).

   lv-out-cost = lv-over-cost + (po-ordl.setup / v-setup-qty).

 END.

ASSIGN
 lv-ext-cost = lv-out-qty * lv-out-cost
 fg-rctd.cost-uom = lv-cost-uom
 fg-rctd.std-cost = (lv-out-cost)
 fg-rctd.ext-cost = (lv-ext-cost + DEC(fg-rctd.frt-cost)).


END PROCEDURE.



PROCEDURE get-values :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR lv-loc AS CHAR NO-UNDO.
  DEF VAR lv-loc-bin AS CHAR NO-UNDO.
  DEF VAR lv-qty-case AS CHAR NO-UNDO.
  DEF VAR lv-cost-uom AS CHAR NO-UNDO.
  DEF VAR lv-std-cost AS CHAR NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR lv-save AS CHAR EXTENT 20 NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.
  
    find first itemfg
        where (itemfg.company  = cocode )
          and itemfg.i-no EQ prmFgItem
        no-lock no-error.

    /* Assign from itemfg only if blank. Don't overwrite if name already set from PO. */
    IF prmName  = "" OR prmPono = "" THEN
        ASSIGN prmName    = itemfg.i-name.
        

/*     find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.  */

    assign
     lv-qty-case = string(itemfg.case-count)
     lv-cost-uom = if itemfg.pur-man then itemfg.pur-uom else itemfg.prod-uom.

    RUN fg/autopost.p (ROWID(itemfg),
                       prmJobno,
                       INT(prmJob_no2),
                       OUTPUT lv-loc, OUTPUT lv-loc-bin).

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.loc     eq lv-loc
          and fg-bin.loc-bin eq lv-loc-bin
          and fg-bin.i-no    eq ""
        no-lock no-error.
    if avail fg-bin then 
      assign
       lv-std-cost = IF prmPono = "" and
                                                  prmJobno = "" 
                                               THEN string(itemfg.last-cost) 
                                               ELSE lv-std-cost
       lv-qty-case = /*IF fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} = "" and
                                                  fg-rctd.job-no:SCREEN-VALUE = "" 
                                                THEN   STRING(itemfg.case-count)
                                                ELSE lv-qty-case
                                                */
                                                STRING(itemfg.case-count)
       lv-cost-uom = itemfg.prod-uom.

    ASSIGN
     lv-save[1] = prmStdCost
     lv-save[2] = prmCost_Uom .

    RUN get-fg-bin-cost.

    ASSIGN
     lv-std-cost = prmStdCost
     lv-cost-uom = prmCost_Uom

     prmStdCost = lv-save[1]
     lv-cost-uom = lv-save[2].


    /**  Find the Job Header record in then job file and use Standard Cost
         from that job. **/
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.i-no    eq prmFgItem
          and job-hdr.job-no  eq prmJobno
          and job-hdr.job-no2 eq int(prmJob_no2)
        NO-LOCK no-error.

    IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ prmJobno
            AND job.job-no2 EQ int(prmJob_no2)
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable-job
          WHERE reftable-job.reftable EQ "jc/jc-calc.p"
            AND reftable-job.company  EQ job.company
            AND reftable-job.loc      EQ ""
            AND reftable-job.code     EQ STRING(job.job,"999999999")
            AND reftable-job.code2    EQ prmFgItem
          NO-LOCK NO-ERROR.
    END.

    if avail job-hdr and job-hdr.std-tot-cost gt 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = string(job-hdr.std-tot-cost).
    ELSE
    IF AVAIL reftable-job AND reftable-job.val[5] GT 0 THEN
      ASSIGN
       lv-cost-uom = "M"
       lv-std-cost = string(reftable-job.val[5]).

    /** If no Job Header is avail for this Job# then Find the Item
        record for then item and use Standard Cost from that item. **/
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(prmPono)
            and po-ordl.i-no      eq prmFgItem
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        ASSIGN
         lv-cost-uom = po-ordl.pr-uom.
         lv-std-cost = STRING(po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1)).

        RUN convert-vend-comp-curr(INPUT-OUTPUT lv-std-cost).

        RUN show-freight.
      END.
     
      else
      if avail itemfg          AND
         DEC(lv-std-cost) EQ 0 THEN DO:
        assign
         lv-cost-uom = itemfg.prod-uom
         lv-std-cost = STRING(itemfg.total-std-cost).

        IF /*itemfg.total-std-cost EQ 0 AND*/ itemfg.isaset THEN DO:
            RUN fg/costset.p (ROWID(itemfg), OUTPUT v-cost).

          IF lv-cost-uom NE "M" THEN DO:
            RUN sys/ref/ea-um-fg.p (lv-cost-uom, OUTPUT ll-ea).
            IF ll-ea THEN lv-cost-uom = "EA".
            RUN sys/ref/convcuom.p("M", lv-cost-uom,
                                   0, 0, 0, 0, v-cost, OUTPUT v-cost).
            IF ll-ea THEN lv-cost-uom = itemfg.prod-uom.
          END.

          lv-std-cost = STRING(v-cost).
        END.
      END.
    END.

    IF prmLoc    EQ "" OR prmLocBin EQ "" THEN
        ASSIGN
        prmLoc   = lv-loc
        prmLocBin = lv-loc-bin.

    IF INT(prmQty_Cas) EQ 0 THEN
      prmQty_Cas = string(lv-qty-case).

    IF prmCost_Uom EQ "" THEN
      prmCost_Uom = lv-cost-uom.

    IF DEC(prmStdCost) EQ 0 THEN
      prmStdCost  = lv-std-cost.

    IF INT(prmCasUnit) EQ 0 THEN
      prmCasUnit = "1".

    /*IF fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ ? 
        OR fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} EQ "?" THEN
     fg-rctd.partial:SCREEN-VALUE IN BROWSE {&browse-name} = "0".*/



END PROCEDURE.


PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
 
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ prmFgItem
          AND fg-bin.job-no  EQ prmJobno
          AND fg-bin.job-no2 EQ INT(prmJob_no2)
          AND fg-bin.loc     EQ prmLoc
          AND fg-bin.loc-bin EQ prmLocBin
          AND fg-bin.tag     EQ prmTagno
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       prmStdCost = STRING(fg-bin.std-tot-cost)
       prmCost_Uom = STRING(fg-bin.pur-uom).
  

END PROCEDURE.


PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.

   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ po-ordl.company AND
        b-po-ord.po-no   EQ po-ordl.po-no
        NO-LOCK NO-ERROR.

   IF AVAIL b-po-ord THEN
   DO:
      FIND FIRST vend WHERE
           vend.company EQ b-po-ord.company AND
           vend.vend-no EQ b-po-ord.vend-no
           NO-LOCK NO-ERROR.

      IF AVAIL vend THEN
      DO:
         FIND FIRST b-company WHERE
              b-company.company EQ cocode
              NO-LOCK.

         IF vend.curr-code NE b-company.curr-code THEN
         DO:
            FIND FIRST currency WHERE
                 currency.company EQ b-po-ord.company AND
                 currency.c-code EQ vend.curr-code
                 NO-LOCK NO-ERROR.

            IF AVAIL currency THEN
            DO:
               ip-cost = ip-cost * currency.ex-rate.

               RELEASE currency.
            END.
         END.

         RELEASE b-company.
         RELEASE vend.
      END.

      RELEASE b-po-ord.
   END.
END PROCEDURE.

PROCEDURE show-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.


  IF fgpofrt-log THEN do: 
 
    ASSIGN
     ld = DEC(prmFrtCost)
     prmExtCost =
         STRING(DEC(prmExtCost) - ld).

    RUN get-freight-cost (OUTPUT ld).

    ASSIGN
     prmFrtCost = STRING(ld)
     prmExtCost =
         STRING(DEC(prmExtCost) + ld).
  END.

END PROCEDURE.


PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-cost LIKE fg-rctd.frt-cost NO-UNDO.

  DEF VAR lv-rowid AS ROWID NO-UNDO.
  DEF VAR ld-qty AS DEC NO-UNDO.
  DEF VAR ld-wgt AS DEC EXTENT 2 NO-UNDO.
  DEF VAR ld-cst AS DEC EXTENT 2 NO-UNDO.


  
    RELEASE po-ord.

    FIND FIRST po-ordl
        WHERE po-ordl.company   EQ fg-rctd.company
          AND po-ordl.po-no     EQ INT(prmPono)
          AND po-ordl.i-no      EQ prmFgItem
          AND po-ordl.job-no    EQ prmJobno
          AND po-ordl.job-no2   EQ INT(prmJob_no2)
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN
      RUN po/getfrtcs.p (ROWID(po-ordl),
                         DEC(prmTQty),
                         OUTPUT op-cost).

    RUN convert-vend-comp-curr(INPUT-OUTPUT op-cost).
  

END PROCEDURE.


PROCEDURE get-next-tag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ipc-i-no AS CHAR NO-UNDO.
  DEF OUTPUT PARAMETER opc-next-tag AS CHAR NO-UNDO.
  DEF BUFFER bf-loadtag FOR loadtag.
  DEF VAR io-tag-no AS INT NO-UNDO.

  FIND LAST bf-loadtag NO-LOCK
      WHERE bf-loadtag.company     EQ cocode
        AND bf-loadtag.item-type   EQ NO
        AND bf-loadtag.is-case-tag EQ NO
        AND bf-loadtag.tag-no      BEGINS ipc-i-no
        AND SUBSTR(bf-loadtag.tag-no,1,15) EQ ipc-i-no
      USE-INDEX tag NO-ERROR.

  io-tag-no = (IF AVAIL bf-loadtag THEN INT(SUBSTR(bf-loadtag.tag-no,16,5)) ELSE 0) + 1.

  opc-next-tag = STRING(CAPS(ipc-i-no),"x(15)") + STRING(io-tag-no,"99999").

END PROCEDURE.


PROCEDURE create-loadtag :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       From r-loadtg.w
------------------------------------------------------------------------------*/

DEF INPUT-OUTPUT PARAM io-tag-no AS CHAR NO-UNDO.
DEF INPUT PARAM fg-rctd-row AS ROWID NO-UNDO.

DEF BUFFER b-loadtag FOR loadtag.
DEF BUFFER b-po-ordl FOR po-ordl.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER b-fg-rctd FOR fg-rctd.

DEFINE VARIABLE li                    AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-got-job            AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lv-out-cost           AS DECIMAL       DECIMALS 4 NO-UNDO.
DEFINE VARIABLE lv-out-qty            AS DECIMAL       NO-UNDO.
DEFINE VARIABLE lv-from-uom           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-cost-uom           AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-ord-qty            AS INTEGER       NO-UNDO.
DEFINE VARIABLE lv-ord-uom            AS CHARACTER     NO-UNDO.
DEFINE VARIABLE lv-setup-included     AS LOGICAL       NO-UNDO.
DEFINE VARIABLE lv-setup-per-cost-uom AS DECIMAL       NO-UNDO.
DEFINE VARIABLE v-bwt                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-len                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-wid                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE v-dep                 LIKE po-ordl.s-len NO-UNDO.
DEFINE VARIABLE dRFIDTag              AS DECIMAL       NO-UNDO.

DEF VAR v-fgrecpt   AS LOG NO-UNDO. 
DEF VAR tb_ret      AS LOG INIT YES NO-UNDO.
DEF VAR v-loadtag   AS CHAR NO-UNDO .
DEF VAR v-mult      AS INT NO-UNDO.
DEF VAR v-cas-lab   AS LOG NO-UNDO.
DEF VAR v-tags      AS DEC NO-UNDO.

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company EQ cocode
    AND sys-ctrl.name    EQ "FGRECPT"
  NO-LOCK NO-ERROR.
ASSIGN
v-fgrecpt = AVAIL sys-ctrl AND sys-ctrl.char-fld EQ "LoadTag".

FIND FIRST sys-ctrl
  WHERE sys-ctrl.company eq cocode
    AND sys-ctrl.name    eq "LOADTAG"
  NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN
ASSIGN v-loadtag = sys-ctrl.char-fld
       v-mult    = sys-ctrl.int-fld
       v-cas-lab = sys-ctrl.log-fld
       v-tags    = sys-ctrl.dec-fld.

FIND b-fg-rctd WHERE ROWID(b-fg-rctd) EQ fg-rctd-row NO-LOCK NO-ERROR.
FIND FIRST itemfg WHERE itemfg.company EQ cocode
                    AND itemfg.i-no    EQ b-fg-rctd.i-no
                  NO-LOCK NO-ERROR.


CREATE loadtag.
ASSIGN
  loadtag.company      = cocode
  loadtag.tag-no       = io-tag-no
  loadtag.item-type    = NO /*FGitem*/
  loadtag.job-no       = b-fg-rctd.job-no
  loadtag.job-no2      = b-fg-rctd.job-no2
  /*
  loadtag.ord-no       = IF CAN-FIND(FIRST cust WHERE cust.company = cocode
                                                  AND cust.cust-no     = itemfg.cust-no
                                                  AND cust.active      = "X")
                         THEN 0 ELSE b-fg-rctd.ord-no
  */
  loadtag.i-no         = CAPS(b-fg-rctd.i-no)
  loadtag.i-name       = b-fg-rctd.i-name
  loadtag.qty          = b-fg-rctd.qty
  loadtag.qty-case     = b-fg-rctd.qty-case
  loadtag.case-bundle  = b-fg-rctd.cases-stack
  loadtag.pallet-count = b-fg-rctd.qty MOD b-fg-rctd.units-pallet
  loadtag.partial      = b-fg-rctd.qty MOD b-fg-rctd.qty-case
  loadtag.sts          = "Printed" 
  loadtag.tag-date     = TODAY
  loadtag.tag-time     = TIME
  loadtag.misc-dec[1]  = b-fg-rctd.tot-wt
  /*
  loadtag.misc-dec[2]  = b-fg-rctd.pallt-wt
  loadtag.misc-char[2] = b-fg-rctd.lot
  */
  loadtag.po-no = INT(b-fg-rctd.po-no).

IF v-fgrecpt AND tb_ret THEN loadtag.tot-cases  = (loadtag.pallet-COUNT - loadtag.partial) / loadtag.case-bundle.

IF v-loadtag = "CentBox" THEN DO:
  ASSIGN loadtag.loc     = itemfg.def-loc
         loadtag.loc-bin = itemfg.def-loc-bin.
  FIND FIRST fg-bin WHERE fg-bin.company EQ itemfg.company
                      AND fg-bin.i-no    EQ itemfg.i-no
                      AND fg-bin.job-no  EQ b-fg-rctd.job-no
                      AND fg-bin.tag     EQ loadtag.tag-no
                    NO-LOCK NO-ERROR.
  IF AVAIL fg-bin THEN
  ASSIGN loadtag.loc     = fg-bin.loc
         loadtag.loc-bin = fg-bin.loc-bin.
  
END.
ELSE RUN fg/autopost.p (ROWID(itemfg), b-fg-rctd.job-no, b-fg-rctd.job-no2,
                        OUTPUT loadtag.loc , OUTPUT loadtag.loc-bin).

IF RFIDTag-log THEN DO:
  FIND FIRST oe-ctrl WHERE oe-ctrl.company = loadtag.company 
                     NO-ERROR.
  dRFIDTag = IF AVAIL oe-ctrl AND oe-ctrl.spare-char-1 <> ""
             THEN dec(oe-ctrl.spare-char-1) ELSE 111110000000000000000001.
  oe-ctrl.spare-char-1 = string(dRFIDTag + 1).
  CREATE rfidtag.
  ASSIGN rfidtag.company   = loadtag.company
         rfidtag.item-type = loadtag.item-type
         rfidtag.tag-no    = loadtag.tag-no
         rfidtag.rfidtag   = STRING(dRFIDTag).
  RELEASE oe-ctrl.
END.


FIND CURRENT loadtag NO-LOCK NO-ERROR.
FIND CURRENT b-fg-rctd NO-LOCK NO-ERROR.

END PROCEDURE.

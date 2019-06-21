

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

DEFINE TEMP-TABLE ttViewFGrece NO-UNDO
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
        FIELD goods             AS CHAR
        FIELD rece           AS CHAR  
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

DEFINE DATASET dsViewFGrece FOR ttViewFGrece .

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


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsViewFGrece.
DEFINE NEW SHARED VAR vuser AS CHAR NO-UNDO.
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.
DEFINE VAR custcount AS CHAR NO-UNDO.
/*DEF NEW SHARED VAR cocode AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode AS CHAR NO-UNDO.*/
DEF NEW SHARED VAR g_company AS CHAR NO-UNDO.
DEF NEW SHARED VAR g_loc AS CHAR NO-UNDO.
DEF VAR v-auto-add-tag AS LOG NO-UNDO.
DEF VAR lv-msg AS CHAR NO-UNDO.
DEF VAR v-next-tag AS cha NO-UNDO.
{sys/inc/var.i "new shared"}
 {pc/pcprdd4u.i NEW}
{fg/invrecpt.i NEW}
{jc/jcgl-sh.i  NEW}
{fg/fullset.i  NEW}
{fg/fg-post3.i NEW}
DEFINE VARIABLE v-webrootpath AS CHARACTER NO-UNDO.


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
    vuser = prmUser .

FIND FIRST sys-ctrl WHERE sys-ctrl.company = prmComp AND sys-ctrl.NAME = "WEBROOT" NO-LOCK NO-ERROR.
IF AVAIL sys-ctrl THEN ASSIGN v-webrootpath = sys-ctrl.char-fld.


DO TRANSACTION:
  {sys/inc/fgpofrt.i}
  {sys/inc/fgrecpt.i}
  {sys/inc/autopost.i}
  {sys/inc/fgsetrec.i}
  {sys/inc/oeship.i}
  {sys/inc/fgsecur.i}
  {sys/inc/rfidtag.i}

  {sys/inc/sspostfg.i}

   {sys/inc/closejob.i FGPost}
   {sys/inc/fgpostgl.i}
   {sys/inc/adjustgl.i}
   {sys/inc/fgemails.i}
   {sys/inc/postdate.i}
   {sys/inc/fgpost.i}
END.

DEF TEMP-TABLE w-fg-rctd NO-UNDO LIKE fg-rctd FIELD row-id   AS ROWID
                                    FIELD has-rec  AS LOG INIT NO
                                    FIELD invoiced AS LOG INIT NO.


DEF TEMP-TABLE tt-email NO-UNDO FIELD tt-recid AS RECID
                        FIELD job-no LIKE job-hdr.job-no
                        FIELD job-no2 LIKE job-hdr.job-no2
                        FIELD i-no LIKE itemfg.i-no
                        FIELD qty AS INT
                        FIELD cust-no AS cha
                        INDEX tt-cust IS PRIMARY cust-no DESCENDING .

DEF VAR v-ssfgscan AS LOG NO-UNDO.
DEF VAR v-fgpostgl AS CHAR NO-UNDO.
DEF VAR v-post-date AS DATE INITIAL TODAY.  
DEF STREAM logFile.
DEF STREAM st-email.

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

IF prmAction = "getnkvalue" THEN DO:
  
    FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
          AND sys-ctrl.name    EQ "SSFGSCAN" NO-LOCK NO-ERROR.
   IF NOT AVAIL sys-ctrl THEN DO TRANSACTION:
      CREATE sys-ctrl.
      ASSIGN sys-ctrl.company = cocode
             sys-ctrl.NAME = "SSFGSCAN"
             sys-ctrl.descrip = "Prompt for the Warehouse/Bin?"
             sys-ctrl.log-fld = YES.
   END.
   v-ssfgscan = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE YES.
    CREATE ttViewFGrece .
          ASSIGN ttViewFGrece.vDate = STRING(v-ssfgscan).


END.   /* nk-1 SSFGScan  */

IF prmAction = "ValidateUpdate" THEN DO: 
   
  FIND FIRST fg-rctd WHERE fg-rctd.company = cocode AND fg-rctd.r-no EQ int(prmSeqno) NO-LOCK NO-ERROR.

  IF prmRecKey = "delete" THEN DO:
      FIND FIRST loadtag WHERE loadtag.company = cocode
                      AND loadtag.item-type = NO
                      AND loadtag.tag-no = prmTagno
                      NO-LOCK NO-ERROR.
      IF AVAIL loadtag THEN
          FIND FIRST fg-bin WHERE fg-bin.company = cocode
                        AND fg-bin.i-no = loadtag.i-no
                        AND fg-bin.tag = loadtag.tag-no
                        AND fg-bin.qty > 0
                        NO-LOCK NO-ERROR.
      IF NOT AVAIL fg-bin THEN DO:
          cError = "No Inventory On Hand Exists, Item cannot be deleted." .
              RETURN.
      END.
  END.


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

    /*IF INT(prmPono) NE 0 AND prmFgItem NE ""      THEN DO:

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
     END.*/

     /*find first itemfg where (itemfg.company  = cocode ) and 
         (itemfg.i-no = prmFgItem ) no-lock no-error.
     IF NOT AVAIL itemfg THEN DO:
         cError =  "Invalid Item. Try help. " .
         RETURN .
     END.*/

    /* IF oeship-log                   AND
         CAN-FIND(FIRST itemfg
                  WHERE itemfg.company EQ cocode
                  AND itemfg.i-no    EQ prmFgItem
                  AND itemfg.isaset
                  AND itemfg.alloc) THEN DO:
         cError = "Item may not be an unassembled set header...".
         RETURN .
     END.*/

    /* FIND FIRST reftable WHERE reftable.reftable EQ "FGSTATUS"
         AND reftable.company  EQ cocode
         AND reftable.loc      EQ ""
         AND reftable.code     EQ prmFgItem
         NO-LOCK NO-ERROR.
     IF AVAIL reftable AND reftable.code2 = "I" THEN DO:
         cError =  prmFgItem + " has InActive Status. Receipt cannot be created for the Inactive Item." .
         RETURN .
     END. */
    
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

      IF prmTagno EQ "" THEN DO:
          ASSIGN cError = "Tag# cannot be blank".
          RETURN.
      END.

      

      ASSIGN lv-msg = "" .
      prmTagno = CAPS(prmTagno).
      IF prmTagno NE "" THEN DO:

          IF prmRecKey NE "Delete" THEN
              DO:
              IF (CAN-FIND(FIRST b-fg-rctd WHERE
                           b-fg-rctd.company     EQ cocode AND
                           b-fg-rctd.tag         EQ prmTagno AND
                           RECID(b-fg-rctd)      NE RECID(fg-rctd)) OR
                  CAN-FIND(FIRST b-fg-rdtlh
                           WHERE b-fg-rdtlh.company   EQ cocode
                           AND b-fg-rdtlh.tag       EQ prmTagno
                           AND b-fg-rdtlh.qty       GT 0
                           AND b-fg-rdtlh.rita-code NE "S")) THEN
                  lv-msg = "Tag# has already been used, please re-enter".
              END. /*lv-do-what NE "Delete"*/
          ELSE
              DO:
              IF CAN-FIND(FIRST b-fg-rctd WHERE
                          b-fg-rctd.company     EQ cocode AND
                          b-fg-rctd.tag         EQ prmTagno AND
                          b-fg-rctd.rita-code NE "P" AND
                          RECID(b-fg-rctd)      NE RECID(fg-rctd)) THEN
                  lv-msg = "Tag# has already been used, please re-enter".
              END. /*lv-do-what eq "Delete"*/

              IF lv-msg EQ ""                                                   AND
                  fgrecpt-int EQ 1                                               AND
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
     /* IF (prmPono) NE ""              AND
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
              NO-LOCK NO-ERROR.*/
          
          /* IF ip-type EQ 1                   AND
          AVAIL po-ord                   AND
          adm-adding-record              AND
          NOT CAN-FIND(FIRST tt-fg-rctd) THEN DO:
          
          RUN fg/d-selpos.w (ROWID(po-ord), NO).
          
          RUN create-from-po.       
          
          RUN update-ttt.
          END.*/
    /*END.  */


    /* RUN valid-job-no.*/
    prmJobno = FILL(" ",6 - LENGTH(TRIM(prmJobno))) + TRIM(prmJobno).

   /* IF prmJobno EQ "" THEN DO:
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
      END.*/

      /* RUN valid-job-no2.*/
      /*IF prmJobno NE "" THEN DO:
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
    END.*/

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
       
      RUN get-values.

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

      RUN get-matrix (NO).
     
      RUN fg/comprcpt.p (ROWID(fg-rctd)).
      
      FIND FIRST reftable
      WHERE reftable.reftable EQ "fg-rctd.user-id"
        AND reftable.company  EQ fg-rctd.company
        AND reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")
       EXCLUSIVE-LOCK NO-ERROR.

      IF NOT AVAIL reftable THEN DO:
          CREATE reftable.
          ASSIGN
              reftable.reftable = "fg-rctd.user-id"
              reftable.company  = fg-rctd.company
              reftable.loc      = STRING(fg-rctd.r-no,"9999999999")
              reftable.code     = prmUser .
          END.

          ASSIGN
              reftable.code2   = prmUser
              fg-rctd.upd-date = TODAY
              fg-rctd.upd-time = TIME.

          FIND FIRST fg-rcpts NO-LOCK
              WHERE fg-rcpts.r-no   EQ fg-rctd.r-no
              AND fg-rcpts.linker BEGINS "fg-rctd: "
              NO-ERROR.
          IF AVAIL fg-rcpts THEN reftable.dscr = fg-rcpts.linker.
          
          RUN fg/invrecpt.p (ROWID(fg-rctd), 1).
  
   IF SSPostFG-log AND SSPostFG-char = "LOADTAG" THEN
     RUN fg-post .
      
      ASSIGN prmAction = "Select" .


END.  /**** update  ***/ 


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

FOR EACH fg-rctd WHERE 
       fg-rctd.company eq cocode and 
           fg-rctd.r-no ge lv-frst-rno and 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") AND
            ((prmFgItem = "delete" and fg-rctd.t-qty < 0) or  
            (prmFgItem <> "delete" and fg-rctd.t-qty >= 0))  
           use-index fg-rctd NO-LOCK ,
           FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
             reftable.company  EQ fg-rctd.company AND 
             reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND 
             ( not reftable.dscr begins "fg-rctd: ") NO-LOCK :
      
           ASSIGN
           prmAction = "Select" 
           prmSeqno  = string(fg-rctd.r-no).

END.
     ASSIGN
           prmAction = "Select" .


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
             ( not reftable.dscr begins "fg-rctd: ") NO-LOCK: 

            create ttViewFGrece.
            assign
                ttViewFGrece.vRno             = fg-rctd.r-no
                ttViewFGrece.vDate            = string(fg-rctd.rct-date)
                ttViewFGrece.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
                ttViewFGrece.vTag             = fg-rctd.tag
                ttViewFGrece.vPo_no           = fg-rctd.po-no
                ttViewFGrece.vJob_no          = fg-rctd.job-no
                ttViewFGrece.vJob_no2         = fg-rctd.job-no2
                ttViewFGrece.vItem            = fg-rctd.i-no
                ttViewFGrece.vItemName        = fg-rctd.i-name
                ttViewFGrece.vLoc             = fg-rctd.loc 
                ttViewFGrece.vLocBin          = fg-rctd.loc-bin
                ttViewFGrece.vCases           = fg-rctd.cases 
                ttViewFGrece.vQtyCas          = fg-rctd.qty-case 
                ttViewFGrece.vCasUnit         = fg-rctd.cases-unit  
                ttViewFGrece.vPartial         = fg-rctd.partial
                ttViewFGrece.vStdCost         = fg-rctd.std-cost
                ttViewFGrece.vCostUom         = fg-rctd.cost-uom
                ttViewFGrece.vT_Qty           = fg-rctd.t-qty
                ttViewFGrece.vFrtCost         = fg-rctd.frt-cost
                ttViewFGrece.vExtCost         = fg-rctd.ext-cost
                ttViewFGrece.vStackCode       = fg-rctd.stack-code 
                ttViewFGrece.vCreatedBy       = reftable.code
                ttViewFGrece.vCreate2         = reftable.code2  
                ttViewFGrece.vTot_Wt          = fg-rctd.tot-wt 
                ttViewFGrece.vRecKey          = fg-rctd.rec_key .


               

                      /* v-count = v-count + 1.
                      IF v-count = 50 THEN LEAVE MAIN-LOOP.*/
                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "select" THEN DO:*/

/***********************************List delete pro**********************************************/

IF prmAction = "GridSelect" THEN DO:

    FOR EACH fg-rctd WHERE fg-rctd.company eq cocode and 
        fg-rctd.r-no ge lv-frst-rno and 
        LOOKUP(fg-rctd.rita-code,"R,E") > 0 and 
        ((prmRecKey = "delete" and fg-rctd.t-qty < 0) or  
        (prmRecKey <> "delete" and fg-rctd.t-qty >= 0))  
                   use-index fg-rctd NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
        reftable.company  EQ fg-rctd.company AND 
        reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND ( NOT reftable.dscr begins "fg-rctd: ") NO-LOCK :
        
        create ttViewFGrece.
            assign
                ttViewFGrece.vRno             = fg-rctd.r-no
                ttViewFGrece.vDate            = string(fg-rctd.rct-date)
                ttViewFGrece.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
                ttViewFGrece.vTag             = fg-rctd.tag
                ttViewFGrece.vPo_no           = fg-rctd.po-no
                ttViewFGrece.vJob_no          = fg-rctd.job-no
                ttViewFGrece.vJob_no2         = fg-rctd.job-no2
                ttViewFGrece.vItem            = fg-rctd.i-no
                ttViewFGrece.vItemName        = fg-rctd.i-name
                ttViewFGrece.vLoc             = fg-rctd.loc 
                ttViewFGrece.vLocBin          = fg-rctd.loc-bin
                ttViewFGrece.vCases           = fg-rctd.cases 
                ttViewFGrece.vQtyCas          = fg-rctd.qty-case 
                ttViewFGrece.vCasUnit         = fg-rctd.cases-unit  
                ttViewFGrece.vPartial         = fg-rctd.partial
                ttViewFGrece.vStdCost         = fg-rctd.std-cost
                ttViewFGrece.vCostUom         = fg-rctd.cost-uom
                ttViewFGrece.vT_Qty           = fg-rctd.t-qty
                ttViewFGrece.vFrtCost         = fg-rctd.frt-cost
                ttViewFGrece.vExtCost         = fg-rctd.ext-cost
                ttViewFGrece.vStackCode       = fg-rctd.stack-code 
                ttViewFGrece.vCreatedBy       = reftable.code
                ttViewFGrece.vCreate2         = reftable.code2  
                ttViewFGrece.vTot_Wt          = fg-rctd.tot-wt 
                ttViewFGrece.vRecKey          = fg-rctd.rec_key .
                      
       END. /*FOR EACH vend-whse-trans*/
  END.   /*IF prmAction = "Gridselect" THEN DO:*/

  IF prmAction = "GridSearch" THEN DO:

    FOR EACH fg-rctd WHERE fg-rctd.company eq cocode and 
        fg-rctd.r-no ge lv-frst-rno and 
        LOOKUP(fg-rctd.rita-code,"R,E") > 0 and 
        (fg-rctd.r-no = int(prmSeqno) OR prmSeqno = "" ) AND
        (fg-rctd.i-no BEGINS prmFgItem OR prmFgItem = "" ) AND
        (fg-rctd.job-no = prmJobno OR prmJobno = "" ) AND
        (fg-rctd.po-no = prmPono OR prmPono = "" ) AND
        (fg-rctd.rct-date = DATE(prmRcptDate) OR prmRcptDate = "" ) AND
        (fg-rctd.tag BEGINS prmTagno OR prmTagno = "" ) AND
        ((prmRecKey = "delete" and fg-rctd.t-qty < 0) or  
        (prmRecKey <> "delete" and fg-rctd.t-qty >= 0))  
                   use-index fg-rctd NO-LOCK, 
        FIRST reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND 
        reftable.company  EQ fg-rctd.company AND 
        reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999") AND ( NOT reftable.dscr begins "fg-rctd: ") NO-LOCK :
        
        create ttViewFGrece.
            assign
                ttViewFGrece.vRno             = fg-rctd.r-no
                ttViewFGrece.vDate            = string(fg-rctd.rct-date)
                ttViewFGrece.vTransTime       = STRING(fg-rctd.trans-time,'HH:MM')
                ttViewFGrece.vTag             = fg-rctd.tag
                ttViewFGrece.vPo_no           = fg-rctd.po-no
                ttViewFGrece.vJob_no          = fg-rctd.job-no
                ttViewFGrece.vJob_no2         = fg-rctd.job-no2
                ttViewFGrece.vItem            = fg-rctd.i-no
                ttViewFGrece.vItemName        = fg-rctd.i-name
                ttViewFGrece.vLoc             = fg-rctd.loc 
                ttViewFGrece.vLocBin          = fg-rctd.loc-bin
                ttViewFGrece.vCases           = fg-rctd.cases 
                ttViewFGrece.vQtyCas          = fg-rctd.qty-case 
                ttViewFGrece.vCasUnit         = fg-rctd.cases-unit  
                ttViewFGrece.vPartial         = fg-rctd.partial
                ttViewFGrece.vStdCost         = fg-rctd.std-cost
                ttViewFGrece.vCostUom         = fg-rctd.cost-uom
                ttViewFGrece.vT_Qty           = fg-rctd.t-qty
                ttViewFGrece.vFrtCost         = fg-rctd.frt-cost
                ttViewFGrece.vExtCost         = fg-rctd.ext-cost
                ttViewFGrece.vStackCode       = fg-rctd.stack-code 
                ttViewFGrece.vCreatedBy       = reftable.code
                ttViewFGrece.vCreate2         = reftable.code2  
                ttViewFGrece.vTot_Wt          = fg-rctd.tot-wt 
                ttViewFGrece.vRecKey          = fg-rctd.rec_key .

                      
       END. /*FOR EACH vend-whse-trans*/
 /*END.  /*end of usercust*/*/


   
  END.   /*IF prmAction = "Gridselect" THEN DO:*/




/**********************************end of delete pro***************************/



  

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
  FOR EACH fg-rctd WHERE fg-rctd.company eq cocode and 
           fg-rctd.r-no EQ int(prmCreatedBy)  and 
           (fg-rctd.rita-code eq "R" or fg-rctd.rita-code eq "E") NO-LOCK:

  lv-linker = IF AVAIL fg-rctd                 AND
                 CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ fg-rctd.company
                            AND itemfg.i-no    EQ fg-rctd.i-no
                            AND itemfg.isaset) THEN
                "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
              ELSE "".

  END.

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



PROCEDURE fg-post :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  def buffer b-fg-rcpts for fg-rcpts.
  def buffer b-fg-rdtl for fg-rdtl.
  def buffer b-fg-bin for fg-bin.
  DEF BUFFER b-itemfg FOR itemfg.
  def buffer b-itemfg1 for itemfg.
  def buffer ps-rctd for fg-rctd .
  def buffer b-po-ordl for po-ordl.
  def buffer b-oe-ordl for oe-ordl.

  def var v-one-item as log.
  def var v-dec as dec decimals 10.
  def var v-po-no like rm-rcpt.po-no no-undo.
  def var x as int no-undo.
  def var i as int no-undo.
  def var v-r-qty like fg-rctd.qty no-undo.
  def var v-i-qty like fg-rctd.qty no-undo.
  def var v-t-qty like fg-rctd.qty no-undo.
  def var v-overrun-qty like fg-rctd.qty no-undo.
  def var v-underrun-qty like fg-rctd.qty no-undo.
  DEF VAR v-reduce-qty AS INT NO-UNDO.
  DEF VAR v-est-no AS cha NO-UNDO.
  def var v-recid as recid no-undo.
  DEF VAR v-cost AS DEC NO-UNDO.
  DEF VAR v-binqty AS INT NO-UNDO.
  DEF VAR v-qty AS INT NO-UNDO.
  DEF VAR v-tagcost AS DEC NO-UNDO.
  def var ld-cvt-qty as dec no-undo.
  def var ld-cvt-cost as dec DECIMALS 10 no-undo.
  def var v-autobin  as cha no-undo.
  def var v-newhdr as log no-undo. 
  def var v-fin-qty as dec no-undo.
  def var choice as log no-undo.
  def var v-trnum like gl-ctrl.trnum no-undo.
  def var uperiod as int no-undo.
  def var sysdate as date init today no-undo.    
  def var v-date like sysdate no-undo.
  DEF VAR v-underrun AS DEC NO-UNDO.
  DEF VAR v-qty-received AS INT NO-UNDO.
  DEF VAR v-got-fgemail AS LOG NO-UNDO.
  DEF VAR v-fgemail-file AS cha NO-UNDO.
  DEF VAR li-tag-no AS INT NO-UNDO.
  DEF VAR ll-qty-changed AS LOG NO-UNDO.
  DEF VAR ll-whs-item AS LOG NO-UNDO.

  DEFINE VARIABLE fgPostLog AS LOGICAL NO-UNDO.

  fgPostLog = SEARCH('logs/fgpstall.log') NE ?.
  IF fgPostLog THEN
  OUTPUT STREAM logFile TO VALUE( v-webrootpath + '/fgpstall.' +
         STRING(TODAY,'99999999') + '.' + STRING(TIME) + '.log').

  
  IF fgPostLog THEN RUN fgPostLog ('Started').
  FIND FIRST period NO-LOCK
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date.

  find first sys-ctrl  where sys-ctrl.company eq cocode
                         and sys-ctrl.name    eq "AUTOPOST"
       no-lock no-error.
  v-autobin = IF AVAIL sys-ctrl THEN sys-ctrl.char-fld ELSE "".

  DISABLE TRIGGERS FOR LOAD OF itemfg.
  DISABLE TRIGGERS FOR LOAD OF b-oe-ordl.

  FOR EACH w-fg-rctd:
    DELETE w-fg-rctd.
  END.

  /* Create a single workfile record for the finished good being posted */
  CREATE w-fg-rctd.
  BUFFER-COPY fg-rctd TO w-fg-rctd
  ASSIGN w-fg-rctd.row-id  = ROWID(fg-rctd)
         w-fg-rctd.has-rec = YES.

  FOR EACH w-fg-rctd,

        FIRST itemfg
        WHERE itemfg.company EQ cocode
          AND itemfg.i-no    EQ w-fg-rctd.i-no

        BY w-fg-rctd.tag
        BY w-fg-rctd.rct-date
        BY w-fg-rctd.r-no:

      IF fgPostLog THEN RUN fgPostLog ('Start fg/fg-post.i ' + TRIM(itemfg.i-no)).
      {fg/fg-post.i w-fg-rctd w-fg-rctd}

      FIND CURRENT po-ordl NO-LOCK NO-ERROR.
      FIND CURRENT fg-bin NO-LOCK NO-ERROR.

      IF fgPostLog THEN RUN fgPostLog ('End fg/fg-post.i - Start fg/fgemails.i').
      IF w-fg-rctd.rita-code = "R" THEN DO:
         {fg/fgemails.i}
      END.

      IF fgPostLog THEN RUN fgPostLog ('End fg-bin - Start fg-rctd').

      FIND FIRST fg-rctd WHERE ROWID(fg-rctd) EQ w-fg-rctd.row-id NO-ERROR.

    IF AVAIL fg-rctd THEN DO:
      ASSIGN
       fg-rctd.rita-code = "P"  /* posted */
       fg-rctd.post-date = v-post-date
       fg-rctd.trans-time = TIME
       fg-rctd.tag2      = w-fg-rctd.tag2.

      FOR EACH fg-rcpts
          WHERE fg-rcpts.company EQ fg-rctd.company
            AND fg-rcpts.r-no    EQ fg-rctd.r-no:
        fg-rcpts.rita-code = fg-rctd.rita-code.
      END.
    END.

    IF fgPostLog THEN RUN fgPostLog ('End loop'). 
  END.  /* for each w-fg-rctd */

  FIND CURRENT itemfg NO-LOCK NO-ERROR.

  IF fgPostLog THEN RUN fgPostLog ('End fg/fgemails.i - Start loadtag').
  FOR EACH w-fg-rctd
      BREAK BY w-fg-rctd.i-no
            BY w-fg-rctd.job-no
            BY w-fg-rctd.job-no2
            BY w-fg-rctd.loc
            BY w-fg-rctd.loc-bin
            BY w-fg-rctd.tag:

    IF LAST-OF(w-fg-rctd.tag) THEN DO:
      IF TRIM(w-fg-rctd.tag) NE "" THEN 
      /* Ensure Bin/Tags Qty is correct.  Task 01270602 */

      FOR EACH fg-bin NO-LOCK
          WHERE fg-bin.company EQ g_company
            AND fg-bin.i-no    EQ w-fg-rctd.i-no
            AND fg-bin.tag     EQ w-fg-rctd.tag
          USE-INDEX tag:
        RUN fg/calcbinq.p (ROWID(fg-bin)).
      END.

      /* IF w-fg-rctd.tag <> "" then*/
      FIND FIRST loadtag
          WHERE loadtag.company   EQ g_company
            AND loadtag.item-type EQ NO
            AND loadtag.tag-no    EQ w-fg-rctd.tag
            AND loadtag.i-no      EQ w-fg-rctd.i-no
            AND loadtag.job-no    EQ w-fg-rctd.job-no
          USE-INDEX tag EXCLUSIVE-LOCK NO-ERROR.
      IF fgPostLog THEN RUN fgPostLog ('End loadtag - Start fg-bin').

      IF AVAIL loadtag THEN DO:
        FIND FIRST fg-bin
            WHERE fg-bin.company EQ g_company
              AND fg-bin.i-no    EQ loadtag.i-no
              AND fg-bin.tag     EQ loadtag.tag-no
            /*AND fg-bin.job-no = loadtag.job-no
              AND fg-bin.job-no2 = loadtag.job-no2*/
              AND fg-bin.qty     GT 0
            USE-INDEX tag NO-LOCK NO-ERROR.
        IF w-fg-rctd.rita-code = "T" AND /*loadtag.tot-cases = w-fg-rctd.cases*/
           TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0) = w-fg-rctd.cases THEN  /* full qty transfer*/ 
          ASSIGN
           loadtag.loc          = w-fg-rctd.loc2   
           loadtag.loc-bin      = w-fg-rctd.loc-bin2
           loadtag.qty          = fg-bin.qty
           loadtag.pallet-count = fg-bin.qty
           loadtag.partial      = fg-bin.partial-count
           loadtag.tot-cases    = (loadtag.qty - loadtag.partial) / loadtag.qty-case.
        ELSE /*partial transfer */
          ASSIGN
           loadtag.loc     = w-fg-rctd.loc
           loadtag.loc-bin = w-fg-rctd.loc-bin. 
          FIND CURRENT loadtag NO-LOCK NO-ERROR.
      END.
    END.
  END.

  FOR EACH w-inv:
    DELETE w-inv.
  END.

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Second For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE w-fg-rctd.invoiced,
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK:

    CREATE w-inv.
    w-inv.row-id = w-fg-rctd.row-id.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Second For Each w-fg-rctd').

  IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/invrecpt.p').
  RUN fg/invrecpt.p (?, 2).
  IF fgPostLog THEN RUN fgPostLog ('End Run fg/invrecpt.p').

  IF fgPostLog THEN RUN fgPostLog ('End First - Start Third For Each w-fg-rctd').
  FOR EACH w-fg-rctd WHERE TRIM(w-fg-rctd.tag) EQ "",
      FIRST itemfg
      WHERE itemfg.company EQ cocode
        AND itemfg.i-no    EQ w-fg-rctd.i-no
      NO-LOCK
      BREAK BY w-fg-rctd.i-no:

    IF LAST-OF(w-fg-rctd.i-no) THEN DO:
      IF fgPostLog THEN RUN fgPostLog ('Begin Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).
      RUN fg/updfgcs1.p (RECID(itemfg), NO).
      IF fgPostLog THEN RUN fgPostLog ('End Run fg/updfgcs1.p for ' + w-fg-rctd.i-no).

      FOR EACH oe-ordl
          WHERE oe-ordl.company EQ cocode
            AND oe-ordl.opened  EQ YES
            AND oe-ordl.i-no    EQ w-fg-rctd.i-no
            AND oe-ordl.job-no  EQ ""
            AND oe-ordl.cost    EQ 0
          USE-INDEX opened NO-LOCK
          BREAK BY oe-ordl.ord-no
          TRANSACTION:
           
        DO i = 1 TO 1000:
          FIND b-oe-ordl WHERE ROWID(b-oe-ordl) EQ ROWID(oe-ordl) EXCLUSIVE NO-ERROR NO-WAIT.
          IF AVAIL b-oe-ordl THEN DO:
            IF itemfg.prod-uom EQ "M" THEN
              b-oe-ordl.cost = itemfg.total-std-cost.
            ELSE
              RUN sys/ref/convcuom.p((IF LOOKUP(itemfg.prod-uom,fg-uom-list) GT 0
                                      THEN "EA" ELSE itemfg.prod-uom),
                                     "M", 0, 0, 0, 0,
                                     itemfg.total-std-cost, OUTPUT b-oe-ordl.cost).
            LEAVE.
          END.
        END.
      END.
    END.
  END.
  IF fgPostLog THEN RUN fgPostLog ('End Third For Each w-fg-rctd').

  IF v-fgpostgl NE "None" THEN DO TRANSACTION:
    /* gdm - 11050906 */
    REPEAT:
     FIND FIRST gl-ctrl EXCLUSIVE-LOCK
       WHERE gl-ctrl.company EQ cocode NO-ERROR NO-WAIT.
     IF AVAIL gl-ctrl THEN DO:
       ASSIGN v-trnum       = gl-ctrl.trnum + 1
              gl-ctrl.trnum = v-trnum.

       FIND CURRENT gl-ctrl NO-LOCK.

       IF fgPostLog THEN RUN fgPostLog ('Begin Run gl-from-work 1').
       RUN gl-from-work (1, v-trnum).
       IF fgPostLog THEN RUN fgPostLog ('End 1 - Begin Run gl-from-work 2').
       RUN gl-from-work (2, v-trnum).
       IF fgPostLog THEN RUN fgPostLog ('End Run gl-from-work 2').
       LEAVE .
     END. /* IF AVAIL gl-ctrl */
    END. /* REPEAT */
    /* gdm - 11050906 */
  END.
  find first w-job no-error.
  if avail w-job THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start jc/d-jclose.p').
    /*run jc/d-jclose.w.*/
    IF fgPostLog THEN RUN fgPostLog ('End jc/d-jclose.p').
  END.

  if v-adjustgl then do TRANSACTION:
    /** GET next G/L TRANS. POSTING # **/
    find first gl-ctrl where gl-ctrl.company eq cocode exclusive-lock.
    assign
     v-trnum       = gl-ctrl.trnum + 1
     gl-ctrl.trnum = v-trnum.
    FIND CURRENT gl-ctrl NO-LOCK.
    IF fgPostLog THEN RUN fgPostLog ('Start For Each work-job').
    for each work-job break by work-job.actnum:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-job.actnum
       gltrans.jrnl    = "ADJUST"
       gltrans.tr-date = v-post-date
       gltrans.period  = period.pnum
       gltrans.trnum   = v-trnum.
    
      if work-job.fg then
        assign
         gltrans.tr-amt  = - work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT FG".
      else
        assign
         gltrans.tr-amt  = work-job.amt
         gltrans.tr-dscr = "ADJUSTMENT COGS".

        RELEASE gltrans.
    end. /* each work-job */
    IF fgPostLog THEN RUN fgPostLog ('End For Each work-job').
  end.
  /*IF v-got-fgemail THEN DO:
    IF fgPostLog THEN RUN fgPostLog ('Start Run send-fgemail').
    RUN send-fgemail (v-fgemail-file).
    IF fgPostLog THEN RUN fgPostLog ('End Run send-fgemail').
  END.*/
  IF fgPostLog THEN RUN fgPostLog ('End').
  IF fgPostLog THEN OUTPUT STREAM logFile CLOSE.
 
END PROCEDURE.

PROCEDURE fgPostLog :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE INPUT PARAMETER ipLogText AS CHARACTER NO-UNDO.

  PUT STREAM logFile UNFORMATTED STRING(TODAY,'99.99.9999') ' '
    STRING(TIME,'hh:mm:ss am') ' : ' ipLogText SKIP.

END PROCEDURE.


PROCEDURE gl-from-work :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF INPUT PARAM ip-run AS INT NO-UNDO.
  DEF INPUT PARAM ip-trnum AS INT NO-UNDO.
  
  def var credits as dec init 0 no-undo.
  def var debits as dec init 0 no-undo. 

  
  FIND FIRST period
      WHERE period.company EQ cocode
        AND period.pst     LE v-post-date
        AND period.pend    GE v-post-date
      NO-LOCK.

  for each work-gl 
      where (ip-run eq 1 and work-gl.job-no ne "")
         or (ip-run eq 2 and work-gl.job-no eq "")
      break by work-gl.actnum:
      
    assign
     debits  = debits  + work-gl.debits
     credits = credits + work-gl.credits.

    if last-of(work-gl.actnum) then do:
      create gltrans.
      assign
       gltrans.company = cocode
       gltrans.actnum  = work-gl.actnum
       gltrans.jrnl    = "FGPOST"
       gltrans.period  = period.pnum
       gltrans.tr-amt  = debits - credits
       gltrans.tr-date = v-post-date
       gltrans.tr-dscr = if work-gl.job-no ne "" then "FG Receipt from Job"
                                                 else "FG Receipt from PO"
       gltrans.trnum   = ip-trnum.

      assign
       debits  = 0
       credits = 0.
    end.
  end.

END PROCEDURE.



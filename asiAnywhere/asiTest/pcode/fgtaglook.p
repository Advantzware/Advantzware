/*------------------------------------------------------------------------
    File        : fgtaglook.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttfgtagLook NO-UNDO 
    FIELD  tag             AS CHAR  
    FIELD vPoNo            AS INT
    FIELD vOrdQty          AS DEC
    FIELD vINo             AS CHARACTER
    FIELD vIName           AS CHARACTER
    FIELD vJobNo           AS CHARACTER
    FIELD vJob2            AS INT
    FIELD vLoc             AS CHARACTER
    FIELD vLocBin          AS CHAR
    FIELD vunitCount       AS INT
    FIELD vitmtype         AS CHAR
    FIELD viscstag         AS CHAR
    FIELD vunitprplt       AS INT
    FIELD vUom             AS CHAR
    FIELD vStdCost         AS DEC
    FIELD  cost            AS DEC
    FIELD  totqty          AS DEC
    FIELD  extcost         AS DECIMAL
    FIELD  unit            AS INT
    FIELD  ordno           AS INT
    FIELD  qtycase         AS INT
    FIELD  plltcnt         AS INT
    FIELD  pruom           AS CHAR
    FIELD  frtcst          AS DEC
    FIELD  partial         AS INT
     .

DEFINE DATASET dsfgtagLook FOR ttfgtagLook .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmitmtype      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcurval       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsfgtagLook.
   
DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR lv-cost AS DEC DECIMALS 4 NO-UNDO.

DEF VAR v-cost AS DEC NO-UNDO.
DEF VAR lv-loc LIKE fg-rctd.loc NO-UNDO.
DEF VAR lv-loc-bin LIKE fg-rctd.loc-bin NO-UNDO.
DEF VAR lv-qty-case LIKE fg-rctd.qty-case NO-UNDO.
DEF VAR lv-cases LIKE fg-rctd.cases NO-UNDO.
DEF VAR lv-cases-unit LIKE fg-rctd.cases-unit NO-UNDO.
DEF VAR lv-partial LIKE fg-rctd.partial NO-UNDO.
DEF VAR ll-casetag AS LOG NO-UNDO.
DEF VAR lv-do-what AS cha NO-UNDO.
DEF VAR v-count AS INT NO-UNDO.

IF prmAction    = ? THEN ASSIGN prmAction    = "".
IF prmUser      = ? THEN ASSIGN prmUser      = "".
IF prmCondition = ? THEN ASSIGN prmCondition = "".
IF prmText      = ? THEN ASSIGN prmText      = "".
IF prmField     = ? THEN ASSIGN prmField     = "".
IF prmitmtype   = ? THEN ASSIGN prmitmtype   = "".
IF prmcurval    = ? THEN ASSIGN prmcurval    = "".


FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

{custom/globdefs.i}
{sys/inc/VAR.i "new shared" }
ASSIGN cocode = prmComp .

DEF BUFFER reftable-job FOR reftable.

{fg/invrecpt.i NEW}



{sys/inc/fgpofrt.i}
  {sys/inc/fgrecpt.i}
  {sys/inc/autopost.i}
  {sys/inc/fgsetrec.i}
  {sys/inc/oeship.i}
  {sys/inc/fgsecur.i}

FIND FIRST sys-ctrl
      WHERE sys-ctrl.company EQ prmComp
        AND sys-ctrl.name    EQ "CASETAG"
      NO-LOCK NO-ERROR.
  IF NOT AVAIL sys-ctrl THEN
  DO TRANSACTION:
    CREATE sys-ctrl.
    ASSIGN
     sys-ctrl.company  = prmComp
     sys-ctrl.name     = "CASETAG"
     sys-ctrl.descrip  = "Case Label Format?   Use Case Label as LoadTag?".
  END.
  ll-casetag = sys-ctrl.log-fld.

  
IF prmcurval <> "Delete" THEN do:

  if prmAction = "PoSelect" then do:
 v-count = 0.
 MAIN-LOOP:

    FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     and loadtag.is-case-tag = ll-casetag NO-LOCK:
   
   
   RELEASE fg-bin.
   
     FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         BY fg-bin.qty DESC:
       LEAVE.
     END.

     find first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = IF lv-do-what EQ "Delete" THEN loadtag.tot-cases
                                                ELSE loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   IF lv-do-what EQ "Delete" THEN
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
    .
    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
      
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

    v-count = v-count + 1.
                      IF v-count = 100 THEN LEAVE MAIN-LOOP.

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "PoSearch" then do:
     if prmField = "Tag#"  then do:
         if prmCondition = "EQUAL" then do:

             FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO
                     AND loadtag.tag-no = prmText
                     and loadtag.is-case-tag = ll-casetag NO-LOCK:
   RELEASE fg-bin.
   
     FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         BY fg-bin.qty DESC:
       LEAVE.
     END.

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = IF lv-do-what EQ "Delete" THEN loadtag.tot-cases
                                                ELSE loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   IF lv-do-what EQ "Delete" THEN
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case) 
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
      .                 

    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */
             
 END.
 
 IF prmCondition = "BEGIN" then do:

                       FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     AND loadtag.tag-no BEGINS prmText
                     and loadtag.is-case-tag = ll-casetag NO-LOCK:
   
   
   RELEASE fg-bin.
   
     FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         BY fg-bin.qty DESC:
       LEAVE.
     END.

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
 

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = IF lv-do-what EQ "Delete" THEN loadtag.tot-cases
                                                ELSE loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   IF lv-do-what EQ "Delete" THEN
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
       .                 

    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */
              
         END.
     
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
          if prmCondition = "EQUAL" then do:
                        FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     and loadtag.is-case-tag = ll-casetag NO-LOCK:
   
   
   RELEASE fg-bin.
   
     FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         BY fg-bin.qty DESC:
       LEAVE.
     END.

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
 

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = IF lv-do-what EQ "Delete" THEN loadtag.tot-cases
                                                ELSE loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   IF lv-do-what EQ "Delete" THEN
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
       .                 

   RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */              
          END. /*FOR EACH prmcondition*/

          IF prmCondition = "BEGIN" then do:
                        FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     and loadtag.is-case-tag = ll-casetag NO-LOCK:
   
   
   RELEASE fg-bin.
   
     FOR EACH fg-bin NO-LOCK
         WHERE fg-bin.company EQ loadtag.company
           AND fg-bin.i-no    EQ loadtag.i-no
           AND fg-bin.tag     EQ loadtag.tag-no
           AND fg-bin.qty     GT 0
         BY fg-bin.qty DESC:
       LEAVE.
     END.

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
 

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = IF lv-do-what EQ "Delete" THEN loadtag.tot-cases
                                                ELSE loadtag.case-bundle
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   IF lv-do-what EQ "Delete" THEN
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 
 
   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count  
        .                 

    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */
              
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */


END.  /* prmcerv <> delete */


/**************************************************************************************/

IF prmcurval = "Delete" THEN DO:

if prmAction = "PoSelect" then do:
v-count = 0.
 MAIN-LOOP:
    FOR EACH loadtag WHERE  loadtag.company = cocode and 
        loadtag.item-type = NO NO-LOCK, 
        FIRST fg-bin WHERE fg-bin.company = loadtag.company 
        AND fg-bin.i-no = loadtag.i-no 
        AND fg-bin.job-no = loadtag.job-no 
        AND fg-bin.job-no2 = loadtag.job-no2 
        /*  AND fg-bin.loc = loadtag.loc ~
        AND fg-bin.loc-bin = loadtag.loc-bin ~
        */ 
        AND fg-bin.tag = loadtag.tag-no 
        AND fg-bin.qty > 0  NO-LOCK :

     find first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      =  loadtag.tot-cases
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.
   
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
    .
    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
      
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

    v-count = v-count + 1.
          IF v-count = 100 THEN LEAVE MAIN-LOOP.

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "PoSearch" then do:
     if prmField = "Tag#"  then do:
         if prmCondition = "EQUAL" then do:

             
   FOR EACH loadtag WHERE  loadtag.company = cocode and 
        loadtag.tag-no = prmText AND
        loadtag.item-type = NO NO-LOCK, 
        FIRST fg-bin WHERE fg-bin.company = loadtag.company 
        AND fg-bin.i-no = loadtag.i-no 
        AND fg-bin.job-no = loadtag.job-no 
        AND fg-bin.job-no2 = loadtag.job-no2 
        /*  AND fg-bin.loc = loadtag.loc ~
        AND fg-bin.loc-bin = loadtag.loc-bin ~
        */ 
        AND fg-bin.tag = loadtag.tag-no 
        AND fg-bin.qty > 0  NO-LOCK :

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      =  loadtag.tot-cases
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case) 
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
      .                 

    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */
             
 END.
 
 IF prmCondition = "BEGIN" then do:

    FOR EACH loadtag WHERE  loadtag.company = cocode and 
        loadtag.tag-no BEGINS prmText AND
        loadtag.item-type = NO NO-LOCK, 
        FIRST fg-bin WHERE fg-bin.company = loadtag.company 
        AND fg-bin.i-no = loadtag.i-no 
        AND fg-bin.job-no = loadtag.job-no 
        AND fg-bin.job-no2 = loadtag.job-no2 
        /*  AND fg-bin.loc = loadtag.loc ~
        AND fg-bin.loc-bin = loadtag.loc-bin ~
        */ 
        AND fg-bin.tag = loadtag.tag-no 
        AND fg-bin.qty > 0  NO-LOCK :

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
 

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      =  loadtag.tot-cases
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.
   
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
       .                 

    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */
              
         END.
     
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
          if prmCondition = "EQUAL" then do:
      FOR EACH loadtag WHERE  loadtag.company = cocode and 
        loadtag.i-no = prmText AND
        loadtag.item-type = NO NO-LOCK, 
        FIRST fg-bin WHERE fg-bin.company = loadtag.company 
        AND fg-bin.i-no = loadtag.i-no 
        AND fg-bin.job-no = loadtag.job-no 
        AND fg-bin.job-no2 = loadtag.job-no2 
        /*  AND fg-bin.loc = loadtag.loc ~
        AND fg-bin.loc-bin = loadtag.loc-bin ~
        */ 
        AND fg-bin.tag = loadtag.tag-no 
        AND fg-bin.qty > 0  NO-LOCK :

     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
 

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      =  loadtag.tot-cases       
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.

   
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 

   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count
       .                 

   RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */              
          END. /*FOR EACH prmcondition*/

  IF prmCondition = "BEGIN" then do:
     FOR EACH loadtag WHERE  loadtag.company = cocode and 
        loadtag.i-no BEGINS prmText AND
        loadtag.item-type = NO NO-LOCK, 
        FIRST fg-bin WHERE fg-bin.company = loadtag.company 
        AND fg-bin.i-no = loadtag.i-no 
        AND fg-bin.job-no = loadtag.job-no 
        AND fg-bin.job-no2 = loadtag.job-no2 
        /*  AND fg-bin.loc = loadtag.loc ~
        AND fg-bin.loc-bin = loadtag.loc-bin ~
        */ 
        AND fg-bin.tag = loadtag.tag-no 
        AND fg-bin.qty > 0  NO-LOCK :
     FIND first itemfg  where itemfg.company  = cocode 
             and itemfg.i-no = loadtag.i-no
                 no-lock no-error.
 

   IF AVAIL fg-bin THEN
     ASSIGN
      lv-loc        = fg-bin.loc
      lv-loc-bin    = fg-bin.loc-bin
      lv-qty-case   = fg-bin.case-count
      lv-cases      = TRUNC((fg-bin.qty - fg-bin.partial-count) /
                            fg-bin.case-count,0)
      lv-cases-unit = fg-bin.cases-unit
      lv-partial    = fg-bin.partial-count.

   ELSE
     ASSIGN
      lv-loc        = loadtag.loc
      lv-loc-bin    = loadtag.loc-bin
      lv-qty-case   = loadtag.qty-case
      lv-cases      = loadtag.tot-cases         
      lv-cases-unit = loadtag.case-bundle
      lv-partial    = loadtag.partial.
   
     ASSIGN
      lv-cases   = lv-cases * -1
      lv-partial = lv-partial * -1. 
 
   CREATE ttfgtagLook.

   ASSIGN
    ttfgtagLook.tag        = loadtag.tag-no
    ttfgtagLook.vPoNo      = loadtag.po-no
    ttfgtagLook.vJobNo     = loadtag.job-no 
    ttfgtagLook.vJob2      = int(loadtag.job-no2)
    ttfgtagLook.vINo       = loadtag.i-no 
    ttfgtagLook.vIName     = loadtag.i-name
    ttfgtagLook.vLoc       = STRING(lv-loc)
    ttfgtagLook.vLocBin    = string(lv-loc-bin)
    ttfgtagLook.vunitCount = INT(lv-qty-case)
    ttfgtagLook.unit       = INT(lv-cases)
    ttfgtagLook.vunitprplt = INT(lv-cases-unit)
    ttfgtagLook.partial    = INT(lv-partial)
    ttfgtagLook.totqty     = (ttfgtagLook.unit * ttfgtagLook.vunitCount) + 
                                    ttfgtagLook.partial
    ttfgtagLook.ordno      = loadtag.ord-no
    ttfgtagLook.vOrdQty    = loadtag.qty
    ttfgtagLook.plltcnt    = loadtag.pallet-count  
        .                 

    RUN get-values.

   FIND FIRST job-hdr WHERE job-hdr.company = prmComp
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttfgtagLook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttfgtagLook.vStdCost = reftable.val[5].
    else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, ttfgtagLook.vUom, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        ttfgtagLook.vStdCost = v-cost.
      END.
     
      else
      if avail itemfg then
        assign
         ttfgtagLook.vUom = itemfg.prod-uom
         ttfgtagLook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR ttfgtagLook.vStdCost = 0 THEN DO:
      FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp
                           AND oe-ordl.i-no = ttfgtagLook.vINo
                           AND oe-ordl.job-no = ttfgtagLook.vJobNo
                           AND oe-ordl.job-no2 = loadtag.job-no2 NO-LOCK NO-ERROR.
      IF AVAIL oe-ordl THEN
           ASSIGN ttfgtagLook.vStdCost = oe-ordl.cost
                  ttfgtagLook.vUom     = "M".
   END.

  END. /* for each loadtag */
              
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */


END.  /*      */









PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  
  
  
  
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ ttfgtagLook.vINo
          AND fg-bin.job-no  EQ ttfgtagLook.vJobNo
          AND fg-bin.job-no2 EQ ttfgtagLook.vJob2
          AND fg-bin.loc     EQ ttfgtagLook.vLoc
          AND fg-bin.loc-bin EQ ttfgtagLook.vLocBin
          AND fg-bin.tag     EQ ttfgtagLook.tag
        NO-LOCK NO-ERROR.
    IF AVAIL fg-bin THEN
      ASSIGN
       ttfgtagLook.vStdCost = fg-bin.std-tot-cost
       ttfgtagLook.vUom     = fg-bin.pur-uom.
  

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

  {sys/inc/fgrecpt.i}
  fgrecpt = fgrecpt.

 
    find first itemfg
         where itemfg.company  = cocode 
          and itemfg.i-no EQ loadtag.i-no
        no-lock no-error.

    /*find first fg-ctrl where fg-ctrl.company eq cocode no-lock no-error.*/
 IF AVAIL itemfg THEN
    ASSIGN
    ttfgtagLook.vIName = itemfg.i-name
    lv-qty-case = string(itemfg.case-count)
    lv-cost-uom = if itemfg.pur-man then itemfg.pur-uom else itemfg.prod-uom.
    
   IF AVAIL itemfg THEN
    RUN fg/autopost.p (ROWID(itemfg) ,
                       ttfgtagLook.vJobNo,
                       ttfgtagLook.vJob2,
                       OUTPUT lv-loc, OUTPUT lv-loc-bin).

    find first fg-bin
        where fg-bin.company eq cocode
          and fg-bin.loc     eq lv-loc
          and fg-bin.loc-bin eq lv-loc-bin
          and fg-bin.i-no    eq ""
        no-lock no-error.
    if avail fg-bin then 
      assign
       lv-std-cost = IF ttfgtagLook.vPoNo = 0 and
                        ttfgtagLook.vJobNo = "" 
                     THEN string(IF AVAIL itemfg THEN itemfg.last-cost ELSE 0) 
                     ELSE lv-std-cost
       lv-qty-case = IF AVAIL itemfg THEN STRING(itemfg.case-count) ELSE "0" 
       lv-cost-uom = IF AVAIL itemfg THEN itemfg.prod-uom ELSE "".

    ASSIGN
     lv-save[1] =  string(ttfgtagLook.vStdCost) 
     lv-save[2] =  ttfgtagLook.vUom     .

    RUN get-fg-bin-cost.

    ASSIGN
     lv-std-cost =  STRING(ttfgtagLook.vStdCost) 
     lv-cost-uom =  ttfgtagLook.vUom     

      ttfgtagLook.vStdCost = DEC(lv-save[1])
      ttfgtagLook.vUom     = lv-save[2].

    /**  Find the Job Header record in then job file and use Standard Cost
         from that job. **/
    find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.i-no    eq ttfgtagLook.vINo
          and job-hdr.job-no  eq ttfgtagLook.vJobNo
          and job-hdr.job-no2 eq ttfgtagLook.vJob2
        NO-LOCK no-error.

    IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttfgtagLook.vJobNo
            AND job.job-no2 EQ ttfgtagLook.vJob2
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable-job
          WHERE reftable-job.reftable EQ "jc/jc-calc.p"
            AND reftable-job.company  EQ job.company
            AND reftable-job.loc      EQ ""
            AND reftable-job.code     EQ STRING(job.job,"999999999")
            AND reftable-job.code2    EQ ttfgtagLook.vINo
          NO-LOCK NO-ERROR.
    END.
       
    if avail job-hdr and job-hdr.std-tot-cost gt 0 then
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
            and po-ordl.po-no     eq ttfgtagLook.vPoNo
            and po-ordl.i-no      eq ttfgtagLook.vINo
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        ASSIGN
         lv-cost-uom = po-ordl.pr-uom.
         lv-std-cost = STRING(po-ordl.cost).

        RUN show-freight.
      END.
     
      else
      if avail itemfg          AND
         DEC(lv-std-cost) EQ 0 THEN DO:
        assign
         lv-cost-uom = itemfg.prod-uom
         lv-std-cost = STRING(itemfg.total-std-cost).

        IF AVAIL itemfg AND itemfg.isaset THEN DO:
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

    IF ttfgtagLook.vLoc      EQ "" OR
       ttfgtagLook.vLocBin   EQ "" THEN
      ASSIGN
       ttfgtagLook.vLoc      = lv-loc
       ttfgtagLook.vLocBin   = lv-loc-bin.

    IF ttfgtagLook.vunitCount EQ 0 THEN
      ttfgtagLook.vunitCount = int(lv-qty-case).

    IF ttfgtagLook.vUom EQ "" THEN
      ttfgtagLook.vUom = lv-cost-uom.

    IF ttfgtagLook.vStdCost EQ 0 THEN
      ttfgtagLook.vStdCost = DEC(lv-std-cost).

    
END PROCEDURE.

PROCEDURE show-freight :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.


  IF fgpofrt-log THEN 
 
    ASSIGN
     ld = ttfgtagLook.frtcst
     ttfgtagLook.extcost = ttfgtagLook.extcost - ld.

    RUN get-freight-cost (OUTPUT ld).

    ASSIGN
     ttfgtagLook.frtcst = ld
     ttfgtagLook.extcost = ttfgtagLook.extcost + ld.


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
        WHERE po-ordl.company   EQ cocode
          AND po-ordl.po-no     EQ ttfgtagLook.vPoNo
          AND po-ordl.i-no      EQ ttfgtagLook.vINo
          AND po-ordl.job-no    EQ ttfgtagLook.vJobNo
          AND po-ordl.job-no2   EQ ttfgtagLook.vJob2
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN
      RUN po/getfrtcs.p (ROWID(po-ordl),
                         ttfgtagLook.totqty,
                         OUTPUT op-cost).


END PROCEDURE.

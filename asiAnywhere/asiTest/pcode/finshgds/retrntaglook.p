/*------------------------------------------------------------------------
    File        : retrntaglook.p
    Purpose     : 
    Syntax      :       
    Description : Return a Dataset of UserMaintenance
    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/
/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttreturntaglook NO-UNDO 
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
    FIELD  abc             AS CHAR
     .

DEFINE DATASET dsreturntaglook FOR ttreturntaglook .

DEFINE INPUT PARAMETER prmAction       AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmField        AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmText         AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmitmtype      AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmcurval       AS CHARACTER  NO-UNDO.
DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsreturntaglook.
   
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

  


  if prmAction = "PoSelect" then do:

    FOR EACH loadtag WHERE loadtag.company = prmComp 
                     and loadtag.item-type = NO 
                     and loadtag.is-case-tag = ll-casetag  NO-LOCK BY loadtag.tag-no:

       FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = loadtag.company
                                 AND fg-rdtlh.tag = loadtag.tag-no
                                 and fg-rdtlh.rita-code ne "S" NO-LOCK NO-ERROR.
       IF AVAIL fg-rdtlh  THEN NEXT.

        
   CREATE ttreturntaglook.

   ASSIGN
    ttreturntaglook.tag        = loadtag.tag-no
    ttreturntaglook.vPoNo      = loadtag.po-no
    ttreturntaglook.vJobNo     = loadtag.job-no 
    ttreturntaglook.vJob2      = int(loadtag.job-no2)
    ttreturntaglook.vINo       = loadtag.i-no 
    ttreturntaglook.vIName     = loadtag.i-name
    ttreturntaglook.vLoc       = STRING(loadtag.loc)
    ttreturntaglook.vLocBin    = string(loadtag.loc-bin)
    ttreturntaglook.vunitCount = INT(loadtag.qty-case)
    ttreturntaglook.unit       = INT(loadtag.case-bundle)
    ttreturntaglook.vunitprplt = INT(loadtag.case-bundle)
    ttreturntaglook.partial    = INT(loadtag.partial)
    ttreturntaglook.totqty     = loadtag.pallet-count
    ttreturntaglook.ordno      = loadtag.ord-no
    ttreturntaglook.vOrdQty    = loadtag.qty
    ttreturntaglook.plltcnt    = loadtag.pallet-count
    .
    RUN get-fg-bin-cost.

   
   

  END. /* for each loadtag */



END.  /*ifif prmAction <> "search" */

/******************Search***********************************/
 
IF prmAction = "PoSearch" then do:
    if prmField = "Tag#"  then do:
        if prmCondition = "EQUAL" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO
                AND loadtag.tag-no = prmText
                and loadtag.is-case-tag = ll-casetag NO-LOCK BY loadtag.tag-no:

        FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = loadtag.company
                                 AND fg-rdtlh.tag = loadtag.tag-no
                                 and fg-rdtlh.rita-code ne "S" NO-LOCK NO-ERROR.
       IF AVAIL fg-rdtlh  THEN NEXT.


   
   CREATE ttreturntaglook.

   ASSIGN
     ttreturntaglook.tag        = loadtag.tag-no
    ttreturntaglook.vPoNo      = loadtag.po-no
    ttreturntaglook.vJobNo     = loadtag.job-no 
    ttreturntaglook.vJob2      = int(loadtag.job-no2)
    ttreturntaglook.vINo       = loadtag.i-no 
    ttreturntaglook.vIName     = loadtag.i-name
    ttreturntaglook.vLoc       = STRING(loadtag.loc)
    ttreturntaglook.vLocBin    = string(loadtag.loc-bin)
    ttreturntaglook.vunitCount = INT(loadtag.qty-case)
    ttreturntaglook.unit       = INT(loadtag.case-bundle)
    ttreturntaglook.vunitprplt = INT(loadtag.case-bundle)
    ttreturntaglook.partial    = INT(loadtag.partial)
    ttreturntaglook.totqty     = loadtag.pallet-count
    ttreturntaglook.ordno      = loadtag.ord-no
    ttreturntaglook.vOrdQty    = loadtag.qty
    ttreturntaglook.plltcnt    = loadtag.pallet-count
    .
    RUN get-fg-bin-cost.

   
            END. /* for each loadtag */
        END.

        
        IF prmCondition = "BEGIN" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO 
                AND loadtag.tag-no BEGINS prmText
                and loadtag.is-case-tag = ll-casetag NO-LOCK BY loadtag.tag-no:

               FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = loadtag.company
                                 AND fg-rdtlh.tag = loadtag.tag-no
                                 and fg-rdtlh.rita-code ne "S" NO-LOCK NO-ERROR.
       IF AVAIL fg-rdtlh  THEN NEXT.
                
            


   CREATE ttreturntaglook.

   ASSIGN
     ttreturntaglook.tag        = loadtag.tag-no
    ttreturntaglook.vPoNo      = loadtag.po-no
    ttreturntaglook.vJobNo     = loadtag.job-no 
    ttreturntaglook.vJob2      = int(loadtag.job-no2)
    ttreturntaglook.vINo       = loadtag.i-no 
    ttreturntaglook.vIName     = loadtag.i-name
    ttreturntaglook.vLoc       = STRING(loadtag.loc)
    ttreturntaglook.vLocBin    = string(loadtag.loc-bin)
    ttreturntaglook.vunitCount = INT(loadtag.qty-case)
    ttreturntaglook.unit       = INT(loadtag.case-bundle)
    ttreturntaglook.vunitprplt = INT(loadtag.case-bundle)
    ttreturntaglook.partial    = INT(loadtag.partial)
    ttreturntaglook.totqty     = loadtag.pallet-count
    ttreturntaglook.ordno      = loadtag.ord-no
    ttreturntaglook.vOrdQty    = loadtag.qty
    ttreturntaglook.plltcnt    = loadtag.pallet-count
    .
    RUN get-fg-bin-cost.

   
            END. /* for each loadtag */
        END.
     END.  /* if prmField = state  */


     if prmField = "Item"  then do:
         if prmCondition = "EQUAL" then do:
             FOR EACH loadtag WHERE loadtag.company = prmComp 
                 and loadtag.item-type = NO 
                 and loadtag.is-case-tag = ll-casetag NO-LOCK BY loadtag.tag-no:

                FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = loadtag.company
                                 AND fg-rdtlh.tag = loadtag.tag-no
                                 and fg-rdtlh.rita-code ne "S" NO-LOCK NO-ERROR.
       IF AVAIL fg-rdtlh  THEN NEXT.
                 
   
   CREATE ttreturntaglook.

   ASSIGN
     ttreturntaglook.tag        = loadtag.tag-no
    ttreturntaglook.vPoNo      = loadtag.po-no
    ttreturntaglook.vJobNo     = loadtag.job-no 
    ttreturntaglook.vJob2      = int(loadtag.job-no2)
    ttreturntaglook.vINo       = loadtag.i-no 
    ttreturntaglook.vIName     = loadtag.i-name
    ttreturntaglook.vLoc       = STRING(loadtag.loc)
    ttreturntaglook.vLocBin    = string(loadtag.loc-bin)
    ttreturntaglook.vunitCount = INT(loadtag.qty-case)
    ttreturntaglook.unit       = INT(loadtag.case-bundle)
    ttreturntaglook.vunitprplt = INT(loadtag.case-bundle)
    ttreturntaglook.partial    = INT(loadtag.partial)
    ttreturntaglook.totqty     = loadtag.pallet-count
    ttreturntaglook.ordno      = loadtag.ord-no
    ttreturntaglook.vOrdQty    = loadtag.qty
    ttreturntaglook.plltcnt    = loadtag.pallet-count
    .
    RUN get-fg-bin-cost. 
   

             END. /* for each loadtag */              
         END. /*FOR EACH prmcondition*/

        IF prmCondition = "BEGIN" then do:
            FOR EACH loadtag WHERE loadtag.company = prmComp 
                and loadtag.item-type = NO 
                and loadtag.is-case-tag = ll-casetag NO-LOCK BY loadtag.tag-no:

                FIND FIRST fg-rdtlh WHERE fg-rdtlh.company = loadtag.company
                                 AND fg-rdtlh.tag = loadtag.tag-no
                                 and fg-rdtlh.rita-code ne "S" NO-LOCK NO-ERROR.
       IF AVAIL fg-rdtlh  THEN NEXT.
                
   
 
   CREATE ttreturntaglook.

   ASSIGN
    ttreturntaglook.tag        = loadtag.tag-no
    ttreturntaglook.vPoNo      = loadtag.po-no
    ttreturntaglook.vJobNo     = loadtag.job-no 
    ttreturntaglook.vJob2      = int(loadtag.job-no2)
    ttreturntaglook.vINo       = loadtag.i-no 
    ttreturntaglook.vIName     = loadtag.i-name
    ttreturntaglook.vLoc       = STRING(loadtag.loc)
    ttreturntaglook.vLocBin    = string(loadtag.loc-bin)
    ttreturntaglook.vunitCount = INT(loadtag.qty-case)
    ttreturntaglook.unit       = INT(loadtag.case-bundle)
    ttreturntaglook.vunitprplt = INT(loadtag.case-bundle)
    ttreturntaglook.partial    = INT(loadtag.partial)
    ttreturntaglook.totqty     = loadtag.pallet-count
    ttreturntaglook.ordno      = loadtag.ord-no
    ttreturntaglook.vOrdQty    = loadtag.qty
    ttreturntaglook.plltcnt    = loadtag.pallet-count
    .
    RUN get-fg-bin-cost.

   

            END. /* for each loadtag */
          END.    /*if prmCondition = BEGIN*/    
     END.  /* if prmField = state  */
END.  /* IF prmAction = search then do: */





/**************************************************************************************/









PROCEDURE get-fg-bin-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
 
    IF ttreturntaglook.vJobNo <> "" THEN
    FIND FIRST fg-bin
        WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ ttreturntaglook.vINo
          AND fg-bin.job-no  EQ ttreturntaglook.vJobNo
          AND fg-bin.job-no2 EQ INT(ttreturntaglook.vJob2)
          AND fg-bin.loc     EQ ttreturntaglook.vLoc
          AND fg-bin.loc-bin EQ ttreturntaglook.vLocBin
          AND fg-bin.tag     EQ ttreturntaglook.tag
        NO-LOCK NO-ERROR.
    ELSE DO:
        FIND FIRST loadtag WHERE loadtag.company = cocode
                             AND loadtag.item-type = NO
                             AND loadtag.tag-no = ttreturntaglook.tag NO-LOCK NO-ERROR.
        IF AVAIL loadtag THEN
           FIND FIRST fg-bin WHERE
                fg-bin.company EQ cocode
                  AND fg-bin.i-no    EQ ttreturntaglook.vINo 
                  AND fg-bin.job-no  EQ loadtag.job-no
                  AND fg-bin.job-no2 EQ loadtag.job-no2
                  AND fg-bin.loc     EQ ttreturntaglook.vLoc
                  AND fg-bin.loc-bin EQ ttreturntaglook.vLocBin 
                  AND fg-bin.tag     EQ ttreturntaglook.tag 
                  NO-LOCK NO-ERROR.
                  
    END.
    IF AVAIL fg-bin THEN
      ASSIGN
       ttreturntaglook.vStdCost = fg-bin.std-tot-cost
       ttreturntaglook.vUom = fg-bin.pur-uom.

  FIND FIRST job-hdr WHERE
       job-hdr.company = cocode
                  AND job-hdr.job-no = loadtag.job-no
                  AND job-hdr.job-no2 = loadtag.job-no2
                  AND job-hdr.i-no = loadtag.i-no NO-LOCK NO-ERROR.
   /*IF AVAIL job-hdr THEN 
      ASSIGN fg-rctd.std-cost:SCREEN-VALUE = string(job-hdr.std-mat-cost +
                                                job-hdr.std-lab-cost +
                                                job-hdr.std-fix-cost +
                                                job-hdr.std-var-cost) . */

   IF NOT AVAIL job-hdr THEN DO:
      FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ ttreturntaglook.vJobNo
            AND job.job-no2 EQ int(ttreturntaglook.vJob2)
        NO-LOCK NO-ERROR.
      IF AVAIL job THEN
      FIND FIRST reftable
          WHERE reftable.reftable EQ "jc/jc-calc.p"
            AND reftable.company  EQ job.company
            AND reftable.loc      EQ ""
            AND reftable.code     EQ STRING(job.job,"999999999")
            AND reftable.code2    EQ ttreturntaglook.vINo
          NO-LOCK NO-ERROR.
    END.
   if avail job-hdr and job-hdr.std-tot-cost gt 0 then
      ttreturntaglook.vStdCost = job-hdr.std-tot-cost.
    ELSE 
    IF AVAIL reftable AND reftable.val[5] GT 0 THEN
      ttreturntaglook.vStdCost = reftable.val[5].
    else do:
     /* find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name})
            and po-ordl.i-no      eq fg-rctd.i-no:SCREEN-VALUE IN BROWSE {&browse-name}
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        run sys/ref/convcuom.p(po-ordl.pr-uom, fg-rctd.cost-uom:SCREEN-VALUE IN BROWSE {&browse-name}, 0,
                               po-ordl.s-len, po-ordl.s-wid, 0,
                               po-ordl.cost, output v-cost).

        fg-rctd.std-cost:SCREEN-VALUE IN BROWSE {&browse-name} = STRING(v-cost).
      END.
     
      else */
      if avail itemfg then
        assign
         ttreturntaglook.vUom = itemfg.prod-uom
         ttreturntaglook.vStdCost = itemfg.total-std-cost.
    end.
   
   IF NOT AVAIL job-hdr OR dec(ttreturntaglook.vStdCost ) = 0 THEN DO:
      IF ttreturntaglook.vJobNo <> ""  THEN
         FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                           AND oe-ordl.i-no = ttreturntaglook.vINo
                           AND oe-ordl.job-no = ttreturntaglook.vJobNo
                           AND oe-ordl.job-no2 = ttreturntaglook.vJob2 NO-LOCK NO-ERROR.
      ELSE IF AVAIL loadtag THEN
         FIND FIRST oe-ordl WHERE oe-ordl.company = cocode
                           AND oe-ordl.i-no = ttreturntaglook.vINo
                           AND oe-ordl.ord-no = loadtag.ord-no NO-LOCK NO-ERROR.

      IF AVAIL oe-ordl THEN
           ASSIGN ttreturntaglook.vStdCost = oe-ordl.cost
                  ttreturntaglook.vUom = "M".
   END.

END PROCEDURE.








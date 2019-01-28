
DEF INPUT PARAMETER cocode AS CHAR NO-UNDO.
DEF INPUT PARAMETER ipr-po-ordl-row AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipr-fg-rctd-row AS ROWID NO-UNDO.
DEF INPUT PARAMETER ipiQty-case LIKE fg-rctd.qty-case NO-UNDO.
DEF INPUT PARAMETER ipiCases LIKE fg-rctd.cases NO-UNDO.
DEF INPUT PARAMETER ipiPartial LIKE fg-rctd.partial NO-UNDO.
DEF INPUT PARAMETER ipcJob-no LIKE fg-rctd.job-no NO-UNDO.
DEFINE INPUT  PARAMETER ipiJob-no2 LIKE fg-rctd.job-no2  NO-UNDO.
DEF INPUT PARAMETER ipcCostUom LIKE fg-rctd.cost-uom NO-UNDO.
DEF INPUT PARAMETER ipdT-qty LIKE fg-rctd.t-qty  NO-UNDO.


DEF OUTPUT PARAMETER lv-use-full-qty AS LOG NO-UNDO.
DEF OUTPUT PARAMETER lv-full-qty AS DEC NO-UNDO.
DEF OUTPUT PARAMETER opd-cost-uom AS CHAR NO-UNDO.
DEF OUTPUT PARAMETER opd-std-cost LIKE fg-rctd.std-cost NO-UNDO.
DEF OUTPUT PARAMETER opd-ext-cost LIKE fg-rctd.ext-cost NO-UNDO.
DEF OUTPUT PARAMETER opd-frt-cost LIKE fg-rctd.frt-cost NO-UNDO.
DEF OUTPUT PARAMETER opd-setup-per-cost-uom AS DEC NO-UNDO.
/* DEF INPUT PARAMETER ipr-itemfg-row AS ROWID NO-UNDO. */
DEF VAR fg-uom-list AS CHAR NO-UNDO.
DEF VAR lv-ord-uom AS CHAR NO-UNDO.
DEF VAR v-bwt like po-ordl.s-len no-undo.
DEF VAR v-len like po-ordl.s-len no-undo.
DEF VAR v-wid like po-ordl.s-len no-undo.
DEF VAR v-dep like po-ordl.s-len no-undo.
{sys/inc/fgpofrt.i}  

DEF BUFFER b-loadtag FOR loadtag.
DEF BUFFER b-po-ordl FOR po-ordl.

DEF VAR li AS INT NO-UNDO.
DEF VAR lv-got-job AS LOG NO-UNDO.
DEF VAR lv-out-cost AS DEC DECIMALS 4 NO-UNDO.
DEF VAR lv-out-qty as dec no-undo.
DEF VAR lv-from-uom AS CHAR NO-UNDO.
DEF VAR lv-cost-uom AS CHAR NO-UNDO.
DEF VAR lv-ord-qty AS INT NO-UNDO.
DEF VAR lv-frt-cost LIKE fg-rctd.frt-cost NO-UNDO.
DEF VAR lv-setup-included AS LOG NO-UNDO.
DEF VAR lv-setup-per-cost-uom AS DEC NO-UNDO.
DEF VAR g_company AS CHAR NO-UNDO.
DEF VAR lv-adjusted-qty AS DEC NO-UNDO.
DEF VAR lv-save-cost-uom LIKE po-ordl.pr-uom NO-UNDO.

FIND b-po-ordl WHERE ROWID(b-po-ordl) EQ ipr-po-ordl-row NO-LOCK  NO-ERROR.
FIND fg-rctd WHERE ROWID(fg-rctd) EQ ipr-fg-rctd-row NO-LOCK NO-ERROR.

FIND itemfg WHERE itemfg.company EQ cocode
    AND itemfg.i-no EQ b-po-ordl.i-no NO-LOCK NO-ERROR.
IF NOT AVAIL b-po-ordl OR NOT AVAIL fg-rctd OR NOT AVAIL itemfg THEN
    RETURN.

DEF VAR dRFIDTag AS DEC NO-UNDO.

RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
ASSIGN
 v-bwt       = 0
 v-len       = itemfg.t-len
 v-wid       = itemfg.t-wid
 v-dep       = 0
 v-len       = b-po-ordl.s-len
 v-wid       = b-po-ordl.s-wid.


 g_company = cocode.
 lv-setup-included = NO.
 lv-ord-qty = b-po-ordl.ord-qty.
 lv-ord-uom = b-po-ordl.pr-qty-uom.

 RUN rm/convquom.p(lv-ord-uom, 'EA',                   
     v-bwt, v-len, v-wid, v-dep,
     lv-ord-qty, OUTPUT lv-ord-qty).
 
 /* Update of database: cost-uom */
 ASSIGN
    lv-save-cost-uom = b-po-ordl.pr-uom
    lv-out-cost = b-po-ordl.cost * (IF b-po-ordl.disc NE 0 THEN (1 - (b-po-ordl.disc / 100)) ELSE 1).
    lv-out-qty = (ipiQty-case * ipiCases) + ipiPartial.
 
 /* Get the adjusted quantity, taking into account other tags */
 RUN get-set-full-qty (INPUT ipcJob-no, INPUT ipiJob-no2, 
                       INPUT b-po-ordl.i-no, INPUT lv-out-qty /* new qty */, 
                       INPUT 0 /* cost to set */, 
                       INPUT b-po-ordl.pr-uom, OUTPUT lv-full-qty).
 
 IF lv-out-qty LE lv-ord-qty AND lv-out-qty GT 0 THEN 
     lv-adjusted-qty = lv-ord-qty.
 ELSE
     lv-adjusted-qty = lv-out-qty.

 IF lv-full-qty GT lv-adjusted-qty THEN
     ASSIGN lv-adjusted-qty = lv-full-qty
            lv-use-full-qty = TRUE.
 
 /* if the quantity is less than po quantity, assume setup cost is 
    included in the po cost */
 
 IF lv-adjusted-qty < lv-ord-qty AND lv-adjusted-qty GT 0 THEN
    ASSIGN lv-out-cost = b-po-ordl.cons-cost
           lv-from-uom = b-po-ordl.cons-uom
           lv-setup-included = YES.
 ELSE 
   lv-from-uom = lv-save-cost-uom.

 RUN convert-vend-comp-curr(INPUT b-po-ordl.po-no, INPUT-OUTPUT lv-out-cost).

 /* Update of database: std-cost */
 ASSIGN
    /* wfk - 09261318 - take out update to make this a read-only procedure */
    /* fg-rctd.std-cost = lv-out-cost */
    lv-cost-uom = itemfg.prod-uom
    v-len = b-po-ordl.s-len
    v-wid = b-po-ordl.s-wid.

 IF lv-from-uom EQ "L" THEN
    ASSIGN
       lv-from-uom = "EA"
       lv-out-cost = ABSOLUTE(lv-out-cost / lv-out-qty).

 /* convert cost pr-uom*/
 IF lv-from-uom EQ lv-cost-uom OR
    (LOOKUP(lv-from-uom,fg-uom-list) GT 0 AND
     LOOKUP(lv-cost-uom,fg-uom-list) GT 0) THEN.
 ELSE
    RUN rm/convcuom.p(lv-from-uom, lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      lv-out-cost, OUTPUT lv-out-cost).
 
 /* Using adjusted qty, which includes other tags for cost calculation purposes */
 IF LOOKUP(lv-cost-uom,fg-uom-list) EQ 0 THEN
    RUN rm/convquom.p("EA", lv-cost-uom,                   
                      v-bwt, v-len, v-wid, v-dep,
                      lv-adjusted-qty, OUTPUT lv-out-qty).
 
 
 IF lv-setup-included THEN
     lv-setup-per-cost-uom = 0.
 ELSE 
    lv-setup-per-cost-uom = b-po-ordl.setup / lv-out-qty .
    IF lv-out-cost EQ ? THEN lv-out-cost = 0.
 /* wfk - 09261318 - take out update to make this a read-only procedure */    
 ASSIGN
    opd-cost-uom = lv-cost-uom
    opd-std-cost = lv-out-cost + lv-setup-per-cost-uom
    opd-ext-cost = (lv-out-qty * opd-std-cost) 
    .
 
 IF fgpofrt-log THEN 
 DO:
    /* Update of database: frt-cost */
    RUN get-freight-cost (OUTPUT lv-frt-cost).

    opd-ext-cost = opd-ext-cost + lv-frt-cost.
                       

 END.
 opd-setup-per-cost-uom = lv-setup-per-cost-uom.
 IF lv-use-full-qty THEN DO:  
  RUN get-set-full-qty (INPUT ipcJob-no, INPUT ipiJob-no2, 
                       INPUT b-po-ordl.i-no, INPUT lv-out-qty /* new qty */, 
                       INPUT opd-std-cost /* cost to set */, 
                       INPUT opd-cost-uom, OUTPUT lv-full-qty).
 END.


 PROCEDURE get-set-full-qty :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  DEF INPUT PARAMETER ipc-job-no     AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ipi-job-no2    AS INT  NO-UNDO.
  DEF INPUT PARAMETER ipc-i-no       AS CHAR NO-UNDO.
  DEF INPUT PARAMETER ipd-current-qty AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-cost-to-set AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-cost-to-set-uom AS CHAR NO-UNDO.
  def OUTPUT parameter op-out-qty as DEC no-undo.


  def var v-len like po-ordl.s-len no-undo.
  def var v-wid like po-ordl.s-len no-undo.
  def var v-dep like po-ordl.s-len no-undo. 
  def var v-bwt like po-ordl.s-len no-undo.
  def var lv-out-qty as dec no-undo.
  def var lv-out-cost as dec no-undo.
  DEF VAR lv-calc-cost AS DEC.
  DEF VAR lv-recalc-cost AS DEC.
  DEF VAR lv-ext-cost AS DEC.
  DEF VAR v-rec-qty AS INT NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER b-fg-rctd FOR fg-rctd.
  DEF BUFFER b1-fg-rctd FOR fg-rctd.


  cocode = g_company.
  
  lv-out-qty = 0.
  FOR EACH b-fg-rctd WHERE b-fg-rctd.company eq g_company and
           (b-fg-rctd.rita-code eq "R" or b-fg-rctd.rita-code eq "E")
           AND trim(b-fg-rctd.job-no) = trim(ipc-job-no)
           AND b-fg-rctd.job-no2 = INT(ipi-job-no2)
           AND b-fg-rctd.i-no = ipc-i-no 
           NO-LOCK :
      /* Already consider the qty of the current record via the ipd-current-qty */
      IF AVAIL(fg-rctd) AND ROWID(fg-rctd) EQ ROWID(b-fg-rctd) THEN
          NEXT.
      lv-out-qty = lv-out-qty + b-fg-rctd.t-qty.     
      IF ip-cost-to-set GT 0 THEN DO:

          /* convert cost to b1-fg-rctd uom */

          FIND b1-fg-rctd WHERE ROWID(b1-fg-rctd) EQ ROWID(b-fg-rctd)
              EXCLUSIVE-LOCK NO-ERROR.
          IF AVAIL b1-fg-rctd THEN DO:
        
            find itemfg where itemfg.company eq cocode
                          and itemfg.i-no  eq b-fg-rctd.i-no
                        use-index i-no no-lock no-error.
            
            ASSIGN
              v-bwt             = 0
              v-dep             = 0.
            
            IF AVAIL itemfg THEN
              ASSIGN v-len       = itemfg.t-len
                     v-wid       = itemfg.t-wid.
            
            /* Always find just to get quantity */
            find first po-ordl where po-ordl.company = cocode
                                 and po-ordl.po-no   = integer(b-fg-rctd.po-no)
                                 and po-ordl.i-no    = b-fg-rctd.i-no
                                 and po-ordl.job-no  = b-fg-rctd.job-no
                                 and po-ordl.job-no2 = b-fg-rctd.job-no2
                                 and po-ordl.item-type = no
                                 no-lock no-error.
            IF NOT AVAIL po-ordl THEN
                find first po-ordl where po-ordl.company = cocode
                                     and po-ordl.po-no   = integer(b-fg-rctd.po-no)
                                     and po-ordl.i-no    = b-fg-rctd.i-no
                                     and po-ordl.item-type = no
                                     no-lock no-error.
            
            
            IF AVAIL po-ordl THEN
              ASSIGN
                v-len = po-ordl.s-len
                v-wid = po-ordl.s-wid.
            lv-calc-cost = ip-cost-to-set.
            lv-recalc-cost = lv-calc-cost.
            IF b-fg-rctd.cost-uom EQ ip-cost-to-set-uom               OR
              (LOOKUP(ipcCostUom,fg-uom-list) GT 0 AND
               LOOKUP(b-fg-rctd.cost-uom,fg-uom-list) GT 0)   THEN.
            ELSE
               RUN rm/convcuom.p(b-fg-rctd.cost-uom, ip-cost-to-set-uom, 
                                 v-bwt, v-len, v-wid, v-dep,
                                 lv-calc-cost, OUTPUT lv-recalc-cost).
            
            b1-fg-rctd.std-cost = lv-recalc-cost.
            ASSIGN
             lv-ext-cost = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost                          
             b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
          END.

      END.
  END.
   
  lv-out-qty = lv-out-qty + ipd-current-qty.

  op-out-qty = lv-out-qty.
END PROCEDURE.


PROCEDURE get-freight-cost :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEF OUTPUT PARAM op-cost LIKE fg-rctd.frt-cost NO-UNDO.

   DEF BUFFER b-po-ordl-2 FOR po-ordl.

   FIND FIRST b-po-ordl-2 WHERE
        b-po-ordl-2.company   EQ b-po-ordl.company AND
        b-po-ordl-2.po-no     EQ b-po-ordl.po-no AND
        b-po-ordl-2.i-no      EQ b-po-ordl.i-no AND
        b-po-ordl-2.job-no    EQ b-po-ordl.job-no AND
        b-po-ordl-2.job-no2   EQ b-po-ordl.job-no2 AND
        b-po-ordl-2.item-type EQ NO
        NO-LOCK NO-ERROR.
  
   IF AVAIL b-po-ordl-2 THEN DO:
       RUN po/getfrtcs.p (ROWID(b-po-ordl-2),
                         ipdT-qty,
                         OUTPUT op-cost).
  
       RUN convert-vend-comp-curr(INPUT b-po-ordl-2.po-no, INPUT-OUTPUT op-cost).
   END.

END PROCEDURE.

PROCEDURE convert-vend-comp-curr :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
   DEFINE INPUT PARAMETER ip-po-no AS INT NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.
   
   DEF BUFFER b-po-ord FOR po-ord.
   DEF BUFFER b-company FOR company.

   FIND FIRST b-po-ord WHERE
        b-po-ord.company EQ cocode AND
        b-po-ord.po-no EQ ip-po-no
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

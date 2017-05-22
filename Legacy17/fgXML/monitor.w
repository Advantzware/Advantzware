/* monitor.w */

{custom/monitor.w "fgXML" "fgXML"}
 /* fgxml/fgxmlDefs.i - temp-table FGReceiptRow defined */
 
 /* 
 {sys/inc/rfidtag.i}
 {sys/inc/fgsetrec.i}
 {sys/inc/fgpofrt.i}
 */
 def var RFIDTag-log like sys-ctrl.log-fld no-undo.
 def var RFIDTag-cha like sys-ctrl.char-fld no-undo.
 def var fgsetrec like sys-ctrl.char-fld no-undo.
 def var fgsetrec-log like sys-ctrl.log-fld no-undo.
 def var fgsetrec-int like sys-ctrl.int-fld no-undo.
 def var fgpofrt-log like sys-ctrl.log-fld no-undo.

 DEF VAR v-auto-add-tag AS LOG NO-UNDO.
 

/* ************************  Function Prototypes ********************** */
FUNCTION maxComponentQty RETURNS DECIMAL 
	(  ) FORWARD.

/* **********************  Internal Procedures  *********************** */


PROCEDURE convert-vend-comp-curr:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
   DEFINE INPUT-OUTPUT PARAMETER ip-cost AS DEC DECIMALS 4 NO-UNDO.

   DEF BUFFER b-po-ord FOR po-ord.
   DEF BUFFER b-company FOR company.
   
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

PROCEDURE create-loadtag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
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
      WHERE sys-ctrl.company EQ cocode
        AND sys-ctrl.name    EQ "LOADTAG"
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
      loadtag.pallet-count = IF b-fg-rctd.units-pallet GT 0 THEN 
                                b-fg-rctd.qty MOD b-fg-rctd.units-pallet
                             ELSE
                                0
      loadtag.partial      = IF b-fg-rctd.qty-case GT 0 THEN 
                                b-fg-rctd.qty MOD b-fg-rctd.qty-case
                             ELSE
                                0
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
      oe-ctrl.spare-char-1 = STRING(dRFIDTag + 1).
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

PROCEDURE get-freight-cost:
/*------------------------------------------------------------------------------
 Purpose:
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
          AND po-ordl.po-no     EQ INT(fg-rctd.po-no)
          AND po-ordl.i-no      EQ fg-rctd.i-no
          AND po-ordl.job-no    EQ fg-rctd.job-no
          AND po-ordl.job-no2   EQ fg-rctd.job-no2
          AND po-ordl.item-type EQ NO
        NO-LOCK NO-ERROR.

    IF AVAIL po-ordl THEN
      RUN po/getfrtcs.p (ROWID(po-ordl),
                         fg-rctd.t-qty,
                         OUTPUT op-cost).

    RUN convert-vend-comp-curr(INPUT-OUTPUT op-cost).
  

END PROCEDURE.

PROCEDURE get-job-no:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER iplAskPasswd AS LOG NO-UNDO.
  DEF VAR lvPasswordEntered AS LOG NO-UNDO.
  DEF VAR lcRitaCode AS CHAR NO-UNDO.
  IF AVAIL(fg-rctd) THEN
      lcRitaCode = fg-rctd.rita-code.
  ELSE
      lcRitaCode = "R".
  
    fg-rctd.job-no =
        FILL(" ",6 - LENGTH(TRIM(fg-rctd.job-no))) +
        TRIM(fg-rctd.job-no).

/*    IF TRIM(fg-rctd.job-no) NE TRIM(lv-job-no)  OR */
/*       DEC(fg-rctd.job-no2) NE DEC(lv-job-no2) THEN*/
/*      RUN new-job-no.                              */

/*    IF fg-rctd.job-no EQ "" THEN DO:                                             */
/*      IF fgrecpt                                                AND              */
/*         fg-rctd.po-no:SCREEN-VALUE IN BROWSE {&browse-name} EQ "" AND           */
/*         lcRitaCode NE "E"                                  AND                  */
/*         iplAskPasswd THEN DO:                                                   */
/*                                                                                 */
/*        /* Check password for override */                                        */
/*        RUN sys/ref/d-psswrd.w (INPUT "FGRecptPassWord", INPUT "FGRecptPassWord",*/
/*                                OUTPUT lvPasswordEntered).                       */
/*      END. /* If nk1 is set to validate blank job/po */                          */
/*    END. /* If job# blank */                                                     */

    /*ELSE*/
     if fg-rctd.job-no <> "" then DO:
      IF int(fg-rctd.po-no) NE 0 THEN DO:
       
      END.

      FIND FIRST job-hdr
          WHERE job-hdr.company EQ cocode
            AND job-hdr.job-no  EQ fg-rctd.job-no
          NO-LOCK NO-ERROR.
      IF NOT AVAIL job-hdr THEN DO:
      END.
    END.
  

END PROCEDURE.

PROCEDURE get-linker:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF OUTPUT PARAM op-linker AS CHAR NO-UNDO.


  op-linker = IF AVAIL fg-rctd                 AND
                 CAN-FIND(FIRST itemfg
                          WHERE itemfg.company EQ fg-rctd.company
                            AND itemfg.i-no    EQ fg-rctd.i-no
                            AND itemfg.isaset) THEN
                "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")
              ELSE "".


END PROCEDURE.

PROCEDURE get-next-tag:
/*------------------------------------------------------------------------------
 Purpose:
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

PROCEDURE get-set-full-qty:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF INPUT PARAMETER ip-cost-to-set AS DEC NO-UNDO.
  DEF INPUT PARAMETER ip-on-screen AS LOG NO-UNDO.
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
  def var fg-uom-list  as char NO-UNDO.
  
  /*  cocode = g_company.*/
  RUN sys/ref/uom-fg.p (?, OUTPUT fg-uom-list).
  lv-out-qty = 0.
  FOR EACH b-fg-rctd WHERE b-fg-rctd.company eq g_company and
           (b-fg-rctd.rita-code eq "R" or b-fg-rctd.rita-code eq "E")
           AND trim(b-fg-rctd.job-no) = trim(fg-rctd.job-no)
           AND b-fg-rctd.job-no2 = (fg-rctd.job-no2)
           AND b-fg-rctd.i-no = fg-rctd.i-no 
           AND recid(b-fg-rctd) <> recid(fg-rctd)
           NO-LOCK,     
    FIRST ASI.reftable WHERE reftable.reftable EQ "fg-rctd.user-id" AND
      reftable.company  EQ fg-rctd.company AND
      reftable.loc      EQ STRING(fg-rctd.r-no,"9999999999")  AND
      reftable.dscr    EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999")   
    use-index loc:

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
                                 and po-ordl.po-no   = int(b-fg-rctd.po-no)
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
            IF fg-rctd.cost-uom EQ b-fg-rctd.cost-uom               OR
              (LOOKUP(fg-rctd.cost-uom,fg-uom-list) GT 0 AND
               LOOKUP(b-fg-rctd.cost-uom,fg-uom-list) GT 0)   THEN.
            ELSE
               RUN rm/convcuom.p(fg-rctd.cost-uom, b-fg-rctd.cost-uom, 
                                 v-bwt, v-len, v-wid, v-dep,
                                 lv-calc-cost, OUTPUT lv-recalc-cost).
            
            b1-fg-rctd.std-cost = lv-recalc-cost.
            ASSIGN
             lv-ext-cost = b1-fg-rctd.t-qty * b1-fg-rctd.std-cost                          
             b1-fg-rctd.ext-cost = lv-ext-cost + b1-fg-rctd.frt-cost.
          END.

      END.
  END.
  IF ip-on-screen THEN lv-out-qty = lv-out-qty + fg-rctd.t-qty.
  op-out-qty = lv-out-qty.

END PROCEDURE.

PROCEDURE get-values:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF VAR lv-loc AS CHAR NO-UNDO.
  DEF VAR lv-loc-bin AS CHAR NO-UNDO.
  DEF VAR lv-qty-case AS int NO-UNDO.
  DEF VAR lv-cost-uom AS char NO-UNDO.
  DEF VAR lv-std-cost AS dec NO-UNDO.
  DEF VAR v-cost AS DEC DECIMALS 10 NO-UNDO.
  DEF VAR lv-save AS char EXTENT 20 NO-UNDO.
  DEF VAR ll-ea AS LOG NO-UNDO.

  DEF BUFFER reftable-job FOR reftable.
  
  find first itemfg where itemfg.company = cocode 
               and itemfg.i-no EQ fg-rctd.i-no        no-lock no-error.
  IF AVAIL itemfg THEN DO:
     IF fg-rctd.i-name = "" THEN
            ASSIGN fg-rctd.i-name = itemfg.i-name.
        
     assign
         lv-qty-case = itemfg.case-count
         lv-cost-uom = if itemfg.pur-man then itemfg.pur-uom else itemfg.prod-uom.
    
     RUN fg/autopost.p (ROWID(itemfg),
                           fg-rctd.job-no,
                           fg-rctd.job-no2,
                           OUTPUT lv-loc, OUTPUT lv-loc-bin).
        
     find first fg-bin
            where fg-bin.company eq cocode
              and fg-bin.loc     eq lv-loc
              and fg-bin.loc-bin eq lv-loc-bin
              and fg-bin.i-no    eq ""
            no-lock no-error.
     if avail fg-bin then 
          assign
           lv-std-cost = IF fg-rctd.po-no = "" and fg-rctd.job-no = "" THEN itemfg.last-cost ELSE lv-std-cost
           lv-qty-case = itemfg.case-count
           lv-cost-uom = itemfg.prod-uom.
    
     ASSIGN
         lv-save[1] = string(fg-rctd.std-cost)
         lv-save[2] = fg-rctd.cost-uom.
    
         /*from RUN get-fg-bin-cost.*/
     FIND FIRST fg-bin WHERE fg-bin.company EQ cocode
          AND fg-bin.i-no    EQ fg-rctd.i-no
          AND fg-bin.job-no  EQ fg-rctd.job-no
          AND fg-bin.job-no2 EQ fg-rctd.job-no2
          AND fg-bin.loc     EQ fg-rctd.loc
          AND fg-bin.loc-bin EQ fg-rctd.loc-bin
          AND fg-bin.tag     EQ fg-rctd.tag        NO-LOCK NO-ERROR.
     IF AVAIL fg-bin THEN
          ASSIGN fg-rctd.std-cost = fg-bin.std-tot-cost
                 fg-rctd.cost-uom = fg-bin.pur-uom. 
  
     ASSIGN lv-std-cost = fg-rctd.std-cost
            lv-cost-uom = fg-rctd.cost-uom
            fg-rctd.std-cost = dec(lv-save[1])
            fg-rctd.cost-uom = lv-save[2].

    /**  Find the Job Header record in then job file and use Standard Cost
         from that job. **/
     find first job-hdr
        where job-hdr.company eq cocode
          and job-hdr.i-no    eq fg-rctd.i-no
          and job-hdr.job-no  eq fg-rctd.job-no
          and job-hdr.job-no2 eq fg-rctd.job-no2
        NO-LOCK no-error.

     IF NOT AVAIL job-hdr THEN DO:
        FIND FIRST job
          WHERE job.company EQ cocode
            AND job.job-no  EQ fg-rctd.job-no
            AND job.job-no2 EQ fg-rctd.job-no2
            NO-LOCK NO-ERROR.
        IF AVAIL job THEN
           FIND FIRST reftable-job
             WHERE reftable-job.reftable EQ "jc/jc-calc.p"
               AND reftable-job.company  EQ job.company
               AND reftable-job.loc      EQ ""
               AND reftable-job.code     EQ STRING(job.job,"999999999")
               AND reftable-job.code2    EQ fg-rctd.i-no
                 NO-LOCK NO-ERROR.
     END.

     if avail job-hdr and job-hdr.std-tot-cost gt 0 THEN
        ASSIGN lv-cost-uom = "M"
              lv-std-cost = job-hdr.std-tot-cost.
     ELSE
     IF AVAIL reftable-job AND reftable-job.val[5] GT 0 THEN
        ASSIGN lv-cost-uom = "M"
               lv-std-cost = (reftable-job.val[5]).

    /** If no Job Header is avail for this Job# then Find the Item
        record for then item and use Standard Cost from that item. **/
     else do:
      find first po-ordl
          where po-ordl.company   eq cocode           
            and po-ordl.po-no     eq int(fg-rctd.po-no)
            and po-ordl.i-no      eq fg-rctd.i-no
            and po-ordl.item-type eq no
          no-lock no-error.
          
      if avail po-ordl THEN DO:
        ASSIGN
         lv-cost-uom = po-ordl.pr-uom.
         lv-std-cost = (po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1)).

        RUN convert-vend-comp-curr(INPUT-OUTPUT lv-std-cost).

        RUN show-freight.
        
      END.
     
      else
      if avail itemfg          AND
         DEC(lv-std-cost) EQ 0 THEN DO:
        assign
         lv-cost-uom = itemfg.prod-uom
         lv-std-cost = (itemfg.total-std-cost).

        IF /*itemfg.total-std-cost EQ 0 AND*/ itemfg.isaset THEN DO:
            RUN fg/costset.p (ROWID(itemfg), OUTPUT v-cost).

          IF lv-cost-uom NE "M" THEN DO:
            RUN sys/ref/ea-um-fg.p (lv-cost-uom, OUTPUT ll-ea).
            IF ll-ea THEN lv-cost-uom = "EA".
            RUN sys/ref/convcuom.p("M", lv-cost-uom,
                                   0, 0, 0, 0, v-cost, OUTPUT v-cost).
            IF ll-ea THEN lv-cost-uom = itemfg.prod-uom.
          END.
          lv-std-cost = (v-cost).          
          
        END.  /* itemfg.isaset */
        
      END. /* avail itemfg */
    END.  /* else do */

    /* #pn# If there is a tag, quantites should default from there */
    /* #pn# task 10311308                                          */
    IF fg-rctd.tag GT "" THEN  RUN new-tag.
    IF fg-rctd.loc EQ "" OR
       fg-rctd.loc-bin EQ "" THEN
      ASSIGN
       fg-rctd.loc = lv-loc
       fg-rctd.loc-bin = lv-loc-bin.

    IF fg-rctd.qty-case EQ 0 THEN fg-rctd.qty-case = lv-qty-case.
    IF fg-rctd.cost-uom EQ "" THEN fg-rctd.cost-uom = lv-cost-uom.
    IF fg-rctd.std-cost EQ 0 THEN fg-rctd.std-cost = lv-std-cost.
    IF fg-rctd.cases-unit EQ 0 THEN fg-rctd.cases-unit = 1.
    IF fg-rctd.partial EQ ? THEN fg-rctd.partial = 0.

    assign FGReceiptRow.std-cost = fg-rctd.std-cost
           FGReceiptRow.cost-uom = fg-rctd.cost-uom
           .
  END.

END PROCEDURE.

PROCEDURE new-tag:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
 DEF VAR hProc       AS HANDLE NO-UNDO.
 DEF VAR dMaxQty     AS DECIMAL NO-UNDO.
 DEF VAR cHeaderItem AS CHAR NO-UNDO.
 DEF VAR dMaxCompQty AS DEC NO-UNDO. /* Max component quantity */
 DEF VAR dTotalQty   AS DEC NO-UNDO.
 DEF BUFFER bfItemfg FOR itemfg.

 IF avail itemfg and itemfg.isaset /*NOT ll-set-parts*/ THEN DO:      
        FIND FIRST loadtag NO-LOCK
            WHERE loadtag.company   EQ cocode
              AND loadtag.item-type EQ NO
              AND loadtag.tag-no    EQ fg-rctd.tag
            NO-ERROR.
        
        IF AVAIL loadtag THEN DO:
            fg-rctd.i-no = loadtag.i-no.
            fg-rctd.stack-code = loadtag.misc-char[2] .  /* task 12051302 */
            IF fg-rctd.cases = 0 THEN fg-rctd.cases = loadtag.case-bundle.
            IF fg-rctd.qty-case = 0 THEN fg-rctd.qty-case = loadtag.qty-case.
            /* Task 12061305 */
            IF loadtag.job-no <> "" THEN DO:
                ASSIGN
                    fg-rctd.job-no = loadtag.job-no
                    fg-rctd.job-no2 = /* FILL(" ",6 - LENGTH(TRIM(string(loadtag.job-no2)))) +
                                                                           TRIM(string(loadtag.job-no2))*/
                                      loadtag.job-no2.
/*                IF NOT fgRecptPassWord-log THEN                 */
/*                    RUN get-job-no (INPUT YES) NO-ERROR.        */
/*                ELSE                                            */
/*                    /* run with 'no' so no message until save */*/
/*                    RUN get-job-no (INPUT NO) NO-ERROR.         */
/*                IF ERROR-STATUS:ERROR THEN RETURN NO-APPLY.     */
            END.  /* Task 12061305 */
            if fg-rctd.po-no = "" then fg-rctd.po-no = string(loadtag.po-no). 
        END.
    END.
    ELSE DO:
        FIND FIRST fg-bin NO-LOCK
            WHERE fg-bin.company EQ cocode
              AND fg-bin.tag EQ fg-rctd.tag
              AND fg-rctd.tag NE ""
              NO-ERROR.
       
        /* Obtain quantity of set header record */                        
       dMaxCompQty = maxComponentQty().
        IF AVAIL fg-bin  THEN DO:

          /* dTotalQty is the qty in other lines with the same item number */
          RUN get-set-full-qty (INPUT fg-bin.std-tot-cost, INPUT yes, OUTPUT dTotalQty).
          
          dTotalQty = dTotalQty - fg-rctd.t-qty.
          
          /* dMaxCompQty is the max that can be used from fg-bin */
          IF ABS(dMaxCompQty) GT 0 AND ABS(dTotalQty) GT 0 THEN
            dMaxCompQty = dMaxCompQty - ABS(dTotalQty).
          IF dMaxCompQty LT 0 THEN
            dMaxCompQty = 0.
          IF /*ABS(DECIMAL(fg-rctd.t-qty:SCREEN-VALUE IN BROWSE {&browse-name})) GE fg-bin.qty */ 
              ABS(dTotalQty) + ABS(fg-bin.qty) LE ABS(dMaxCompQty) THEN
            ASSIGN
              fg-rctd.i-no = fg-bin.i-no
              fg-rctd.cases = (-1 * TRUNC((fg-bin.qty - fg-bin.partial-count) / fg-bin.case-count,0))
              fg-rctd.qty-case = (fg-bin.case-count)
              fg-rctd.cases-unit = (fg-bin.cases-unit)
              fg-rctd.partial = (-1 * fg-bin.partial-count)
              fg-rctd.t-qty = (-1 * fg-bin.qty).
          ELSE
            ASSIGN            
              fg-rctd.i-no = fg-bin.i-no
              fg-rctd.cases = 0 /* STRING(-1 * TRUNC(dMaxCompQty / fg-bin.case-count,0)) */
              fg-rctd.qty-case = (dMaxCompQty)
              fg-rctd.cases-unit = (fg-bin.cases-unit)
              fg-rctd.partial = (-1 * dMaxCompQty)
              fg-rctd.t-qty = (-1 * dMaxCompQty).

            /* Task 12061305 */
            IF fg-bin.job-no <> "" THEN
                ASSIGN
                  fg-rctd.job-no = fg-bin.job-no
                  fg-rctd.job-no2 = fg-bin.job-no2 /*FILL(" ",6 - LENGTH(TRIM(string(fg-bin.job-no2)))) +
                                                                          TRIM(string(fg-bin.job-no2)) */
                  .
        END.
    END.


END PROCEDURE.

PROCEDURE postMonitor:
/*------------------------------------------------------------------------------
  Purpose:     import montiored file, create receipt record, post
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/
  DEFINE VARIABLE monitorFile AS CHARACTER NO-UNDO FORMAT 'X(50)'.
  DEFINE VARIABLE attrList    AS CHARACTER NO-UNDO FORMAT 'X(4)'.
  DEFINE VARIABLE errStatus   AS INTEGER   NO-UNDO.
  DEFINE VARIABLE saveMonitor AS CHARACTER NO-UNDO.
  DEFINE VARIABLE lReturn     AS LOGICAL   NO-UNDO.
  DEFINE VARIABLE cFile       AS CHARACTER NO-UNDO.
  DEFINE VARIABLE cErrorMsg   AS CHARACTER NO-UNDO.
  DEFINE VARIABLE iNextRNo    AS INTEGER   NO-UNDO.
  
  find first sys-ctrl where sys-ctrl.company eq cocode
                        and sys-ctrl.name    eq "RFIDTag" no-lock no-error. 
  if avail sys-ctrl then assign RFIDTag-log = sys-ctrl.log-fld
                                RFIDTag-cha = sys-ctrl.char-fld.
                                
  FIND FIRST sys-ctrl WHERE sys-ctrl.company EQ cocode
                        AND sys-ctrl.name    EQ "FGPOTAG#" NO-LOCK NO-ERROR.
  v-auto-add-tag = IF AVAIL sys-ctrl THEN sys-ctrl.log-fld ELSE NO.

  find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGSETREC"
    no-lock no-error.
  if avail sys-ctrl then assign fgsetrec     = sys-ctrl.char-fld
                               fgsetrec-log = sys-ctrl.log-fld
                               fgsetrec-int = sys-ctrl.int-fld.
  find first sys-ctrl
    where sys-ctrl.company eq cocode
      and sys-ctrl.name    eq "FGPOFRT"
    no-lock no-error.
  if avail sys-ctrl then fgpofrt-log = sys-ctrl.log-fld.
    
  INPUT FROM OS-DIR(monitorImportDir) NO-ECHO.
  REPEAT:
      SET monitorFile ^ attrList.
      IF attrList NE 'f' OR monitorFile BEGINS '.' OR
         INDEX(monitorFile,'.xml') EQ 0 THEN NEXT.
      IF SEARCH(monitorImportDir + '/processed/' + monitorFile) NE ? THEN DO:
          RUN monitorActivity ('ERROR File: ' + monitorFile + ' already processed',YES,'').
          cFile = REPLACE(monitorFile,'.xml','.err').
          OS-RENAME VALUE(monitorImportDir + '/' + monitorFile)
                    VALUE(monitorImportDir + '/' + cFile).
          NEXT.
      END. /* if search */
      RUN monitorActivity ('fgXML Data Import',YES,monitorFile).
      ASSIGN
           labelLine = FILL(' ',1000)
           SUBSTR(labelLine,1)  = 'Job No'
           SUBSTR(labelLine,11) = 'Item No'
           SUBSTR(labelLine,28) = 'Quantity'
           SUBSTR(labelLine,37) = 'PO No'
           SUBSTR(labelLine,46) = 'Error'
           .
      RUN monitorActivity (labelLine,NO,'').
      cFile = SEARCH(monitorImportDir + '/' + monitorFile).
      
      FIND LAST fg-rctd NO-LOCK USE-INDEX fg-rctd NO-ERROR.
      IF AVAILABLE fg-rctd AND fg-rctd.r-no GT iNextRNo THEN
      iNextRNo = fg-rctd.r-no.
      FIND LAST fg-rcpth NO-LOCK USE-INDEX r-no NO-ERROR.
      IF AVAILABLE fg-rcpth AND fg-rcpth.r-no GT iNextRNo THEN
      iNextRNo = fg-rcpth.r-no.

      DO WHILE TRUE:
          iNextRNo = iNextRNo + 1.
          FIND FIRST fg-rcpth NO-LOCK
               WHERE fg-rcpth.r-no EQ iNextRNo
               USE-INDEX r-no NO-ERROR.
        IF AVAILABLE fg-rcpth THEN NEXT.
        FIND FIRST fg-rctd NO-LOCK
             WHERE fg-rctd.r-no EQ iNextRNo
             USE-INDEX fg-rctd NO-ERROR.
        IF AVAILABLE fg-rctd THEN NEXT.
        LEAVE.
      END. /* while true */

      EMPTY TEMP-TABLE FGReceiptRow.  
      TEMP-TABLE FGReceiptRow:READ-XML ("File",cFile,"Empty",?,NO).
      
      FOR EACH FGReceiptRow:
          
          CREATE fg-rctd.
          BUFFER-COPY FGReceiptRow TO fg-rctd.
          ASSIGN
              fg-rctd.r-no = iNextRNo
              fg-rctd.rita-code = "R"
              fg-rctd.trans-time   = TIME
              FGReceiptRow.TableRowid = ROWID(fg-rctd)
              iNextRNo = iNextRNo + 1
              .
               
          RUN pValidateFGImport (OUTPUT cErrorMsg).
          ASSIGN
              dataLine = FILL(' ',1000)
              SUBSTR(dataLine,1)  = FGReceiptRow.job-no + "-" + STRING(FGReceiptRow.job-no2)
              SUBSTR(dataLine,11) = FGReceiptRow.i-no
              SUBSTR(dataLine,26) = STRING(FGReceiptRow.t-qty,">>,>>>,>>9")
              SUBSTR(dataLine,37) = FGReceiptRow.po-no
              SUBSTR(dataLine,46) = cErrorMsg
            .
            
          RUN monitorActivity (dataLine,NO,'').
          IF cErrorMsg NE "" THEN NEXT.
                    
          ASSIGN /*fg-rctd.t-qty    = DEC(ls-tmp-qty)
                 fg-rctd.pur-uom  = ls-tmp-uom
                 fg-rctd.cost-uom = ls-tmp-uom*/
                 fg-rctd.ext-cost = fg-rctd.std-cost * fg-rctd.t-qty /
                                    (IF fg-rctd.cost-uom EQ "M" THEN 1000 ELSE 1).
          IF fg-rctd.po-no GT "" THEN DO:
             FIND FIRST po-ord WHERE po-ord.company EQ fg-rctd.company
                                 AND po-ord.po-no EQ INTEGER(fg-rctd.po-no)
                                 NO-LOCK NO-ERROR.
             IF AVAIL po-ord THEN
                FIND FIRST po-ordl WHERE po-ordl.company EQ po-ord.company
                                     AND po-ordl.po-no EQ po-ord.po-no
                                     AND po-ordl.i-no  EQ fg-rctd.i-no 
                                     NO-LOCK NO-ERROR.
             
             IF AVAIL po-ordl THEN
                FIND FIRST itemfg WHERE itemfg.company EQ po-ordl.company
                                    AND itemfg.i-no EQ po-ordl.i-no 
                                    NO-LOCK NO-ERROR.     
             IF AVAIL itemfg AND itemfg.pur-man = TRUE AND AVAIL(po-ordl) AND po-ordl.job-no GT "" THEN
                ASSIGN fg-rctd.job-no = po-ordl.job-no
                       fg-rctd.job-no2 = po-ordl.job-no2.      
          END.
          FIND itemfg WHERE itemfg.company = cocode
                        AND itemfg.i-no = fg-rctd.i-no
                        NO-LOCK NO-ERROR.
          IF NOT itemfg.isaset /*ll-set-parts*/ THEN DO:
             FIND FIRST fg-rcpts WHERE fg-rcpts.r-no EQ fg-rctd.r-no NO-ERROR.
             IF NOT AVAIL fg-rcpts THEN DO:
               CREATE fg-rcpts.
               fg-rcpts.r-no       = fg-rctd.r-no.
             END.
             DEF VAR lv-linker AS CHAR NO-UNDO.
             RUN get-linker (OUTPUT lv-linker).
             ASSIGN fg-rcpts.company    = cocode
                    fg-rcpts.i-no       = fg-rctd.i-no
                    fg-rcpts.i-name     = fg-rctd.i-name
                    fg-rcpts.trans-date = fg-rctd.rct-date
                    fg-rcpts.linker     = lv-linker.
          END.
          ELSE DO:             
/*             IF lQtyChanged AND CAN-FIND(FIRST fg-rcpts                                                                              */
/*                          WHERE fg-rcpts.company EQ cocode                                                                           */
/*                            AND fg-rcpts.linker  EQ "fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))                                 */
/*                     AND NOT (FGSetRec-int EQ 1 AND itemfg.alloc NE YES) THEN DO:                                                    */
/*               MESSAGE "Set Parts Receipts will be reset since the set header quantity was changed. Please review the Set Parts tab."*/
/*               VIEW-AS ALERT-BOX INFO BUTTONS OK.                                                                                    */
/*               RUN DeleteSetParts (INPUT ("fg-rctd: " + STRING(fg-rctd.r-no,"9999999999"))).                                         */
/*             END.                                                                                                                    */
             IF NOT (FGSetRec-Int EQ 1 AND itemfg.alloc NE YES) THEN
                RUN fg/comprcpt.p (ROWID(fg-rctd)).
          END.
                        
          IF v-auto-add-tag AND FGReceiptRow.tag EQ "" THEN DO:
             disable triggers for load of fg-rctd.
              
             DEF VAR v-next-tag AS CHAR NO-UNDO.
             RUN get-next-tag (INPUT fg-rctd.i-no, OUTPUT v-next-tag). 
             RUN create-loadtag (INPUT-OUTPUT v-next-tag, INPUT ROWID(fg-rctd)).
             assign /*FGreceiptRow.tag = v-next-tag  this assignment make process run twice */
                    fg-rctd.tag = v-next-tag.                           
                                                                          
          END.
          
          /* If total quantity was used for cost, update the other records with the new cost */  
/*          IF lv-cost-basis = "FULLQTY" THEN DO:                                          */
/*             RUN get-set-full-qty (INPUT fg-rctd.std-cost, INPUT YES, OUTPUT v-full-qty).*/
/*          END.                                                                           */          
          
          IF /*NOT ll-set-parts*/ itemfg.isaset THEN RUN fg/invrecpt.p (ROWID(fg-rctd), 1).   
          
          RUN fg/fgpost.p (INPUT TABLE FGReceiptRow, input fg-rctd.tag).
              
      END. /* reach fgreceiptrow */
            
      /* be sure it hasn't been previously processed */
      OS-RENAME VALUE(monitorImportDir + '/' + monitorFile)
                VALUE(monitorImportDir + '/processed/' + monitorFile).
      errStatus = OS-ERROR.
      IF errStatus NE 0 THEN
      RUN monitorActivity ('ERROR: Moving ' + monitorFile,YES,'').
  END. /* os-dir repeat */
  INPUT CLOSE.
END PROCEDURE.

PROCEDURE pValidateFGImport:
    DEFINE OUTPUT PARAMETER opcErrorMsg AS CHARACTER NO-UNDO.
    
    DEFINE VARIABLE cUOMList AS CHARACTER NO-UNDO.
    
    /* validate rita-code */
    IF NOT CAN-DO("R,T",FGReceiptRow.rita-code) THEN
    opcErrorMsg = "Invalid RITA Code".
                
    /* validate tag */
    
    /* validate po-no */
                
    /* validate cost-uom */
    IF FGReceiptRow.std-cost <> 0 THEN DO:
      RUN sys/ref/uom-fg.p (NO, OUTPUT cUOMList).
      IF INDEX(cUOMList,FGReceiptRow.cost-uom) EQ 0 THEN
      opcErrorMsg = "Invalid Cost UOM".
    END.
    /* qty validation */
    IF FGReceiptRow.t-qty = 0 THEN DO:         
      ASSIGN FGReceiptRow.t-qty = FGReceiptRow.cases * FGReceiptRow.qty-case + FGReceiptRow.partial
             fg-rctd.t-qty = FGReceiptRow.t-qty 
             .             
      IF FGReceiptRow.t-qty = 0 THEN
         opcErrorMsg = "Receipt Qty is 0.".
    END.    
    /* cost calc if no cost imported */
    IF FGReceiptRow.std-cost = 0 THEN DO:
        
       run get-values.
       assign FGReceiptRow.std-cost = fg-rctd.std-cost
              FGReceiptRow.cost-uom = fg-rctd.cost-uom
              FGReceiptRow.tag = fg-rctd.tag
              .
    
       IF FGReceiptRow.job-no <> "" THEN DO:
          FIND job-hdr WHERE job-hdr.company = cocode
                         AND job-hdr.job-no = FGReceiptRow.job-no                
                         AND job-hdr.job-no2 = FGReceiptRow.job-no2 NO-LOCK NO-ERROR. 
          IF AVAIL job-hdr THEN FGReceiptRow.std-cost = (job-hdr.std-mat-cost +
                                   job-hdr.std-lab-cost +
                                   job-hdr.std-fix-cost +
                                   job-hdr.std-var-cost) . 
       END.
       ELSE IF FGReceiptRow.po-no <> "" THEN DO:
           DEF VAR lv-cost AS DEC NO-UNDO.
           FIND FIRST po-ordl WHERE po-ordl.company = cocode AND
                              po-ordl.po-no = integer(FGReceiptRow.po-no) AND
                              po-ordl.i-no = FGReceiptRow.i-no
                                  NO-LOCK NO-ERROR.
           IF AVAIL po-ordl THEN DO:
                  ASSIGN FGReceiptRow.cost-uom = po-ordl.pr-uom
                         lv-cost = po-ordl.cost * (IF po-ordl.disc NE 0 THEN (1 - (po-ordl.disc / 100)) ELSE 1)
                         .
                  
                  DEF VAR lv-use-full-qty AS LOG.
                  DEF VAR lv-full-qty AS DEC NO-UNDO.      
                  DEF VAR lvCalcCostUom LIKE fg-rctd.cost-uom NO-UNDO.
                  DEF VAR lvCalcStdCost LIKE fg-rctd.std-cost NO-UNDO.
                  DEF VAR lvCalcExtCost LIKE fg-rctd.ext-cost NO-UNDO.
                  DEF VAR lvCalcFrtCost LIKE fg-rctd.frt-cost NO-UNDO.
                  DEF VAR lvSetupPerCostUom AS DEC NO-UNDO.  
                  RUN fg/calcRcptCostFromPO.p 
                    (INPUT cocode ,
                     INPUT ROWID(po-ordl),
                     INPUT ROWID(fg-rctd),
                     INPUT FGReceiptRow.qty-case,
                     INPUT STRING(FGReceiptRow.cases),
                     INPUT fg-rctd.partial,
                     INPUT fg-rctd.job-no,
                     INPUT fg-rctd.job-no2,
                     INPUT fg-rctd.cost-uom,
                     INPUT fg-rctd.t-qty,
                     OUTPUT lv-use-full-qty,
                     OUTPUT lv-full-qty,
                     OUTPUT lvCalcCostUom,
                     OUTPUT lvCalcStdCost,
                     OUTPUT lvCalcExtCost,
                     OUTPUT lvCalcFrtCost,
                     OUTPUT lvSetupPerCostUom).
      
                  ASSIGN FGReceiptRow.cost-uom = lvCalcCostUom
                         FGReceiptRow.std-cost = (lvCalcStdCost)
                         FGReceiptRow.ext-cost = (lvCalcExtCost)
                         fg-rctd.cost-uom = lvCalcCostUom
                         fg-rctd.std-cost = lvCalcStdCost
                         fg-rctd.ext-cost = lvCalcExtCost
                         lv-cost = FGreceiptRow.std-cost.   
                         
                  /*RUN convert-vend-comp-curr(INPUT-OUTPUT lv-cost).*/
                  FIND FIRST po-ord WHERE po-ord.company EQ po-ordl.company AND
                                          po-ord.po-no   EQ po-ordl.po-no    NO-LOCK NO-ERROR.
                  IF AVAIL po-ord THEN DO:
                     FIND FIRST vend WHERE vend.company EQ po-ord.company AND
                                           vend.vend-no EQ po-ord.vend-no NO-LOCK NO-ERROR.
                     IF AVAIL vend THEN  DO:
                        FIND FIRST company WHERE company.company EQ cocode NO-LOCK.
                        IF vend.curr-code NE company.curr-code THEN DO:
                           FIND FIRST currency WHERE currency.company EQ po-ord.company AND
                                                     currency.c-code EQ vend.curr-code  
                                                     NO-LOCK NO-ERROR.
                           IF AVAIL currency THEN lv-cost = lv-cost * currency.ex-rate.               
                        END.
                     END.
                  END.      
           END.
           ASSIGN FGReceiptRow.std-cost = lv-cost
                  fg-rctd.std-cost = lv-cost.                                          
       END.
       ELSE IF FGReceiptRow.std-cost = 0 and FGReceiptRow.i-no <> "" THEN DO:
            FIND itemfg WHERE itemfg.company = cocode
                          AND itemfg.i-no = FGReceiptRow.i-no NO-LOCK NO-ERROR.
            IF AVAIL itemfg THEN 
               ASSIGN FGReceiptRow.std-cost = itemfg.avg-cost
                      FGReceiptRow.cost-uom = itemfg.prod-uom
                      fg-rctd.std-cost = itemfg.avg-cost
                      fg-rctd.cost-uom = itemfg.prod-uom
                      .
       END. 
    END.
    
    
    
END PROCEDURE.

PROCEDURE show-freight:
/*------------------------------------------------------------------------------
 Purpose:
 Notes:
------------------------------------------------------------------------------*/
  DEF VAR ld AS DEC NO-UNDO.

  IF fgpofrt-log THEN 
  DO:
    ASSIGN
     ld = (fg-rctd.frt-cost)
     fg-rctd.ext-cost = (fg-rctd.ext-cost - ld).

    RUN get-freight-cost (OUTPUT ld).

    ASSIGN
     fg-rctd.frt-cost = ld
     fg-rctd.ext-cost = fg-rctd.ext-cost + ld.
  END.


END PROCEDURE.


/* ************************  Function Implementations ***************** */

FUNCTION maxComponentQty RETURNS DECIMAL 
	  ( /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  DEFINE VARIABLE cHeaderItem AS CHAR        NO-UNDO.
  DEFINE VARIABLE dMaxQty     AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE dMaxCompQty AS DECIMAL     NO-UNDO.
  DEFINE VARIABLE hProc       AS HANDLE      NO-UNDO.
  DEFINE BUFFER   bfItemfg FOR itemfg.
  /* Obtain quantity of set header record */
/*  RUN get-link-handle IN adm-broker-hdl                                      */
/*                       (THIS-PROCEDURE,'container-source':U,OUTPUT char-hdl).*/
/*  IF VALID-HANDLE(WIDGET-HANDLE(char-hdl)) THEN DO:                          */
/*    hProc = WIDGET-HANDLE(char-hdl).                                         */
/*    RUN get-header-qty IN hProc (OUTPUT cHeaderItem, OUTPUT dMaxQty).        */
    assign cHeaderItem = fg-rctd.i-no
           dMaxQty = fg-rctd.t-qty.  
    IF cHeaderItem GT "" THEN
      FIND bfItemfg WHERE bfItemfg.company EQ cocode 
        AND bfItemfg.i-no = cHeaderItem NO-LOCK NO-ERROR.
  
  /* Obtain the Quantity for current component */
  IF AVAIL bfItemfg THEN
    RUN fg/fullset.p (INPUT ROWID(bfItemFg)).

  FIND FIRST tt-fg-set WHERE tt-fg-set.part-no EQ fg-rctd.i-no
    NO-LOCK NO-ERROR.
  
  IF AVAIL tt-fg-set THEN
    dMaxCompQty = dMaxQty * tt-fg-set.part-qty-dec.
  RETURN dMaxCompQty.   /* Function return value. */

		
		
END FUNCTION.

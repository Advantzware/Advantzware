            /*------------------------------------------------------------------------
    File        : gsa2.p
    Purpose     : 

    Syntax      :

    Description : Return a Dataset of Corrugated Estimat

    Author(s)   : 
    Created     : june 14 2010 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttGsaFile2 NO-UNDO 
     FIELD vMatcost  AS DECIMAL EXTENT 6
     FIELD vMatpct  AS DECIMAL EXTENT 6
     FIELD vLabcost  AS DECIMAL EXTENT 6
     FIELD vLabpct  AS DECIMAL EXTENT 6
    
     FIELD ld-gsa-fm  AS DECIMAL    /* brock  comm*/
     FIELD ld-qty  AS INTEGER           /* "Overrides for QTY" */
     FIELD ld-gsa-war-tot AS DECIMAL    /* "Total Charge" */
     FIELD ld-gsa-war-cnt  AS INTEGER  /* "Pallet Count" */
     FIELD ld-gsa-war-u-p AS INTEGER   /* "Units per Pallet" */
     FIELD ld-gsa-war-uni  AS DECIMAL        /* "Total Units" */
     FIELD ld-gsa-war-u-c   AS INTEGER  /*"Unit Count"*/
     FIELD ld-gsa-war-hdl AS DECIMAL   /* "Pallet Handling Charge" */
     FIELD ld-gsa-brd AS DECIMAL        /* "GS&A Board %" */
     FIELD ld-gsa-mat AS DECIMAL        /* "GS&A Material %" */
     FIELD ld-gsa-lab AS DECIMAL        /* "GS&A Labor %" */
     FIELD ld-gsa-war AS DECIMAL        /* "Warehousing Mark Up %" */
     /*FIELD ld-gsa-war-uni AS DECIMAL    /* "=====  GS&A MARK UP PERCENTAGES  ====" */*/
     FIELD ld-gsa-war-pal  AS INTEGER   /* "Total Pallets" */
     FIELD ld-gsa-war-amt   AS INTEGER    /* "Cost per Pallet" */
     FIELD ld-gsa-war-per AS INTEGER    /* "Pallets Delivered/Month" */
     FIELD cha-value  AS CHAR  
     FIELD lkjd AS CHAR 
    .

DEFINE DATASET dsGsaFile2 FOR ttGsaFile2.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmRowId     AS CHAR  NO-UNDO.
DEFINE INPUT PARAMETER prmIpQty     AS INT  NO-UNDO.
DEFINE INPUT PARAMETER prmIpRels    AS INT  NO-UNDO.

DEFINE INPUT PARAMETER prmEst       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmQty       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBoard     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmMaterial  AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmLabor     AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmWHMark    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmBrComm    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnCount   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPallCount AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmTotUnit   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmUnitprPal AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmPaHandCh  AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmPallDelMon AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmTotCharge AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCostPerPall AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmTotPall AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmSeq        AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmGetqty     AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmForm       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlank      AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmDoGsa      AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmDoMr       AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmDoSpeed    AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmDropRc     AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER prmVendor    AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER prmGsaMat   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmGsaLab   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmGsaWar   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmGsaFm   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmGsaMonth   AS INT NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGsaFile2.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

    IF prmAction    = ? THEN ASSIGN   prmAction = "" .  
    IF prmUser      = ? THEN ASSIGN   prmUser   = "".   

    IF prmIpQty     = ? THEN ASSIGN   prmIpQty  = 0.   
    IF prmIpRels    = ? THEN ASSIGN   prmIpRels = 0.   
    IF prmQty       = ? THEN ASSIGN   prmQty    = 0.   
    IF prmBoard     = ? THEN ASSIGN   prmBoard  = 0.   
    IF prmMaterial  = ? THEN ASSIGN   prmMaterial = 0. 
    IF prmLabor     = ? THEN ASSIGN   prmLabor   = 0.  
    IF prmWHMark    = ? THEN ASSIGN   prmWHMark  = 0.  
    IF prmBrComm    = ? THEN ASSIGN   prmBrComm  = 0.  
    IF prmUnCount   = ? THEN ASSIGN   prmUnCount = 0.  
    IF prmPallCount = ? THEN ASSIGN   prmPallCount = 0. 
    IF prmTotUnit   = ? THEN ASSIGN   prmTotUnit   = 0.
    IF prmUnitprPal = ? THEN ASSIGN   prmUnitprPal = 0.
    IF prmPaHandCh  = ? THEN ASSIGN   prmPaHandCh  = 0.
    IF prmPallDelMon = ? THEN ASSIGN  prmPallDelMon = 0.
    IF prmTotCharge  = ? THEN ASSIGN  prmTotCharge  = 0.
        
    IF prmSeq        = ? THEN ASSIGN  prmSeq        = 0.
    IF prmGetqty     = ? THEN ASSIGN  prmGetqty     = 0.
    IF prmForm       = ? THEN ASSIGN  prmForm       = 0.
    IF prmBlank      = ? THEN ASSIGN  prmBlank      = 0.
                                                       
    IF prmDoGsa      = ? THEN ASSIGN  prmDoGsa      = "No".
    IF prmDoMr       = ? THEN ASSIGN  prmDoMr       = "No" .
    IF prmDoSpeed    = ? THEN ASSIGN  prmDoSpeed    = "No" .
    IF prmDropRc     = ? THEN ASSIGN  prmDropRc     = "No".   
    
    IF prmVendor     = ? THEN ASSIGN prmVendor   = "".        
    

DEF VAR prmComp AS CHAR NO-UNDO.
DEF VAR prmLoc AS CHAR NO-UNDO.


    FIND FIRST usercomp WHERE
         usercomp.user_id = prmUser AND
         usercomp.loc = '' AND
         usercomp.company_default = YES
         NO-LOCK NO-ERROR.

    prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
    prmLoc = /*IF AVAIL usercomp THEN usercomp.loc  ELSE*/ "MAIN".

    def VAR io-do-speed as log no-undo.
    def VAR io-do-mr as log no-undo.
    def VAR io-do-gsa as log no-undo.
    def VAR io-v-drop-rc as log no-undo.
    def VAR io-v-match-up as dec no-undo.
    def VAR io-ink-all-forms AS LOG NO-UNDO.
    def var v-brd-only like sys-ctrl.log-fld init no no-undo.
    def var v-brd-cost as dec no-undo.

     DEF BUFFER b-probe FOR probe.
    /*{custom/globdefs.i}*/

    {sys/inc/var.i NEW SHARED}    
    {cec/print4.i "new shared" "new shared"}
    {cec/print42.i "new shared"}  
    
    DEF BUFFER probe-ref FOR reftable.
    DEF BUFFER probe-fm FOR reftable.
    DEF BUFFER probe-board FOR reftable.
    
    DEF VAR ld-msf-per AS DEC INIT 1 NO-UNDO.
    DEF VAR v-dec AS DEC NO-UNDO.

    def new shared buffer xeb for eb.
DEF NEW SHARED buffer xest for est.
DEF NEW shared buffer xef for ef.
def var v-avg-com as log NO-UNDO.
def var v-avg-tan as log NO-UNDO.
DEF NEW SHARED TEMP-TABLE tt-rel NO-UNDO LIKE reftable.
def new shared var v-summ as log init NO NO-UNDO.
DEF VAR lv-override AS LOG NO-UNDO.
DEF BUFFER bf-est FOR est.
DEF BUFFER bf-eb FOR eb.
DEF BUFFER bf-ef FOR ef.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.
def new shared var tmp-dir as cha no-undo.
DEF VAR lv-eqty LIKE est-op.qty NO-UNDO.
def var vn-out like xef.n-out NO-UNDO.
DEF VAR lv-tot-up AS INT NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>9.99" NO-UNDO.
DEF VAR ld-hand-pct AS DEC NO-UNDO.
DEF VAR v-rm$ AS DEC NO-UNDO.
def new shared var fr-tot-pre as dec.
def var v-msf     as dec NO-UNDO.
def var v-blk-wt  as dec NO-UNDO.
DEF VAR ld-fg-amt AS DEC NO-UNDO.
DEF VAR li-rels AS INT NO-UNDO.
    def buffer xcar for car.
DEF VAR ld-fg-rate AS DEC NO-UNDO.
DEF VAR ll-tandem AS LOG NO-UNDO.
def var v-mat     as dec NO-UNDO.
    def var v-lab     as dec NO-UNDO.
    def var v-foh     as dec NO-UNDO.
    def var v-voh     as dec NO-UNDO.
DEF VAR ls-outfile AS cha NO-UNDO.
    
    DEFINE NEW SHARED VARIABLE g_company AS CHARACTER NO-UNDO.
    DEFINE NEW SHARED VARIABLE g_loc AS CHARACTER NO-UNDO.
    /*getqty*/
    def var call_id as recid no-undo.
def var v-vend-no   like e-item-vend.vend-no init "".
DEF var v-vend-list AS CHAR NO-UNDO.
def var lv-error as log no-undo.
def var lv-ef-recid as recid no-undo.
def new shared var k_frac as dec init "6.25" no-undo.
def new shared var day_str as cha form "x(10)" no-undo.
def new shared var tim_str as cha form "x(8)" no-undo.

def new shared var v-prep-mat like tprep-mat no-undo.  /* for probemk cost */
def new shared var v-prep-lab like tprep-lab no-undo.
def new shared var qty as int NO-UNDO.
def new shared var v-drop-rc as log no-undo.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.
def new shared var maxpage as int form ">9" no-undo.

def new shared workfile w-form
    field form-no like ef.form-no
    field min-msf as   log init no.

def new shared buffer xop for est-op.

def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.
def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.

def workfile w-est field w-est-no like est.est-no
                   field w-row-id as   rowid.
/*def var i as int no-undo.*/
def var li-seq as int no-undo.
/*def shared var cocode as cha no-undo.*/    

DEFINE VAR v AS INT NO-UNDO.
DEF VAR v-line LIKE probe.line no-undo.
  DEF VAR v-yld-qty AS DEC FORMAT ">>>,>>>" NO-UNDO.
  DEF VAR v-hdr-depth AS CHAR FORMAT "x(5)" NO-UNDO.
  DEF VAR v-n-out AS INT NO-UNDO.
  DEF VAR ll-use-defaults AS LOG NO-UNDO.
  DEF BUFFER reftable-fm FOR reftable.
  DEF BUFFER reftable-broker-pct FOR reftable.
  DEF BUFFER b-est-qty-2 FOR est-qty.
  DEF VAR v-count-2 AS INT NO-UNDO.

DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.
def var v-gsa as log init no no-undo.
def var v-bqty as int no-undo.
def var v-module as char format "x(60)" no-undo.


ASSIGN
 cocode = prmComp
 locode = prmLoc  
 g_company = prmComp 
 g_loc  = prmLoc
    .

find first sys-ctrl where
  sys-ctrl.company eq cocode AND
  sys-ctrl.name    eq "CEBROWSE"
  no-lock no-error.

/*if not avail sys-ctrl then do transaction:
   create sys-ctrl.
   assign sys-ctrl.company = cocode
          sys-ctrl.name    = "CEBROWSE"
          sys-ctrl.descrip = "# of Records to be displayed in browser"
          sys-ctrl.log-fld = YES
          sys-ctrl.char-fld = "CE"
          sys-ctrl.int-fld = 30.
end.*/

IF sys-ctrl.char-fld NE "" THEN
   tmp-dir = sys-ctrl.char-fld.
ELSE
   tmp-dir = "users\".

   IF INDEX(tmp-dir ,'P:',1) > 0 THEN ASSIGN
     tmp-dir  = REPLACE(tmp-dir ,'P:',"D:").

IF LOOKUP(SUBSTRING(tmp-dir,LENGTH(tmp-dir)),"\,/") EQ 0 THEN
   tmp-dir = tmp-dir + "\".

tmp-dir = REPLACE(tmp-dir,"/","\").

FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) AND est.company = prmComp NO-LOCK NO-ERROR.
FIND FIRST ef WHERE ef.est-no = est.est-no AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
ASSIGN
    v-form-no = ef.form-no .
   FOR EACH probe WHERE probe.company = est.company and probe.est-no = est.est-no 
      AND probe.probe-date ne ? EXCLUSIVE-LOCK, 
      FIRST reftable WHERE reftable.reftable EQ "probe.board" AND 
        reftable.company  EQ probe.company AND 
        reftable.loc      EQ ""            AND 
        reftable.code     EQ probe.est-no  AND 
        reftable.code2    EQ STRING(probe.line,"9999999999") EXCLUSIVE-LOCK 
        BY probe.company BY probe.est-no BY probe.probe-date BY probe.est-qty:
   ASSIGN 
       prmRowId = STRING( ROWID(probe)) .
   END.

  ASSIGN  vprint = YES .
 
   
DO TRANSACTION:
  {sys/inc/cewhschg.i}
  {cec/msfcalc.i}
  {est/calcpcts.i est}
  FIND CURRENT calcpcts NO-LOCK NO-ERROR.
END.

DO TRANSACTION:
  {sys/inc/cerun.i C}
  
  ASSIGN
   do-speed  = sys-ctrl.log-fld
   vmclean   = sys-ctrl.char-fld ne ""
   vsuthrlnd = lookup(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") ne 0 .
  

  {sys/inc/cerun.i F}
  {sys/inc/cewhatif.i}
  {sys/inc/ceprint.i}
END.

{sys/inc/ceprep.i}



    find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "SETPRINT"
      no-lock no-error.
  if  avail sys-ctrl then do:
     vmclean2 = sys-ctrl.char-fld eq "McLean".
  END.


if prmAction = "select" then do:

FOR EACH ce-ctrl WHERE ce-ctrl.company = prmComp and  
        ce-ctrl.loc = prmLoc SHARE-LOCK.

        CREATE ttGsaFile2 .

        ASSIGN
           ttGsaFile2.vMatcost[1] = ce-ctrl.mat-cost[1] 
           ttGsaFile2.vMatcost[2] = ce-ctrl.mat-cost[2] 
           ttGsaFile2.vMatcost[3] = ce-ctrl.mat-cost[3] 
           ttGsaFile2.vMatcost[4] = ce-ctrl.mat-cost[4] 
           ttGsaFile2.vMatcost[5] = ce-ctrl.mat-cost[5] 
           ttGsaFile2.vMatcost[6] = ce-ctrl.mat-cost[6] 

           ttGsaFile2.vMatpct[1] = ce-ctrl.mat-pct[1]
           ttGsaFile2.vMatpct[2] = ce-ctrl.mat-pct[2]
           ttGsaFile2.vMatpct[3] = ce-ctrl.mat-pct[3]
           ttGsaFile2.vMatpct[4] = ce-ctrl.mat-pct[4]
           ttGsaFile2.vMatpct[5] = ce-ctrl.mat-pct[5]
           ttGsaFile2.vMatpct[6] = ce-ctrl.mat-pct[6]

           ttGsaFile2.vLabcost[1] = ce-ctrl.lab-cost[1]
           ttGsaFile2.vLabcost[2] = ce-ctrl.lab-cost[2]
           ttGsaFile2.vLabcost[3] = ce-ctrl.lab-cost[3]
           ttGsaFile2.vLabcost[4] = ce-ctrl.lab-cost[4]
           ttGsaFile2.vLabcost[5] = ce-ctrl.lab-cost[5]
           ttGsaFile2.vLabcost[6] = ce-ctrl.lab-cost[6]

           ttGsaFile2.vLabpct[1] = ce-ctrl.lab-cost[1]
           ttGsaFile2.vLabpct[2] = ce-ctrl.lab-cost[2]
           ttGsaFile2.vLabpct[3] = ce-ctrl.lab-cost[3]
           ttGsaFile2.vLabpct[4] = ce-ctrl.lab-cost[4]
           ttGsaFile2.vLabpct[5] = ce-ctrl.lab-cost[5]
           ttGsaFile2.vLabpct[6] = ce-ctrl.lab-cost[6] .




  DEF VAR li-qty AS INT NO-UNDO.
  
  FIND probe NO-LOCK WHERE probe.LINE EQ prmSeq AND probe.company = prmComp  AND probe.est-no  = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst)  NO-ERROR.
  IF AVAIL probe  THEN
      ASSIGN
      qty = prmQty .

  
  IF AVAIL probe THEN
  FIND FIRST est NO-LOCK
      WHERE est.company EQ probe.company
        AND est.est-no  EQ probe.est-no
      NO-ERROR.
  
  find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.

  IF AVAIL est THEN DO:
   
   ASSIGN
     ttGsaFile2.ld-gsa-brd     = calcpcts.val[1]
     ttGsaFile2.ld-gsa-mat     = prmGsaMat 
     ttGsaFile2.ld-gsa-lab     = prmGsaLab 
     ttGsaFile2.ld-gsa-war     = prmGsaWar 
     ttGsaFile2.ld-gsa-fm      = prmGsaFm  
     ttGsaFile2.ld-gsa-war-tot = ctrl2[1]
     ttGsaFile2.ld-qty         = qty 
     ttGsaFile2.cha-value      = cewhschg-cha     .
         
    IF cewhschg-cha BEGINS "$" THEN DO:
      /*FOR EACH brd
          WHERE (brd.form-no EQ v-form-no OR (NOT vmclean2))
            AND CAN-FIND(FIRST ITEM WHERE item.company  EQ cocode
                                      AND item.i-no     EQ brd.i-no
                                      AND item.mat-type EQ "D"):

        li-qty = li-qty + brd.qty.
      END.*/

        ASSIGN
            li-qty = prmGsaMonth .
      
      FIND FIRST eb
          WHERE eb.company  EQ cocode
            AND eb.est-no   EQ est.est-no
            AND (eb.form-no EQ v-form-no OR (NOT vmclean2))
          NO-LOCK NO-ERROR.

      IF cewhschg-cha EQ "$/MSF" THEN DO:
      /*  lv-head8 = "Cost per MSF".*/
        IF AVAIL eb THEN DO:
          ld-msf-per = eb.t-len * eb.t-wid * eb.tr-cnt.
          IF v-corr THEN ld-msf-per = ld-msf-per * .007.
                    ELSE ld-msf-per = ld-msf-per / 144.
          ld-msf-per = ld-msf-per / 1000.
        END.
      END.

      ASSIGN
       ttGsaFile2.ld-gsa-war-u-c = IF AVAIL eb THEN eb.tr-cnt
                                    ELSE ROUND(qty / li-qty,0)
       ttGsaFile2.ld-gsa-war-u-p = 1
       ttGsaFile2.ld-gsa-war-amt = cewhschg-dec
       ttGsaFile2.ld-gsa-war-hdl = cewhschg-int
       ttGsaFile2.ld-gsa-war-per = li-qty.

      IF ttGsaFile2.ld-gsa-war-u-c NE 0 THEN
         v-dec = ld-qty / ttGsaFile2.ld-gsa-war-u-c.

      {sys/inc/roundup.i v-dec}

      ttGsaFile2.ld-gsa-war-uni = v-dec.

    END.

   /* IF NOT vprint OR est.override THEN*/
    IF probe.LINE <> 1 AND est.override THEN
    FOR EACH b-probe NO-LOCK
        WHERE b-probe.company    EQ est.company
          AND b-probe.est-no     EQ est.est-no
          /*AND ROWID(b-probe)     NE ROWID(probe)*/
           AND b-probe.LINE      = INT(probe.LINE - 1)
        BY b-probe.est-qty
        BY b-probe.probe-date DESC
        BY b-probe.probe-time DESC:
        
      ASSIGN
       ttGsaFile2.ld-gsa-mat = b-probe.gsa-mat
       ttGsaFile2.ld-gsa-lab = b-probe.gsa-lab
       ttGsaFile2.ld-gsa-war = b-probe.gsa-war
       ttGsaFile2.ld-gsa-war-per = b-probe.gsa-war-per.

      FIND FIRST probe-ref NO-LOCK
          WHERE probe-ref.reftable EQ "probe-ref"
            AND probe-ref.company  EQ b-probe.company
            AND probe-ref.loc      EQ ""
            AND probe-ref.code     EQ b-probe.est-no
            AND probe-ref.code2    EQ STRING(b-probe.line,"9999999999")
          NO-ERROR.
      IF AVAIL probe-ref THEN ttGsaFile2.ld-gsa-brd = probe-ref.val[1].

      FIND FIRST probe-fm NO-LOCK
          WHERE probe-fm.reftable EQ "gsa-fm"
            AND probe-fm.company  EQ b-probe.company
            AND probe-fm.loc      EQ ""
            AND probe-fm.code     EQ b-probe.est-no
          NO-ERROR.

      IF AVAIL probe-fm THEN
         ttGsaFile2.ld-gsa-fm = probe-fm.val[1].
      
     /* IF  vprint THEN
        ASSIGN
         ttGsaFile2.ld-gsa-war-amt = b-probe.gsa-war-amt
         ttGsaFile2.ld-gsa-war-hdl = b-probe.gsa-war-hdl
         ttGsaFile2.ld-gsa-war-tot = b-probe.gsa-war-tot
         ttGsaFile2.ld-gsa-war-u-c = b-probe.gsa-war-u-c
         ttGsaFile2.ld-gsa-war-cnt = b-probe.gsa-war-cnt
         ttGsaFile2.ld-gsa-war-uni = b-probe.gsa-war-uni
         ttGsaFile2.ld-gsa-war-pal = b-probe.gsa-war-pal
         ttGsaFile2.ld-gsa-war-per = b-probe.gsa-war-per
         ttGsaFile2.ld-gsa-war-u-p = b-probe.gsa-war-u-p.*/

      IF b-probe.est-qty GE prmQty THEN LEAVE.
   END.

            RUN calc-count.
            RUN calc-pallets.
            RUN calc-war-tot.


  END.    
    END.


END.  /*if prmAction <> "search" then do*/ 

IF prmAction  = "update" THEN DO:    

FIND probe NO-LOCK WHERE probe.LINE EQ prmSeq AND probe.company = prmComp  AND probe.est-no  = FILL(" ",8 - LENGTH(TRIM(prmEst))) + TRIM(prmEst) NO-ERROR.

IF AVAIL probe THEN
FIND FIRST est NO-LOCK
    WHERE est.company EQ probe.company
      AND est.est-no  EQ probe.est-no
    NO-ERROR.
FIND FIRST ef WHERE ef.est-no = est.est-no AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
FIND FIRST eb WHERE eb.est-no = est.est-no AND eb.company = prmComp AND eb.form-no = ef.form-no AND eb.blank-no = prmBlank NO-LOCK NO-ERROR. 


IF AVAIL est THEN 
   find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
    find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
    find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.


  FIND FIRST ce-ctrl NO-LOCK
      WHERE ce-ctrl.company EQ est.company
        AND ce-ctrl.loc     EQ est.loc 
      NO-ERROR.

 /* RUN update-pcts.*/

  RUN update-probe.

  
qtty = 0. 

  ASSIGN
       v-vend-no  = prmVendor .

  IF v-vend-no EQ "&nbsp;" OR v-vend-no EQ ? THEN DO:
        ASSIGN
          v-vend-no = "".
  END.  

  find first xef where xef.company = xest.company 
        AND xef.est-no = xest.est-no.              
  find first xeb where xeb.company = xest.company 
        AND xeb.est-no   eq xest.est-no
        and xeb.form-no eq xef.form-no.
  find first xop where xop.company = xest.company 
        AND xop.est-no    eq xest.est-no
        and xop.op-speed eq 0
        no-lock no-error.

  RUN ce/com/istandem.p (ROWID(xest), OUTPUT ll-tandem).        

  save-lock = xef.op-lock.


  do transaction:
            /*{cec/msfcalc.i}       */
            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "CEDFAULT" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CEDFAULT"
                sys-ctrl.log-fld = no
                sys-ctrl.descrip = "Use CERUN & CEGSA log values on Whatif?  " +
                        "No uses saved est. values!".
                MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
            end. */
  
            ll-use-defaults = sys-ctrl.log-fld.

            {est/recalc-mr.i xest}
            FIND CURRENT recalc-mr NO-LOCK.

            /*{sys/inc/cerun.i C}
            ASSIGN
            do-speed  = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.recalc
            do-mr     = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE (recalc-mr.val[1] EQ 1)
            vmclean   = sys-ctrl.char-fld NE ""
            vmclean2  = NO
            vsuthrlnd = LOOKUP(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") NE 0
            v-module  = IF AVAIL company THEN company.NAME ELSE cocode
            v-module  = v-module + " - " + IF AVAIL loc THEN loc.dscr ELSE locode.

            IF sys-ctrl.char-fld EQ "Brick" THEN v-module = v-module + " - ISO# CS-03-1-F".

            {sys/inc/ctrtext.i "v-module" 60}.
             */

            ASSIGN 
                vmclean2  = NO.


            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "CEGSA" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CEGSA"
                sys-ctrl.descrip = "Default for GS&A override".
                MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE sys-ctrl.log-fld.
            end.
            */
            do-gsa = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.override.


            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "CESLIT" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CESLIT"
                sys-ctrl.descrip = "Ask 'Drop Slitter...' question at OE cost calculation?"
                sys-ctrl.log-fld = no.
                MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE sys-ctrl.log-fld.
            end.
            */
            v-drop-rc = sys-ctrl.log-fld.

            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "COMBCOST" no-lock no-error.
            /*if not avail sys-ctrl then do:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "COMBCOST"
                sys-ctrl.descrip = "Average Cost for Combination Items?" 
                sys-ctrl.log-fld = no.
                MESSAGE sys-ctrl.descrip
                VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
            end.
            */
            
            assign
                v-avg-com = sys-ctrl.log-fld
                v-avg-tan = sys-ctrl.int-fld eq 0.             
        end.
        
        EMPTY TEMP-TABLE tt-rel.


        if vprint then do:  
            ASSIGN
            do-speed    = LOGICAL(prmDoSpeed)
            do-mr       = LOGICAL(prmDoMr)
            do-gsa      = LOGICAL(prmDoGsa)
            v-summ      = NO
            lv-override = NO
            .
                                                    

            IF do-speed THEN                  
            IF lv-override THEN
            for each probe where probe.company = xest.company AND probe.est-no = xest.est-no:
                delete probe.                 
            end.
        end.

        ELSE DO:
            find first sys-ctrl where sys-ctrl.company eq cocode and sys-ctrl.name    eq "FGCOST" no-lock no-error.
            /*if not avail sys-ctrl then do transaction:
                create sys-ctrl.
                assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "FGCOST"
                sys-ctrl.log-fld = no
                sys-ctrl.descrip = "Create FG Cost in Job File with only Board Cost?".
                MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                    UPDATE sys-ctrl.log-fld.
            end.
            */
            v-brd-only = sys-ctrl.log-fld.
        END.       
        
        FOR EACH eb fields(company est-no form-no blank-no) NO-LOCK
            WHERE eb.company EQ xest.company
            AND eb.est-no  EQ xest.est-no,
                FIRST reftable NO-LOCK
                    WHERE reftable.reftable EQ "ce/com/selwhif1.w"
                    AND reftable.company  EQ eb.company
                    AND reftable.loc      EQ eb.est-no
                    AND reftable.code     EQ STRING(eb.form-no,"9999999999")
                    AND reftable.code2    EQ STRING(eb.blank-no,"9999999999"):                                  
                CREATE tt-rel. 
                BUFFER-COPY reftable TO tt-rel.                
        END.        
       

        DO TRANSACTION:          
            {est/op-lock.i xest}
            FIND bf-est WHERE RECID(bf-est) EQ RECID(xest).
            FIND CURRENT recalc-mr.

            ASSIGN
                bf-est.recalc    = do-speed               
                recalc-mr.val[1] = INT(do-mr)
                bf-est.override  = do-gsa
                op-lock.val[1]   = INT(bf-est.recalc)
                op-lock.val[2]   = recalc-mr.val[1].
         
            FIND CURRENT bf-est NO-LOCK.
            FIND CURRENT recalc-mr NO-LOCK.
            FIND CURRENT op-lock NO-LOCK.
            FIND xest WHERE RECID(xest) EQ RECID(bf-est).              
        END.

        FORM day_str v-module tim_str TO 79
     SKIP(1)
     "Combination Est#" xest.est-no FORMAT "x(8)"
     "UserID:" xest.updated-id
     "Prober:" probe.probe-user
     SKIP(1)
     with frame hdr page-top STREAM-IO width 80 no-labels no-box.

FORM "Sales Rep:" kli.sman kli.sname SKIP
     "Cust:" kli.cust-no
             kli.cust-add[1] FORMAT "x(29)" TO 44
     "Ship:" kli.ship-add[1] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[2] FORMAT "x(29)" TO 44
             kli.ship-add[2] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[3] FORMAT "x(29)" TO 44
             kli.ship-add[3] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[4] FORMAT "x(29)" TO 44
             kli.ship-add[4] FORMAT "x(29)" TO 80
             SKIP
    WITH STREAM-IO NO-LABELS NO-BOX DOWN WIDTH 80 FRAME kli.
if retry then output close.

qty = 0.

for each xef FIELDS(company est-no form-no)
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
    no-lock,
    each xeb FIELDS(yld-qty bl-qty yrprice)
    where xeb.company eq xef.company
      and xeb.est-no  eq xef.est-no
      and xeb.form-no eq xef.form-no
    no-lock:
  qty = qty + if xeb.yrprice then xeb.yld-qty else xeb.bl-qty.
end.

/*{est/probeset.i qty 0}*/

v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

ASSIGN
 outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,v-probe-fmt)
 outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,v-probe-fmt)
 outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,v-probe-fmt)
 outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,v-probe-fmt).

/*output to value(outfile1).*/

find first ce-ctrl where (ce-ctrl.company = cocode and ce-ctrl.loc     = locode) no-lock no-error.
assign
v-tt-tot     = 0
v-fac-tot    = 0
v-ord-cost   = 0

ctrl[1]  = ce-ctrl.whse-mrkup / 100
ctrl[2]  = ce-ctrl.hand-pct / 100
ctrl[3]  = ce-ctrl.rm-rate
ctrl[4]  = ce-ctrl.spec-%[1]
ctrl[5]  = int(ce-ctrl.comm-add)
ctrl[6]  = int(ce-ctrl.shp-add)
ctrl[7]  = int(ce-ctrl.sho-labor)
ctrl[8]  = int(ce-ctrl.trunc-99)
ctrl[11] = ce-ctrl.spec-%[2]
ctrl[12] = ce-ctrl.spec-%[3]
ctrl[13] = int(ce-ctrl.spec-add[1])
ctrl[14] = int(ce-ctrl.spec-add[2])
ctrl[15] = int(ce-ctrl.spec-add[3])
ctrl[16] = int(ce-ctrl.spec-add[6])
ctrl[17] = int(ce-ctrl.spec-add[7])
ctrl[18] = int(ce-ctrl.spec-add[8]).

FIND FIRST reftable-broker-pct
     WHERE reftable-broker-pct.reftable EQ "ce-ctrl.broker-pct"
       AND reftable-broker-pct.company  EQ ce-ctrl.company
       AND reftable-broker-pct.loc      EQ ce-ctrl.loc
     NO-LOCK NO-ERROR.

IF AVAIL reftable-broker-pct THEN
   ctrl[19] = reftable-broker-pct.val[1].

FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "ce-ctrl.fg-rate-farm"
      AND reftable.company  EQ ce-ctrl.company
      AND reftable.loc      EQ ce-ctrl.loc
    NO-ERROR.  
fg-rate-f = IF AVAIL reftable THEN reftable.val[1] ELSE 0.

FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "ce-ctrl.rm-rate-farm"
      AND reftable.company  EQ ce-ctrl.company
      AND reftable.loc      EQ ce-ctrl.loc
    NO-ERROR.  
rm-rate-f = IF AVAIL reftable THEN reftable.val[1] ELSE 0.

FIND FIRST reftable NO-LOCK
    WHERE reftable.reftable EQ "ce-ctrl.hand-pct-farm"
      AND reftable.company  EQ ce-ctrl.company
      AND reftable.loc      EQ ce-ctrl.loc
    NO-ERROR.  
hand-pct-f = (IF AVAIL reftable THEN reftable.val[1] ELSE 0) / 100.

DO TRANSACTION:
  FOR each est-op
      WHERE est-op.company EQ xest.company 
        AND est-op.est-no  EQ xest.est-no
        AND est-op.line    GT 500:
    DELETE est-op.
  END.
  FOR EACH est-op FIELDS(qty)
      WHERE est-op.company EQ xest.company
        AND est-op.est-no  EQ xest.est-no
        AND est-op.line    LT 500
      BY est-op.qty:
    lv-eqty = est-op.qty.
    LEAVE.
  END.
  FOR EACH est-op
      WHERE est-op.company EQ xest.company 
        AND est-op.est-no  EQ xest.est-no
        AND est-op.qty     EQ lv-eqty
        AND est-op.line    LT 500:
    CREATE xop.
    BUFFER-COPY est-op EXCEPT rec_key TO xop.
    xop.line = est-op.line + 500.
  END.
END.

for each kli:
  delete kli.
end.

for each ink:
  delete ink.
end.

for each flm:
  delete flm.
end.

for each cas:
  delete cas.
end.

for each car:
  delete car.
end.

for each blk:
  delete blk.
end.

for each xjob:
  delete xjob.
end.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no:

   xxx = 0.
   for each xeb where xeb.company = xest.company
               AND xeb.est-no eq xest.est-no and xeb.form-no = xef.form-no
       BY xeb.blank-no:
      find first kli where kli.cust-no = xeb.cust-no no-error.
      if not avail kli then do:
         find first sman   where   sman.sman    = xeb.sman no-lock no-error.
         find first cust   where   cust.company = cocode and
                                   cust.cust-no = xeb.cust-no no-lock no-error.
         find first shipto where shipto.company = cust.company and
                                 shipto.cust-no = cust.cust-no and
                                 shipto.ship-id = xeb.ship-id no-lock no-error.
         create kli.
         if avail sman then assign kli.sman    = sman.sman
                                     kli.sname   = sman.sname.
         if xeb.cust-no ne "Temp" then assign
         kli.cust-no = xeb.cust-no
         kli.cust-add[1] = cust.name
         kli.cust-add[2] = cust.addr[1]
         kli.cust-add[3] = cust.addr[2]
         kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
         else assign
         kli.cust-no = xeb.cust-no
         kli.cust-add[1] = xeb.ship-name
         kli.cust-add[2] = xeb.ship-addr[1]
         kli.cust-add[3] = xeb.ship-addr[2]
         kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                           xeb.ship-zip.

         if kli.cust-add[3] = "" then assign
            kli.cust-add[3] = kli.cust-add[4] kli.cust-add[4] = "".
         if kli.cust-add[2] = "" then assign
            kli.cust-add[2] = kli.cust-add[3] kli.cust-add[3] = kli.cust-add[4]
            kli.cust-add[4] = "".
         assign
         kli.ship-add[1] = shipto.ship-name
         kli.ship-add[2] = shipto.ship-addr[1]
         kli.ship-add[3] = shipto.ship-addr[2]
         kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                                         " " + shipto.ship-zip.
         if kli.ship-add[3] = "" then
         assign kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
         if kli.ship-add[2] = "" then
         assign kli.ship-add[2] = kli.ship-add[3]
                kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
      end.
      find first blk where blk.snum = xeb.form-no and
                           blk.bnum = xeb.blank-no no-error.
      if not avail blk then do:
         create blk.
         assign
          blk.kli      = kli.cust-no
          blk.id       = xeb.part-no
          blk.snum     = xeb.form-no
          blk.bnum     = xeb.blank-no
          blk.qreq     = xeb.bl-qty
          blk.qyld     = xeb.yld-qty
          blk.yr$      = xeb.yrprice
          blk.stock-no = xeb.stock-no
          blk.pur-man  = xeb.pur-man.
      end.
      xxx = xxx + (xeb.t-sqin * xeb.num-up).
   end.
   for each xeb fields(form-no blank-no t-sqin num-up) where
       xeb.company = xest.company
       AND xeb.est-no eq xest.est-no
       AND xeb.form-no eq xef.form-no no-lock,
       first blk  where blk.snum eq xeb.form-no
                    and blk.bnum eq xeb.blank-no:
       blk.pct = (xeb.t-sqin * xeb.num-up) / xxx.
   end.
end.

/* print header */
ASSIGN
 day_str  = STRING(TODAY,"99/99/9999")
 tim_str  = STRING(TIME,"hh:mm am").

display day_str v-module tim_str
        TRIM(xest.est-no) @ xest.est-no
        xest.updated-id
        probe.probe-user
        with frame hdr .

for each kli with frame kli:
   display kli.sman kli.sname
           kli.cust-no
           kli.cust-add[1] kli.ship-add[1] 
           kli.cust-add[2] kli.ship-add[2]
           kli.cust-add[3] kli.ship-add[3]
           kli.cust-add[4] kli.ship-add[4].
   down.
end.

FOR EACH xef
    WHERE xef.company EQ xest.company 
      AND xef.est-no  EQ xest.est-no
    WITH FRAME brd no-labels no-box width 80 down stream-io:

  RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

  ASSIGN
   brd-l[1] = xef.trim-l
   brd-w[1] = xef.trim-w
   brd-l[2] = xef.gsh-len.

  IF xef.roll THEN brd-l[3] = xef.trim-l.
  brd-w[2] = xef.gsh-wid.
  IF brd-l[2] EQ 0 AND brd-w[2] = 0 THEN
    ASSIGN
     brd-l[2] = xef.lsh-len
     brd-w[2] = xef.lsh-wid.
  brd-w[3] = IF xef.roll THEN xef.nsh-len ELSE 0.
  brd-sq[1] = brd-l[1] * brd-w[1].
  brd-sq[2] = brd-l[2] * brd-w[2].
  brd-sq[3] = brd-l[3] * brd-w[3].

  IF v-corr THEN
    ASSIGN
     brd-sf[1] = brd-sq[1] * .007
     brd-sf[2] = brd-sq[2] * .007
     brd-sf[3] = brd-sq[3] * .007.
   ELSE
     ASSIGN
      brd-sf[1] = brd-sq[1] / 144
      brd-sf[2] = brd-sq[2] / 144
      brd-sf[3] = brd-sq[3] / 144.

  FIND FIRST item
      where (item.company = cocode)
        AND item.i-no EQ xef.board
      NO-LOCK NO-ERROR.
  IF AVAIL item THEN
  FIND FIRST e-item OF item NO-LOCK NO-ERROR.
  ASSIGN
  brd-wu[1] = brd-sf[1]  * item.basis-w
  brd-wu[2] = brd-sf[2]  * item.basis-w
  brd-wu[3] = (brd-sf[3] * item.basis-w) / 2000
  zzz = 0
  tmpstore    = "FORM " +
                TRIM(STRING(xef.form-no,">9")) +
                " OF " +
                TRIM(STRING(xest.form-qty,">9"))
  v-hdr-depth = IF xef.nsh-dep EQ 0 AND
                   xef.gsh-dep EQ 0 THEN "" ELSE "Depth".

  DISPLAY SKIP(1)
          tmpstore                           FORMAT "x(13)"
          "  Width  Length  "
          v-hdr-depth
          "#On  Sq.Inches      Sq.Feet     Wgt/Units"
          SKIP.

  FOR EACH xeb
      WHERE xeb.company EQ xest.company 
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
      BREAK BY xeb.blank-no:

    ASSIGN
    /* set total # of blanks on all forms */
    tt-blk = tt-blk + IF xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty
    /* set total # of blanks on this form */
    t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up
    /* set total qty of all blanks for this form */
    t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] +
                            if xeb.yrprice THEN xeb.yld-qty ELSE xeb.bl-qty.
    /* find sheet qty needed for this form (without spoil)*/
    IF (xeb.yld-qty / xeb.num-up) > zzz THEN
       ASSIGN zzz = xeb.yld-qty / (xeb.num-up * xef.n-out * xef.n-out-l * xef.n-out-d).

    {sys/inc/roundup.i zzz}

    ASSIGN
    t-shtfrm[xeb.form-no] = zzz
    call_id = RECID(xeb)
    brd-l[4]  = xeb.t-len
    brd-w[4]  = xeb.t-wid
    brd-sq[4] = brd-l[4] * brd-w[4]
    brd-sf[4] = IF v-corr THEN brd-sq[4] * .007 ELSE brd-sq[4] / 144
    brd-wu[4] = brd-sf[4] * item.basis-w
    vbsf = vbsf + IF v-corr THEN (xeb.t-sqin * .007) ELSE (xeb.t-sqin / 144).

    DISPLAY "  Blk"
            SPACE(0)
            xeb.blank-no                       FORMAT "99"
            "Size:"
            brd-w[4]                           FORMAT ">>>9.99<<<"
            brd-l[4]                           FORMAT ">>>9.99<<<" 
            xeb.t-dep WHEN xeb.t-dep NE 0      FORMAT ">>>9.99<<<"
            1                                  FORMAT ">>>" 
            SPACE(4)
            brd-sq[4]
            brd-sf[4]                              
            "Sf/BL"
            brd-wu[4]
            SPACE(0)
            "/MBL"
            SKIP
        WITH NO-BOX NO-LABELS COLOR VALUE("blu/brown") WIDTH 80 FRAME aa2-1 STREAM-IO.

    IF NOT vsuthrlnd THEN DO WITH FRAME aa2-1:
      ASSIGN
       brd-w[4]:FORMAT  = ">>>9.99"
       brd-l[4]:FORMAT  = ">>>9.99"
       xeb.t-dep:FORMAT = ">>>9.99".

      DISPLAY {sys/inc/k16v.i brd-w[4]} @ brd-w[4]
              {sys/inc/k16v.i brd-l[4]} @ brd-l[4]
              "" @ xeb.t-dep
              {sys/inc/k16v.i xeb.t-dep} WHEN xeb.t-dep NE 0 @ xeb.t-dep.
    END.

    IF LAST(xeb.blank-no) THEN DO:

      lv-tot-up = 0.

      FOR EACH bf-eb FIELDS(num-up)
          WHERE bf-eb.company EQ xef.company
            AND bf-eb.est-no  EQ xef.est-no
            AND bf-eb.form-no EQ xef.form-no
          NO-LOCK:
          lv-tot-up  = lv-tot-up + bf-eb.num-up.
      END.

      DISPLAY " NetSht Size:"
              brd-w[1]                            FORMAT ">>>9.99<<<"
              brd-l[1]                            FORMAT ">>>9.99<<<"
              xef.nsh-dep WHEN xef.nsh-dep NE 0   FORMAT ">>>9.99<<<"
              lv-tot-up                           FORMAT ">>>"  
              SPACE(4)
              brd-sq[1]
              brd-sf[1]
              "Sf/NS"
              brd-wu[1]
              SPACE(0)
              "/MNS"
              SKIP

              " GrsSht Size:"
              brd-w[2]                            FORMAT ">>>9.99<<<"
              brd-l[2]                            FORMAT ">>>9.99<<<"
              xef.gsh-dep WHEN xef.gsh-dep NE 0   FORMAT ">>>9.99<<<"
              vn-out                              FORMAT ">>>" 
              SPACE(4)
              brd-sq[2]
              brd-sf[2]
              "Sf/GS"
              brd-wu[2]
              SPACE(0)
              "/MGS" SKIP
          WITH NO-BOX NO-LABELS COLOR VALUE("blu/brown") WIDTH 80 FRAME aa2-2 STREAM-IO.

      IF NOT vsuthrlnd THEN DO WITH FRAME aa2-2:
        ASSIGN
         brd-w[1]:FORMAT    = ">>>9.99"
         brd-l[1]:FORMAT    = ">>>9.99"
         xef.nsh-dep:FORMAT = ">>>9.99"
         brd-w[2]:FORMAT    = ">>>9.99"
         brd-l[2]:FORMAT    = ">>>9.99"
         xef.gsh-dep:FORMAT = ">>>9.99".

        DISPLAY {sys/inc/k16v.i brd-w[1]} @ brd-w[1]
                {sys/inc/k16v.i brd-l[1]} @ brd-l[1]
                "" @ xef.nsh-dep
                {sys/inc/k16v.i xef.nsh-dep} WHEN xef.nsh-dep NE 0 @ xef.nsh-dep
                {sys/inc/k16v.i brd-w[2]} @ brd-w[2]
                {sys/inc/k16v.i brd-l[2]} @ brd-l[2]
                "" @ xef.gsh-dep
                {sys/inc/k16v.i xef.gsh-dep} WHEN xef.gsh-dep NE 0 @ xef.gsh-dep.
      END.
    END.
  END.
  FIND xeb WHERE RECID(xeb) = call_id NO-LOCK NO-ERROR. qty = xeb.yld-qty.

  IF brd-w[3] NE 0 THEN
    DISPLAY "Roll  Size :" brd-w[3]                FORMAT ">>9.99<<" TO 22
        WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa3 STREAM-IO.

  IF NOT vsuthrlnd THEN
    IF brd-w[3] NE 0 THEN DISPLAY {sys/inc/k16v.i brd-w[3]} @ brd-w[3] WITH FRAME aa3.

  DISPLAY SKIP(1)
"   Qty      --- Description ------ -- Size / Color ----- --- Style / Part No ---"
      WITH NO-BOX NO-LABELS WIDTH 80 FRAME aa5 DOWN STREAM-IO.

  FOR EACH xeb
      WHERE xeb.company EQ xest.company
        AND xeb.est-no  EQ xest.est-no
        AND xeb.form-no EQ xef.form-no
      BY xeb.blank-no
      WITH STREAM-IO FRAME blk NO-BOX NO-LABELS WIDTH 80 DOWN:

    FIND FIRST style
        WHERE style.company EQ cocode
          AND style.style   EQ xeb.style
        NO-LOCK NO-ERROR.

    ASSIGN
     ld-len = xeb.len * ld-metric
     ld-wid = xeb.wid * ld-metric
     ld-dep = xeb.dep * ld-metric.

    IF ld-metric NE 1 THEN DO:
      {sys/inc/roundup.i ld-len}
      {sys/inc/roundup.i ld-wid}
      {sys/inc/roundup.i ld-dep}
    END.

    ELSE
      ASSIGN
       ld-len = {sys/inc/k16v.i ld-len}
       ld-wid = {sys/inc/k16v.i ld-wid}
       ld-dep = {sys/inc/k16v.i ld-dep}.

    ASSIGN
     sizcol[1]  = TRIM(STRING(ld-len,lv-format)) + "x" +
                  TRIM(STRING(ld-wid,lv-format)) + "x" +
                  TRIM(STRING(ld-dep,lv-format))
     sizcol[2]  = xeb.i-coldscr
     stypart[1] = style.dscr
     stypart[2] = xeb.part-no
     dsc[1]     = xeb.part-dscr1
     dsc[2]     = xeb.part-dscr2.

    DISPLAY /*xeb.cust-no*/
            xeb.yld-qty FORMAT ">>>,>>>,>>9"
              xeb.bl-qty WHEN NOT xeb.yrprice @ xeb.yld-qty SPACE(1)
            dsc[1] FORMAT "x(22)"  
            sizcol[1] FORMAT "x(21)"   
            stypart[1] FORMAT "x(23)" SKIP
            SPACE(3) /* 10*/
            "#UP= " + STRING(xeb.num-up,">>9")
            dsc[2] FORMAT "x(22)"
            sizcol[2] FORMAT "x(21)"
            stypart[2] FORMAT "x(23)" SKIP WITH STREAM-IO.
    DOWN.
  END.
END.  /* for each xef */

PUT SKIP(1)
   "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M    TOTAL" skip.
dm-tot[3] = 0. dm-tot[4] = 0. dm-tot[5] = 0.



/* b o a r d        */ RUN cec/box/pr42-brd.p (v-vend-no, OUTPUT v-vend-list).

v-brd-cost = v-brd-cost + dm-tot[5].
  
/* adders           */ RUN cec/box/pr42-add.p (v-vend-list).



FIND CURRENT probe-board NO-ERROR.
IF AVAIL probe-board THEN
  probe-board.val[1] = probe-board.val[1] + dm-tot[5].
FIND CURRENT probe-board NO-LOCK NO-ERROR.

/* i n k s          */ RUN cec/com/pr4-ink.p.

/* film             */ RUN cec/com/pr4-flm.p.

/* case/tray/pallet */ RUN cec/com/pr4-cas.p.

/* special          */ RUN cec/com/pr4-spe.p.



for each blk:
   find first xjob
        where xjob.i-no     eq blk.id
          and xjob.form-no  eq blk.snum
          and xjob.blank-no eq blk.bnum
        no-error.

   if not avail xjob then do:
     create xjob.
     assign
      xjob.form-no  = blk.snum
      xjob.blank-no = blk.bnum
      xjob.cust-no  = blk.kli.
   end.

   assign
    xjob.mat      = blk.cost - blk.lab
    xjob.lab      = blk.lab
    xjob.i-no     = blk.id
    xjob.pct      = blk.pct
    xjob.stock-no = blk.stock-no
    xjob.pur-man  = blk.pur-man.
end.


display     "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" to 61
            dm-tot[5] / (tt-blk / 1000) format ">>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            skip(1)
    with STREAM-IO frame ac5 no-labels no-box.

/* prep */ run cec/com/pr4-prp.p.


/* misc */ run cec/com/pr4-mis.p.

put skip(1)
   "Machine Description    MR (Hrs) Run  Speed    Rate     MR $    Run $  Total Cost" .
   

/* machines */
run cec/com/pr4-mch.p.

ctrl2[2] = 0.

FOR EACH blk:
  FIND FIRST xjob
      WHERE xjob.i-no     EQ blk.id
        AND xjob.form-no  EQ blk.snum
        AND xjob.blank-no EQ blk.bnum
      NO-ERROR.

  ld-hand-pct = IF blk.pur-man THEN hand-pct-f ELSE ctrl[2].

  IF ld-hand-pct NE 0 THEN
    ASSIGN
     v-rm$    = xjob.mat * ld-hand-pct
     blk.cost = blk.cost + v-rm$
     blk.lab  = blk.lab  + v-rm$
     ctrl2[2] = ctrl2[2] + v-rm$
     xjob.lab = xjob.lab + v-rm$.
END.

if ctrl2[2] ne 0 or ctrl2[3] ne 0 then do:
   put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
   op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
end.

assign
 fr-tot     = 0
 fr-tot-pre = 0
 v-msf      = 0.

for each xef where xef.company = xest.company
               AND xef.est-no eq xest.est-no,
    each xeb where xeb.company = xef.company
               AND xeb.est-no   eq xest.est-no
               and xeb.form-no eq xef.form-no,
   first carrier where carrier.company eq cocode
                    and carrier.carrier eq xeb.carrier no-lock,
   first carr-mtx where carr-mtx.company  eq cocode
      and carr-mtx.carrier  eq carrier.carrier
      and carr-mtx.del-zone eq xeb.dest-code no-lock:

  find first car where car.id eq xeb.part-no no-error.
  if not avail car then do:
    create car.
    assign
     car.carrier = carrier.carrier
     car.dscr    = carr-mtx.del-zone
     car.id      = xeb.part-no
     car.snum    = xeb.form-no
     car.bnum    = xeb.blank-no.
  end.
   
  find first item
      where (item.company = cocode)
        and item.i-no     eq xef.board
        and item.mat-type eq "B"
        and item.avg-w    gt 0
      no-lock no-error.
    
  assign
   v-msf    = (xeb.t-sqin - xeb.t-win) * xeb.bl-qty / 144000
   v-msf    = v-msf * if avail item then item.avg-w else 1
   v-blk-wt = xef.weight * v-msf
   car.msf  = car.msf + v-msf.

  if xef.medium ne "" then do:
    find first item where (item.company = cocode) and
               item.i-no = xef.medium no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    (item.basis-w * (1 - (item.shrink / 100)) * v-msf).
  end.
  if xef.flute ne "" then do:
    find first item where (item.company = cocode) and
               item.i-no = xef.flute no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    (item.basis-w * v-msf).
  end.

  /*
  if xef.lam-code ne "" then do:
    find first item {sys/look/item.w} and
               item.i-no = xef.lam-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.
  if xef.adh-code ne "" then do:
    find first item {sys/look/item.w} and
               item.i-no = xef.adh-code no-lock no-error.
    if avail item
    then v-blk-wt = v-blk-wt +
                    ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                     xeb.bl-qty * xeb.t-sqin / item.sqin-lb).
  end.*/
  
  car.qty = car.qty + v-blk-wt.

  find first blk
      where blk.snum eq xeb.form-no
        and blk.bnum eq xeb.blank-no
      no-lock no-error.
  if avail blk then blk.fg-wt = blk.fg-wt + v-blk-wt.

  /* add pallet & case for total weight */
  find first cas
      where cas.typ  eq 1
        and cas.snum eq xeb.form-no
        and cas.bnum eq xeb.blank-no
      no-error.
  if avail cas then do:
    find first item
        where (item.company = cocode)
          and item.i-no eq cas.ino
        no-lock no-error.
    if avail item then do:
      car.qty = car.qty + (cas.qty * ce-ctrl.def-cas-w /*item.basis-w*/).
      if avail blk then blk.fg-wt = blk.fg-wt + (ce-ctrl.def-cas-w /*item.basis-w*/).
    end.
    release item.
    find first cas
        where cas.typ  eq 3
          and cas.snum eq xeb.form-no
          and cas.bnum eq xeb.blank-no
        no-error.
    if avail cas then
    find first item
        where (item.company = cocode)
          and item.i-no eq cas.ino
        no-lock no-error.
    if avail item then do:
      car.qty = car.qty + (cas.qty * ce-ctrl.def-pal-w /*item.basis-w*/).
      if avail blk then blk.fg-wt = blk.fg-wt + (cas.qty * ce-ctrl.def-pal-w /*item.basis-w*/).
    end.
  end.
end.

ASSIGN
 fg-wt     = 0
 ld-fg-amt = 0.

for each car break by car.id:
  p-qty = 0.
  for each cas
      where cas.typ  eq 3
        and cas.snum eq car.snum
        and cas.bnum eq car.bnum:
        
    p-qty = p-qty + cas.qty.    
  end.
  
  ASSIGN
   z       = 0
   li-rels = 0.

  FOR EACH bf-eb fields(company est-no form-no blank-no bl-qty) NO-LOCK
      WHERE bf-eb.company EQ xest.company
        AND bf-eb.est-no  EQ xest.est-no
        AND bf-eb.part-no EQ car.id:
    z = z + bf-eb.bl-qty.
    FIND FIRST tt-rel
        WHERE tt-rel.reftable EQ "ce/com/selwhif1.w"
          AND tt-rel.company  EQ bf-eb.company
          AND tt-rel.loc      EQ bf-eb.est-no
          AND tt-rel.code     EQ STRING(bf-eb.form-no,"9999999999")
          AND tt-rel.code2    EQ STRING(bf-eb.blank-no,"9999999999")
        NO-ERROR.
    li-rels = li-rels + (IF AVAIL tt-rel THEN tt-rel.val[1] ELSE 1).
  END.

  find first xeb
      where xeb.company = xest.company
        AND xeb.est-no    eq xest.est-no
        and xeb.form-no  eq car.snum
        and xeb.blank-no eq car.bnum
      no-lock no-error.
  find first carrier
      where carrier.company eq cocode
        and carrier.loc     eq locode
        and carrier.carrier eq car.carrier
      no-lock no-error.
  release carr-mtx.
  if avail carrier then
  find first carr-mtx
      where carr-mtx.company  eq cocode
        and carr-mtx.loc      eq locode
        and carr-mtx.carrier  eq carrier.carrier
        and carr-mtx.del-zone eq car.dscr
       no-lock no-error.
  
  assign
   yyy   = 0
   zzz   = 0
   v-msf = 0.
   
  for each xcar
      where xcar.carrier eq car.carrier
        and xcar.dscr    eq car.dscr:  /* Group by zone? */
    assign
     zzz   = zzz + xcar.qty    /* zzz = total wt for price lookup */
     v-msf = v-msf + xcar.msf.  
  end.

  if xeb.fr-out-c ne 0 then
    yyy = xeb.fr-out-c * xxx / 100.
    
  else
  if xeb.fr-out-m ne 0 then
    yyy = xeb.fr-out-m * z / 1000.
    
  else  
  if avail carr-mtx then do:
    if carrier.chg-method eq "P" then
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * p-qty.
      if carr-mtx.weight[i] ge p-qty then leave.
    end.
    
    else
    if carrier.chg-method eq "W" then
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * car.qty / 100.
      if carr-mtx.weight[i] ge zzz then leave.
    end.
    
    else
    do i = 1 to 10:
      yyy = carr-mtx.rate[i] * car.msf.
      if carr-mtx.weight[i] ge v-msf then leave.
    end.
       
    if yyy lt carr-mtx.min-rate then yyy = carr-mtx.min-rate.
        
    yyy = yyy + (carr-mtx.min-rate * (li-rels - 1)).
  end.
  
  assign
   fg-wt    = fg-wt + car.qty
   car.cost = car.cost + yyy
   fr-tot   = fr-tot + yyy.
  
  if xeb.chg-method eq "P" then fr-tot-pre = fr-tot-pre + yyy.

  find first blk where blk.id eq car.id no-error.
  ASSIGN
  blk.sell = blk.sell + yyy  /* use sell for freight costs for now */
  ld-fg-rate = IF blk.pur-man THEN fg-rate-f ELSE ce-ctrl.fg-rate
  blk.lab  = blk.lab  + (car.qty / 100 * ld-fg-rate)
  blk.cost = blk.cost + (car.qty / 100 * ld-fg-rate)
  ld-fg-amt = ld-fg-amt + (car.qty / 100 * ld-fg-rate).
end.

if ld-fg-amt gt 1 then put "Finished Goods Handling" ld-fg-amt to 80 skip.

op-tot[5] = op-tot[5] + ld-fg-amt.

put "TOTAL  OPERATIONS        " op-tot[3] format ">>>>9.99" to 59
    op-tot[4] format ">>>>>9.99" to 69
    op-tot[5] format ">>>>,>>9.99" to 80 skip(1).

/* mat */
   do i = 1 to 6:
      ctrl[9] = ce-ctrl.mat-pct[i] / 100.
      if ce-ctrl.mat-cost[i] > dm-tot[5]  then leave.
   end.

/* lab */
   do i = 1 to 6:
      ctrl[10] = ce-ctrl.lab-pct[i] / 100.
      if ce-ctrl.lab-cost[i] > op-tot[5]  then leave.
   end.
   DO TRANSACTION:
     FIND CURRENT calcpcts EXCLUSIVE-LOCK.
     ASSIGN
      calcpcts.val[1] = ctrl[9] * 100
      calcpcts.val[2] = v-brd-cost.
     FIND CURRENT calcpcts NO-LOCK NO-ERROR.
   END.

assign
 gsa-mat = ctrl[9]  * 100
 gsa-lab = ctrl[10] * 100
 gsa-com = ce-ctrl.comm-mrkup
 gsa-war = ce-ctrl.whse-mrkup
 qty     = tt-blk.

FIND FIRST reftable-fm NO-LOCK
     WHERE reftable-fm.reftable EQ "gsa-fm"
       AND reftable-fm.company  EQ xest.company
       AND reftable-fm.loc      EQ ""
       AND reftable-fm.code     EQ xest.est-no
     NO-ERROR.

 FIND FIRST cust WHERE
         cust.company EQ xeb.company AND
         cust.cust-no EQ xeb.cust-no
         NO-LOCK NO-ERROR.

IF AVAIL reftable-fm THEN
   gsa-fm = reftable-fm.val[1].
ELSE
   IF AVAIL cust AND cust.scomm NE 0 THEN
      gsa-fm = cust.scomm.
ELSE
   gsa-fm = ctrl[19].

/*output close.

hide frame kalk1 no-pause.
hide frame jobstd1 no-pause.
run cec/gsa.p (ROWID(probe), qty, 1).
  */   

RUN update-pcts .


assign
ctrl[9]  = gsa-mat / 100
ctrl[10] = gsa-lab / 100
ctrl[1]  = gsa-war / 100
ctrl[19] = gsa-fm / 100.

/*output to value(outfile1) append.*/
run cec/com/pr4-tots.p.
output close.

run cec/com/pr4-mis2-copy.p.

IF (v-avg-com AND NOT ll-tandem) OR
   (v-avg-tan AND ll-tandem)     THEN DO:
  assign
   v-mat = 0  
   v-lab = 0
   v-foh = 0
   v-voh = 0.

  for each xjob:
    assign
     v-mat = v-mat + xjob.mat
     v-lab = v-lab + xjob.lab
     v-foh = v-foh + xjob.foh
     v-voh = v-voh + xjob.voh.
  end.

  for each blk,    
      first xjob
      where xjob.i-no     eq blk.id
        and xjob.form-no  eq blk.snum
        and xjob.blank-no eq blk.bnum:

    assign
     blk.fact = fac-tot * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     blk.cost = tt-tot  * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.mat = v-mat   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.lab = v-lab   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.foh = v-foh   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk)
     xjob.voh = v-voh   * ((if blk.yr$ then blk.qyld else blk.qreq) / tt-blk).
  end.
end.

ASSIGN
   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999"
   ls-outfile = tmp-dir + TRIM(xest.est-no) + ".p" + string(probe.line,v-probe-fmt).

if vprint then do:
  run cec/com/probemk-copy.p (ROWID(probe)).
  
  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else
    dos silent type value(outfile2) >> value(outfile3).

  if search(outfile1) <> ? then 
    dos silent  type value(outfile3) > value(ls-outfile).

  RUN cec/probeu3.p (ROWID(probe)).

END.  

END.


/******************procedure*********************/

PROCEDURE calc-count :

    ttGsaFile2.ld-gsa-war-cnt = DEC(ttGsaFile2.ld-gsa-war-u-c) * DEC(ttGsaFile2.ld-gsa-war-u-p).
 
END PROCEDURE.

PROCEDURE calc-pallets :

  
    ttGsaFile2.ld-gsa-war-pal  = DEC(ttGsaFile2.ld-gsa-war-uni) /
                                         DEC(ttGsaFile2.ld-gsa-war-u-p).
  
END PROCEDURE.


PROCEDURE update-pcts :

 ASSIGN
   gsa-mat  = prmMaterial
   gsa-lab  = prmLabor
   gsa-war  = prmWHMark
   gsa-fm   = prmBrComm
   ctrl2[1] = prmTotCharge .

  DO TRANSACTION:
    FIND CURRENT calcpcts EXCLUSIVE-LOCK.
    calcpcts.val[1] = prmBoard.
    FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  END.

END PROCEDURE.

PROCEDURE calc-war-tot :

  DEF VAR li-qty AS INT NO-UNDO.
  DEF VAR ld-tot AS DEC NO-UNDO.

  ASSIGN
     li-qty = INT(ttGsaFile2.ld-gsa-war-pal)
     ld-tot = li-qty * DEC(ttGsaFile2.ld-gsa-war-hdl)
     li-qty = li-qty - INT(ttGsaFile2.ld-gsa-war-per).

    DO WHILE li-qty GT 0 AND INT(ttGsaFile2.ld-gsa-war-per) GT 0:
      ASSIGN
       ld-tot = ld-tot + (li-qty * DEC(ttGsaFile2.ld-gsa-war-amt))
       li-qty = li-qty - INT(ttGsaFile2.ld-gsa-war-per).
    END.
        
    IF cewhschg-cha EQ "$/MSF" THEN ld-tot = ld-tot * ld-msf-per.

    
    ttGsaFile2.ld-gsa-war-tot = (ROUND(ld-tot,2)).
  

END PROCEDURE.


PROCEDURE update-probe :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes:       
------------------------------------------------------------------------------*/

  FIND CURRENT probe EXCLUSIVE-LOCK.

  IF AVAIL probe THEN DO:
    ASSIGN
     probe.gsa-mat      = prmMaterial
     probe.gsa-lab      = prmLabor
     probe.gsa-war      = prmWHMark
     probe.gsa-war-amt  = prmCostPerPall
     probe.gsa-war-hdl  = prmPaHandCh
     probe.gsa-war-tot  = prmTotCharge
     probe.gsa-war-u-c  = prmUnCount
     probe.gsa-war-cnt  = prmPallCount
     probe.gsa-war-uni  = prmTotUnit
     probe.gsa-war-pal  = prmTotPall
     probe.gsa-war-per  = prmPallDelMon
     probe.gsa-war-u-p  = prmUnitprPal .

    
FIND FIRST probe-ref
        WHERE probe-ref.reftable EQ "probe-ref"
          AND probe-ref.company  EQ probe.company
          AND probe-ref.loc      EQ ""
          AND probe-ref.code     EQ probe.est-no
          AND probe-ref.code2    EQ STRING(probe.line,"9999999999")
        NO-ERROR.
    IF NOT AVAIL probe-ref THEN DO:
      CREATE probe-ref.
      ASSIGN
       probe-ref.reftable = "probe-ref"
       probe-ref.company  = probe.company
       probe-ref.loc      = ""
       probe-ref.code     = probe.est-no
       probe-ref.code2    = STRING(probe.line,"9999999999").
    END.

    probe-ref.val[1] = prmBoard.

    FIND FIRST probe-fm
         WHERE probe-fm.reftable EQ "gsa-fm"
           AND probe-fm.company  EQ probe.company
           AND probe-fm.loc      EQ ""
           AND probe-fm.code     EQ probe.est-no
         NO-ERROR.

    IF NOT AVAIL probe-fm THEN
    DO:
       CREATE probe-fm.
       ASSIGN probe-fm.reftable = "gsa-fm"
              probe-fm.company  = probe.company
              probe-fm.loc      = ""
              probe-fm.code     = probe.est-no.
    END.
    probe-fm.val[1] = prmBrComm .
  END.

END PROCEDURE.



PROCEDURE InsertQty:
    DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
    DEF BUFFER b-reft FOR reftable.
    /*DEF BUFFER op-lock FOR reftable.*/    

    CREATE tt-qtty.
    ASSIGN     
         do-speed        = LOGICAL(prmDoSpeed)
         do-mr           = LOGICAL(prmDoMr)
         do-gsa          = LOGICAL(prmDoGsa)
         v-drop-rc       = LOGICAL(prmDropRc)
         v-do-all-forms-ink   = NO 
         v-match-up      = 0
         io-do-speed     = LOGICAL(prmDoSpeed) 
         io-do-mr        = LOGICAL(prmDoMr)
         io-v-match-up   = 0 

         tt-qtty.qtty[1]  = prmGetqty 
         /*tt-qtty.qtty[2]  = prmQty2 
         tt-qtty.qtty[3]  = prmQty3  
         tt-qtty.qtty[4]  = prmQty4 
         tt-qtty.qtty[5]  = prmQty5  
         tt-qtty.qtty[6]  = prmQty6    
         tt-qtty.qtty[7]  = prmQty7 
         tt-qtty.qtty[8]  = prmQty8  
         tt-qtty.qtty[9]  = prmQty9    
         tt-qtty.qtty[10] = prmQty10    
         tt-qtty.qtty[11] = prmQty11    
         tt-qtty.qtty[12] = prmQty12    
         tt-qtty.qtty[13] = prmQty13    
         tt-qtty.qtty[14] = prmQty14    
         tt-qtty.qtty[15] = prmQty15    
         tt-qtty.qtty[16] = prmQty16    
         tt-qtty.qtty[17] = prmQty17    
         tt-qtty.qtty[18] = prmQty18    
         tt-qtty.qtty[19] = prmQty19    
         tt-qtty.qtty[20] = prmQty20    
         tt-qtty.qtty[21] = prmQty21    
         tt-qtty.qtty[22] = prmQty22    
         tt-qtty.qtty[23] = prmQty23    
         tt-qtty.qtty[24] = prmQty24    
         tt-qtty.qtty[25] = prmQty25    
         tt-qtty.qtty[26] = prmQty26    
         tt-qtty.qtty[27] = prmQty27    
         tt-qtty.qtty[28] = prmQty28    
         tt-qtty.rel[1]  = prmRels1   
         tt-qtty.rel[2]  = prmRels2   
         tt-qtty.rel[3]  = prmRels3   
         tt-qtty.rel[4]  = prmRels4   
         tt-qtty.rel[5]  = prmRels5   
         tt-qtty.rel[6]  = prmRels6   
         tt-qtty.rel[7]  = prmRels7   
         tt-qtty.rel[8]  = prmRels8   
         tt-qtty.rel[9]  = prmRels9   
         tt-qtty.rel[10] = prmRels10  
         tt-qtty.rel[11] = prmRels11  
         tt-qtty.rel[12] = prmRels12  
         tt-qtty.rel[13] = prmRels13  
         tt-qtty.rel[14] = prmRels14  
         tt-qtty.rel[15] = prmRels15  
         tt-qtty.rel[16] = prmRels16  
         tt-qtty.rel[17] = prmRels17  
         tt-qtty.rel[18] = prmRels18  
         tt-qtty.rel[19] = prmRels19  
         tt-qtty.rel[20] = prmRels20  
         tt-qtty.rel[21] = prmRels21  
         tt-qtty.rel[22] = prmRels22  
         tt-qtty.rel[23] = prmRels23 
         tt-qtty.rel[24] = prmRels24  
         tt-qtty.rel[25] = prmRels25  
         tt-qtty.rel[26] = prmRels26  
         tt-qtty.rel[27] = prmRels27  
         tt-qtty.rel[28] = prmRels28*/
         .
    
  /*DO i = 1 TO NUM-ENTRIES(prmEstList):
    ASSIGN
     lv-est-no = ENTRY(i,prmEstList)
     lv-est-no = FILL(" ",8 - LENGTH(TRIM(lv-est-no))) + TRIM(lv-est-no).

    FIND FIRST est
        WHERE est.company    EQ xest.company
          AND est.loc        EQ xest.loc
          AND est.est-no     EQ lv-est-no
          AND ((est.est-type LT 5 AND xest.est-type LT 5) OR
               (est.est-type GE 5 AND xest.est-type GE 5))
        NO-LOCK NO-ERROR.

    IF AVAIL est THEN DO:
      FIND FIRST w-est WHERE w-est-no EQ lv-est-no NO-ERROR.
      IF NOT AVAIL w-est THEN DO:
        CREATE w-est.
        ASSIGN
         w-est-no = est.est-no
         w-row-id = ROWID(est).
      END.
    END.
  END.*/

  FOR EACH w-est,
      EACH b-reft
      WHERE b-reft.reftable EQ "est/getqty.w"
        AND b-reft.company  EQ xest.company
        AND b-reft.loc      EQ xest.loc
        AND b-reft.code     EQ w-est-no
      NO-LOCK:

    FIND reftable WHERE ROWID(reftable) EQ ROWID(b-reft) EXCLUSIVE NO-WAIT NO-ERROR.
    IF AVAIL reftable THEN DELETE reftable.
    ELSE DO:
        ASSIGN 
            cError = "Estimate Record is being changed by someone else, wait a moment and try again...".
            RETURN.     
    END.
  END.

  FIND FIRST reftable
      WHERE reftable.reftable EQ "est/getqty.w"
        AND reftable.code2    EQ STRING(li-seq,"9999999999")
      USE-INDEX code2 NO-LOCK NO-ERROR.

  IF AVAIL reftable OR li-seq EQ 0 THEN DO:
    FIND LAST reftable WHERE reftable.reftable EQ "est/getqty.w"
        USE-INDEX code2 NO-LOCK NO-ERROR.
    li-seq = (IF AVAIL reftable THEN INT(reftable.code2) ELSE 0) + 1.
  END.

  FOR EACH w-est,
      FIRST est WHERE ROWID(est) EQ w-row-id NO-LOCK:

    CREATE reftable.
    ASSIGN
     reftable.reftable = "est/getqty.w"
     reftable.company  = est.company
     reftable.loc      = est.loc
     reftable.code     = est.est-no
     reftable.code2    = STRING(li-seq,"9999999999").
  END.

  FIND FIRST reftable
      WHERE reftable.reftable EQ "est/getqty.w2"
        AND reftable.company  EQ xest.company
        AND reftable.loc      EQ ""
        AND reftable.code     EQ xest.est-no
      NO-ERROR.

  IF NOT AVAIL reftable THEN DO:
    CREATE reftable.
    ASSIGN
     reftable.reftable = "est/getqty.w2"
     reftable.company  = xest.company
     reftable.loc      = ""
     reftable.code     = xest.est-no.
  END.
  reftable.val[1] = io-v-match-up.

  {est/op-lock.i xest}

  ASSIGN
   op-lock.val[1] = INT(io-do-speed)
   op-lock.val[2] = INT(io-do-mr).
   

END.


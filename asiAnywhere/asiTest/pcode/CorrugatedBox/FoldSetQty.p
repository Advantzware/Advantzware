


/*------------------------------------------------------------------------
    File        : FoldSetQty.p
    Purpose     : Get Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

        DEFINE TEMP-TABLE ttFoldSetvarable NO-UNDO
       
        FIELD vFoldSeqno            AS INT    
        FIELD vFoldQty              AS INT 
        FIELD vFoldGetqty           AS INT  
        FIELD vFoldRoid             AS CHAR 
        FIELD FoldSet               AS CHAR 
        FIELD GsaMat            AS DECIMAL
        FIELD GsaLab            AS DECIMAL
        FIELD GsaWar            AS DECIMAL
        FIELD GsaBrd            AS DECIMAL 
        FIELD GsaMonth          AS INT    .


    DEFINE DATASET dsFoldSetQty FOR ttFoldSetvarable .
    DEFINE INPUT PARAMETER prmUser       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmAction     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty1       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty2       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty3       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty4       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty5       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty6       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty7       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty8       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty9       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty10      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty11      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty12      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty13      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty14      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty15      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty16      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty17      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty18      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty19      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty20      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty21      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty22      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty23      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty24      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty25      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty26      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty27      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmQty28      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels1      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels2      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels3      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels4      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels5      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels6      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels7      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels8      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels9      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels10     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels11     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels12     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels13     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels14     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels15     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels16     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels17     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels18     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels19     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels20     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels21     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels22     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels23     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels24     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels25     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels26     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels27     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmRels28     AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoGsa      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoMr       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoSpeed    AS CHARACTER NO-UNDO.
       
    DEFINE INPUT PARAMETER prmEstList    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmEstimate   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmForm       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBlank      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmLvoverride   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmVendor    AS CHARACTER NO-UNDO.

    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldSetQty.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.
    
    IF prmUser   = ?  THEN ASSIGN prmUser = "".
    IF prmAction = ?  THEN ASSIGN prmAction = "".
    IF prmQty1   = ?  THEN ASSIGN prmQty1  = 0.
    IF prmQty2   = ?  THEN ASSIGN prmQty2  = 0.
    IF prmQty3   = ?  THEN ASSIGN prmQty3  = 0.
    IF prmQty4   = ?  THEN ASSIGN prmQty4  = 0.
    IF prmQty5   = ?  THEN ASSIGN prmQty5  = 0.
    IF prmQty6   = ?  THEN ASSIGN prmQty6  = 0.
    IF prmQty7   = ?  THEN ASSIGN prmQty7  = 0.
    IF prmQty8   = ?  THEN ASSIGN prmQty8  = 0.
    IF prmQty9   = ?  THEN ASSIGN prmQty9  = 0.
    IF prmQty10  = ?  THEN ASSIGN prmQty10 = 0.
    IF prmQty11  = ?  THEN ASSIGN prmQty11 = 0.
    IF prmQty12  = ?  THEN ASSIGN prmQty12 = 0.
    IF prmQty13  = ?  THEN ASSIGN prmQty13 = 0.
    IF prmQty14  = ?  THEN ASSIGN prmQty14 = 0.
    IF prmQty15  = ?  THEN ASSIGN prmQty15 = 0.
    IF prmQty16  = ?  THEN ASSIGN prmQty16 = 0.
    IF prmQty17  = ?  THEN ASSIGN prmQty17 = 0.
    IF prmQty18  = ?  THEN ASSIGN prmQty18 = 0.
    IF prmQty19  = ?  THEN ASSIGN prmQty19 = 0.
    IF prmQty20  = ?  THEN ASSIGN prmQty20 = 0.
    IF prmQty21  = ?  THEN ASSIGN prmQty21 = 0.
    IF prmQty22  = ?  THEN ASSIGN prmQty22 = 0.
    IF prmQty23  = ?  THEN ASSIGN prmQty23 = 0.
    IF prmQty24  = ?  THEN ASSIGN prmQty24 = 0.
    IF prmQty25  = ?  THEN ASSIGN prmQty25 = 0.
    IF prmQty26  = ?  THEN ASSIGN prmQty26 = 0.
    IF prmQty27  = ?  THEN ASSIGN prmQty27 = 0.
    IF prmQty28  = ?  THEN ASSIGN prmQty28 = 0. 
    IF prmRels1   = ?  THEN ASSIGN prmRels1  = 0.
    IF prmRels2   = ?  THEN ASSIGN prmRels2  = 0.
    IF prmRels3   = ?  THEN ASSIGN prmRels3  = 0.
    IF prmRels4   = ?  THEN ASSIGN prmRels4  = 0.
    IF prmRels5   = ?  THEN ASSIGN prmRels5  = 0.
    IF prmRels6   = ?  THEN ASSIGN prmRels6  = 0.
    IF prmRels7   = ?  THEN ASSIGN prmRels7  = 0.
    IF prmRels8   = ?  THEN ASSIGN prmRels8  = 0.
    IF prmRels9   = ?  THEN ASSIGN prmRels9  = 0.
    IF prmRels10  = ?  THEN ASSIGN prmRels10 = 0.
    IF prmRels11  = ?  THEN ASSIGN prmRels11 = 0.
    IF prmRels12  = ?  THEN ASSIGN prmRels12 = 0.
    IF prmRels13  = ?  THEN ASSIGN prmRels13 = 0.
    IF prmRels14  = ?  THEN ASSIGN prmRels14 = 0.
    IF prmRels15  = ?  THEN ASSIGN prmRels15 = 0.
    IF prmRels16  = ?  THEN ASSIGN prmRels16 = 0.
    IF prmRels17  = ?  THEN ASSIGN prmRels17 = 0.
    IF prmRels18  = ?  THEN ASSIGN prmRels18 = 0.
    IF prmRels19  = ?  THEN ASSIGN prmRels19 = 0.
    IF prmRels20  = ?  THEN ASSIGN prmRels20 = 0.
    IF prmRels21  = ?  THEN ASSIGN prmRels21 = 0.
    IF prmRels22  = ?  THEN ASSIGN prmRels22 = 0.
    IF prmRels23  = ?  THEN ASSIGN prmRels23 = 0.
    IF prmRels24  = ?  THEN ASSIGN prmRels24 = 0.
    IF prmRels25  = ?  THEN ASSIGN prmRels25 = 0.
    IF prmRels26  = ?  THEN ASSIGN prmRels26 = 0.
    IF prmRels27  = ?  THEN ASSIGN prmRels27 = 0.
    IF prmRels28  = ?  THEN ASSIGN prmRels28 = 0. 
    IF prmDoGsa = ?    THEN ASSIGN prmDoGsa = "".
    IF prmDoMr = ?     THEN ASSIGN prmDoMr = "".
    IF prmDoSpeed = ?  THEN ASSIGN prmDoSpeed = "".
    IF prmEstList = ?   THEN ASSIGN prmEstList = "".

    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".
    IF prmForm = ?  THEN ASSIGN prmForm = 0.
    IF prmBlank = ?  THEN ASSIGN prmBlank = 0.
    IF prmLvoverride = ? THEN ASSIGN prmLvoverride = "Yes".
    IF prmVendor     = ? THEN ASSIGN prmVendor      = "".
    
DEF NEW SHARED VAR sh-gsa-mat       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-lab       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-war       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-fm        AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-brd       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-where   AS CHAR NO-UNDO.
ASSIGN
  sh-gsa-where = "Main" .
       

/* Local Variable Definitions ---                                       */
def VAR io-do-speed as log no-undo.
def VAR io-do-mr as log no-undo.
def VAR io-do-gsa as log no-undo.
def VAR io-v-drop-rc as log no-undo.
def VAR io-v-match-up as dec no-undo.
def VAR io-ink-all-forms AS LOG NO-UNDO.
def var v-brd-only like sys-ctrl.log-fld init no no-undo.
def var v-brd-cost as dec no-undo.
def var summary-rpt as log format "SUM/DET" init no.
def new shared var tmp-dir as cha no-undo.
def var ls-outfile as cha no-undo.

/*def input parameter lv-mclean as log no-undo.
def output parameter op-error as log no-undo.
*/

    
{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}
{sys/inc/var.i "new shared"}


def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

def var v-layout as log NO-UNDO.

def var v-msf as dec no-undo.
def var v-dec as dec NO-UNDO.
def var v-skip-pct as log NO-UNDO.

def var lv-brd-l           like eb.len no-undo.
def var lv-brd-w           like lv-brd-l no-undo.
def var lv-brd-sq          as dec format ">>>>9.9<<<<" no-undo.
def var lv-brd-sf          as dec format ">>>>>9.9<<"  no-undo.
def var lv-brd-wu          like lv-brd-sq no-undo.

DEF VAR ld-metric AS DEC INIT 1 NO-UNDO.
DEF VAR lv-format AS CHAR INIT ">>>>9.9<<<<" NO-UNDO.
DEF VAR ld-wid AS DEC NO-UNDO.
DEF VAR ld-len AS DEC NO-UNDO.
DEF VAR ld-dep AS DEC NO-UNDO.
DEF VAR ld-fg-rate AS DEC NO-UNDO.

DEF BUFFER bf-est FOR est.
DEF BUFFER bf-probe FOR probe.
DEF BUFFER reftable-fm FOR reftable.
DEF BUFFER reftable-fold-pct FOR reftable.
DEF BUFFER b-item FOR ITEM.

    def var v-part-no like xeb.part-no.
    def var v-part-d1 like xeb.part-dscr1.
    def var v-part-d2 like xeb.part-dscr2.
    DEF VAR v-yld AS DEC NO-UNDO.
    def var v-dest-cd like xeb.dest-code.
    def var v-carrier like xeb.carrier.
    DEF VAR li-blk AS INT NO-UNDO.

DEF VAR v-t-win AS DEC DECIMALS 4 NO-UNDO.
  def var fil_id as recid no-undo.
 
def var lv-ef-recid as recid no-undo.
def new shared buffer xop for est-op.

def var call_id as recid no-undo.
def var v-vend-no   like e-item-vend.vend-no init "".
DEF var v-vend-list AS CHAR NO-UNDO.
def var lv-error as log no-undo.

def new shared var v-do-gsa like do-gsa no-undo.
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



def new shared temp-table tt-qtty field qtty like qtty
                                  field rel like rels.
def TEMP-TABLE q-sort no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort1 no-undo field qty as dec field rel as int.
def TEMP-TABLE q-sort2 no-undo field qty as dec field rel as int.


def workfile w-est field w-est-no like est.est-no
                   field w-row-id as   rowid.

DEF TEMP-TABLE tt-ei NO-UNDO
    FIELD run-qty AS DECIMAL DECIMALS 3 EXTENT 20
    FIELD run-cost AS DECIMAL DECIMALS 4 EXTENT 20.

DEF BUFFER b-cost FOR reftable.
DEF BUFFER b-qty FOR reftable.

/*def var i as int no-undo.*/
def var li-seq as int no-undo.
/*def shared var cocode as cha no-undo.*/   

DEF VAR prmComp AS CHAR NO-UNDO.
DEFINE VAR v AS INT NO-UNDO.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   
assign
 cocode = prmComp
 locode = usercomp.loc
 . 

ASSIGN 
    locode = "MAIN".


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


DEF VAR v-line LIKE probe.line no-undo.
  DEF VAR v-yld-qty AS DEC FORMAT ">>>,>>>" NO-UNDO.
  DEF VAR v-hdr-depth AS CHAR FORMAT "x(5)" NO-UNDO.
  DEF VAR v-n-out AS INT NO-UNDO.
  DEF VAR ll-use-defaults AS LOG NO-UNDO.
 
  DEF BUFFER reftable-broker-pct FOR reftable.
  DEF BUFFER b-est-qty-2 FOR est-qty.
  DEF VAR v-count-2 AS INT NO-UNDO.

DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.

def var v-gsa as log init no no-undo.
def var v-bqty as int no-undo.
def var v-module as char format "x(60)" no-undo.


FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
FIND FIRST ef WHERE ef.est-no = est.est-no AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
FIND FIRST eb WHERE eb.est-no = est.est-no AND eb.company = prmComp AND eb.form-no = prmForm AND eb.blank-no = prmBlank NO-LOCK NO-ERROR. 


find xest where recid(xest) = recid(est) EXCLUSIVE-LOCK NO-ERROR.
find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

vprint = YES.

lv-ef-recid = recid(xef).




IF prmAction = "doCalc" THEN DO:
   
IF xest.metric THEN
  ASSIGN
   ld-metric = 25.4
   lv-format = "->>,>>>mm".

/*{cec/get-vend.i}  /* get vendor number */*/
ASSIGN
       v-vend-no  = prmVendor .

IF v-vend-no EQ "&nbsp;" OR v-vend-no EQ ? THEN DO:
    ASSIGN
        v-vend-no = "".
  END.

find first ce-ctrl WHERE (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode) no-lock no-error.
assign
 qtty     = 0
 ctrl[1]  = ce-ctrl.whse-mrkup / 100
 ctrl[2]  = ce-ctrl.hand-pct / 100
 ctrl[3]  = ce-ctrl.rm-rate
 ctrl[4]  = ce-ctrl.spec-%[1]
 ctrl[5]  = integer(ce-ctrl.comm-add)
 ctrl[6]  = integer(ce-ctrl.shp-add)
 ctrl[7]  = integer(ce-ctrl.sho-labor)
 ctrl[8]  = integer(ce-ctrl.trunc-99)
 ctrl[11] = ce-ctrl.spec-%[2]
 ctrl[12] = ce-ctrl.spec-%[3]
 ctrl[13] = integer(ce-ctrl.spec-add[1])
 ctrl[14] = integer(ce-ctrl.spec-add[2])
 ctrl[15] = integer(ce-ctrl.spec-add[3])
 ctrl[16] = integer(ce-ctrl.spec-add[6])
 ctrl[17] = integer(ce-ctrl.spec-add[7])
 ctrl[18] = integer(ce-ctrl.spec-add[8])
 ctrl2    = 0.

FIND FIRST reftable
     WHERE reftable.reftable EQ "ce-ctrl.fold-pct"
       AND reftable.company  EQ ce-ctrl.company
       AND reftable.loc      EQ ce-ctrl.loc
     NO-LOCK NO-ERROR.

IF AVAIL reftable THEN
   ctrl[19] = reftable.val[1].

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

find first xef where xef.company = xest.company 
                 AND xef.est-no eq xest.est-no.
find first xeb where xeb.company = xest.company AND xeb.est-no eq xest.est-no.
find first xop where xop.company = xest.company 
                 AND xop.est-no    eq xest.est-no
                 and xop.op-speed eq 0
                 no-lock no-error.

save-lock = xef.op-lock.
pause 0.

DO TRANSACTION:
  {est/recalc-mr.i xest}
  FIND CURRENT recalc-mr NO-LOCK.

  ASSIGN
   do-speed = xest.recalc
   do-mr    = recalc-mr.val[1] EQ 1
   do-gsa   = xest.override.

  {sys/inc/cerun.i F}
  vmclean = LOOKUP(cerunf,"McLean,HOP") gt 0.

  {ce/msfcalc.i}
END.

summary-rpt = vmclean.

IF vprint THEN DO:
  /*DO i = 1 TO 4:
    IF xest.est-qty[i] NE 0 THEN qtty[i] = xest.est-qty[i].
  END.*/

  FIND FIRST est-qty
      WHERE est-qty.company EQ xest.company
        AND est-qty.est-no  EQ xest.est-no
      NO-LOCK NO-ERROR.
  IF AVAIL est-qty THEN DO i = 1 TO 20:
    IF est-qty.qty[i] NE 0 THEN
      ASSIGN
       qtty[i + 4] = est-qty.qty[i]
       rels[i + 4] = est-qty.qty[i + 20].
  END.

  {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}

  FIND FIRST tt-qtty NO-ERROR.
  IF AVAIL tt-qtty THEN DELETE tt-qtty.
  CREATE tt-qtty.

  DO i = 1 TO 28:
    ASSIGN
     tt-qtty.qtty[i] = qtty[i]
     tt-qtty.rel[i]  = IF qtty[i] EQ 0 THEN 0
                       ELSE
                       IF rels[i] EQ 0 THEN 1 ELSE rels[i].
  END.

  v-do-all-forms-ink = NO.

 /* RUN est/getqty.w (INPUT-OUTPUT do-speed, INPUT-OUTPUT do-mr, INPUT-OUTPUT do-gsa, INPUT-OUTPUT v-drop-rc, INPUT-OUTPUT v-match-up,
                    INPUT-OUTPUT v-do-all-forms-ink, INPUT NO, OUTPUT lv-error). */
  RUN InsertQty. 

  IF lv-error THEN RETURN ERROR.

   IF prmLvoverride = "Yes"  THEN
  FOR EACH probe
      WHERE probe.company EQ xest.company
        AND probe.est-no  EQ xest.est-no:
    DELETE probe.                 
  END.
  
  DO i = 1 to 28:
        qtty[i] = tt-qtty.qtty[i].
        rels[i] = tt-qtty.rel[i].
  end.
  {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}
  do i = 1 to 28:
        if qtty[i] eq 0 then rels[i] = 0.
        else if rels[i] eq 0 then rels[i] = 1.
  end.
end.
else qtty[1] = qty.

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
END.

session:set-wait-state("General").

form day_str v-module tim_str to 79  skip(2)
     with frame hdr page-top width 80 no-labels no-box stream-io.

FORM "Set Est#" xest.est-no FORMAT "x(8)"
     "SlsRep:" kli.sname
     "UserID:" xest.updated-id
     "Prober:" probe.probe-user
     SKIP
     "Cust:" kli.cust-no
             kli.cust-add[1] FORMAT "x(29)" TO 44
     "Ship:" kli.ship-add[1] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[2] FORMAT "x(29)" TO 44
             kli.ship-add[2] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[3] FORMAT "x(29)" TO 44
             kli.ship-add[3] FORMAT "x(29)" TO 80 SKIP
             kli.cust-add[4] FORMAT "x(29)" TO 44
             kli.ship-add[4] FORMAT "x(29)" TO 80
             SKIP(1)

   "Finished Good:"
   v-part-no
   v-part-d1  SKIP
   v-part-d2  AT 32
   SKIP(1)

  WITH NO-LABELS NO-BOX DOWN WIDTH 80 STREAM-IO FRAME kli.

if retry then output close.

find first xeb
    where xeb.company  eq xest.company
      and xeb.est-no   eq xest.est-no
      and xeb.form-no  eq 0
      and xeb.blank-no eq 0
    no-lock no-error.
assign
 v-part-no = if avail xeb then xeb.part-no else ""
 v-part-d1 = if avail xeb then xeb.part-dscr1 else ""
 v-part-d2 = if avail xeb then xeb.part-dscr2 else "". 

for each xjob:
  delete xjob.
end.

do vmcl = 1 to 28:
  if qtty[vmcl] eq 0 then next.

  assign
   t-shtfrm   = 0
   op-tot     = 0
   v-brd-cost = 0
   qty        = qtty[vmcl]
   t-blksht   = 0
   t-blkqty   = 0.

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

  vbsf = 0.

  do transaction:
    for each est-op WHERE est-op.company = xest.company 
                      AND est-op.est-no eq xest.est-no
                      and est-op.line  gt 500:
        delete est-op.
    end.
    for each est-op WHERE est-op.company = xest.company 
                      AND est-op.est-no eq xest.est-no
                      and est-op.line  lt 500:
        create xop.
        buffer-copy est-op to xop
        assign
           xop.line = est-op.line + 500.
    end.
  end.

  {est/probeset.i qtty[vmcl] v-match-up}

  IF probe.LINE LT 100 THEN
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(probe.line,"99")
            outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".a" + string(probe.line,"99")
            outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".s" + string(probe.line,"99")
            ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".p" + string(probe.line,"99").
  ELSE
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(probe.line,"999")
            outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".a" + string(probe.line,"999")
            outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".s" + string(probe.line,"999")
            ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".p" + string(probe.line,"999").

  output to value(outfile1).

for each xef NO-LOCK
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no:
    xxx = 0.
   
    for each xeb NO-LOCK
        where xeb.company eq xef.company
          and xeb.est-no  eq xef.est-no 
          and xeb.form-no eq xef.form-no:
   
       find first kli where
            kli.cust-no = xeb.cust-no
            no-error.
   
       if not avail kli then do:
          find first sman where
               sman.company EQ cocode AND
               sman.sman    EQ xeb.sman
               no-lock no-error.
   
          find first cust where 
               cust.company = cocode and
               cust.cust-no = xeb.cust-no
               no-lock no-error.
   
          find first shipto where
               shipto.company EQ cocode AND
               shipto.cust-no = cust.cust-no and
               shipto.ship-no = xeb.ship-no
               no-lock no-error.
   
          create kli.
          if avail sman then
             assign
                kli.sman    = sman.sman
                kli.sname   = sman.sname.
   
          if xeb.cust-no ne "Temp" then
             assign
                kli.cust-no = xeb.cust-no
                kli.cust-add[1] = cust.name
                kli.cust-add[2] = cust.addr[1]
                kli.cust-add[3] = cust.addr[2]
                kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
          else
             assign
                kli.cust-no = xeb.cust-no
                kli.cust-add[1] = xeb.ship-name
                kli.cust-add[2] = xeb.ship-addr[1]
                kli.cust-add[3] = xeb.ship-addr[2]
                kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                                  xeb.ship-zip.
   
          if kli.cust-add[3] = "" then
             assign
                kli.cust-add[3] = kli.cust-add[4]
                kli.cust-add[4] = "".
   
          if kli.cust-add[2] = "" then
             assign
                kli.cust-add[2] = kli.cust-add[3]
                kli.cust-add[3] = kli.cust-add[4]
                kli.cust-add[4] = "".
   
          IF AVAIL shipto THEN
          DO:
             if shipto.ship-no = 1 then
                assign
                   kli.ship-add[1] = "SAME"
                   kli.ship-add[2] = "" kli.ship-add[2] = ""
                   kli.ship-add[4] = "".
             else
                assign
                   kli.ship-add[1] = shipto.ship-name
                   kli.ship-add[2] = shipto.ship-addr[1]
                   kli.ship-add[3] = shipto.ship-addr[2]
                   kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                     " " + shipto.ship-zip.
          END.
   
          if kli.ship-add[3] = "" then
             assign
                kli.ship-add[3] = kli.ship-add[4]
                kli.ship-add[4] = "".
          if kli.ship-add[2] = "" then
             assign
                kli.ship-add[2] = kli.ship-add[3]
                kli.ship-add[3] = kli.ship-add[4]
                kli.ship-add[4] = "".
       end.
   
       qty = qtty[vmcl].
       
       find first blk where
            blk.snum = xeb.form-no and
            blk.bnum = xeb.blank-no
            no-error.

       if not avail blk then do:
   
          create blk.
          assign
           blk.kli      = kli.cust-no
           blk.id       = xeb.part-no
           blk.snum     = xeb.form-no
           blk.bnum     = xeb.blank-no
           blk.qreq     = qty
           blk.qyld     = qty
           blk.yr$      = xeb.yrprice
           blk.stock-no = xeb.stock-no
           blk.pur-man  = xeb.pur-man.
       end.
       xxx = xxx + (xeb.t-sqin * xeb.num-up).
    end.
   
    for each xeb FIELDS(t-sqin num-up)
        where xeb.company eq xef.company
          and xeb.est-no  eq xef.est-no
          and xeb.form-no eq xef.form-no
        no-lock,
        first blk
        where blk.snum eq xeb.form-no
          and blk.bnum eq xeb.blank-no:
       blk.pct = (xeb.t-sqin * xeb.num-up) / xxx.
    end.
end.

/* print header */
ASSIGN
 day_str  = STRING(TODAY,"99/99/9999")
 tim_str  = STRING(TIME,"hh:mm am") 
 v-module = IF cerunf EQ "HOP" THEN "FCD-0101" ELSE ""
 v-module = FILL(" ",59 - LENGTH(TRIM(v-module))) + TRIM(v-module).

display day_str v-module tim_str with frame hdr.
for each kli with frame kli:
   display trim(xest.est-no) @ xest.est-no
           kli.sman
           xest.updated-id
           probe.probe-user
           kli.sname
           kli.cust-no
           kli.cust-add
           kli.ship-add
           v-part-no
           v-part-d1
           v-part-d2.
end.
for each xef
    where xef.company eq xest.company
      and xef.est-no  eq xest.est-no
with frame brd no-labels no-box width 80 stream-io down:
   /*if xef.lam-dscr = "R"
   then*/ assign brd-l[1] = xef.trim-l
               brd-w[1] = xef.trim-w.
   /*else assign brd-l[1] = xef.trim-w
               brd-w[1] = xef.trim-l.*/
   /* calc. sheet dimensions & weight */

  tt-blk = qtty[vmcl].

  RUN ce/box/prokalk2.p.

  if cerunf eq "HOP" then
    assign
     brd-l[2] = xef.nsh-len
     brd-w[2] = xef.nsh-wid.
  else
    assign
     brd-l[2] = xef.gsh-len
     brd-w[2] = xef.gsh-wid.

   if xef.roll = true THEN ASSIGN brd-l[3] = xef.gsh-len.
   if brd-l[2] = 0 and brd-w[2] = 0 then assign brd-l[2] = xef.lsh-len
                                                brd-w[2] = xef.lsh-wid .

   ASSIGN
      brd-w[3] = if xef.roll eq true then xef.roll-wid else 0
      brd-sq[1] = xef.trim-l * xef.trim-w
      brd-sq[2] = brd-l[2] * brd-w[2]
      brd-sq[3] = brd-l[3] * brd-w[3]
      brd-sf[1] = if v-corr then (brd-sq[1] * .007) else (brd-sq[1] / 144)
      brd-sf[2] = if v-corr then (brd-sq[2] * .007) else (brd-sq[2] / 144)
      brd-sf[3] = if v-corr then (brd-sq[3] * .007) else (brd-sq[3] / 144).

   find first item where (item.company = cocode) and item.i-no = xef.board no-lock no-error.
   if avail item then
      find first e-item of item no-lock no-error.

   ASSIGN
      brd-wu[1] = brd-sf[1]  * item.basis-w
      brd-wu[2] = brd-sf[2]  * item.basis-w
      brd-wu[3] = (brd-sf[3] * item.basis-w) / 2000
      lv-brd-sf = xef.gsh-len * xef.gsh-wid * xef.gsh-qty
      lv-brd-sf = (if v-corr then (lv-brd-sf * .007) else (lv-brd-sf / 144)) / 1000 /*tot msf*/.
   
   if avail item then lv-brd-wu = (lv-brd-sf * item.basis-w) / 2000.

   ASSIGN
      zzz = 0
      t-blksht[xef.form-no] = 0.

   for each xeb NO-LOCK
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no 
         and xeb.form-no eq xef.form-no:

      ASSIGN
         v-yld = IF xeb.cust-% lt 0 then -1 / xeb.cust-% else xeb.cust-%
         /* set total # of blanks on all forms */
         tt-blk = qtty[vmcl].

      /* set total # of blanks on this form */
      if xeb.blank-no = 1 then
         t-blksht[xef.form-no] = t-blksht[xef.form-no] + xeb.num-up.

      /* set total qty of all blanks for this form */
      assign
       t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] + (qtty[vmcl] * v-yld)
       brd-l[4]  = xeb.t-len
       brd-w[4]  = xeb.t-wid
       brd-sq[4] = brd-l[4] * brd-w[4]
       brd-sf[4] = if v-corr then (brd-sq[4] * .007) else (brd-sq[4] / 144)
       brd-wu[4] = brd-sf[4] * item.basis-w

      /* find sheet qty needed for this form (without spoil)*/
      zzz = qtty[vmcl] * v-yld / (xeb.num-up * xef.n-out * xef.n-out-l).
      /*if xest.form-qty = 1 then zzz = zzz * 2.*/
      {sys/inc/roundup.i zzz}
      IF zzz GT t-shtfrm[xeb.form-no] THEN
         t-shtfrm[xeb.form-no] = zzz.

      ASSIGN
         call_id = recid(xeb)
         vbsf = vbsf + if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144).
   end.

   find xeb where recid(xeb) = call_id no-lock no-error.

   ASSIGN
      qty = qtty[vmcl]
      tmpstore = if xest.form-qty eq 1 then "BOTTOM/LID"
                 else
                 if xest.form-qty eq 2 then
                    if xef.form-no = 1 then "BOTTOM" else "LID"
                    else
                       "FORM " + trim(string(xef.form-no,">9")).

  if summary-rpt then
   put skip
       "         --- Qty --- --- Description ---- -- Size/Color ---" skip.

  else
   display skip
   tmpstore format "x(12)"
   " Width   Length   Sq.Inches  Sq.Feet/Sheet    Weight per Units" skip
   "Blank  Size:" brd-w[4] to 21
                  brd-l[4] to 30
                  brd-sq[4] to 42
                  brd-sf[4] to 52 "Sf/Blk"
                  brd-wu[4] to 70 space(0) "/M Blks" skip
   "Feed  Sheet:" brd-w[1] to 21 brd-l[1] to 30 brd-sq[1] to 42
            brd-sf[1] to 52 "Sf/Sht"
            brd-wu[1] to 70 space(0) "/M Shts" skip
   "Gross Sheet:" brd-w[2] to 21 brd-l[2] to 30 brd-sq[2] to 42
            brd-sf[2] to 52 "Sf/Sht"  brd-wu[2] to 70 space(0) "/M Shts" skip
   "Roll  Size :" /*brd-l[3]  to 30  when brd-l[3] ne 0*/
                  brd-w[3]  to 21  when brd-w[3] ne 0
            /*    brd-sq[3] to 42  when brd-w[3] ne 0
                  brd-wu[3] to 70  when brd-w[3] ne 0
                  "Tons"           when brd-w[3] ne 0 */
    lv-brd-sf TO 52 "MSF"
    lv-brd-wu TO 70 "Tons"  skip(1)
"- # UP - --- Qty --- --- Description ---- -- Size/Color --- --- Style/Part # --".

   for each xeb NO-LOCK
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no 
         and xeb.form-no eq xef.form-no
       with frame blk no-box no-labels width 80 stream-io down:
      find first style  where  style.company = cocode and
                                 style.style = xeb.style no-lock no-error.

      ASSIGN
       ld-len = xeb.len * ld-metric
       ld-wid = xeb.wid * ld-metric
       ld-dep = xeb.dep * ld-metric.

      IF ld-metric NE 1 THEN DO:
        {sys/inc/roundup.i ld-len}
        {sys/inc/roundup.i ld-wid}
        {sys/inc/roundup.i ld-dep}
      END.

      ASSIGN
       sizcol[1]  = TRIM(STRING(ld-len,lv-format)) + "x" +
                    TRIM(STRING(ld-wid,lv-format)) + "x" +
                    TRIM(STRING(ld-dep,lv-format))
       sizcol[2]  = xeb.i-coldscr
       stypart[1] = style.dscr
       stypart[2] = xeb.part-no
       dsc[1]     = xeb.part-dscr1
       dsc[2]     = xeb.part-dscr2.

      if summary-rpt then do:
        stypart = "".
        put tmpstore format "x(7)".
      end.

      else
        /*if xest.form-qty > 1
        then*/ put space(4) string(xeb.num-up,">>9")     format "x(3)".
        /*else put space(4) string(xeb.num-up / 2,">>9") format "x(3)".*/

      put space(2)
          qtty[vmcl] format ">>>,>>>,>>9" space(1)
          dsc[1] format "x(20)"
          sizcol[1] format "x(17)"
          stypart[1] format "x(19)" skip
          space(21)
          dsc[2] format "x(20)"
          sizcol[2] format "x(17)"
          stypart[2] format "x(19)" skip.
      down.
   end.
   put skip(1).
end.
tmpstore = "".

put "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M      TOTAL" skip.

assign
 dm-tot[3] = 0
 dm-tot[4] = 0
 dm-tot[5] = 0.

/* b o a r d        */ run ce/box/pr42-brd.p (v-vend-no).
v-brd-cost = v-brd-cost + dm-tot[5].

/* i n k s          */ run ce/box/pr42-ink.p.

/* films            */ run ce/box/pr42-flm.p.

/* case/tray/pallet */ run ce/box/pr42-cas.p.

/* special          */ run ce/box/pr42-spe.p.

for each blk:
   accumulate blk.cost (total).
end.

for each blk:
   find first xjob
        where xjob.i-no     eq blk.id
          and xjob.form-no  eq blk.snum
          and xjob.blank-no eq blk.bnum
          and xjob.qty      eq blk.qreq
        no-error.
   if not avail xjob then do:
      create xjob.
      assign
       xjob.i-no = blk.id
       xjob.qty  = blk.qreq.
   end.
   assign
    xjob.mat      = blk.cost - blk.lab
    xjob.lab      = blk.lab
    xjob.qty      = blk.qreq
    xjob.form-no  = blk.snum
    xjob.blank-no = blk.bnum
    xjob.pct      = blk.pct
    xjob.stock-no = blk.stock-no
    xjob.pur-man  = xeb.pur-man.
end.

display     "TOTAL  DIRECT  MATERIALS "
            dm-tot[3] format ">>>9.99" to 61
            dm-tot[5] / (tt-blk / 1000) format ">>>>9.99" to 69
            dm-tot[5] format ">>>>,>>9.99" to 80
            skip(1)
    with frame ac5 no-labels no-box stream-io.

/* prep */ run ce/box/pr42-prp.p .

/* misc. */ run ce/box/pr42-mis.p . /* misc. */

put skip(1)
   "Machine Description    MR (Hrs) Run  Speed    Rate     MR $    Run $  Total Cost" .

/* machines */
  run ce/box/pr42-mch.p.

  if ctrl2[2] ne 0 or ctrl2[3] ne 0 then do:
    put "Raw Mat'l Handling" (ctrl2[2] + ctrl2[3]) to 80 skip.
    op-tot[5] = op-tot[5] + (ctrl2[2] + ctrl2[3]).
  end.
 
  fr-tot = 0.
  for each xef
       where xef.company eq xest.company
         and xef.est-no  eq xest.est-no:

   release item.
   release e-item.

   if xef.form-no eq 1 then do:
     find first xeb of xef no-lock no-error.
     if avail xeb then 
         find first item
             where (item.company = cocode)
               and item.i-no eq xeb.tr-no
             no-lock no-error.
     if avail item then
       find first e-item of item no-lock no-error.
   end.

   if avail e-item and item.mat-type eq "Z" then do:
     find first xeb
         where xeb.company  eq xest.company
           and xeb.est-no   eq xest.est-no 
           and xeb.form-no  eq 0
           and xeb.blank-no eq 0
         no-error.
     tr-tot = ((xeb.len * xeb.wid * xeb.dep) * qtty[vmcl]) /
               (item.case-l * item.case-w * item.case-d).
     /*do j = 1 to 10:
       if e-item.run-qty[j] < tr-tot then next.
       fr-tot = round(tr-tot * e-item.run-cost[j],2).
       leave.
     end.
     */

     EMPTY TEMP-TABLE tt-ei.
     CREATE tt-ei.
     DO j = 1 TO 10:
        ASSIGN
           tt-ei.run-qty[j] = e-item.run-qty[j]
           tt-ei.run-cost[j] = e-item.run-cost[j].
     END.
     
     FIND FIRST b-qty WHERE
          b-qty.reftable = "blank-vend-qty" AND
          b-qty.company = e-item.company AND
          b-qty.CODE    = e-item.i-no
          NO-LOCK NO-ERROR.
     
     IF AVAIL b-qty THEN
     DO:
        FIND FIRST b-cost WHERE
             b-cost.reftable = "blank-vend-cost" AND
             b-cost.company = e-item.company AND
             b-cost.CODE    = e-item.i-no
             NO-LOCK NO-ERROR.
     
        DO j = 1 TO 10:
           ASSIGN
              tt-ei.run-qty[j + 10] = b-qty.val[j]
              tt-ei.run-cost[j + 10] = b-cost.val[j].
        END.
     END.

     do j = 1 to 20:
        if tt-ei.run-qty[j] < tr-tot then next.
        fr-tot = round(tr-tot * tt-ei.run-cost[j],2).
        leave.
     end.
   end.

   for each xeb
       where xeb.company eq xef.company
         and xeb.est-no  eq xef.est-no 
         and xeb.form-no eq xef.form-no:
      assign
       v-dest-cd = xeb.dest-code
       v-carrier = xeb.carrier.
      if v-carrier eq "" then do:
        find first eb
            where eb.company eq xest.company
              and eb.est-no  eq xest.est-no
              and eb.carrier ne ""
             no-lock no-error.
        if avail eb then do:
          v-carrier = eb.carrier.
          if v-dest-cd eq "" then v-dest-cd = eb.dest-code.
        end.
      end.
      find first carrier
          where carrier.company = cocode
            and carrier.loc = locode
            and carrier.carrier = v-carrier
          no-lock no-error.
      if avail carrier then
      find first carr-mtx
          where carr-mtx.company  eq cocode
            and carr-mtx.loc      eq locode
            and carr-mtx.carrier  eq carrier.carrier
            and carr-mtx.del-zone eq v-dest-cd
          no-lock no-error.
      find first car where car.id = xeb.part-no no-error.
      if not avail car then do:
         create car.
         assign
         car.carrier = carrier.carrier car.dscr    = carr-mtx.del-zone
         car.id      = xeb.part-no     car.snum   = xeb.form-no
         car.bnum   = xeb.blank-no.
      end.
      
      find first item
          where (item.company = cocode)
            and item.i-no     eq xef.board
            and item.mat-type eq "B"
            and item.avg-w    gt 0
          no-lock no-error.

      assign
       v-yld   = IF xeb.cust-% lt 0 then -1 / xeb.cust-% else xeb.cust-%
       li-blk  = qtty[vmcl] * v-yld
       v-msf   = (xeb.t-sqin - xeb.t-win) * li-blk / 144000
       v-msf   = v-msf * if avail item then item.avg-w else 1
       car.qty = car.qty + (xef.weight * v-msf)
       car.msf = car.msf + v-msf.
       
      if xef.medium ne "" then do:
         find first item where (item.company = cocode) and
                    item.i-no = xef.medium no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        (item.basis-w * (1 - (item.shrink / 100)) * v-msf).
      end.
      if xef.flute ne "" then do:
         find first item where (item.company = cocode) and
                    item.i-no = xef.flute no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        (item.basis-w * v-msf).
      end.
      if xef.lam-code ne "" then do:
         find first item where (item.company = cocode) and
                    item.i-no = xef.lam-code no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                         li-blk * xeb.t-sqin / item.sqin-lb).
      end.
      if xef.adh-code ne "" then do:
         find first item where (item.company = cocode) and
                    item.i-no = xef.adh-code no-lock no-error.
         if avail item
         then car.qty = car.qty +
                        ((INT(xef.medium ne "") + INT(xef.flute ne "")) *
                         li-blk * xeb.t-sqin / item.sqin-lb).
      end.

      /* add pallet & case for total weight */
      for each cas where cas.id = xeb.part-no:
         find first item where item.company = cocode and
                               item.i-no = cas.ino no-lock no-error.
         car.qty = car.qty + (cas.qty *
                   if item.mat-type eq "D" then ce-ctrl.def-pal-w else
                   if item.mat-type eq "C" then ce-ctrl.def-cas-w else
                   IF CAN-DO("5,6",item.mat-type) THEN
                     (item.weight-100 / 100) ELSE 0).
      end.
   end.
  end.

  output close.

  DO TRANSACTION:
    {est/calcpcts.i xest}
    calcpcts.val[2] = v-brd-cost.
    FIND CURRENT calcpcts NO-LOCK NO-ERROR.
  END. 


/**********************/
  CREATE ttFoldSetvarable .
    ASSIGN
    ttFoldSetvarable.vFoldSeqno  = probe.LINE
    ttFoldSetvarable.vFoldQty    =  qty
    ttFoldSetvarable.vFoldGetqty =  probe.est-qty
    ttFoldSetvarable.vFoldRoid   = STRING(ROWID(probe))  
    ttFoldSetvarable.GsaMat     =  gsa-mat
    ttFoldSetvarable.GsaLab     =  gsa-lab
    ttFoldSetvarable.GsaWar     =  gsa-war  .
                   
/************************/    

  v-do-gsa = (vprint and do-gsa).
  IF NOT do-gsa THEN
  run ce/box/pr42totscopy.p (ROWID(probe), li-rels).

  output close.
  IF NOT do-gsa THEN
  run ce/box/pr42mis2.p.

  if vprint AND NOT do-gsa then run ce/box/probemk.p (ROWID(probe)).

  for each blk:
    find first xjob
        where xjob.i-no eq blk.id
          and xjob.qty  eq blk.qreq.
    assign xjob.mat = xjob.mat / (blk.qyld / 1000)
           xjob.lab = xjob.lab / (blk.qyld / 1000)
           xjob.foh = xjob.foh / (blk.qyld / 1000)
           xjob.voh = xjob.voh / (blk.qyld / 1000).
  end.

  if not vprint then DO TRANSACTION:

     IF probe.LINE LT 100 THEN
     DO:
        if opsys eq "unix" then do:
           unix silent rm value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"99")).
           unix silent rm value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"99")).
        end.
        else do: /* if opsys eq "MSDOS" then */
           dos silent del value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"99")).
           dos silent del value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"99")).
        end.
     END.
     ELSE
     DO:
        if opsys eq "unix" then do:
           unix silent rm value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"999")).
           unix silent rm value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"999")).
        end.
        else do: /* if opsys eq "MSDOS" then */
           dos silent del value(tmp-dir + TRIM(xest.est-no) + "-*.*" + string(probe.line,"999")).
           dos silent del value(tmp-dir + TRIM(xest.est-no) +   ".*" + string(probe.line,"999")).
        end.
     END.

     FIND CURRENT probe.
     DELETE probe.
  end.
end.  /* do vmcl = 1 to 28: */

if vprint then do vmcl = 1 to 28:
  if qtty[vmcl] eq 0 then next.

  FOR EACH probe
      WHERE probe.company    EQ xest.company
        AND probe.est-no     EQ xest.est-no
        AND probe.probe-date EQ TODAY
        AND probe.est-qty    EQ qtty[vmcl]
        AND probe.freight    EQ rels[vmcl]
      NO-LOCK
      BY probe.probe-time DESC:
    LEAVE.
  END.

  IF probe.LINE LT 100 THEN
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(probe.line,"99")
           outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                      string(1,"99")     + ".a" + string(probe.line,"99")
           outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                      string(1,"99")     + ".s" + string(probe.line,"99")
           ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                      string(1,"99")     + ".p" + string(probe.line,"99").
  ELSE
     assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".v" + string(probe.line,"999")
            outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".a" + string(probe.line,"999")
            outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".s" + string(probe.line,"999")
            ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                       string(1,"99")     + ".p" + string(probe.line,"999").

  if vmclean then do:
    output to value(outfile3) append.

    vhld = 0.
     
    {ce/mclean.i 1}
    
    put skip.

    output close.
  end.

  if opsys = "unix" then
    unix silent cat value(outfile2) >> value(outfile3).
  else /* if opsys = "msdos" then */
    dos silent type value(outfile2) >> value(outfile3).

  dos silent type value(outfile3) > value(ls-outfile).

  RUN ce/probeu3.p (ROWID(probe)).
end.

DO TRANSACTION:
  FIND CURRENT op-lock NO-ERROR.
  IF AVAIL op-lock THEN DELETE op-lock.
END.

END.



PROCEDURE InsertQty:
    DEF VAR lv-est-no LIKE est.est-no NO-UNDO.
    DEF BUFFER b-reft FOR reftable.
    /*DEF BUFFER op-lock FOR reftable.*/

       

    CREATE tt-qtty.
    ASSIGN     
         do-speed        = LOGICAL(prmDoSpeed)
         do-mr           = LOGICAL(prmDoMr)
         do-gsa          = LOGICAL(prmDoGsa)
         v-drop-rc       = NO
         v-do-all-forms-ink   = NO 
         v-match-up      = 0
         io-do-speed     = LOGICAL(prmDoSpeed) 
         io-do-mr        = LOGICAL(prmDoMr)
         io-v-match-up   = 0 
         io-do-gsa       = LOGICAL(prmDoGsa)

         tt-qtty.qtty[1]  = prmQty1 
         tt-qtty.qtty[2]  = prmQty2 
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
         tt-qtty.rel[28] = prmRels28
         .    
    

  DO i = 1 TO NUM-ENTRIES(prmEstList):
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
  END.

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









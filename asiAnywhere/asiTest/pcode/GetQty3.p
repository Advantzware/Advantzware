


/*------------------------------------------------------------------------
    File        : GetQty3.p
    Purpose     : Get Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
    
    DEFINE TEMP-TABLE ttShared3varable NO-UNDO
        FIELD seqno             AS INT  
        FIELD qty1              AS INT 
        FIELD getqty            AS INT  
        FIELD roid              AS CHAR 
        FIELD GsaMat            AS DECIMAL
        FIELD GsaLab            AS DECIMAL
        FIELD GsaWar            AS DECIMAL
        FIELD GsaFm             AS DECIMAL
        FIELD GsaMonth          AS INT.

    DEFINE DATASET dsGet3qty FOR ttShared3varable .
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
    DEFINE INPUT PARAMETER prmMatchup    AS DECIMAL NO-UNDO.
    DEFINE INPUT PARAMETER prmDoGsa      AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoMr       AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDoSpeed    AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmDropRc     AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmInkAlForms AS CHARACTER NO-UNDO.    
    DEFINE INPUT PARAMETER prmEstList    AS CHARACTER NO-UNDO.

    DEFINE INPUT PARAMETER prmEstimate   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmForm       AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmBlank      AS INTEGER NO-UNDO.
    DEFINE INPUT PARAMETER prmLvoverride   AS CHARACTER NO-UNDO.
    DEFINE INPUT PARAMETER prmVendor   AS CHARACTER NO-UNDO.


    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGet3qty.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER prmRecId  AS CHAR NO-UNDO.

    ASSIGN prmEstList = prmEstimate  .

    
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
    IF prmMatchup = ?  THEN ASSIGN prmMatchup = 0.
    IF prmDoGsa = ?    THEN ASSIGN prmDoGsa = "no".
    IF prmDoMr = ?     THEN ASSIGN prmDoMr = "no".
    IF prmDoSpeed = ?  THEN ASSIGN prmDoSpeed = "no".
    IF prmDropRc = ?   THEN ASSIGN prmDropRc = "no".
    IF prmInkAlForms = ?  THEN ASSIGN prmInkAlForms = "no".
    IF prmEstList = ?   THEN ASSIGN prmEstList = "".

    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".
    IF prmForm = ?  THEN ASSIGN prmForm = 0.
    IF prmBlank = ?  THEN ASSIGN prmBlank = 0.
    IF prmLvoverride = ? THEN ASSIGN prmLvoverride = "Yes".
    IF prmVendor     = ? THEN ASSIGN prmVendor   = "". 
    
DEF NEW SHARED VAR sh-gsa-mat       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-lab       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-war       AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-fm        AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-war-tot   AS DECIMAL NO-UNDO.
DEF NEW SHARED VAR sh-gsa-brd       AS DECIMAL NO-UNDO. 
DEFINE NEW SHARED VAR getqty3 AS LOG NO-UNDO.
ASSIGN getqty3 = YES.

/* Local Variable Definitions ---                                       */
def VAR io-do-speed as log no-undo.
def VAR io-do-mr as log no-undo.
def VAR io-do-gsa as log no-undo.
def VAR io-v-drop-rc as log no-undo.
def VAR io-v-match-up as dec no-undo.
def VAR io-ink-all-forms AS LOG NO-UNDO.
def var v-brd-only like sys-ctrl.log-fld init no no-undo.
def var v-brd-cost as dec no-undo.
DEF NEW SHARED VAR tmp-dir AS cha NO-UNDO.


/*def input parameter lv-mclean as log no-undo.
def output parameter op-error as log no-undo.
*/

    
{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}
{sys/inc/var.i "new shared"}


def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

def var call_id as recid no-undo.


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


def new shared var v-qtty like qtty no-undo.
def new shared var v-do-gsa like do-gsa no-undo.

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
def NEW shared var g_company as cha no-undo.
def NEW shared var g_loc as cha no-undo.

def var ls-outfile as cha no-undo.
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
 g_company = prmComp   
 locode = usercomp.loc
 . 

ASSIGN 
    locode = "MAIN"
    g_loc  = locode
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


{sys/inc/f16to32.i}

  DEF VAR v-yld-qty AS DEC FORMAT ">>>,>>>" NO-UNDO.
  
  DEF VAR v-n-out AS INT NO-UNDO.
  
  DEF BUFFER reftable-fm FOR reftable.
  DEF BUFFER reftable-broker-pct FOR reftable.
  DEF BUFFER b-est-qty-2 FOR est-qty.
  DEF VAR v-count-2 AS INT NO-UNDO.


  /*********************************/
  def var v-part-no like xeb.part-no NO-UNDO.
  def var v-part-d1 like xeb.part-dscr1 NO-UNDO.
  def var v-part-d2 like xeb.part-dscr2 NO-UNDO.
  def var vn-out    like xef.n-out NO-UNDO.
  def var v-yld     as   DEC NO-UNDO.
  DEF VAR v-line LIKE probe.line no-undo.
  def var v-vend-no   like e-item-vend.vend-no init "" NO-UNDO.
  DEF var v-vend-list AS CHAR NO-UNDO.
  DEF VAR v-hdr-depth AS CHAR FORMAT "x(5)" NO-UNDO.
  def var lv-brd-sf as dec format ">>>>>9.9<<"  no-undo.
  def var lv-brd-wu as dec format ">>>>9.9<<<<"no-undo.
  DEF VAR ll-use-defaults AS LOG NO-UNDO.
  DEF VAR ld-board AS DEC NO-UNDO.
/**********************************************/

DEF TEMP-TABLE tt-bqty NO-UNDO FIELD tt-bqty AS INT FIELD tt-brel AS INT.

def var v-gsa as log init no no-undo.
def var v-bqty as int no-undo.
def var v-module as char format "x(60)" no-undo.


FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
FIND FIRST ef WHERE ef.est-no = est.est-no AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
FIND FIRST eb WHERE eb.est-no = est.est-no AND eb.company = prmComp AND eb.form-no = prmForm AND eb.blank-no = prmBlank NO-LOCK NO-ERROR. 


find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

ASSIGN vprint = YES.

lv-ef-recid = recid(xef).



IF prmAction = "doCalc" THEN DO:
    
qtty = 0.

/*{cec/get-vend.i}  /* get vendor number */*/

ASSIGN
       v-vend-no  = prmVendor .

IF v-vend-no EQ "&nbsp;" OR v-vend-no EQ ? THEN DO:
    ASSIGN
        v-vend-no = "".
END.

find first xop where xop.company = xest.company
                 AND xop.est-no = xest.est-no and
                     xop.op-speed = 0 no-lock no-error.

save-lock = xef.op-lock.



do transaction:
  {cec/msfcalc.i}

  if not avail sys-ctrl then do:
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
  end.
  ll-use-defaults = sys-ctrl.log-fld.

  {est/recalc-mr.i xest}
  FIND CURRENT recalc-mr NO-LOCK.

  {sys/inc/cerun.i C}
  assign
   do-speed  = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.recalc
   do-mr     = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE (recalc-mr.val[1] EQ 1)
   v-do-all-forms-ink = recalc-mr.val[2] EQ 1
   vmclean   = sys-ctrl.char-fld ne ""
   vsuthrlnd = lookup(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") ne 0
   v-module = module.

  if sys-ctrl.char-fld eq "Brick" then
    assign
     v-module = v-module + " - ISO# CS-03-1-F"
     {sys/inc/ctrtext.i "v-module" 60}.
  {sys/inc/ctrtext.i "v-module" 60}.
  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CEGSA"
      no-lock no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company = cocode
     sys-ctrl.name    = "CEGSA"
     sys-ctrl.descrip = "Default for GS&A override".
    MESSAGE sys-ctrl.descrip
        VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
        UPDATE sys-ctrl.log-fld.
  end.
  do-gsa = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.override.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CESLIT"
      no-lock no-error.
  if not avail sys-ctrl then do:
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
  v-drop-rc = sys-ctrl.log-fld.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "CEDFAULT"
      no-lock no-error.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "SETPRINT"
      no-lock no-error.
  if not avail sys-ctrl then do:
    create sys-ctrl.
    assign
     sys-ctrl.company  = cocode
     sys-ctrl.name     = "SETPRINT"
     sys-ctrl.descrip  = "Default for Set Estimate Printout"
     sys-ctrl.char-fld = "ASI".
    message "System control record NOT found. Enter default for Set Est Printout"
    update sys-ctrl.char-fld.
  end.
  vmclean2 = sys-ctrl.char-fld eq "McLean".
  if vmclean2 then v-match-up = sys-ctrl.dec-fld.
end.

find first ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode)  no-lock no-error.
assign
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
ctrl[18] = int(ce-ctrl.spec-add[8])
v-gsa    = index("SB",ce-ctrl.sell-by) eq 0.

FIND FIRST reftable
     WHERE reftable.reftable EQ "ce-ctrl.broker-pct"
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

if not v-gsa then do-gsa = no.

if vprint then do:
  /*{cec/blkeqsht.i}*/
  j = 0.
  for each est-qty
      where est-qty.company = xest.company
        and est-qty.est-no  = xest.est-no
      no-lock: 
    do i = 1 to 20:
      if est-qty.qty[i] <> 0 then do:
        j = j + 1.

        IF j LE EXTENT(qtty) THEN
          ASSIGN
           qtty[j] = est-qty.qty[i]
           rels[j] = est-qty.qty[i + 20].
      end.              
    end.
  end.

  find first tt-qtty no-error.
  if avail tt-qtty then delete tt-qtty.
  create tt-qtty.
  do i = 1 to 28:
     assign tt-qtty.qtty[i] = qtty[i]
            tt-qtty.rel[i] = IF qtty[i] EQ 0 THEN 0
                             ELSE
                             IF rels[i] EQ 0 THEN 1 ELSE rels[i].
  end.

  /*run est/getqty.w (input-output do-speed, input-output do-mr, input-output do-gsa, input-output v-drop-rc, input-output v-match-up,
                    INPUT-OUTPUT v-do-all-forms-ink, input vmclean2, output lv-error) no-error.

  if lv-error then return error.*/


  RUN InsertQty .

  IF prmLvoverride = "Yes" THEN
  for each probe where probe.company = xest.company and
                       probe.est-no = xest.est-no:
     delete probe.                 
  end.

  do i = 1 to 28:
     ASSIGN
        qtty[i] = tt-qtty.qtty[i]
        rels[i] = tt-qtty.rel[i].
  end.

  {sys/inc/srtqty.i &sub=i &ext=28 &qty=qtty &rel=rels}

  do i = 1 to 28:
    if qtty[i] eq 0 then rels[i] = 0.
    else
    if rels[i] eq 0 then rels[i] = 1.
  end.  
end.  /* vprint */
else do:
  assign
     qtty[1] = qty.

  IF v-shared-rel EQ 0 THEN /*no quote from o/e*/
     FOR EACH est-qty WHERE
         est-qty.company EQ xest.company AND
         est-qty.est-no  EQ xest.est-no
         NO-LOCK:

         DO i = 1 TO 20:
            IF est-qty.qty[i] EQ qty THEN DO:
               rels[1] = est-qty.qty[i + 20].
               LEAVE.
            END.
         END.
     END.
  ELSE
     rels[1] = v-shared-rel. 

  IF rels[1] = 0 THEN
     rels[1] = 1.

  find first sys-ctrl
      where sys-ctrl.company eq cocode
        and sys-ctrl.name    eq "FGCOST"
      no-lock no-error.
  if not avail sys-ctrl then do transaction:
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
  v-brd-only = sys-ctrl.log-fld.
end.

DO TRANSACTION:
  {est/op-lock.i xest}
  FIND est WHERE RECID(est) EQ RECID(xest).
  FIND CURRENT recalc-mr.
  FIND CURRENT op-lock.

  ASSIGN
   est.recalc       = do-speed
   recalc-mr.val[1] = INT(do-mr)
   recalc-mr.val[2] = INT(v-do-all-forms-ink)
   est.override     = do-gsa
   op-lock.val[1]   = INT(est.recalc)
   op-lock.val[2]   = recalc-mr.val[1].
  FIND est WHERE RECID(est) EQ RECID(xest) NO-LOCK.
  FIND xest WHERE RECID(xest) EQ RECID(est) NO-LOCK.
  FIND CURRENT recalc-mr NO-LOCK.
  FIND CURRENT op-lock NO-LOCK.  
END.

session:set-wait-state("General").

assign day_str = string(today,"99/99/99")
       tim_str = string(time,"hh:mm am") .

form day_str FORM "x(8)" v-module tim_str to 79  skip(1)
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
   "Last Ordered:" xest.ord-date
   SKIP(1)
  WITH NO-LABELS NO-BOX DOWN WIDTH 80 FRAME kli STREAM-IO.

find first xeb
    where xeb.company = xest.company
      and xeb.est-no    eq xest.est-no
      and xeb.form-no  eq 0
      and xeb.blank-no eq 0
    NO-LOCK NO-ERROR.
IF NOT AVAIL xeb THEN /* task 04091012*/
   find first xeb
    where xeb.company = xest.company
      and xeb.est-no    eq xest.est-no
      and xeb.form-no  eq 0 NO-LOCK.
       
   assign
     v-part-no = xeb.part-no
     v-part-d1 = xeb.part-dscr1
     v-part-d2 = xeb.part-dscr2.

do vmcl = 1 to 28:   /* ??? 28 not 4*/
  if qtty[vmcl] eq 0 then next.

  IF v-do-all-forms-ink AND xest.est-type EQ 6 AND
     INDEX(PROGRAM-NAME(1),"all-inks") EQ 0 AND
     INDEX(PROGRAM-NAME(2),"all-inks") EQ 0 AND
     INDEX(PROGRAM-NAME(3),"all-inks") EQ 0 AND
     INDEX(PROGRAM-NAME(4),"all-inks") EQ 0 AND
     INDEX(PROGRAM-NAME(5),"all-inks") EQ 0 THEN
     /*calculate ink totals across all forms for sets*/
     RUN cec\all-inks.p(INPUT qtty[vmcl],
                        INPUT xest.est-no,
                        OUTPUT TABLE tt-all-forms-ink).

  assign
   v-op-qty     = 0
   v-tt-tot     = 0
   v-fac-tot    = 0
   v-ord-cost   = 0
   v-brd-cost   = 0
   t-shtfrm     = 0
   t-blksht     = 0
   t-blkqty     = 0
   v-qtty[vmcl] = qtty[vmcl].

  for each blk:
      delete blk.
  end.

  for each xjob:
      delete xjob.
  end.

  for each brd:
    delete brd.
  end.

  for each cas WHERE cas.snum EQ 0:
    delete cas.
  end.

  for each car WHERE car.snum EQ 0:
    delete car.
  end.

  FOR EACH est-op
      WHERE est-op.company EQ xest.company 
        AND est-op.est-no  EQ xest.est-no 
        AND est-op.line    LT 500
      NO-LOCK
      BREAK BY est-op.qty:
    
    IF FIRST-OF(est-op.qty) THEN DO:
      IF FIRST(est-op.qty) OR
         CAN-FIND(FIRST est-qty
                  WHERE est-qty.company EQ est-op.company
                    AND est-qty.est-no  EQ est-op.est-no
                    AND est-qty.eqty    EQ est-op.qty)
      THEN v-op-qty = est-op.qty.
      IF est-op.qty GE qtty[vmcl] THEN LEAVE.
    END.
  END.

  {est/probeset.i qtty[vmcl] v-match-up}

  for each ef where ef.company = xest.company and
                    ef.est-no eq xest.est-no no-lock:
      assign  op-tot    = 0
              ctrl2     = 0
              v-form-no = ef.form-no.
      for each kli:
        delete kli.
      end.
      for each ink:
        delete ink.
      end.
      for each flm:
        delete flm.
      end.
      for each cas WHERE cas.snum NE 0:
        delete cas.
      end.
      for each car WHERE car.snum NE 0:
        delete car.
      end.
      for each w-form:
        delete w-form.
      end.

      IF probe.LINE LT 100 THEN
         assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                          string(v-form-no,"99")     + ".v" + string(probe.line,"99")
                outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                           string(v-form-no,"99")     + ".a" + string(probe.line,"99")
                outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                           string(v-form-no,"99")     + ".s" + string(probe.line,"99")
                outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"99")
                ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                           string(v-form-no,"99")     + ".p" + string(probe.line,"99").
      ELSE
         assign outfile1 = tmp-dir + trim(xest.est-no) + "-"  +
                          string(v-form-no,"99")     + ".v" + string(probe.line,"999")
                outfile2 = tmp-dir + trim(xest.est-no) + "-"  +
                           string(v-form-no,"99")     + ".a" + string(probe.line,"999")
                outfile3 = tmp-dir + trim(xest.est-no) + "-"  +
                           string(v-form-no,"99")     + ".s" + string(probe.line,"999")
                outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"999")
                ls-outfile = tmp-dir + trim(xest.est-no) + "-"  +
                           string(v-form-no,"99")     + ".p" + string(probe.line,"999").

      output to value(outfile1) .
      vbsf = 0.
      do transaction:
        for each est-op where est-op.company = xest.company
                          and est-op.est-no  eq xest.est-no
                          and est-op.line   gt 500
                          and (est-op.s-num eq v-form-no or (not vmclean2))
                    exclusive:
           delete est-op.
        end.
        for each est-op where est-op.company = xest.company
                          and est-op.est-no  eq xest.est-no
                          and est-op.line   lt 500
                          and (est-op.s-num eq v-form-no or (not vmclean2))
                          exclusive:
           create xop.
           buffer-copy est-op to xop
           assign
            xop.line = est-op.line + 500.
        end.
      end.
      for each xef where xef.company = xest.company
                     and xef.est-no    eq xest.est-no
                     and (xef.form-no eq v-form-no or (not vmclean2))
          NO-LOCK:

          RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

          xxx = 0.

          for each xeb where xeb.company = xest.company and
                             xeb.est-no eq xest.est-no and 
                             xeb.form-no eq xef.form-no
              NO-LOCK:
              find first kli where kli.cust-no eq xeb.cust-no no-error.
              if not avail kli then do:
                 find first sman   where   sman.company eq cocode and
                                           sman.sman    eq xeb.sman no-lock no-error.
                 find first cust   where   cust.company eq cocode and
                                           cust.cust-no eq xeb.cust-no no-lock no-error.
                 find first shipto where   shipto.company eq cocode and
                                           shipto.cust-no eq xeb.cust-no and
                                           shipto.ship-id eq xeb.ship-id no-lock no-error.
                 create kli.
                 if avail sman then assign kli.sman    = sman.sman
                                           kli.sname   = sman.sname.
                 if xeb.cust-no ne "Temp" then assign kli.cust-no = xeb.cust-no
                                                      kli.cust-add[1] = cust.name
                                                      kli.cust-add[2] = cust.addr[1]
                                                      kli.cust-add[3] = cust.addr[2]
                                                      kli.cust-add[4] = cust.city + ", " + cust.state + " " + cust.zip.
                 else assign kli.cust-no = xeb.cust-no
                             kli.cust-add[1] = xeb.ship-name
                             kli.cust-add[2] = xeb.ship-addr[1]
                             kli.cust-add[3] = xeb.ship-addr[2]
                             kli.cust-add[4] = xeb.ship-city + ", " + xeb.ship-state + " " +
                                               xeb.ship-zip.

                 if kli.cust-add[3] eq "" then assign kli.cust-add[3] = kli.cust-add[4] 
                                                      kli.cust-add[4] = "".
                 if kli.cust-add[2] eq "" then assign kli.cust-add[2] = kli.cust-add[3] 
                                                      kli.cust-add[3] = kli.cust-add[4]
                                                      kli.cust-add[4] = "".
                 assign kli.ship-add[1] = shipto.ship-name
                        kli.ship-add[2] = shipto.ship-addr[1]
                        kli.ship-add[3] = shipto.ship-addr[2]
                        kli.ship-add[4] = shipto.ship-city + ", " + shipto.ship-state +
                                                         " " + shipto.ship-zip.
                 if kli.ship-add[3] eq "" then
                     assign kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
                 if kli.ship-add[2] eq "" then
                     assign kli.ship-add[2] = kli.ship-add[3]
                            kli.ship-add[3] = kli.ship-add[4] kli.ship-add[4] = "".
              end.  /* not avail kli */
              assign v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty
                     qty   = qtty[vmcl] * v-yld.
              find first blk where blk.snum eq xeb.form-no and
                                   blk.bnum eq xeb.blank-no no-error.
              if not avail blk then do:
                 create blk.
                 assign blk.kli     = kli.cust-no 
                        blk.id      = xeb.part-no
                        blk.snum    = xeb.form-no
                        blk.bnum    = xeb.blank-no
                        blk.qreq    = qty
                        blk.qyld    = qty
                        blk.yr$     = xeb.yrprice
                        blk.pur-man = xeb.pur-man.
              end.
              xxx = xxx + (xeb.t-sqin * xeb.num-up * vn-out).
          end.  /* for each xeb */
          for each xeb FIELDS(form-no blank-no t-sqin num-up) where
              xeb.company = xest.company AND
              xeb.est-no eq xest.est-no AND
              xeb.form-no eq xef.form-no
              NO-LOCK:
              find first blk where blk.snum eq xeb.form-no and blk.bnum eq xeb.blank-no.
              blk.pct = (xeb.t-sqin * xeb.num-up * vn-out) / xxx.
          end.
      end.  /* for each xef */
      /* print header */
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
                  v-part-d2
                  xest.ord-date.
      end.
      for each xef where xef.company = xest.company 
                     and xef.est-no eq xest.est-no
                     and (xef.form-no eq v-form-no or (not vmclean2))
           NO-LOCK
            with frame brd no-labels no-box width 80 down stream-io:

           qty = qtty[vmcl].

           RUN cec/box/prokalk2.p (500, ?).

           RUN est/ef-#out.p (ROWID(xef), OUTPUT vn-out).

           assign brd-l[1] = xef.nsh-len
                  brd-w[1] = xef.nsh-wid
                 /* calc. sheet dimensions & weight */
                  brd-l[2] = xef.gsh-len.
           if xef.roll = true then brd-l[3] = xef.trim-l.
           brd-w[2] = xef.gsh-wid.
           if brd-l[2] = 0 and brd-w[2] = 0 then
              assign brd-l[2] = xef.lsh-len
                     brd-w[2] = xef.lsh-wid .
           ASSIGN
             brd-w[3] = if xef.roll eq true then xef.nsh-len else 0
             brd-sq[1] = xef.nsh-len * xef.nsh-wid
             brd-sq[2] = brd-l[2] * brd-w[2]
             brd-sq[3] = brd-l[3] * brd-w[3].

           if v-corr then
               assign brd-sf[1] = brd-sq[1] * .007
                      brd-sf[2] = brd-sq[2] * .007
                      brd-sf[3] = brd-sq[3] * .007.
           else assign brd-sf[1] = brd-sq[1] / 144
                       brd-sf[2] = brd-sq[2] / 144
                       brd-sf[3] = brd-sq[3] / 144.

           find first item where (item.company = cocode) and item.i-no = xef.board no-lock no-error.
           if avail item then find first e-item of item no-lock no-error.
           ASSIGN
              brd-wu[1] = brd-sf[1]  * item.basis-w
              brd-wu[2] = brd-sf[2]  * item.basis-w
              brd-wu[3] = (brd-sf[3] * item.basis-w) / 2000
              lv-brd-sf = xef.gsh-len * xef.gsh-wid * xef.gsh-qty   
              lv-brd-sf = (if v-corr then (lv-brd-sf * .007) else (lv-brd-sf / 144)) / 1000 /*tot msf*/.

           if avail item then lv-brd-wu = (lv-brd-sf * item.basis-w) / 2000.

           zzz = 0.

           for each xeb where xeb.company = xest.company 
                          and xeb.est-no eq xest.est-no
                          and xeb.form-no eq xef.form-no
               
               BREAK BY xeb.blank-no:

             ASSIGN
             v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty
             /* set total # of blanks on all forms */
             tt-blk = qtty[vmcl].
             /* set total # of blanks on this form */
             /* set total qty of all blanks for this form */
             if (xeb.num-up * vn-out) gt t-blksht[xef.form-no] then
               t-blksht[xef.form-no] = xeb.num-up * vn-out.

             assign
              t-blkqty[xeb.form-no] = t-blkqty[xeb.form-no] + (qtty[vmcl] * v-yld)
              brd-l[4]  = xeb.t-len
              brd-w[4]  = xeb.t-wid
              brd-sq[4] = brd-l[4] * brd-w[4]
              brd-sf[4] = if v-corr then brd-sq[4] * .007 else brd-sq[4] / 144
              brd-wu[4] = brd-sf[4] * item.basis-w 
              /* find sheet qty needed for this form (without spoil)*/
              zzz = (qtty[vmcl] * v-yld / (xeb.num-up * vn-out)).
            
             i = 1.
             FOR EACH eb NO-LOCK
                 WHERE eb.company EQ xeb.company
                   AND eb.est-no  EQ xeb.est-no
                   AND eb.form-no NE 0
                   AND eb.form-no NE xeb.form-no
                   AND eb.part-no EQ xeb.part-no:
               i = i + 1.
             END.
             zzz = zzz / i.

             {sys/inc/roundup.i zzz}

             /*Get highest qty count, not last one*/
             IF zzz > t-shtfrm[xeb.form-no] THEN
                t-shtfrm[xeb.form-no] = zzz.

             ASSIGN
                call_id = recid(xeb)
                vbsf = vbsf + if v-corr then (xeb.t-sqin * .007) else (xeb.t-sqin / 144)
                qty         = qtty[vmcl] * v-yld
                tmpstore    = "FORM " + trim(string(xef.form-no,">9"))
                v-hdr-depth = IF xeb.t-dep   EQ 0 AND
                                 xef.nsh-dep EQ 0 AND
                                 xef.gsh-dep EQ 0 THEN "" ELSE "Depth".

             IF FIRST(xeb.blank-no) THEN
               display tmpstore
                       space(5)
                       "   Width  Length  "
                       v-hdr-depth
                       "    #On Qty/Set      Sq.Feet     Wgt/Units"
                       skip
                   with no-box no-labels color value("blu/brown") width 80 frame aa2-0 STREAM-IO.

             display "  Blank Size:"
                     brd-w[4]                           format ">>>9.99<<<"
                     brd-l[4]                           format ">>>9.99<<<" 
                     xeb.t-dep WHEN xeb.t-dep NE 0      format ">>>9.99<<<"
                     1                                  format ">>>,>>>" 
                     v-yld                              FORMAT ">>>,>>>"
                     brd-sf[4]                              
                     "Sf/BL"
                     brd-wu[4]
                     space(0)
                     "/MBL"
                     SKIP
                 with no-box no-labels color value("blu/brown") width 80 frame aa2-1 STREAM-IO.

             IF v-yld LT 1 THEN DO WITH FRAME aa2-1:
                v-yld:FORMAT = ">>>>9.9<<<<".

               DISPLAY v-yld.
             END.

             if not vsuthrlnd THEN DO WITH FRAME aa2-1:
               ASSIGN
                brd-w[4]:FORMAT  = ">>>9.99"
                brd-l[4]:FORMAT  = ">>>9.99"
                xeb.t-dep:FORMAT = ">>>9.99".

               display {sys/inc/k16v.i brd-w[4]} @ brd-w[4]
                       {sys/inc/k16v.i brd-l[4]} @ brd-l[4]
                       "" @ xeb.t-dep
                       {sys/inc/k16v.i xeb.t-dep} WHEN xeb.t-dep NE 0 @ xeb.t-dep.
             END.

             IF LAST(xeb.blank-no) THEN DO:
               DISPLAY " NetSht Size:"
                       brd-w[1]                            format ">>>9.99<<<"
                       brd-l[1]                            format ">>>9.99<<<"
                       xef.nsh-dep WHEN xef.nsh-dep NE 0   format ">>>9.99<<<"
                       xeb.num-up                          format ">>>,>>9"
                       SPACE(9)
                       brd-sf[1]
                       "Sf/NS"
                       brd-wu[1]
                       space(0)
                       "/MNS"
                       skip

                       " GrsSht Size:"
                       brd-w[2]                            format ">>>9.99<<<"
                       brd-l[2]                            format ">>>9.99<<<"
                       xef.gsh-dep WHEN xef.gsh-dep NE 0   format ">>>9.99<<<"
                       vn-out                              format ">>>,>>9"
                       SPACE(9)
                       brd-sf[2]
                       "Sf/GS"
                       brd-wu[2]
                       space(0)
                       "/MGS" SKIP
                   with no-box no-labels color value("blu/brown") width 80 frame aa2-2 stream-io.

               if not vsuthrlnd THEN DO WITH FRAME aa2-2:
                 ASSIGN
                  brd-w[1]:FORMAT    = ">>>9.99"
                  brd-l[1]:FORMAT    = ">>>9.99"
                  xef.nsh-dep:FORMAT = ">>>9.99"
                  brd-w[2]:FORMAT    = ">>>9.99"
                  brd-l[2]:FORMAT    = ">>>9.99"
                  xef.gsh-dep:FORMAT = ">>>9.99".

                 display {sys/inc/k16v.i brd-w[1]} @ brd-w[1]
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

           if brd-w[3] ne 0 then
             display "Roll  Size :" brd-w[3]                format ">>9.99<<" to 22
                 with no-box no-labels  width 80 frame aa3 stream-io.

           if not vsuthrlnd then
             if brd-w[3] ne 0 then display {sys/inc/k16v.i brd-w[3]} @ brd-w[3] with frame aa3.
           
           display lv-brd-sf TO 52 "MSF"
                   lv-brd-wu TO 70 "Tons"
               with frame aa3.


           display skip(1)
                   "- # UP - -- Qty --- --- Description ----"
                   "-- Size/Color --- --- Style/Part # --"
                   with no-box no-labels  width 80 frame aa4 stream-io.

           tmpstore = "".
           for each xeb where xeb.company = xest.company 
                          and xeb.est-no eq xest.est-no 
                          and xeb.form-no eq xef.form-no
               NO-LOCK
                       with frame blk no-box no-labels width 80 down stream-io:
              v-yld = if xeb.yld-qty lt 0 then -1 / xeb.yld-qty else xeb.yld-qty.
              find first style  where  style.company eq cocode and
                                       style.style eq xeb.style no-lock no-error.
              ASSIGN
                 dsc[1] = xeb.part-dscr1
                 dsc[2] = xeb.part-dscr2
                 sizcol[1] = string(xeb.len) + "x" + string(xeb.wid) + "x" + string(xeb.dep)
                 sizcol[2] = xeb.i-coldscr
                 stypart[1] = style.dscr
                 stypart[2] = xeb.part-no.

              put space(4) string(xeb.num-up,">>9")     format "x(3)".
              put space(2)
                  qtty[vmcl] * v-yld format ">>,>>>,>>9" space(1)
                  dsc[1] format "x(20)"
                  space(1)
                  sizcol[1] format "x(17)"
                  space(1)
                  stypart[1] skip
                  space(20)
                  dsc[2] format "x(20)"
                  space(1)
                  sizcol[2] format "x(17)"
                  space(1)
                  stypart[2] skip.
              down.
           end. 
           put skip(1).
      end.  /* for each xef */
      /* if line-counter > 40 then page.  ??? */

      put "Materials                 Weight Caliper    QTY/Unit    MR $  Matl$/M    TOTAL" skip.
      assign dm-tot-3  = 0
             dm-tot-4  = 0
             dm-tot-5  = 0
             dm-tot[3] = 0
             dm-tot[4] = 0
             dm-tot[5] = 0
             ld-board  = 0.
    
      for each blk where blk.snum eq ef.form-no or (not vmclean2),
          first xeb where xeb.company eq xest.company 
                      and xeb.est-no eq xest.est-no
                      and xeb.form-no  eq blk.snum
                      and xeb.blank-no eq blk.bnum  no-lock:
          find first xjob where xjob.i-no eq blk.id
                            and xjob.qty  eq blk.qreq no-error.
          if not avail xjob then do:
             create xjob.
             assign xjob.i-no     = blk.id
                    xjob.qty      = blk.qreq
                    xjob.form-no  = xeb.form-no
                    xjob.blank-no = xeb.blank-no.
          end.
          assign xjob.qty      = blk.qreq
                 xjob.cust-no  = xeb.cust-no
                 xjob.pct      = blk.pct
                 xjob.stock-no = xeb.stock-no
                 xjob.pur-man  = xeb.pur-man.
      end.

      qty = qtty[vmcl].
     
      /* b o a r d        */
      run cec/box/pr42-brd.p (v-vend-no, OUTPUT v-vend-list).
      ASSIGN
         v-brd-cost = v-brd-cost + dm-tot[5]
         ld-board   = dm-tot[5].    

      /* adders           */
      run cec/box/pr42-add.p (v-vend-list).     
 

      FIND CURRENT probe-board NO-ERROR.
      
      IF AVAIL probe-board THEN
         probe-board.val[1] = probe-board.val[1] + dm-tot[5].

      FIND CURRENT probe-board NO-LOCK NO-ERROR.

      /* i n k s          */
      run cec/box/pr42-ink.p (v-vend-no, INPUT TABLE tt-all-forms-ink).

      /* films            */
      run cec/box/pr42-flm.p (v-vend-no).

      /* case/tray/pallet */
      run cec/box/pr42-cas.p (v-vend-no).

      /* special          */
      run cec/box/pr42-spe.p (v-vend-no).

     for each blk where blk.snum eq ef.form-no or (not vmclean2),
         first xeb where xeb.company eq xest.company 
                     and xeb.est-no eq xest.est-no
                     and xeb.form-no  eq blk.snum
                     and xeb.blank-no eq blk.bnum  no-lock:
         find first xjob where xjob.i-no eq blk.id
                           and xjob.qty  eq blk.qreq no-error.
         if not avail xjob then do:
            create xjob.
            assign xjob.i-no     = blk.id
                   xjob.qty      = blk.qreq
                   xjob.form-no  = xeb.form-no
                   xjob.blank-no = xeb.blank-no.
         end.

         assign xjob.mat      = blk.cost - blk.lab
                xjob.lab      = blk.lab
                xjob.qty      = blk.qreq
                xjob.cust-no  = xeb.cust-no
                xjob.pct      = 1.00
                xjob.stock-no = xeb.stock-no
                xjob.pur-man  = xeb.pur-man.
      end.

      /* prep             */
      run cec/box/pr42-prp.p.

      /* misc.            */
      run cec/box/pr42-mis.p.

      x = 2.
      for each est-op where est-op.company = xest.company 
                        and est-op.est-no eq xest.est-no
                        and est-op.line > 500
                      no-lock:
         x = x + 1.
      end.
      
      put skip(1)
       "Machine Desc       Out     SU   Run Speed     Rate     MR $  Run $/M  Total Cost".
      run cec/box/pr42-mch.p.

      

      /*output close.*/

      DO TRANSACTION:
        {est/calcpcts.i xest}
        
        FIND CURRENT calcpcts EXCLUSIVE-LOCK NO-ERROR.
        IF do-gsa THEN calcpcts.val[2] = ld-board.
        FIND CURRENT calcpcts NO-LOCK NO-ERROR.
      END.

      v-do-gsa = do-gsa.

       DEFINE VAR li-qty AS INT NO-UNDO.
       FOR EACH brd
           WHERE (brd.form-no EQ v-form-no OR (NOT vmclean2))
             AND CAN-FIND(FIRST ITEM WHERE item.company  EQ cocode
                                       AND item.i-no     EQ brd.i-no
                                       AND item.mat-type EQ "D"):
      
         li-qty = li-qty + brd.qty.
       END.
      /********************/
       /* IF NOT  do-gsa THEN*/
      run cec/box/pr42totscopy.p (ROWID(probe)).

      CREATE ttShared3varable .
        ASSIGN
            ttShared3varable.seqno  = probe.LINE
            ttShared3varable.qty1   = qty 
            ttShared3varable.getqty = probe.est-qty
            ttShared3varable.roid   = string(ROWID(probe)) 

            ttShared3varable.GsaMat        =  sh-gsa-mat
            ttShared3varable.GsaLab        =  sh-gsa-lab
            ttShared3varable.GsaWar        =  sh-gsa-war
            ttShared3varable.GsaFm         =  sh-gsa-fm
            ttShared3varable.GsaMonth      =  li-qty .

     
      IF NOT do-gsa THEN
      run cec/box/pr42mis2copy.p.

      if opsys eq "unix" then
         unix silent copy value(outfile1) value(outfile3).
      else /* if opsys eq "MSDOS" then */
         /*dos silent copy value(outfile1) value(outfile3).*/
         OS-COPY value(outfile1) value(outfile3).
         
      assign   v-tt-tot[v-form-no]   = tt-tot
               v-fac-tot[v-form-no]  = fac-tot
               v-ord-cost[v-form-no] = ord-cost.
      
      if (not vmclean2) then leave.
  end.  /* for each ef */
  
  assign   tt-tot   = 0
           fac-tot  = 0
           ord-cost = 0.

  do j = 1 to 99:
    assign   tt-tot   = tt-tot   + v-tt-tot[j]
             fac-tot  = fac-tot  + v-fac-tot[j]
             ord-cost = ord-cost + v-ord-cost[j].
  end.
 IF NOT do-gsa THEN DO:
     if vprint then run cec/box/probemk.p (ROWID(probe)).
 END.
  

  FOR EACH xjob:
    ACCUMULATE xjob.mat (TOTAL).
  END.

  FOR EACH blk BREAK BY blk.id:
    FIND FIRST xjob
        WHERE xjob.i-no EQ blk.id
          AND xjob.qty  EQ blk.qreq
        NO-ERROR.

    IF v-brd-only THEN DO:
      IF FIRST(blk.id) THEN ord-cost = v-brd-cost.

      ASSIGN
       xjob.mat = xjob.mat / (ACCUM TOTAL xjob.mat) *
                  v-brd-cost / (tt-blk / 1000)
       xjob.lab = 0
       xjob.foh = 0
       xjob.voh = 0.
    END.

    ELSE
      ASSIGN
       xjob.mat = xjob.mat / (tt-blk / 1000)
       xjob.lab = xjob.lab / (tt-blk / 1000)
       xjob.foh = xjob.foh / (tt-blk / 1000)
       xjob.voh = xjob.voh / (tt-blk / 1000).
  END.
  
 
end.  /* do vmcl = 1 to 28: */   

DO v = 1 to 28:
  v-qtty[v] = qtty[v].
end.
/*if vprint then
for each ef where ef.company = xest.company and
                  ef.est-no eq xest.est-no no-lock:
    v-form-no = ef.form-no.
    run cec/pr4-mcl.p.
    if (not vmclean2) then leave.
end.  /* for each ef */*/

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
         do-speed            = LOGICAL(prmDoSpeed)
         do-mr               = LOGICAL(prmDoMr)
         do-gsa              = LOGICAL(prmDoGsa)
         v-drop-rc           = LOGICAL(prmDropRc)
         v-do-all-forms-ink  = LOGICAL(prmInkAlForms)
         v-match-up          = prmMatchup
         io-do-speed         = LOGICAL(prmDoSpeed) 
         io-do-mr            = LOGICAL(prmDoMr) 
         io-v-match-up       =  prmMatchup 


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









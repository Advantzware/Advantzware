


/*------------------------------------------------------------------------
    File        : GetQty.p
    Purpose     : Get Quantity

    Syntax      :

    Description : Return a Dataset of Request For Order

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

    DEFINE TEMP-TABLE ttGetqty NO-UNDO
        FIELD lv-qty-1  AS INTEGER
        FIELD lv-qty-2  AS INTEGER
        FIELD lv-qty-3  AS INTEGER
        FIELD lv-qty-4  AS INTEGER
        FIELD lv-qty-5  AS INTEGER
        FIELD lv-qty-6  AS INTEGER
        FIELD lv-qty-7  AS INTEGER
        FIELD lv-qty-8  AS INTEGER
        FIELD lv-qty-9  AS INTEGER
        FIELD lv-qty-10 AS INTEGER
        FIELD lv-qty-11 AS INTEGER
        FIELD lv-qty-12 AS INTEGER
        FIELD lv-qty-13 AS INTEGER
        FIELD lv-qty-14 AS INTEGER
        FIELD lv-qty-15 AS INTEGER
        FIELD lv-qty-16 AS INTEGER
        FIELD lv-qty-17 AS INTEGER
        FIELD lv-qty-18 AS INTEGER
        FIELD lv-qty-19 AS INTEGER
        FIELD lv-qty-20 AS INTEGER
        FIELD lv-qty-21 AS INTEGER
        FIELD lv-qty-22 AS INTEGER
        FIELD lv-qty-23 AS INTEGER
        FIELD lv-qty-24 AS INTEGER
        FIELD lv-qty-25 AS INTEGER
        FIELD lv-qty-26 AS INTEGER
        FIELD lv-qty-27 AS INTEGER
        FIELD lv-qty-28 AS INTEGER        
        FIELD lv-rels-1 AS INTEGER
        FIELD lv-rels-2 AS INTEGER
        FIELD lv-rels-3 AS INTEGER
        FIELD lv-rels-4 AS INTEGER
        FIELD lv-rels-5 AS INTEGER
        FIELD lv-rels-6 AS INTEGER
        FIELD lv-rels-7 AS INTEGER
        FIELD lv-rels-8 AS INTEGER
        FIELD lv-rels-9 AS INTEGER
        FIELD lv-rels-10 AS INTEGER
        FIELD lv-rels-11 AS INTEGER
        FIELD lv-rels-12 AS INTEGER
        FIELD lv-rels-13 AS INTEGER
        FIELD lv-rels-14 AS INTEGER
        FIELD lv-rels-15 AS INTEGER
        FIELD lv-rels-16 AS INTEGER
        FIELD lv-rels-17 AS INTEGER
        FIELD lv-rels-18 AS INTEGER
        FIELD lv-rels-19 AS INTEGER
        FIELD lv-rels-20 AS INTEGER
        FIELD lv-rels-21 AS INTEGER
        FIELD lv-rels-22 AS INTEGER
        FIELD lv-rels-23 AS INTEGER
        FIELD lv-rels-24 AS INTEGER
        FIELD lv-rels-25 AS INTEGER
        FIELD lv-rels-26 AS INTEGER
        FIELD lv-rels-27 AS INTEGER
        FIELD lv-rels-28 AS INTEGER
        
        FIELD lv-match-up AS DECIMAL
        FIELD v-do-gsa AS LOGICAL
        FIELD v-do-mr AS LOGICAL
        FIELD v-do-speed AS LOGICAL
        FIELD v-drop-rc AS LOGICAL
        FIELD v-ink-all-forms AS LOGICAL       
        
        .
    DEFINE TEMP-TABLE ttSharedvarable NO-UNDO
       
        FIELD seq-no            AS INT    
        FIELD qty1                AS INT 
        FIELD get-qty             AS INT  
        FIELD ro-id               AS CHAR 
        
        FIELD GsaMat            AS DECIMAL
        FIELD GsaLab            AS DECIMAL
        FIELD GsaWar            AS DECIMAL
        FIELD GsaFm             AS DECIMAL
        FIELD GsaBrd            AS DECIMAL 
        FIELD GsaMonth          AS INT.
       
       


    DEFINE DATASET dsGetqty FOR ttSharedvarable .
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


    DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsGetqty.
    DEFINE OUTPUT PARAMETER cError  AS CHAR NO-UNDO.
    DEFINE OUTPUT PARAMETER prmRecId  AS CHAR NO-UNDO.

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
    IF prmDoGsa = ?    THEN ASSIGN prmDoGsa = "".
    IF prmDoMr = ?     THEN ASSIGN prmDoMr = "".
    IF prmDoSpeed = ?  THEN ASSIGN prmDoSpeed = "".
    IF prmDropRc = ?   THEN ASSIGN prmDropRc = "".
    IF prmInkAlForms = ?  THEN ASSIGN prmInkAlForms = "no".
    IF prmEstList = ?   THEN ASSIGN prmEstList = "".

    IF prmEstimate = ?   THEN ASSIGN prmEstimate = "".
    IF prmForm = ?  THEN ASSIGN prmForm = 0.
    IF prmBlank = ?  THEN ASSIGN prmBlank = 0.
    IF prmLvoverride = ? THEN ASSIGN prmLvoverride = "Yes".
    IF prmVendor     = ? THEN ASSIGN prmVendor = "".

    
/* Local Variable Definitions ---                                       */
def VAR io-do-speed as log no-undo.
def VAR io-do-mr as log no-undo.
def VAR io-do-gsa as log no-undo.
def VAR io-v-drop-rc as log no-undo.
def VAR io-v-match-up as dec no-undo.
def VAR io-ink-all-forms AS LOG NO-UNDO.
def var v-brd-only like sys-ctrl.log-fld init no no-undo.
def var v-brd-cost as dec no-undo.


/*def input parameter lv-mclean as log no-undo.
def output parameter op-error as log no-undo.
*/

    
{cec/print4.i "new shared" "new shared"}
{cec/print42.i "new shared"}
{sys/inc/var.i "new shared"}

def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared var tmp-dir as cha no-undo.

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
 locode = usercomp.loc
 . 

ASSIGN 
    locode = "MAIN".

    FIND first sys-ctrl where
    sys-ctrl.company eq cocode AND
    sys-ctrl.name    eq "CEBROWSE"
    no-lock no-error.

  /*if not avail sys-ctrl then DO TRANSACTION:
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


FIND FIRST est WHERE est.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND est.company = prmComp NO-LOCK NO-ERROR.
FIND FIRST ef WHERE ef.est-no = est.est-no AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
FIND FIRST eb WHERE eb.est-no = est.est-no AND eb.company = prmComp AND eb.form-no = prmForm AND eb.blank-no = prmBlank NO-LOCK NO-ERROR. 


find xest where recid(xest) = recid(est) NO-LOCK NO-ERROR.
find xef where recid(xef) = recid(ef) NO-LOCK NO-ERROR.
find xeb where recid(xeb) = recid(eb) NO-LOCK NO-ERROR.

vprint = YES.

lv-ef-recid = recid(xef).




IF prmAction = "doCalc" THEN DO:
   /* {cec/get-vend.i}   /* get vendor number */*/
    FIND FIRST probe WHERE probe.company = est.company
                       AND probe.est-no = est.est-no
                     NO-LOCK NO-ERROR.
    ASSIGN
       v-vend-no  = prmVendor .

    IF v-vend-no = "&nbsp" THEN
        v-vend-no = "".
       
       
  qtty = 0. 
  do transaction:
     {cec/msfcalc.i}
     {cec/rollfac.i}
  end.
  
    find first ce-ctrl where (ce-ctrl.company = cocode and 
       ce-ctrl.loc     = locode) no-lock no-error.
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
    
  {sys/inc/setsh.i}

  ASSIGN
  save-qty = qty  /* need to check qty value not sure where value is assigned ???? */
  save-lock = xef.op-lock.


  do transaction:
      find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "CEDFAULT"
      no-lock no-error.
    
        if not avail sys-ctrl then do:
            create sys-ctrl.
            assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CEDFAULT"
                sys-ctrl.log-fld = no
                sys-ctrl.descrip = "Ask CERUN & CEGSA log values on Whatif?  " +
                        "No uses saved est. values!".
                /*MESSAGE sys-ctrl.descrip
                    VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
                UPDATE sys-ctrl.log-fld.
                */
        end.
        ll-use-defaults = sys-ctrl.log-fld.

        {est/recalc-mr.i xest}
        FIND CURRENT recalc-mr NO-LOCK.

        {sys/inc/cerun.i C}
        ASSIGN
            do-speed  = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.recalc
            do-mr     = IF ll-use-defaults THEN sys-ctrl.log-fld ELSE (recalc-mr.val[1] EQ 1)
            vmclean   = sys-ctrl.char-fld NE ""
            vsuthrlnd = lookup(sys-ctrl.char-fld,"Suthrlnd,Clevelnd,Brick") ne 0.

        {sys/inc/cerun.i}

        ASSIGN
            v-bqty = sys-ctrl.int-fld
            v-module = module.

        if sys-ctrl.char-fld eq "Brick" then
       assign  v-module = v-module + " - ISO# CS-03-1-F"
               {sys/inc/ctrtext.i "v-module" 60}.
        {sys/inc/ctrtext.i "v-module" 60}.
        find first sys-ctrl where sys-ctrl.company eq cocode
                          and sys-ctrl.name    eq "CEGSA" no-lock no-error.
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
        do-gsa = IF NOT v-gsa THEN do-gsa ELSE
             IF ll-use-defaults THEN sys-ctrl.log-fld ELSE xest.override.

        find first sys-ctrl  where sys-ctrl.company eq cocode
                           and sys-ctrl.name    eq "CESLIT"
         no-lock no-error.
        if not avail sys-ctrl then do:
            create sys-ctrl.
            assign
                sys-ctrl.company = cocode
                sys-ctrl.name    = "CESLIT"
                sys-ctrl.descrip = "Ask 'Drop Slitter...' question at OE cost calculation?"
                sys-ctrl.log-fld = no.
       /*MESSAGE sys-ctrl.descrip
           VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
           UPDATE sys-ctrl.log-fld.
           */
    end.
    v-drop-rc = sys-ctrl.log-fld.
  END.

  
  if vprint then do:
    /* {cec/blkeqsht.i}*/
    
     j = 0.
     EMPTY TEMP-TABLE tt-bqty.
     FOR EACH est-qty NO-LOCK
         WHERE est-qty.company EQ xest.company
           AND est-qty.est-no  EQ xest.est-no:
       DO i = 1 TO 20:
         IF est-qty.qty[i] NE 0 THEN DO:

           j = j + 1.
           CREATE tt-bqty.
           ASSIGN
            tt-bqty = est-qty.qty[i]
            tt-brel = est-qty.qty[i + 20].
           IF v-bqty GT 0             AND
              (j EQ 1 OR v-bqty GT 1) AND
              j + 1 LE EXTENT(qtty)   THEN DO:
             CREATE tt-bqty.
             RUN cec/pr4-bqty.p (v-vend-no, est-qty.qty[i], OUTPUT tt-bqty).
             DO v-count-2 = 1 TO 20:
                IF est-qty.qty[v-count-2] EQ tt-bqty THEN
                DO:
                   tt-brel = est-qty.qty[v-count-2 + 20].
                   LEAVE.
                END.
             END.
           END.
         END.   
       END.  /* do i */ 
     END.
         
     FOR EACH tt-bqty WHERE tt-bqty GT 99999999:
       tt-bqty = 99999999.
     END.

     j = 0.
     FOR EACH tt-bqty BREAK BY tt-bqty:
       IF FIRST-OF(tt-bqty) THEN DO:
         j = j + 1.
         IF j LE EXTENT(qtty) THEN
           ASSIGN
            qtty[j] = tt-bqty
            rels[j] = tt-brel.
       END.
     END.

     {sys/inc/srtqty.i &sub=i &ext=EXTENT(qtty) &qty=qtty &fil=1 &rel=rels}
     FOR EACH tt-qtty:
       DELETE tt-qtty.
     END.
     CREATE tt-qtty.
     DO i = 1 TO EXTENT(qtty):
        assign tt-qtty.qtty[i] = qtty[i]
               tt-qtty.rel[i] = IF qtty[i] EQ 0 THEN 0
                                ELSE
                                IF rels[i] EQ 0 THEN 1
                                ELSE rels[i].
     end.

     v-do-all-forms-ink = NO.

     /*run est/getqty.w (input-output do-speed, input-output do-mr, input-output do-gsa, input-output v-drop-rc,
                       input-output v-match-up, INPUT-OUTPUT v-do-all-forms-ink, input no, output lv-error). 
    */
     RUN InsertQty.

     if lv-error then return error.

     
     IF prmLvoverride = "Yes" THEN
     for each probe where probe.company = xest.company and
                       probe.est-no = xest.est-no:
        delete probe.                 
     end.
  FIND CURRENT tt-qtty NO-LOCK NO-ERROR.
     DO i = 1 TO EXTENT(qtty):
         
        ASSIGN
           qtty[i] = tt-qtty.qtty[i]
           rels[i] = tt-qtty.rel[i].
        
     end.
     {sys/inc/srtqty.i &sub=i &ext=EXTENT(qtty) &qty=qtty &fil=2 &rel=rels}
     DO i = 1 TO EXTENT(qtty):
        if qtty[i] eq 0 then rels[i] = 0.
        else if rels[i] eq 0 then rels[i] = 1.
     end.

  end.   /* vprint */
  else do:
     assign qtty[1] = qty.

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

     find first sys-ctrl  where sys-ctrl.company eq cocode
                            and sys-ctrl.name    eq "FGCOST"
                  no-lock no-error.
     if not avail sys-ctrl then do transaction:
         create sys-ctrl.
         assign  sys-ctrl.company = cocode
                 sys-ctrl.name    = "FGCOST"
                 sys-ctrl.log-fld = no
                 sys-ctrl.descrip = "Create FG Cost in Job File with only Board Cost?".
         /*MESSAGE sys-ctrl.descrip
             VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
             UPDATE sys-ctrl.log-fld. */
     end.
     v-brd-only = sys-ctrl.log-fld.
    
  end.  /* else vprint  */

  find first sys-ctrl  where sys-ctrl.company eq cocode
                            and sys-ctrl.name    eq "FGCOST"
                  no-lock no-error.
  ASSIGN
      v-brd-only = sys-ctrl.log-fld.

  DO TRANSACTION:
    {est/op-lock.i xest}
    /*FIND xef WHERE RECID(xef) EQ lv-ef-recid .
    FIND xef WHERE RECID(xef) EQ lv-ef-recid NO-LOCK.
    */
    FIND est WHERE RECID(est) EQ RECID(xest).
    FIND CURRENT recalc-mr.
    ASSIGN
     est.recalc       = do-speed
     recalc-mr.val[1] = INT(do-mr)
     est.override     = do-gsa 
     op-lock.val[1]   = INT(est.recalc)
     op-lock.val[2]   = recalc-mr.val[1].
    
    FIND est WHERE RECID(est) EQ RECID(xest) NO-LOCK.
    FIND xest WHERE RECID(xest) EQ RECID(est) NO-LOCK.
    FIND CURRENT recalc-mr NO-LOCK.
    FIND CURRENT op-lock NO-LOCK.  
  END.

  find first sman   where   sman.company = cocode and
                          sman.sman    = xeb.sman no-lock no-error.
  find first cust   where   cust.company = cocode and
                          cust.cust-no = xeb.cust-no no-lock no-error.
  find first shipto where shipto.company = cocode and
                          shipto.cust-no = xeb.cust-no and
                        shipto.ship-id = xeb.ship-id no-lock no-error.
  find first style  where  style.company = cocode and
                         style.style = xeb.style no-lock no-error.
  if cust.cust-no ne "Temp" then
     assign cust-ad[1] = cust.name cust-ad[2] = cust.addr[1] cust-ad[3] = cust.addr[2]
            cust-ad[4] = cust.city + ", " + cust.state + " " + cust.zip.
  else
     assign cust-ad[1] = xeb.ship-name
            cust-ad[2] = xeb.ship-addr[1]
            cust-ad[3] = xeb.ship-addr[2]
            cust-ad[4] = xeb.ship-city + ", " + xeb.ship-state + " " + xeb.ship-zip.
   
  if cust-ad[3] eq "" then  assign cust-ad[3] = cust-ad[4]
                                   cust-ad[4] = "".
  if cust-ad[2] eq "" then  assign cust-ad[2] = cust-ad[3]
                                   cust-ad[3] = cust-ad[4]
                                   cust-ad[4] = "".
  assign ship-ad[1] = shipto.ship-name
         ship-ad[2] = shipto.ship-addr[1]
         ship-ad[3] = shipto.ship-addr[2]
         ship-ad[4] = shipto.ship-city + ", " + shipto.ship-state + " " +
                      shipto.ship-zip.
 
  if ship-ad[3] eq "" then assign  ship-ad[3] = ship-ad[4]
                                   ship-ad[4] = "".
  if ship-ad[2] eq "" then assign  ship-ad[2] = ship-ad[3]
                                   ship-ad[3] = ship-ad[4]
                                   ship-ad[4] = "".
  assign dsc[1]     = xeb.part-dscr1 dsc[2] = xeb.part-dscr2
         sizcol[1]  =
         string(int(xeb.len - .499999) + ((xeb.len - int(xeb.len - .499999))
                                      / k_frac)) + "x" +
         string(int(xeb.wid - .499999) + ((xeb.wid - int(xeb.wid - .499999))
                                      / k_frac)) + "x" +
         string( int(xeb.dep - .499999) + ((xeb.dep - int(xeb.dep - .499999))
                                      / k_frac))
         sizcol[2]  = xeb.i-coldscr
         stypart[1] = style.dscr
         stypart[2] = xeb.part-no
         brd-l[1]   = xeb.t-len
         brd-l[2]   = xef.nsh-len
         brd-l[3]   = xef.gsh-len
         brd-w[1]   = xeb.t-wid
         brd-w[2]   = xef.nsh-wid
         brd-w[3]   = xef.gsh-wid.

  if brd-l[3] = 0 and brd-w[3] = 0 then assign brd-l[3] = xef.lsh-len
                                             brd-w[3] = xef.lsh-wid.
  if xef.roll = true then assign brd-w[4] = xef.roll-wid
                               brd-l[4] = sh-len.
  else brd-w[4] = 0.

  assign brd-sq[1] = xeb.t-sqin
         brd-sq[2] = brd-l[2] * brd-w[2]
         brd-sq[3] = brd-l[3] * brd-w[3]
         brd-sq[4] = brd-l[4] * brd-w[4].
  if v-corr then  assign   brd-sf[1] = brd-sq[1] * .007
                           brd-sf[2] = brd-sq[2] * .007
                           brd-sf[3] = brd-sq[3] * .007.
  else assign  brd-sf[1] = brd-sq[1] / 144
               brd-sf[2] = brd-sq[2] / 144
               brd-sf[3] = brd-sq[3] / 144.
               
  do transaction:
     /* take out window if any */
     call_id = recid(xeb).
     find xeb where recid(xeb) = call_id no-error.
     xeb.t-win = 0.
     find xeb where recid(xeb) = call_id no-lock no-error.
  end.
  
  brd-wu[1] = xeb.t-sqin - xeb.t-win.
  find first item where (item.company = cocode) and
                      item.i-no = xef.board no-lock no-error.

  if v-corr then brd-wu[1] = brd-wu[1] * .007.
  else brd-wu[1] = brd-wu[1] / 144.

  assign brd-wu[1] = brd-wu[1] * if avail item then item.basis-w else 1
         brd-wu[2] = brd-sf[2] * if avail item then item.basis-w else 1
         brd-wu[3] = brd-sf[3] * if avail item then item.basis-w else 1
         day_str = string(today,"99/99/9999")
         tim_str = string(time,"hh:mm am") .
         
  /*form day_str
       v-module
       tim_str to 79  skip(1)
       with frame hdr page-top width 80 no-labels no-box stream-io. */

  for each blk:
      delete blk.
  end.
  for each xjob:
      delete xjob.
  end.                           


  /******************** l  o  o  p  **************/
  loupe:
  do k = 1 to EXTENT(qtty) with color value("White/blue"):
    ASSIGN
     v-op-qty = 0
     op-tot   = 0
     dm-tot   = 0
     ctrl2    = 0.

    FOR EACH w-form: 
      DELETE w-form.
    END.
    FOR EACH ink:
      DELETE ink.
    END.

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
        IF est-op.qty GE qtty[k] THEN LEAVE.
      END.
    END.

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

    qty = qtty[k] * IF xeb.yld-qty EQ 0 THEN 1 ELSE xeb.yld-qty.
    if qty = 0 then leave loupe.           
    
    vmcl = k.
    
    {est/probeset.i qtty[k] io-v-match-up}

     maxpage = k.
    run cec/prokalk.p . 
    
    ASSIGN
       k = maxpage  /* k used in kmr-run.i */
       qty = qtty[k] * xeb.yld-qty.

    /*find first xop where xop.company = xest.company and xop.est-no = xest.est-no and xop.line >= 500 no-lock no-error.*/
    find first item where (item.company = cocode) and item.i-no = xef.board no-lock no-error.
    if available item then do:
       find first e-item of item no-lock no-error.

       assign     brd-sf[4] = /*if avail xop then */ brd-sf[3] * /*xop.num-sh*/ xef.gsh-qty   / 1000  /*else 0*/  /* tot msf */
                  brd-wu[4] = (brd-sf[4] * item.basis-w) / 2000. /* tons*/
    end.

    IF probe.LINE LT 100 THEN
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"99").
    ELSE
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"999")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"999")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"999")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"999").

    output to value(outfile1) .

 display day_str FORM "x(10)" v-module tim_str with frame hdr stream-io.

    display "Est#" TRIM(xest.est-no) FORMAT "x(8)"
          "SlsRep:" sman.sname when avail sman
          "UserID:" xest.updated-id
          "Prober:" probe.probe-user
          skip
          "Cust:" xeb.cust-no
                  cust-ad[1] FORMAT "x(29)" TO 44
          "Ship:" ship-ad[1] FORMAT "x(29)" TO 80 SKIP
      with no-labels no-box frame qwqw STREAM-IO.

    if cust-ad[2] ne "" or ship-ad[2] ne "" then
      put cust-ad[2] FORMAT "x(29)" TO 44
          ship-ad[2] FORMAT "x(29)" TO 80 SKIP.
    if cust-ad[3] ne "" OR ship-ad[3] ne "" then
      put cust-ad[3] FORMAT "x(29)" TO 44
          ship-ad[3] FORMAT "x(29)" TO 80 SKIP.
    if cust-ad[4] ne "" OR ship-ad[4] ne "" then
      put cust-ad[4] FORMAT "x(29)" TO 44
          ship-ad[4] FORMAT "x(29)" TO 80 SKIP.

    display /*skip(1)*/
            " --Qty---- --- Description ------ -- Size / Color ----- --- Style / Part No ---"
            qty / xeb.yld-qty format ">>,>>>,>>9"
            dsc[1] space(1) sizcol[1] space(2) stypart[1] skip
            space(11)
            dsc[2] space(1) sizcol[2] space(2) stypart[2] skip
            space(56) "Last Ordered:" xest.ord-date
            skip(1)
            with no-box no-labels  stream-io width 80 frame aa1 .

    ASSIGN
     v-yld-qty   = xeb.yld-qty
     v-hdr-depth = IF xeb.t-dep   EQ 0 AND
                      xef.nsh-dep EQ 0 AND
                      xef.gsh-dep EQ 0 THEN "" ELSE "Depth".

    RUN est/ef-#out.p (ROWID(xef), OUTPUT v-n-out).

    display space(13)
            "   Width  Length  "
            v-hdr-depth
            "    #On Qty/Set      Sq.Feet     Wgt/Units"
            skip
            "  Blank Size:"
            brd-w[1]                            format ">>>9.99<<<"
            brd-l[1]                            format ">>>9.99<<<" 
            xeb.t-dep WHEN xeb.t-dep NE 0       format ">>>9.99<<<"
            1                                   format ">>>,>>>" 
            v-yld-qty
            brd-sf[1]                              
            "Sf/BL"
            brd-wu[1]
            space(0)
            "/MBL" skip

            " NetSht Size:"
            brd-w[2]                            format ">>>9.99<<<"
            brd-l[2]                            format ">>>9.99<<<"
            xef.nsh-dep WHEN xef.nsh-dep NE 0   format ">>>9.99<<<"
            xeb.num-up                          format ">>>,>>9"
            SPACE(9)
            brd-sf[2]
            "Sf/NS"
            brd-wu[2]
            space(0)
            "/MNS"
            skip

            " GrsSht Size:"
            brd-w[3]                            format ">>>9.99<<<"
            brd-l[3]                            format ">>>9.99<<<"
            xef.gsh-dep WHEN xef.gsh-dep NE 0   format ">>>9.99<<<"
            v-n-out                             format ">>>,>>9"
            SPACE(9)
            brd-sf[3]
            "Sf/GS"
            brd-wu[3]
            space(0)
            "/MGS" skip

        with stream-io no-box no-labels color value("blu/brown") width 80 frame aa2. 

    IF v-yld-qty LT 0 THEN DO WITH FRAME aa:
      ASSIGN
       v-yld-qty        = -1 / v-yld-qty
       v-yld-qty:FORMAT = ">>>>9.9<<<<".

      DISPLAY v-yld-qty.
    END.

    IF NOT vsuthrlnd THEN DO WITH FRAME aa2:
       ASSIGN
        brd-w[1]:FORMAT    = ">>>9.99"
        brd-l[1]:FORMAT    = ">>>9.99"
        xeb.t-dep:FORMAT   = ">>>9.99"
        brd-w[2]:FORMAT    = ">>>9.99"
        brd-l[2]:FORMAT    = ">>>9.99"
        xef.nsh-dep:FORMAT = ">>>9.99"
        brd-w[3]:FORMAT    = ">>>9.99"
        brd-l[3]:FORMAT    = ">>>9.99"
        xef.gsh-dep:FORMAT = ">>>9.99".

       display {sys/inc/k16v.i brd-w[1]} @ brd-w[1]
               {sys/inc/k16v.i brd-l[1]} @ brd-l[1]
               "" @ xeb.t-dep
               {sys/inc/k16v.i xeb.t-dep} WHEN xeb.t-dep NE 0 @ xeb.t-dep
               {sys/inc/k16v.i brd-w[2]} @ brd-w[2]
               {sys/inc/k16v.i brd-l[2]} @ brd-l[2]
               "" @ xef.nsh-dep
               {sys/inc/k16v.i xef.nsh-dep} WHEN xef.nsh-dep NE 0 @ xef.nsh-dep
               {sys/inc/k16v.i brd-w[3]} @ brd-w[3]
               {sys/inc/k16v.i brd-l[3]} @ brd-l[3]
               "" @ xef.gsh-dep
               {sys/inc/k16v.i xef.gsh-dep} WHEN xef.gsh-dep NE 0 @ xef.gsh-dep.
    END.

    if brd-w[4] ne 0 then
       display     "  Roll Size:" brd-w[4]                format ">>9.99<<" to 22
                with stream-io no-box no-labels  width 80 frame aa3.

    if not vsuthrlnd THEN DO WITH FRAME aa3:
       brd-w[4]:FORMAT = ">>>9.99".

       if brd-w[4] ne 0 then
          display {sys/inc/k16v.i brd-w[4]} @ brd-w[4].
    END.
    display brd-sf[4] TO 52 "MSF"
            brd-wu[4] TO 70 "Tons"
        with frame aa3.

    assign  v-fac-hdr = "Mat$/M" + (if v-rollfac then "SF" else "")
            v-fac-hdr = fill(" ",8 - length(trim(v-fac-hdr))) + trim(v-fac-hdr).

    display skip(1) "Materials            Weight  Vendor          QTY/Unit  SU $"
             v-fac-hdr space(7) "TOTAL" skip
             with stream-io no-box no-labels color value("blu/brown") width 80 frame aa4.
    maxpage = k.

    IF NOT do-gsa THEN DO:
    
    /* board    */  run cec/pr4-brd.p (v-vend-no, OUTPUT v-vend-list).
    v-brd-cost = dm-tot[5].

    /* adders   */  run cec/pr4-add.p (v-vend-list).

    END.

    FIND CURRENT probe-board NO-ERROR.
    IF AVAIL probe-board THEN
      probe-board.val[1] = probe-board.val[1] + dm-tot[5].
    
    FIND CURRENT probe-board NO-LOCK NO-ERROR.
    
    /* i n k s  */  run cec/pr4-ink.p (v-vend-no).

    /* films    */  run cec/pr4-flm.p (v-vend-no).

    /* cs/tr/pal*/  run cec/pr4-cas.p (v-vend-no).

    /* special  */  run cec/pr4-spe.p (v-vend-no).    

    do with frame ac5 no-labels no-box:
       display "TOTAL  DIRECT  MATERIALS "
              dm-tot[5] / (qty / 1000) / v-sqft-fac format ">>,>>9.99" to 68
              dm-tot[5] format ">>>>,>>9.99"                           to 80
              skip(1) with stream-io.
    end.

    run cec/pr4-prp.p.
    run cec/pr4-mis.p.
    k = maxpage.


    FIND FIRST xeb WHERE
         xeb.company EQ xest.company AND
         xeb.est-no EQ xest.est-no AND
         xeb.form-no NE 0
         NO-LOCK NO-ERROR.
    
    run cec/pr4-mch.p.  

    if v-gsa then do:
       /* mat */
       DO i = 1 TO EXTENT(ce-ctrl.mat-pct):
          ctrl[9] = ce-ctrl.mat-pct[i] / 100.
          if ce-ctrl.mat-cost[i] > dm-tot[5] + tprep-mat 
          then leave.
       end.
       /* lab */
       DO i = 1 TO EXTENT(ce-ctrl.lab-pct):
          ctrl[10] = ce-ctrl.lab-pct[i] / 100.
          if ce-ctrl.lab-cost[i] > op-tot[5] + tprep-lab
          then leave.
       end.
    end.
    DO TRANSACTION:
      {est/calcpcts.i xest}
      IF v-gsa THEN
        ASSIGN
         calcpcts.val[1] = ctrl[9] * 100
         calcpcts.val[2] = v-brd-cost.
      FIND CURRENT calcpcts NO-LOCK NO-ERROR.
    END.

    assign  gsa-mat = ctrl[9]  * 100
            gsa-lab = ctrl[10] * 100
            gsa-com = ce-ctrl.comm-mrkup
            gsa-war = ctrl[1] * 100.

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
    ELSE IF AVAIL cust AND cust.scomm NE 0 THEN
       gsa-fm = cust.scomm.
    ELSE
       gsa-fm = ctrl[19]. 

        output close.

    DEFINE VAR li-qty AS INT NO-UNDO.
       FOR EACH brd
           WHERE (brd.form-no EQ v-form-no OR (NOT vmclean2))
             AND CAN-FIND(FIRST ITEM WHERE item.company  EQ cocode
                                       AND item.i-no     EQ brd.i-no
                                       AND item.mat-type EQ "D"):
      
         li-qty = li-qty + brd.qty.
       END.
       
/**************************/
       CREATE ttSharedvarable .
           ASSIGN
           ttSharedvarable.seq-no      =  probe.LINE   
           ttSharedvarable.qty1        =  qty    
           ttSharedvarable.get-qty     =  probe.est-qty   
           ttSharedvarable.ro-id       = STRING(ROWID(probe))
          
           ttSharedvarable.GsaMat        =  gsa-mat
           ttSharedvarable.GsaLab        =  gsa-lab
           ttSharedvarable.GsaWar        =  gsa-war
           ttSharedvarable.GsaFm         =  gsa-fm
           ttSharedvarable.GsaMonth      =  li-qty
                 .
           
           
    assign
    ctrl[9]  = gsa-mat / 100
    ctrl[10] = gsa-lab / 100
    ctrl[1]  = gsa-war / 100
    ctrl[19] = gsa-fm / 100
    v-prep-mat = tprep-mat
    v-prep-lab = tprep-lab.

  IF NOT do-gsa THEN DO:
   
   run cec/pr4-tots-copy.p.

   run cec/probemk.p (ROWID(probe)).
   
   find first blk where blk.id eq xeb.part-no no-error.
   find first xjob
        where xjob.i-no eq blk.id
          and xjob.qty  eq qtty[k]
        no-error.
   if not available xjob then do:
     create xjob.
     assign
      xjob.i-no     = blk.id
      xjob.qty      = qtty[k]
      xjob.cust-no  = xeb.cust-no
      xjob.form-no  = xeb.form-no
      xjob.blank-no = xeb.blank-no
      xjob.pct      = 1.00
      xjob.stock-no = xeb.stock-no.
   end.

   if v-brd-only then
     assign
      xjob.mat = v-brd-cost / (qtty[k] / 1000)
      xjob.lab = 0
      xjob.voh = 0
      xjob.foh = 0
      ord-cost = v-brd-cost.

   else
     assign
      xjob.mat = (dm-tot[5] + mis-tot[1] + v-prep-mat) / (qtty[k] / 1000)
      xjob.lab = (opsplit$[1] + mis-tot[3] + v-prep-lab) /
                 (qtty[k] / 1000)
      xjob.voh = opsplit$[2] / (qtty[k] / 1000)
      xjob.foh = opsplit$[3] / (qtty[k] / 1000).

     IF probe.LINE LT 100 THEN
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"99")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"99")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"99")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"99")
              ls-outfile = tmp-dir + trim(est.est-no) + ".p" + string(probe.line,"99").
    ELSE
       assign outfile1 = tmp-dir + trim(xest.est-no) + ".v" + string(probe.line,"999")
              outfile2 = tmp-dir + trim(xest.est-no) + ".a" + string(probe.line,"999")
              outfile3 = tmp-dir + trim(xest.est-no) + ".s" + string(probe.line,"999")
              outfile4 = tmp-dir + trim(xest.est-no) + ".z" + string(probe.line,"999")
              ls-outfile = tmp-dir + trim(est.est-no) + ".p" + string(probe.line,"999").

    if search(outfile1) <> ? then do:       
       dos silent  type value(outfile3) > value(ls-outfile).
       dos silent  type value(outfile2) >> value(ls-outfile).    
    end.
    else next.

   /*if vprint then run cec/pr4-mcl.p.*/
     END.

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
         v-drop-rc       = LOGICAL(prmDropRc)
         v-do-all-forms-ink   = NO 
         v-match-up      = prmMatchup
         io-do-speed     = LOGICAL(prmDoSpeed) 
         io-do-mr        = LOGICAL(prmDoMr)
         io-v-match-up   = prmMatchup 
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









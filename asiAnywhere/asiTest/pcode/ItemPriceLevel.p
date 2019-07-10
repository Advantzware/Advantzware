            /*------------------------------------------------------------------------
    File        : ItemPriceLevel.p
    Purpose     : Cust Part

    Syntax      :

    Description : Return a Dataset of all AdderLook

    Author(s)   : 
    Created     : Nov 23 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttItemPriceLevel NO-UNDO 
    FIELD Level      AS INTEGER
    FIELD Add1  LIKE item.i-name.

DEFINE DATASET dsItemPriceLevel FOR ttItemPriceLevel.

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOrder      AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmLevel     AS INTEGER  NO-UNDO.
DEFINE INPUT PARAMETER prmComp      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmLine      AS INT NO-UNDO.
DEFINE OUTPUT PARAMETER cError AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsItemPriceLevel.
       
IF prmAction = ? THEN ASSIGN prmAction = "".
IF prmOrder  = ? THEN ASSIGN prmOrder  = 0.
IF prmLevel  = ? THEN ASSIGN prmLevel  = 0.
IF prmComp   = ? THEN ASSIGN prmComp   = "".

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
   {sys/inc/var.i new shared }

def new shared buffer xoe-ord for oe-ord.
def var lv-tmp-recid as recid no-undo.
DEF VAR ld-prev-t-price LIKE oe-ordl.t-price NO-UNDO.
define new shared var v-price-lev as int no-undo.
define new shared var v-i-qty like oe-ordl.qty no-undo. /* INPUT QUANTITY */
define new shared var v-procat like oe-prmtx.procat no-undo. /* ITEM CATEGORY */
DEFINE NEW Shared variable g_company AS CHAR NO-UNDO.
DEFINE NEW Shared variable g_loc AS CHAR NO-UNDO.

DEF BUFFER b-job-hdr FOR job-hdr.

{ce/print4.i "new shared"}
{ce/print42.i "new shared"}

def new shared var lv-qty as int no-undo.
def new shared var qty as INT NO-UNDO.
DEF NEW SHARED VAR v-shared-rel AS INT NO-UNDO.

ASSIGN
    cocode = prmComp 
    locode = "MAIN"
    g_loc = locode 
    g_company = prmComp .


IF prmAction = "Level" THEN DO:
    MESSAGE "level" prmLevel prmOrder prmComp .

    find xoe-ord where xoe-ord.company = PrmComp and
                     xoe-ord.ord-no = prmOrder no-lock no-error.
     FIND FIRST oe-ord WHERE oe-ord.company = prmComp AND oe-ord.ord-no = prmOrder NO-LOCK NO-ERROR . 

    FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrder 
             AND oe-ordl.LINE = prmLine NO-LOCK NO-ERROR .

  assign v-i-qty = 0
         v-price-lev = 0.
  /* Get appropriate level */
  /*run oe/oe-level.p.*/
 
  ASSIGN 
   v-price-lev =    prmLevel .

   ASSIGN
    lv-tmp-recid    = RECID(oe-ordl)
    ld-prev-t-price = oe-ordl.t-price.
   
   run oe/oe-repr1.p.
  /* {&open-query-{&browse-name}}
   reposition {&browse-name} to recid lv-tmp-recid.*/

   RUN oe/calcordt.p (ROWID(oe-ord)).

   IF ld-prev-t-price NE oe-ordl.t-price THEN RUN oe/creditck.p (ROWID(oe-ord), YES).

   ASSIGN prmAction = "Select" .

END.

IF prmAction = "Select" THEN DO:
    
   
    find xoe-ord where xoe-ord.company = PrmComp and
                     xoe-ord.ord-no = prmOrder no-lock no-error.
    FIND FIRST oe-ordl WHERE oe-ordl.company = prmComp AND oe-ordl.ord-no = prmOrder 
             AND oe-ordl.LINE = prmLine NO-LOCK NO-ERROR .

  assign v-i-qty = 0
         v-price-lev = 0.
  /* Get appropriate level */
  run oe/oe-level.p.
 
  CREATE ttItemPriceLevel.
    ASSIGN 
        ttItemPriceLevel.Level  = v-price-lev .
END.

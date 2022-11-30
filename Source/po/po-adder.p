/* ---------------------------------------------------------------------------*/
/* po detail line - add adder cost to board                                   */
/* ---------------------------------------------------------------------------*/

def input parameter v-recid  as recid.
def input parameter v-recid1 as recid.

{sys/inc/var.i shared}
{sys/inc/venditemcost.i}
def var v-tot-cost as dec no-undo.
def var v-cost     as dec no-undo.
def var v-add-cost as dec no-undo.
def var v-qty-comp as dec no-undo.
def var v-setup like e-item-vend.setup no-undo.

DEF VARIABLE dadder-setup AS DECIMAL NO-UNDO .

def shared var v-part-dscr1 as char format "x(30)".
def shared var v-part-dscr2 as char format "x(30)".
def shared var v-basis-w like item.basis-w no-undo.
def shared var v-len like item.s-len no-undo.
def shared var v-wid like item.s-wid no-undo.
def shared var v-dep like item.s-dep no-undo.
def shared var v-gl-desc as char format "x(30)" no-undo.
def shared var v-tot-msf as dec format ">>,>>9.999" init 0 no-undo.
def shared var v-adder as dec extent 2.
def shared var v-po-qty as log initial true no-undo.

DEFINE VARIABLE dCostTotal  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostPerUOM AS DECIMAL   NO-UNDO.
DEFINE VARIABLE dCostSetup  AS DECIMAL   NO-UNDO.
DEFINE VARIABLE cCostUOM    AS CHARACTER NO-UNDO.
DEFINE VARIABLE lError      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cMessage    AS CHARACTER NO-UNDO.
DEFINE VARIABLE hdPOProcs   AS HANDLE    NO-UNDO.
DEFINE VARIABLE cVendorItemID AS CHARACTER NO-UNDO.

def buffer xjob-mat for job-mat.


DEF TEMP-TABLE tt-eiv NO-UNDO
    FIELD run-qty AS DEC DECIMALS 3 EXTENT 20
    FIELD run-cost AS DEC DECIMALS 4 EXTENT 20
    FIELD setups AS DEC DECIMALS 2 EXTENT 20.

RUN po/POProcs.p PERSISTENT SET hdPOProcs.

find xjob-mat where recid(xjob-mat) eq v-recid1 no-lock.

find po-ordl where recid(po-ordl) eq v-recid exclusive-lock.
FIND FIRST po-ord WHERE
       po-ord.company EQ po-ordl.company AND
       po-ord.po-no EQ po-ordl.po-no
       NO-LOCK.

assign
 v-adder[1] = po-ordl.cost
 v-adder[2] = po-ordl.cons-cost.

do with frame po-ordlf:
  for each job-mat no-lock
      where job-mat.company  eq xjob-mat.company
        and job-mat.job      eq xjob-mat.job
        and job-mat.frm      eq xjob-mat.frm
        and job-mat.job-no   eq xjob-mat.job-no
        and job-mat.job-no2  eq xjob-mat.job-no2
      use-index seq-idx,

      first item no-lock
      where item.company  eq job-mat.company
        and item.i-no     eq job-mat.i-no
        and item.mat-type eq "A":
    ASSIGN 
        v-cost  = 0
        v-setup = 0
        .
    IF lNewVendorItemCost THEN DO:     
        RUN GetVendorCost(
            INPUT  po-ordl.company, 
            INPUT  item.i-no, 
            INPUT  "RM", 
            INPUT  po-ord.vend-no, 
            INPUT  po-ord.cust-no, 
            INPUT  "", 
            INPUT  0, 
            INPUT  0,
            INPUT  po-ordl.ord-qty, 
            INPUT  po-ordl.pr-qty-uom,
            INPUT  po-ordl.s-len,
            INPUT  po-ordl.s-wid, 
            INPUT  po-ordl.s-dep,
            INPUT  "IN", 
            INPUT  item.basis-w, 
            INPUT  "LB/EA", 
            INPUT  YES,
            OUTPUT dCostPerUOM, 
            OUTPUT dCostSetup, 
            OUTPUT cCostUOM,
            OUTPUT dCostTotal, 
            OUTPUT cVendorItemID,
            OUTPUT lError, 
            OUTPUT cMessage).  
            
        ASSIGN 
            v-cost  = dCostPerUOM
            v-setup = dCostSetup
            .  
    END.
    
    ELSE DO:
        find first e-item no-lock
            where e-item.company eq po-ordl.company
              and e-item.i-no    eq po-ordl.i-no
            no-error.
        
        find first e-item-vend no-lock
            where e-item-vend.company eq item.company
              and e-item-vend.i-no    eq item.i-no
              and e-item-vend.vend-no eq po-ord.vend-no
            no-error.
    END.   
    if avail e-item and avail e-item-vend AND po-ord.vend-no NE "" then do:
      if po-ordl.pr-qty-uom eq e-item.std-uom then
         v-qty-comp = po-ordl.ord-qty.
      else
         run sys/ref/convquom.p(po-ordl.pr-qty-uom, e-item.std-uom,
                                v-basis-w, v-len, v-wid, v-dep,
                                po-ordl.ord-qty, output v-qty-comp).

      v-setup = 0.

      EMPTY TEMP-TABLE tt-eiv.
      CREATE tt-eiv.
      DO i = 1 TO 10:
         ASSIGN
            tt-eiv.run-qty[i] = e-item-vend.run-qty[i]
            tt-eiv.run-cost[i] = e-item-vend.run-cost[i]
            tt-eiv.setups[i] = e-item-vend.setups[i].
      END.
      
      IF AVAIL e-item-vend THEN
      DO:

      
         DO i = 1 TO 10:
            ASSIGN
               tt-eiv.run-qty[i + 10] = e-item-vend.runQtyXtra[i]
               tt-eiv.run-cost[i + 10] = e-item-vend.runCostXtra[i]
               tt-eiv.setups[i + 10] = e-item-vend.setupsXtra[i].
         END.
      END.

      do i = 1 to 20:
         if v-qty-comp le tt-eiv.run-qty[i] then
         leave.
      end.
    
      ASSIGN
         v-setup = tt-eiv.setups[i]
         v-cost = /*((*/ tt-eiv.run-cost[i] /** v-qty-comp) + v-setup ) / v-qty-comp*/
         dadder-setup = dadder-setup + v-setup .
      /* This adds the Adder cost in */
      IF e-item.std-uom NE po-ordl.pr-uom THEN
        RUN sys/ref/convcuom.p(e-item.std-uom, po-ordl.pr-uom, job-mat.basis-w,
                               job-mat.len, job-mat.wid, item.s-dep,
                               v-cost, OUTPUT v-cost).
    END.
    FIND FIRST po-ordl-add NO-LOCK 
         WHERE po-ordl-add.company    EQ po-ordl.company
           AND po-ordl-add.po-no      EQ po-ordl.po-no  
           AND po-ordl-add.line       EQ po-ordl.line   
           AND po-ordl-add.adder-i-no EQ job-mat.i-no 
         NO-ERROR. 
    IF AVAILABLE po-ordl-add THEN 
        RUN PO_UpdatePoAdders IN hdPOProcs(
            INPUT po-ordl.company,
            INPUT po-ordl.po-no,
            INPUT po-ordl.line,
            INPUT job-mat.i-no,
            INPUT v-cost,
            INPUT v-setup,
            INPUT po-ordl.pr-uom
            ). 
     ELSE
        RUN PO_CreatePoAdders IN hdPOProcs(
            INPUT po-ordl.company,
            INPUT po-ordl.po-no,
            INPUT po-ordl.line,
            INPUT job-mat.i-no,
            INPUT v-cost,
            INPUT v-setup,
            INPUT po-ordl.pr-uom
            ).
    ASSIGN 
        v-add-cost   = v-add-cost + v-cost
        dadder-setup = dadder-setup + v-setup
        .
  END.
  ASSIGN
   po-ordl.cost      = po-ordl.cost + v-add-cost
   po-ordl.cons-cost = po-ordl.cost
   po-ordl.setup     = po-ordl.setup + dadder-setup .

  IF po-ordl.pr-uom NE po-ordl.cons-uom THEN
    RUN sys/ref/convcuom.p(po-ordl.pr-uom, po-ordl.cons-uom,
                           v-basis-w, v-len, v-wid, v-dep,
                           po-ordl.cost, OUTPUT po-ordl.cons-cost).
end.

assign
 v-adder[1] = po-ordl.cost      - v-adder[1]
 v-adder[2] = po-ordl.cons-cost - v-adder[2].
 
IF VALID-HANDLE(hdPOProcs) THEN 
    DELETE PROCEDURE hdPOProcs.

/* end ---------------------------------- copr. 1998  advanced software, inc. */

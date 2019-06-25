/*------------------------------------------------------------------------
    File        : FoldRouteVal.p
    Purpose     :folding Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 13  may 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */


DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmSnum         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBnum         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmMcode        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMdscr        AS CHAR NO-UNDO.

DEFINE INPUT PARAMETER prmNout         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmOpmr         AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmOpwaste      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmOpspeed      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmOpspoil      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmOpcrew       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmOpcrew2      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmOpRate       AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmOpRate2      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmPlates       AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER prmFountains    AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmInk          AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmCoat         AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmLine         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmQty          AS INT NO-UNDO.


DEFINE OUTPUT PARAMETER cError AS CHARACTER NO-UNDO. 

IF prmUser       = ?  THEN ASSIGN    prmUser        = "".
IF prmAction     = ?  THEN ASSIGN    prmAction      = "".
IF prmComp       = ?  THEN ASSIGN    prmComp        = "".
IF prmEstNum     = ?  THEN ASSIGN    prmEstNum      = "".
IF prmFormNo     = ?  THEN ASSIGN    prmFormNo      = 0.

IF prmSnum       = ?  THEN ASSIGN    prmSnum        = 0.
IF prmBnum       = ?  THEN ASSIGN    prmBnum        = 0.
IF prmMcode      = ?  THEN ASSIGN    prmMcode       = "".
IF prmMdscr      = ?  THEN ASSIGN    prmMdscr       = "".
IF prmNout       = ?  THEN ASSIGN    prmNout        = 0.
IF prmOpmr       = ?  THEN ASSIGN    prmOpmr        = 0.
IF prmOpwaste    = ?  THEN ASSIGN    prmOpwaste     = 0.
IF prmOpspeed    = ?  THEN ASSIGN    prmOpspeed     = 0.
IF prmOpspoil    = ?  THEN ASSIGN    prmOpspoil     = 0.
IF prmOpcrew     = ?  THEN ASSIGN    prmOpcrew      = 0.
IF prmOpcrew2    = ?  THEN ASSIGN    prmOpcrew2     = 0.
IF prmOpRate     = ?  THEN ASSIGN    prmOpRate      = 0.
IF prmOpRate2    = ?  THEN ASSIGN    prmOpRate2     = 0.
IF prmPlates     = ?  THEN ASSIGN    prmPlates      = 0.
IF prmFountains  = ?  THEN ASSIGN    prmFountains   = 0.
IF prmInk        = ?  THEN ASSIGN    prmInk         = 0.
IF prmCoat       = ?  THEN ASSIGN    prmCoat         = 0.

IF prmLine       = ?  THEN ASSIGN    prmLine        = 0.
IF prmQty        = ?  THEN ASSIGN    prmQty         = 0.

{ce/mach-ink.i new}

def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
DEF BUFFER op-lock FOR reftable.
def buffer xop for est-op.

def new shared var xcal    as de no-undo.
def new shared var sh-wid  as de no-undo.
def new shared var sh-len  as de no-undo.
def new shared var fil_id  as recid no-undo.
def new shared var maxco   as int no-undo.
def new shared var qty     as int no-undo.
def NEW shared var v-chk-qty as dec no-undo.
def NEW shared var v-sht-qty as dec no-undo.
def NEW shared var v-rc-seq as int init 9999 no-undo.
DEF NEW SHARED VAR cocode  AS CHAR NO-UNDO.
DEF NEW SHARED VAR locode  AS CHAR NO-UNDO.
def NEW SHARED var  x  as   int no-undo.
def NEW SHARED var  y  as   int no-undo.
DEF NEW SHARED VAR  k  as   int no-undo.

def var z          as   int no-undo.
def var xxx        as   dec no-undo.
def var yyy        as   dec no-undo.
def var zzz        as   dec no-undo.
def var tmpstore   as   cha no-undo.

DEFINE VAR vEstimate AS CHAR NO-UNDO.
DEFINE VAR prmLoc AS CHAR NO-UNDO.
DEF VAR lv-eqty LIKE est-qty.eqty NO-UNDO.
def var ll-import-stds as log no-undo.
def var lv-d-seq like est-op.d-seq no-undo.
def var lv-dept like est-op.dept no-undo.
def var lv-op-sb like est-op.op-sb no-undo.
def var lv-b-num like est-op.b-num no-undo.
def var lv-n-out like est-op.n-out no-undo.
def var v-passes   as   int no-undo.
def var ll-machine-modified as log no-undo.
DEF VAR ll-import-selected AS LOG NO-UNDO.
DEF VAR ll-import-all AS LOG NO-UNDO.
def var v-avail as log no-undo.
DEF VAR lv-qty LIKE est-op.qty NO-UNDO.
DEF VAR li-cnt AS INT NO-UNDO.
DEF VAR lv-foam-depts AS CHAR INIT "DC,RC" NO-UNDO.
DEF VAR lv-n-out-depts AS CHAR INIT "CR,RC" NO-UNDO.
DEF VAR prev-m-code LIKE est-op.m-code NO-UNDO.
DEF VAR ll-foam AS LOG NO-UNDO.

DEFINE VAR i LIKE est-op.LINE NO-UNDO.

 def var j as int no-undo.
  def var v-outw     like xef.n-out no-undo.
  def var v-outl     like xef.n-out-l no-undo.
  def var v-rate     like est-op.op-rate no-undo.
  def var v-recid    as   recid no-undo.

/*******procdu*********/
def var chr-handle as char no-undo.
  def var ls-tmp as cha no-undo.
  def var v-run as dec no-undo.
  def var v-on-f as int no-undo.
  DEF VAR sh-dep AS DEC NO-UNDO.
  DEF VAR li-aqueous AS INT NO-UNDO.

  DEF BUFFER b-est-op FOR est-op.
  DEF BUFFER b-mach FOR mach.


    FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_DEFault = YES
     NO-LOCK NO-ERROR.
prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".
prmLoc  =  "MAIN" .
ll-import-all = FALSE.
cocode = prmComp.
locode = prmLoc.

vEstimate =  FILL(" ",8 - LENGTH(TRIM(prmEstNum))) + TRIM(prmEstNum).


/*************************************prmAction***************************************************/  

IF prmAction = "RouteAdd"  THEN DO:
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST est-qty WHERE est-qty.est-no = vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
    FIND first ef  where ef.company eq est.company
                      and ef.est-no  eq est.est-no
                      and ef.form-no eq prmSnum NO-LOCK NO-ERROR.
    IF NOT AVAIL ef then do:
      cError = "Must enter a valid Form#" .
      return .
    end.
   

    
   find first mach  where mach.company eq prmComp and  mach.loc  eq prmLoc
          and mach.m-code eq prmMcode  no-lock no-error.

    IF NOT AVAIL mach THEN DO:
      cError = "Must enter a valid Machine Code, try help" .
    END.

    IF CAN-FIND(FIRST reftable
                WHERE reftable.reftable EQ "mach.obsolete"
                  AND reftable.company  EQ mach.company
                  AND reftable.loc      EQ mach.loc
                  AND reftable.code     EQ mach.m-code
                  AND reftable.val[1]   EQ 1) THEN DO:
      cError = "Machine is obsolete, please choose a different machine" .
      RETURN .
    END.
    RUN valid-mach NO-ERROR.

END.  /****validation******/



IF prmAction = "RouteUpdate" THEN DO:

   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST est-qty WHERE est-qty.est-no = vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
    FIND first ef  where ef.company eq est.company
                      and ef.est-no  eq est.est-no
                      and ef.form-no eq prmSnum NO-LOCK NO-ERROR.
    IF NOT AVAIL ef then do:
        ASSIGN
      cError = "Must enter a valid Form#" .
      return .
    end.
    
   
  RUN valid-mach NO-ERROR. 
  
  
END. /*end of validation*/


 

/**********************procedure*******************************/

 PROCEDURE valid-mach :

  def var i as int no-undo.
  def var chr-handle as char no-undo.
  def var ls-tmp as cha no-undo.
 DEF VAR ld AS DEC NO-UNDO.

  DEF BUFFER xop FOR est-op.  
  DEF BUFFER b-ef FOR ef.
  DEF BUFFER b-eb FOR eb.
  DEF BUFFER b-mach FOR mach.
  
    FIND FIRST mach where (mach.company eq cocode
                           and  mach.loc     eq locode)
        AND mach.m-code EQ prmMcode NO-LOCK NO-ERROR.

    MESSAGE "test" prmMcode prmSnum est-qty.company est-qty.est-no.

    IF NOT AVAIL mach THEN cError = "Must enter a valid Machine Code, try help".

    IF cError EQ "" THEN
      IF CAN-FIND(FIRST reftable
                  WHERE reftable.reftable EQ "mach.obsolete"
                    AND reftable.company  EQ mach.company
                    AND reftable.loc      EQ mach.loc
                    AND reftable.code     EQ mach.m-code
                    AND reftable.val[1]   EQ 1) THEN
        cError = "Machine is obsolete, please choose a different machine".

    IF cError EQ "" THEN DO:
      FIND FIRST b-ef
          WHERE b-ef.company EQ est-qty.company
            AND b-ef.est-no  EQ est-qty.est-no
            AND b-ef.form-no EQ prmSnum
          NO-LOCK NO-ERROR.

      FIND FIRST b-eb
          WHERE b-eb.company  EQ b-ef.company
            AND b-eb.est-no   EQ b-ef.est-no
            AND b-eb.form-no  EQ b-ef.form-no
           /* AND b-eb.blank-no EQ 1 */
          NO-LOCK NO-ERROR.
      IF NOT AVAIL b-eb THEN
      FIND FIRST b-eb
          WHERE b-eb.company  EQ b-ef.company
            AND b-eb.est-no   EQ b-ef.est-no
            AND b-eb.form-no  EQ b-ef.form-no
          NO-LOCK NO-ERROR.

      find xest where recid(xest) eq recid(est) no-lock.
      lv-d-seq = mach.d-seq.
      lv-dept  = mach.dept[1].

      IF LOOKUP(lv-dept,"RC,GU") NE 0 AND AVAIL b-ef                     AND
        /* (adm-adding-record OR*/
          (prmNout EQ 0) THEN
        prmNout = IF lv-dept EQ "RC" THEN (b-ef.n-out) ELSE (b-ef.n-out-l).
  
      if mach.p-type ne "B" then
        assign
         lv-op-sb = yes
         lv-b-num = 0.
       
      else
        assign
         lv-op-sb = no
         lv-b-num = if prmBnum eq 0 then 1 else prmBnum.
       
      if lookup(lv-dept,"RC,GU") eq 0 then lv-n-out = 0.

      IF lv-dept EQ "PR" THEN DO:
        RUN ce/mach-ink.p.

        /*i = INT(adm-new-record).*/
        i = 0.
        FOR EACH xop
            WHERE xop.company EQ est-op.company
              AND xop.est-no  EQ est-op.est-no
              AND (xop.qty    EQ est-op.qty OR est.est-type GE 2)
              AND xop.s-num   EQ b-ef.form-no
              AND xop.line    LT 500
              AND (ROWID(xop) NE ROWID(est-op) /*OR NOT adm-new-record*/  )
              AND CAN-FIND(FIRST b-mach
                           WHERE b-mach.company EQ xop.company
                             AND b-mach.m-code  EQ xop.m-code
                             AND (b-mach.dept[1] EQ "PR" OR
                                  b-mach.dept[2] EQ "PR" OR
                                  b-mach.dept[3] EQ "PR" OR
                                  b-mach.dept[4] EQ "PR" OR
                                  b-mach.dept[1] EQ "CT" OR
                                  b-mach.dept[2] EQ "CT" OR
                                  b-mach.dept[3] EQ "CT" OR
                                  b-mach.dept[4] EQ "CT"))
            NO-LOCK BY xop.d-seq BY xop.line:

          i = i + 1.
          IF ROWID(xop) EQ ROWID(est-op) THEN LEAVE.
        END.
       
        FIND FIRST w-ink
            WHERE w-ink.form-no EQ b-ef.form-no
              AND w-ink.pass    EQ i
            NO-ERROR.

        IF AVAIL w-ink THEN DO:
          IF prmInk NE 0 THEN
            w-ink.inks = prmInk.
          IF prmCoat NE 0 THEN
            w-ink.varn = prmCoat .

          IF w-ink.press NE mach.pr-type THEN
            cError = "WRONG PRESS TYPE for selected Ink..".
          ELSE
          IF mach.max-color LT w-ink.inks + w-ink.varn THEN
            cError = "NOT ENOUGH COLORS on PRESS for selected Inks...".
        END.

        ELSE cError = "No Inks defined...".
      END.

      IF LOOKUP(lv-dept,"RS,RC") GT 0 OR mach.p-type EQ "R" THEN
        ASSIGN
         sh-len = IF b-ef.roll THEN b-ef.gsh-wid ELSE b-ef.nsh-wid
         sh-wid = IF b-ef.roll THEN b-ef.gsh-len ELSE b-ef.nsh-len.
      ELSE
      IF LOOKUP(lv-dept,"PR,GU,LM") GT 0 OR b-ef.n-out-l LE 1 THEN
        ASSIGN
         sh-len = b-ef.nsh-wid
         sh-wid = b-ef.nsh-len.
      ELSE
        ASSIGN
         sh-len = b-ef.trim-w
         sh-wid = b-ef.trim-l.

      IF mach.p-type EQ "B" THEN
        ASSIGN
         sh-len = b-eb.t-len
         sh-wid = b-eb.t-wid.

      IF cError EQ ""            AND
         mach.min-len GT sh-len THEN
        cError = "BOARD too small for Machine" .

      IF cError EQ ""            AND
         mach.max-len LT sh-len THEN
        cError = "BOARD too large for Machine ".

      IF cError EQ ""            AND
         mach.min-wid GT sh-wid THEN
        cError = "BOARD too small for Machine " .

      IF cError EQ ""            AND
         mach.max-wid LT sh-wid THEN
        cError = "BOARD too large for Machine " .

      IF cError EQ ""             AND
         mach.min-cal gt b-ef.cal THEN
        cError = "BOARD CALIPER too small for Machine!".

      IF cError EQ ""             AND
         mach.max-cal lt b-ef.cal THEN
        cError = "BOARD CALIPER too large for Machine!".

      qty = est-qty.eqty.

      FIND xef WHERE ROWID(xef) EQ ROWID(b-ef) NO-LOCK NO-ERROR.
      FIND xeb WHERE ROWID(xeb) EQ ROWID(b-eb) NO-LOCK NO-ERROR.
        
      run ce/mach-qty.p (ROWID(mach)).

      IF cError EQ ""               AND
         v-chk-qty lt mach.min-run THEN
        cError = "RUN QTY. too small for Machine!".

      IF cError EQ ""               AND
         v-chk-qty gt mach.max-run THEN
        cError = "RUN QTY. too large for Machine!".
    END.

    
 /* END.*/
  
END PROCEDURE.



PROCEDURE is-it-foam :

  DEF BUFFER b-ef FOR ef.
    ll-foam = NO.
    FIND FIRST b-ef
        WHERE b-ef.company EQ est.company
          AND b-ef.est-no  EQ est.est-no
          AND b-ef.form-no EQ prmSnum
        NO-LOCK NO-ERROR.
    IF AVAIL b-ef THEN RUN cec/isitfoam.p (ROWID(b-ef), OUTPUT ll-foam).
  
END PROCEDURE.

PROCEDURE set-lock :

  def input parameter ip-form-no like ef.form-no no-undo.
  def input parameter ip-op-lock like ef.op-lock no-undo.
  
  find first ef
      where ef.company eq est-qty.company
        and ef.est-no  eq est-qty.est-no
        and ef.form-no eq ip-form-no
      no-error.

  if avail ef then do:

     /*task 020050908*/
     IF ip-op-lock EQ ef.op-lock THEN
     DO:
        {est/op-lock.i xest}
        
        ASSIGN
           op-lock.val[1] = INT(NOT ip-op-lock)
           op-lock.val[2] = op-lock.val[1].

        RELEASE op-lock.
     END.

     ef.op-lock = ip-op-lock.

     release ef.
  end.
  
END PROCEDURE.

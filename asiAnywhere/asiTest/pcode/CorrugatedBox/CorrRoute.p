/*------------------------------------------------------------------------
    File        : CorrRoute.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 02  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttCorrRoute NO-UNDO
    FIELD  vEstNum     AS CHAR 
    FIELD  vCustPart   AS CHAR
    FIELD  vEstDate    AS DATETIME    
    FIELD  vFormNo     AS INT     
    FIELD  vFormQty    AS INT     
    FIELD  vBlankNo    AS INT      
    FIELD  vBlankQty   AS INT

    FIELD vSnum        AS INT
    FIELD vBnum        AS INT
    FIELD vMcode       AS CHARACTER
    FIELD vMdscr       AS CHARACTER
    FIELD vOppass      AS INT
    FIELD vNout        AS INT
    FIELD vOpmr        AS DECIMAL
    FIELD vOpwaste     AS INT
    FIELD vOpspeed     AS INT
    FIELD vOpspoil     AS DECIMAL
    FIELD vOpcrew      AS DECIMAL
    FIELD vOpcrew2     AS DECIMAL
    FIELD vOpRate      AS DECIMAL
    FIELD vOpRate2     AS DECIMAL 
    FIELD vPlates      AS INT 
    FIELD vFountains   AS INT 
    FIELD vAtype1      AS CHAR
    FIELD vAtqty1      AS INT
    FIELD vAtype2      AS CHAR
    FIELD vAtqty2      AS INT
    FIELD vAtype3      AS CHARACTER
    FIELD vAtqty3      AS INT
    FIELD vLine        AS INT
    FIELD vtype        AS INT
    

        .
DEFINE DATASET dsCorrRoute FOR ttCorrRoute.

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmActSelect   AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmComp        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmEstNum      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmFormNo      AS INT NO-UNDO.

DEFINE INPUT PARAMETER prmSnum         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBnum         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmMcode        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMdscr        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmOppass       AS INT NO-UNDO.
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
DEFINE INPUT PARAMETER prmAtype1       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAtqty1       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmAtype2       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAtqty2       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmAtype3       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAtqty3       AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmLine         AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmQty          AS INT NO-UNDO.


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrRoute .
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
IF prmOppass     = ?  THEN ASSIGN    prmOppass      = 0.
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
IF prmAtype1     = ?  THEN ASSIGN    prmAtype1      = "".
IF prmAtqty1     = ?  THEN ASSIGN    prmAtqty1      = 0.
IF prmAtype2     = ?  THEN ASSIGN    prmAtype2      = "".
IF prmAtqty2     = ?  THEN ASSIGN    prmAtqty2      = 0.
IF prmAtype3     = ?  THEN ASSIGN    prmAtype3      = "".
IF prmAtqty3     = ?  THEN ASSIGN    prmAtqty3      = 0.
IF prmLine       = ?  THEN ASSIGN    prmLine        = 0.
IF prmActSelect  = ?  THEN ASSIGN    prmActSelect   = "".
IF prmQty        = ?  THEN ASSIGN    prmQty         = 0.

{est/d-machex.i NEW}

def new shared buffer xest for est.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.

def buffer xop for est-op.

def new shared var xcal    as de no-undo.
def new shared var sh-wid  as de no-undo.
def new shared var sh-len  as de no-undo.
def new shared var fil_id  as recid no-undo.
def new shared var maxco   as int no-undo.
def new shared var qty     as int no-undo.
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
DEF VAR v-override-mode AS LOG NO-UNDO.

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

MESSAGE "prmAction" prmAction.


/*************************************prmAction***************************************************/  




IF prmAction = "RouteAdd"  THEN DO:

    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    i = 1.
    for each xop
        where xop.company eq est.company
        and xop.est-no  eq est.est-no
        and xop.line    lt 500
        no-lock
        by xop.line descending:
        i = xop.line + 1.
        leave.
      end.

      CREATE est-op.
      assign
          est-op.company = est.company
          est-op.est-no  = est.est-no
          est-op.auto    = false
          est-op.line    = i
          est-op.s-num   = prmSnum
          est-op.b-num   = if est.est-type eq 5 then 1 else 0
          est-op.op-pass = prmOppass
          est-op.n-out   = prmNout
          est-op.qty     = IF est.est-type NE 8 THEN est-qty.eqty ELSE prmQty   
          est-op.m-code  =  prmMcode
          est-op.m-dscr  =  prmMdscr
          est-op.att-type[1] =  prmAtype1
          est-op.att-qty[1]  =  prmAtqty1
          est-op.att-type[2] =  prmAtype2                                                         
          est-op.att-qty[2]  =  prmAtqty2
          est-op.att-type[3] =  prmAtype3
          est-op.att-qty[3]  =  prmAtqty3  .    


   FIND xest WHERE RECID(xest) EQ RECID(est).

   run set-import-stds ("add", yes).

  FIND FIRST mach  where (mach.company eq prmComp 
                          and  mach.loc     eq prmLoc)
        AND mach.m-code EQ est-op.m-code
      NO-LOCK NO-ERROR.

  FIND FIRST xef
      WHERE xef.company EQ est-op.company
        AND xef.est-no  EQ est-op.est-no
        AND xef.form-no EQ est-op.s-num
      NO-LOCK NO-ERROR.

  RELEASE xeb.
  IF AVAIL xef THEN
  FIND FIRST xeb
      WHERE xeb.company   EQ xef.company
        AND xeb.est-no    EQ xef.est-no
        AND xeb.form-no   EQ xef.form-no
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK NO-ERROR.

  ASSIGN
   est-op.d-seq  = mach.d-seq
   est-op.dept   = mach.dept[1]
   est-op.op-sb  = mach.p-type ne "B"
   est-op.m-code = mach.m-code
   est-op.m-dscr = mach.m-dscr.

  IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[1] = mach.mr-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "M R",
                        INPUT-OUTPUT est-op.op-crew[1]).
  END.

  IF est-op.op-crew[2] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[2] = mach.run-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "RUN",
                        INPUT-OUTPUT est-op.op-crew[2]).
  END.

  ASSIGN
   est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
   est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.
         
  IF ll-import-selected THEN est-op.op-spoil = mach.run-spoil.
        
  IF mach.p-type NE "B" THEN
    est-op.b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
     
  ELSE
  IF est-op.b-num EQ 0 THEN est-op.b-num = 1.

  RUN is-it-foam.
     
  IF NOT CAN-DO(lv-n-out-depts,lv-dept)                 AND
     (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam) THEN est-op.n-out = 0.
    
  FOR EACH xop
      WHERE xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        AND xop.line    LT 500
        AND (NOT ll-foam OR NOT CAN-DO(lv-foam-depts,xop.dept))
      BREAK BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.dept
            BY xop.line:
            
    /*IF FIRST-OF(xop.dept) THEN j = 0.*/
      IF FIRST-OF(xop.dept) THEN j = 1.

    MESSAGE "hello".
    
    ASSIGN
     j           = j + 1
     xop.op-pass = j.
  END.
  
  j = 0.
  for each xop
      where xop.company eq est-op.company
        and xop.est-no  eq est-op.est-no
        and xop.line    lt 500
      by xop.qty
      by xop.s-num
      by xop.b-num
      by xop.d-seq
      by xop.op-pass
      by xop.rec_key:
      
   /* {sys/inc/machpos.w xop share}  */
      find FIRST reftable  where reftable.reftable eq "MachinePosition"
          and reftable.company  eq string(xop.company,"x(10)") +
          string(xop.est-no,"x(10)")
          and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
          and reftable.code     eq ""
          and reftable.code2    eq ""
          SHARE-LOCK no-error.

    assign
     j        = j + 1
     xop.line = j.
     
    if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
  end.

  if not xef.op-lock AND NOT ll-foam then do:
     v-outw = xef.n-out.    
     if v-outw gt 1 then
     for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq by xop.line:
        
       v-outw = v-outw - xop.n-out.  
       if v-outw le 0 then do:
         v-recid = recid(xop).
         leave.
       end.
     end.
     
     v-outl = xef.n-out-l.    
     if v-outl gt 1 then
     for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq desc by xop.line desc:
         
       if recid(xop) eq v-recid then leave.       
       v-outl = v-outl - xop.n-out.      
       if v-outl le 0 then leave.
     end.
     
     if v-outw + v-outl lt 0 then do :
       cError =  "Number Out for 'CR or RC' machine passes do not match layout...".
       return .
     end.
  end.    
  
  assign
   fil_id  = recid(est-op)
   v-recid = fil_id.

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, NOT ll-import-selected).
  END.

  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, YES).
  END.

  fil_id = v-recid.
        

  ASSIGN
    prmLine   = i
    prmAction = "View".    
     
END.






IF prmAction = "RouteCopy"  THEN DO:

    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR. 

    v-override-mode = NO.

    i = 1.
    for each xop
        where xop.company eq est.company
        and xop.est-no  eq est.est-no
        and xop.line    lt 500
        no-lock
        by xop.line descending:
        i = xop.line + 1.
        leave.
      end.

      CREATE est-op.
      assign
          est-op.company     = est.company
          est-op.est-no      = est.est-no
          est-op.auto        = false
          est-op.line        = i
          est-op.s-num       = prmSnum
          est-op.b-num       = if est.est-type eq 5 then 1 else 0
          est-op.op-pass     = prmOppass
          est-op.n-out       = prmNout
          est-op.qty         = IF est.est-type NE 8 THEN  est-qty.eqty ELSE prmQty
          est-op.m-code      = prmMcode
          est-op.m-dscr      = prmMdscr
          est-op.op-mr       = prmOpmr      
          est-op.op-waste    = prmOpwaste  
          est-op.op-speed    = prmOpspeed  
          est-op.op-spoil    = prmOpspoil  
          est-op.op-crew[1]  = prmOpcrew   
          est-op.op-crew[2]  = prmOpcrew2  
          est-op.op-rate[1]  = prmOpRate   
          est-op.op-rate[2]  = prmOpRate2  
          est-op.plates      = prmPlates   
          est-op.fountains   = prmFountains
          est-op.att-type[1] = prmAtype1
          est-op.att-qty[1]  = prmAtqty1
          est-op.att-type[2] = prmAtype2                                                         
          est-op.att-qty[2]  = prmAtqty2
          est-op.att-type[3] = prmAtype3
          est-op.att-qty[3]  = prmAtqty3  .    


   FIND xest WHERE RECID(xest) EQ RECID(est).

  FIND FIRST mach  where (mach.company eq prmComp 
                          and  mach.loc     eq prmLoc)
        AND mach.m-code EQ est-op.m-code
      NO-LOCK NO-ERROR.

  FIND FIRST xef
      WHERE xef.company EQ est-op.company
        AND xef.est-no  EQ est-op.est-no
        AND xef.form-no EQ est-op.s-num
      NO-LOCK NO-ERROR.

  RELEASE xeb.
  IF AVAIL xef THEN
  FIND FIRST xeb
      WHERE xeb.company   EQ xef.company
        AND xeb.est-no    EQ xef.est-no
        AND xeb.form-no   EQ xef.form-no
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK NO-ERROR.

  ASSIGN
   est-op.d-seq  = mach.d-seq
   est-op.dept   = mach.dept[1]
   est-op.op-sb  = mach.p-type ne "B"
   est-op.m-code = mach.m-code
   est-op.m-dscr = mach.m-dscr.

  IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[1] = mach.mr-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "M R",
                        INPUT-OUTPUT est-op.op-crew[1]).
  END.

  IF est-op.op-crew[2] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[2] = mach.run-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "RUN",
                        INPUT-OUTPUT est-op.op-crew[2]).
  END.

  ASSIGN
   est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
   est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.
         
  IF ll-import-selected THEN est-op.op-spoil = mach.run-spoil.
        
  IF mach.p-type NE "B" THEN
    est-op.b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
     
  ELSE
  IF est-op.b-num EQ 0 THEN est-op.b-num = 1.

  RUN is-it-foam.
     
  IF NOT CAN-DO(lv-n-out-depts,lv-dept)                 AND
     (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam) THEN est-op.n-out = 0.
    
  FOR EACH xop
      WHERE xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        AND xop.line    LT 500
        AND (NOT ll-foam OR NOT CAN-DO(lv-foam-depts,xop.dept))
      BREAK BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.dept
            BY xop.line:
            
    /*IF FIRST-OF(xop.dept) THEN j = 0.*/
      IF FIRST-OF(xop.dept) THEN j = 1.
    
    ASSIGN
     j           = j + 1
     xop.op-pass = j.
  END.
  
  j = 0.
  for each xop
      where xop.company eq est-op.company
        and xop.est-no  eq est-op.est-no
        and xop.line    lt 500
      by xop.qty
      by xop.s-num
      by xop.b-num
      by xop.d-seq
      by xop.op-pass
      by xop.rec_key:
      
   /* {sys/inc/machpos.w xop share}  */
      find FIRST reftable  where reftable.reftable eq "MachinePosition"
          and reftable.company  eq string(xop.company,"x(10)") +
          string(xop.est-no,"x(10)")
          and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
          and reftable.code     eq ""
          and reftable.code2    eq ""
          SHARE-LOCK no-error.

    assign
     j        = j + 1
     xop.line = j.
     
    if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
  end.

  if not xef.op-lock AND NOT ll-foam then do:
     v-outw = xef.n-out.    
     if v-outw gt 1 then
     for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq by xop.line:
        
       v-outw = v-outw - xop.n-out.  
       if v-outw le 0 then do:
         v-recid = recid(xop).
         leave.
       end.
     end.
     
     v-outl = xef.n-out-l.    
     if v-outl gt 1 then
     for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq desc by xop.line desc:
         
       if recid(xop) eq v-recid then leave.       
       v-outl = v-outl - xop.n-out.      
       if v-outl le 0 then leave.
     end.
     
     if v-outw + v-outl lt 0 then do :
       cError =  "Number Out for 'CR or RC' machine passes do not match layout...".
       return .
     end.
  end.    
  
  assign
   fil_id  = recid(est-op)
   v-recid = fil_id.

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, NOT ll-import-selected).
  END.

  MESSAGE "rowidestop" ROWID(est-op).

  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, YES).
  END.

  fil_id = v-recid.    

        ASSIGN
            prmLine   = i
            prmAction = "View".
        MESSAGE "addtest" prmLine prmAction prmQty.

     
END.






IF prmAction = "RouteAddStds"  THEN DO:

    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.

    run set-import-stds ("add", no).

    i = 1.
    for each xop
        where xop.company eq est.company
        and xop.est-no  eq est.est-no
        and xop.line    lt 500
        no-lock
        by xop.line descending:
        i = xop.line + 1.
        leave.
      end.

      CREATE est-op.
      assign
          est-op.company = est.company
          est-op.est-no  = est.est-no
          est-op.auto    = false
          est-op.line    = i
          est-op.s-num   = prmSnum
          est-op.b-num   = if est.est-type eq 5 then 1 else 0
          est-op.op-pass = prmOppass
          est-op.n-out   = prmNout
          est-op.op-waste  =  prmOpwaste
          est-op.op-speed  =  prmOpspeed
          est-op.op-spoil   = prmOpspoil
          est-op.op-crew[1] =  prmOpcrew
          est-op.op-crew[2] =  prmOpcrew2
          est-op.qty     = IF est.est-type NE 8 THEN est-qty.eqty ELSE prmQty
          est-op.m-code  =  prmMcode
          est-op.m-dscr  =  prmMdscr
          est-op.op-mr       = prmOpmr
          est-op.att-type[1] =  prmAtype1
          est-op.att-qty[1]  =  prmAtqty1
          est-op.att-type[2] =  prmAtype2                                                         
          est-op.att-qty[2]  =  prmAtqty2
          est-op.att-type[3] =  prmAtype3
          est-op.att-qty[3]  =  prmAtqty3  .


   FIND xest WHERE RECID(xest) EQ RECID(est).   

  FIND FIRST mach  where (mach.company eq prmComp 
                          and  mach.loc     eq prmLoc)
        AND mach.m-code EQ est-op.m-code
      NO-LOCK NO-ERROR.

  FIND FIRST xef
      WHERE xef.company EQ est-op.company
        AND xef.est-no  EQ est-op.est-no
        AND xef.form-no EQ est-op.s-num
      NO-LOCK NO-ERROR.

  RELEASE xeb.
  IF AVAIL xef THEN
  FIND FIRST xeb
      WHERE xeb.company   EQ xef.company
        AND xeb.est-no    EQ xef.est-no
        AND xeb.form-no   EQ xef.form-no
        AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
      NO-LOCK NO-ERROR.

  ASSIGN
   est-op.d-seq  = mach.d-seq
   est-op.dept   = mach.dept[1]
   est-op.op-sb  = mach.p-type ne "B"
   est-op.m-code = mach.m-code
   est-op.m-dscr = mach.m-dscr.

  IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[1] = mach.mr-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "M R",
                        INPUT-OUTPUT est-op.op-crew[1]).
  END.

  IF est-op.op-crew[2] EQ 0 OR ll-import-selected THEN DO:
    est-op.op-crew[2] = mach.run-crusiz.
    RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "RUN",
                        INPUT-OUTPUT est-op.op-crew[2]).
  END.

  ASSIGN
   est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
   est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.
         
  IF ll-import-selected THEN est-op.op-spoil = mach.run-spoil.
        
  IF mach.p-type NE "B" THEN
    est-op.b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.
     
  ELSE
  IF est-op.b-num EQ 0 THEN est-op.b-num = 1.

  RUN is-it-foam.
     
  IF NOT CAN-DO(lv-n-out-depts,lv-dept)                 AND
     (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam) THEN est-op.n-out = 0.
    
  FOR EACH xop
      WHERE xop.company EQ est-op.company
        AND xop.est-no  EQ est-op.est-no
        AND xop.line    LT 500
        AND (NOT ll-foam OR NOT CAN-DO(lv-foam-depts,xop.dept))
      BREAK BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.dept
            BY xop.line:
            
    /*IF FIRST-OF(xop.dept) THEN j = 0.*/
      IF FIRST-OF(xop.dept) THEN j = 1.

    MESSAGE "hello".
    
    ASSIGN
     j           = j + 1
     xop.op-pass = j.
  END.
  
  j = 0.
  for each xop
      where xop.company eq est-op.company
        and xop.est-no  eq est-op.est-no
        and xop.line    lt 500
      by xop.qty
      by xop.s-num
      by xop.b-num
      by xop.d-seq
      by xop.op-pass
      by xop.rec_key:
      
   /* {sys/inc/machpos.w xop share}  */
      find FIRST reftable  where reftable.reftable eq "MachinePosition"
          and reftable.company  eq string(xop.company,"x(10)") +
          string(xop.est-no,"x(10)")
          and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
          and reftable.code     eq ""
          and reftable.code2    eq ""
          SHARE-LOCK no-error.

    assign
     j        = j + 1
     xop.line = j.
     
    if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
  end.

  if not xef.op-lock AND NOT ll-foam then do:
     v-outw = xef.n-out.    
     if v-outw gt 1 then
     for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq by xop.line:
        
       v-outw = v-outw - xop.n-out.  
       if v-outw le 0 then do:
         v-recid = recid(xop).
         leave.
       end.
     end.
     
     v-outl = xef.n-out-l.    
     if v-outl gt 1 then
     for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq desc by xop.line desc:
         
       if recid(xop) eq v-recid then leave.       
       v-outl = v-outl - xop.n-out.      
       if v-outl le 0 then leave.
     end.
     
     if v-outw + v-outl lt 0 then do :
       cError =  "Number Out for 'CR or RC' machine passes do not match layout...".
       return .
     end.
  end.    
  
  assign
   fil_id  = recid(est-op)
   v-recid = fil_id.

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, NOT ll-import-selected).
  END.

  MESSAGE "befmachrek" ll-import-all ROWID(est-op).

  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

  FOR EACH ef 
      WHERE ef.company EQ est-op.company
        AND ef.est-no  EQ est-op.est-no
      NO-LOCK:
    RUN set-lock (ef.form-no, YES).
  END.

  fil_id = v-recid.



        /*  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).*/

        ASSIGN
            prmLine   = i
            prmAction = "View".
        MESSAGE "addtest" prmLine prmAction prmQty.

     
END.





IF prmAction = "RouteUpdate" THEN DO:
   
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
    FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line LT 500 and  ((est-op.qty eq est-qty.eqty and est.est-type ne 8) or 
                                     (est-op.qty eq 0 and est.est-type ge 7)) AND est-op.LINE = prmLine EXCLUSIVE-LOCK  NO-ERROR.

   IF AVAIL est-op  THEN DO:
       ASSIGN
          est-op.s-num     = prmSnum
          est-op.m-code    =  prmMcode
          est-op.m-dscr    =  prmMdscr
          est-op.op-mr     = prmOpmr
          est-op.op-pass = prmOppass
          est-op.n-out   = prmNout
          est-op.op-waste  =  prmOpwaste
          est-op.op-speed  =  prmOpspeed
          est-op.op-spoil   = prmOpspoil
          est-op.op-crew[1] =  prmOpcrew
          est-op.op-crew[2] =  prmOpcrew2

          est-op.att-type[1] =  prmAtype1
          est-op.att-qty[1]  =  prmAtqty1
          est-op.att-type[2] =  prmAtype2                                                         
          est-op.att-qty[2]  =  prmAtqty2
          est-op.att-type[3] =  prmAtype3
          est-op.att-qty[3]  =  prmAtqty3  .

       MESSAGE "qtyll" est-qty.eqty est-op.qty.

      /* RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).*/
   END.

        ASSIGN
            prmAction = "View".
 
 END.





 IF prmAction = "RouteImport" THEN DO:   
   
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    
   FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
   
   FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line LT 500 and  ((est-op.qty eq est-qty.eqty and est.est-type ne 8) or 
                                     (est-op.qty eq  0 and est.est-type ge 7)) AND est-op.LINE = prmLine EXCLUSIVE-LOCK  NO-ERROR.

   run set-import-stds ("update", yes).

   IF AVAIL est-op  THEN DO:
       ASSIGN
          est-op.s-num      = prmSnum
          est-op.m-code     = prmMcode
          est-op.m-dscr     = prmMdscr
          est-op.op-mr      = prmOpmr
          est-op.op-pass    = prmOppass
          est-op.n-out      = prmNout
          
           /*est-op.op-waste  =  prmOpwaste
          est-op.op-speed  =  prmOpspeed
          est-op.op-spoil   = prmOpspoil
          est-op.op-crew[1] =  prmOpcrew
          est-op.op-crew[2] =  prmOpcrew2
          */

          est-op.att-type[1] =  prmAtype1
          est-op.att-qty[1]  =  prmAtqty1
          est-op.att-type[2] =  prmAtype2                                                         
          est-op.att-qty[2]  =  prmAtqty2
          est-op.att-type[3] =  prmAtype3
          est-op.att-qty[3]  =  prmAtqty3  .     

        FIND xest WHERE RECID(xest) EQ RECID(est).
   
        FIND FIRST mach  where (mach.company eq prmComp 
            and  mach.loc     eq prmLoc)
            AND mach.m-code EQ est-op.m-code NO-LOCK NO-ERROR.    

        FIND FIRST xef
            WHERE xef.company EQ est-op.company
                AND xef.est-no  EQ est-op.est-no
                AND xef.form-no EQ est-op.s-num
                NO-LOCK NO-ERROR.

        RELEASE xeb.
        IF AVAIL xef THEN
            FIND FIRST xeb
                WHERE xeb.company   EQ xef.company
                AND xeb.est-no    EQ xef.est-no
                AND xeb.form-no   EQ xef.form-no
                AND (xeb.blank-no EQ est-op.b-num OR est-op.b-num EQ 0)
                NO-LOCK NO-ERROR.

            ASSIGN
                est-op.d-seq  = mach.d-seq
                est-op.dept   = mach.dept[1]
                est-op.op-sb  = mach.p-type ne "B"
                est-op.m-code = mach.m-code
                est-op.m-dscr = mach.m-dscr.     

        IF est-op.op-crew[1] EQ 0 OR ll-import-selected THEN DO:
            est-op.op-crew[1] = mach.mr-crusiz.
            RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "M R",
                        INPUT-OUTPUT est-op.op-crew[1]).            
        END.

        IF est-op.op-crew[2] EQ 0 OR ll-import-selected THEN DO:
            est-op.op-crew[2] = mach.run-crusiz.
            RUN est/getcrusz.p (ROWID(mach), ROWID(xeb), est-op.dept, "RUN",
                        INPUT-OUTPUT est-op.op-crew[2]).           
        END.

        ASSIGN
            est-op.op-rate[1] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[1]) + 
                       mach.mr-varoh  + mach.mr-fixoh
            est-op.op-rate[2] = (mach.lab-rate[mach.lab-drate] * est-op.op-crew[2]) + 
                       mach.run-varoh + mach.run-fixoh.       
         
        IF ll-import-selected THEN est-op.op-spoil = mach.run-spoil.
        
        IF mach.p-type NE "B" THEN
            est-op.b-num = IF xest.est-type EQ 5 THEN 1 ELSE 0.     
        ELSE
            IF est-op.b-num EQ 0 THEN est-op.b-num = 1.
                RUN is-it-foam.        
     
        IF NOT CAN-DO(lv-n-out-depts,lv-dept)                 AND
            (NOT CAN-DO(lv-foam-depts,lv-dept) OR NOT ll-foam) THEN est-op.n-out = 0.      
    
        FOR EACH xop
          WHERE xop.company EQ est-op.company
            AND xop.est-no  EQ est-op.est-no
            AND xop.line    LT 500
            AND (NOT ll-foam OR NOT CAN-DO(lv-foam-depts,xop.dept))
          BREAK BY xop.qty
            BY xop.s-num
            BY xop.b-num
            BY xop.dept
            BY xop.line:
            
            IF FIRST-OF(xop.dept) THEN j = 0.
    
            ASSIGN
            j           = j + 1
            xop.op-pass = j.
        END.
  
        j = 0.
        for each xop
          where xop.company eq est-op.company
            and xop.est-no  eq est-op.est-no
            and xop.line    lt 500
            by xop.qty
            by xop.s-num
            by xop.b-num
            by xop.d-seq
            by xop.op-pass
            by xop.rec_key:                

            find first {sys/inc/machposw.i reftable "xop"}
                share-lock no-error.

            assign
            j        = j + 1
            xop.line = j.
     
            if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
        end.

        if not xef.op-lock AND NOT ll-foam then do:
            v-outw = xef.n-out.    
            if v-outw gt 1 then
            for each xop
              where xop.company eq est-op.company
                and xop.est-no  eq est-op.est-no
                and xop.qty     eq est-op.qty
                and xop.s-num   eq est-op.s-num
                and lookup(xop.dept,lv-n-out-depts) gt 0
                and xop.line    lt 500
                no-lock by xop.d-seq by xop.line:
        
                    v-outw = v-outw - xop.n-out.  
                    if v-outw le 0 then do:
                        v-recid = recid(xop).
                        leave.
                    end.
        end.
     
        v-outl = xef.n-out-l.    
        if v-outl gt 1 then
        for each xop
         where xop.company eq est-op.company
           and xop.est-no  eq est-op.est-no
           and xop.qty     eq est-op.qty
           and xop.s-num   eq est-op.s-num
           and lookup(xop.dept,lv-n-out-depts) gt 0
           and xop.line    lt 500
         no-lock by xop.d-seq desc by xop.line desc:
         
            if recid(xop) eq v-recid then leave.       
            v-outl = v-outl - xop.n-out.      
            if v-outl le 0 then leave.
            end.
     
            if v-outw + v-outl lt 0 then do on endkey undo, retry:
                /*message "Number Out for 'CR or RC' machine passes do not match layout..."
                view-as alert-box.
                return error.
                */
                ASSIGN
                    cerror = "Number Out for 'CR or RC' machine passes do not match layout...".
                RETURN.
            end.
        end.    
  
        assign
            fil_id  = recid(est-op)
            v-recid = fil_id.

        FOR EACH ef 
          WHERE ef.company EQ est-op.company
            AND ef.est-no  EQ est-op.est-no
            NO-LOCK:        

                RUN set-lock (ef.form-no, NOT ll-import-selected).
        END. 

        RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

        FOR EACH ef 
          WHERE ef.company EQ est-op.company
            AND ef.est-no  EQ est-op.est-no
            NO-LOCK:
                RUN set-lock (ef.form-no, YES).
        END.        

        fil_id = v-recid.        

        ASSIGN
            prmAction = "View".  

     END.
 
 END.




/*******************************************************************************************************/

 IF prmAction = "RouteBuild" THEN DO:
    DEF VAR ll AS LOG NO-UNDO.
    DEF BUFFER b-est-qty FOR est-qty.

    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.    
    
    FIND xest WHERE RECID(xest) EQ RECID(est).

    ll = NO.

    IF xest.est-type ge 7 THEN DO:
                
        for each ef
            where ef.company eq est-qty.company
            and ef.est-no  eq est-qty.est-no
            no-lock:
     
                run set-lock (ef.form-no, no).
        end.            
    
        FIND FIRST xef WHERE xef.company = est-qty.company 
                     AND xef.est-no = est-qty.est-no
                   NO-LOCK NO-ERROR.
        FIND FIRST xeb WHERE xeb.company = est-qty.company 
                     AND xeb.est-no = est-qty.est-no
                     AND xeb.form-no = xef.form-no
                   NO-LOCK NO-ERROR.
      

        RUN cec/mach-seq.p (0, 0, xest.est-type EQ 8).
     
    END.
    ELSE DO:       
    FOR EACH b-est-qty
        WHERE b-est-qty.company EQ est-qty.company
          AND b-est-qty.est-no  EQ est-qty.est-no
        NO-LOCK BREAK BY b-est-qty.eqty:

      IF FIRST(b-est-qty.eqty)    AND
         NOT LAST(b-est-qty.eqty) THEN
          ll = YES.
        /*MESSAGE "Build routings for all quantities?" SKIP
                "  (Yes=AllQtys    No=" +
                TRIM(STRING(est-qty.eqty,">>>,>>>,>>>")) + " Only)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll.*/

      IF ll OR ROWID(est-qty) EQ ROWID(b-est-qty) THEN DO:  
        FOR EACH ef
            WHERE ef.company EQ b-est-qty.company
              AND ef.est-no  EQ b-est-qty.est-no
            NO-LOCK:
      
          RUN set-lock (ef.form-no, NO).
        END.          
    
    
        FIND FIRST xef WHERE xef.company = b-est-qty.company 
                         AND xef.est-no = b-est-qty.est-no
                       NO-LOCK NO-ERROR.
        FIND FIRST xeb WHERE xeb.company = b-est-qty.company 
                         AND xeb.est-no = b-est-qty.est-no
                         AND xeb.form-no = xef.form-no
                       NO-LOCK NO-ERROR.
     

        RUN cec/mach-seq.p (0, b-est-qty.eqty, NO).       
        
      END.
    END.
  END.


 END.
   
 /********************************************************************************************/



 IF prmAction = "RouteDelete" THEN DO:     

    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line LT 500 and  ((est-op.qty eq est-qty.eqty and est.est-type ne 8) or 
                                     (est-op.qty eq 0 and est.est-type ge 7)) AND est-op.LINE = prmLine EXCLUSIVE-LOCK  NO-ERROR.    

    IF AVAIL est-op THEN DO:
        DELETE est-op.
    END.    

     FIND LAST est-op WHERE est-op.company = est.company AND est-op.est-no = est.est-no 
          AND est-op.line LT 500 and  ((est-op.qty eq est-qty.eqty and est.est-type ne 8) or 
                                     (est-op.qty eq 0 and est.est-type ge 7)) NO-LOCK NO-ERROR.
    
     IF AVAIL est-op THEN do: 
         ASSIGN
         prmLine = est-op.LINE 
         prmAction = "View"   .
     END.
 END.


 IF prmActSelect = "Select" THEN DO:
    
   FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.

  FOR EACH est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
      AND est-op.line LT 500 and  ((est-op.qty eq est-qty.eqty and est.est-type ne 8) or 
                                   (est-op.qty eq 0 and est.est-type ge 7)) NO-LOCK :
      IF AVAIL est-op  THEN DO:
          CREATE ttCorrRoute.
          ASSIGN
              ttCorrRoute.vSnum          = est-op.s-num       
              ttCorrRoute.vBnum          = est-op.b-num       
              ttCorrRoute.vMcode         = est-op.m-code      
              ttCorrRoute.vMdscr         = est-op.m-dscr      
              ttCorrRoute.vOppass        = est-op.op-pass     
              ttCorrRoute.vNout          = est-op.n-out       
              ttCorrRoute.vOpmr          = est-op.op-mr       
              ttCorrRoute.vOpwaste       = est-op.op-waste    
              ttCorrRoute.vOpspeed       = est-op.op-speed    
              ttCorrRoute.vOpspoil       = est-op.op-spoil    
              ttCorrRoute.vOpcrew        = est-op.op-crew[1]  
              ttCorrRoute.vOpcrew2       = est-op.op-crew[2] 
              ttCorrRoute.vOpRate        = est-op.op-rate[1]
              ttCorrRoute.vOpRate2       = est-op.op-rate[2]
              ttCorrRoute.vPlates        = est-op.plates      
              ttCorrRoute.vFountains     = est-op.fountains   
              ttCorrRoute.vAtype1        = est-op.att-type[1] 
              ttCorrRoute.vAtqty1        = est-op.att-qty[1]  
              ttCorrRoute.vAtype2        = est-op.att-type[2] 
              ttCorrRoute.vAtqty2        = est-op.att-qty[2]  
              ttCorrRoute.vAtype3        = est-op.att-type[3] 
              ttCorrRoute.vAtqty3        = est-op.att-qty[3] 
              ttCorrRoute.vLine          = est-op.LINE 

               .

             
                END. /* end ttCorrRouteof est-prep*/
  END. /* end of for each*/
 END. /* end of select*/


 IF prmAction = "View" THEN DO:
     
     FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
     FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line LT 500 and  ((est-op.qty eq est-qty.eqty and est.est-type ne 8) or 
                                     (est-op.qty eq 0 and est.est-type ge 7)) AND est-op.LINE = prmLine NO-LOCK  NO-ERROR.
        IF AVAIL est-op  THEN DO:
            CREATE ttCorrRoute.
            ASSIGN
                ttCorrRoute.vSnum          = est-op.s-num       
                ttCorrRoute.vBnum          = est-op.b-num       
                ttCorrRoute.vMcode         = est-op.m-code      
                ttCorrRoute.vMdscr         = est-op.m-dscr      
                ttCorrRoute.vOppass        = est-op.op-pass     
                ttCorrRoute.vNout          = est-op.n-out       
                ttCorrRoute.vOpmr          = est-op.op-mr       
                ttCorrRoute.vOpwaste       = est-op.op-waste    
                ttCorrRoute.vOpspeed       = est-op.op-speed    
                ttCorrRoute.vOpspoil       = est-op.op-spoil    
                ttCorrRoute.vOpcrew        = est-op.op-crew[1]  
                ttCorrRoute.vOpcrew2       = est-op.op-crew[2] 
                ttCorrRoute.vOpRate        = est-op.op-rate[1]
                ttCorrRoute.vOpRate2       = est-op.op-rate[2]
                ttCorrRoute.vPlates        = est-op.plates      
                ttCorrRoute.vFountains     = est-op.fountains   
                ttCorrRoute.vAtype1        = est-op.att-type[1] 
                ttCorrRoute.vAtqty1        = est-op.att-qty[1]  
                ttCorrRoute.vAtype2        = est-op.att-type[2] 
                ttCorrRoute.vAtqty2        = est-op.att-qty[2]  
                ttCorrRoute.vAtype3        = est-op.att-type[3] 
                ttCorrRoute.vAtqty3        = est-op.att-qty[3] 
                ttCorrRoute.vLine          = est-op.LINE 
                ttCorrRoute.vtype          = est.est-type

                 .

           
  END. /* end of for each*/  
 END. /* end of view*/

/**********************procedure*******************************/

 PROCEDURE valid-mach :
  
  def var i as int no-undo.
  {sys/inc/cepanel.i}

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
  
    assign
     prmMcode = mach.m-code
     prmMdscr = mach.m-dscr
     lv-d-seq = mach.d-seq
     lv-dept  = mach.dept[1].

    find first xef
        where xef.company eq est.company
          and xef.est-no  eq est.est-no
          and xef.form-no eq prmSnum
        no-lock no-error.

    find first xeb
        where xeb.company eq est.company
          and xeb.est-no  eq est.est-no
          and xeb.form-no eq xef.form-no
          and (xeb.blank-no eq int(prmBnum ) or
               int(prmBnum) eq 0) no-lock no-error.

    find first style  where style.company eq PrmComp
          and style.style eq xeb.style
        no-lock no-error.

    find xest where recid(xest) eq recid(est) no-lock.

    
    v-on-f = prmNout .

    if v-on-f ne 0 or NOT CAN-DO(lv-n-out-depts,lv-dept) then do:
      if xef.lam-dscr eq "R"                         /*or
         (xef.lam-dscr ne "R" and xef.xgrain eq "S")*/ then
        assign
         sh-wid = xef.nsh-wid
         sh-len = xef.nsh-len.
      else
        assign
         sh-wid = xef.nsh-len
         sh-len = xef.nsh-wid.

      do while true:
        sh-dep = xef.cal.

        IF mach.p-type eq "B" THEN
          ASSIGN
           sh-len = xeb.t-wid
           sh-wid = xeb.t-len.

        IF ll-foam THEN DO:
          sh-dep = xeb.t-dep.

          IF mach.p-type NE "B" THEN DO:
           /* {sys/inc/machpos.w est-op NO}*/
              find first reftable
                  where reftable.reftable eq "MachinePosition"
                  and reftable.company  eq string(est-op.company,"x(10)") +
                  string(est-op.est-no,"x(10)")
                  and reftable.loc      eq string(est-op.line - if est-op.line gt 500 then 500 else 0,"9999999999")
                  and reftable.code     eq ""
                  and reftable.code2    eq ""
                  NO-LOCK no-error.

            IF AVAIL reftable THEN
            DO i = 1 TO 3:
              CASE reftable.val[i]:
                WHEN 1.0 THEN sh-len = reftable.val[i + 3].
                WHEN 2.0 THEN sh-wid = reftable.val[i + 3].
                WHEN 3.0 THEN sh-dep = reftable.val[i + 3].
              END CASE.
            END.
          END.
        END.

        IF lv-dept EQ "RC" THEN DO:
          xcal = sh-dep.
          RUN cec/rc-mach.p (BUFFER mach, v-on-f, NO).
          IF AVAIL mach THEN LEAVE.
        END.
      
        ELSE DO:
          {cec/mach-seq.i sh-len sh-wid sh-dep}
        END.

        if not avail mach then do:
          cError =  "Estimate specifications outside machine limits" .
          return .
        end.
      end.
    end.
        
    IF CAN-DO(lv-n-out-depts,lv-dept) AND v-on-f LE 0 THEN DO:
      cError =  "out  must not be zero...".
      RETURN .
    END.

    if mach.p-type ne "B" then
      assign
       lv-op-sb = yes
       lv-b-num = if xest.est-type eq 5 then 1 else 0.
       
    else
      assign
       lv-op-sb = no
       lv-b-num = if prmBnum eq 0 then 1 else prmBnum.
       
    IF NOT CAN-DO(lv-n-out-depts,lv-dept) THEN lv-n-out = 0.

    if xeb.i-pass gt 0 then do:
          /* press, check ink */
          if mach.dept[1] eq "PR" or mach.dept[2] eq "PR" or
             mach.dept[3] eq "PR" or mach.dept[4] eq "PR" then do:
                find first item where (item.company = prmComp and  (item.mat-type = "I" or item.mat-type = "V"))
                                and item.i-no eq xeb.i-code[1]
                     no-lock no-error.
                if not avail item and mach.dept[2] eq "" and
                  mach.dept[3] eq "" and mach.dept[4] eq "" 
                then do:
                     cError =  "No Inks defined !".
                     return .                   
                end.
                else if avail item and item.press-type ne mach.pr-type then do:
                    cError =  "WRONG PRESS TYPE for selected Ink!" .
                    return .
                end.
                assign
                  maxco    = 0
                  v-passes = /*INT(adm-new-record)*/ 0 .

                FOR EACH b-est-op
                    WHERE b-est-op.company EQ est-op.company
                      AND b-est-op.est-no  EQ est-op.est-no
                      AND (b-est-op.eqty   EQ est-op.eqty OR est.est-type GE 7)
                      AND b-est-op.s-num   EQ prmSnum
                      AND b-est-op.line    LT 500
                      AND (ROWID(b-est-op) NE ROWID(est-op) /*OR NOT adm-new-record*/ )
                      AND CAN-FIND(FIRST b-mach
                                   WHERE b-mach.company EQ b-est-op.company
                                     AND b-mach.m-code  EQ b-est-op.m-code
                                     AND (b-mach.dept[1] EQ "PR" OR
                                          b-mach.dept[2] EQ "PR" OR
                                          b-mach.dept[3] EQ "PR" OR
                                          b-mach.dept[4] EQ "PR" OR
                                          b-mach.dept[1] EQ "CT" OR
                                          b-mach.dept[2] EQ "CT" OR
                                          b-mach.dept[3] EQ "CT" OR
                                          b-mach.dept[4] EQ "CT"))
                    NO-LOCK BY b-est-op.d-seq BY b-est-op.line:

                   v-passes = v-passes + 1.
                   IF ROWID(b-est-op) EQ ROWID(est-op) THEN LEAVE.
                END.

                DO i = 1 TO 10:
                   IF xeb.i-ps[i] NE v-passes THEN NEXT.
                   FIND FIRST item NO-LOCK
                       where (item.company = prmComp)
                         AND item.i-no     EQ xeb.i-code[i]
                         AND INDEX("IV",item.mat-type) GT 0
                         AND item.ink-type NE "A"
                       NO-ERROR.
                   IF AVAIL item THEN maxco = maxco + 1.
                END.

                IF mach.max-color LT maxco THEN DO:
                   cError = "NOT ENOUGH COLORS on PRESS for selected Inks!" .
                   return .
                end.
          end.  /* dept = "PR" */
    end.  /* x-eb.i-pass */
    
    /*ll-machine-modified = est-op.m-code:MODIFIED in browse {&browse-name}.*/
/*  end.  */
  
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
      where ef.company eq est.company
        and ef.est-no  eq est.est-no
        and ef.form-no eq ip-form-no
      no-error.
  if avail ef then do:
    ef.op-lock = ip-op-lock.
    release ef.
  end.
  
END PROCEDURE.


/*------------------------------------------------------------------------------------------------------*/

PROCEDURE set-import-stds :

  def input parameter ip-add-update  as   char           no-undo.
  def input parameter ip-import-stds like ll-import-stds no-undo.

  IF ip-add-update = "Update" AND ip-import-stds = NO THEN
     v-override-mode = YES.
  ELSE
     v-override-mode = NO.

  IF ip-import-stds THEN
  FOR EACH xop NO-LOCK
      WHERE xop.company EQ est.company
        AND xop.est-no  EQ est.est-no
        AND xop.line    LT 500
        AND (NOT AVAIL est-op OR ROWID(xop) NE ROWID(est-op)),
      FIRST mach NO-LOCK
      where (mach.company eq cocode and  mach.loc     eq locode)
        AND mach.m-code EQ xop.m-code,
      FIRST reftable NO-LOCK
      WHERE reftable.reftable EQ "mach.obsolete"
        AND reftable.company  EQ mach.company
        AND reftable.loc      EQ mach.loc
        AND reftable.code     EQ mach.m-code
        AND reftable.val[1]   EQ 1:
    /*MESSAGE "Machine: " + TRIM(mach.m-code) +
            " is obsolete, please replace or standards will not be imported"
        VIEW-AS ALERT-BOX ERROR.*/
    ip-import-stds = NO.
    LEAVE.
  END.

  ASSIGN
   ll-import-stds     = ip-import-stds
   ll-import-selected = ip-import-stds.

  /*if ip-add-update eq "update" then do with frame {&frame-name}:
    apply "entry" to est-op.s-num in browse {&browse-name}.
  end.
  */

END PROCEDURE.

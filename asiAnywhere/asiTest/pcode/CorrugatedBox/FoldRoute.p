/*------------------------------------------------------------------------
    File        : FoldRoute.p
    Purpose     : Corrugated Box

    Syntax      :

    Description : Return a Dataset of all Corrugated Box

    Author(s)   : 
    Created     : 02  march 2009 
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  DEFinitions  ************************** */
DEFINE TEMP-TABLE ttFoldRoute NO-UNDO
    
    FIELD vSnum        AS INT
    FIELD vBnum        AS INT
    FIELD vMcode       AS CHARACTER
    FIELD vMdscr       AS CHARACTER
   
    FIELD vNout        AS INT
    FIELD vMR          AS DECIMAL
    FIELD vOpwaste     AS INT
    FIELD vOpspeed     AS INT
    FIELD vOpspoil     AS DECIMAL
    FIELD vOpcrew      AS DECIMAL
    FIELD vOpcrew2     AS DECIMAL
    FIELD vOpRate      AS DECIMAL
    FIELD vOpRate2     AS DECIMAL 
    FIELD vPlates      AS INT 
    FIELD vFountains   AS INT 
    FIELD vInks        AS INT
    FIELD vCoat        AS INT 

    FIELD vLine        AS INT
    FIELD vtype        AS INT
    

        .
DEFINE DATASET dsFoldRoute FOR ttFoldRoute.

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


DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsFoldRoute .
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
IF prmActSelect  = ?  THEN ASSIGN    prmActSelect   = "".
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



/*************************************prmAction***************************************************/  

IF prmAction = "RouteAdd"  THEN DO:
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
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


IF prmAction = "RouteAdd"  THEN DO:

    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.

    run set-import-stds ("add", yes).    

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
          est-op.auto    = NO
          est-op.line    = i
          est-op.s-num   = prmSnum
          est-op.b-num   = if est.est-type eq 5 then 1 else 0
          est-op.qty     = IF est.est-type EQ 1 THEN est-qty.eqty ELSE prmQty 
          est-op.m-code  =  prmMcode
          est-op.m-dscr  =  prmMdscr
            .

         find xest where recid(xest) eq recid(est).   
          
          FIND FIRST mach  where (mach.company eq prmComp 
                                  and  mach.loc     eq prmLoc)
              and mach.m-code eq est-op.m-code
              no-lock no-error.

          find first xef
              where xef.company eq est-op.company
              and xef.est-no  eq est-op.est-no
              and xef.form-no eq est-op.s-num
              no-lock no-error.
          ASSIGN
              est-op.d-seq      = mach.d-seq
              est-op.dept       = mach.dept[1]
              est-op.op-sb      = mach.p-type ne "B"
              est-op.m-code     = mach.m-code
              est-op.m-dscr     = mach.m-dscr
              est-op.op-rate[1] = mach.mr-trate
              est-op.op-rate[2] = mach.run-trate.
          
          if est-op.op-crew[1] eq 0 or ll-import-selected then
              est-op.op-crew[1] = mach.mr-crusiz.
          
          if est-op.op-crew[2] eq 0 or ll-import-selected  then
              est-op.op-crew[2] = mach.run-crusiz.

          if ll-import-selected then 
              assign
              est-op.op-spoil = mach.run-spoil
              est-op.num-col  = 0
              est-op.num-coat = 0.

          for each xop
              where xop.company eq est-op.company
              and xop.est-no  eq est-op.est-no
              and xop.line    lt 500
              break by xop.qty
              by xop.s-num
              by xop.b-num
              by xop.dept
              by xop.line:

              if first-of(xop.dept) then j = 0.
              assign
                  j           = j + 1
                  xop.op-pass = j.
              end.

              j = 0.
              for each xop
                  where xop.company EQ est-op.company
                  AND xop.est-no  EQ est-op.est-no
                  and xop.line    lt 500
                  by xop.qty by xop.s-num by xop.b-num by xop.d-seq by xop.op-pass:

                 /* {sys/inc/machpos.w xop share}*/
                  find first reftable
                      where reftable.reftable eq "MachinePosition"
                      and reftable.company  eq string(xop.company,"x(10)") +
                      string(xop.est-no,"x(10)")
                      and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
                      and reftable.code     eq ""
                      and reftable.code2    eq ""
                      SHARE-LOCK no-error.
                                                                                                  
                      ASSIGN
                      j        = j + 1
                      xop.line = j.
                  if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
                end.  

                assign
                    fil_id  = recid(est-op)
                    v-recid = fil_id.
                FOR EACH ef 
                    WHERE ef.company EQ est-qty.company
                    AND ef.est-no  EQ est-qty.est-no NO-LOCK:

                    RUN set-lock (ef.form-no, NOT ll-import-selected).
                  END.                 

                  RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

                  FOR EACH ef 
                      WHERE ef.company EQ est-qty.company
                      AND ef.est-no  EQ est-qty.est-no  NO-LOCK:

                      RUN set-lock (ef.form-no, YES).
                  END.                

                  fil_id = v-recid.      

        ASSIGN
            prmLine   = i
            prmAction = "View".
        

     
END.

/**********************************************************************************************************/




IF prmAction = "RouteCopy"  THEN DO:
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
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


IF prmAction = "RouteCopy"  THEN DO:

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
          est-op.auto    = NO
          est-op.line    = i
          est-op.s-num   = prmSnum
          est-op.b-num   = if est.est-type eq 5 then 1 else 0
          est-op.qty     = IF est.est-type EQ 1 THEN est-qty.eqty ELSE prmQty 
          est-op.m-code  =  prmMcode
          est-op.m-dscr  =  prmMdscr

          est-op.op-mr        = prmOpmr     
          est-op.op-waste     = prmOpwaste  
          est-op.op-speed     = prmOpspeed  
          est-op.op-spoil     = prmOpspoil  
          est-op.op-crew[1]   = prmOpcrew   
          est-op.op-crew[2]   = prmOpcrew2  
          est-op.op-rate[1]   = prmOpRate   
          est-op.op-rate[2]   = prmOpRate2  
          est-op.plates       = prmPlates   
          est-op.fountains    = prmFountains 
          est-op.num-col      = prmInk 
          est-op.num-coat     = prmCoat
          .

          /*find xest where recid(xest) eq recid(est).

          run set-import-stds ("add", yes).
          
          FIND FIRST mach  where (mach.company eq prmComp 
                                  and  mach.loc     eq prmLoc)
              and mach.m-code eq est-op.m-code
              no-lock no-error.

          find first xef
              where xef.company eq est-op.company
              and xef.est-no  eq est-op.est-no
              and xef.form-no eq est-op.s-num
              no-lock no-error.
          ASSIGN
              est-op.d-seq      = mach.d-seq
              est-op.dept       = mach.dept[1]
              est-op.op-sb      = mach.p-type ne "B"
              est-op.m-code     = mach.m-code
              est-op.m-dscr     = mach.m-dscr
              est-op.op-rate[1] = mach.mr-trate
              est-op.op-rate[2] = mach.run-trate.
          
          if est-op.op-crew[1] eq 0 or ll-import-selected then
              est-op.op-crew[1] = mach.mr-crusiz.
          
          if est-op.op-crew[2] eq 0 or ll-import-selected  then
              est-op.op-crew[2] = mach.run-crusiz.

          if ll-import-selected then 
              assign
              est-op.op-spoil = mach.run-spoil
              est-op.num-col  = 0
              est-op.num-coat = 0.

          for each xop
              where xop.company eq est-op.company
              and xop.est-no  eq est-op.est-no
              and xop.line    lt 500
              break by xop.qty
              by xop.s-num
              by xop.b-num
              by xop.dept
              by xop.line:

              if first-of(xop.dept) then j = 0.
              assign
                  j           = j + 1
                  xop.op-pass = j.
              end.

              j = 0.
              for each xop
                  where xop.company EQ est-op.company
                  AND xop.est-no  EQ est-op.est-no
                  and xop.line    lt 500
                  by xop.qty by xop.s-num by xop.b-num by xop.d-seq by xop.op-pass:

                 /* {sys/inc/machpos.w xop share}*/
                  find first reftable
                      where reftable.reftable eq "MachinePosition"
                      and reftable.company  eq string(xop.company,"x(10)") +
                      string(xop.est-no,"x(10)")
                      and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
                      and reftable.code     eq ""
                      and reftable.code2    eq ""
                      SHARE-LOCK no-error.
                                                                                                  
                      ASSIGN
                      j        = j + 1
                      xop.line = j.
                  if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
                end.  

                assign
                    fil_id  = recid(est-op)
                    v-recid = fil_id.
                FOR EACH ef 
                    WHERE ef.company EQ est-qty.company
                    AND ef.est-no  EQ est-qty.est-no NO-LOCK:

                    RUN set-lock (ef.form-no, NOT ll-import-selected).
                  END.

                  RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

                  FOR EACH ef 
                      WHERE ef.company EQ est-qty.company
                      AND ef.est-no  EQ est-qty.est-no  NO-LOCK:

                      RUN set-lock (ef.form-no, YES).
                  END.

                  fil_id = v-recid.
               */   
                  
        /*  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).*/

        ASSIGN
            prmLine   = i
            prmAction = "View".
        

     
END.








/**************************************************************************************************************/

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
          est-op.auto    = NO
          est-op.line    = i
          est-op.s-num   = prmSnum
          est-op.b-num   = if est.est-type eq 5 then 1 else 0
          est-op.qty     = IF est.est-type EQ 1 THEN est-qty.eqty ELSE prmQty 
          est-op.m-code  =  prmMcode
          est-op.m-dscr  =  prmMdscr
          est-op.op-waste  =  prmOpwaste
          est-op.op-speed  =  prmOpspeed
          est-op.op-spoil   = prmOpspoil
          est-op.op-crew[1] =  prmOpcrew
          est-op.op-crew[2] =  prmOpcrew2
          est-op.op-mr       = prmOpmr
            .

          find xest where recid(xest) eq recid(est).       
          
          FIND FIRST mach  where (mach.company eq prmComp 
                                  and  mach.loc     eq prmLoc)
              and mach.m-code eq est-op.m-code
              no-lock no-error.

          find first xef
              where xef.company eq est-op.company
              and xef.est-no  eq est-op.est-no
              and xef.form-no eq est-op.s-num
              no-lock no-error.
          ASSIGN
              est-op.d-seq      = mach.d-seq
              est-op.dept       = mach.dept[1]
              est-op.op-sb      = mach.p-type ne "B"
              est-op.m-code     = mach.m-code
              est-op.m-dscr     = mach.m-dscr
              est-op.op-rate[1] = mach.mr-trate
              est-op.op-rate[2] = mach.run-trate.
          
          if est-op.op-crew[1] eq 0 or ll-import-selected then
              est-op.op-crew[1] = mach.mr-crusiz.
          
          if est-op.op-crew[2] eq 0 or ll-import-selected  then
              est-op.op-crew[2] = mach.run-crusiz.

          if ll-import-selected then 
              assign
              est-op.op-spoil = mach.run-spoil
              est-op.num-col  = 0
              est-op.num-coat = 0.

          for each xop
              where xop.company eq est-op.company
              and xop.est-no  eq est-op.est-no
              and xop.line    lt 500
              break by xop.qty
              by xop.s-num
              by xop.b-num
              by xop.dept
              by xop.line:

              if first-of(xop.dept) then j = 0.
              assign
                  j           = j + 1
                  xop.op-pass = j.
              end.

              j = 0.
              for each xop
                  where xop.company EQ est-op.company
                  AND xop.est-no  EQ est-op.est-no
                  and xop.line    lt 500
                  by xop.qty by xop.s-num by xop.b-num by xop.d-seq by xop.op-pass:

                 /* {sys/inc/machpos.w xop share}*/
                  find first reftable
                      where reftable.reftable eq "MachinePosition"
                      and reftable.company  eq string(xop.company,"x(10)") +
                      string(xop.est-no,"x(10)")
                      and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
                      and reftable.code     eq ""
                      and reftable.code2    eq ""
                      SHARE-LOCK no-error.
                                                                                                  
                      ASSIGN
                      j        = j + 1
                      xop.line = j.
                  if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
                end.  

                assign
                    fil_id  = recid(est-op)
                    v-recid = fil_id.
                FOR EACH ef 
                    WHERE ef.company EQ est-qty.company
                    AND ef.est-no  EQ est-qty.est-no NO-LOCK:

                    RUN set-lock (ef.form-no, NOT ll-import-selected).
                  END.

                  RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

                  FOR EACH ef 
                      WHERE ef.company EQ est-qty.company
                      AND ef.est-no  EQ est-qty.est-no  NO-LOCK:

                      RUN set-lock (ef.form-no, YES).
                  END.

                  fil_id = v-recid.
        /*  RUN cec/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).*/

        ASSIGN
            prmLine   = i
            prmAction = "View".
          
END.






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



IF prmAction = "RouteUpdate" THEN DO:
   
   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
   FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line < 500 and  ((est-op.qty eq est-qty.eqty and est.est-type eq 1) or 
                                  (est-op.qty eq 0 and est.est-type ne 1))  AND est-op.LINE = prmLine EXCLUSIVE-LOCK  NO-ERROR.

   IF AVAIL est-op  THEN DO:
       ASSIGN
          est-op.s-num     = prmSnum
          est-op.n-out     = prmNout
          est-op.m-code    =  prmMcode
          est-op.m-dscr    =  prmMdscr
          est-op.op-mr     = prmOpmr
          est-op.op-waste  =  prmOpwaste
          est-op.op-speed  =  prmOpspeed
          est-op.op-spoil   = prmOpspoil
          est-op.op-crew[1] =  prmOpcrew
          est-op.op-crew[2] =  prmOpcrew2
         /* est-op.NUM-COL    =  prmInk
          est-op.num-coat   =  prmCoat*/
           .

      /* RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).*/
   END.    

        ASSIGN
            prmAction = "View".
 
 END.


IF prmAction = "RouteImport" THEN DO: 
    
    FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    
    FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
    
    FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line < 500 and  ((est-op.qty eq est-qty.eqty and est.est-type eq 1) or 
                                  (est-op.qty eq 0 and est.est-type ne 1))  AND est-op.LINE = prmLine EXCLUSIVE-LOCK  NO-ERROR.

    run set-import-stds ("update", yes).

   IF AVAIL est-op  THEN DO:
       ASSIGN
          est-op.s-num     = prmSnum
          est-op.n-out     = prmNout
          est-op.m-code    =  prmMcode
          est-op.m-dscr    =  prmMdscr
          est-op.op-mr     = prmOpmr
          est-op.op-waste  =  prmOpwaste
          est-op.op-speed  =  prmOpspeed
          est-op.op-spoil   = prmOpspoil
          est-op.op-crew[1] =  prmOpcrew
          est-op.op-crew[2] =  prmOpcrew2
         /* est-op.NUM-COL    =  prmInk
          est-op.num-coat   =  prmCoat*/
           .

       find xest where recid(xest) eq recid(est).   
          
          FIND FIRST mach  where (mach.company eq prmComp 
                                  and  mach.loc     eq prmLoc)
              and mach.m-code eq est-op.m-code
              no-lock no-error.

          find first xef
              where xef.company eq est-op.company
              and xef.est-no  eq est-op.est-no
              and xef.form-no eq est-op.s-num
              no-lock no-error.
          ASSIGN
              est-op.d-seq      = mach.d-seq
              est-op.dept       = mach.dept[1]
              est-op.op-sb      = mach.p-type ne "B"
              est-op.m-code     = mach.m-code
              est-op.m-dscr     = mach.m-dscr
              est-op.op-rate[1] = mach.mr-trate
              est-op.op-rate[2] = mach.run-trate.
          
          if est-op.op-crew[1] eq 0 or ll-import-selected then
              est-op.op-crew[1] = mach.mr-crusiz.
          
          if est-op.op-crew[2] eq 0 or ll-import-selected  then
              est-op.op-crew[2] = mach.run-crusiz.

          if ll-import-selected then 
              assign
              est-op.op-spoil = mach.run-spoil
              est-op.num-col  = 0
              est-op.num-coat = 0.

          for each xop
              where xop.company eq est-op.company
              and xop.est-no  eq est-op.est-no
              and xop.line    lt 500
              break by xop.qty
              by xop.s-num
              by xop.b-num
              by xop.dept
              by xop.line:

              if first-of(xop.dept) then j = 0.
              assign
                  j           = j + 1
                  xop.op-pass = j.
              end.

              j = 0.
              for each xop
                  where xop.company EQ est-op.company
                  AND xop.est-no  EQ est-op.est-no
                  and xop.line    lt 500
                  by xop.qty by xop.s-num by xop.b-num by xop.d-seq by xop.op-pass:

                 /* {sys/inc/machpos.w xop share}*/
                  find first reftable
                      where reftable.reftable eq "MachinePosition"
                      and reftable.company  eq string(xop.company,"x(10)") +
                      string(xop.est-no,"x(10)")
                      and reftable.loc      eq string(xop.line - if xop.line gt 500 then 500 else 0,"9999999999")
                      and reftable.code     eq ""
                      and reftable.code2    eq ""
                      SHARE-LOCK no-error.
                                                                                                  
                      ASSIGN
                      j        = j + 1
                      xop.line = j.
                  if avail reftable then reftable.loc = string(xop.line,"9999999999"). 
                end.  

                assign
                    fil_id  = recid(est-op)
                    v-recid = fil_id.
                FOR EACH ef 
                    WHERE ef.company EQ est-qty.company
                    AND ef.est-no  EQ est-qty.est-no NO-LOCK:

                    RUN set-lock (ef.form-no, NOT ll-import-selected).
                  END.     
                  
                  
                  RUN ce/mach-rek.p (IF ll-import-all THEN ? ELSE ROWID(est-op)).

                  FOR EACH ef 
                      WHERE ef.company EQ est-qty.company
                      AND ef.est-no  EQ est-qty.est-no  NO-LOCK:

                      RUN set-lock (ef.form-no, YES).
                  END.                

                  fil_id = v-recid. 
   END.    

        ASSIGN
            prmAction = "View".

END.


IF prmAction = "RouteBuild" THEN DO:

      DEF VAR ll AS LOG NO-UNDO.
  DEF VAR lv-msg AS CHAR NO-UNDO.
  DEF VAR lv-msg1 AS CHAR NO-UNDO.

  DEF BUFFER b-est-qty FOR est-qty.


  {est/checkuse.i}
        
  v-override-mode = NO.  

  FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.  
  FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
  
  find xest where recid(xest) eq recid(est).

  IF xest.est-type EQ 1 THEN DO:
    ll = NO.

    FOR EACH b-est-qty
        WHERE b-est-qty.company EQ est-qty.company
          AND b-est-qty.est-no  EQ est-qty.est-no
        NO-LOCK BREAK BY b-est-qty.eqty:

      /*IF FIRST(b-est-qty.eqty)    AND
         NOT LAST(b-est-qty.eqty) THEN
        MESSAGE "Build routings for all quantities?" SKIP
                "  (Yes=AllQtys    No=" +
                TRIM(STRING(est-qty.eqty,">>>,>>>,>>>")) + " Only)"
            VIEW-AS ALERT-BOX QUESTION BUTTON YES-NO
            UPDATE ll. */

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

        MESSAGE "eqty" b-est-qty.eqty.

        RUN ce/mach-seq.p (b-est-qty.eqty).
      END.
    END.
  END.

  ELSE DO:  
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

    IF xest.est-type EQ 2 THEN RUN ce/box/mach-seq.p.

    ELSE
    IF xest.est-type EQ 3 THEN RUN ce/tan/mach-seq.p. 

    ELSE                       RUN ce/com/mach-seq.p (0).
  END.
END.
          
 IF prmAction = "RouteDelete" THEN DO:

     FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
     FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line < 500 and  ((est-op.qty eq prmQty and est.est-type eq 1) or 
                                  (est-op.qty eq 0 and est.est-type ne 1))  AND est-op.LINE = prmLine EXCLUSIVE-LOCK  NO-ERROR.
    IF AVAIL est-op THEN DO:
        DELETE est-op.
    END.

     FIND LAST est-op WHERE est-op.company = est.company AND est-op.est-no = est.est-no 
          AND est-op.line < 500 and  ((est-op.qty eq prmQty and est.est-type eq 1) or 
                                  (est-op.qty eq 0 and est.est-type ne 1))  NO-LOCK NO-ERROR.
    
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
      AND est-op.line < 500 and  ((est-op.qty eq prmQty and est.est-type eq 1) or 
                                  (est-op.qty eq 0 and est.est-type ne 1)) NO-LOCK :
      
      IF AVAIL est-op  THEN DO:
          CREATE ttFoldRoute.
          ASSIGN
              ttFoldRoute.vSnum          = est-op.s-num       
              ttFoldRoute.vBnum          = est-op.b-num       
              ttFoldRoute.vMcode         = est-op.m-code      
              ttFoldRoute.vMdscr         = est-op.m-dscr      
                 
              ttFoldRoute.vNout          = est-op.n-out       
              ttFoldRoute.vMR            = est-op.op-mr       
              ttFoldRoute.vOpwaste       = est-op.op-waste    
              ttFoldRoute.vOpspeed       = est-op.op-speed    
              ttFoldRoute.vOpspoil       = est-op.op-spoil    
              ttFoldRoute.vOpcrew        = est-op.op-crew[1]  
              ttFoldRoute.vOpcrew2       = est-op.op-crew[2] 
              ttFoldRoute.vOpRate        = est-op.op-rate[1]
              ttFoldRoute.vOpRate2       = est-op.op-rate[2]
              ttFoldRoute.vPlates        = est-op.plates      
              ttFoldRoute.vFountains     = est-op.fountains   
              ttFoldRoute.vInks          = est-op.num-col 
              ttFoldRoute.vCoat          = est-op.num-coat               
              ttFoldRoute.vLine          = est-op.LINE 
              .
             
      END. /* end ttFoldRouteof est-prep*/
  END. /* end of for each*/
 END. /* end of select*/


 IF prmAction = "View" THEN DO:
     
     FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
     FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
    FIND FIRST est-op WHERE est-op.company = est-qty.company AND est-op.est-no = est-qty.est-no 
        AND est-op.line < 500 and  ((est-op.qty eq prmQty and est.est-type eq 1) or 
                                  (est-op.qty eq 0 and est.est-type ne 1))  AND est-op.LINE = prmLine NO-LOCK  NO-ERROR.
        IF AVAIL est-op  THEN DO:
            CREATE ttFoldRoute.
            ASSIGN
                ttFoldRoute.vSnum          = est-op.s-num       
                ttFoldRoute.vBnum          = est-op.b-num       
                ttFoldRoute.vMcode         = est-op.m-code      
                ttFoldRoute.vMdscr         = est-op.m-dscr      
                   
                ttFoldRoute.vNout          = est-op.n-out       
                ttFoldRoute.vMR            = est-op.op-mr       
                ttFoldRoute.vOpwaste       = est-op.op-waste    
                ttFoldRoute.vOpspeed       = est-op.op-speed    
                ttFoldRoute.vOpspoil       = est-op.op-spoil    
                ttFoldRoute.vOpcrew        = est-op.op-crew[1]  
                ttFoldRoute.vOpcrew2       = est-op.op-crew[2] 
                ttFoldRoute.vOpRate        = est-op.op-rate[1]
                ttFoldRoute.vOpRate2       = est-op.op-rate[2]
                ttFoldRoute.vPlates        = est-op.plates      
                ttFoldRoute.vFountains     = est-op.fountains   
                ttFoldRoute.vInks          = est-op.num-col 
                ttFoldRoute.vCoat          = est-op.num-coat  
                
                ttFoldRoute.vLine          = est-op.LINE 
                ttFoldRoute.vtype          = est.est-type

                 .

           
  END. /* end of for each*/  
 END. /* end of view*/

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


/*------------------------------------------------------------------------------------------------*/

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
        VIEW-AS ALERT-BOX ERROR. */
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

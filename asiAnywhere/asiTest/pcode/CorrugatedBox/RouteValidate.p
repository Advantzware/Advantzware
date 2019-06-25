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

DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmAction      AS CHAR NO-UNDO.
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
    
    FIND FIRST mach where mach.company eq prmComp and  mach.loc  eq prmLoc
          AND mach.m-code EQ prmMcode NO-LOCK NO-ERROR.

    IF ((AVAIL mach AND mach.p-type EQ "B") OR (prmBnum NE 0 )) and
       not can-find(first eb
                    where eb.company  eq est.company
                      and eb.est-no   eq est.est-no
                      and eb.form-no  eq prmBnum
                      and eb.blank-no eq prmSnum)
    then do:
      cError =  "Must enter a valid Blank#" .
      RETURN.
         end.
    


    IF prmOppass  NE 1 AND
         NOT CAN-FIND(FIRST ef-nsh
                      WHERE ef-nsh.company EQ est.company
                        AND ef-nsh.est-no  EQ est.est-no
                        AND ef-nsh.form-no EQ prmSnum
                        AND ef-nsh.pass-no EQ prmOppass)
      THEN DO:
        cError = "Net Sheet does not exist for this Pass ..." .
        RETURN .
      END.

   IF prmAtype1 <> ""  THEN DO:
      FIND FIRST mach-attach NO-LOCK
        WHERE mach-attach.company  EQ prmComp
          AND mach-attach.m-code   EQ prmMcode
          AND mach-attach.att-type EQ prmAtype1  NO-ERROR.
   IF NOT AVAIL mach-attach THEN DO:
       cError = "Invalid Attachment Type for Machine, try help".
   END.
   END.

   IF prmAtype2 <> ""  THEN DO:
      FIND FIRST mach-attach NO-LOCK
        WHERE mach-attach.company  EQ prmComp
          AND mach-attach.m-code   EQ prmMcode
          AND mach-attach.att-type EQ prmAtype2  NO-ERROR.
   IF NOT AVAIL mach-attach THEN DO:
       cError = "Invalid Attachment Type for Machine, try help".
   END.
   END.

   IF prmAtype3 <> ""  THEN DO:
      FIND FIRST mach-attach NO-LOCK
        WHERE mach-attach.company  EQ prmComp
          AND mach-attach.m-code   EQ prmMcode
          AND mach-attach.att-type EQ prmAtype3  NO-ERROR.
   IF NOT AVAIL mach-attach THEN DO:
       cError = "Invalid Attachment Type for Machine, try help".
   END.
   END.

   
   RUN valid-mach NO-ERROR.

END.  /****validation******/



IF prmAction = "RouteUpdate" THEN DO:

   FIND FIRST est WHERE est.est-no =  vEstimate AND est.company = prmComp NO-LOCK NO-ERROR.
   FIND FIRST est-qty WHERE est-qty.est-no =  vEstimate AND est-qty.company = prmComp NO-LOCK NO-ERROR.
    FIND first ef  where ef.company eq est.company
                      and ef.est-no  eq est.est-no
                      and ef.form-no eq prmSnum NO-LOCK NO-ERROR.
    IF NOT AVAIL ef then do:
        ASSIGN
      cError = "Must enter a valid Form#" .
      return .
    end.
    
    FIND FIRST mach where mach.company eq prmComp and  mach.loc  eq prmLoc
          AND mach.m-code EQ prmMcode NO-LOCK NO-ERROR.

    IF ((AVAIL mach AND mach.p-type EQ "B") OR (prmBnum NE 0 )) and
       not can-find(first eb
                    where eb.company  eq est.company
                      and eb.est-no   eq est.est-no
                      and eb.form-no  eq prmBnum
                      and eb.blank-no eq prmSnum)
    then do:
        ASSIGN
      cError =  "Must enter a valid Blank#" .
      RETURN.
    end.
    


   /* IF prmOppass  NE 1 AND
         NOT CAN-FIND(FIRST ef-nsh
                      WHERE ef-nsh.company EQ est.company
                        AND ef-nsh.est-no  EQ est.est-no
                        AND ef-nsh.form-no EQ prmSnum
                        AND ef-nsh.pass-no EQ prmOppass)
      THEN DO:
        ASSIGN
        cError = "Net Sheet does not exist for this Pass ..." .
        RETURN .
      END.*/

   IF prmAtype1 <> ""  THEN DO:
   FIND FIRST mach-attach NO-LOCK
        WHERE mach-attach.company  EQ prmComp
          AND mach-attach.m-code   EQ prmMcode
          AND mach-attach.att-type EQ prmAtype1  NO-ERROR.
   IF NOT AVAIL mach-attach THEN DO:
       ASSIGN
       cError = "Invalid Attachment Type1 for Machine, try help".
   END.
   END.
   IF prmAtype2 <> ""  THEN DO:
   FIND FIRST mach-attach NO-LOCK
        WHERE mach-attach.company  EQ prmComp
          AND mach-attach.m-code   EQ prmMcode
          AND mach-attach.att-type EQ prmAtype2  NO-ERROR.
   IF NOT AVAIL mach-attach THEN DO:
       ASSIGN
       cError = "Invalid Attachment Type2 for Machine, try help".
   END.
   END.
   IF prmAtype3 <> ""  THEN DO:
   FIND FIRST mach-attach NO-LOCK
        WHERE mach-attach.company  EQ prmComp
          AND mach-attach.m-code   EQ prmMcode
          AND mach-attach.att-type EQ prmAtype3  NO-ERROR.
   IF NOT AVAIL mach-attach THEN DO:
       ASSIGN
       cError = "Invalid Attachment Type3 for Machine, try help".
   END.
   END.

RUN valid-mach NO-ERROR.
 /************valid machine code *******/


   END. /*end of validation*/


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

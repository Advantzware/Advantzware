/*------------------------------------------------------------------------
    File      : CorrLayNumLen.p
    Purpose   :  Corrugated Machine Lookup
    Syntax    :

    Description : Return a Dataset of MachineLookup

    Author(s)   : 
    Created     : 21 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
DEFINE TEMP-TABLE ttCorrLayNumLenLook NO-UNDO 
        
        FIELD vIsh-len   AS DECIMAL
        FIELD vIsh-wid   AS DECIMAL
        FIELD vGsh-len   AS DECIMAL
        FIELD vGsh-wid   AS DECIMAL
        FIELD vNsh-len   AS DECIMAL
        FIELD vNsh-wid   AS DECIMAL
        FIELD vTrim-1   AS DECIMAL
        FIELD vTrim-w   AS DECIMAL
        FIELD vN-out   AS DECIMAL
        FIELD vN-out-l   AS DECIMAL
        FIELD vN-cuts   AS DECIMAL
        FIELD vGsh-dep   AS DECIMAL
        FIELD vNsh-dep   AS DECIMAL
        FIELD vTrim-d   AS DECIMAL
        FIELD vN-out-d  AS DECIMAL
        FIELD vNum-wid  AS DECIMAL 
        FIELD vNum-len  AS DECIMAL
        FIELD vNum-up  AS DECIMAL
        FIELD die-inc  AS DECIMAL 
        FIELD vgrosslen AS DECIMAL
        FIELD  netlenfive AS DECIMAL 
        
        .
    
DEFINE DATASET dsCorrLayNumLenLook FOR ttCorrLayNumLenLook .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser       AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMach      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBoard     AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmXgrain    AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmNout      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNoutLen   AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNumLen    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNumWid    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNumUp     AS DECIMAL NO-UNDO.

DEFINE INPUT PARAMETER prmGrossLen    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmGrossWid    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmGrossDep    AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNetLen      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNetWid      AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER prmNetDep      AS DECIMAL NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorrLayNumLenLook.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".

DEF VAR prmComp AS CHAR NO-UNDO.
def new shared buffer xef for ef.
def new shared buffer xeb for eb.
def new shared buffer xest for est.
DEF var k_frac as dec init 6.25 no-undo.
def var lv-is-foam as log no-undo.
DEF TEMP-TABLE w-eb NO-UNDO LIKE eb.
DEF TEMP-TABLE w-ef NO-UNDO LIKE ef.

{sys/inc/var.i new shared}

def var ld-gsh-wid as dec no-undo.
def var ld-gsh-len as dec no-undo.
def var ld-gsh-dep as dec no-undo.
def var ld-nsh-wid as dec no-undo.
def var ld-nsh-len as dec no-undo.
def var ld-nsh-dep as dec no-undo.

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp 
    locode = "MAIN" .


if prmAction =  "On-Len" then do:
        
  FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
  FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
            
             find first style where style.company = prmComp and
                 style.style = eb.style  no-lock no-error.
             
             find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
         assign xef.m-code = prmMach
                 xef.lsh-lock = no
                 xef.roll   = YES
                 xef.board = prmBoard 
                 xef.xgrain = prmXgrain
                 xeb.num-len = INT(prmNumLen)
                 xeb.num-wid = INT(prmNumWid)
                .
       
         run cec/calc-dim1.p  .
         find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
         CREATE  ttCorrLayNumLenLook.
          assign 
                ttCorrLayNumLenLook.vIsh-len = ({sys/inc/k16.i xef.lsh-len} )
                ttCorrLayNumLenLook.vIsh-wid = ({sys/inc/k16.i xef.lsh-wid} )
                ttCorrLayNumLenLook.vGsh-len = ({sys/inc/k16.i xef.gsh-len} )
                ttCorrLayNumLenLook.vGsh-wid = ({sys/inc/k16.i xef.gsh-wid} )
                ttCorrLayNumLenLook.vNsh-len = ({sys/inc/k16.i xef.nsh-len} )
                ttCorrLayNumLenLook.vNsh-wid = ({sys/inc/k16.i xef.nsh-wid} )
                ttCorrLayNumLenLook.vTrim-1 = ({sys/inc/k16.i xef.trim-l} )
                ttCorrLayNumLenLook.vTrim-w = ({sys/inc/k16.i xef.trim-w} )
                ttCorrLayNumLenLook.vN-out  = (xef.n-out)
                ttCorrLayNumLenLook.vN-out-l = (xef.n-out-l)
                ttCorrLayNumLenLook.vN-cuts  = (xef.n-cuts)
                ttCorrLayNumLenLook.vNum-wid = (xeb.num-wid)
                ttCorrLayNumLenLook.vNum-len = (xeb.num-len)
                ttCorrLayNumLenLook.vNum-up = (xeb.num-up)
                ttCorrLayNumLenLook.die-inc = (xef.die-in)
                .
    
END.  /*if prmAction <> "search" then do*/ 

IF prmAction = "OutLen"  THEN DO:
  
     CREATE  ttCorrLayNumLenLook.

    ttCorrLayNumLenLook.vN-cuts   = ((DEC(prmNout )   - 1) +  (DEC(prmNoutLen) - 1)).

    IF ttCorrLayNumLenLook.vN-cuts  LT 0 THEN ttCorrLayNumLenLook.vN-cuts = 0.

   assign
        ld-gsh-wid = DEC(prmGrossWid)
        ld-gsh-len = DEC(prmGrossLen)
        ld-gsh-dep = DEC(prmGrossDep)
        ld-nsh-wid = DEC(prmNetWid)
        ld-nsh-len = DEC(prmNetLen)
        ld-nsh-dep = DEC(prmNetDep)

       {sys/inc/k16bb.i ld-gsh-wid}
       {sys/inc/k16bb.i ld-gsh-len}
       {sys/inc/k16bb.i ld-gsh-dep}
       {sys/inc/k16bb.i ld-nsh-wid}
       {sys/inc/k16bb.i ld-nsh-len}
       {sys/inc/k16bb.i ld-nsh-dep}
      
       if prmMach <> "" then 
          find first mach where mach.company = cocode and
                            mach.loc = locode and
                            mach.m-code = prmMach 
                            no-lock no-error.

       find first item where item.company = cocode and
                             item.i-no = prmBoard
                             no-lock no-error.

       assign ld-gsh-wid = if not avail item or item.i-code eq "E" THEN ((INT(prmNout) *  DEC(IF prmXgrain  EQ "S" THEN ld-nsh-len ELSE ld-nsh-wid)) +
                           if avail mach and mach.dept[1] eq "RC" then
                             (2 * mach.min-trimw) else 0)
                           else ld-gsh-wid
              ld-gsh-len = if not avail item or item.i-code eq "E" THEN ((INT(prmNoutLen) *
                                                                          DEC(IF prmXgrain EQ "S" THEN ld-nsh-wid ELSE ld-nsh-len)) +
                           if avail mach and mach.dept[1] eq "RC" then
                             (2 * mach.min-triml) else 0)
                           else ld-gsh-len  .
             /* ld-gsh-dep = if not avail item or item.i-code eq "E" then
                            (input ef.n-out-d * ef.nsh-dep)
                           else ld-gsh-dep.*/
       assign  ttCorrLayNumLenLook.vGsh-len = ({sys/inc/k16.i ld-gsh-len})
               ttCorrLayNumLenLook.vGsh-wid = ({sys/inc/k16.i ld-gsh-wid})
             /* ef.gsh-dep:screen-value = string({sys/inc/k16.i ld-gsh-dep})*/
              .
 END.

 if prmAction =  "Fold-On-Len" then do:

     FOR EACH w-ef:
      DELETE w-ef.
    END.
    FOR EACH w-eb:
      DELETE w-eb.
    END.
        
    MESSAGE "fold" prmEstimate .
  FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
  FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
            
             find first style where style.company = prmComp and
                 style.style = eb.style  no-lock no-error.
              
             calc-dim1: DO ON ERROR UNDO, LEAVE.

             find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
         assign xef.m-code = prmMach
                 xef.lsh-lock = no
                 xef.roll   = YES
                 xef.board = prmBoard 
                 xef.xgrain = prmXgrain
                 xeb.num-len = INT(prmNumLen)
                 xeb.num-wid = INT(prmNumWid)
                .
       
         run ce/calc-dim1.p  .
         find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
          CREATE w-ef.
        BUFFER-COPY xef TO w-ef.
        CREATE w-eb.
        BUFFER-COPY xeb TO w-eb.
        UNDO calc-dim1, LEAVE calc-dim1.
        END.

         CREATE  ttCorrLayNumLenLook.
          assign 
                ttCorrLayNumLenLook.vIsh-len = ( w-ef.lsh-len )
                ttCorrLayNumLenLook.vIsh-wid = ( w-ef.lsh-wid )
                ttCorrLayNumLenLook.vGsh-len = ( w-ef.gsh-len )
                ttCorrLayNumLenLook.vGsh-wid = ( w-ef.gsh-wid )
                ttCorrLayNumLenLook.vNsh-len = ( w-ef.nsh-len )
                ttCorrLayNumLenLook.vNsh-wid = ( w-ef.nsh-wid )
                ttCorrLayNumLenLook.vTrim-1 = ( w-ef.trim-l )
                ttCorrLayNumLenLook.vTrim-w = ( w-ef.trim-w )
                ttCorrLayNumLenLook.vN-out  = (w-ef.n-out)
               /* ttCorrLayNumLenLook.vN-out-l = (w-ef.n-out-l)*/
                ttCorrLayNumLenLook.vN-cuts  = (w-ef.n-cuts)
                ttCorrLayNumLenLook.vNum-wid = (w-eb.num-wid)
                ttCorrLayNumLenLook.vNum-len = (w-eb.num-len)
                ttCorrLayNumLenLook.vNum-up = (w-eb.num-up)
                ttCorrLayNumLenLook.die-inc = (w-ef.die-in)
                .
    
          IF AVAIL w-ef THEN DELETE w-ef.
          IF AVAIL w-eb THEN DELETE w-eb.
END.  /*if prmAction <> "search" then do*/ 


IF prmAction = "Fold-out-len" THEN DO:

      /* def var ld-gsh-wid as dec no-undo.
       def var ld-gsh-len as dec no-undo.
       def var ld-gsh-dep as dec no-undo.  */
       
        FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
        FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
      
       if prmMach  <> "" then 
          find first mach where mach.company = prmComp and
                            mach.loc = ef.loc and
                            mach.m-code = prmMach 
                            no-lock no-error.

        CREATE  ttCorrLayNumLenLook.

    ttCorrLayNumLenLook.vN-cuts   = ((DEC(prmNout )   - 1) +  (DEC(prmNoutLen) - 1)).

    IF ttCorrLayNumLenLook.vN-cuts  LT 0 THEN ttCorrLayNumLenLook.vN-cuts = 0.

       find first item where item.company = prmComp and
                             item.i-no = prmBoard
                             no-lock no-error.

       assign ld-gsh-wid = if not avail item or item.i-code eq "E" THEN ((INT(prmNout) * DEC(prmNetWid)) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-triml) else 0)
                        else DEC(prmGrossWid)
              ld-gsh-len = if not avail item or item.i-code eq "E" THEN (DEC(prmNetLen) +
                        if avail mach and mach.dept[1] eq "RC" then
                          (2 * mach.min-trimw) else 0)
                     else DEC(prmGrossLen).
       
       assign  ttCorrLayNumLenLook.vGsh-len = ( ld-gsh-len)
               ttCorrLayNumLenLook.vGsh-wid = ( ld-gsh-wid) .
   
END.

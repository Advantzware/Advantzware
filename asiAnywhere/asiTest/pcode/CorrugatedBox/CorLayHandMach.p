/*------------------------------------------------------------------------
    File      : CorLayHandMach.p
    Purpose   :  Corrugated Machine Lookup
    Syntax    :

    Description : Return a Dataset of MachineLookup

    Author(s)   : 
    Created     : 21 Feb 2009
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */

DEFINE TEMP-TABLE ttCorLayHandMachLookup NO-UNDO 
        FIELD lsh-len   AS DECIMAL
        FIELD lsh-wid   AS DECIMAL
        FIELD gsh-len   AS DECIMAL
        FIELD gsh-wid   AS DECIMAL
        FIELD nsh-len   AS DECIMAL
        FIELD nsh-wid   AS DECIMAL
        FIELD trim-l   AS DECIMAL
        FIELD trim-w   AS DECIMAL
        FIELD n-out   AS DECIMAL
        FIELD n-out-l   AS DECIMAL
        FIELD n-cuts   AS DECIMAL
        FIELD gsh-dep   AS DECIMAL
        FIELD nsh-dep   AS DECIMAL
        FIELD trim-d   AS DECIMAL
        FIELD n-out-d  AS DECIMAL
        FIELD num-wid  AS DECIMAL 
        FIELD num-len  AS DECIMAL
        FIELD num-up  AS DECIMAL
        FIELD flute   AS CHAR
        FIELD test    AS CHAR
        FIELD mdesc    AS CHAR 
        FIELD die-in   AS DECIMAL 
        FIELD gjkl  AS CHAR 
        
        .
    
DEFINE DATASET dsCorLayHandMachLookup FOR ttCorLayHandMachLookup .

DEFINE INPUT PARAMETER prmAction    AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmUser        AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmField     AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmCondition AS char  NO-UNDO.
DEFINE INPUT PARAMETER prmText      AS CHARACTER  NO-UNDO.
DEFINE INPUT PARAMETER prmEstimate  AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmForm      AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBlankno   AS INT NO-UNDO.
DEFINE INPUT PARAMETER prmBoard     AS CHAR NO-UNDO. 
DEFINE INPUT PARAMETER prmStyle     AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmMach      AS CHAR NO-UNDO.
DEFINE INPUT PARAMETER prmXgrain    AS CHAR NO-UNDO.

DEFINE INPUT-OUTPUT PARAMETER DATASET FOR dsCorLayHandMachLookup.

IF prmAction      = ? THEN ASSIGN prmAction      = "".
IF prmUser        = ? THEN ASSIGN prmUser        = "".
IF prmCondition   = ? THEN ASSIGN prmCondition   = "".
IF prmText        = ? THEN ASSIGN prmText        = "".
IF prmField       = ? THEN ASSIGN prmField       = "".
IF prmEstimate    = ? THEN ASSIGN prmEstimate    = "".
IF prmStyle       = ? THEN ASSIGN prmStyle       = "".
IF prmXgrain      = ? THEN ASSIGN prmXgrain      = "".

{sys/inc/var.i NEW shared}

DEF NEW SHARED buffer xest for est.
DEF NEW SHARED buffer xef  for ef.
DEF NEW SHARED buffer xeb  for eb.

DEF BUFFER b-eiv FOR e-item-vend.
DEF BUFFER eiv-adders FOR reftable.

def var tr-l as de.
def var tr-w as de.  /* store mach or ctrl trim defaults */
def var llen like xef.lsh-len.
def var lwid like xef.lsh-wid.
def var aaa as int init 1.
def var bbb as int init 1.  /* for xgrain flip-flop */
def new shared temp-table  formule 
                           field formule as de extent 12.
def var op as ch extent 12.
def var nextop as int.
def var num as de extent 12.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var k_frac as dec init 6.25 no-undo.
def var lv-is-foam as log no-undo.
def var lv-industry as cha no-undo.
DEF VAR li AS INT NO-UNDO.
DEFINE VARIABLE prmComp AS CHAR NO-UNDO.
DEF TEMP-TABLE tt-adder NO-UNDO FIELD tt-adder AS CHAR.

{sys/inc/f16to32.i}

{sys/inc/ceroute.i C}

FIND FIRST usercomp WHERE
     usercomp.user_id = prmUser AND
     usercomp.loc = '' AND
     usercomp.company_default = YES
     NO-LOCK NO-ERROR.

prmComp = IF AVAIL usercomp THEN usercomp.company ELSE "001".

ASSIGN 
    cocode = prmComp 
    locode = "MAIN" .


if prmAction =  "Hand" then do:

    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
    FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
    
      find xef where recid(xef) = recid(ef).
      find xeb where recid(xeb) = recid(eb).

    find first style where style.company = cocode and
                            style.style = prmStyle
                          no-lock no-error.
  if avail style and style.type = "F" then lv-is-foam = yes.
  if avail style then lv-industry = style.industry.

  find first ce-ctrl where ce-ctrl.company = cocode and
                           ce-ctrl.loc     = locode no-lock no-error.
  if prmMach ne "" then
      find first mach where (mach.company = cocode and
                             mach.loc     = locode)
      and mach.m-code = prmMach  no-lock no-error.
  if avail mach then do:
      CREATE ttCorLayHandMachLookup .
     assign tr-l = mach.min-triml
            tr-w = mach.min-trimw
            ttCorLayHandMachLookup.lsh-len = mach.max-wid
            ttCorLayHandMachLookup.lsh-wid = mach.max-len
            xef.lam-dscr = "S"
            xef.roll = mach.p-type = "R".   
     find first item where item.company = cocode 
                      and item.mat-type = "A"  
                      and item.i-no eq prmBoard
                   use-index i-no no-lock no-error.
     if not avail item or item.i-code eq "E" then
         assign ttCorLayHandMachLookup.gsh-wid = ( trunc(mach.max-len / xeb.t-len,0) * xeb.t-len +
                                 (2 * tr-w) )
                ttCorLayHandMachLookup.gsh-len = (trunc(mach.max-wid / xeb.t-wid,0) * xeb.t-wid +
                                 (2 * tr-l) )
                ttCorLayHandMachLookup.gsh-dep = if lv-is-foam then (trunc(mach.max-dep / xeb.t-dep,0) * xeb.t-dep ) else 0 .                     
     else if avail item then
          assign ttCorLayHandMachLookup.gsh-wid = (item.s-wid)
                 ttCorLayHandMachLookup.gsh-len = (item.s-len)
                 ttCorLayHandMachLookup.gsh-dep = if lv-is-foam then (item.s-dep) else 0.
/*
message "mach : lsh w:" xef.lsh-wid  "L :" xef.lsh-len skip
               "  gsh: w:" xef.gsh-wid  "L:" xef.gsh-len skip
               xef.m-code view-as alert-box.
*/               
  end.
  else assign tr-l = ce-ctrl.ls-triml
              tr-w = ce-ctrl.ls-trimw
              xef.roll = ce-ctrl.avg-cscost <> 0
              xef.lam-dscr = "S"
              ttCorLayHandMachLookup.lsh-wid = ce-ctrl.ls-length
              ttCorLayHandMachLookup.lsh-len = (ce-ctrl.ls-width)
              ttCorLayHandMachLookup.gsh-len = (xeb.t-len)
              ttCorLayHandMachLookup.gsh-wid = (xeb.t-wid)
              .
  assign tr-l = tr-l * 2
         tr-w = tr-w * 2.
          
  find first style where (style.company eq cocode)  and style.style = xeb.style no-lock no-error.

  find first item where item.company eq cocode
                   and item.i-no    eq prmBoard
         no-lock no-error.
  if avail item then do:
    find e-item of item no-lock no-error.
    IF AVAIL e-item THEN
    FIND FIRST e-item-vend OF e-item NO-LOCK NO-ERROR.

    assign xef.i-code = item.i-code
           xef.weight = item.basis-w.
    
    if not xef.lsh-lock then do:
       xef.cal = item.cal.    
       if item.i-code eq "R" then do:
         if xef.roll then assign xef.roll-wid = item.r-wid
                                 ttCorLayHandMachLookup.gsh-wid  = item.r-wid
                                 ttCorLayHandMachLookup.lsh-len = item.r-wid
                                 .
         else do:
            assign ttCorLayHandMachLookup.gsh-wid  = item.s-wid
                   ttCorLayHandMachLookup.gsh-len  = item.s-len
                   ttCorLayHandMachLookup.lsh-len =  item.s-len
                   ttCorLayHandMachLookup.lsh-wid =  item.s-wid
                   xef.lam-dscr = "S"          /* feed type */
                   xef.roll     = no
                   xef.roll-wid = 0.
         end.
       
       end. /* i-code = "R" */
       else
       if item.i-code eq "E" then do:
          ttCorLayHandMachLookup.gsh-wid = xef.lsh-wid.
          if avail e-item-vend and xef.roll then do:
             do i = 1 to 26:
                if (prmXgrain ne "S" and
                    e-item-vend.roll-w[i] lt xef.lsh-len) or
                   (prmXgrain eq "S" and
                    e-item-vend.roll-w[i] lt xef.lsh-wid)
                then next.
                if e-item-vend.roll-w[i] gt 0 then xef.gsh-wid = e-item-vend.roll-w[i].
                leave.
             end.
          end.  
          /* ysk ???
          if xef.lam-dscr eq "R" or (xef.lam-dscr ne "R" and xef.xgrain eq "S")
          then do:
                  assign xef.gsh-wid      = xef.lsh-wid
                         xef.gsh-len      = xef.lsh-len
                         xef.nsh-wid = xef.gsh-wid
                         xef.nsh-len = xef.gsh-len.
                  
          end.
          else*/ assign ttCorLayHandMachLookup.gsh-wid      = xef.lsh-wid
                      ttCorLayHandMachLookup.gsh-len      = xef.lsh-len
                      ttCorLayHandMachLookup.nsh-len = xef.gsh-len
                      ttCorLayHandMachLookup.nsh-wid = xef.gsh-wid.

          xef.roll-wid = xef.gsh-wid.
      end.   /* item.i-code = "E" */
    end. /* lsh-lock */
  end. /* avail item */
  /*  if not avail item -> all vars got values above from ce-ctrl */

  if ttCorLayHandMachLookup.n-out = 0 then ttCorLayHandMachLookup.n-out = 1.
  if ttCorLayHandMachLookup.n-out-l = 0 then ttCorLayHandMachLookup.n-out-l = 1.
  if ttCorLayHandMachLookup.n-out-d = 0 then ttCorLayHandMachLookup.n-out-d = 1.
  ttCorLayHandMachLookup.n-cuts = (ttCorLayHandMachLookup.n-out - 1) + (ttCorLayHandMachLookup.n-out-l - 1) + (ttCorLayHandMachLookup.n-out-d - 1).
  if ttCorLayHandMachLookup.n-cuts lt 0 then xef.n-cuts = 0.
  
  assign llen = ttCorLayHandMachLookup.gsh-len / ttCorLayHandMachLookup.n-out
         lwid = ttCorLayHandMachLookup.gsh-wid / ttCorLayHandMachLookup.n-out-l.

  IF CAN-DO("P,R",style.type) THEN DO:
    ASSIGN
     num[1] = 0
     num[2] = xeb.t-wid * xeb.num-len.

    FOR EACH eb NO-LOCK
        WHERE eb.company EQ xeb.company
          AND eb.est-no  EQ xeb.est-no
          AND eb.eqty    EQ xeb.eqty
          AND eb.form-no EQ xeb.form-no:
      num[1] = num[1] + (eb.t-len * eb.num-wid).
    END.

    IF prmXgrain EQ "B" THEN
      ASSIGN
       zzz    = num[1]
       num[1] = num[2]
       num[2] = zzz.
  END.

  ELSE DO:
    if xef.lam-dscr eq "R" /*or (xef.lam-dscr ne "R" and prmXgrain eq "S") */ then
       assign  zzz  = llen
               llen = lwid
               lwid = zzz.

    IF xeb.sty-lock OR ceroute-dec EQ 1 THEN DO:
      CREATE formule.
      ASSIGN
       formule[1]  = xeb.t-wid
       formule[3]  = xeb.t-wid
       formule[5]  = xeb.t-wid
       formule[7]  = xeb.t-wid
       formule[9]  = xeb.t-wid
       formule[2]  = xeb.t-len
       formule[4]  = xeb.t-len
       formule[6]  = xeb.t-len
       formule[8]  = xeb.t-len
       formule[10] = xeb.t-len
       formule[12] = xeb.die-in.
    END.
          
    ELSE DO:
      RUN est/u2kinc1c.p (RECID(xeb)).
      RUN est/u2kinc2c.p (RECID(xeb)).
      FIND FIRST formule.
    END.

    num = 0. /* zero array */

    IF ceroute-dec EQ 1 THEN DO:
      ASSIGN
       op[1] = "1"
       op[2] = "1"

       ttCorLayHandMachLookup.n-out   = 1
       ttCorLayHandMachLookup.n-out-l = 1.
      
      IF prmXgrain EQ "B" THEN
        ASSIGN
         num[1] = formule[1]
         num[2] = formule[2].
      ELSE
        ASSIGN
         num[2] = formule[1]
         num[1] = formule[2].
    END.

    ELSE DO:
      IF INDEX("B",prmXgrain) EQ 0 THEN DO:
           /* aaa = 2 -> Blk W on Press.Len *** aaa = 1 -> Blk W on Press.Wid */
        assign  aaa = 2
                bbb = 1.

        do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[aaa] + formule[use-w[j] + (use-w[j] - 1)] <= (lwid - tr-w)
            or i = 1   /* at least 1 up!!! */
            then do:
              assign
                 op[aaa]  = string(i)
                 num[aaa] = num[aaa] + formule[use-w[j] + (use-w[j] - 1)].
            end.
            else do:
               leave.
            end.
        end.
        do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[bbb] + formule[use-l[j] * 2] <= (llen - tr-l)
            or i = 1
            then assign op[bbb]  = string(i)
                        num[bbb] = num[bbb] + formule[use-l[j] * 2].
            else do:
              leave.
            end.  
        end.
      end.

      else 
      if (xef.lam-dscr eq "R" or
         (xef.lam-dscr ne "R" and index("SB",prmXgrain) gt 0)) then do:

         assign aaa = 1
                bbb = 2. /* aaa = # on layout width, bbb = # layout length */

         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[aaa] + formule[use-l[j] + (use-l[j] - 1)] <= (llen - tr-l)
            or i = 1
            then assign op[aaa]  = string(i)
                    num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
            else leave.
         end.
         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[bbb] + formule[use-w[j] * 2] <= (lwid - tr-w)
            or i = 1
            then assign op[bbb]  = string(i)
                        num[bbb] = num[bbb] + formule[use-w[j] * 2].
            else leave.
         end.
      end.
    END.

    if not xef.lsh-lock then /* autocalc */
    do:
      if prmXgrain = "B" then do:
        assign xeb.num-wid = int(op[1])
               xeb.num-len = int(op[2]).
 
        if xeb.t-len * xeb.num-len gt num[2] then  num[2] = xeb.t-len * xeb.num-len.
        if xeb.t-wid * xeb.num-wid gt num[1] then  num[1] = xeb.t-wid * xeb.num-wid.  
      end.
      else do:
        assign xeb.num-wid = int(op[1])
               xeb.num-len = int(op[2]).      
        if xeb.t-len * xeb.num-wid gt num[1] then  num[1] = xeb.t-len * xeb.num-wid.
        if xeb.t-wid * xeb.num-len gt num[2] then  num[2] = xeb.t-wid * xeb.num-len.
      end.  
    end.

    assign xeb.num-up  = xeb.num-wid * xeb.num-len
           xef.die-in  = formule[12] * xeb.num-up.
  END.

      do:
         assign   ttCorLayHandMachLookup.nsh-wid = num[2] + tr-w
                  ttCorLayHandMachLookup.nsh-len = num[1] + tr-l 
                  ttCorLayHandMachLookup.trim-w  = num[2]
                  ttCorLayHandMachLookup.trim-l  = num[1].
         if ttCorLayHandMachLookup.lsh-wid lt ttCorLayHandMachLookup.nsh-wid then ttCorLayHandMachLookup.lsh-wid = ttCorLayHandMachLookup.nsh-wid.
         if ttCorLayHandMachLookup.lsh-len lt ttCorLayHandMachLookup.nsh-len then ttCorLayHandMachLookup.lsh-len = ttCorLayHandMachLookup.nsh-len.
  end.
  if ttCorLayHandMachLookup.gsh-wid lt ttCorLayHandMachLookup.nsh-wid then ttCorLayHandMachLookup.gsh-wid = ttCorLayHandMachLookup.nsh-wid.
  if ttCorLayHandMachLookup.gsh-len lt ttCorLayHandMachLookup.nsh-len then ttCorLayHandMachLookup.gsh-len = ttCorLayHandMachLookup.nsh-len.

  IF item.i-code eq "E" then do:
    if avail mach and mach.dept[1] eq "RC" then
      assign  ttCorLayHandMachLookup.nsh-wid = ttCorLayHandMachLookup.nsh-wid - tr-w
              ttCorLayHandMachLookup.nsh-len = ttCorLayHandMachLookup.nsh-len - tr-l.

    IF ceroute-dec NE 1 THEN        
      assign ttCorLayHandMachLookup.n-out   = trunc(ttCorLayHandMachLookup.lsh-wid / ttCorLayHandMachLookup.nsh-wid,0)
             ttCorLayHandMachLookup.n-out-l = trunc(ttCorLayHandMachLookup.lsh-len / ttCorLayHandMachLookup.nsh-len,0).

    assign ttCorLayHandMachLookup.n-out-d = 1
           ttCorLayHandMachLookup.nsh-dep = xeb.t-dep
           ttCorLayHandMachLookup.trim-d  = xeb.t-dep.
  END.

  IF AVAIL mach THEN DO:
    IF ttCorLayHandMachLookup.n-out   GT mach.num-wid AND mach.num-wid NE 0 THEN
      ttCorLayHandMachLookup.n-out   = mach.num-wid.
    IF ttCorLayHandMachLookup.n-out-l GT mach.num-len AND mach.num-len NE 0 THEN
      ttCorLayHandMachLookup.n-out-l = mach.num-len.
  END.

  IF item.i-code eq "E" then do:
    assign xeb.num-dep = 1
           ttCorLayHandMachLookup.gsh-wid = if not avail item or item.i-code eq "E" then
                         ( (ttCorLayHandMachLookup.n-out   * ttCorLayHandMachLookup.nsh-wid) +
                              if avail mach and mach.dept[1] eq "RC" then
                                tr-w else 0 )
                         else ttCorLayHandMachLookup.gsh-wid
           ttCorLayHandMachLookup.gsh-len = if not avail item or item.i-code eq "E" then
                                ( (ttCorLayHandMachLookup.n-out-l * ttCorLayHandMachLookup.nsh-len) +
                              if avail mach and mach.dept[1] eq "RC" then
                                tr-l else 0 )
                             else ttCorLayHandMachLookup.gsh-len
           ttCorLayHandMachLookup.gsh-dep = if not avail item or item.i-code eq "E" then
                              (ttCorLayHandMachLookup.n-out-d * ttCorLayHandMachLookup.nsh-dep)
                            else ttCorLayHandMachLookup.gsh-dep.
                     
    if ttCorLayHandMachLookup.n-out-d eq ? then ttCorLayHandMachLookup.n-out-d = 0.
    if ttCorLayHandMachLookup.gsh-dep eq ? then ttCorLayHandMachLookup.gsh-dep = 0.
  END.

  IF lv-is-foam THEN
    ASSIGN
     ttCorLayHandMachLookup.n-out-d = 1
     ttCorLayHandMachLookup.nsh-dep = xeb.t-dep
     ttCorLayHandMachLookup.trim-d  = xeb.t-dep
     ttCorLayHandMachLookup.gsh-dep = xeb.t-dep
     xeb.num-dep = 1. 

  ASSIGN
   ttCorLayHandMachLookup.gsh-len = ROUND(ttCorLayHandMachLookup.gsh-len * li-16-32,0)
   ttCorLayHandMachLookup.gsh-len = ttCorLayHandMachLookup.gsh-len / li-16-32
   ttCorLayHandMachLookup.gsh-wid = ROUND(ttCorLayHandMachLookup.gsh-wid * li-16-32,0)
   ttCorLayHandMachLookup.gsh-wid = ttCorLayHandMachLookup.gsh-wid / li-16-32
   ttCorLayHandMachLookup.gsh-dep = ROUND(ttCorLayHandMachLookup.gsh-dep * li-16-32,0)
   ttCorLayHandMachLookup.gsh-dep = ttCorLayHandMachLookup.gsh-dep / li-16-32
   ttCorLayHandMachLookup.nsh-len = ROUND(ttCorLayHandMachLookup.nsh-len * li-16-32,0)
   ttCorLayHandMachLookup.nsh-len = ttCorLayHandMachLookup.nsh-len / li-16-32
   ttCorLayHandMachLookup.nsh-wid = ROUND(ttCorLayHandMachLookup.nsh-wid * li-16-32,0)
   ttCorLayHandMachLookup.nsh-wid = ttCorLayHandMachLookup.nsh-wid / li-16-32
   ttCorLayHandMachLookup.nsh-dep = ROUND(ttCorLayHandMachLookup.nsh-dep * li-16-32,0)
   ttCorLayHandMachLookup.nsh-dep = ttCorLayHandMachLookup.nsh-dep / li-16-32
   ttCorLayHandMachLookup.trim-l  = ROUND(ttCorLayHandMachLookup.trim-l * li-16-32,0)
   ttCorLayHandMachLookup.trim-l  = ttCorLayHandMachLookup.trim-l / li-16-32
   ttCorLayHandMachLookup.trim-w  = ROUND(ttCorLayHandMachLookup.trim-w * li-16-32,0)
   ttCorLayHandMachLookup.trim-w  = ttCorLayHandMachLookup.trim-w / li-16-32
   ttCorLayHandMachLookup.trim-d  = ROUND(ttCorLayHandMachLookup.trim-d * li-16-32,0)
   ttCorLayHandMachLookup.trim-d  = ttCorLayHandMachLookup.trim-d / li-16-32.

  
  IF prmXgrain = "S" THEN ASSIGN zzz = ttCorLayHandMachLookup.gsh-len
                                  ttCorLayHandMachLookup.gsh-len =  ttCorLayHandMachLookup.gsh-wid
                                  ttCorLayHandMachLookup.gsh-wid = zzz.

  FIND FIRST mach WHERE  mach.company = prmComp AND mach.m-code = prmMach  NO-LOCK NO-ERROR.
        IF AVAIL mach THEN DO:
             ASSIGN
                ttCorLayHandMachLookup.mdesc =  mach.m-dscr    .
        END.

  FIND FIRST item WHERE item.company = prmComp AND ITEM.i-no = prmBoard NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN DO:
                ASSIGN
                    ttCorLayHandMachLookup.flute = ITEM.flute  . 
                FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
                IF AVAIL stack-flute  THEN do:
                    ASSIGN
                        ttCorLayHandMachLookup.test            = stack-flute.row-value[1].
                END. /*IF AVAIL ITEM  THEN DO:*/

            END.

    
END.



/***************************machine ******************************************/

IF prmAction = "Machine" THEN DO:

   
    FIND FIRST ef WHERE ef.est-no = FILL(" ",8 - LENGTH(TRIM(prmEstimate))) + TRIM(prmEstimate) AND ef.company = prmComp AND ef.form-no = prmForm NO-LOCK NO-ERROR.
  FIND FIRST eb WHERE eb.est-no = ef.est-no AND eb.company = ef.company AND eb.form-no = ef.form-no AND eb.blank-no = prmBlankno NO-LOCK NO-ERROR.
            
             find first style where style.company = prmComp and
                 style.style = eb.style  no-lock no-error.
             IF AVAIL style THEN
                 DO:
                 IF style.type = "F" then lv-is-foam = yes.
                 END.
             find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
         assign xef.m-code = prmMach
               xef.lsh-lock = no
                 xef.roll   = YES
                 xef.board = prmBoard
                 xef.xgrain = prmXgrain 
                .
       
         run cec/calc-dim.p  .
         find xef where recid(xef) = recid(ef).
         find xeb where recid(xeb) = recid(eb).
            CREATE ttCorLayHandMachLookup .
          assign 
                ttCorLayHandMachLookup.lsh-len = ({sys/inc/k16.i xef.lsh-len} )
                ttCorLayHandMachLookup.lsh-wid = ({sys/inc/k16.i xef.lsh-wid} )
                ttCorLayHandMachLookup.gsh-len = ({sys/inc/k16.i xef.gsh-len} )
                ttCorLayHandMachLookup.gsh-wid = ({sys/inc/k16.i xef.gsh-wid} )
                ttCorLayHandMachLookup.nsh-len = ({sys/inc/k16.i xef.nsh-len} )
                ttCorLayHandMachLookup.nsh-wid = ({sys/inc/k16.i xef.nsh-wid} )
                ttCorLayHandMachLookup.trim-l = ({sys/inc/k16.i xef.trim-l} )
                ttCorLayHandMachLookup.trim-w = ({sys/inc/k16.i xef.trim-w} )
                ttCorLayHandMachLookup.n-out  = (xef.n-out)
                ttCorLayHandMachLookup.n-out-l = (xef.n-out-l)
                ttCorLayHandMachLookup.n-cuts  = (xef.n-cuts)
                /*ef.roll:SCREEN-VALUE = IF xef.roll THEN "Y" ELSE "N"
                lv-is-roll = xef.roll
                ef.adder[01]:SCREEN-VALUE = xef.adder[01]
                ef.adder[02]:SCREEN-VALUE = xef.adder[02]
                ef.adder[03]:SCREEN-VALUE = xef.adder[03]
                ef.adder[04]:SCREEN-VALUE = xef.adder[04]
                ef.adder[05]:SCREEN-VALUE = xef.adder[05]
                ef.adder[06]:SCREEN-VALUE = xef.adder[06]
                ef.adder[07]:SCREEN-VALUE = xef.adder[07]
                ef.adder[08]:SCREEN-VALUE = xef.adder[08]
                ef.adder[09]:SCREEN-VALUE = xef.adder[09]
                ef.adder[10]:SCREEN-VALUE = xef.adder[10]
                ef.adder[11]:SCREEN-VALUE = xef.adder[11]
                ef.adder[12]:SCREEN-VALUE = xef.adder[12]*/
                ttCorLayHandMachLookup.num-wid = (xeb.num-wid)
                ttCorLayHandMachLookup.num-len = (xeb.num-len)
                ttCorLayHandMachLookup.num-up = (xeb.num-up)
                ttCorLayHandMachLookup.die-in = (xef.die-in)
                .
        /* RUN roll-display.*/

         if lv-is-foam then       
           assign ttCorLayHandMachLookup.gsh-dep = ({sys/inc/k16.i xef.gsh-dep} )
                  ttCorLayHandMachLookup.nsh-dep = ({sys/inc/k16.i xef.nsh-dep} )
                  ttCorLayHandMachLookup.trim-d  = ({sys/inc/k16.i xef.trim-d} )
                  ttCorLayHandMachLookup.n-out-d  = (xef.n-out-d)
                    .
         
         FIND FIRST mach WHERE  mach.company = prmComp AND mach.m-code = prmMach  NO-LOCK NO-ERROR.
        IF AVAIL mach THEN DO:
             ASSIGN
                ttCorLayHandMachLookup.mdesc =  mach.m-dscr    .
        END.

  FIND FIRST item WHERE item.company = prmComp AND ITEM.i-no = prmBoard NO-LOCK NO-ERROR.
            IF AVAIL ITEM THEN DO:
                ASSIGN
                    ttCorLayHandMachLookup.flute = ITEM.flute  . 
                FIND FIRST stack-flute WHERE stack-flute.company = prmComp AND (stack-flute.code = ITEM.flute ) NO-LOCK NO-ERROR.
                IF AVAIL stack-flute  THEN do:
                    ASSIGN
                        ttCorLayHandMachLookup.test            = stack-flute.row-value[1].
                END. /*IF AVAIL ITEM  THEN DO:*/
            END.


END.

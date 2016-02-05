/* cec/calc-dim.p  from cec/u2k.p  */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

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

DEF TEMP-TABLE tt-adder NO-UNDO FIELD tt-adder AS CHAR.

{sys/inc/f16to32.i}

{sys/inc/ceroute.i C}

  FIND CURRENT xeb.
  FIND CURRENT xef.
  find first style where style.company = cocode and
                         style.style = xeb.style
                         no-lock no-error.
  if avail style and style.type = "F" then lv-is-foam = yes.
  if avail style then lv-industry = style.industry.
         
  find first ce-ctrl where ce-ctrl.company = cocode and
                           ce-ctrl.loc     = locode no-lock no-error.
  if xef.m-code ne "" then
      find first mach {sys/ref/mach.w} and mach.m-code = xef.m-code  no-lock no-error.
  if avail mach then do:
     assign tr-l = mach.min-triml
            tr-w = mach.min-trimw
            xef.lsh-len = mach.max-wid
            xef.lsh-wid = mach.max-len
            xef.lam-dscr = "S"
            xef.roll = mach.p-type = "R".   
     find first item where item.company = cocode 
                      and item.mat-type = "A"  
                      and item.i-no eq xef.board
                   use-index i-no no-lock no-error.
     if not avail item or item.i-code eq "E" then
         assign xef.gsh-wid = ( trunc(mach.max-len / xeb.t-len,0) * xeb.t-len +
                                 (2 * tr-w) )
                xef.gsh-len = (trunc(mach.max-wid / xeb.t-wid,0) * xeb.t-wid +
                                 (2 * tr-l) )
                xef.gsh-dep = if lv-is-foam then (trunc(mach.max-dep / xeb.t-dep,0) * xeb.t-dep ) else 0 .                     
     else if avail item then
          assign xef.gsh-wid = (item.s-wid)
                 xef.gsh-len = (item.s-len)
                 xef.gsh-dep = if lv-is-foam then (item.s-dep) else 0.
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
              xef.lsh-wid = ce-ctrl.ls-length
              xef.lsh-len = (ce-ctrl.ls-width)
              xef.gsh-len = (xeb.t-len)
              xef.gsh-wid = (xeb.t-wid)
              .
  assign tr-l = tr-l * 2
         tr-w = tr-w * 2.
          
         
  find first style {sys/ref/style.w} and style.style = xeb.style no-lock no-error.

  find first item where item.company eq cocode
                   and item.i-no    eq xef.board
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
                                 xef.gsh-wid  = item.r-wid
                                 xef.lsh-len = item.r-wid
                                 .
         else do:
            assign xef.gsh-wid  = item.s-wid
                   xef.gsh-len  = item.s-len
                   xef.lsh-len =  item.s-len
                   xef.lsh-wid =  item.s-wid
                   xef.lam-dscr = "S"          /* feed type */
                   xef.roll     = no
                   xef.roll-wid = 0.
         end.
       
       end. /* i-code = "R" */
       else
       if item.i-code eq "E" then do:
          xef.gsh-wid = xef.lsh-wid.
          if avail e-item-vend and xef.roll then do:
             do i = 1 to 26:
                if (xef.xgrain ne "S" and
                    e-item-vend.roll-w[i] lt xef.lsh-len) or
                   (xef.xgrain eq "S" and
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
          else*/ assign xef.gsh-wid      = xef.lsh-wid
                      xef.gsh-len      = xef.lsh-len
                      xef.nsh-len = xef.gsh-len
                      xef.nsh-wid = xef.gsh-wid.

          xef.roll-wid = xef.gsh-wid.
      end.   /* item.i-code = "E" */
    end. /* lsh-lock */
  end. /* avail item */
  
  /*  if not avail item -> all vars got values above from ce-ctrl */

  if xef.n-out = 0 then xef.n-out = 1.
  if xef.n-out-l = 0 then xef.n-out-l = 1.
  if xef.n-out-d = 0 then xef.n-out-d = 1.
  xef.n-cuts = (xef.n-out - 1) + (xef.n-out-l - 1) + (xef.n-out-d - 1).
  if xef.n-cuts lt 0 then xef.n-cuts = 0.
  
  assign llen = xef.gsh-len / xef.n-out
         lwid = xef.gsh-wid / xef.n-out-l.

  /*IF CAN-DO("P,R",style.type) THEN DO:
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

    IF xef.xgrain EQ "B" THEN
      ASSIGN
       zzz    = num[1]
       num[1] = num[2]
       num[2] = zzz.
  END.

  ELSE*/ DO:
    if xef.lam-dscr eq "R" /*or (xef.lam-dscr ne "R" and xef.xgrain eq "S") */ then
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

       xef.n-out   = 1
       xef.n-out-l = 1.
      
      IF xef.xgrain EQ "B" THEN
        ASSIGN
         num[1] = formule[1]
         num[2] = formule[2].
      ELSE
        ASSIGN
         num[2] = formule[1]
         num[1] = formule[2].
    END.

    ELSE DO:
      IF INDEX("B",xef.xgrain) EQ 0 THEN DO:
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
         (xef.lam-dscr ne "R" and index("SB",xef.xgrain) gt 0)) then do:

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


    /* if not xef.lsh-lock then /* autocalc */ */
/*     do: */
/*       if xef.xgrain = "B" then do: */
/*         assign xeb.num-wid = int(op[1]) */
/*                xeb.num-len = int(op[2]). */
/*    */
/*         if xeb.t-len * xeb.num-len gt num[2] then  num[2] = xeb.t-len * xeb.num-len. */
/*         if xeb.t-wid * xeb.num-wid gt num[1] then  num[1] = xeb.t-wid * xeb.num-wid. */
/*       end. */
/*       else do: */
/*         assign xeb.num-wid = int(op[1]) */
/*                xeb.num-len = int(op[2]). */
/*         if xeb.t-len * xeb.num-wid gt num[1] then  num[1] = xeb.t-len * xeb.num-wid. */
/*         if xeb.t-wid * xeb.num-len gt num[2] then  num[2] = xeb.t-wid * xeb.num-len. */
/*       end. */
/*     end. */

    assign xeb.num-up  = xeb.num-wid * xeb.num-len
       /*    xef.die-in  = formule[12] * xeb.num-up  not for Artios CAD */.
           
  END.

  do:
         assign num[1] = xef.trim-l
                   num[2] = xef.trim-w  
                   xef.nsh-wid = num[2]  + tr-w
                   xef.nsh-len = num[1]  + tr-l 
                   xef.trim-w  = num[2]  
                   xef.trim-l  = num[1]   .
         if xef.lsh-wid lt xef.nsh-wid then xef.lsh-wid = xef.nsh-wid.
         if xef.lsh-len lt xef.nsh-len then xef.lsh-len = xef.nsh-len.
         
  end.
  if xef.gsh-wid lt xef.nsh-wid then xef.gsh-wid = xef.nsh-wid.
  if xef.gsh-len lt xef.nsh-len then xef.gsh-len = xef.nsh-len.

  IF item.i-code eq "E" then do:
    if avail mach and mach.dept[1] eq "RC" then
      assign  xef.nsh-wid = xef.nsh-wid - tr-w
              xef.nsh-len = xef.nsh-len - tr-l.

    IF ceroute-dec NE 1 THEN        
      assign xef.n-out   = trunc(xef.lsh-wid / xef.nsh-wid,0)
             xef.n-out-l = trunc(xef.lsh-len / xef.nsh-len,0).

    assign xef.n-out-d = 1
           xef.nsh-dep = xeb.t-dep
           xef.trim-d  = xeb.t-dep.
  END.

  IF AVAIL mach THEN DO:
    IF xef.n-out   GT mach.num-wid AND mach.num-wid NE 0 THEN
      xef.n-out   = mach.num-wid.
    IF xef.n-out-l GT mach.num-len AND mach.num-len NE 0 THEN
      xef.n-out-l = mach.num-len.
  END.

  IF item.i-code eq "E" then do:
    assign xeb.num-dep = 1
           xef.gsh-wid = if not avail item or item.i-code eq "E" then
                         ( (xef.n-out   * xef.nsh-wid) +
                              if avail mach and mach.dept[1] eq "RC" then
                                tr-w else 0 )
                         else xef.gsh-wid
           xef.gsh-len = if not avail item or item.i-code eq "E" then
                                ( (xef.n-out-l * xef.nsh-len) +
                              if avail mach and mach.dept[1] eq "RC" then
                                tr-l else 0 )
                             else xef.gsh-len
           xef.gsh-dep = if not avail item or item.i-code eq "E" then
                              (xef.n-out-d * xef.nsh-dep)
                            else xef.gsh-dep.
                     
    if xef.n-out-d eq ? then xef.n-out-d = 0.
    if xef.gsh-dep eq ? then xef.gsh-dep = 0.
  END.

  IF lv-is-foam THEN
    ASSIGN
     xef.n-out-d = 1
     xef.nsh-dep = xeb.t-dep
     xef.trim-d  = xeb.t-dep
     xef.gsh-dep = xeb.t-dep
     xeb.num-dep = 1. 

  IF v-cecscrn-char NE "Decimal" THEN
  ASSIGN
   xef.gsh-len = ROUND(xef.gsh-len * li-16-32,0)
   xef.gsh-len = xef.gsh-len / li-16-32
   xef.gsh-wid = ROUND(xef.gsh-wid * li-16-32,0)
   xef.gsh-wid = xef.gsh-wid / li-16-32
   xef.gsh-dep = ROUND(xef.gsh-dep * li-16-32,0)
   xef.gsh-dep = xef.gsh-dep / li-16-32
   xef.nsh-len = ROUND(xef.nsh-len * li-16-32,0)
   xef.nsh-len = xef.nsh-len / li-16-32
   xef.nsh-wid = ROUND(xef.nsh-wid * li-16-32,0)
   xef.nsh-wid = xef.nsh-wid / li-16-32
   xef.nsh-dep = ROUND(xef.nsh-dep * li-16-32,0)
   xef.nsh-dep = xef.nsh-dep / li-16-32
   xef.trim-l  = ROUND(xef.trim-l * li-16-32,0)
   xef.trim-l  = xef.trim-l / li-16-32
   xef.trim-w  = ROUND(xef.trim-w * li-16-32,0)
   xef.trim-w  = xef.trim-w / li-16-32
   xef.trim-d  = ROUND(xef.trim-d * li-16-32,0)
   xef.trim-d  = xef.trim-d / li-16-32.

  IF xef.xgrain = "S" THEN ASSIGN zzz = xef.gsh-len
                                  xef.gsh-len =  xef.gsh-wid
                                  xef.gsh-wid = zzz.

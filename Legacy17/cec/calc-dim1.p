/* cec/calc-dim1.p  from cec/u2k.p  same as cec/calc-dim.p but called from num-len,num-wid */

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def var tr-l as de NO-UNDO.
def var tr-w as de NO-UNDO.  /* store mach or ctrl trim defaults */
def var llen like xef.lsh-len NO-UNDO.
def var lwid like xef.lsh-wid NO-UNDO.
def var aaa as int init 1 NO-UNDO.
def var bbb as int init 1 NO-UNDO.  /* for xgrain flip-flop */
def new shared temp-table  formule 
                           field formule as de extent 12.
def var op as ch extent 12 NO-UNDO.
def var nextop as INT NO-UNDO.
def var num as de extent 12 NO-UNDO.
def var curnum as ch NO-UNDO.
def var kar as ch format "x" NO-UNDO.  /* style formula kalk variables */
def var k_frac as dec init 6.25 no-undo.
def var lv-industry as cha no-undo.

{sys/inc/f16to32.i}
 
  if not xef.lsh-lock then /* autocalc */
  do:
    if xef.xgrain = "B" THEN
       ASSIGN
          num[2] = xeb.t-len * xeb.num-wid
          num[1] = xeb.t-wid * xeb.num-len.
    
    ELSE
       ASSIGN
          num[1] = xeb.t-len * xeb.num-wid
          num[2] = xeb.t-wid * xeb.num-len.
  end.
  else do:  /* no need to calc num-len,num-wid */
  end.

  find style {sys/ref/styleW.i} and
           style.style = XEB.style no-lock no-error.

      if XEF.board ne "" THEN
      find first item {sys/look/itemW.i} and item.i-no = XEF.board no-lock no-error.
      if avail item then
      find first e-item
          where e-item.company eq item.company
            and /* e-item.loc  eq item.loc
            and */ e-item.i-no eq item.i-no
          no-lock no-error.
      IF AVAIL e-item THEN
      FIND FIRST e-item-vend OF e-item NO-LOCK NO-ERROR.
      find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
      find first mach {sys/look/machW.i} and mach.m-code = XEF.m-code
      no-lock no-error.
      if available mach then assign TR-L = mach.min-triml * 2
                                    TR-W = mach.min-trimw * 2.
      else assign TR-L = ce-ctrl.ls-triml * 2
                  TR-W = ce-ctrl.ls-trimw * 2.
          
      IF xeb.sty-lock THEN DO:
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

      if index("B",xef.xgrain) GT 0 then do:
         do i = 1 to XEB.num-wid:
            j = i.
            if i > 13 then j = 13.
            ASSIGN
               op[1]  = string(i)
               num[1] = num[1] + formule[use-w[j] + (use-w[j] - 1)].
         end.
         do i = 1 to XEB.num-len:
            j = i.
            if i > 13 then j = 13.
            ASSIGN
               op[2]  = string(i)
               num[2] = num[2] + formule[use-l[j] * 2].
         end.
         ASSIGN
          xef.trim-w = num[2]
          xef.trim-l = num[1].
      end.

      ELSE do:
            do i = 1 to XEB.num-len:
               j = i.
               if i > 13 then j = 13.
               ASSIGN
                  op[1]  = string(i)
                  num[1] = num[1] + formule[use-l[j] + (use-l[j] - 1)].
            end.
            do i = 1 to XEB.num-wid:
               j = i.
               if i > 13 then j = 13.
               ASSIGN
                  op[2]  = string(i)
                  num[2] = num[2] + formule[use-w[j] * 2].
            end.
            ASSIGN
             xef.trim-w = num[1]
             xef.trim-l = num[2].
      end.
  
      ASSIGN
         xeb.num-up = xeb.num-wid * xeb.num-len
         xef.die-in = formule[12] * xeb.num-up.

      if xef.i-code eq "E" then do:
         if xef.roll eq true then do:
            xef.gsh-wid = 0.
            if avail e-item-vend and e-item-vend.roll-w[1] ne 0 then
            do i = 1 to 26:
               if xef.xgrain ne "S" and
                  e-item-vend.roll-w[i] lt (xef.trim-w + tr-w) or
                  xef.xgrain eq "S" and
                  e-item-vend.roll-w[i] lt (xef.trim-l + tr-l)
               then next.
               if e-item-vend.roll-w[i] gt 0 then assign xef.gsh-wid = e-item-vend.roll-w[i].
               leave.
            end.
          
            IF xef.gsh-wid EQ 0 THEN
              IF xef.xgrain EQ "S" THEN
                xef.gsh-wid = xef.trim-l + tr-l.
              ELSE
                xef.gsh-wid = xef.trim-w + tr-w.
            
            IF xef.xgrain EQ "S" THEN
              xef.gsh-len = xef.trim-w + tr-w.
            ELSE
              xef.gsh-len = xef.trim-l + tr-l. 

            xef.roll-wid = xef.gsh-wid.
         end.

         else
         if xef.xgrain eq "S" then 
           assign 
            xef.gsh-wid = xef.trim-l + tr-l
            xef.gsh-len = xef.trim-w + tr-w.

         else 
           assign 
            xef.gsh-wid = xef.trim-w + tr-w
            xef.gsh-len = xef.trim-l + tr-l.

         if xef.xgrain eq "S" then
           assign
            xef.nsh-len = xef.gsh-wid
            xef.nsh-wid = xef.gsh-len.
         else
           assign
            xef.nsh-wid = xef.gsh-wid
            xef.nsh-len = xef.gsh-len.
      end.

      else do: /* real item */
         assign 
          xef.nsh-wid  = xef.trim-w + tr-w
          xef.nsh-len  = xef.trim-l + tr-l.

         if xef.roll eq true then
           if xef.xgrain eq "S" then
             xef.gsh-len = xef.nsh-wid. 
           else 
             xef.gsh-len = xef.nsh-len.

         if xef.xgrain eq "S" then 
           assign
            xef.lsh-len = xef.gsh-wid
            xef.lsh-wid = xef.gsh-len.
         else
           assign
            xef.lsh-len = xef.gsh-len
            xef.lsh-wid = xef.gsh-wid.
      end.
  
    IF item.i-code eq "E" then do:
       if avail mach and mach.dept[1] eq "RC" then
          assign  xef.nsh-wid = xef.nsh-wid - tr-w
                  xef.nsh-len = xef.nsh-len - tr-l.
      
       assign xef.n-out   = trunc(xef.lsh-wid / IF xef.xgrain EQ "S" THEN xef.nsh-len ELSE xef.nsh-wid,0)
              xef.n-out-l = trunc(xef.lsh-len / IF xef.xgrain EQ "S" THEN xef.nsh-wid ELSE xef.nsh-len,0)
              xef.n-out-d = 1
              xef.nsh-dep = xeb.t-dep
              xef.trim-d  = xeb.t-dep.
    END.

    ELSE
      assign xef.n-out   = trunc(xef.gsh-wid / IF xef.xgrain EQ "S" THEN xef.nsh-len ELSE xef.nsh-wid,0)
             xef.n-out-l = trunc(xef.gsh-len / IF xef.xgrain EQ "S" THEN xef.nsh-wid ELSE xef.nsh-len,0).

    ASSIGN
     xef.n-out-d = 1
     xef.nsh-dep = xeb.t-dep
     xef.trim-d  = xeb.t-dep
     xef.n-cuts  = (xef.n-out - 1) + (xef.n-out-l - 1) + (xef.n-out-d - 1).

    IF xef.n-cuts LT 0 THEN xef.n-cuts = 0.

    IF AVAIL mach THEN DO:
      IF xef.n-out   GT mach.num-wid AND mach.num-wid NE 0 THEN
        xef.n-out   = mach.num-wid.
      IF xef.n-out-l GT mach.num-len AND mach.num-len NE 0 THEN
        xef.n-out-l = mach.num-len.
    END.

    IF xef.n-out   LE 0 THEN xef.n-out   = 1.
    IF xef.n-out-l LE 0 THEN xef.n-out-l = 1.

    IF item.i-code eq "E" then do:
      assign xeb.num-dep = 1
             xef.gsh-wid = if not avail item or item.i-code eq "E" then
                           ( (xef.n-out   * IF xef.xgrain EQ "S" THEN xef.nsh-len ELSE xef.nsh-wid) +
                                if avail mach and mach.dept[1] eq "RC" then
                                  tr-w else 0 )
                           else xef.gsh-wid
             xef.gsh-len = if not avail item or item.i-code eq "E" then
                                  ( (xef.n-out-l * IF xef.xgrain EQ "S" THEN xef.nsh-wid ELSE xef.nsh-len) +
                                if avail mach and mach.dept[1] eq "RC" then
                                  tr-l else 0 )
                               else xef.gsh-len
             xef.gsh-dep = if not avail item or item.i-code eq "E" then
                                (xef.n-out-d * xef.nsh-dep)
                              else xef.gsh-dep.
                     
      if xef.n-out-d eq ? then xef.n-out-d = 0.
      if xef.gsh-dep eq ? then xef.gsh-dep = 0.
    END.
  
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

  IF AVAIL mach AND mach.dept[1] EQ "RC" THEN
    IF xef.gsh-len LT xef.trim-l + tr-l OR
       xef.gsh-wid LT xef.trim-w + tr-w THEN
      RUN cec/calc-dim.p.
    ELSE.
  ELSE
    IF xef.nsh-len LT xef.trim-l + tr-l OR
       xef.nsh-wid LT xef.trim-w + tr-w THEN
      RUN cec/calc-dim.p.

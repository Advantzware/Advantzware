/* cec/calc-dim1.p  from cec/u2k.p  same as cec/calc-dim.p but called from num-len,num-wid 
                       and not update num-len num-wid
*/

{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

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
DEF VAR ld AS DEC NO-UNDO.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var k_frac as dec init 6.25 no-undo.
def var lv-is-foam as log no-undo.
def var lv-industry as cha no-undo.
DEF VAR ld-tons AS DEC INIT -1 NO-UNDO.


      {sys/inc/celayout.i}
        
      IF celayout-dec GT 0 THEN RUN est/boardton.p (ROWID(ef), OUTPUT ld-tons).

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
      find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
      find first mach {sys/look/machW.i} and mach.m-code = XEF.m-code
      no-lock no-error.
      if available mach then assign TR-L = mach.min-triml * 2
                                    TR-W = mach.min-trimw * 2.
      else assign TR-L = ce-ctrl.ls-triml * 2
                  TR-W = ce-ctrl.ls-trimw * 2.

      run est/u2kinc1.p (recid(xeb)).
      run est/u2kinc2.p (recid(xeb)).
      find first formule .
  
      num = 0. /* zero array */

      if index("SB",xef.xgrain) GT 0 then do:
            do i = 1 to XEB.num-len:
               j = i.
               if i > 13 then j = 13.
               op[1]  = string(i).
               num[1] = num[1] + formule[use-w[j] + (use-w[j] - 1)].
            end.
            do i = 1 to XEB.num-wid:
               j = i.
               if i > 13 then j = 13.
               op[2]  = string(i).
               num[2] = num[2] + formule[use-l[j] * 2].
            end.
            ASSIGN
             xef.trim-w = num[2]
             xef.trim-l = num[1].
      end.

      ELSE do:
            do i = 1 to XEB.num-wid:
               j = i.
               if i > 13 then j = 13.
               op[1]  = string(i).
               num[1] = num[1] + formule[use-l[j] + (use-l[j] - 1)].
            end.
            do i = 1 to XEB.num-len:
               j = i.
               if i > 13 then j = 13.
               op[2]  = string(i).
               num[2] = num[2] + formule[use-w[j] * 2].
            end.
            ASSIGN
             xef.trim-w = num[1]
             xef.trim-l = num[2].
      end.



         ASSIGN
          xeb.num-up = xeb.num-wid * xeb.num-len
          xef.die-in = 0.

         do i = 1 to 4:
            if xef.leaf[i] ne "" and xef.leaf-bnum[i] ne 0 and
               ((xef.leaf-w[i] ne 0) and (xef.leaf-l[i] ne 0)) then
            do:
               find first item {sys/look/itemW.i} and item.i-no eq xef.leaf[i]
               no-lock no-error.
               if item.mat-type ne "W" then next.
               xef.die-in = xef.die-in +
                             ((xef.leaf-w[i] + xef.leaf-l[i]) * 2 * xeb.num-up).
            end.
         end.

         xef.die-in = xef.die-in + (formule[12] * xeb.num-up).

      if xef.i-code eq "E" then do:
         if xef.roll eq true then do:
            xef.gsh-wid = 0.
            IF ld-tons LT celayout-dec AND e-item.roll-w[1] NE 0 THEN
            do i = 1 to 26:
               if xef.xgrain ne "S" and
                  e-item.roll-w[i] lt (xef.trim-w + tr-w) or
                  xef.xgrain eq "S" and
                  e-item.roll-w[i] lt (xef.trim-l + tr-l)
               then next.
               if e-item.roll-w[i] gt 0 then assign xef.gsh-wid = e-item.roll-w[i].
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

      /* if layout less than mach minimums, make SH = minimum */
      if available mach and mach.m-code = XEF.m-code and item.i-code ne "R"
      then do:
         IF XEF.gsh-len LT mach.min-wid THEN XEF.gsh-len = mach.min-wid.
         IF XEF.gsh-wid LT mach.min-len THEN XEF.gsh-wid = mach.min-len.
         IF XEF.gsh-len GT mach.max-wid THEN XEF.gsh-len = mach.max-wid.
         IF XEF.gsh-wid GT mach.max-len THEN XEF.gsh-wid = mach.max-len.

         IF XEF.xgrain EQ "S" THEN
           ASSIGN
            xef.nsh-len = xef.gsh-wid
            xef.nsh-wid = xef.gsh-len.
         ELSE
           ASSIGN
            xef.nsh-len = xef.gsh-len
            xef.nsh-wid = xef.gsh-wid.

         if XEF.roll = true then do:
            IF item.i-code EQ "E" AND ld-tons LT celayout-dec AND e-item.roll-w[1] NE 0 THEN
            do i = 1 to 26:
               if e-item.roll-w[i] < XEF.gsh-wid then next.
               XEF.gsh-wid = e-item.roll-w[i].
               leave.
            end.
            if item.i-code = "R"
            then XEF.gsh-wid = item.r-wid.
            XEF.roll-wid = XEF.gsh-wid.
            IF XEF.xgrain EQ "S" THEN XEF.nsh-len = XEF.gsh-wid.
                                 ELSE XEF.nsh-wid = XEF.gsh-wid.
         end.
      end.

      IF AVAIL mach AND mach.p-type EQ "R" THEN
      DO i = 1 TO 20:
        IF mach.max-pan-ss[i] / 1000 LT xef.nsh-len THEN LEAVE.
        ELSE xef.gsh-len = mach.max-pan-ss[i] / 1000.
      END.

      IF xef.nsh-len LT xef.trim-l + tr-l OR
         xef.nsh-wid LT xef.trim-w + tr-w THEN
        RUN ce/calc-dim.p.

     /* xef.brd-dscr = input xef.brd-dscr. ???*/
      if xef.adh-code ne "" then xef.adh-sqin = xef.gsh-len * xef.gsh-wid.


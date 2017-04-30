/* ------------------------------------------------------- cec/u2k.p 1/92 cd  */
/* Cost Estimating - Page2 - Calculate Layout, Pass #1                        */
/* -------------------------------------------------------------------------- */

/*
{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared frame est-2.

def shared var local-head as char format "x(78)" init
" Label Code  Description             Cost/M      S/B    Length    Width  "
no-undo.
*/
def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def var tr-l as de.
def var tr-w as de.  /* store mach or ctrl trim defaults */
def var llen like xef.lsh-len.
def var lwid like xef.lsh-wid.
def var aaa as int init 1.
def var bbb as int init 1.  /* for xgrain flip-flop */
def var zzz as dec no-undo.
def new shared temp-table  formule 
                           field formule as de extent 12.
def var op as ch extent 12.
def var nextop as int.
def var num as de extent 12.
def var curnum as ch.
def var kar as ch format "x".  /* style formula kalk variables */
def var i as int no-undo.
def var j as int no-undo.
def var k_frac as dec init 6.25 no-undo.
/*
def shared var head as ch format "x(78)" extent 20.
def    shared var pass_it as recid.

{cec/est-2.f &fil=xest &fil2=xef &fil3=xeb}

*/

{sys/inc/f16to32.i}

assign lwid = xef.lsh-wid
       llen = xef.lsh-len.

blok:
do on error undo with frame est-2:

   find first ce-ctrl where ce-ctrl.company = cocode and
                            ce-ctrl.loc     = locode no-lock no-error.
   if xef.m-code ne "" then
   find first mach {sys/ref/machW.i} and mach.m-code = xef.m-code
   no-lock no-error.
   if avail mach then assign tr-l = mach.min-triml
                             tr-w = mach.min-trimw.
   else assign tr-l = ce-ctrl.ls-triml
               tr-w = ce-ctrl.ls-trimw.
   assign tr-l = tr-l * 2
          tr-w = tr-w * 2.
          
   find first style {sys/ref/styleW.i} and style.style = xeb.style
   no-lock no-error.
/*
   pause 0.
   display "                P l e a s e   W a i t !     C A L C U L A T I N G"
   with frame kalk row lorow overlay no-box no-labels width 81
   color value("blink-" + col-look).
*/
   find first item 
       where item.company eq cocode
         and item.i-no    eq xef.board
       no-lock no-error.
   if avail item then find e-item of item no-lock no-error.
   else do:
      bell.
      message
      "            B O A R D   F i e l d    Must be  A  Valid Item   Code!".
      hide message. undo blok , leave blok.
   end.

   assign
    xef.i-code = item.i-code
    xef.weight = item.basis-w.
    
   if not xef.lsh-lock then do:
      xef.cal = item.cal.
      
      if item.i-code eq "R" then do:
         if xef.roll then do:
/*         
            if item.r-wid > xef.lsh-wid and xef.lsh-wid ne 0
            then display skip(1)
"    W A R N I N G !    Board dimensions are greater than Layout Dimensions.   "
            skip(1)
            with frame warn1 color value(col-norm) no-labels
                 overlay centered row 16.
            hide frame warn1.
            assign xef.roll-wid = item.r-wid
                   xef.gsh-wid  = item.r-wid.
  */
         end.
         else do:
      /*      if (xef.xgrain ne "S" and
                (item.s-wid > xef.lsh-len and xef.lsh-len ne 0) or
                (item.s-len > xef.lsh-wid and xef.lsh-wid ne 0))   or
               (xef.xgrain = "S" and
                (item.s-len > xef.lsh-len and xef.lsh-len ne 0) or
                (item.s-wid > xef.lsh-wid and xef.lsh-wid ne 0))
            then display skip(1)
"    W A R N I N G !    Board dimensions are greater than Layout Dimensions.   "
               skip(1)
            with frame warn2 color value(col-norm) no-labels
                 overlay centered row 16.
            hide frame warn2.
    */
            assign xef.gsh-wid  = item.s-wid
                   xef.gsh-len  = item.s-len
                   xef.lam-dscr = "S"          /* feed type */
                   xef.roll     = no
                   xef.roll-wid = 0.
         end.
      end. /* i-code = "R" */
      else
      if item.i-code eq "E" then do:
         if xef.roll then do:
            do i = 1 to 26:
               if (xef.lam-dscr ne "R" and xef.xgrain ne "S" and
                    e-item.roll-w[i] lt xef.lsh-len) or
                 ((xef.lam-dscr eq "R" or
                   (xef.lam-dscr ne "R" and xef.xgrain eq "S" )) and
                   e-item.roll-w[i] lt xef.lsh-wid)
               then next.
               if e-item.roll-w[i] gt 0 then xef.gsh-wid = e-item.roll-w[i].
               leave.
            end.
         end.
         else assign xef.gsh-wid = xef.lsh-wid.  
         if xef.lam-dscr eq "R" or
                   (xef.lam-dscr ne "R" and xef.xgrain eq "S")
               then do:
                  assign xef.gsh-wid      = xef.lsh-wid
                         xef.gsh-len      = xef.lsh-len
                         xef.nsh-wid = xef.gsh-wid
                         xef.nsh-len = xef.gsh-len.
                  if xef.lam-dscr eq "R" then xef.gsh-len = xef.lsh-len.
               end.
               else assign xef.gsh-wid      = xef.lsh-wid
                           xef.gsh-len      = xef.lsh-len
                           xef.nsh-len = xef.gsh-len
                           xef.nsh-wid = xef.gsh-wid.

               xef.roll-wid = xef.gsh-wid.
      end.
      
      assign
       llen = xef.gsh-len / xef.n-out
       lwid = xef.gsh-wid / xef.n-out-l.

      if xef.lam-dscr eq "R" or
         (xef.lam-dscr ne "R" and xef.xgrain eq "S") then
        assign
         zzz  = llen
         llen = lwid
         lwid = zzz.
      
   /*   run cec/u2kinc1.p.
      run cec/u2kinc2.p.
   */   
      run est/u2kinc1c.p (recid(xeb)).
      run est/u2kinc2c.p (recid(xeb)).
      find first formule .
      num = 0. /* zero array */

      /* Sheet fed, no Xgrain - or Roll w/Blk Xgrain */
      if (xef.lam-dscr ne "R" and index("SB",xef.xgrain) eq 0) or
         (xef.lam-dscr eq "R" and xef.xgrain = "B") then do:
         /* aaa = 2 -> Blk W on Press.Len *** aaa = 1 -> Blk W on Press.Wid */
         assign
          aaa = 2
          bbb = 1.
         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[aaa] + formule[use-l[j] + (use-l[j] - 1)] <= (llen - tr-l)
            or i = 1   /* at least 1 up!!! */
            then assign
                 op[aaa]  = string(i)
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
      /* Roll Fed, no Xgrain - or Sheet w/Xgrain */
      else
      if (xef.lam-dscr eq "R" or
         (xef.lam-dscr ne "R" and index("SB",xef.xgrain) gt 0)) then do:
         assign
          aaa = 1
          bbb = 2. /* aaa = # on layout width, bbb = # layout length */
         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[aaa] + formule[use-l[j] + (use-l[j] - 1)] <= (lwid - tr-w)
            or i = 1
            then assign op[aaa]  = string(i)
                    num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
            else leave.
         end.
         do i = 1 to 50:
            j = i.
            if i > 13 then j = 13.
            if num[bbb] + formule[use-w[j] * 2] <= (llen - tr-l)
            or i = 1
            then assign op[bbb]  = string(i)
                        num[bbb] = num[bbb] + formule[use-w[j] * 2].
            else leave.
         end.
      end.
      if xef.xgrain eq "B" then do:
        assign
         xeb.num-wid = int(op[2])
         xeb.num-len = int(op[1]).
         
        if xeb.t-len * xeb.num-wid gt num[2] then
          num[2] = xeb.t-len * xeb.num-wid.
        if xeb.t-wid * xeb.num-len gt num[1] then
          num[1] = xeb.t-wid * xeb.num-len.
      end.
      
      else do:
        assign
         xeb.num-wid = int(op[1])
         xeb.num-len = int(op[2]).
         
        if xeb.t-len * xeb.num-wid gt num[1] then
          num[1] = xeb.t-len * xeb.num-wid.
        if xeb.t-wid * xeb.num-len gt num[2] then
          num[2] = xeb.t-wid * xeb.num-len.
      end.
      
      assign
       xeb.num-up  = xeb.num-wid * xeb.num-len
       xef.die-in  = formule[12] * xeb.num-up.
      if xef.lam-dscr eq "R" or
         (xef.lam-dscr ne "R" and xef.xgrain eq "S")
      then do:
         assign   xef.nsh-wid = num[2] + tr-l
                  xef.nsh-len = num[1] + tr-w 
                  xef.trim-w  = num[2]
                  xef.trim-l  = num[1].
         if xef.lsh-wid lt xef.nsh-wid then xef.lsh-wid = xef.nsh-wid.
         if xef.lsh-len lt xef.nsh-len then xef.lsh-len = xef.nsh-len.
      end.
      else do:
           assign 
                  xef.nsh-wid = num[1] + tr-w
                  xef.nsh-len = num[2] + tr-l 
                  xef.trim-w  = num[1]
                  xef.trim-l  = num[2].
         if xef.lsh-wid lt xef.nsh-wid then xef.lsh-wid = xef.nsh-wid.
         if xef.lsh-len lt xef.nsh-len then xef.lsh-len = xef.nsh-len.
      end.
      if xef.gsh-wid lt xef.nsh-wid then xef.gsh-wid = xef.nsh-wid.
      if xef.gsh-len lt xef.nsh-len then xef.gsh-len = xef.nsh-len.
   end.
/*message "in u2k Gsh W" xef.gsh-wid  " L:" xef.gsh-len skip
                "Nsh W" xef.nsh-wid " L:" xef.nsh-len skip
                num[1] num[2] tr-l tr-w
                 view-as alert-box.
*/ 
   do i = 1 to 6:
      if xef.adder[i] ne "" then do:
         find first item
             where item.company eq cocode
               and item.i-no    eq xef.adder[i]
             no-lock no-error.
         if avail item then xef.weight = xef.weight + item.basis-w.
      end.
   end.
end.
/* end ---------------------------------- copr. 1992  advanced software, inc. */

/* cec/calc-dim.p  from cec/u2k.p  */

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

 /* Calculate dimension 
     1.  item -> machine -> control table 
         item : ef.board - Is it a real item or estimate  i-code = "R" or "E"
                           roll ?
         machine :  ef.m-code 
         control file : ce-ctrl.
 */

  {sys/inc/celayout.i}

  xef.roll = NO.

  find first style where style.company = cocode and
                         style.style = xeb.style
                         no-lock no-error.
  if avail style and style.type = "F" then lv-is-foam = yes.
  if avail style then lv-industry = style.industry.

  find first ce-ctrl where ce-ctrl.company = cocode and
                           ce-ctrl.loc     = locode no-lock no-error.
  if xef.m-code ne "" then
      find first mach {sys/ref/machW.i} and mach.m-code = xef.m-code  no-lock no-error.
  if avail mach then do:
     assign tr-l = mach.min-triml
            tr-w = mach.min-trimw
            xef.lsh-len = mach.max-wid
            xef.lsh-wid = mach.max-len
            xef.lam-dscr = "S"
         /*   xef.roll = mach.p-type = "R" */.   
     find first item where item.company = cocode 
                      and item.mat-type = "A"  
                      and item.i-no eq xef.board
                   use-index i-no no-lock no-error.
     if not avail item or item.i-code eq "E" then
         assign xef.gsh-wid = ( trunc(mach.max-wid / xeb.t-wid,0) * xeb.t-wid +
                                 (2 * mach.min-trimw) )
                xef.gsh-len = (trunc(mach.max-len / xeb.t-len,0) * xeb.t-len +
                                 (2 * mach.min-triml) )
                xef.gsh-dep = if lv-is-foam then (trunc(mach.max-dep / xeb.t-dep,0) * xeb.t-dep ) else 0 .                     
     else if avail item then
          assign xef.gsh-wid = (item.s-wid)
                 xef.gsh-len = (item.s-len)
                 xef.gsh-dep = if lv-is-foam then (item.s-dep) else 0.
               
  end.
  else assign tr-l = ce-ctrl.ls-triml
              tr-w = ce-ctrl.ls-trimw
              /*xef.roll = ce-ctrl.avg-cscost <> 0 */
              xef.lam-dscr = "S"
              xef.lsh-wid = ce-ctrl.ls-length
              xef.lsh-len = ce-ctrl.ls-width
              xef.gsh-len = (xeb.t-len)
              xef.gsh-wid = (xeb.t-wid)
              .
  assign 
   tr-l = tr-l * 2
   tr-w = tr-w * 2.

  /*IF xef.xgrain EQ "S" THEN
    ASSIGN
     lwid = xef.lsh-wid
     llen = xef.lsh-len.
  ELSE*/
    ASSIGN
     lwid = xef.lsh-len
     llen = xef.lsh-wid.

  find first style {sys/ref/styleW.i} and style.style = xeb.style no-lock no-error.

  find first item where item.company eq cocode
                    and item.i-no    eq xef.board
         no-lock no-error.
  if avail item then do:
    find e-item of item no-lock no-error.
 
    assign xef.i-code = item.i-code
           xef.weight = item.basis-w.
        
    IF celayout-dec GT 0 THEN RUN est/boardton.p (ROWID(ef), OUTPUT ld-tons).

    if not xef.lsh-lock then do:
       xef.cal = item.cal.

       if item.i-code eq "R" then do:
         if item.r-wid gt 0 then 
           assign
            xef.gsh-len  = IF xef.xgrain EQ "S" THEN xef.lsh-wid
                                                ELSE xef.lsh-len
            xef.gsh-wid  = item.r-wid
            xef.roll     = yes
            xef.roll-wid = xef.gsh-wid.

         else 
           assign
            xef.gsh-len  = item.s-len
            xef.gsh-wid  = item.s-wid
            xef.roll     = no
            xef.roll-wid = 0.

         if xef.xgrain eq "S" then
           assign 
            xef.nsh-len = xef.gsh-wid
            xef.nsh-wid = xef.gsh-len
            xef.lsh-len = xef.gsh-wid
            llen = xef.gsh-len
            lwid = xef.gsh-wid.
         else
           assign 
            xef.nsh-len = xef.gsh-len
            xef.nsh-wid = xef.gsh-wid
            xef.lsh-wid = xef.gsh-len
            llen = xef.gsh-wid
            lwid = xef.gsh-len.
       end. /* i-code = "R" */

       else do:
          if xef.roll then do:
             IF ld-tons LT celayout-dec THEN
             do i = 1 to 26:
                if (xef.xgrain EQ "S" and
                    e-item.roll-w[i] lt xef.lsh-len) OR
                   (xef.xgrain NE "S" and
                    e-item.roll-w[i] lt xef.lsh-wid)
                then next.
                if e-item.roll-w[i] gt 0 then xef.gsh-wid = e-item.roll-w[i].              
                leave.
             end.

             if xef.xgrain eq "S" then 
               assign 
                xef.gsh-wid = xef.lsh-len
                xef.gsh-len = xef.lsh-wid
                xef.nsh-len = xef.gsh-wid
                xef.nsh-wid = xef.gsh-len.

             else 
               assign
                xef.gsh-wid = xef.lsh-wid
                xef.gsh-len = xef.lsh-len
                xef.nsh-wid = xef.gsh-wid
                xef.nsh-len = xef.gsh-len.

             xef.roll-wid = xef.gsh-wid.
          end.
       end.   /* item.i-code = "E" */

    end. /* lsh-lock */
  end. /* avail item */

  /*  if not avail item -> all vars got values above from ce-ctrl */
IF not xef.lsh-lock then do: 
  
  if xef.n-out = 0 then xef.n-out = 1.
  if xef.n-out-l = 0 then xef.n-out-l = 1.
  if xef.n-out-d = 0 then xef.n-out-d = 1.
  xef.n-cuts = (xef.n-out - 1) + /*(xef.n-out-l - 1) +*/ (xef.n-out-d - 1).
  if xef.n-cuts lt 0 then xef.n-cuts = 0.  

  run est/u2kinc1.p (recid(xeb)).
  run est/u2kinc2.p (recid(xeb)).
  find first formule.

  IF formule[1] NE xeb.t-wid THEN
    ASSIGN
     formule[1]  = xeb.t-wid
     formule[3]  = xeb.t-wid
     formule[5]  = xeb.t-wid
     formule[7]  = xeb.t-wid
     formule[9]  = xeb.t-wid.

  IF formule[2] NE xeb.t-len THEN
    ASSIGN
     formule[2]  = xeb.t-len
     formule[4]  = xeb.t-len
     formule[6]  = xeb.t-len
     formule[8]  = xeb.t-len
     formule[10] = xeb.t-len.
  
  num = 0. /* zero array */

  if index("SB",xef.xgrain) eq 0 then do:
         /* aaa = 2 -> Blk W on Press.Len *** aaa = 1 -> Blk W on Press.Wid */
            aaa = 2.
            bbb = 1.
            do i = 1 to 50:
               j = i.
               if i gt 13 then j = 13.
               if num[aaa] + formule[use-l[j] + (use-l[j] - 1)]
               le (llen - tr-w) then do:
                  op[aaa]  = string(i).
                  num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
               end.
               else leave.
            end.
            do i = 1 to 50:
               j = i.
               if i gt 13 then j = 13.
               if num[bbb] + formule[use-w[j] * 2]
               le (lwid - tr-l) then do:
                  op[bbb]  = string(i).
                  num[bbb] = num[bbb] + formule[use-w[j] * 2].
               end.
               else leave.
            end.
  end.

  else do:
            aaa = 1.
            bbb = 2.
            do i = 1 to 50:
               j = i.
               if i gt 13 then j = 13.
               if num[aaa] + formule[use-l[j] + (use-l[j] - 1)]
               le (lwid - tr-l) then do:
                  op[aaa]  = string(i).
                  num[aaa] = num[aaa] + formule[use-l[j] + (use-l[j] - 1)].
               end.
               else leave.
            end.
            do i = 1 to 50:
               j = i.
               if i gt 13 then j = 13.
               if num[bbb] + formule[use-w[j] * 2]
               le (llen - tr-w) then do:
                  op[bbb]  = string(i).
                  num[bbb] = num[bbb] + formule[use-w[j] * 2].
               end.
               else leave.
            end.
  end.

         xeb.num-wid = integer(op[2]).
         xeb.num-len = integer(op[1]).
     
         /* CTS - default num-wid / num-len to max defined in mach or ce-ctrl */
         {sys/inc/maxnumon.i &blank = xeb}
         /* CTS end */
         xeb.num-up = xeb.num-wid * xeb.num-len.
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

         assign
          xef.die-in = xef.die-in + (formule[12] * xeb.num-up)
          xef.trim-l = if avail mach and mach.min-wid gt num[1] then
                         mach.min-wid else num[1]
          xef.trim-w = if avail mach and mach.min-len gt num[2] then
                         mach.min-len else num[2].
end.

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

         RUN round-dim (INPUT-OUTPUT xef.gsh-wid).

         RUN round-dim (INPUT-OUTPUT xef.gsh-len).

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
          xef.nsh-wid = xef.trim-w + tr-w
          xef.nsh-len = xef.trim-l + tr-l.

         RUN round-dim (INPUT-OUTPUT xef.nsh-wid).

         RUN round-dim (INPUT-OUTPUT xef.nsh-len).

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

      IF AVAIL mach AND mach.p-type EQ "R" THEN
      DO i = 1 TO 20:
        IF mach.max-pan-ss[i] / 1000 LT xef.nsh-len THEN LEAVE.
        ELSE xef.gsh-len = mach.max-pan-ss[i] / 1000.
      END.

      /* xef.brd-dscr = input xef.brd-dscr. ???*/
      if xef.adh-code ne "" then xef.adh-sqin = xef.gsh-len * xef.gsh-wid.

RETURN.

PROCEDURE round-dim:
  DEF INPUT-OUTPUT PARAM io-dim AS DEC NO-UNDO.

  DEF VAR li AS INT NO-UNDO.


  IF celayout-chr NE "None" THEN DO:
    CASE celayout-chr:
      WHEN "1/8" THEN li = 8.
      OTHERWISE li = 10000000.
    END CASE.
    
    io-dim = ROUND(io-dim * li,0) / li.
  END.

END PROCEDURE.

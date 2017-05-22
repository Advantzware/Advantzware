/* ------------------------------------------------------- cec/u2k3.p 3/94 cd */
/* Cost estimating - Page2 - Calculate Layout, Pass #3 (2 in Corr!!)          */
/* -------------------------------------------------------------------------- */

/*
{sys/inc/var.i shared}
{sys/form/s-top.f}
*/
def shared var cocode as cha no-undo.
def shared var locode as cha no-undo.
def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def shared var local-head as char format "x(78)" init
" Label Code  Description             Cost/M      S/B    Length    Width  "
no-undo.
/*
def     shared var gsh-mod as logical.
*/
def var undo-sh as log no-undo.
/*def new shared var formule as de extent 12.
*/
def            var op      as ch extent 12.
def            var nextop  as int.
def            var num     as de extent 12.
def            var curnum  as ch.
def            var kar     as ch format "x".  /* Style formula Kalk variables */
def            var tr-l    as de.
def            var tr-w    as de.  /* Store mach or ctrl trim defaults */
/*
def     shared var head    as ch format "x(78)" extent 20.
def     shared var pass_it as recid.
*/
def var k_frac as dec init 6.25 no-undo.

/*
def shared frame est-2.
{cec/est-2.f &fil=xest &fil2=xef &fil3=xeb &fil4=xeb2}
*/
{cec/foam.i xeb}
{sys/inc/f16to32.i}


undo-sh = no.
blok:
do on error undo with frame est-2:
  /*
  pause 0.
  display
      "                P l e a s e   W a i t !     C A L C U L A T I N G"
      with frame kalk row lorow overlay no-box no-labels width 81
      color value("blink-" + col-look).
  */
  find first style
      {sys/ref/styleW.i}
        and style.style eq xeb.style
      no-lock no-error.
      
  if xef.board ne "" then
  find first item
      {sys/look/itemW.i}
        and item.i-no eq xef.board
      no-lock no-error.
      
  find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.
  
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq xef.m-code
      no-lock no-error.
  if avail mach then
    assign
     tr-l = mach.min-triml * 2
     tr-w = mach.min-trimw * 2.
  else
    assign
     tr-l = ce-ctrl.ls-triml * 2
     tr-w = ce-ctrl.ls-trimw * 2.

/*  if xeb.num-wid entered or xeb.num-len entered then ysk ??? */ do:
  
    if not xef.lsh-lock then xef.die-in = xef.die-in / xeb.num-up.
    xeb.num-up = xeb.num-wid * xeb.num-len.
    if not xef.lsh-lock then xef.die-in = xef.die-in * xeb.num-up.
    if xef.xgrain eq "B" then
      assign
       num[2] = xeb.t-len * xeb.num-wid
       num[1] = xeb.t-wid * xeb.num-len.
      
    else
      assign
       num[1] = xeb.t-len * xeb.num-wid
       num[2] = xeb.t-wid * xeb.num-len.
    
    if xef.lam-dscr eq "R" or
       (xef.lam-dscr ne "R" and xef.xgrain eq "S")
    then do:
       assign 
        xef.trim-w  = num[1]
        xef.trim-l  = num[2].
        
       if xef.lsh-wid lt xef.nsh-wid then xef.lsh-wid = xef.nsh-wid.
       if xef.lsh-len lt xef.nsh-len then xef.lsh-len = xef.nsh-len.
    end.
    
    else do:
      assign 
       xef.nsh-wid = num[2] +
                     if avail mach and mach.dept[1] eq "RC" then 0 else tr-l
       xef.nsh-len = num[1] +
                     if avail mach and mach.dept[1] eq "RC" then 0 else tr-w
       xef.trim-w  = num[2]
       xef.trim-l  = num[1].
       
      if xef.lsh-wid lt xef.nsh-len then xef.lsh-wid = xef.nsh-len.
      if xef.lsh-len lt xef.nsh-wid then xef.lsh-len = xef.nsh-wid.
    end.
    
    if avail mach                             and
       (not avail item or item.i-code eq "E") then do:
      assign
       xef.gsh-wid = (trunc(mach.max-len / xef.nsh-wid,0) * xef.nsh-wid) +
                     if avail mach and mach.dept[1] eq "RC" then
                       (2 * mach.min-trimw) else 0
       xef.gsh-len = (trunc(mach.max-wid / xef.nsh-len,0) * xef.nsh-len) +
                     if avail mach and mach.dept[1] eq "RC" then
                       (2 * mach.min-triml) else 0.

      display {sys/inc/k16.i xef.gsh-wid}
              {sys/inc/k16.i xef.gsh-len}.
    end.

    if xef.n-cuts eq (xef.n-out - 1) + (xef.n-out-l - 1) + (xef.n-out-d - 1)
    then xef.n-cuts = 0.
      
    assign
     xef.n-out   = int(trunc((xef.gsh-wid / xef.nsh-wid),0))
     xef.n-out-l = int(trunc((xef.gsh-len / xef.nsh-len),0))
     xef.n-out-d = int(trunc((xef.gsh-dep / xef.nsh-dep),0)).
       
    if xef.n-cuts eq 0 then
      xef.n-cuts = (xef.n-out - 1) + (xef.n-out-l - 1) + (xef.n-out-d - 1).
  end. /* not gsh-mod */

/*  if xef.n-out entered or xef.n-out-l entered or xef.n-out-d entered then */ 
   do:
    assign
     xef.gsh-wid = (xef.nsh-wid * xef.n-out) +
                   if avail mach and mach.dept[1] eq "RC" then tr-l else 0
     xef.gsh-len = (xef.nsh-len * xef.n-out-l) +
                   if avail mach and mach.dept[1] eq "RC" then tr-w else 0
     xef.gsh-dep = (xef.nsh-dep * xef.n-out-d) +
                   if avail mach and mach.dept[1] eq "RC" then tr-d else 0
     xef.n-cuts  = (xef.n-out - 1) + (xef.n-out-l - 1) + (xef.n-out-d - 1).
       
    display {sys/inc/k16.i xef.gsh-wid}
            {sys/inc/k16.i xef.gsh-len}.
              
    if v-foam then display {sys/inc/k16.i xef.gsh-dep}.
  end.

  if xef.nsh-wid gt xef.gsh-wid then do:
    bell. message "Net Sheet Width is LARGER than Gross Sheet Width!".
    hide message. undo-sh = true. return.
  end.
  
  if xef.nsh-len gt xef.gsh-len then do:
    bell. message "Net Sheet Length is LARGER than Gross Sheet Length!".
    hide message. undo-sh = true. return.
  end.
  
  if xef.nsh-dep gt xef.gsh-dep then do:
    bell. message "Net Sheet Depth is LARGER than Gross Sheet Depth!".
    hide message. undo-sh = true. return.
  end.
/* not here     
  display {sys/inc/k16.i xef.gsh-len}
          {sys/inc/k16.i xef.gsh-wid}
          {sys/inc/k16.i xef.lsh-len}
          {sys/inc/k16.i xef.lsh-wid}
          {sys/inc/k16.i xef.nsh-len}
          {sys/inc/k16.i xef.nsh-wid}
          {sys/inc/k16.i xef.trim-w}
          {sys/inc/k16.i xef.trim-l}
          xeb.num-up  xeb.num-l xeb.num-w
          xef.n-cut.

  {sys/inc/k16b.i xef.gsh-len}
  {sys/inc/k16b.i xef.gsh-wid}
  {sys/inc/k16b.i xef.nsh-wid}
  {sys/inc/k16b.i xef.nsh-len}
  
  if v-foam then do:
    display {sys/inc/k16.i xef.gsh-dep}
            {sys/inc/k16.i xef.nsh-dep}
            {sys/inc/k16.i xef.trim-d}
            xeb.num-d.

    {sys/inc/k16b.i xef.gsh-dep}
    {sys/inc/k16b.i xef.nsh-dep}
  end.
*/  
end. /* blok */
/*
hide frame kalk no-pause.
*/

/* END ---------------------------------- copr. 1994  Advanced Software, Inc. */

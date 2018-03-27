/* ----------------------------------------------------- cec/foam.p 03/00 JLF */
                                                
{sys/inc/var.i shared}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

def var v-dim as dec extent 4.  /* LWD of Blank */
def var v-gsh as dec extent 4.  /* LWD of Gross Sheet */
def var v-out as int extent 3 init 1.
def var v-trm as dec.
def var v-lwd as int extent 4.
DEF VAR v-max-out AS INT INIT 1 NO-UNDO.

{cec/bestfitc.i SHARED}


FIND FIRST tt-ef NO-ERROR.
FIND FIRST tt-eb NO-ERROR.
IF NOT AVAIL tt-ef OR NOT AVAIL tt-eb THEN LEAVE.

if tt-ef.board ne "" then do:
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq tt-ef.m-code
      use-index m-code no-lock no-error.
  if avail mach then v-trm = max(mach.min-trimw,mach.min-triml).
      
  find first item
      {sys/look/itemW.i}
        and item.i-no eq tt-ef.board
      no-lock no-error.

  if avail item then do:
     assign
      v-gsh[1]    = if item.r-wid gt 0 then item.r-wid else item.s-wid
      v-gsh[2]    = item.s-len
      v-gsh[3]    = item.s-dep
      v-dim[1]    = tt-eb.wid + v-trm
      v-dim[2]    = tt-eb.len + v-trm
      v-dim[3]    = tt-eb.dep + v-trm
      tt-ef.n-out   = 1
      tt-ef.n-out-l = 1
      tt-ef.n-out-d = 1
      tt-ef.gsh-wid = v-gsh[1]
      tt-ef.gsh-len = v-gsh[2]
      tt-ef.gsh-dep = v-gsh[3].
    
     IF item.r-wid EQ 0 THEN
     do i = 1 to 3:
        do j = 1 to 2:
           assign
            v-out[1] = trunc(/*item.s-wid*/ v-gsh[1] / v-dim[1],0)
            v-out[2] = trunc(/*item.s-len*/ v-gsh[2] / v-dim[2],0)
            v-out[3] = trunc(/*item.s-dep*/ v-gsh[3] / v-dim[3],0).
          
           if (v-out[1]    * v-out[2]  * v-out[3])    gt 
               v-max-out
              /*(tt-ef.n-out-l * tt-ef.n-out * tt-ef.n-out-d)*/ then
             assign
              tt-ef.gsh-wid = v-gsh[1]
              tt-ef.gsh-len = v-gsh[2]
              tt-ef.gsh-dep = v-gsh[3]
              v-max-out = v-out[1] * v-out[2] * v-out[3].
              
           assign
            v-gsh[4] = v-gsh[2]
            v-gsh[2] = v-gsh[3]
            v-gsh[3] = v-gsh[4]
            
            /*v-dim[4] = v-dim[2]
            v-dim[2] = v-dim[3]
            v-dim[3] = v-dim[4]*/ .
        end.
       
        assign
         v-gsh[4] = v-gsh[3]
         v-gsh[3] = v-gsh[2]
         v-gsh[2] = v-gsh[1]
         v-gsh[1] = v-gsh[4]
        
         /*v-dim[4] = v-dim[3]
         v-dim[3] = v-dim[2]
         v-dim[2] = v-dim[1]
         v-dim[1] = v-dim[4]*/ .
     end.
  end.

  assign
   tt-eb.num-len = 1
   tt-eb.num-wid = 1
   tt-eb.num-dep = 1
   tt-eb.num-up  = 1
   tt-ef.nsh-len = tt-eb.t-len
   tt-ef.nsh-wid = tt-eb.t-wid
   tt-ef.nsh-dep = tt-eb.t-dep
   tt-ef.trim-l  = tt-ef.nsh-len
   tt-ef.trim-w  = tt-ef.nsh-wid
   tt-ef.trim-d  = tt-ef.nsh-dep
   tt-ef.n-out-l = trunc(tt-ef.gsh-len / tt-ef.nsh-len,0)
   tt-ef.n-out   = trunc(tt-ef.gsh-wid / tt-ef.nsh-wid,0)
   tt-ef.n-out-d = trunc(tt-ef.gsh-dep / tt-ef.nsh-dep,0).
end.

/* end ---------------------------------- copr. 2000  advanced software, inc. */

/* --------------------------------------------------- sys/ref/convquom.p 9/94*/
/* UOM Conversion from one unit to another for Board Primarily.               */
/* -------------------------------------------------------------------------- */

def SHARED var cocode     as   char  format "x(3)"  no-undo.

def input  parameter v-fr-uom  like job-mat.sc-uom no-undo.
def input  parameter v-to-uom  like job-mat.sc-uom no-undo.
def input  parameter v-basis-w like job-mat.basis-w no-undo.
def input  parameter v-len     like job-mat.len no-undo.
def input  parameter v-wid     like job-mat.wid no-undo.
def input  parameter v-dep     like job-mat.dep no-undo.
def input  parameter v-in-qty  as   dec no-undo.
def output parameter v-out-qty as   DEC no-undo.

if v-in-qty eq 0 then do:
  v-out-qty = 0.
  leave.
end.

if v-len eq 0 then v-len = 12.
    
if v-dep eq 0 then v-dep = 1.

IF ((v-fr-uom EQ "LB" AND v-to-uom EQ "EA") OR
    (v-fr-uom EQ "EA" AND v-to-uom EQ "LB"))    AND
   (v-basis-w EQ 0 OR v-wid EQ 0 OR v-len EQ 0) THEN DO:
  v-out-qty = v-in-qty.
  LEAVE.
END.

{ce/msfcalc.i}

/* Convert from uom to SHEETS */
if v-fr-uom begins "MSH" then
  v-in-qty = v-in-qty * 1000.
  
else
if v-fr-uom begins "MSF" then
  v-in-qty = 1000 * v-in-qty / (if v-corr then (v-len * v-wid * .007)
                                          else (v-len * v-wid / 144)).
                             
else
if v-fr-uom begins "TON" and v-basis-w ne 0 and v-len ne 0 and v-wid ne 0 then
  v-in-qty = 2000 * 1000 * v-in-qty /
             (v-basis-w * (if v-corr then (v-len * v-wid * .007)
                                     else (v-len * v-wid / 144))).
                                           
else
if v-fr-uom begins "LB" and v-basis-w ne 0 and v-len ne 0 and v-wid ne 0 then
  v-in-qty = 1000 * v-in-qty /
             (v-basis-w * (if v-corr then (v-len * v-wid * .007)
                                     else (v-len * v-wid / 144))).
                                           
else
if v-fr-uom begins "SF" then
  v-in-qty = v-in-qty / (if v-corr then (v-len * v-wid * .007)
                                   else (v-len * v-wid / 144)).
                                         
else
if v-fr-uom begins "MLF" then
  v-in-qty = (1000 * v-in-qty) / (v-len / 12).
  
else
if v-fr-uom begins "MLI" then
  v-in-qty = (1000 * v-in-qty) / v-len.
  
else
if v-fr-uom begins "LF" THEN 
    v-in-qty = v-in-qty / (v-len / 12).
else
if v-fr-uom begins "BF" then
  v-in-qty = v-in-qty / ((v-len * v-wid * v-dep) / 144).

else
if v-fr-uom begins "ROLL" THEN 
  assign
   v-in-qty = v-in-qty * v-len
   v-len    = 12.
else
fromuom:
repeat:
  /* put qty into an EA uom */
  find first uom
      where uom.uom  eq v-fr-uom
        and uom.mult ne 0
      no-lock no-error.
  if avail uom then do:
    v-in-qty = (if v-in-qty = 0 then 1 else v-in-qty * uom.mult).
    
    if uom.other ne "" and uom.other ne uom.uom then do:
      v-fr-uom = uom.other.
      next fromuom.
    end.
  end.
  
  else v-in-qty = (if v-in-qty eq 0 then 1 else v-in-qty).
  
  leave fromuom.
end.

/* Convert SHEETS to uom */
if v-to-uom begins "MSH" then
  v-out-qty = v-in-qty / 1000.
  
else
if v-to-uom begins "MSF" then
  v-out-qty = (if v-corr then (v-len * v-wid * .007)
                         else (v-len * v-wid / 144)) *
              v-in-qty / 1000.
                 
else
if v-to-uom begins "TON" and v-basis-w ne 0 and v-len ne 0 and v-wid ne 0 then
  v-out-qty = (if v-corr then (v-len * v-wid * .007)
                         else (v-len * v-wid / 144)) *
              v-in-qty / 1000 * v-basis-w / 2000.
                 
else
if v-to-uom begins "LB" and v-basis-w ne 0 and v-len ne 0 and v-wid ne 0 then
  v-out-qty = (if v-corr then (v-len * v-wid * .007)
                         else (v-len * v-wid / 144)) *
              v-in-qty / 1000 * v-basis-w.
                 
else
if v-to-uom begins "SF" then
  v-out-qty = (if v-corr then (v-len * v-wid * .007)
                         else (v-len * v-wid / 144)) * v-in-qty.
                            
else
if v-to-uom begins "MLF" then
  v-out-qty = ((v-len / 12) * v-in-qty) / 1000.
  
else
if v-to-uom begins "MLI" then
  v-out-qty = (v-len * v-in-qty) / 1000.
  
else
if v-to-uom begins "LF" THEN 
     v-out-qty = (v-len / 12) * v-in-qty.
 
  
else
if v-to-uom begins "BF" then
  v-out-qty = ((v-len * v-wid * v-dep) / 144) * v-in-qty.
  
else
touom:
repeat:
  find first uom
      where uom.uom  eq v-to-uom
        and uom.mult ne 0
      no-lock no-error.
  if avail uom then do:
    v-in-qty = (if v-in-qty ne 0 then (v-in-qty / uom.mult) else 0).
    if uom.other ne "" and uom.other ne uom.uom then do:
      v-to-uom = uom.other.
      next touom.
    end.
  end.  

  v-out-qty = (if v-in-qty ne 0 then v-in-qty else 1).
  leave touom.
end.
IF v-fr-uom = "LB" THEN

    

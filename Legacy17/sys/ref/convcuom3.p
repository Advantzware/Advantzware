/* --------------------------------------------------- sys/ref/convcuom3.p 9/94*/
/* UOM Conversion from one Unit to Another for Board Primarily.               */
/* -------------------------------------------------------------------------- */

DEF INPUT  PARAMETER cocode AS CHAR NO-UNDO.
def input  parameter v-fr-uom  like job-mat.sc-uom  no-undo.
def input  parameter v-to-uom  like job-mat.sc-uom  no-undo.
def input  parameter v-basis-w like job-mat.basis-w no-undo.
def input  parameter v-len     like job-mat.len     no-undo.
def input  parameter v-wid     like job-mat.wid     no-undo.
def input  parameter v-dep     like job-mat.wid     no-undo.
def input  parameter v-in-cst  as   dec decimals 10 no-undo.
def output parameter v-out-cst as   dec decimals 10 no-undo.


if v-in-cst eq 0 then do:
  v-out-cst = 0.
  leave.
end.

if v-len eq 0 then v-len = 12.

if v-dep eq 0 then v-dep = 1.

IF ((v-fr-uom EQ "LB" AND v-to-uom EQ "EA") OR
    (v-fr-uom EQ "EA" AND v-to-uom EQ "LB"))    AND
   (v-basis-w EQ 0 OR v-wid EQ 0 OR v-len EQ 0) THEN DO:
  v-out-cst = v-in-cst.
  LEAVE.
END.

{ce/msfcalc.i}

if v-fr-uom begins "MSH" or v-fr-uom eq "M" then
  v-in-cst = v-in-cst / 1000.
  
else
if v-fr-uom begins "MSF" then
  v-in-cst = (if v-corr then (v-len * v-wid * .007)
                        else (v-len * v-wid / 144)) * v-in-cst / 1000.
                
else
if v-fr-uom begins "TON" and v-wid ne 0 and v-len ne 0 and v-basis-w ne 0 then
  v-in-cst = (if v-corr then (v-len * v-wid * .007)
                        else (v-len * v-wid / 144)) *
             v-in-cst / 1000 * v-basis-w / 2000.

else
if v-fr-uom begins "LB" and v-wid ne 0 and v-len ne 0 and v-basis-w ne 0 then
  v-in-cst = (if v-corr then (v-len * v-wid * .007)
                        else (v-len * v-wid / 144)) *
             v-in-cst / 1000 * v-basis-w.
                
else
if v-fr-uom begins "SF" then
  v-in-cst = (if v-corr then (v-len * v-wid * .007)
                        else (v-len * v-wid / 144)) * v-in-cst.
                           
else
if v-fr-uom begins "MLF" then
  v-in-cst = ((v-len / 12) * v-in-cst) / 1000.
  
else
if v-fr-uom begins "MLI" then
  v-in-cst = (v-len  * v-in-cst) / 1000.
  
else
if v-fr-uom begins "LF" then
  v-in-cst = (v-len / 12) * v-in-cst.
  
else
if v-fr-uom begins "BF" then
  v-in-cst = ((v-len * v-wid * v-dep) / 144) * v-in-cst.

else
if v-fr-uom begins "ROLL" then
  assign
   v-in-cst = v-in-cst / v-len
   v-len    = 12.
  
else
fromuom:
repeat:
  /* put cost into an EA uom */
  find first uom
      where uom.uom  eq v-fr-uom
        and uom.mult ne 0
      no-lock no-error.
  if avail uom then do:
    v-in-cst = (if v-in-cst eq 0 then 1 else v-in-cst / uom.mult).
     
    if uom.other ne "" and uom.other ne uom.uom then do:
      v-fr-uom = uom.other.
      next fromuom.
    end.
  end.
    
  else v-in-cst = (if v-in-cst eq 0 then 1 else v-in-cst).
    
  leave fromuom.
end.

if v-to-uom begins "MSH" or v-to-uom eq "M" then
  v-out-cst = v-in-cst * 1000.
  
else
if v-to-uom begins "MSF" then
  v-out-cst = (1000 * v-in-cst) /
              (if v-corr then (v-len * v-wid * .007)
                         else (v-len * v-wid / 144)).
                          
else
if v-to-uom begins "TON" and v-wid ne 0 and v-len ne 0 and v-basis-w ne 0 then
  v-out-cst = (2000 * 1000 * v-in-cst) /
              (v-basis-w * (if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144))).
                            
else
if v-to-uom begins "LB" and v-wid ne 0 and v-len ne 0 and v-basis-w ne 0 then
  v-out-cst = (1000 * v-in-cst) /
              (v-basis-w * (if v-corr then (v-len * v-wid * .007)
                                      else (v-len * v-wid / 144))).
                            
else
if v-to-uom begins "SF" then
  v-out-cst = v-in-cst / (if v-corr then (v-len * v-wid * .007)
                                    else (v-len * v-wid / 144)).
                            
else
if v-to-uom begins "MLF" then
  v-out-cst = (1000 * v-in-cst) / (v-len / 12).
  
else
if v-to-uom begins "MLI" then
  v-out-cst = (1000 * v-in-cst) / v-len.
  
else
if v-to-uom begins "LF" then
  v-out-cst = v-in-cst / (v-len / 12).
  
else
if v-to-uom begins "BF" then
  v-out-cst = v-in-cst / ((v-len * v-wid * v-dep) / 144).
  
else
touom:
repeat:
  find first uom
      where uom.uom  eq v-to-uom
        and uom.mult ne 0
      no-lock no-error.
  if avail uom then do:
    v-in-cst = (if v-in-cst ne 0 then (v-in-cst * uom.mult) else 0).
      
    if uom.other ne "" and uom.other ne uom.uom then do:
      v-to-uom = uom.other.
      next touom.
    end.
  end.
    
  v-out-cst = v-in-cst.
    
  leave touom.
end.

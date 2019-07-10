/* --------------------------------------------------- po/po-ordls.p 5/01 JLF */
/* Create Scoring allowance for a PO Line Item                                */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def buffer b-ref1 for reftable.
def buffer b-ref2 for reftable.

def var k_frac as dec init 6.25 no-undo.
{sys/inc/f16to32.i}
{cec/descalc.i NEW}

def var v-xg-flag as log.
def var v         as int.
def var v-char    as char.

find po-ordl where recid(po-ordl) eq v-recid no-lock no-error.
if avail po-ordl then do:
  {po/po-ordls.i} 
  {po/poordls2.w}
end.
else do:
  find job-mat where recid(job-mat) eq v-recid no-lock no-error.  
  {po/poordls1.w}
  if avail b-ref1 then delete b-ref1.
  if avail b-ref2 then delete b-ref2.
end.

if not avail b-ref1 or not avail b-ref2 then do:
  if not avail b-ref1 then create b-ref1.
  assign
   b-ref1.reftable = "POLSCORE"
   b-ref1.company  = cocode
   b-ref1.loc      = "1".
   
  if not avail b-ref2 then create b-ref2.
  assign
   b-ref2.reftable = "POLSCORE"
   b-ref2.company  = cocode
   b-ref2.loc      = "2".
   
  if avail po-ordl then
    assign
     b-ref1.code     = string(po-ordl.po-no,"9999999999")
     b-ref1.code2    = string(po-ordl.line, "9999999999")
     b-ref2.code     = string(po-ordl.po-no,"9999999999")
     b-ref2.code2    = string(po-ordl.line, "9999999999").
  else
    assign
     b-ref1.code     = string(recid(job-mat),"9999999999")
     b-ref1.code2    = ""
     b-ref2.code     = string(recid(job-mat),"9999999999")
     b-ref2.code2    = "".
     
  {po/poordls3.w}

  IF AVAIL eb THEN DO:
    IF NOT CAN-FIND(FIRST style
                    WHERE style.company EQ eb.company
                      AND style.style   EQ eb.style
                      AND style.type    EQ "D") THEN DO:

      v-xg-flag = (ef.xgrain eq "S" AND ef.est-type GE 5) OR
                   ef.xgrain eq "B".

      ASSIGN
       b-ref1.val  = 0
       b-ref1.dscr = ""
       b-ref2.val  = 0
       b-ref2.dscr = "".

      IF v-xg-flag THEN DO:
        DO x = 1 TO EXTENT(eb.k-len-scr-type2):
          IF x LE 12 THEN
            b-ref1.dscr = b-ref1.dscr + STRING(eb.k-len-scr-type2[x],"X").
          ELSE
          IF x LE 20 THEN
            b-ref2.dscr = b-ref2.dscr + STRING(eb.k-len-scr-type2[x],"X").
        END.
        DO x = 1 TO 12:
          IF x LE 12 THEN
            b-ref1.val[x]      = {sys/inc/k16v.i eb.k-len-array2[x]}.
          ELSE
          IF x LE 20 THEN
            b-ref2.val[x - 12] = {sys/inc/k16v.i eb.k-len-array2[x]}.
        END.
      END.
    
      ELSE DO:
        DO x = 1 TO EXTENT(eb.k-len-scr-type2):
          IF x LE 12 THEN
            b-ref1.dscr = b-ref1.dscr + STRING(eb.k-wid-scr-type2[x],"X").
          ELSE
          IF x LE 20 THEN
            b-ref2.dscr = b-ref2.dscr + STRING(eb.k-wid-scr-type2[x],"X").
        END.
        DO x = 1 TO 12:
          IF x LE 12 THEN
            b-ref1.val[x]      = {sys/inc/k16v.i eb.k-wid-array2[x]}.
          ELSE
          IF x LE 20 THEN
            b-ref2.val[x - 12] = {sys/inc/k16v.i eb.k-wid-array2[x]}.
        END.
      END.
    END.
  END.
      
  ELSE
  if avail box-design-hdr then do:
    run ce/descalc.p (input recid(est), input recid(ef)).

    if v-lscore-c begins "No Design" then leave.

    if v-xg-flag then do:
      v = 1.
    
      do x = 1 to length(v-lscore-c):
        if (substr(v-lscore-c,x,1) eq " " or
            x eq length(v-lscore-c))     and
           v-char ne ""                  and
           v le 26                       then do:
           
          if x eq length(v-lscore-c) then
            v-char = v-char + substr(v-lscore-c,x,1).
           
          if v ge 1 then do:
            if v le 12 then
              b-ref1.val[v]      = dec(trim(v-char)).
            else
            if v le 20 then
              b-ref2.val[v - 12] = dec(trim(v-char)).
              
            v-char = "".
          end.
          
          v = v + 1.
        end.
        
        else v-char = v-char + substr(v-lscore-c,x,1).
      end.
    end.
  
    else do:
      v = 0.
    
      for each box-design-line of box-design-hdr no-lock,
          first w-box-design-line
          where w-box-design-line.line-no eq box-design-line.line-no:
          
        v = v + 1.
        if v le 12 then
          b-ref1.val[v]      = w-box-design-line.wscore-d.
        else
        if v le 20 then
          b-ref2.val[v - 12] = w-box-design-line.wscore-d.
      end.
    end.
  end.
end.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

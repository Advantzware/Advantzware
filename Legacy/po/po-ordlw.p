/* --------------------------------------------------- po/po-ordlw.p 5/01 JLF */
/* Create Scoring allowance for a PO Line Item                                */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.
DEF INPUT PARAMETER v-type AS CHAR.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def var k_frac as dec init 6.25 no-undo.
{sys/inc/f16to32.i}
{cec/descalc.i NEW}

def var v-xg-flag as LOG NO-UNDO.
def var v         as INT NO-UNDO.
def var v-char    as CHAR NO-UNDO.
DEF VAR v-farm-out-scores AS LOG NO-UNDO INIT YES.
DEF VAR v-est-qty-rowid AS ROWID NO-UNDO.

/* Indicates to always create scores records for FG items, not just board */
find first sys-ctrl where sys-ctrl.company eq cocode
                      and sys-ctrl.name    eq "POFarmOutScores" no-lock no-error.
IF AVAIL sys-ctrl AND sys-ctrl.log-fld THEN v-farm-out-scores  = YES.
ELSE v-farm-out-scores = NO.

find po-ordl where recid(po-ordl) eq v-recid no-lock no-error.
if avail po-ordl then do:
  {po/poordls2W.i}
end.
else do:
  find job-mat where recid(job-mat) eq v-recid no-lock no-error.
end.

IF avail(po-ordl) and  po-ordl.item-type THEN
  FIND ITEM WHERE item.company = po-ordl.company
              AND ITEM.i-no = po-ordl.i-no
            NO-LOCK NO-ERROR.
if avail po-ordl 
      AND NOT (AVAIL(ITEM) AND (LOOKUP(ITEM.mat-TYPE, "B,P")= 0)) then do:
  {po/poordls3W.i}
  IF v-farm-out-scores THEN DO:
    FIND itemfg WHERE itemfg.company = cocode 
                  AND itemfg.i-no = po-ordl.i-no NO-LOCK NO-ERROR.
    IF AVAIL itemfg AND itemfg.est-no GT "" THEN DO:
      v-est-qty-rowid = ?.
      FOR EACH est-qty WHERE est-qty.company = cocode
                         AND est-qty.est-no = itemfg.est-no
                       NO-LOCK,
        EACH ef WHERE ef.company = est-qty.company 
                  AND ef.est-no  = est-qty.est-no 
                  AND ef.eqty    = est-qty.eqty NO-LOCK,
        EACH eb WHERE eb.company = ef.company
                  AND eb.est-no  = ef.est-no 
                  AND eb.stock-no = po-ordl.i-no
                  AND eb.eqty    = ef.eqty NO-LOCK.
            v-est-qty-rowid = ROWID(est-qty).

      END.
      IF v-est-qty-rowid NE ? THEN DO:
          FIND est-qty WHERE ROWID(est-qty) = v-est-qty-rowid
                       NO-LOCK NO-ERROR.
          FIND FIRST ef WHERE ef.company = est-qty.company 
                          AND ef.est-no  = est-qty.est-no 
                          AND ef.eqty    = est-qty.eqty 
                        NO-LOCK NO-ERROR.
          FIND FIRST eb WHERE eb.company = ef.company
                          AND eb.est-no  = ef.est-no 
                          AND eb.stock-no = po-ordl.i-no
                          AND eb.eqty    = ef.eqty 
                        NO-LOCK NO-ERROR.
      END.
    END.
  END.

  IF AVAIL eb THEN DO:

    IF NOT CAN-FIND(FIRST style
                    WHERE style.company EQ eb.company
                      AND style.style   EQ eb.style
                      AND LOOKUP(style.type,"D,P,R") GT 0) THEN DO:

      ASSIGN
       v-xg-flag = (ef.xgrain eq "S" AND ef.est-type GE 5) OR
                   ef.xgrain eq "B"
       .

      IF v-xg-flag OR v-type = "LENGTH" THEN DO:
        DO x = 1 TO EXTENT(eb.k-len-scr-type2):
          ASSIGN po-ordl.scoreType[x] = STRING(eb.k-len-scr-type2[x],"X").
        END.
        DO x = 1 TO EXTENT(eb.k-len-array2):
          ASSIGN po-ordl.scorePanels[x]      = {sys/inc/k16v.i eb.k-len-array2[x]}.
        END.
      END.
    
      ELSE DO:
        DO x = 1 TO EXTENT(eb.k-wid-scr-type2):
          ASSIGN po-ordl.scoreType[x] = STRING(eb.k-wid-scr-type2[x],"X").
        END.
        DO x = 1 TO EXTENT(eb.k-wid-array2):
          ASSIGN po-ordl.scorePanels[x]      = {sys/inc/k16v.i eb.k-wid-array2[x]}.
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
              po-ordl.scorePanels[v]      = dec(trim(v-char)).
            else
            if v le 20 then
              po-ordl.scorePanels[v - 12] = dec(trim(v-char)).
              
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
          po-ordl.scorePanels[v]      = w-box-design-line.wscore-d.
        else
        if v le 20 then
          po-ordl.scorePanels[v - 12] = w-box-design-line.wscore-d.
      end.
    end.
  end.
end.
  
/* end ---------------------------------- copr. 2001  advanced software, inc. */

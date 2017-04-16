
DEF PARAM BUFFER io-po-ordl FOR po-ordl.

DEF INPUT PARAM ip-uom LIKE io-po-ordl.cons-uom NO-UNDO.
DEF INPUT-OUTPUT PARAM io-cost AS DEC DECIMALS 10 NO-UNDO.

DEF BUFFER b-item FOR item.
DEF BUFFER b-po-ordl FOR po-ordl.

DEF VAR ld-cst AS DEC NO-UNDO.
DEF VAR ld-qty AS DEC NO-UNDO.
DEF VAR v-total-po-wid AS DEC NO-UNDO.
DEF VAR v-rm-issue-cost AS DEC NO-UNDO.

def var v-len like po-ordl.s-len no-undo.
def var v-wid like po-ordl.s-len no-undo.
def var v-dep like po-ordl.s-len no-undo. 
def var v-bwt like po-ordl.s-len no-undo.
DEF VAR v-num-items AS INT NO-UNDO.
DEF VAR v-single-item-div AS INT NO-UNDO.

FUNCTION Floor RETURNS INTEGER (INPUT ipdValue AS DECIMAL):
    IF (ipdValue GE 0) OR (TRUNCATE(ipdValue,0) = ipdValue) THEN
        RETURN INTEGER (TRUNCATE(ipdValue,0)).
    ELSE
        RETURN integer(TRUNCATE(ipdValue,0) - 1).
END.

RELEASE po-ord.
IF AVAIL io-po-ordl THEN
FIND FIRST po-ord WHERE
     po-ord.company EQ io-po-ordl.company AND
     po-ord.po-no   EQ io-po-ordl.po-no
     NO-LOCK NO-ERROR.

IF AVAIL po-ord AND po-ord.type EQ "S" THEN DO:
  FIND FIRST item NO-LOCK
      WHERE item.company EQ po-ord.company
        AND item.i-no    EQ io-po-ordl.i-no
      NO-ERROR.

  ld-qty = io-po-ordl.cons-qty.
 
  IF io-po-ordl.cons-uom NE ip-uom THEN
    RUN custom/convquom.p (io-po-ordl.company,
                           io-po-ordl.cons-uom,
                           ip-uom,
                           item.basis-w,
                           io-po-ordl.s-len,
                           io-po-ordl.s-wid,
                           item.s-dep,
                           io-po-ordl.cons-qty,
                           OUTPUT ld-qty).

  io-cost = io-po-ordl.t-cost / ld-qty.

  FOR EACH b-po-ordl fields(company i-no) WHERE
      b-po-ordl.company EQ io-po-ordl.company AND
      b-po-ordl.po-no EQ io-po-ordl.po-no
      NO-LOCK,
      FIRST b-item FIELDS(s-wid) WHERE
            b-item.company EQ b-po-ordl.company AND
            b-item.i-no    EQ b-po-ordl.i-no AND
            b-item.mat-type EQ "B"
            NO-LOCK:
      ASSIGN
         v-total-po-wid = v-total-po-wid + b-item.s-wid
         v-num-items = v-num-items + 1.
  END.

  FOR EACH rm-rcpth NO-LOCK
      WHERE rm-rcpth.company   EQ po-ord.company
        AND rm-rcpth.vend-no   EQ po-ord.vend-no
        AND rm-rcpth.po-no     EQ TRIM(STRING(po-ord.po-no,">>>>>>>>>>"))
        AND rm-rcpth.rita-code EQ "I"
      USE-INDEX vend,
      EACH rm-rdtlh NO-LOCK
      WHERE rm-rdtlh.r-no             EQ rm-rcpth.r-no
        AND rm-rdtlh.rita-code        EQ rm-rcpth.rita-code,
      FIRST b-item NO-LOCK
      WHERE b-item.company EQ rm-rcpth.company
        AND b-item.i-no    EQ rm-rcpth.i-no
      BY rm-rcpth.trans-date
      BY ROWID(rm-rdtlh):

    v-rm-issue-cost = rm-rdtlh.cost.
    
    IF AVAIL item THEN
    DO:
       ASSIGN
          v-len = io-po-ordl.s-len
          v-wid = io-po-ordl.s-wid
          v-bwt = 0.
       
       if (v-len eq 0 or v-wid eq 0 or v-bwt eq 0) then do:
          find first job
              where job.company eq ITEM.company
                and job.job-no  eq b-po-ordl.job-no
                and job.job-no2 eq b-po-ordl.job-no2
              no-lock no-error.
       
          if avail job then do :
            for each job-mat
                where job-mat.company eq ITEM.company
                  and job-mat.job     eq job.job
                  and job-mat.job-no  eq job.job-no
                  and job-mat.job-no2 eq job.job-no2
                  and job-mat.i-no    eq b-po-ordl.i-no
                no-lock
                by job-mat.frm desc:
                     
              if job-mat.frm eq b-po-ordl.s-num then leave.
            end.
                 
            if avail job-mat then
              assign
               v-len = if v-len eq 0 then job-mat.len     else v-len
               v-wid = if v-wid eq 0 then job-mat.wid     else v-wid
               v-bwt = if v-bwt eq 0 then job-mat.basis-w else v-bwt.
          end.
       
          if v-len eq 0 then v-len = item.s-len.
       
          if v-wid eq 0 then
            v-wid = if item.r-wid ne 0 then item.r-wid else item.s-wid.
       
          if v-bwt eq 0 then v-bwt = item.basis-w.
       end.

       IF rm-rcpth.pur-uom NE "LF" and avail item THEN
          RUN custom/convcuom.p (item.company,
                                 rm-rcpth.pur-uom, "LF",                    
                                 v-bwt, v-len, v-wid, v-dep,
                                 v-rm-issue-cost, OUTPUT v-rm-issue-cost).

       /*result is in EA*/
       ld-cst = v-rm-issue-cost * (item.s-len / 12.0) * (item.s-wid / v-total-po-wid).
       
       IF v-num-items EQ 1 THEN
          v-single-item-div = FLOOR(b-ITEM.r-wid / ITEM.s-wid).
       ELSE
          v-single-item-div = 0.

       IF "EA" NE ip-uom AND AVAIL ITEM THEN
          RUN custom/convcuom.p (item.company,
                                 "EA", ip-uom,                    
                                 v-bwt, v-len, v-wid, v-dep,
                                 ld-cst, OUTPUT ld-cst).
    END.
  END.

  IF ld-cst GT 0 THEN
  DO:
     IF v-num-items EQ 1 AND v-single-item-div NE 0 THEN
        ld-cst = ld-cst / v-single-item-div.

     io-cost = io-cost + ld-cst.
  END.
END.

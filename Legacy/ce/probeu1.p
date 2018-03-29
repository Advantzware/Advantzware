/* --------------------------------------------------- ce/probeu1.p 03/97 JLF */
/* 'What if' UPDATE                                                           */
/* -------------------------------------------------------------------------- */

def input param v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF BUFFER b-probe FOR probe.
DEF BUFFER b-probemk FOR reftable.
DEF BUFFER probe-ref FOR reftable.

def shared var tmp-dir as cha no-undo.
DEF SHARED VAR lv-cebrowse-dir AS CHAR NO-UNDO.

{ce/print4.i shared shared}
{ce/print42.i shared}

def var qm as de NO-UNDO.
def var v as int NO-UNDO.
def var v-comm like tt-tot NO-UNDO.
def var v-yld as dec NO-UNDO.
def var v-skip-pct as log NO-UNDO.
def var v-prf-s as dec NO-UNDO.
def var v-pct-s as dec NO-UNDO.
DEF VAR v-probe-fmt AS CHAR NO-UNDO.

DEF TEMP-TABLE w-probe NO-UNDO FIELD w-rowid AS ROWID.

{sys/inc/cerun.i F}
{ce/msfcalc.i}
{cec/combasis.i}

FIND probe WHERE RECID(probe) EQ v-recid NO-LOCK NO-ERROR.

/*IF probe.comm NE 0 THEN*/ v-com = probe.comm.

v = 0.

FOR EACH mclean:
  DELETE mclean.
END.

FOR EACH b-probe
    WHERE b-probe.company    EQ probe.company
      AND b-probe.est-no     EQ probe.est-no
      AND b-probe.probe-date EQ probe.probe-date
    NO-LOCK
    BY b-probe.est-qty
    BY b-probe.freight
    BY b-probe.rec_key:

  IF v GE 5 THEN
    IF CAN-FIND(FIRST w-probe WHERE w-rowid EQ ROWID(probe)) THEN LEAVE.
    ELSE DO:
      EMPTY TEMP-TABLE w-probe.
      v = 0.
    END.

  CREATE w-probe.
  ASSIGN
   w-rowid = ROWID(b-probe)
   v       = v + 1.
END.

ASSIGN
 v    = 0
 qtty = 0.

FOR EACH w-probe,
    FIRST b-probe WHERE ROWID(b-probe) EQ w-rowid NO-LOCK
    BY b-probe.est-qty
    BY b-probe.freight
    BY b-probe.rec_key:

  v = v + 1.

  ASSIGN
   qtty[v] = b-probe.est-qty
   rels[v] = b-probe.freight.

  FOR EACH est-summ
      WHERE est-summ.company EQ b-probe.company
        AND est-summ.est-no  EQ b-probe.est-no
        AND est-summ.e-num   EQ b-probe.line
        AND SUBSTR(est-summ.summ-tot,31) NE ""
      USE-INDEX est-qty NO-LOCK:

    v-form-no = INT(SUBSTR(est-summ.summ-tot,21,10)).

    {ce/pr4-mcln.i SUBSTR(est-summ.summ-tot,31) v est-summ.per-m}
    mclean.rec-type = SUBSTR(est-summ.summ-tot,01,20).
  END.
END.

ASSIGN
 qty   = probe.est-qty
 qm    = qty / 1000
 v-pct = IF ce-ctrl.sell-by NE "N" THEN probe.gross-profit
				                   ELSE probe.net-profit
 v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

if vmclean2 then
for each probeit
    where probeit.company eq probe.company
      and probeit.est-no  eq probe.est-no
      and probeit.line    eq probe.line
    no-lock:
   find first xeb
       where xeb.company eq xest.company
         and xeb.est-no  eq xest.est-no
         and xeb.form-no ne 0
         and xeb.part-no eq probeit.part-no
       no-lock no-error.
   FIND FIRST eb WHERE ROWID(eb) EQ ROWID(xeb) NO-LOCK NO-ERROR.

   v-form-no = xeb.form-no.
   
   RUN get-dir-proc(INPUT trim(xest.est-no) + "-" + string(v-form-no,"99")
                          + ".v" + string(probe.line,v-probe-fmt),
                   OUTPUT tmp-dir).

   os-copy value(tmp-dir + trim(xest.est-no) + "-"  +
				     string(v-form-no,"99")     + ".v" +
				     string(probe.line,v-probe-fmt))
		     value(lv-cebrowse-dir + trim(xest.est-no) + "-"  +
				     string(v-form-no,"99")     + ".s" +
				     string(probe.line,v-probe-fmt)).

   output to value(lv-cebrowse-dir + trim(xest.est-no) + "-"  +
			     string(v-form-no,"99")     + ".s" +
			     string(probe.line,v-probe-fmt)) append.

   ASSIGN
    v-prf-s = probeit.sell-price - probeit.fact-cost
    v-pct-s = v-prf-s / probeit.fact-cost * 100.

   if not vmclean then do:
      {ce/probepr1.i 50}
   end.

   else do:
      {ce/pr4-mcl.i 1}
   end.
   
   output close.

   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

   RUN get-dir-proc(INPUT trim(xest.est-no) + "-" + string(v-form-no,"99")
                          + ".a" + string(probe.line,v-probe-fmt),
                    OUTPUT tmp-dir).

   if opsys eq "unix" then
      unix silent cat value(tmp-dir + trim(xest.est-no) + "-"  +
				      string(v-form-no,"99")     + ".a" +
				      string(probe.line,v-probe-fmt))            >>
		      value(lv-cebrowse-dir + trim(xest.est-no) + "-"  +
				      string(v-form-no,"99")     + ".s" +
				      string(probe.line,v-probe-fmt)).
   else /* if opsys eq "MSDOS" then */
      dos silent type value(tmp-dir + trim(xest.est-no) + "-"  +
				      string(v-form-no,"99")     + ".a" +
				      string(probe.line,v-probe-fmt))            >>
		      value(lv-cebrowse-dir + trim(xest.est-no) + "-"  +
				      string(v-form-no,"99")     + ".s" +
				      string(probe.line,v-probe-fmt)).    

end.

else do:
   
   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

   RUN get-dir-proc(INPUT trim(xest.est-no) + "-01.v"
						  + string(probe.line,v-probe-fmt),
                    OUTPUT tmp-dir).

   if xest.est-type eq 2 then do:
      os-copy value(tmp-dir + trim(xest.est-no) + "-01.v"
                    + string(probe.line,v-probe-fmt))
              value(lv-cebrowse-dir + trim(xest.est-no) + "-01.s"
                    + string(probe.line,v-probe-fmt)).

      output to value(lv-cebrowse-dir + trim(xest.est-no) + "-01.s"
					      + string(probe.line,v-probe-fmt)) append.
   end.

   else do:

      RUN get-dir-proc(INPUT trim(xest.est-no) + ".v"
						     + string(probe.line,v-probe-fmt),
                       OUTPUT tmp-dir).

      os-copy value(tmp-dir + trim(xest.est-no) + ".v"
                    + string(probe.line,v-probe-fmt))
              value(lv-cebrowse-dir + trim(xest.est-no) + ".s"
                    + string(probe.line,v-probe-fmt)).

       output to value(lv-cebrowse-dir + trim(xest.est-no) + ".s"
					      + string(probe.line,v-probe-fmt)) append.
   end.

   ASSIGN
    v-prf-s = probe.sell-price - probe.fact-cost
    v-pct-s = v-prf-s / probe.fact-cost * 100.
   
   if not vmclean then do:
      if xest.est-type eq 2 then do:
	     {ce/probepr2.i 50}
      end.

      else do:
	     {ce/probepr2.i 48}
      end.
   end.

   else do:
      {ce/pr4-mcl.i 1}
   end.
  
   output close.

   v-probe-fmt = IF probe.LINE LT 100 THEN "99" ELSE "999".

   if xest.est-type eq 2 then do:

      RUN get-dir-proc(INPUT trim(xest.est-no) + "-01.a"
						   + string(probe.line,v-probe-fmt),
                       OUTPUT tmp-dir).

      if opsys eq "unix" then
	 unix silent cat value(tmp-dir + trim(xest.est-no) + "-01.a"
						   + string(probe.line,v-probe-fmt)) >>
			 value(lv-cebrowse-dir + trim(xest.est-no) + "-01.s"
						   + string(probe.line,v-probe-fmt)).
      else /* if opsys eq "MSDOS" then */
	 dos silent type value(tmp-dir + trim(xest.est-no) + "-01.a"
						   + string(probe.line,v-probe-fmt)) >>
			 value(lv-cebrowse-dir + trim(xest.est-no) + "-01.s"
						   + string(probe.line,v-probe-fmt)).
   end.

   else do:

      RUN get-dir-proc(INPUT trim(xest.est-no) + ".a"
						   + string(probe.line,v-probe-fmt),
                       OUTPUT tmp-dir).

      if opsys eq "unix" then
	 unix silent cat value(tmp-dir + trim(xest.est-no) + ".a"
						   + string(probe.line,v-probe-fmt)) >>
			 value(lv-cebrowse-dir + trim(xest.est-no) + ".s"
						   + string(probe.line,v-probe-fmt)).
      else /* if opsys eq "MSDOS" then */
	 dos silent type value(tmp-dir + trim(xest.est-no) + ".a"
						   + string(probe.line,v-probe-fmt)) >>
			 value(lv-cebrowse-dir + trim(xest.est-no) + ".s"
						   + string(probe.line,v-probe-fmt)).
   end.
end.

FOR EACH mclean:
  DELETE mclean.
END.

if xest.est-type eq 2 then run ce/probeu2.p (recid(probe)).

tmp-dir = lv-cebrowse-dir.

RUN ce/probeu3.p (ROWID(probe)).

PROCEDURE get-dir-proc:

   DEF INPUT PARAMETER ip-search AS CHAR NO-UNDO.
   DEF OUTPUT PARAMETER op-tmp-dir AS CHAR NO-UNDO.

   DEF VAR viDirCount AS INT NO-UNDO.

   DO viDirCount = 1 TO 3:

      CASE viDirCount:
          WHEN 1 THEN
             op-tmp-dir = lv-cebrowse-dir.
          WHEN 2 THEN
             op-tmp-dir = "users\".
          WHEN 3 THEN
             op-tmp-dir = ".\".
      END CASE.

      IF LOOKUP(SUBSTRING(op-tmp-dir,LENGTH(op-tmp-dir)),"\,/") EQ 0 THEN
         op-tmp-dir = op-tmp-dir + "\".

      op-tmp-dir = REPLACE(op-tmp-dir,"/","\").

      IF viDirCount EQ 3 OR SEARCH(op-tmp-dir + ip-search) NE ? THEN
         LEAVE.
   END.
END PROCEDURE.

/* end ---------------------------------- copr. 1997  advanced software, inc. */

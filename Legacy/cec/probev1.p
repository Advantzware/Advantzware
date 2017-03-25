/* -------------------------------------------------- cec/probev1.p 05/99 JLF */
/*                                                                            */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid.

{sys/inc/var.i shared}
{sys/form/s-top.f}

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

{cec/print4.i "shared" "shared"}

if not avail ce-ctrl then
find first ce-ctrl
    where ce-ctrl.company eq cocode
      and ce-ctrl.loc     eq locode
    no-lock no-error.

def shared frame probe.
DEF SHARED FRAME probe-peach.
DEF VAR cerunc-dec AS DEC NO-UNDO.

{cec/probe.f}

FIND FIRST sys-ctrl WHERE
     sys-ctrl.company EQ xest.company AND
     sys-ctrl.NAME EQ "CERUNC"
     NO-LOCK.

cerunc-dec = sys-ctrl.dec-fld.

{sys/inc/cerun.i}

find probe where recid(probe) eq v-recid no-lock.

assign
 voverall = round(probe.sell-price / probe.bsf,2)
 vtot-msf = probe.tot-lbs / 1000.

IF sys-ctrl.char-fld NE "PEACHTRE" THEN
DO:
   FIND FIRST reftable WHERE
        reftable.reftable EQ "probe.board" AND
        reftable.company  EQ probe.company AND
        reftable.loc      EQ ""            AND
        reftable.code     EQ probe.est-no  AND
        reftable.code2    EQ STRING(probe.line,"9999999999")
        NO-LOCK NO-ERROR.

   IF AVAIL reftable AND ce-ctrl.sell-by EQ "F" THEN
       ASSIGN cm-disp = "[   CM$: " + STRING(reftable.val[8])
              cmah-disp = "CMAH: " + STRING(reftable.val[9])
              cmoh-disp = "CMOH: " + STRING(reftable.val[10]) 
              cm%-disp  = "CM%: " + STRING(reftable.val[11],"->>9.99") + "%"
                          + "   ]".
   ELSE
       ASSIGN cm-disp = ""
              cmah-disp = ""
              cmoh-disp = ""
              cm%-disp  = "".

   IF cerunc-dec EQ 0 THEN
      display probe.est-qty
              probe.freight
              probe.fact-cost
              probe.full-cost
              probe.sell-price
              probe.net-profit   @ probe.net-profit
              probe.gross-profit @ probe.gross-profit
                (probe.sell-price - probe.fact-cost) / probe.fact-cost * 100
                WHEN ce-ctrl.sell-by EQ "S" @ probe.gross-profit
             voverall 
             probe.gsh-qty format ">>>>>9"
             vtot-msf
             cm-disp WHEN AVAIL reftable AND v-print-cm
             cmah-disp WHEN AVAIL reftable AND v-print-cm
             cmoh-disp WHEN AVAIL reftable AND v-print-cm
             cm%-disp WHEN AVAIL reftable AND v-print-cm
          with frame probe.
   ELSE
      display probe.est-qty
              probe.freight
              probe.fact-cost
              probe.full-cost
              probe.sell-price
              probe.net-profit   @ probe.net-profit
              probe.gross-profit @ probe.gross-profit
                (probe.sell-price - probe.fact-cost) / probe.fact-cost * 100
                WHEN ce-ctrl.sell-by EQ "S" @ probe.gross-profit
             voverall 
             probe.gsh-qty format ">>>>>9"
             vtot-msf
             cm-disp WHEN AVAIL reftable AND v-print-cm
             cmah-disp WHEN AVAIL reftable AND v-print-cm
             cmoh-disp WHEN AVAIL reftable AND v-print-cm
             cm%-disp WHEN AVAIL reftable AND v-print-cm
          with frame probe-big.
END.
ELSE
DO:
   FIND FIRST reftable WHERE
        reftable.reftable EQ "probe.board" AND
        reftable.company  EQ probe.company AND
        reftable.loc      EQ ""            AND
        reftable.code     EQ probe.est-no  AND
        reftable.code2    EQ STRING(probe.line,"9999999999")
        NO-LOCK.
   IF reftable.val[6] GT 0 THEN 
        dContPerManHr =  reftable.val[5] / reftable.val[6].
   ELSE 
        dContPerManHr = reftable.val[5].
   IF cerunc-dec EQ 0 THEN
      display probe.est-qty
              probe.freight
	          reftable.val[2]
	          reftable.val[3]
	          reftable.val[5]
	          dContPerManHr
              probe.sell-price
              voverall 
	          probe.gsh-qty format ">>>>>9"
	          vtot-msf
	   with frame probe-peach.
   ELSE
      display probe.est-qty
           probe.freight
	       reftable.val[2]
	       reftable.val[3]
	       reftable.val[5]
	       dContPerManHr
           probe.sell-price
           voverall 
	       probe.gsh-qty format ">>>>>9"
	       vtot-msf
	   with frame probe-peach-big.   
END.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

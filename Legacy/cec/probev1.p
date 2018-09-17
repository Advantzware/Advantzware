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
   IF ce-ctrl.sell-by EQ "F" THEN
       ASSIGN cm-disp = "[   CM$: " + STRING(probe.grossProfitPerM)
              cmah-disp = "CMAH: " + STRING(probe.grossProfitPerManhourAssemb)
              cmoh-disp = "CMOH: " + STRING(probe.grossProfitPerManHourOther)
              cm%-disp  = "CM%: " + STRING(probe.grossProfitPctTemp,"->>9.99") + "%"
                          + "   ]"
                          .

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
             cm-disp WHEN v-print-cm
             cmah-disp WHEN v-print-cm
             cmoh-disp WHEN v-print-cm
             cm%-disp WHEN v-print-cm
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
             cm-disp WHEN v-print-cm
             cmah-disp WHEN v-print-cm
             cmoh-disp WHEN v-print-cm
             cm%-disp WHEN v-print-cm
          with frame probe-big.
END.
ELSE
DO:

   IF probe.manHoursTotal GT 0 THEN 
        dContPerManHr =  probe.boardContributionTotal / probe.manHoursTotal.
   ELSE 
        dContPerManHr = probe.boardContributionTotal .
   
   IF cerunc-dec EQ 0 THEN
      display probe.est-qty
              probe.freight
	          probe.boardCostPerM
	          probe.boardCostPct
	          probe.boardContributionTotal 
	          dContPerManHr
              probe.sell-price
              voverall 
	          probe.gsh-qty format ">>>>>9"
	          vtot-msf
	   with frame probe-peach.
   ELSE
      display probe.est-qty
           probe.freight
	       probe.boardCostPerM
	       probe.boardCostPct
	       probe.boardContributionTotal 
	       dContPerManHr
           probe.sell-price
           voverall 
	       probe.gsh-qty format ">>>>>9"
	       vtot-msf
	   with frame probe-peach-big.   
END.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

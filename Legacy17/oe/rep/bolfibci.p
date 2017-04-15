/* ---------------------------------------------- oe/rep/bolfibci.p 02/06 YSK */
/* Print Fibre Container International BOL(Commercial Invoice)                                                  */
/* -------------------------------------------------------------------------- */
DEF INPUT PARAM ip-cust-no AS cha.
DEF INPUT PARAM ip-ship-id AS cha.

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer xoe-bolh     for oe-bolh.
def buffer xoe-boll     for oe-boll.
def buffer b-itemfg     for itemfg.

{oe/rep/oe-lad.i}

def var v-frt-pay-dscr      as   char.
def var v-due-date          like oe-ordl.prom-date.
def var v-sname             like oe-ord.sname.
def var v-lines             as   int.

def var v-part-dscr         as   char.
def var v-job-po            as   char.
def var v-part-qty          as   dec.

def var v-tot-pal           like oe-bolh.tot-pallets.
def var v-tot-wt            like oe-bolh.tot-wt.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr1 AS cha FORM "x(30)" NO-UNDO.
def var v-ship-addr2 AS cha FORM "x(30)" NO-UNDO.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-ship-addr3 as   char format "x(30)".
def var v-cust-addr3 as   char format "x(30)".
DEF VAR v-row-line AS INT NO-UNDO.
def var v-company-name  like company.NAME  NO-UNDO.
def var v-comp-addr1 AS cha format "x(30)" NO-UNDO.
def var v-comp-addr2 AS cha format "x(30)" NO-UNDO.
def var v-comp-city  like company.city  NO-UNDO.
def var v-comp-state like company.state  NO-UNDO.
def var v-comp-zip   like company.zip  NO-UNDO.
def var v-comp-addr3 as char format "x(30)"  NO-UNDO.
DEF VAR v-line-cnt AS INT NO-UNDO.

def workfile w2 no-undo
    field cases            as   int format ">>9"
    field cas-cnt          as   int format ">>>>>9".

find first oe-bolh no-lock no-error.
find first carrier no-lock no-error.
find first cust no-lock no-error.
find first oe-ord no-lock no-error.

find first company where company.company eq cocode no-lock no-error.
find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.
ASSIGN v-company-name = company.NAME
       v-comp-addr1 = company.addr[1]
       v-comp-addr2 = company.addr[2]
       .
for each report   where report.term-id eq v-term-id 
                    AND report.key-03 <> "N" ,
    first oe-bolh where recid(oe-bolh) eq report.rec-id,
    first cust NO-LOCK where cust.company eq cocode
                         and cust.cust-no eq oe-bolh.cust-no
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first carrier where carrier.company eq cocode
                         and carrier.carrier eq oe-bolh.carrier no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    assign
     v-ship-name    = shipto.ship-name
     v-ship-addr1 = shipto.ship-addr[1]
     v-ship-addr2 = shipto.ship-addr[2]
     v-ship-addr3   = shipto.ship-city + ", " +
                      shipto.ship-state + "  " +
                      shipto.ship-zip
     v-cust-addr3   = cust.city + ", " +
                      cust.state + "  " +
                      cust.zip.

    if trim(v-ship-addr3) eq "," then v-ship-addr3 = "".
    if trim(v-cust-addr3) eq "," then v-cust-addr3 = "".

    v-frt-pay-dscr = "".
    FOR EACH oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      v-sname[1] = oe-ord.sname[1].

	  CASE oe-ord.frt-pay:
           WHEN "P" THEN v-frt-pay-dscr = "PREPAID".
           WHEN "C" THEN v-frt-pay-dscr = "COLLECT".
           WHEN "B" THEN v-frt-pay-dscr = "PPD/CHG".
           WHEN "T" THEN v-frt-pay-dscr = "3rdPARTY".
      END CASE.
	
      LEAVE.
    END.

    v-due-date = oe-ord.due-date.
    
    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,
        first oe-ordl NO-LOCK where oe-ordl.company eq cocode
                        and oe-ordl.ord-no  eq oe-boll.ord-no
                        by oe-ordl.prom-date:        
      v-due-date = oe-ordl.prom-date.     
      leave.  
    end.
    {oe/rep/bolfibci.i}

    page.
    /*page stream last-page.*/
  end. /* first-of(oe-bolh.bol-no) */

end. /* for each oe-bolh */


/* end ---------------------------------- copr. 2000  Advanced Software, Inc. */

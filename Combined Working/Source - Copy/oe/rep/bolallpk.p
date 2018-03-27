/* --------------------------------------------- oe/rep/bolallpk.p  03/00 DJK */
/* PRINT All Packaging                                                        */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}
{sys/form/r-top.i}

{oe/rep/oe-lad.i}

def var v-time as char format "x(8)" no-undo.
def var v-carrier like carrier.dscr no-undo.
def var v-frt-pay-dscr as char no-undo.
DEF VAR v-ord-no LIKE oe-boll.ord-no NO-UNDO.
DEF VAR v-fob-code LIKE oe-ord.fob-code NO-UNDO.
DEF VAR v-terms LIKE oe-ord.terms NO-UNDO.
def var v-tot-pkgs as int format ">>>>>9" no-undo.
def var v-coname like coname no-undo.
def var v-printlines as int no-undo.
def var v-qty-uom like oe-ordl.pr-uom no-undo.
def var v-to-ship like oe-boll.qty no-undo.

def var v-ship-name  like shipto.ship-name.
def var v-ship-addr  like shipto.ship-addr.
def var v-ship-city  like shipto.ship-city.
def var v-ship-state like shipto.ship-state.
def var v-ship-zip   like shipto.ship-zip.
def var v-shiplines as int no-undo.
def var v-comp-name  like shipto.ship-name.
def var v-comp-addr  like shipto.ship-addr.
def var v-comp-city  like shipto.ship-city.
def var v-comp-state like shipto.ship-state.
def var v-comp-zip   like shipto.ship-zip.
def var v-comp-phone as char format "x(15)".  
def var already-printed as logical init no.

def buffer inhouse-cust for cust.
def var v-inhouse-name like cust.name.
def var v-inhouse-addr like cust.addr.
def var v-inhouse-city like cust.city.
def var v-inhouse-state like cust.state.
def var v-inhouse-zip   like cust.zip.
def var v-inhouse-phone as char format "x(15)". 
def var v-inhouse-notes like shipto.notes.
def var pg-num as integer.
def var v-ship-id like shipto.ship-id.
def var v-bill-i like oe-ord.bill-i.

/*GET THE *ONLY* inhouse customer*/
find first inhouse-cust where inhouse-cust.active = "X" no-lock no-error.

if avail inhouse-cust then do:
  assign
    v-inhouse-name     = inhouse-cust.name
    v-inhouse-addr[1]  = inhouse-cust.addr[1]
    v-inhouse-addr[2]  = inhouse-cust.addr[2]
    v-inhouse-city     = inhouse-cust.city
    v-inhouse-state    = inhouse-cust.state
    v-inhouse-zip      = inhouse-cust.zip
    v-inhouse-phone    = substring(inhouse-cust.area-code,1,3) + "-" + substring(inhouse-cust.phone,1,3) +
                         "-" + substring(inhouse-cust.phone,4,4).
                    
  find first shipto where (shipto.company eq inhouse-cust.company
  and  shipto.cust-no eq inhouse-cust.cust-no) no-lock no-error.
  
  if avail shipto THEN v-inhouse-notes[1] = shipto.notes[1].
end.                    
                  
find first company where company.company eq cocode no-lock no-error.

find first oe-ctrl where oe-ctrl.company eq cocode no-lock no-error.

form
  oe-boll.loc-bin format "x(6)" at 1
  oe-boll.i-no format "x(15)" at 8
  itemfg.i-name format "x(30)" at 24
  v-to-ship format ">>>,>>>" at 55
  "__________" at 63 skip
  itemfg.part-dscr1 format "x(30)" at 24 
  with frame ln-s down no-box no-labels STREAM-IO width 90.

form
               "Delivery Instructions"  at 5                                               skip
               "---------------------"  at 5                                               skip
               oe-rel.ship-i[1]         at 5                                               skip
               oe-rel.ship-i[2]         at 5                                               skip
               oe-rel.ship-i[3]         at 5                                               skip
               oe-rel.ship-i[4]         at 5                                               skip(1)
               
               "Special Instructions"   at 5                                               skip
               "--------------------"   at 5                                               skip
               oe-ord.bill-i[1]         at 5                                               skip
               oe-ord.bill-i[2]         at 5                                               skip
               oe-ord.bill-i[3]         at 5                                               skip
               oe-ord.bill-i[4]         at 5                                               skip(1)

               "Return Policy"          at 5                                               skip
               "-------------"          at 5                                               skip
               v-inhouse-notes[1]       at 5                                               skip(1)
    with frame instructions no-box no-labels STREAM-IO width 90.

form
   "LOC" at 1 "Item Number" at 8 "Item Name" at 24 "Qty To Ship" at 51 "Qty Shipped" at 63 skip
   "Description" at 24
   "--------------------------------------------------------------------------" at 1 skip
with frame instructions2 no-box no-labels STREAM-IO width 90.

form header
  "Received By:______________________________" at 1 " Date Rec'd:____________________"
  with frame hd-bottom-comp no-box no-labels page-bottom STREAM-IO width 80.


assign
 tmpstore    = fill("-",80)
 v-last-page = 0.

for each report   where report.term-id eq v-term-id,
    first oe-bolh where recid(oe-bolh) eq report.rec-id
    break by oe-bolh.bol-no:

  if first-of(oe-bolh.bol-no) then do:
    find first cust
	    where cust.company eq cocode
	      and cust.cust-no eq oe-bolh.cust-no
	    no-lock no-error.

    RUN oe/custxship.p (oe-bolh.company,
                        oe-bolh.cust-no,
                        oe-bolh.ship-id,
                        BUFFER shipto).

    find first carrier
	    where carrier.company eq oe-bolh.company
	      and carrier.carrier eq oe-bolh.carrier
	    no-lock no-error.
	  
    assign
     v-carrier      = if avail carrier then carrier.dscr else ""
     v-frt-pay-dscr = ""
     v-ord-no       = 0
     v-qty-uom      = "".
      
    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK,

        first oe-ord
	    where oe-ord.company eq oe-boll.company
	      and oe-ord.ord-no  eq oe-boll.ord-no
	    NO-LOCK:

      ASSIGN
       v-ord-no    = oe-boll.ord-no
       v-fob-code  = oe-ord.fob-code
       v-terms     = oe-ord.terms
       v-bill-i[1] = oe-ord.bill-i[1]       
       v-bill-i[2] = oe-ord.bill-i[2]    
       v-bill-i[3] = oe-ord.bill-i[3]    
       v-bill-i[4] = oe-ord.bill-i[4].    

	  case oe-ord.frt-pay:
           when "P" THEN v-frt-pay-dscr = "PREPAID".
           when "C" THEN v-frt-pay-dscr = "COLLECT".
           when "B" THEN v-frt-pay-dscr = "PPD/CHG".
           when "T" THEN v-frt-pay-dscr = "3rdPARTY".
      end case.
	
	  find first oe-ordl
	      where oe-ordl.company eq oe-ord.company
	        and oe-ordl.ord-no  eq oe-ord.ord-no
	      no-lock no-error.
	    
	  if avail oe-ordl then DO:
	    find first uom where uom.uom eq oe-ordl.pr-uom no-lock no-error.
	  
	    v-qty-uom = if avail uom then substr(uom.dscr,1,8)
		            else oe-ordl.pr-uom.
      END.
	
      LEAVE.
    END. 
      
    v-time = string(time,"hh:mm am").

    assign
     v-tot-pkgs = 0
     v-page-tot = 0.

    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no NO-LOCK:
	    
     assign
	   v-tot-pkgs = v-tot-pkgs + oe-boll.cases
	   v-page-tot = v-page-tot + 3.
	 
	  if oe-boll.partial gt 0 then v-tot-pkgs = v-tot-pkgs + 1.
	
	  find first itemfg
	      where itemfg.company eq oe-boll.company
	        and itemfg.i-no    eq oe-boll.i-no
	      no-lock no-error.
	    
	  if avail itemfg and itemfg.part-dscr1 eq "" then
	
	  v-page-tot = v-page-tot - 1.
    end.

    v-page-tot = if (v-page-tot / 34) le 1 then 1
	             else round(((v-page-tot / 34) +
		         (if (v-page-tot modulo 34) ne 0 then .5 else 0)),0).

    if first-of(oe-bolh.bol-no) then do:
	  if oe-ctrl.pr-broker and avail cust and shipto.broker then
	    v-coname = cust.name.
	  
	  else do:
	    find first company where company.company = cocode no-lock no-error.
	    if avail company then v-coname = company.name.
	  end.

	  if avail shipto then
	    assign
	     v-ship-id      = shipto.ship-id
	     v-ship-name    = shipto.ship-name
	     v-ship-addr[1] = shipto.ship-addr[1]
	     v-ship-addr[2] = shipto.ship-addr[2]
	     v-ship-city    = shipto.ship-city
	     v-ship-state   = shipto.ship-state
	     v-ship-zip     = shipto.ship-zip.

      assign
       v-comp-name    = cust.name
	   v-comp-addr[1] = cust.addr[1]
       v-comp-addr[2] = cust.addr[2]
	   v-comp-city    = cust.city
	   v-comp-state   = cust.state
	   v-comp-zip     = cust.zip
	   v-comp-phone   = cust.area-code + "-" + substring(cust.phone,1,3) +
		                 "-" + substring(cust.phone,4,4).
         
	  form header
           "BILL OF LADING"                     at 10
           "BOL No:"                            at 63
           oe-bolh.bol-no
           skip(1)    
	       v-inhouse-name                       at 10
           "Order No:"                          at 50
           TRIM(STRING(v-ord-no,">>>>>>>>"))    FORMAT "x(8)"
		   v-inhouse-addr[1]                    at 10
		   v-inhouse-addr[2]                    at 10
           "BOL Date:"                          at 50
           oe-bolh.bol-date
		   v-inhouse-city                       at 10
           v-inhouse-state
           v-inhouse-zip
           "Phone:"                             at 10
           v-inhouse-phone
           /*"Freight:"                           at 51
           oe-bolh.freight       task# 05250501 YSK 05/25/05*/
           skip(1)
		   "SalesPerson:"                       at 10
           oe-ord.sname[1]
           "Tax:"                               AT 55
           oe-ord.tax-gr
           skip(1)
		   "Ship To:"                           at 5
           v-ship-id                            at 25
           "Customer:"                          at 44
           v-comp-name
           v-ship-name                          at 13
           "Contact:"                           at 45
           oe-ord.contact
           v-ship-addr[1]                       at 13
           "Phone:"                             at 47
           v-comp-phone
           skip(1)
           v-ship-addr[2]                       at 13
           v-ship-city                          at 13
           v-ship-state
           v-ship-zip
           "P O No."                            at 49
           oe-ord.po-no
           skip(1)        
           "FOB"                                at 5
           "Terms"                              at 35
           "Ship Via"                           at 60
           "---------------"                    at 5
           "---------------"                    at 35
           "---------------"                    at 60
           v-fob-code                           AT 5
           v-terms                              at 35
           oe-bolh.carrier                      at 60
           skip(1)
               
          with frame hd-top-comp no-box no-labels page-top STREAM-IO width 90.
   	  view frame hd-top-comp.

      end.
    end. /* first-of(oe-bolh.bol-no) */

    v-printlines = 38.

    for each oe-boll where oe-boll.company eq oe-bolh.company and oe-boll.b-no eq oe-bolh.b-no:
	  
      find first oe-ordl
	      where oe-ordl.company eq cocode
	        and oe-ordl.ord-no  eq oe-boll.ord-no
	        and oe-ordl.i-no    eq oe-boll.i-no
	        and oe-ordl.line    eq oe-boll.line
	      no-lock no-error.

      if v-printlines gt 50 then do:
        display with frame instructions2.
	    v-printlines = 22.
      end.

      if avail oe-ordl then
	  find first oe-ord
	      where oe-ord.company eq oe-ordl.company
	        and oe-ord.ord-no  eq oe-ordl.ord-no
	      no-lock no-error.
	    
      find first itemfg
	      where itemfg.company eq oe-boll.company
	        and itemfg.i-no    eq oe-boll.i-no
	      no-lock no-error.

      if oe-boll.cases gt 0 or oe-boll.partial ne 0 then do:
      
	  release oe-rel.
	
	  find first oe-rell
	      where oe-rell.company eq oe-boll.company
	        and oe-rell.r-no    eq oe-boll.r-no
            AND oe-rell.ord-no  EQ oe-boll.ord-no
	        and oe-rell.i-no    eq oe-boll.i-no
	        and oe-rell.line    eq oe-boll.line
	       no-lock no-error.

	  if avail oe-rell then do:
	    find first oe-relh of oe-rell no-lock.
	  
	    find first oe-rel
	        where oe-rel.company eq oe-rell.company
		      and oe-rel.ord-no  eq oe-rell.ord-no
		      and oe-rel.line    eq oe-rell.line
		      and oe-rel.link-no eq oe-rell.r-no
		      and oe-rel.ship-no eq oe-relh.ship-no
		      and oe-rel.i-no    eq oe-rell.i-no
	        no-lock no-error.

	    if not avail oe-rel then
	    find first oe-rel
		    where oe-rel.company  eq oe-rell.company
		      and oe-rel.ord-no   eq oe-rell.ord-no
		      and oe-rel.line     eq oe-rell.line
		      and oe-rel.rel-date eq oe-relh.rel-date
		      and oe-rel.ship-no  eq oe-relh.ship-no
		      and oe-rel.i-no     eq oe-rell.i-no
		    no-lock no-error.
	  end. 
	
      if already-printed = no then do:
        display oe-rel.ship-i[1]     
                oe-rel.ship-i[2]     
                oe-rel.ship-i[3]     
                oe-rel.ship-i[4]     
                v-bill-i[1]       
                v-bill-i[2]    
                v-bill-i[3]    
                v-bill-i[4]    
                v-inhouse-notes[1]    
            with frame instructions no-box no-labels width 80.
        display with frame instructions2.
        already-printed = yes.
      end.  

	  if avail oe-ordl and oe-ordl.pr-uom eq "CS" then
	    v-to-ship = oe-boll.cases + (if oe-boll.partial gt 0 then 1 else 0).
	  else
  	    v-to-ship = oe-boll.qty.

	  display oe-boll.loc-bin   
              oe-boll.i-no      
	          itemfg.i-name when avail itemfg
	          v-to-ship      
              itemfg.part-dscr1 when avail itemfg
	      with frame ln-s.
	
	  down with frame ln-s.
	  v-printlines = v-printlines + 2.

      oe-boll.printed = yes.
    end. /* for each oe-boll */   
    
    oe-bolh.printed = yes.
  end. /* for each oe-bolh */

  view frame hd-bottom-comp.
  page.
  already-printed = no.
end.

/* END ---------------------------------- copr. 1992  Advanced Software, Inc. */


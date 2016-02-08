/* -------------------------------------------------- jc/jdedwrdx.p 12/02 JLF */
/*                                                                            */
/* Export AR to JD Edwards                                                    */
/*                                                                            */
/* -------------------------------------------------------------------------- */

def input parameter v-recid as recid no-undo.
DEF INPUT PARAMETER ip-first AS LOGICAL NO-UNDO.

{sys/inc/var.i shared}
{sys/form/r-top.i}

def buffer b-job-mch for job-mch.
def buffer b-shipto for shipto.

def var v-jded      as   char                                          no-undo.
def var v-dec       as   dec                                           no-undo.
def var v-int       as   int                                           no-undo.
def var v-date      as   date                                          no-undo.
DEF VAR v-xls-header AS CHAR NO-UNDO.
DEF VAR v-file AS CHAR NO-UNDO.
DEF VAR v-terms-d AS CHAR NO-UNDO.
DEF VAR v-part-no AS CHAR NO-UNDO.
DEF VAR v-part-dscr1 AS CHAR NO-UNDO.
DEF VAR v-part-dscr2 AS CHAR NO-UNDO.
DEF VAR v-item-no AS CHAR NO-UNDO.

def new shared temp-table tt-cost field tt-dscr as   char
                                  field tt-cost as   dec
                                  field tt-type like item.mat-type
                                  index tt-cost tt-type tt-dscr
                                  index tt-dscr tt-dscr.
                                  
def stream s-jded.
                              
{sys/inc/jdedwdir.i}
if not sys-ctrl.log-fld then leave.
         
find first ce-ctrl {sys/look/ce-ctrlW.i} no-lock no-error.

v-xls-header = "Corp Code,Company Name,System,Invoice No,Invoice Date,Discount,Billing Terms,Line Amt Gross,Line Amt Net,Freight,Tax,Set up,Sold to Cust No," +
               "Start Date,Sold to Cust Name 1,Sold to Cust Name 2,Sold to Address Line 1,Sold to Address Line 2,Sold to Address Line 3,Sold to City,Sold to State," + 
               "Sold to Zip Code," + "Ship to Cust No,Start Date,Ship to Cust Name 1,Ship to Cust Name 2,Ship to Address Line 1,Ship to Address Line 2," +
               "Ship to Address Line 3,Ship to City,Ship to State,Ship to Zip Code,Order No,Item No,Order Date,Cust PO No,Release No,Line Qty,Requested Delivery Date," +
               "Category,Cust Part No,Cust Item Description 1,Cust Item Description 2,Estimate No,Warehouse Item Flag,Item Price,UOM,MSF,Weight,Weight Type,Style,Flute," +
               "Number of Inks,Test,Outer Liner Type, Ship Date,Ship Qty,BOL,Carrier, FOB Code,Direct Labor,Fixed Overhead,Variable Overhead,Board,Foam,PVC/Paper,Adders,Ink," +  
               "Glue,Cases,Pallets,Foil Stamping,Banding,Laminate for Medium & Liner,Miscellaneous,Wrap for Setup Boxes,Stitching,Tape,Varnish & Coatings," +
               "Wax & Window,Trailer Charge,Other 1,Other 2,Other 3,Other 4,Prep Material,Misc Material,Freight Charge,Special Charge 1,Special Charge 2," +
               "Special Charge 3,GS&A Material,GS&A Labor,Royalty,Warehousing Charge,Style Markup,Board Markup".


   
if opsys eq "UNIX" and substr(v-out,1,1) ne v-slash then
    v-out = v-slash + v-out.

  if substr(v-out,length(v-out),1) eq v-slash then
    substr(v-out,length(v-out),1) = "".

  v-file = v-out + v-slash + "JDEDWARD.CSV".  

 IF ip-first = YES THEN DO:
   output stream s-jded to value(v-out + v-slash + "JDEDWARD.CSV").
   put stream s-jded unformatted v-xls-header skip.
   OUTPUT STREAM s-jded CLOSE.  
 END.


for first ar-invl where recid(ar-invl) eq v-recid no-lock,
    first ar-inv where ar-inv.x-no eq ar-invl.x-no no-lock:
    
/*   if opsys eq "UNIX" and substr(v-out,1,1) ne v-slash then */
/*     v-out = v-slash + v-out.                               */
/*                                                            */
/*   if substr(v-out,length(v-out),1) eq v-slash then         */
/*     substr(v-out,length(v-out),1) = "".                    */

  output stream s-jded to value(v-out + v-slash + "JDEDWARD.CSV") append.
  
  v-jded = "".
  
  /* Company Info: */
    
  /* Corporation Code */
  v-jded = v-jded + string("ASI","x(3)") + ",".

  /* Company Name */
  v-jded = v-jded + string(coname,"x(30)") + ",".
  
  /* SORCE SYSTEM */
  v-jded = v-jded + string("ASI","x(3)") + ",".
  
  /* Invoice Info: */
  
  /* Invoice Number */
  v-jded = v-jded + string(ar-inv.inv-no,">>>>>>>>>>") + ",".

  /* Invoice Date */
  assign
   v-date = ar-inv.inv-date
   v-jded = v-jded + string(year(v-date),"9999") +
                     string(month(v-date),"99")  +
                     string(month(v-date),"99") + ",".
                    
  /* Face of invoice discounts */
  v-jded = v-jded + string(ar-invl.amt * ar-invl.disc / 100,">>>>>>>9.99") + ",".
  
  /* Billing Terms */
  ASSIGN
   v-terms-d = ar-inv.terms-d
   v-terms-d = REPLACE(v-terms-d,","," ")
   v-terms-d = REPLACE(v-terms-d,'"',' ').
  v-jded = v-jded + string(v-terms-d,"x(30)") + ",".
  
  /* Invoice Line Amount Gross Sales */
  v-dec = 0.
  if ar-invl.tax then
  run ar/calctax2.p (ar-inv.tax-code,no,ar-invl.amt,ar-inv.company,ar-invl.i-no,output v-dec).

  ASSIGN
  v-jded = v-jded + string(ar-invl.amt +
                           ar-invl.t-freight +
                           v-dec,">>>>>>>9.99") + ","
    
  /* Line Amount Net */
  v-jded = v-jded + string(ar-invl.amt,">>>>>>>9.99") + ","
  
  /* Face of invoice Freight */
  v-jded = v-jded + string(ar-invl.t-freight,">>>>>>>9.99") + ","
  
  /* Face of invoice taxes */
  v-jded = v-jded + string(v-dec,">>>>>>>9.99") + ","
   
  /* Face of invoice set-up */
  v-jded = v-jded + string(0,">>>>>>>9.99") + ",".
  
  /* Sold Customer Info: */
  find first cust
      where cust.company eq cocode
        and cust.cust-no eq ar-inv.cust-no
      no-lock no-error.
  if avail cust then
  find first reftable
      where reftable.reftable eq "JDEDWARDCUST#"
        and reftable.company  eq cocode
        and reftable.loc      eq ""
        and reftable.code     eq cust.cust-no
        and reftable.code2    eq cust.cust-no
      no-lock no-error.

  /* Customer number */
  v-jded = v-jded + string(if avail reftable then reftable.dscr
                                             else ar-inv.cust-no,"x(8)") + ",".
  
  /* START DATE */
  assign
   v-date = if avail cust then cust.date-field[1] else ar-inv.inv-date
   v-jded = v-jded + string(year(v-date),"9999") +
                     string(month(v-date),"99")  +
                     string(month(v-date),"99") + ",".
  
  /* Customer Name 1 */
  v-jded = v-jded + string(if avail cust then cust.name else "","x(30)") + ",".
  
  /* Customer Name 2 */
  v-jded = v-jded + string("","x(30)") + ",".
  
  /* Address Line 1 */
  v-jded = v-jded + string(if avail cust then cust.addr[1] else "","x(25)") + ",".
  
  /* Address Line 2 */
  v-jded = v-jded + string(if avail cust then cust.addr[2] else "","x(25)") + ",".
  
  /* Address Line 3 */
  v-jded = v-jded + string("","x(25)") + ",".
  
  /* City */
  v-jded = v-jded + string(if avail cust then cust.city else "","x(19)") + ",".
  
  /* State */
  v-jded = v-jded + string(if avail cust then cust.state else "","x(2)") + ",".

  /* Zip */
  v-jded = v-jded + string(if avail cust then cust.zip else "","x(10)") + ",".
  
  /* Ship Customer Info: */
  find first shipto
      where shipto.company eq cocode
        and shipto.cust-no eq ar-inv.cust-no
        and shipto.ship-id eq ar-inv.ship-id
      no-lock no-error.
  if avail shipto then
  find first reftable
      where reftable.reftable eq "JDEDWARDCUST#"
        and reftable.company  eq cocode
        and reftable.loc      eq ""
        and reftable.code     eq cust.cust-no
        and reftable.code2    eq shipto.ship-id
      no-lock no-error.

  /* Customer number */
  v-jded = v-jded + string(if avail reftable then reftable.dscr
                                             else ar-inv.ship-id,"x(8)") + ",".
  
  /* START DATE */
  assign
   v-date = if avail cust then cust.date-field[1] else ar-inv.inv-date
   v-jded = v-jded + string(year(v-date),"9999") +
                     string(month(v-date),"99")  +
                     string(month(v-date),"99") + ",".
  
  /* Customer Name 1 */
  v-jded = v-jded + string(if avail shipto then shipto.ship-name
                                           else "","x(30)") + ",".
  
  /* Customer Name 2 */
  v-jded = v-jded + string("","x(30)") + ",".
  
  /* Address Line 1 */
  v-jded = v-jded + string(if avail shipto then shipto.ship-addr[1]
                                           else "","x(25)") + ",".
  
  /* Address Line 2 */
  v-jded = v-jded + string(if avail shipto then shipto.ship-addr[2]
                                           else "","x(25)") + ",".
  
  /* Address Line 3 */
  v-jded = v-jded + string("","x(25)") + ",".
  
  /* City */
  v-jded = v-jded + string(if avail shipto then shipto.ship-city
                                           else "","x(19)") + ",".
  
  /* State */
  v-jded = v-jded + string(if avail shipto then shipto.ship-state
                                           else "","x(2)") + ",".
  
  /* Zip */
  v-jded = v-jded + string(if avail shipto then shipto.ship-zip
                                           else "","x(10)") + ",".
  
  /* Order Info: */
  if ar-invl.ord-no ne 0 then do:
    find first oe-ordl
        where oe-ordl.company eq cocode
          and oe-ordl.ord-no  eq ar-invl.ord-no
          and oe-ordl.i-no    eq ar-invl.i-no
        no-lock no-error.
    find first oe-ord
        where oe-ord.company eq cocode
          and oe-ord.ord-no  eq ar-invl.ord-no
        no-lock no-error.
  end.
  
  if ar-invl.b-no ne 0 then
  find first oe-bolh where oe-bolh.b-no eq ar-invl.b-no no-lock no-error.

  /* Internal Order Number */
  v-jded = v-jded + string(ar-invl.ord-no,">>>>>>>>>>") + ",".
  
  /* Item Number */
  ASSIGN
      v-item-no = ar-invl.i-no
      v-item-no = REPLACE(v-item-no,","," ")
      v-item-no = REPLACE(v-item-no,'"',' ').
  v-jded = v-jded + string(v-item-no,"x(15)") + ",".
  
  /* Order Date */
  assign
   v-date = if avail oe-ord then oe-ord.ord-date else ar-inv.inv-date
   v-jded = v-jded + string(year(v-date),"9999") +
                     string(month(v-date),"99")  +
                     string(month(v-date),"99") + ",".
                     
  /* Customer PO Number */
  v-jded = v-jded + string(ar-invl.po-no,"x(15)") + ",".
  
  /* Release Number */
  v-jded = v-jded +
           string(ar-invl.ord-no,">>>>>>>>>>") + "-" +
           string(if avail oe-bolh then oe-bolh.rel-no else 0,"999") + "-" +
           string(if avail oe-bolh then oe-bolh.b-ord-no else 0,"99") + ",".
           
  /* Line item order quantity */
  v-jded = v-jded + string(if ar-invl.ord-no ne 0 then ar-invl.qty
                                             else ar-invl.inv-qty,"->>>>>>>>9") + ",".
                                       
  /* Requested Delivery Date */
  assign
   v-date = ar-invl.req-date.

  IF v-date NE ? THEN
     v-jded = v-jded + string(year(v-date),"9999") +
                       string(month(v-date),"99")  +
                       string(month(v-date),"99") + ",".
  ELSE
     v-jded = v-jded + string(year(ar-invl.inv-date),"9999") +
                       string(month(ar-invl.inv-date),"99")  +
                       string(month(ar-invl.inv-date),"99") + ",".
                     
  /* Item Info:(Style) */
  find first itemfg
      where itemfg.company eq cocode
        and itemfg.i-no    eq ar-invl.i-no
      no-lock no-error.
      
  if avail itemfg then
  find first procat
      where procat.company eq cocode
        and procat.procat  eq itemfg.procat
      no-lock no-error.
  
  /* Product Type */
  v-jded = v-jded + string(if avail itemfg then itemfg.procat else "","x(10)") +
                    string(if avail procat then procat.dscr   else "","x(20)") + ",".
           
  /* Customer's Part Number */
  ASSIGN
      v-part-no = ar-invl.part-no
      v-part-no = REPLACE(v-part-no,","," ")
      v-part-no = REPLACE(v-part-no,'"',' ').

  v-jded = v-jded + string(v-part-no,"x(15)") + ",".

  /* Customer's item description line 1 */
  ASSIGN
      v-part-dscr1 = ar-invl.part-dscr1
      v-part-dscr1 = REPLACE(v-part-dscr1,","," ")
      v-part-dscr1 = REPLACE(v-part-dscr1,'"',' ').
  v-jded = v-jded + string(v-part-dscr1,"x(30)") + ",".
  
  /* Customer's item description line 2 */
    ASSIGN
      v-part-dscr2 = ar-invl.part-dscr2
      v-part-dscr2 = REPLACE(v-part-dscr2,","," ")
      v-part-dscr2 = REPLACE(v-part-dscr2,'"',' ').
  v-jded = v-jded + string(v-part-dscr2,"x(30)") + ",".
  
  /* Internal specification number */
  v-jded = v-jded + string(trim(ar-invl.est-no),"x(6)") + ",".
  
  /* Warehouse Item Flag */
  v-jded = v-jded + string(" ","x") + ",".
    
  /* Item price */
  v-jded = v-jded + string(ar-invl.unit-pr,"->>>>>>9.99<<<<") + ",".
  
  /* PRICE TYPE - UOM*/
  v-jded = v-jded + string(trim(ar-invl.pr-uom),"x(4)") + ",".
  
  /* MSF per each */
  v-jded = v-jded + string(ar-invl.sf-sht,"->>>>>>9.99<<<<") + ",".

  /* Weight each */
  assign
   v-dec  = if ar-invl.t-weight ne 0 then (ar-invl.t-weight / ar-invl.ship-qty)
            else
            if avail itemfg then (itemfg.weight-100 / 100)
            else 0
   v-dec  = if v-dec eq ? then 0 else v-dec
   v-jded = v-jded + string(v-dec,"->>>>>>9.99<<<<") + ",".
   
  /* Weight Type */
  v-jded = v-jded + string("P","x") + ",".
  
  /* Style */
  v-jded = v-jded + string(if avail itemfg then itemfg.style else "","x(6)") + ",".
  
  /* Flute */
  v-jded = v-jded + string(if avail itemfg then itemfg.flute else "","x(3)") + ",".
  
  /* Number of inks */
  v-jded = v-jded + string(v-int,"9") + ",".
  
  /* Test */
  v-jded = v-jded + string(if avail itemfg then itemfg.test else "","x(8)") + ",".
  
  /* Outer Liner Type */
  v-jded = v-jded + string(v-int,"9") + ",".

  /* Ship Date */
  assign
   v-date = if avail oe-bolh AND oe-bolh.bol-date NE ? then oe-bolh.bol-date else ar-inv.inv-date
   v-jded = v-jded + string(year(v-date),"9999") +
                     string(month(v-date),"99")  +
                     string(month(v-date),"99") + ",".
              
  /* SHIPPED QUANTITY */
  v-jded = v-jded + string(ar-invl.ship-qty,"->>>>>>>>9") + ",".
  
  /* BOL Number */
  v-jded = v-jded + string(ar-invl.bol-no,">>>>>>>>>>") + ",".
  
  /* Carrier */
  v-jded = v-jded + string(ar-inv.carrier,"x(5)") + ",".
  
  /* FOB Code */
  v-jded = v-jded + string(ar-inv.fob-code,"x(4)") + ",".
  
  run ar/jdedwrd1.p (recid(ar-invl)).
  
  /* Direct Labor */
  find first tt-cost where tt-dscr eq "DL" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Fixed Overhead */
  find first tt-cost where tt-dscr eq "FO" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Variable Overhead */
  find first tt-cost where tt-dscr eq "VO" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
 
  /* Board */
  find first tt-cost where tt-type eq "B" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Foam */
  find first tt-cost where index("1234",tt-type) gt 0 no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",". 

  /* PVC / Paper */
  find first tt-cost where tt-type eq "P" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Adders */
  find first tt-cost where tt-type eq "A" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Ink */
  find first tt-cost where tt-type eq "I" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".

  /* Glue */
  find first tt-cost where tt-type eq "G" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Cases / Bundles */
  find first tt-cost where tt-type eq "C" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Pallets / Bales */
  find first tt-cost where tt-type eq "D" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Foil Stamping */
  find first tt-cost where tt-type eq "F" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Banding */
  find first tt-cost where tt-type eq "J" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".

  /* Laminate for Medium & Liner */
  find first tt-cost where tt-type eq "L" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Miscellaneous */
  find first tt-cost where tt-type eq "M" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",". 
   
  /* Wrap for Setup Boxes */
  find first tt-cost where tt-type eq "R" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",". 

  /* Stitching */
  find first tt-cost where tt-type eq "S" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Tape */
  find first tt-cost where tt-type eq "T" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Varnish & Coatings */
  find first tt-cost where tt-type eq "V" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Wax & Window */
  find first tt-cost where tt-type eq "W" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Trailer Charge */
  find first tt-cost where tt-type eq "Z" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".

  /* Other 1 */
  find first tt-cost where tt-type eq "O1" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Other 2 */
  find first tt-cost where tt-type eq "O2" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Other 3 */
  find first tt-cost where tt-type eq "O3" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Other 4 */
  find first tt-cost where tt-type eq "O4" no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Prep Material */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "Prep.  Material"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Misc Material */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "Misc.  Material"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Freight */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "Freight"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".

  /* Special Charge 1 */
  find first tt-cost
      where tt-type eq ""
        and substr(tt-dscr,1,22) eq ce-ctrl.spec-l[1]
        and ce-ctrl.spec-l[1] ne ""
        and ce-ctrl.spec-%[1] ne 0
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".

  /* Special Charge 2 */
  find first tt-cost
      where tt-type eq ""
        and substr(tt-dscr,1,22) eq ce-ctrl.spec-l[2]
        and ce-ctrl.spec-l[2] ne ""
        and ce-ctrl.spec-%[2] ne 0
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Special Charge 3 */
  find first tt-cost
      where tt-type eq ""
        and substr(tt-dscr,1,22) eq ce-ctrl.spec-l[3]
        and ce-ctrl.spec-l[3] ne ""
        and ce-ctrl.spec-%[3] ne 0
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".


  /* GS&A Material */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "GS&A Material"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* GS&A Labor */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "GS&A Labor"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Royalty */
  find first tt-cost
      where tt-type eq ""
        and substr(tt-dscr,1,22) eq "Royalty"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Warehousing */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "Warehousing"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Style Markup */
  find first tt-cost
      where tt-type eq ""
        and substr(tt-dscr,1,22) eq "Style Markup"
      no-error.
  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".
   
  /* Board Markup */
  find first tt-cost
      where tt-type eq ""
        and tt-dscr eq "Board Markup"
      no-error.

  assign
   v-dec  = if avail tt-cost AND tt-cost NE ? then tt-cost else 0
   v-jded = v-jded + string(v-dec,">>>>>>9.99<<<<") + ",".

  put stream s-jded unformatted v-jded skip.
end.

OUTPUT STREAM s-jded CLOSE.
/* end ---------------------------------- copr. 2002  advanced software, inc. */

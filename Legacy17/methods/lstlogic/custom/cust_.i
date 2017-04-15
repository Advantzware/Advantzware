/* cust_.i */

/*- Programming Notes -------------------------------------------------------

1) fields with no frame reference are scoped to 'FRAME {&FRAME-NAME}'

2) if internal procedures are needed, add'em to 'lstlogic/persist.p'
   alphabetically and use the following syntax:
   RUN <ip-name> IN ListLogic-Handle [(<parameters>)].
   The naming convention used for <ip-name> should begin with 'cust_'.

3) if notes are desired for this listing use the following syntax:
   {methods/lstlogic/shownote.i &db_table="cust" &col="5" &frame-name="f-notes"}

4) if misc fields are desired for this listing use the following syntax:
   {methods/lstlogic/showmisc.i &db_table="cust" &col="5" &frame-name="f-miscflds"}

5) if addresses are desired for this listing use the following syntax:
   {methods/lstlogic/showaddr.i &db_table="cust" &col="5" &frame-name="f-addresses"}

6) if phones are desired for this listing use the following syntax:
   {methods/lstlogic/showphon.i &db_table="cust" &col="5" &frame-name="f-phones"}

---------------------------------------------------------------------------*/

DISPLAY
  cust.cust-no
  cust.name.

 IF  tb_phone THEN
  FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK 
     WITH FRAME phone NO-LABEL STREAM-IO TITLE '----- PHONE CONTACT INFO -----' COLUMN 5 WIDTH 85:
   
     FIND FIRST titlcode WHERE titlcode.titlcode = phone.titlcode NO-LOCK NO-ERROR.
  DISPLAY
    phone.attention LABEL 'CONTACT' FORMAT "x(25)"
    titlcode.DESCRIPTION FORMAT "x(25)"  WHEN AVAIL titlcode  @ phone.titlcode LABEL 'TITLE'  
    phone.e_mail FORMAT "x(25)"  LABEL 'EMAIL'  .
  DOWN.

 END.

 IF  tb_phone THEN
  FOR EACH phone WHERE phone.table_rec_key = cust.rec_key NO-LOCK 
     WITH FRAME phone2 NO-LABEL STREAM-IO COLUMN 5 WIDTH 85:
  DISPLAY
    phone.phone LABEL 'CPhone' FORMAT "x(25)"
    phone.phone_ext LABEL 'Ext' FORMAT "x(25)"   .
  DOWN.

 END.

IF show-shipto THEN
FOR EACH shipto OF cust NO-LOCK
    WITH FRAME shipto STREAM-IO TITLE '----- SHIP TO ADDRESSES -----' COLUMN 5 WIDTH 85:
  DISPLAY
    shipto.ship-no LABEL 'ID#'
    shipto.ship-id
    shipto.ship-name
    shipto.ship-addr[1] LABEL 'Address'.
  IF shipto.ship-addr[2] NE '' THEN
  DO:
    DOWN.
    DISPLAY shipto.ship-addr[2] @ shipto.ship-addr[1].
  END.
  IF shipto.ship-city NE '' OR
     shipto.ship-state NE '' OR
     shipto.ship-zip NE '' THEN
  DO:
    DOWN.
    DISPLAY
      shipto.ship-city + ', ' + shipto.ship-state + ' ' + shipto.ship-zip @
      shipto.ship-addr[1].
  END.
END.
IF show-soldto THEN
FOR EACH soldto OF cust NO-LOCK
    WITH FRAME soldto STREAM-IO TITLE '----- SOLD TO ADDRESSES -----' COLUMN 5 WIDTH 85:
  DISPLAY
    soldto.sold-no LABEL 'ID#'
    soldto.sold-id
    soldto.sold-name
    soldto.sold-addr[1] LABEL 'Address'.
  IF soldto.sold-addr[2] NE '' THEN
  DO:
    DOWN.
    DISPLAY soldto.sold-addr[2] @ soldto.sold-addr[1].
  END.
  IF soldto.sold-city NE '' OR
     soldto.sold-state NE '' OR
     soldto.sold-zip NE '' THEN
  DO:
    DOWN.
    DISPLAY
      soldto.sold-city + ', ' + soldto.sold-state + ' ' + soldto.sold-zip @
      soldto.sold-addr[1].
  END.
END.
DEFINE VARIABLE taglabel AS CHARACTER FORMAT 'X(12)' NO-UNDO.
DEFINE VARIABLE profit LIKE cust.ytd-sales EXTENT 3 NO-UNDO.
DEFINE VARIABLE profit-pct LIKE cust.ytd-sales EXTENT 3 NO-UNDO.

{custom/gperiod.i}
{custom/getperd.i}

ASSIGN
  profit[1] = cust.sales[gperiod] - cust.cost[1]
  profit[2] = cust.ytd-sales - cust.cost[5]
  profit[3] = cust.lyr-sales - cust.cost[6]
  profit-pct[1] = profit[1] / cust.sales[gperiod] * 100
  profit-pct[2] = profit[2] / cust.ytd-sales * 100
  profit-pct[3] = profit[3] / cust.lyr-sales * 100.
IF profit[1] EQ ? THEN profit[1] = 0.
IF profit[2] EQ ? THEN profit[2] = 0.
IF profit[3] EQ ? THEN profit[3] = 0.
IF profit-pct[1] EQ ? THEN profit-pct[1] = 0.
IF profit-pct[2] EQ ? THEN profit-pct[2] = 0.
IF profit-pct[3] EQ ? THEN profit-pct[3] = 0.


IF show-totals THEN
DO WITH FRAME totals STREAM-IO TITLE '----- TOTALS -----' DOWN COLUMN 5 WIDTH 70:
  taglabel = '      Sales:'.
  DISPLAY
    taglabel NO-LABEL
    cust.sales[gperiod] LABEL 'Period To Date'
    cust.ytd-sales LABEL 'Year To Date'
    cust.lyr-sales LABEL 'Prior Year'.
  DOWN.
  taglabel = '      Costs:'.
  DISPLAY
    taglabel
    cust.cost[1] @ cust.sales[gperiod]
    cust.cost[5] @ cust.ytd-sales
    cust.cost[6] @ cust.lyr-sales.
  DOWN.
  taglabel = '     Profit:'.
  DISPLAY
    taglabel
    profit[1] @ cust.sales[gperiod]
    profit[2] @ cust.ytd-sales
    profit[3] @ cust.lyr-sales.
  DOWN.
  taglabel = ' Profit Pct:'.
  DISPLAY
    taglabel
    profit-pct[1] @ cust.sales[gperiod]
    profit-pct[2] @ cust.ytd-sales
    profit-pct[3] @ cust.lyr-sales.
  DOWN.
  taglabel = 'Commissions:'.
  DISPLAY
    taglabel
    cust.comm[1] @ cust.sales[gperiod]
    cust.comm[5] @ cust.ytd-sales
    cust.comm[6] @ cust.lyr-sales.
  DOWN.
  taglabel = '  Total MSF:'.
  DISPLAY
    taglabel
    cust.ptd-msf[gperiod] @ cust.sales[gperiod]
    cust.ytd-msf @ cust.ytd-sales
    cust.lyytd-msf @ cust.lyr-sales.
  DO WITH FRAME totals2 STREAM-IO COLUMN 5 SIDE-LABELS WIDTH 90:
    DISPLAY
    cust.hibal LABEL 'High Balance' COLON 20
    cust.hibal-date LABEL 'On'
    cust.num-inv LABEL 'Total# of Inv. Paid' SKIP
    cust.lpay COLON 20
    cust.lpay-date LABEL 'On'
    cust.avg-pay LABEL 'Avg# Days to Pay' COLON 69 SKIP
    cust.ord-bal
    cust.acc-bal COLON 69 SKIP
    cust.on-account COLON 20.
  END.
  PUT UNFORMATTED SKIP(1).
END.

{methods/lstlogic/shownote.i &db_table="cust" &col="5" &frame-name="f-notes"}
{methods/lstlogic/showmisc.i &db_table="cust" &col="5" &frame-name="f-miscflds"}

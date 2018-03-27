/***************************************************************************\
*****************************************************************************
**  Program: e:\robj8\patch\ed\O856FDX.P
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}
def var lo_trandate as date no-undo.
def var hi_trandate as date no-undo.
{rc/statline.i}
{rc/viewline.i &displayf="lo_trandate label 'Shipped between' hi_trandate 
    label 'and' " }
DEF var ws_shipper_account      AS char NO-UNDO.
DEF var ws_payment_code         AS char NO-UNDO.
DEF var ws_shipped_by           AS char NO-UNDO.
DEF var ws_master_meter_number  AS char NO-UNDO.
DEF var ws_meter_number         AS char NO-UNDO.
DEF var ws_trans_num            AS int  NO-UNDO initial 1.
/* create Fedex Passport TRANS.IN file from shipping information */
DEF temp-table wk
  FIELD tnum           AS int
  FIELD tcode          AS int
  FIELD tvalue         AS char
  INDEX bytype IS UNIQUE primary tnum tcode.
DEF var ws_num LIKE wk.tnum.
DEF var ws_value AS decimal NO-UNDO.
DEF var ws_service_code AS char NO-UNDO.
ASSIGN
  ws_service_code = "G".
{rc/getdates.i &frame=f-view }
FOR EACH edshtran 
WHERE edshtran.bol-adddate >= lo_trandate
  and edshtran.bol-adddate <= hi_trandate,
    EACH edshline OF edshtran
    BREAK
    BY edshtran.partner
    BY edshtran.seq
    BY edshline.pallet-mark
    BY edshline.carton-mark:
  IF LAST-OF (edshline.carton-mark) THEN
  DO:
    RUN create_package (RECID(edshline)).
  END.
END.    /* for each */
output stream s-export to trans.in append.
for each wk:
    export stream s-export wk.
end.
output stream s-export close.
dos silent notepad "trans.in".
procedure create_package:
DEF INPUT PARAM p_line AS RECID.
FIND edshline WHERE RECID(edshline) = p_line NO-LOCK NO-ERROR.
IF NOT AVAIL edshline THEN
RETURN error.
FIND edshtran OF edshline NO-LOCK.
FIND edshtare OF edshline NO-LOCK.
FIND edshpack OF edshline NO-LOCK.
FIND FIRST edshord
  WHERE edshord.partner = edshline.partner
  AND edshord.seq = edshline.seq
  AND edshord.order-no = edshline.order-no NO-LOCK.
/* if shipto code is blank assume shipto = purchase by 
IF edshtran.st-code = ""
then edshtran.st-code = edshtran.by-code.   
*/
FIND edshipto
  WHERE edshipto.partner = edshtran.partner
  AND edshipto.ref-type = 'st'
  AND edshipto.by-code = edshtran.st-code NO-LOCK NO-ERROR.
IF NOT AVAIL edshipto
  THEN
FIND edshipto
  WHERE edshipto.partner = edshtran.partner
  AND edshipto.ref-type = 'by'
  AND edshipto.by-code = edshtran.st-code NO-LOCK NO-ERROR.
ASSIGN
  ws_num = ws_num + 1
  ws_value = 100.00   /* need an algorithm for this */
  .
RUN create_rec ( 0, '001').                     /* trans type, ship a pack */
RUN create_rec ( 1, edshpack.carton-mark).      /* trans id */
RUN create_rec (10, ws_shipper_account).        /* shipper account */
RUN create_rec (11, edshtran.ship-name).        /* dest name */
RUN create_rec (12,
  IF edshtran.ship-address[3] > ""
  THEN edshtran.ship-address[3]
  ELSE IF AVAIL edshipto
  THEN edshipto.attention
  ELSE "").                                   /* dest contact */
RUN create_rec (13, edshtran.ship-address[1]).  /* dest addr1 */
RUN create_rec (14, edshtran.ship-address[2]).  /* dest addr2 */
RUN create_rec (15, edshtran.ship-city).        /* dest city */
RUN create_rec (16, edshtran.ship-st).       /* dest state */
RUN create_rec (17, edshtran.ship-zip).         /* dest zip code */
RUN create_rec (18,
  IF AVAIL edshipto
  THEN edshipto.phone
  ELSE "").                                   /* dest phone */
RUN create_rec (19, edshtran.partner).          /* dest business code */
RUN create_rec (21, string(edshpack.tot-wght)).  /* package wght */
RUN create_rec (22, ws_service_code).           /* service code */
RUN create_rec (23, ws_payment_code).           /* payment code */
RUN create_rec (24, string(edshtran.ship-date,"99999999")). /* ship date */
RUN create_rec (25,
  "PO#" + edshord.cust-po +
  "/BOL#" + edshord.bol-no +
  "/ORD#" + edshord.order-no).                /* reference */
RUN create_rec (26, string(ws_value)).           /* declared  value */
RUN create_rec (32, ws_shipped_by).             /* shipper contact */
RUN create_rec (38, "").                        /* dept notes */
RUN create_rec (50, edshtran.ship-country).     /* dest country code */
RUN create_rec (116, string(edshpack.tot-cartons)).  /* num of packages */
RUN create_rec (497, ws_master_meter_number).   /* passport meter number */
RUN create_rec (498, ws_meter_number).          /* passport meter number */
RUN create_rec (99, "").                        /* end transaction */
END procedure.  /* create package */
procedure create_rec:
DEF INPUT PARAM p_type      LIKE wk.tcode.
DEF INPUT PARAM p_value     LIKE wk.tvalue.
IF p_value = ? THEN
RETURN error.
CREATE wk.
ASSIGN
  wk.tnum         = ws_num
  wk.tcode        = p_type
  wk.tvalue       = p_value.
END procedure.

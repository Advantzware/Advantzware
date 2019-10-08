/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\RMS\o8563020.p
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: Outbound ASI/RMS Sears Ship Notices
01.15.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Changed assignment of cust-dept in 14 rec; although the edi specs 
are for REF DP with department number, Sears is actually looking for the
division number.  Note that in 3060 there is a REF 19 for division.
2.  When retransmitting ASN's we must create a unique BSN01 ID code.  
Program has been modified to append a non-zero eddoc.c-fatime to the bol-no
to generate this.  e.g. 302-1 on first retransmit, 302-2 on second, etc.
The ed/fmdoc document maintenance program has been modified to increment 
eddoc.c-fatime when Reset is selected at the strip menu.
01.13.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Moved the "10" shipment record in the app out file outside the order loop.
2.  Now use bol-no instead of actual internal order number in "10" rec.
10.17.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Added where clause on edshord of edshtran
to only print lines matching order-no and cust-po under each edshord.
Program was repeating every line on shipment for each order.
2.  Renamed streams such that s-out is the report, s-edi is the edi output
and s-err is spare (new shared by driver) for debugging.
09.05.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Removed write of std-ver in RMS header at column 14.  The translator
    interprets this field as fixed length record length.  Since our records
    are variable length it must be blank.
06.10.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Added report to serve as audit trail for user.
05.28.97 by CAH on ricky@812<rprodev> Log#0000:
1.  Merged the two 12 records into one per sample
2.  Removed zero suppression on quantities.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}
{ed/rms/sharedv.i}
{rc/stringv.i}
{rc/ercvars.i 10 " " new}
DEF STREAM s-edi.
def shared stream s-err.
def shared stream s-out.
def shared frame hdg-std.
def var ws_err_path as char no-undo initial "Printer".
DEF VAR n_recs AS INT NO-UNDO.
def var ws_version as char no-undo.
{ed/getpath.i}
{rc/hdg-wide.i "ed/rms/o8563020.p" "OUTBOUND ASN EDIT LIST" "(s-out)"}
{rc/hdg-noco.i}
assign
{rc/ctrtext.i hdg_name 40}
{rc/ctrtext.i hdg_desc 40}
hdg_text = "".
OUTPUT STREAM s-edi TO VALUE(ws_edi_path) APPEND.
view stream s-out frame hdg-std.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.setid = edcode.setid
    AND eddoc.partner = edcode.partner
    AND eddoc.error-count = 0
    AND eddoc.posted = FALSE
    AND eddoc.direction = edcode.direction
    and not eddoc.status-flag = "DEL":
  display stream s-out
    eddoc.partner
    eddoc.seq
    eddoc.docid
    eddoc.userref
  with frame f-doc side-labels width 144.
  FIND edshtran OF eddoc
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edshtran THEN
  DO:
    {rc/listadd.i erclist 1005}
    erctoken[6] = string(eddoc.seq).
  END.
  else do:
  /* 9705 CAH: Option to suppress ASN's on direct shipment to ordering store*/
  if edmast.asn-on-ds = false then do:
    find first edshline
    where edshline.partner = edshtran.partner
    and   edshline.seq = edshtran.seq
    and   edshline.by-code <> edshtran.st-code no-lock no-error.
    if not avail edshline then do:
        {rc/listadd.i erclist 1006}
    end.
  end.
  end.
  if erclist > '' then do:
    {rc/ercput.i "stream s-out"}
    next.
  end.
  ship_date = {rc/dt2ymd.i edshtran.ship-date}.
  assign
  rms_header_partner = edcode.partner
  rms_header_setid = edcode.setid
  rms_header_std-ver = string(integer(edcode.version),"999999")
  .
  put stream s-edi unformatted
    rms_sep_code                            at 01
    rms_header_partner                      at 06
    rms_header_setid                       at 11
    /* rms_header_std-ver                      at 14
        9708 CAH: Cannot write out the std-ver, interpreted by
            translator as record length */
    skip.
    /* 9801 CAH: Added following code to make BSN unique on retransmission */
    shipment_id = edshtran.bol-no.
    if eddoc.c-fatime > 0 then do:
        shipment_id = shipment_id + "-" + string(eddoc.c-fatime).
    end.    
  PUT STREAM s-edi UNFORMATTED
    "01"                                    AT 01
    shipment_id /* edshtran.bol-no */       FORMAT "x(15)"  AT 45
    SKIP.
  display stream s-out
  EDSHTran.purpose-code
  EDSHTran.BOL-No
  string(eddoc.c-fatime) label "Reset Cnt"
  EDSHTran.BOL-Adddate
  string(EDSHTran.BOL-Addtime,"HH:MM") label "BOL-Addtime"
  EDSHTran.lines
  EDSHTran.sf-code
  EDSHTran.Ship-Date
  EDSHTran.carrier
  EDSHTran.carrier-code
  EDSHTran.Trailer-Number
  EDSHTran.st-code
  EDSHTran.Ship-name
  EDSHTran.Ship-address[1]
  EDSHTran.Ship-address[2]
  EDSHTran.Ship-address[3]
  EDSHTran.Ship-city
  EDSHTran.Ship-st
  EDSHTran.Ship-zip
  EDSHTran.Ship-country
  EDSHTran.routing[1]
  EDSHTran.routing[2]
  EDSHTran.routing[3]
  EDSHTran.routing[4]
  EDSHTran.Pro-Number
  EDSHTran.del-date
  EDSHTran.tot-wght
  EDSHTran.tot-cartons
  EDSHTran.tot-volume
  EDSHTran.Package-Code
  EDSHTran.vn-code
  EDSHTran.Ship-Stat
  with frame f-edshtran 3 columns width 144.
  if edshtran.carrier-code = "" 
  then edshtran.carrier-code = "5014".
  PUT STREAM s-edi UNFORMATTED
      "10"                                        AT 01
      edshtran.tot-cartons    FORMAT "9999999"    AT 09
      edshtran.tot-wght       FORMAT "99999999"   AT 16
      
      /* edshord.order-no        FORMAT "x(07)"      AT 24 9801 CAH */
      edshtran.bol-no         format "x(07)"      at 24
      /* edshord.st-code         FORMAT "x(07)"      AT 31 9801 CAH */
      edshtran.st-code        format "x(07)"      at 31
      ship_date               FORMAT "x(06)"      AT 38
      edshtran.carrier-code   FORMAT "x(04)"      AT 44
      SKIP.
  
  FOR EACH edshord OF edshtran EXCLUSIVE:
    invoice_date = {rc/dt2ymd.i edshord.invoice-date}.
    purchase_order_date = {rc/dt2ymd.i edshord.cust-po-date}.
    PUT STREAM s-edi UNFORMATTED
      "12"                                        AT 01
      edshord.by-code         FORMAT "x(05)"      AT 03
      edshord.tot-cartons     FORMAT "9999999"    AT 08
      edshord.tot-wght        FORMAT "99999999"   AT 15
      edshord.cust-po         FORMAT "x(08)"      AT 23
      purchase_order_date     FORMAT "x(06)"      AT 31
      SKIP.
    PUT STREAM s-edi UNFORMATTED
      "14"                                        AT 01
      edshord.cust-div        FORMAT "x(03)"      AT 03 /* 9801 was -dept */
      .
    display stream s-out
  EDSHOrd.Cust-po
  EDSHOrd.Cust-po-date
  EDSHOrd.By-code
  EDSHOrd.Cust-div
  EDSHOrd.Cust-dept
  EDSHOrd.Lines
  EDSHOrd.Last-line
  EDSHOrd.Order-no
  EDSHOrd.Tot-wght
  EDSHOrd.Tot-cartons
  EDSHOrd.Tot-volume
  EDSHOrd.Package-Code
  EDSHOrd.Ship-Stat
    with frame f-edshord 3 columns column 6 width 144.
    /* 9710 CAH: Program was sending out every line for each order,
        must limit it to matching cust-po and order-no */
    FOR EACH edshline
    where edshline.partner = edshord.partner
    and edshline.seq = edshord.seq
    and edshline.cust-po = edshord.cust-po
    and edshline.order-no = edshord.order-no
    EXCLUSIVE:
      PUT STREAM s-edi UNFORMATTED
        "30"                                        AT 01
        edshline.cust-item-no   FORMAT "x(05)"      AT 05
        integer(edshline.cust-po-line)  FORMAT "99" AT 19
        edshline.qty-shipped    FORMAT "9999999-"   AT 21
        SKIP.
      display stream s-out
  EDSHLine.cust-po-line
  EDSHLine.Item-no
  EDSHLine.Qty-orig-ord
  EDSHLine.qty-shipped
  EDSHLine.unit-price
  EDSHLine.Uom-code
  EDSHLine.pack-size
  EDSHLine.case-wght
  EDSHLine.cust-item-no
  EDSHLine.UPC
  EDSHLine.tot-wght
  EDSHLine.tot-cartons
  EDSHLine.tot-volume
  EDSHLine.Ship-Stat
  EDSHLine.Carton-mark
  EDSHLine.Pallet-mark
      with frame f-edshline 3 columns column 11 width 144.
    END.
  END.  /* each order */
  ASSIGN
    {rc/stampcht.i eddoc}
    eddoc.openitem = FALSE
    eddoc.stat = 9
    eddoc.status-flag = "SNT"
    eddoc.posted = TRUE
    .
  put stream s-out skip(1) "Document header marked: " eddoc.status-flag skip.
END.
OUTPUT STREAM s-edi CLOSE.
RETURN.

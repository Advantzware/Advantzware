/***************************************************************************\
*****************************************************************************
**  Program: D:\RPRODEV\ED\RMS\810-3020.p
**       By: Chris Heins
** Descript: Output 810 to RMS
07.15.98 by CAH on \\ricky\robj8\ Log#0000:
1.  Added edit list report, summary, invoice level.
10.17.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Changed stream use to conform with new standards -
    s-out (shared from driver) is edit list (presently not implemented)
    s-err (shared from driver) is debugging stream
    s-edi (new in this proc)   is for the actual edi output.
09.05.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Removed write of std-ver in RMS header at column 14.  The translator
    interprets this field as fixed length record length.  Since our records
    are variable length it must be blank.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{ed/edivars.i}
{ed/rms/sharedv.i}
def shared stream s-out.
def shared stream s-err.
DEF STREAM s-edi.
DEF VAR n_recs AS INT NO-UNDO.
def var ws_version as char no-undo.
{ed/getpath.i}
OUTPUT STREAM s-edi TO VALUE(ws_edi_path) APPEND.
def var ws_tax  as decimal no-undo.
def var ws_misc as decimal no-undo.
form
with frame f-det down width 144.
FOR EACH eddoc EXCLUSIVE
    WHERE eddoc.setid = edcode.setid
    AND eddoc.partner = edcode.partner
    AND eddoc.error-count = 0
    AND eddoc.posted = FALSE
    AND eddoc.direction = edcode.direction:
  FIND edivtran OF eddoc
    EXCLUSIVE-LOCK NO-ERROR.
  IF NOT AVAIL edivtran THEN
  DO:
    ws_erc = 1.
    RETURN.
  END.
  invoice_date = {rc/dt2ymd.i edivtran.invoice-date}.
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
        9709 CAH: cannot write this out, interpreted as record length by RMS */
    skip.
  PUT STREAM s-edi UNFORMATTED
    "10"                                    AT 1
    edivtran.invoice-no     FORMAT "x(06)"  AT 3
    invoice_date            FORMAT "x(06)"  AT 9
    edivtran.cust-po        FORMAT "x(10)"  AT 15
    SKIP.
  PUT STREAM s-edi UNFORMATTED
    "12"                                    AT 01
    edivtran.cust-div       FORMAT "x(03)"  AT 06
    edivtran.cust-dept      FORMAT "x(03)"  AT 13
    edivtran.vn-code        FORMAT "x(09)"  AT 19
    edivtran.promo-code     FORMAT "x(02)"  AT 28
    SKIP.
  PUT STREAM s-edi UNFORMATTED
    "14"                                    at 01
    edivtran.re-code        FORMAT "x(10)"  AT 03
    edivtran.by-code        FORMAT "x(04)"  AT 13
    SKIP.
  FOR EACH edivline OF edivtran EXCLUSIVE:
    PUT STREAM s-edi UNFORMATTED
      "30"                                        at 01
      edivline.cust-item-no   FORMAT "x(05)"      AT 05
      edivline.qty-shipped    FORMAT "9999999"    AT 19
      edivline.unit-price     FORMAT "->>>.999"   AT 27
      edivline.uom-code       FORMAT "x(02)"      AT 36
      SKIP.
  END.
  n_recs = 0.
  FOR EACH edivaddon OF edivtran EXCLUSIVE:
    {rc/incr.i n_recs}.
    ws_rec_code = 50 + n_recs * 2.
    PUT STREAM s-edi UNFORMATTED
      ws_rec_code             FORMAT "99"         AT 01
      edivaddon.allow-charge  FORMAT "A/C"        AT 03
      edivaddon.hand-meth     FORMAT "x(02)"      AT 04
      edivaddon.amount        FORMAT "->>>>.99"   AT 07
      edivaddon.special-svc-code    FORMAT "x(03)"      AT 16
      edivaddon.description[1] FORMAT "x(29)"     AT 19
      SKIP.
      /* 9807 CAH: Added this code to sum the misc amount ... */
      if hand-meth = "02" then do /* included in the invoice amount */ :
        ws_misc = ws_misc + edivaddon.amount.
      end.
 
  END.
  PUT STREAM s-edi UNFORMATTED
    "80"                                        AT 01
    edivtran.wght-uom       FORMAT "x(02)"      AT 03
    SKIP.
  ASSIGN
    {rc/stampcht.i eddoc}
    eddoc.openitem = FALSE
    eddoc.stat = 9
    eddoc.status-flag = "SNT"
    eddoc.posted = TRUE
    .
    
    display stream s-out
        eddoc.partner               column-label "Partner"
        eddoc.setid                 column-label "Set"
        eddoc.seq                   column-label "Seq#"
        edivtran.invoice-no (count)
        edivtran.invoice-date       column-label "Date"
        edivtran.cust-po            column-label "Customer PO#"
        edivtran.tot-gross (total)  column-label "Amount"
        ws_tax (total)              column-label "Tax"
        edivtran.tot-frt (total)    column-label "Freight"
        ws_misc (total)             column-label "Misc."
    with frame f-det down.
    down stream s-out with frame f-det.        
END.
OUTPUT STREAM s-edi CLOSE.
RETURN.

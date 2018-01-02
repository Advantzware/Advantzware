/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\RPMAST.P
**       By: Chris Heins, Report Concepts, Inc. (c) 1997 All Rights Reserved
** Descript: Print Trading partners and associated files
12.21.97 by CAH on \\ricky\robj8\ Log#0000:
1.  Added file-list and proc-order.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/hdg-wide.i "ed/rpmast.p" "TRADING PARTNER REFERENCE LIST"}
DEF VAR view_shiptos AS LOGICAL NO-UNDO INITIAL FALSE
  LABEL "Print Shipto's?".
DEF VAR save_partner LIKE ws_partner NO-UNDO.
DEF VAR ws_path AS CHAR NO-UNDO FORMAT "x(40)" LABEL "Path".
DEF var details AS logical NO-UNDO LABEL "Details".
DEF var byzone AS logical NO-UNDO LABEL "By DC?".
{rc/statline.i}
{rc/viewline.i &displayf=
  "ws_partner
    help 'Enter specific partner, *=ALL or match pattern, HELP=Lookup'
 view_shiptos
    help 'Enter YES to include shipto addresses, NO to suppress'
 details
    help 'Enter YES to print shipto details, NO for summary listing'
 byzone
    help 'Enter YES to print by distribution center' "}
save_partner = ws_partner.
DO WITH FRAME f-view:
  UPDATE ws_partner view_shiptos.
  ws_partner = CAPS(ws_partner).
  IF ws_partner = "" THEN
  ws_partner = "*".
  DISPLAY ws_partner.
  FIND FIRST edmast WHERE edmast.partner MATCHES ws_partner NO-LOCK NO-ERROR.
  IF NOT AVAIL edmast
  THEN
  DO:
    BELL.
    MESSAGE COLOR VALUE(c_err)
    "No EDMast partners match your entry - try again".
    UNDO, RETRY.
  END.
  IF view_shiptos THEN
  UPDATE details byzone.
END.
{rc/getprint.i}
{rc/hdg-noco.i}
hdg_text = IF view_shiptos THEN "With Shiptos"
ELSE ""
.
ASSIGN
{rc/ctrtext.i hdg_name 40}
{rc/ctrtext.i hdg_desc 40}
{rc/ctrtext.i hdg_text 40}.
VIEW FRAME hdg-std.
FOR EACH EDMAST NO-LOCK
  WHERE PARTNER MATCHES ws_partner:
  DISPLAY
  EDMast.Partner
  EDMast.Cust
  EDMast.Vendor
  EDMast.User-name
  EDMast.We-cust
  EDMast.We-vend-no
  EDMast.Seq
  EDMast.Id-trim
  EDMast.ID-Len
  EDMast.ID-Out
  WITH FRAME f-mast 1 DOWN WIDTH 132.
  /* 9706 CAH: This was missing from prior release ... */
  DISPLAY
  EDMast.RE-code  AT 9
  EDMast.Sf-code
  EDMast.Order-no-mask
  EDMast.Ship-to-mask
  EDMast.ASN-on-DS
  EDMast.Del-Days
  EDMast.Item-Length
  EDMast.Item-Prefix
  EDMast.Item-suffix
  WITH FRAME f-mast2 1 DOWN width 132.
  DISPLAY
  EDMast.Path-in COLON 11
  EDMast.Path-out COLON 11
  WITH FRAME f-mast-path SIDE-LABELS.
  /* 06/10/97 FORM LIKE file EDMast */
  FORM
  EDMast.Partner
  EDMast.Cust
  EDMast.Vendor
  EDMast.User-name
  EDMast.We-cust
  EDMast.We-vend-no
  EDMast.Seq
  EDMast.Path-in
  EDMast.Path-out
  EDMast.Id-trim
  EDMast.ID-Len
  EDMast.ID-Out
  WITH FRAME edmast NO-LABELS.
  FOR EACH EDCODE OF EDMAST NO-LOCK:
    IF direction = "O" THEN
    ws_path = path-out.
    ELSE
    ws_path = path-in.
    DISPLAY
    EDCode.Direction AT 4
    EDCode.SetID
    EDCode.Customized
    EDCode.Custom-proc
    EDCode.Agency
    EDCode.Version
    EDCode.Test-prod
    /* edcode.proc-order */
    ws_path
    WITH FRAME F-CODE DOWN WIDTH 144.
    /*
    IF edcode.file-list > "" THEN
    DO:
      DOWN WITH FRAME f-code.
      DISPLAY file-list @ ws_path.
    END.
    */
  END.
  IF view_shiptos THEN
  DO:
    FOR EACH EDSHIPTO OF EDMAST NO-LOCK
      BREAK BY edshipto.partner
      BY (IF byzone THEN edshipto.dest-zone ELSE "1")
      BY edshipto.state BY edshipto.zip:
      IF byzone AND FIRST-OF (IF byzone THEN edshipto.dest-zone ELSE "1")
      THEN
      DO:
        DISPLAY dest-zone LABEL "Delivery Zone/DC"
        WITH FRAME f-zone side-labels.
      END.
      IF details THEN
      DO:
        DISPLAY
        EDShipto.Ref-type             COLON 11
        EDShipto.Name               COLON 41
        EDShipto.Description    COLON 91
        EDShipto.By-code              COLON 11
        EDShipto.Addr1              COLON 41
        EDShipto.Comments[1]    AT 81 NO-LABEL
        EDShipto.St-code              COLON 11
        EDShipto.Addr2              COLON 41
        EDShipto.Comments[2]    AT 81 NO-LABEL
        EDShipto.Cust                 COLON 11
        EDShipto.City               COLON 41
        EDShipto.State              NO-LABEL
        EDShipto.Zip                NO-LABEL
        EDShipto.Comments[3]    AT 81 NO-LABEL
        EDShipto.Ship-to              COLON 11
        EDShipto.Attention          COLON 41  FORMAT "x(25)"
        EDShipto.Country            NO-LABEL
        EDShipto.Comments[4]    AT 81 NO-LABEL
        EDShipto.Cust-Region          COLON 11 LABEL "Cust-Reg"
        EDShipto.Phone              COLON 41
        EDShipto.Comments[5]    AT 81 NO-LABEL
        EDShipto.Dest-Zone            COLON 11
        EDShipto.Fax                COLON 41
        EDShipto.Opened   format "99/99/9999"      COLON 91
        WITH FRAME f-shipto SIDE-LABELS WIDTH 144.
      END.
      ELSE
      IF NOT details THEN
      DO:
        DISPLAY
        edshipto.ref-type
        edshipto.by-code  FORMAT 'x(06)'
        edshipto.st-code  FORMAT 'x(06)'
        edshipto.name
        edshipto.addr1
        edshipto.city
        edshipto.state
        edshipto.zip
        WITH FRAME f-shipsum DOWN width 144.
        IF addr2 > "" THEN
        DO:
          DOWN WITH FRAME f-shipsum.
          DISPLAY edshipto.addr2 @ edshipto.addr1
          WITH FRAME f-shipsum.
        END.
        DOWN WITH FRAME f-shipsum.
      END.  /* not details */
    END.  /* for each */
  END.  /* if */
  FOR EACH edicxref OF edmast NO-LOCK:
    DISPLAY
    EDICXref.Cust-item-no AT 4
    EDICXref.Item-no
    EDICXref.Company
    WITH FRAME f-ic DOWN.
  END.
END.
{rc/endprint.i}
ws_partner = save_partner.

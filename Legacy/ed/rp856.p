/***************************************************************************\
*****************************************************************************
**  Program: E:\RPRODEV\ED\RP856.P
**       By: Chris Heins (c) 1997 Report Concepts, Inc.
** Descript: Print ASN
10.17.97 by CAH on ricky@812<asi/fold/rco35> Log#0000:
1.  Added selection of partner, date range, and unposted status.
2.  Reformatted as 3 columns, removed some repeating fields, etc.
**
*****************************************************************************
\***************************************************************************/
{ed/sharedv.i}
{rc/hdg-std.i "ed/rp856.p" "ADVANCE SHIP NOTICE EDIT LIST"}
DEF VAR view_unposted AS LOGICAL NO-UNDO INITIAL TRUE LABEL "Unposted Only?".
DEF VAR view_partner LIKE ws_partner NO-UNDO.
DEF VAR lo_adddate AS DATE NO-UNDO LABEL "Created From".
DEF VAR hi_adddate AS DATE NO-UNDO LABEL "Thru".
{rc/statline.i}
{rc/viewline.i &displayf="view_partner
	help 'Enter specific partner code or * for ALL, HELP for pick list'
	validate(input frame f-view view_partner = '*'
	    or can-find(first edmast
	where edmast.partner = input frame f-view view_partner),
	'There are no partners which match your entry')
    view_unposted
	help 'Enter YES to print only unposted entries, NO for ALL'
    lo_adddate
	help 'Enter earliest create date or ? for any'
    hi_adddate
	help 'Enter most recent create date or ? for any'
    "}
view_partner = IF ws_partner > "" THEN
ws_partner ELSE "*"
.
DO WITH FRAME f-view:
  UPDATE
    view_partner
    view_unposted
    lo_adddate
    hi_adddate.
  IF view_partner = ""
    OR view_partner = ?
    THEN
  view_partner = "*".
END.
{rc/getprint.i}
{rc/hdg-noco.i}
VIEW FRAME hdg-std.
FOR EACH eddoc NO-LOCK
    WHERE eddoc.fgid = "SH"
    AND eddoc.partner MATCHES view_partner
    AND (IF lo_adddate = ? THEN TRUE ELSE eddoc.adddate >= lo_adddate)
    AND (IF hi_adddate = ? THEN TRUE ELSE eddoc.adddate <= hi_adddate)
    AND (IF view_unposted THEN eddoc.posted = FALSE ELSE TRUE)
    :
  DISPLAY eddoc WITH FRAME f-doc 3 COLUMN width 144
    TITLE "EDDOC DOCUMENT".
  FIND edshtran OF eddoc NO-LOCK.
  DISPLAY edshtran except partner seq
  WITH FRAME f-tran 3 COLUMN TITLE "EDSHTRAN HEADER" width 144.
  FOR EACH EDSHORD OF EDSHTRAN:
    DISPLAY edshord
    except partner seq
    WITH FRAME f-ord 3 COLUMN TITLE "EDSHORD ORDER" width 144.
    FOR EACH edshline NO-LOCK
    where edshline.partner = edshord.partner
      and edshline.seq = edshord.seq
      and edshline.cust-po = edshord.cust-po
      and edshline.order-no = edshord.order-no
    BREAK BY edshline.pallet-mark BY edshline.carton-mark:
      IF FIRST-OF (edshline.pallet-mark) THEN
      DO:
	FIND edshtare OF edshline NO-LOCK NO-ERROR.
	IF AVAIL edshtare THEN
	DO:
	  DISPLAY edshtare
	  except partner seq
	    WITH FRAME f-tare 1 down TITLE "EDSHTARE PALLET" width 144.
	END.
      END.
      IF FIRST-OF (edshline.carton-mark) THEN
      DO:
	FIND edshpack OF edshline NO-LOCK NO-ERROR.
	IF AVAIL edshpack THEN
	DO:
	  DISPLAY edshpack
	    except partner seq
	    WITH FRAME f-pack 1 down TITLE "EDSHPACK PACK" width 144.
	END.
      END.
      DISPLAY edshline
	except partner seq
	WITH 3 COLUMNs FRAME f-line TITLE "EDSHLINE ITEM" width 144.
    END.
  END.
END.
{rc/endprint.i}

/* ------------------------------------------------- oe/oe-frtcl.p 10/96 fwk  */
/* Calculate Freight for Order based on Order Header Customer                 */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}

DEF shared buffer xoe-ord for oe-ord.
DEF buffer frt-oe-ord for oe-ord.
DEF buffer frt-oe-ordl for oe-ordl.
DEF VAR ws_tot LIKE oe-ord.t-freight.
DEF VAR ws_delzone LIKE carr-mtx.del-zone NO-UNDO.
DEF VAR ws_delzip  LIKE carr-mtx.del-zip  NO-UNDO.
DEF VAR ws_carrier  LIKE oe-ord.carrier  NO-UNDO.
DEF VAR ws_loc  LIKE oe-ord.loc  NO-UNDO.
DEF VAR ws_minimum AS LOGICAL NO-UNDO.
DEF VAR ws_cwt-tmp AS DECIMAL NO-UNDO.
DEF VAR mtx_rec AS RECID NO-UNDO.
DEF VAR v-pallet AS DEC NO-UNDO.
DEF VAR v-msf AS DEC NO-UNDO.


RUN oe/ordfrate.p (ROWID(xoe-ord)).
/*
find first frt-oe-ord where RECID(frt-oe-ord) = RECID(xoe-ord) no-error.

assign  frt-oe-ord.t-freight = 0
	frt-oe-ord.t-weight = 0.
for each frt-oe-ordl of frt-oe-ord /*no-lock*/ :
  find first oe-rel where oe-rel.company = cocode and
			  oe-rel.ord-no = frt-oe-ord.ord-no and
			  oe-rel.i-no = frt-oe-ordl.i-no 
			  use-index ord-item no-lock no-error.
  if avail oe-rel then
    find first shipto where shipto.company = cocode and 
			    shipto.cust-no = frt-oe-ord.cust-no and
			    shipto.ship-id = oe-rel.ship-id no-lock no-error.
  ELSE 
    find first cust where cust.company = cocode and
	    cust.cust-no = frt-oe-ord.cust-no no-lock no-error.

     if avail shipto then
      assign ws_delzone = shipto.dest-code
	     ws_delzip = shipto.ship-zip
	     ws_carrier = shipto.carrier
	     ws_loc = shipto.loc.
    else
      assign ws_delzone = cust.del-zone
	     ws_delzip = ''
	     ws_carrier = frt-oe-ord.carrier
	     ws_loc = frt-oe-ord.loc.
 
  /*  only for weight 
      RUN oe/oeccfrt.p (INPUT
	frt-oe-ord.company,
	ws_loc,
	ws_carrier,
	ws_delzone,
	ws_delzip,
	frt-oe-ordl.t-weight,
	INPUT-OUTPUT ws_tot,
	OUTPUT ws_cwt-tmp,
	OUTPUT ws_minimum,    
	OUTPUT mtx_rec). */

      v-pallet = frt-oe-ordl.qty / frt-oe-ordl.cas-cnt.
      IF v-pallet = ? THEN v-pallet = 0.
      {sys/inc/roundup.i v-pallet}  
      FIND FIRST itemfg WHERE itemfg.company = frt-oe-ord.company
                          AND itemfg.i-no = frt-oe-ordl.i-no NO-LOCK NO-ERROR.
      v-msf = IF AVAIL itemfg THEN itemfg.t-sqft * 1000 ELSE 0.

      RUN oe/oeccfrt2.p (INPUT frt-oe-ord.company,
	                     ws_loc,
	                     ws_carrier,
	                     ws_delzone,
	                     ws_delzip,
	                     frt-oe-ordl.t-weight,
                         v-pallet,
                         v-msf,
	                     INPUT-OUTPUT ws_tot,
	                     OUTPUT ws_cwt-tmp,
	                     OUTPUT ws_minimum,
	                     OUTPUT mtx_rec).
      assign frt-oe-ord.t-freight = frt-oe-ord.t-freight + ws_tot
	         frt-oe-ord.t-weight = frt-oe-ord.t-weight + frt-oe-ordl.t-weight
             frt-oe-ordl.t-freight = ws_tot.

end.

 
/*
find first cust where cust.company = cocode and
	    cust.cust-no = xoe-ord.cust-no no-lock no-error.
if avail cust then
  ASSIGN ws_delzone = cust.del-zone.
	 ws_delzip = ''.

  RUN oe/oeccfrt.p (INPUT
	xoe-ord.company,
	xoe-ord.loc,
	xoe-ord.carrier,
	ws_delzone,
	ws_delzip,
	xoe-ord.t-weight,
	INPUT-OUTPUT xoe-ord.t-freight,
	OUTPUT ws_cwt-tmp,
	OUTPUT ws_minimum,
	OUTPUT mtx_rec).
*/
*/
/* end ---------------------------------- copr. 1993  advanced software, inc. */

/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2012 Tom Bascom, Greenfield Technologies                  **
 **  http://www.greenfieldtech.com                                            **
 **                                                                           **
 **  ProTop is free software; you can redistribute it and/or modify it        **
 **  under the terms of the GNU General Public License (GPL) as published     **
 **  by the Free Software Foundation; either version 2 of the License, or     **
 **  at your option) any later version.                                       **
 **                                                                           **
 **  ProTop is distributed in the hope that it will be useful, but WITHOUT    **
 **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or    **
 **  FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License     **
 **  for more details.                                                        **
 **                                                                           **
 **  See TERMS.TXT for more information regarding the Terms and Conditions    **
 **  of use and alternative licensing options for this software.              **
 **                                                                           **
 **  A copy of the GPL is in GPL.TXT which was provided with this package.    **
 **                                                                           **
 **  See http://www.fsf.org for more information about the GPL.               **
 **                                                                           **
 **                                                                           **
 *******************************************************************************
 *******************************************************************************
 *
 * bigb.p
 *
 *
 * Make a stab a guesstimating the effects of changing -B.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 * History:
 *
 *	Gus suggested that this (based on an old PEG post) would make an interesting module.
 *	September 30, 2003
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "BigBGuesstimator".

define temp-table tt_bigb no-undo
  field xid     as integer   format       ">>>>>>9"
  field xpct    as decimal   format       ">>>>>>9%"       label "Pct"
  field bigb    as decimal   format ">>>>>>>>>>>>9"        label "BigB (-B)"
  field bigbGB  as decimal   format     ">>>>>>>>9.999 GB" label "BigB (GB)"
  field dbSize  as decimal   format    ">>>>>>>>>9 GB"     label "DB Size (GB)"
  field db-pct  as decimal   format     ">>>>>>>>9.999%"   label "% DB Size"
  field hr      as decimal   format ">>>>>>>>>>>>9"        label "Hit:1"
  field mpct    as decimal   format         ">>>>9.99999%" label "Miss%"
  field hpct    as decimal   format         ">>>>9.99999%" label "Hit%"
  field osr     as decimal   format     ">>>>>>>>9"        label "OS Reads"
  field xnote   as character format "x(36)"                label "Note"
  field lrx     as decimal extent 5 {&NOSERIALIZE}
  field osrx    as decimal extent 5 {&NOSERIALIZE}

  index xpct-idx is unique primary xpct
  index xid-idx  is unique xid
.

{lib/dumpTT.i tt_bigb}

define variable dbBlkSize as integer no-undo.

/* Predict Big B
 *
 * Based on the calculation in this thread:
 *
 *	http://www.peg.com/lists/dba/history/200301/msg00509.html
 *
 * which originates in a 1998 posting (by me) referring to some interesting
 * research published by IBM.  The formula used here is the one derived by
 * Tim Casey:
 *
 * 	m2 = m1 * sqrt( b1 / b2 )
 *
 * This results in a simple calculation of the expected impact on OS reads
 * which is generally the ultimate goal of tuning -B.
 *
 * This is not an exact calculation -- there should be a locally calibrated
 * constant applied to  ( b1 / b2 ).  The value is probably between 0.5 & 1.5
 * but these results are, IMHO, close enough to give an idea of what to expect
 * plus or minus a bit (maybe 25%).
 *
 * It should also be noted that as increases in -B reduce OS reads you can
 * expect to see a possibly significant increase in logical reads since
 * less time will be spent waiting for OS reads -- this will impact the
 * OS reads in a recursive manner...
 *
 * You will get better results in you sample for a longer period.  10 second
 * samples (for instance) can have quite a lot of variation from sample to
 * sample.  I'd try at least 60 seconds for starters.
 *
 */

procedure predict-b:

  define input parameter p_pct  as decimal no-undo.
  define input parameter p_buf  as integer no-undo.
  define input parameter p_lrx  as decimal no-undo.
  define input parameter p_osrx as decimal no-undo.
  define input parameter p_used as integer no-undo.

/*
  define variable x as integer no-undo.
  define variable z as integer no-undo.
 */

  find tt_bigb where tt_bigb.xpct = p_pct no-error.

  if available tt_bigb then
    assign
      tt_bigb.lrx[3]  = p_lrx
      tt_bigb.osrx[3] = p_osrx
    .
   else
    do:
      create tt_bigb.
      assign
        tt_bigb.xid  = integer( p_pct )				/* % of current Big B	*/
        tt_bigb.xpct = p_pct					/* % of current Big B	*/
        tt_bigb.bigb = p_buf * ( tt_bigb.xpct / 100 )		/* Modified Big B	*/
        tt_bigb.bigbGB = ( tt_bigb.bigb * dbBlkSize ) / ( 1024 * 1024 * 1024 )
        {lib/init-xrec.i tt_bigb.lrx  p_lrx}
        {lib/init-xrec.i tt_bigb.osrx p_osrx}
      .
    end.

  assign

    {lib/upd-xrec.i tt_bigb.lrx  tt_bigb.lrx[3]}		/* Logical Reads	*/
    {lib/upd-xrec.i tt_bigb.osrx tt_bigb.osrx[3]} 		/* OS Reads		*/

    tt_bigb.mpct =						/* Predicted Miss %	*/
      ( if tt_bigb.bigb > p_used then 0 else				/* it all fits in -B	*/
      100 * exp(( p_buf / tt_bigb.bigb ), 0.5 ) *			/* sqrt( OldB / NewB )	*/
      ( 1 - (( tt_bigb.lrx[x] - tt_bigb.osrx[x] ) / tt_bigb.lrx[x] )))	/* Current Miss %	*/

    tt_bigb.hpct = 100 - tt_bigb.mpct				/* Predicted Hit %	*/
    tt_bigb.osr  = tt_bigb.lrx[x] * ( tt_bigb.mpct / 100 )	/* Predicted OS Reads	*/
    tt_bigb.hr   = tt_bigb.lrx[x] / tt_bigb.osr			/* Predicted Hit Ratio	*/
    tt_bigb.db-pct = ( tt_bigb.bigb / p_used ) * 100.
    tt_bigb.hr = ( if tt_bigb.hr = ? then 99999999 else tt_bigb.hr )
  .

  return.

end.


procedure mon-init:

  empty temp-table tt_bigb.

  find dictdb._DbStatus no-lock.
  dbBlkSize = _dbStatus-dbBlkSize.
  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable u-pct as decimal no-undo.
  define variable bfree as integer no-undo.
  define variable used  as integer no-undo.

  define variable i     as integer no-undo.
  define variable b_pct as integer no-undo extent 12
    initial [ 10, 25, 50, 100, 150, 200, 400, 800, 1000, 2000, 5000, 10000 ].

  run updTick.

  find first dictdb._BuffStatus no-lock.
  find first dictdb._ActBuffer  no-lock.

  /* How many blocks is this database using?
   *
   * This is a quick & dirty calculation -- maybe it should be better?
   *
   */

  for each dictdb._AreaStatus no-lock:
    if ( _AreaStatus-Freenum = ? ) then
      bfree = _AreaStatus-Totblocks - _AreaStatus-Hiwater.
     else
      bfree = _AreaStatus-Totblocks - _AreaStatus-Hiwater + _AreaStatus-Freenum.
    if bfree = ? then bfree = _AreaStatus-totblocks.
    used = used + ( _AreaStatus-totblocks - bfree ).
  end.

  do i = 1 to 12:
    run predict-b( b_pct[i], ( _BuffStatus._BfStatus-TotBufs - 2 ), _ActBuffer._Buffer-LogicRds, _ActBuffer._Buffer-OSRds, used ).
  end.

  /*** if we don't RELEASE this the summary screen fails to find it (which makes NO sense -- it's a FIND NO-LOCK...)
   ***
   ***/

  release dictdb._ActBuffer.

  for each tt_bigb no-lock by tt_bigb.xpct:

    tt_bigb.osr = ( tt_bigb.osr / z ).
    tt_bigb.dbSize = ( used * dbBlkSize ) / ( 1024 * 1024 * 1024 ).
    if tt_bigb.xpct = 100 then tt_bigb.xnote = "<== Current -B".

  end.

  add2ds( temp-table tt_bigb:default-buffer-handle ).

  return.

end.

return.

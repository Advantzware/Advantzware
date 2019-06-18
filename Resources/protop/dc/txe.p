/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2006 Tom Bascom, Greenfield Technologies                  **
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
 * txe.p
 *
 *
 * TXE Waits.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	January 8, 2007
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "TXE".

define variable X_TXE-Type as character no-undo extent 9.

{lib/tt_xstat.i}

/* if changes are made to formatting the column offsets in lib/browser.p may need to change */

define temp-table tt_txe no-undo

  field xid          as integer
  field txeType      as character format "x(9)"           label "TXE Type"	/* "x(14)"		*/
  field txeLocks     as decimal   format "->>>>>9"        label "Locks"		/* "->>>>>>>>9"		*/
  field txeWaits     as decimal   format "->>>>>9"        label "Waits"
  field txeLockPct   as decimal   format "->9.9%"         label "Lock%"		/* "->9.99%"		*/
  field concurrLkPct as decimal   format "->9.9%"         label "ConLk%"	/* "->9.99%"		*/
  field concurrLk    as decimal   format "->>>>>"         label "ConLk"
  field concurrWt    as decimal   format "->>>>"          label "ConWt"

  index xid-idx is unique xid
  index txeWaits-idx is primary txeWaits descending
  index txeLocks-idx txeLocks descending
  index txeLockPct-idx txeLockPct descending
.

{lib/dumpTT.i tt_txe}

/* initialize
 *
 */

procedure mon-init:

  define variable i as integer no-undo.

  empty temp-table tt_xstat.

  find first _TxeLock no-lock no-error.

  do i = 1 to 9:
    create tt_xstat.
    assign
      tt_xstat.xid   = i
      tt_xstat.xname = _TxeLock._Txe-Type[i]
      X_TXE-Type[i]  = _TxeLock._Txe-Type[i]
    .
    if _Txe-Type[i] = "" then X_TXE-Type[i] = "TXE[" + string( i ) + "]".
  end.

  run updTick.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable tlist as character no-undo.
  define variable xlist as character no-undo.
  define variable i     as integer   no-undo.

  run updTick.

  find first _TxeLock no-lock no-error.

  do i = 1 to 9:

    run update_xstat (
      input i,
      input X_Txe-Type[i],
      input "m1",
      input "m2",
      input "m3",
      input "m4",
      input "m5",
      input _Txe-Locks[i],
      input ( if _Txe-LocksS[i] = ? then 0 else _Txe-LocksS[i] ),
      input _Txe-Waits[i],
      input ( if _Txe-WaitsS[i] = ? then 0 else _Txe-WaitsS[i] ),
      input 0,
      input 0
    ).

  end.

  run age_xstat.

  for each tt_xstat:

    find tt_txe where tt_txe.xid = tt_xstat.xid no-error.
    if not available tt_txe then create tt_txe.
    assign
      tt_txe.xid          = tt_xstat.xid
      tt_txe.txeType      = tt_xstat.xname
      tt_txe.txeLocks     = ( tt_xstat.stat1[x] / z )
      tt_txe.txeWaits     = ( tt_xstat.stat3[x] / z )
/*    tt_txe.txeLockPct   = tt_xstat.stat-ratio */
      tt_txe.txeLockPct   = (( tt_txe.txeLocks - tt_txe.txeWaits ) / tt_txe.txeLocks ) * 100
      tt_txe.concurrLk    = ( tt_xstat.stat2[x] / z )
      tt_txe.concurrWt    = ( tt_xstat.stat4[x] / z )
      tt_txe.concurrLkPct = (( tt_txe.concurrLk - tt_txe.concurrWt ) / tt_txe.concurrLk ) * 100
    .

  end.

  add2ds( temp-table tt_txe:default-buffer-handle ).

  return.

end.

return.

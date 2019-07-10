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
 * latch.p
 *
 *
 * Latch Waits.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "LatchActivity".

{lib/tt_xstat.i}

/* if changes are made to formatting the column offsets in lib/browser.p may need to change */

define temp-table tt_latch no-undo
  field xid        as integer   format ">>9"             label "Id"
  field latchName  as character format "x(5)"            label "Latch"
  field latchType  as character format "x(5)"            label "Type"  {&NOSERIALIZE}
  field holder     as integer   format "->>>9"           label "Hlder"
  field QHolder    as integer   format "->>>9"           label "QHold" {&NOSERIALIZE}
  field latchReq   as integer   format "->>>>>>>>>9"     label "Requests"
  field latchWts   as integer   format "->>>>>>9"        label "Waits"
  field latchLkPct as decimal   format "->9.99%"         label "Lock%"

  index latchWts-idx   is unique primary latchWts descending latchReq descending xid
  index latchReq-idx   is unique latchReq descending xid
  index latchLkPct-idx is unique latchLkPct descending xid
  index latchName-idx  is unique latchName
  index xid-idx        is unique xid
.

{lib/dumpTT.i tt_latch}

procedure mon-init:

  empty temp-table tt_xstat.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  run updTick.

  for each dictdb._Latch where _Latch-Id <> ? no-lock:

    run update_xstat (
      input _Latch-Id,
      input ( if length( _Latch-name ) >= 5 then substring( _Latch-name, 5 ) else _Latch-name ),
      input ( if _Latch-Type = "MT_LT_SPIN" then "Spin" else ( if _Latch-Type = "MT_LT_QUEUE" then "Queue" else "?" )),
      input "m2",
      input "m3",
      input "m4",
      input "m5",
      input _Latch-lock,
      input _Latch-wait,
      input _Latch-Hold,
      input _Latch-QHold,
      input 0,
      input 0
    ).

  end.

  run age_xstat.

  empty temp-table tt_latch.

  for each tt_xstat no-lock by tt_xstat.stat1[x] descending:

    create tt_latch.
    assign
      tt_latch.xid        = tt_xstat.xid
      tt_latch.latchName  = tt_xstat.xname
      tt_latch.latchType  = tt_xstat.misc1
      tt_latch.holder     = tt_xstat.stat3[3]
      tt_latch.QHolder    = tt_xstat.stat4[3]
      tt_latch.latchReq   = tt_xstat.stat1[x] / z
      tt_latch.latchWts   = tt_xstat.stat2[x] / z
/*    tt_latch.latchLkPct = tt_xstat.stat-ratio  */
      tt_latch.latchLkPct = (( tt_latch.latchReq - tt_latch.latchWts ) / tt_latch.latchReq ) * 100
    no-error.

  end.

  add2ds( temp-table tt_latch:default-buffer-handle ).

  return.

end.

{ssg/latch.i}

return.

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
 * resrc.p
 *
 *
 * Resource Waits.
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

define output parameter dcDescription as character no-undo initial "ResourceWaits".

{lib/tt_xstat.i}

/* if changes are made to formatting the column offsets in lib/browser.p may need to change */

define temp-table tt_resrc no-undo
  field xid        as integer   format ">>9"             label "Id"
  field resrcName  as character format "x(14)"           label "Resource"
  field resrcReq   as integer   format "->>>>>>>>>9"     label "Requests"
  field resrcWts   as integer   format "->>>>>>9"        label "Waits"
  field resrcLkPct as decimal   format "->9.99%"         label "Lock%"

  index resrcWts-idx   is unique primary resrcWts descending resrcReq descending xid
  index resrcReq-idx   is unique resrcReq descending xid
  index resrcLkPct-idx is unique resrcLkPct descending xid
  index resrcName-idx  is unique resrcName
  index xid-idx        is unique xid
.

{lib/dumpTT.i tt_resrc}

procedure mon-init:

  empty temp-table tt_xstat.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  run updTick.

  for each dictdb._Resrc no-lock:

    run update_xstat (
      input _Resrc-Id,
      input _Resrc-name,
      input "m1",
      input "m2",
      input "m3",
      input "m4",
      input "m5",
      input _Resrc-lock,
      input _Resrc-wait,
      input 0,
      input 0,
      input 0,
      input 0
    ).

  end.

  run age_xstat.

  empty temp-table tt_resrc.

  for each tt_xstat no-lock by tt_xstat.stat1[x] descending:

    create tt_resrc.
    assign
      tt_resrc.xid        = tt_xstat.xid
      tt_resrc.resrcName  = tt_xstat.xname
      tt_resrc.resrcReq   = tt_xstat.stat1[x] / z
      tt_resrc.resrcWts   = tt_xstat.stat2[x] / z
/*    tt_resrc.resrcLkPct = tt_xstat.stat-ratio */
      tt_resrc.resrcLkPct = (( tt_resrc.resrcReq - tt_resrc.resrcWts ) / tt_resrc.resrcReq ) * 100
    no-error.

  end.

  add2ds( temp-table tt_resrc:default-buffer-handle ).

  return.

end.

{ssg/resrc.i}

return.

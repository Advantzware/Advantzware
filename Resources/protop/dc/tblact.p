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
 * tblact.p
 *
 *
 * Table Activity
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

define output parameter dcDescription as character no-undo initial "TableActivity".

define variable hasOSRd    as logical   no-undo.

{lib/tt_table.i}

define temp-table tt_tblAct no-undo
  field xid        as integer   format "->>>>9"
  field tblNum     as integer   format "->>>>9"          label "Tbl#"
  field tblAreaNum as integer   format ">>>>9"           label "Area#"
  field tblPool    as character format "x(2)"            label "BX"
  field tblName    as character format "x(30)"           label "Table Name"
  field dbaRM      as decimal   format ">>>>>>>>>>>>"    label "RM Chain"
  field dbaRecs    as decimal   format ">>>>>>>>>>>>"    label "#Records"
  field dbaFragPct as decimal   format ">>9.99%"         label "Frag%"
  field dbaScatter as decimal   format ">>>>"            label "Scat"
  field tblTurn    as decimal   format ">>>>>>9.99"      label "Churn"
  field dbaAvgRow  as decimal   format ">>>>>>>"         label "AvgRow"
  field tblCr      as decimal   format ">>>>>>>>9"       label "Create"
  field tblRd      as decimal   format ">>>>>>>>>9"      label "Read"
  field tblUp      as decimal   format ">>>>>>>>9"       label "Update"
  field tblDl      as decimal   format ">>>>>>>>9"       label "Delete"
  field tblOSRd    as decimal   format ">>>>>>>>9"       label "OS Read"

  index tblRd-idx  is primary tblRd descending
  index tblNum-idx is unique tblNum
  index tblAreaNum-idx tblAreaNum descending
  index dbaRecs-idx dbaRecs descending
  index tblTurn-idx tblTurn descending
  index tblCr-idx   tblCr   descending
  index tblUp-idx   tblUp   descending
  index tblDl-idx   tblDl   descending
  index tblOSRd-idx tblOSRd descending
  index tblPool-idx tblPool descending
  index tblname-idx is unique tblName
  index xid-idx  is unique xid
.

{lib/dumpTT.i tt_tblAct}

define new global shared variable rowLimit as integer no-undo.

procedure chkOSRd:
  hasOSRd = no.
  find _file no-lock where _file._file-name = "_tablestat" no-error.
  if available _file then find _field no-lock of _file where _field._field-name = "_tablestat-osread" no-error.
  hasOSRd = available( _field ).
  return.
end.

procedure mon-init:

  run chkOSRd.

  empty temp-table tt_table.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i        as integer no-undo.
  define variable topLimit as integer no-undo initial 25.				/* formerly 10				*/

  run updTick.

  for each dictdb._TableStat no-lock:
    find tt_tbl no-lock where tt_tbl.xid = _TableStat-id no-error.			/* cached _file records			*/
    if available tt_tbl then run upd-tt_table ( _TableStat._TableStat-id ).
  end.

  /* run age_table. */									/* Does nothing but waste effort...	*/

  empty temp-table tt_tblAct.

  /* top X creates
   */

  i = 0.
  for each tt_table no-lock by tt_table.tbl-cre[x] descending.

    i = i + 1.

    find tt_tblAct where tt_tblAct.tblNum = tt_table.tblNum no-error.			/* prevent duplicates			*/
    if available tt_tblAct then next.

    create tt_tblAct.
    assign
      tt_tblAct.xid        = tt_table.tblNum
      tt_tblAct.tblNum     = tt_table.tblNum
      tt_tblAct.tblAreaNum = tt_table.areaNum
      tt_tblAct.tblPool    = tt_table.tblPool
      tt_tblAct.tblName    = tt_table.tblName
      tt_tblAct.tblCr      = ( tt_table.tbl-cre[x]  / z )
      tt_tblAct.tblRd      = ( tt_table.tbl-rd[x]   / z )
      tt_tblAct.tblUp      = ( tt_table.tbl-upd[x]  / z )
      tt_tblAct.tblDl      = ( tt_table.tbl-del[x]  / z )
      tt_tblAct.tblOSRd    = ( tt_table.tbl-osrd[x] / z )
    .

    run getTableInfo(
      tt_table.tblName,
      output tt_tblAct.dbaRecs,
      output tt_tblAct.dbaRM,
      output tt_tblAct.dbaFragPct,
      output tt_tblAct.dbaScatter,
      output tt_tblAct.dbaAvgRow
    ).
    if tt_tblAct.dbaRecs > 0 then tt_tblAct.tblTurn = tt_tblAct.tblRd / tt_tblAct.dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X reads
   */

  i = 0.
  for each tt_table no-lock by tt_table.tbl-rd[x] descending.

    i = i + 1.

    find tt_tblAct where tt_tblAct.tblNum = tt_table.tblNum no-error.			/* prevent duplicates			*/
    if available tt_tblAct then next.

    create tt_tblAct.
    assign
      tt_tblAct.xid        = tt_table.tblNum
      tt_tblAct.tblNum     = tt_table.tblNum
      tt_tblAct.tblAreaNum = tt_table.areaNum
      tt_tblAct.tblPool    = tt_table.tblPool
      tt_tblAct.tblName    = tt_table.tblName
      tt_tblAct.tblCr      = ( tt_table.tbl-cre[x]  / z )
      tt_tblAct.tblRd      = ( tt_table.tbl-rd[x]   / z )
      tt_tblAct.tblUp      = ( tt_table.tbl-upd[x]  / z )
      tt_tblAct.tblDl      = ( tt_table.tbl-del[x]  / z )
      tt_tblAct.tblOSRd    = ( tt_table.tbl-osrd[x] / z )
    .

    run getTableInfo(
      tt_table.tblName,
      output tt_tblAct.dbaRecs,
      output tt_tblAct.dbaRM,
      output tt_tblAct.dbaFragPct,
      output tt_tblAct.dbaScatter,
      output tt_tblAct.dbaAvgRow
    ).
    if tt_tblAct.dbaRecs > 0 then tt_tblAct.tblTurn = tt_tblAct.tblRd / tt_tblAct.dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X updates
   */

  i = 0.
  for each tt_table no-lock by tt_table.tbl-upd[x] descending.

    i = i + 1.

    find tt_tblAct where tt_tblAct.tblNum = tt_table.tblNum no-error.			/* prevent duplicates			*/
    if available tt_tblAct then next.

    create tt_tblAct.
    assign
      tt_tblAct.xid        = tt_table.tblNum
      tt_tblAct.tblNum     = tt_table.tblNum
      tt_tblAct.tblAreaNum = tt_table.areaNum
      tt_tblAct.tblPool    = tt_table.tblPool
      tt_tblAct.tblName    = tt_table.tblName
      tt_tblAct.tblCr      = ( tt_table.tbl-cre[x]  / z )
      tt_tblAct.tblRd      = ( tt_table.tbl-rd[x]   / z )
      tt_tblAct.tblUp      = ( tt_table.tbl-upd[x]  / z )
      tt_tblAct.tblDl      = ( tt_table.tbl-del[x]  / z )
      tt_tblAct.tblOSRd    = ( tt_table.tbl-osrd[x] / z )
    .

    run getTableInfo(
      tt_table.tblName,
      output tt_tblAct.dbaRecs,
      output tt_tblAct.dbaRM,
      output tt_tblAct.dbaFragPct,
      output tt_tblAct.dbaScatter,
      output tt_tblAct.dbaAvgRow
    ).
    if tt_tblAct.dbaRecs > 0 then tt_tblAct.tblTurn = tt_tblAct.tblRd / tt_tblAct.dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X deletes
   */

  i = 0.
  for each tt_table no-lock by tt_table.tbl-del[x] descending.

    i = i + 1.

    find tt_tblAct where tt_tblAct.tblNum = tt_table.tblNum no-error.			/* prevent duplicates			*/
    if available tt_tblAct then next.

    create tt_tblAct.
    assign
      tt_tblAct.xid        = tt_table.tblNum
      tt_tblAct.tblNum     = tt_table.tblNum
      tt_tblAct.tblAreaNum = tt_table.areaNum
      tt_tblAct.tblPool    = tt_table.tblPool
      tt_tblAct.tblName    = tt_table.tblName
      tt_tblAct.tblCr      = ( tt_table.tbl-cre[x]  / z )
      tt_tblAct.tblRd      = ( tt_table.tbl-rd[x]   / z )
      tt_tblAct.tblUp      = ( tt_table.tbl-upd[x]  / z )
      tt_tblAct.tblDl      = ( tt_table.tbl-del[x]  / z )
      tt_tblAct.tblOSRd    = ( tt_table.tbl-osrd[x] / z )
    .

    run getTableInfo(
      tt_table.tblName,
      output tt_tblAct.dbaRecs,
      output tt_tblAct.dbaRM,
      output tt_tblAct.dbaFragPct,
      output tt_tblAct.dbaScatter,
      output tt_tblAct.dbaAvgRow
    ).
    if tt_tblAct.dbaRecs > 0 then tt_tblAct.tblTurn = tt_tblAct.tblRd / tt_tblAct.dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  add2ds( temp-table tt_tblAct:default-buffer-handle ).

  return.

end.

{ssg/recact.i}

return.

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
 * idxact.p
 *
 *
 * Index Activity
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

define output parameter dcDescription as character no-undo initial "IndexActivity".

{lib/tt_index.i}

define temp-table tt_idxAct no-undo
  field xid        as integer   format "->>>>9"
  field idxNum     as integer   format "->>>>9"       label "Idx#"
  field idxAreaNum as integer   format ">>>>9"        label "Area#"
  field idxPool    as character format "x(2)"         label "BX"
  field idxName    as character format "x(44)"        label "Index Name"

  field idxBlks    as {&BIGINT} format ">>>>>>>>>>9"  label "Blocks"
  field idxUtil    as decimal   format ">>9.99%"      label "Util"
  field idxLvls    as decimal   format ">>>9"         label "Lvls"

  field idxRoot    as {&BIGINT} format ">>>>>>>>>9"   label "Idx Root"
  field idxNote    as character format "   x(4)"      label "  Note "
  field idxCr      as decimal   format ">>>>>>>>9"    label "Create"
  field idxRd      as decimal   format ">>>>>>>>>9"   label "Read"
  field idxSp      as decimal   format ">>>>>>>>9"    label "Split"
  field idxDl      as decimal   format ">>>>>>>>9"    label "Delete"
  field idxBlkDl   as decimal   format ">>>>>>>>9"    label "BlkDl"

  index idxRd-idx  is primary idxRd descending
  index idxNum-idx is unique idxNum
  index idxRt-idx    idxRoot descending
  index idxCr-idx    idxCr   descending
  index idxSp-idx    idxSp   descending
  index idxDl-idx    idxDl   descending
  index idxBlkDl-idx idxDl   descending
  index idxPool-idx  idxPool descending
  index idxName-idx  idxName
  index xid-idx     is unique xid
.

{lib/dumpTT.i tt_idxAct}

define new global shared variable rowLimit as integer no-undo.

procedure mon-init:

  empty temp-table tt_index.

  run updTick.

  return.

end.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i        as integer no-undo.
  define variable topLimit as integer no-undo initial 25.				/* formerly 10				*/

  run updTick.

  for each dictdb._IndexStat no-lock:
    find tt_idx no-lock where tt_idx.xid = _IndexStat-id no-error.
    if available tt_idx then run upd-tt_index ( _IndexStat._IndexStat-id ).
  end.

  /* run age_index. */

  empty temp-table tt_idxAct.

  /* top X creates
   */

  i = 0.
  for each tt_index no-lock by tt_index.idx-cre[x] descending.

    i = i + 1.

    find tt_idxAct where tt_idxAct.idxNum = tt_index.idxNum no-error.			/* prevent duplicates			*/
    if available tt_idxAct then next.

    create tt_idxAct.
    assign
      tt_idxAct.xid        = tt_index.idxNum
      tt_idxAct.idxNum     = tt_index.idxNum
      tt_idxAct.idxareaNum = tt_index.areaNum
      tt_idxAct.idxPool    = tt_index.idxPool
      tt_idxAct.idxNote    = tt_index.idxNote
      tt_idxAct.idxRoot    = tt_index.idxRoot
      tt_idxAct.idxName    = tt_index.tblName + "." + tt_index.idxName
      tt_idxAct.idxCr      = ( tt_index.idx-cre[x]    / z )
      tt_idxAct.idxRd      = ( tt_index.idx-rd[x]     / z )
      tt_idxAct.idxSp      = ( tt_index.idx-split[x]  / z )
      tt_idxAct.idxDl      = ( tt_index.idx-del[x]    / z )
      tt_idxAct.idxBlkDl   = ( tt_index.idx-blkdel[x] / z )
    .

    run getIndexInfo( tt_idxAct.idxName, output tt_idxAct.idxLvls, output tt_idxAct.idxBlks, output tt_idxAct.idxUtil ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X reads
   */

  i = 0.
  for each tt_index no-lock by tt_index.idx-rd[x] descending.

    i = i + 1.

    find tt_idxAct where tt_idxAct.idxNum = tt_index.idxNum no-error.			/* prevent duplicates			*/
    if available tt_idxAct then next.

    create tt_idxAct.
    assign
      tt_idxAct.xid        = tt_index.idxNum
      tt_idxAct.idxNum     = tt_index.idxNum
      tt_idxAct.idxAreaNum = tt_index.areaNum
      tt_idxAct.idxPool    = tt_index.idxPool
      tt_idxAct.idxNote    = tt_index.idxNote
      tt_idxAct.idxRoot    = tt_index.idxRoot
      tt_idxAct.idxName    = tt_index.tblName + "." + tt_index.idxName
      tt_idxAct.idxCr      = ( tt_index.idx-cre[x]    / z )
      tt_idxAct.idxRd      = ( tt_index.idx-rd[x]     / z )
      tt_idxAct.idxSp      = ( tt_index.idx-split[x]  / z )
      tt_idxAct.idxDl      = ( tt_index.idx-del[x]    / z )
      tt_idxAct.idxBlkDl   = ( tt_index.idx-blkdel[x] / z )
    .

    run getIndexInfo( tt_idxAct.idxName, output tt_idxAct.idxLvls, output tt_idxAct.idxBlks, output tt_idxAct.idxUtil ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X splits
   */

  i = 0.
  for each tt_index no-lock by tt_index.idx-split[x] descending.

    i = i + 1.

    find tt_idxAct where tt_idxAct.idxNum = tt_index.idxNum no-error.			/* prevent duplicates			*/
    if available tt_idxAct then next.

    create tt_idxAct.
    assign
      tt_idxAct.xid        = tt_index.idxNum
      tt_idxAct.idxNum     = tt_index.idxNum
      tt_idxAct.idxAreaNum = tt_index.areaNum
      tt_idxAct.idxPool    = tt_index.idxPool
      tt_idxAct.idxNote    = tt_index.idxNote
      tt_idxAct.idxRoot    = tt_index.idxRoot
      tt_idxAct.idxName    = tt_index.tblName + "." + tt_index.idxName
      tt_idxAct.idxCr      = ( tt_index.idx-cre[x]    / z )
      tt_idxAct.idxRd      = ( tt_index.idx-rd[x]     / z )
      tt_idxAct.idxSp      = ( tt_index.idx-split[x]  / z )
      tt_idxAct.idxDl      = ( tt_index.idx-del[x]    / z )
      tt_idxAct.idxBlkDl   = ( tt_index.idx-blkdel[x] / z )
    .

    run getIndexInfo( tt_idxAct.idxName, output tt_idxAct.idxLvls, output tt_idxAct.idxBlks, output tt_idxAct.idxUtil ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X deletes
   */

  i = 0.
  for each tt_index no-lock by tt_index.idx-del[x] descending.

    i = i + 1.

    find tt_idxAct where tt_idxAct.idxNum = tt_index.idxNum no-error.			/* prevent duplicates			*/
    if available tt_idxAct then next.

    create tt_idxAct.
    assign
      tt_idxAct.xid        = tt_index.idxNum
      tt_idxAct.idxNum     = tt_index.idxNum
      tt_idxAct.idxAreaNum = tt_index.areaNum
      tt_idxAct.idxPool    = tt_index.idxPool
      tt_idxAct.idxNote    = tt_index.idxNote
      tt_idxAct.idxRoot    = tt_index.idxRoot
      tt_idxAct.idxName    = tt_index.tblName + "." + tt_index.idxName
      tt_idxAct.idxCr      = ( tt_index.idx-cre[x]    / z )
      tt_idxAct.idxRd      = ( tt_index.idx-rd[x]     / z )
      tt_idxAct.idxSp      = ( tt_index.idx-split[x]  / z )
      tt_idxAct.idxDl      = ( tt_index.idx-del[x]    / z )
      tt_idxAct.idxBlkDl   = ( tt_index.idx-blkdel[x] / z )
    .

    run getIndexInfo( tt_idxAct.idxName, output tt_idxAct.idxLvls, output tt_idxAct.idxBlks, output tt_idxAct.idxUtil ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X block deletes
   */

  i = 0.
  for each tt_index no-lock by tt_index.idx-blkdel[x] descending.

    i = i + 1.

    find tt_idxAct where tt_idxAct.idxNum = tt_index.idxNum no-error.			/* prevent duplicates			*/
    if available tt_idxAct then next.

    create tt_idxAct.
    assign
      tt_idxAct.xid        = tt_index.idxNum
      tt_idxAct.idxNum     = tt_index.idxNum
      tt_idxAct.idxAreaNum = tt_index.areaNum
      tt_idxAct.idxPool    = tt_index.idxPool
      tt_idxAct.idxNote    = tt_index.idxNote
      tt_idxAct.idxRoot    = tt_index.idxRoot
      tt_idxAct.idxName    = tt_index.tblName + "." + tt_index.idxName
      tt_idxAct.idxCr      = ( tt_index.idx-cre[x]    / z )
      tt_idxAct.idxRd      = ( tt_index.idx-rd[x]     / z )
      tt_idxAct.idxSp      = ( tt_index.idx-split[x]  / z )
      tt_idxAct.idxDl      = ( tt_index.idx-del[x]    / z )
      tt_idxAct.idxBlkDl   = ( tt_index.idx-blkdel[x] / z )
    .

    run getIndexInfo( tt_idxAct.idxName, output tt_idxAct.idxLvls, output tt_idxAct.idxBlks, output tt_idxAct.idxUtil ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  add2ds( temp-table tt_idxAct:default-buffer-handle ).

  return.

end.

{ssg/idxact.i}

return.

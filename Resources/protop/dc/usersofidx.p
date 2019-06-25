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
 * usersofidx.p
 *
 *
 * Users of an Index
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

define output parameter dcDescription as character no-undo initial "UsersOfIndex".

{lib/tt_uindex.i}

define temp-table tt_uIdxAct no-undo
  field xid       as integer   format "->>>>9"
  field usrNum    as integer   format ">>>>9"        label "Usr#"
  field usrname   as character format "x(16)"        label "User Name"
  field idxRoot   as {&BIGINT} format ">>>>>>>>>9"   label "Idx Root"
  field idxCr     as decimal   format ">>>>>>>>>9"   label "Create"
  field idxRd     as decimal   format ">>>>>>>>>>9"  label "Read"
  field idxSp     as decimal   format ">>>>>>>>>9"   label "Split"
  field idxDl     as decimal   format ">>>>>>>>>9"   label "Delete"
  field idxBlkDl  as decimal   format ">>>>9"        label "BlkDl"
  field lineNum   as integer   format "->>>>>"       label "Line#"
  field procName  as character format "x(63)"        label "Program Name"

  index usrNum-idx is unique usrNum
  index idxRd-idx  is primary idxRd descending
  index idxRt-idx    idxRoot descending
  index idxCr-idx    idxCr descending
  index idxSp-idx    idxSp descending
  index idxDl-idx    idxDl descending
  index idxBlkDl-idx idxDl descending
  index xid-idx  is unique xid
.

{lib/dumpTT.i tt_uIdxAct}

define temp-table tt_uIdxAct_Info no-undo
  field infoString as character
  index infoString-idx is primary unique infoString
.

{lib/dumpTT.i tt_uIdxAct_Info}

define new global shared variable rowLimit as integer no-undo.

procedure mon-init:

  empty temp-table tt_uindex.

  run updTick.

  return.

end.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i        as integer no-undo.
  define variable j        as integer no-undo.
  define variable k        as integer no-undo.

  define variable uIdxNum  as integer no-undo.
  define variable topLimit as integer no-undo initial 10.

  if argList <> "" then
    do:
      if argList begins "UIDXNUM" then
        assign uIdxNum = integer( entry( 2, argList, "=" )) no-error.
    end.

  run updTick.

  find last _connect no-lock.
  find last _indexstat no-lock.

  assign
    i = integer( recid( _connect ))							/* this is just showing off...		*/
    j = recid( _indexstat )
    k = i * j
  .

  i = uIdxNum.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
  do while i <= k:
    find _userIndexStat no-lock where _userIndexStat-id = i no-error.
    if not available _userIndexStat then leave.
    run upd-tt_uindex ( _UserIndexStat._UserIndexStat-id ).
    i = i + j.
  end.
&ENDIF

  /* run age_index. */

  empty temp-table tt_uIdxAct.
  empty temp-table tt_uIdxAct_Info.

  create tt_uIdxAct_Info.
  tt_uIdxAct_Info.infoString = substitute( 'Users of "&1" &2 idxNum: &3  areaNum: &4  idxRoot: &5', tt_uindex.idxName, tt_uindex.idxNote, tt_uindex.idxNum, tt_uindex.areaNum, tt_uindex.idxRoot ).

  /* top X creates
   */

  i = 0.
  for each tt_uindex no-lock by tt_uindex.idx-cre[x] descending.

    i = i + 1.

    find tt_uIdxAct where tt_uIdxAct.usrNum = tt_uindex.usrNum no-error.		/* prevent duplicates			*/
    if available tt_uIdxAct then next.

    create tt_uIdxAct.
    assign
      tt_uIdxAct.xid       = tt_uindex.usrNum
      tt_uIdxAct.usrNum    = tt_uindex.usrNum
      tt_uIdxAct.usrName   = tt_uindex.usrName
      tt_uIdxAct.idxCr     = ( tt_uindex.idx-cre[x]    / z )
      tt_uIdxAct.idxRd     = ( tt_uindex.idx-rd[x]     / z )
      tt_uIdxAct.idxSp     = ( tt_uindex.idx-split[x]  / z )
      tt_uIdxAct.idxDl     = ( tt_uindex.idx-del[x]    / z )
      tt_uIdxAct.idxBlkDl  = ( tt_uindex.idx-blkdel[x] / z )
    .
    lastStatement( tt_uindex.usrNum + 1, output tt_uIdxAct.lineNum, output tt_uIdxAct.procName ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X reads
   */

  i = 0.
  for each tt_uindex no-lock by tt_uindex.idx-rd[x] descending.

    i = i + 1.

    find tt_uIdxAct where tt_uIdxAct.usrNum = tt_uindex.usrNum no-error.		/* prevent duplicates			*/
    if available tt_uIdxAct then next.

    create tt_uIdxAct.
    assign
      tt_uIdxAct.xid       = tt_uindex.usrNum
      tt_uIdxAct.usrNum    = tt_uindex.usrNum
      tt_uIdxAct.usrName   = tt_uindex.usrName
      tt_uIdxAct.idxCr     = ( tt_uindex.idx-cre[x]    / z )
      tt_uIdxAct.idxRd     = ( tt_uindex.idx-rd[x]     / z )
      tt_uIdxAct.idxSp     = ( tt_uindex.idx-split[x]  / z )
      tt_uIdxAct.idxDl     = ( tt_uindex.idx-del[x]    / z )
      tt_uIdxAct.idxBlkDl  = ( tt_uindex.idx-blkdel[x] / z )
    .
    lastStatement( tt_uindex.usrNum + 1, output tt_uIdxAct.lineNum, output tt_uIdxAct.procName ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X splits
   */

  i = 0.
  for each tt_uindex no-lock by tt_uindex.idx-split[x] descending.

    i = i + 1.

    find tt_uIdxAct where tt_uIdxAct.usrNum = tt_uindex.usrNum no-error.		/* prevent duplicates			*/
    if available tt_uIdxAct then next.

    create tt_uIdxAct.
    assign
      tt_uIdxAct.xid       = tt_uindex.usrNum
      tt_uIdxAct.usrNum    = tt_uindex.usrNum
      tt_uIdxAct.usrName   = tt_uindex.usrName
      tt_uIdxAct.idxCr     = ( tt_uindex.idx-cre[x]    / z )
      tt_uIdxAct.idxRd     = ( tt_uindex.idx-rd[x]     / z )
      tt_uIdxAct.idxSp     = ( tt_uindex.idx-split[x]  / z )
      tt_uIdxAct.idxDl     = ( tt_uindex.idx-del[x]    / z )
      tt_uIdxAct.idxBlkDl  = ( tt_uindex.idx-blkdel[x] / z )
    .
    lastStatement( tt_uindex.usrNum + 1, output tt_uIdxAct.lineNum, output tt_uIdxAct.procName ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X deletes
   */

  i = 0.
  for each tt_uindex no-lock by tt_uindex.idx-del[x] descending.

    i = i + 1.

    find tt_uIdxAct where tt_uIdxAct.usrNum = tt_uindex.usrNum no-error.		/* prevent duplicates			*/
    if available tt_uIdxAct then next.

    create tt_uIdxAct.
    assign
      tt_uIdxAct.xid       = tt_uindex.usrNum
      tt_uIdxAct.usrNum    = tt_uindex.usrNum
      tt_uIdxAct.usrName   = tt_uindex.usrName
      tt_uIdxAct.idxCr     = ( tt_uindex.idx-cre[x]    / z )
      tt_uIdxAct.idxRd     = ( tt_uindex.idx-rd[x]     / z )
      tt_uIdxAct.idxSp     = ( tt_uindex.idx-split[x]  / z )
      tt_uIdxAct.idxDl     = ( tt_uindex.idx-del[x]    / z )
      tt_uIdxAct.idxBlkDl  = ( tt_uindex.idx-blkdel[x] / z )
    .
    lastStatement( tt_uindex.usrNum + 1, output tt_uIdxAct.lineNum, output tt_uIdxAct.procName ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X block deletes
   */

  i = 0.
  for each tt_uindex no-lock by tt_uindex.idx-blkdel[x] descending.

    i = i + 1.

    find tt_uIdxAct where tt_uIdxAct.usrNum = tt_uindex.usrNum no-error.		/* prevent duplicates			*/
    if available tt_uIdxAct then next.

    create tt_uIdxAct.
    assign
      tt_uIdxAct.xid       = tt_uindex.usrNum
      tt_uIdxAct.usrNum    = tt_uindex.usrNum
      tt_uIdxAct.usrName   = tt_uindex.usrName
      tt_uIdxAct.idxCr     = ( tt_uindex.idx-cre[x]    / z )
      tt_uIdxAct.idxRd     = ( tt_uindex.idx-rd[x]     / z )
      tt_uIdxAct.idxSp     = ( tt_uindex.idx-split[x]  / z )
      tt_uIdxAct.idxDl     = ( tt_uindex.idx-del[x]    / z )
      tt_uIdxAct.idxBlkDl  = ( tt_uindex.idx-blkdel[x] / z )
    .
    lastStatement( tt_uindex.usrNum + 1, output tt_uIdxAct.lineNum, output tt_uIdxAct.procName ).

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* message tt_uIdxAct_Info.infoString view-as alert-box. */

  add2ds( temp-table tt_uIdxAct:default-buffer-handle ).
  add2ds( temp-table tt_uIdxAct_Info:default-buffer-handle ).

  return.

end.

return.

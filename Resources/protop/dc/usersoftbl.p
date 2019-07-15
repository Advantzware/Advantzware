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
 * usersoftbl.p
 *
 *
 * User of Table
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

define output parameter dcDescription as character no-undo initial "UsersOfTable".

define variable hasOSRd    as logical   no-undo.

/*
define variable tRange     as integer no-undo.
 */

{lib/tt_utable.i}

define temp-table tt_uTblAct no-undo
  field xid        as integer   format "->>>>9"
  field usrNum     as integer   format ">>>>9"           label "Usr#"
  field usrname    as character format "x(16)"           label "User Name"
  field tblTurn    as decimal   format ">>>>>>>>9.99"    label "Turns"
  field tblCr      as decimal   format ">>>>>>>>>9"      label "Create"
  field tblRd      as decimal   format ">>>>>>>>>9"      label "Read"
  field tblUp      as decimal   format ">>>>>>>>>9"      label "Update"
  field tblDl      as decimal   format ">>>>>>>>>9"      label "Delete"
  field lineNum    as integer   format "->>>>>"          label "Line#"
  field procName   as character format "x(68)"           label "Program Name"

  index usrNum-idx is unique usrNum
  index tblRd-idx  is primary tblRd descending
  index tblTurn-idx tblTurn descending
  index tblCr-idx   tblCr   descending
  index tblUp-idx   tblUp   descending
  index tblDl-idx   tblDl   descending
  index xid-idx  is unique xid
.

{lib/dumpTT.i tt_uTblAct}

define temp-table tt_uTblAct_Info no-undo
  field infoString as character
  index infoString-idx is primary unique infoString
.

{lib/dumpTT.i tt_uTblAct_Info}

define new global shared variable rowLimit as integer no-undo.

define variable dbaRecs    as decimal no-undo.
define variable dbaRM      as decimal no-undo.
define variable dbaFragPct as decimal no-undo.
define variable dbaScat    as decimal no-undo.
define variable dbaAvgRow  as decimal no-undo.

procedure chkOSRd:
  hasOSRd = no.
  find _file no-lock where _file._file-name = "_usertablestat" no-error.
  if available _file then find _field no-lock of _file where _field._field-name = "_usertablestat-osread" no-error.
  hasOSRd = available( _field ).
  return.
end.

define variable numTbls as integer no-undo.
define variable numUsrs as integer no-undo.
define variable numUTbl as integer no-undo.
define variable tblNum  as integer no-undo.
define variable uTblNum as integer no-undo.
define variable xTblNum as integer no-undo.
define variable uTblId  as integer no-undo.

procedure mon-init:

  run chkOSRd.

  find last _connect no-lock.
  find last _tablestat no-lock.

  assign
    numUsrs = integer( recid( _connect ))							/* this is just showing off...		*/
    numTbls = recid( _tablestat )
    numUTbl = numUsrs * numTbls
  .

  empty temp-table tt_utable.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i         as integer no-undo.
  define variable topLimit  as integer no-undo initial 10.

  if argList <> "" then
    do:
      if argList begins "UTBLNUM" then
        do:
          assign xTblNum = integer( entry( 2, argList, "=" )) no-error. 
          if xTblNum <> uTblNum then
            do:

              uTblNum = xTblNum.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
              for each _usertablestat no-lock:
                if _usertablestat-num = uTblNum then
                  do:
                    uTblId = _usertablestat-id. 
                    leave.
                  end.
                if _usertablestat-id > numTbls then leave.
              end.
&ENDIF

            end.
        end.
    end.

  i = uTblId.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
  do while i <= numUTbl:
    find _userTableStat no-lock where _userTableStat-id = i no-error.
    if not available _userTableStat then leave.
    run upd-tt_utable ( _UserTableStat._UserTableStat-id ).
    i = i + numTbls.
  end.
&ENDIF

  run updTick.

  /* run age_uTable. */									/* Does nothing but waste effort...	*/

  empty temp-table tt_uTblAct.
  empty temp-table tt_uTblAct_Info.

  run getTableInfo( tt_utable.tblName, output dbaRecs, output dbaRM, output dbaFragPct, output dbaScat, output dbaAvgRow ).

  create tt_uTblAct_Info.
  tt_uTblAct_Info.infoString = substitute( 'Users of "&1"  tblNum: &2  areaNum: &3  records: &4', tt_utable.tblName, tt_utable.tblNum, tt_utable.areaNum, dbaRecs ).

  for each tt_utable:
    assign
      tt_utable.tbl-Cre[x] = max( 0, tt_utable.tbl-cre[x] )
      tt_utable.tbl-Rd[x]  = max( 0, tt_utable.tbl-rd[x]  )
      tt_utable.tbl-Upd[x] = max( 0, tt_utable.tbl-upd[x] )
      tt_utable.tbl-Del[x] = max( 0, tt_utable.tbl-del[x] )
    .
  end.

  /* top X creates
   */

  i = 0.
  for each tt_utable no-lock by tt_utable.tbl-cre[x] descending.

    i = i + 1.

    find tt_utblAct where tt_utblAct.usrNum = tt_utable.usrNum no-error.		/* prevent duplicates			*/
    if available tt_utblAct then next.

    create tt_utblAct.
    assign
      tt_utblAct.xid       = tt_utable.usrNum
      tt_utblAct.usrNum    = tt_utable.usrNum
      tt_utblAct.usrName   = tt_utable.usrName
      tt_utblAct.tblCr     = ( tt_utable.tbl-cre[x]  / z )
      tt_utblAct.tblRd     = ( tt_utable.tbl-rd[x]   / z )
      tt_utblAct.tblUp     = ( tt_utable.tbl-upd[x]  / z )
      tt_utblAct.tblDl     = ( tt_utable.tbl-del[x]  / z )
    .
    lastStatement( tt_utable.usrNum + 1, output tt_uTblAct.lineNum, output tt_uTblAct.procName ).

    if dbaRecs > 0 then tt_utblAct.tblTurn = tt_utblAct.tblRd / dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X reads
   */

  i = 0.
  for each tt_utable no-lock by tt_utable.tbl-rd[x] descending.

    i = i + 1.

    find tt_utblAct where tt_utblAct.usrNum = tt_utable.usrNum no-error.		/* prevent duplicates			*/
    if available tt_utblAct then next.

    create tt_utblAct.
    assign
      tt_utblAct.xid     = tt_utable.usrNum
      tt_utblAct.usrNum  = tt_utable.usrNum
      tt_utblAct.usrName = tt_utable.usrName
      tt_utblAct.tblCr   = ( tt_utable.tbl-cre[x]  / z )
      tt_utblAct.tblRd   = ( tt_utable.tbl-rd[x]   / z )
      tt_utblAct.tblUp   = ( tt_utable.tbl-upd[x]  / z )
      tt_utblAct.tblDl   = ( tt_utable.tbl-del[x]  / z )
    .
    lastStatement( tt_utable.usrNum + 1, output tt_uTblAct.lineNum, output tt_uTblAct.procName ).

    if dbaRecs > 0 then tt_utblAct.tblTurn = tt_utblAct.tblRd / dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X updates
   */

  i = 0.
  for each tt_utable no-lock by tt_utable.tbl-upd[x] descending.

    i = i + 1.

    find tt_utblAct where tt_utblAct.usrNum = tt_utable.usrNum no-error.		/* prevent duplicates			*/
    if available tt_utblAct then next.

    create tt_utblAct.
    assign
      tt_utblAct.xid     = tt_utable.usrNum
      tt_utblAct.usrNum  = tt_utable.usrNum
      tt_utblAct.usrName = tt_utable.usrName
      tt_utblAct.tblCr   = ( tt_utable.tbl-cre[x]  / z )
      tt_utblAct.tblRd   = ( tt_utable.tbl-rd[x]   / z )
      tt_utblAct.tblUp   = ( tt_utable.tbl-upd[x]  / z )
      tt_utblAct.tblDl   = ( tt_utable.tbl-del[x]  / z )
    .
    lastStatement( tt_utable.usrNum + 1, output tt_uTblAct.lineNum, output tt_uTblAct.procName ).

    if dbaRecs > 0 then tt_utblAct.tblTurn = tt_utblAct.tblRd / dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* top X deletes
   */

  i = 0.
  for each tt_utable no-lock by tt_utable.tbl-del[x] descending.

    i = i + 1.

    find tt_utblAct where tt_utblAct.usrNum = tt_utable.usrNum no-error.		/* prevent duplicates			*/
    if available tt_utblAct then next.

    create tt_utblAct.
    assign
      tt_utblAct.xid     = tt_utable.usrNum
      tt_utblAct.usrNum  = tt_utable.usrNum
      tt_utblAct.usrName = tt_utable.usrName
      tt_utblAct.tblCr   = ( tt_utable.tbl-cre[x]  / z )
      tt_utblAct.tblRd   = ( tt_utable.tbl-rd[x]   / z )
      tt_utblAct.tblUp   = ( tt_utable.tbl-upd[x]  / z )
      tt_utblAct.tblDl   = ( tt_utable.tbl-del[x]  / z )
    .
    lastStatement( tt_utable.usrNum + 1, output tt_uTblAct.lineNum, output tt_uTblAct.procName ).

    if dbaRecs > 0 then tt_utblAct.tblTurn = tt_utblAct.tblRd / dbaRecs.

    if ( topLimit > 0 ) and ( i >= topLimit ) then leave.

  end.

  /* message tt_uTblAct_Info.infoString view-as alert-box. */

  add2ds( temp-table tt_uTblAct:default-buffer-handle ).
  add2ds( temp-table tt_uTblAct_Info:default-buffer-handle ).

  return.

end.

return.

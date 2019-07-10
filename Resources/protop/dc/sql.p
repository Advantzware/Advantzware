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
 * sql.p
 *
 *
 * SQL Activity monitoring.
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

define output parameter dcDescription as character no-undo initial "SQLActivity".

{lib/tt_xstat.i}

define temp-table tt_SQL no-undo
  field xId       as integer   format ">>9"      label "Id"
  field xValid    as logical
  field userNum   as integer   format ">>>>9"    label "Usr#"
&IF DEFINED( OE11 ) &THEN
  field tId       as integer   format "->>>>9"   label "Tenant"
&ENDIF
  field userName  as character format "x(15)"    label "Name"
  field userPID   as character format "x(8)"     label "PID"
  field userFlags as character format "x(5)"     label "Flags"
  field dbAccess  as integer   format "->>>>>>9" label "Blk Acc"
  field osReads   as integer   format "->>>>>>9" label "OS Rd"
  field osWrites  as integer   format "->>>>>>9" label "OS Wr"
  field hitRatio  as decimal   format "->>9.99%" label "Hit%"
  field recLks    as integer   format "->>>>>>9" label "Rec Lck"
  field lkHWM     as integer   format "->>>>>9"  label "Lk HWM"
&IF DEFINED( OE11 ) &THEN
  field procName  as character format "x(58)"    label "SQL Query"
&ELSE
  field procName  as character format "x(65)"    label "SQL Query"
&ENDIF

  index dbAccess-idx is primary dbAccess descending
  index xId-idx      is unique xid

&IF DEFINED( OE11 ) &THEN
  index tId-idx   tId
&ENDIF

  index userName-idx userName
  index userNum-idx  userNum

  index osReads-idx  osReads  descending
  index osWrites-idx osWrites descending
  index hitRatio-idx hitRatio descending
  index recLks-idx   recLks   descending
  index lkHWM-idx    lkHWM    descending
  index xValid-idx   xValid
.

{lib/dumpTT.i tt_SQL}

define new global shared variable rowLimit as integer no-undo.
define new global shared variable dbgMode  as integer no-undo initial 1.

define variable pt_trimStack as character no-undo	/* a list of items to automatically remove from the program stack	*/
  initial ""
.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_xstat.

  run updTick.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i  as integer no-undo.
  define variable j  as integer no-undo.
  define variable lx as integer no-undo.

  define variable ct as character no-undo.

  run updTick.

  for each tt_sql:
    xvalid = no.
  end.

&IF {&CONNECTX} = "yes" &THEN

  for each dictdb._Connect no-lock: /* where _connect-client begins "SQ": */

    ct = "".
    ct =  buffer _connect:handle:buffer-field( "_connect-clientType" ):buffer-value no-error.

    if ( ct = ? ) or (( ct begins "SQ" ) = false ) then next.

    find dictdb._UserIO no-lock where _UserIO-Id = _Connect-Id.

    find tt_sql where tt_sql.xid = _UserIO-Id no-error.
    if not available( tt_sql ) then create tt_sql.
    assign
      tt_sql.xvalid  = yes
      tt_sql.xid     = _UserIO-Id
      tt_sql.userNum = _UserIO-Usr
    .

    find dictdb._LockReq no-lock where _LockReq-Id = _UserIO-Id.

    assign
      tt_sql.userFlags = connectFlags( _Connect-Id )
      tt_sql.userName  = connectName( _Connect-Id, tt_sql.userFlags )
      tt_sql.userPID   = string( _connect-PID )
    .

    lastStatement( _Connect-Id, output lx, output tt_sql.procName ).

&IF DEFINED( OE11 ) &THEN
      tt_sql.tId  = _userIO-tenantId.
&ENDIf

    run update_xstat (
      input _UserIO-Id,				/* unique key		*/
      input "", 				/* u_name		*/
      input "", 				/* u_flags		*/
      input "", 				/* u_pid		*/
      input "", 				/* string( _Connect-CacheLineNumber[1] ) + "," + _Connect-CacheInfo[1] */
      input "", 				/* _connect-clientType	*/
      input "",
      input _UserIO-dbaccess,			/* stat1		*/
      input _UserIO-dbread,			/* stat2		*/
      input _UserIO-dbwrite,			/* stat3		*/
      input 0,
      input _LockReq-RecLock,
      input _LockReq-RecWait
    ).

  end.

&ENDIF

  run age_xstat.

  i = 0.
  for each tt_sql where tt_sql.xvalid = yes:

    find tt_xstat where tt_xstat.xid = tt_sql.xid no-error.
    if ( not available tt_xstat ) /* or ( tt_xstat.stat1[x] = 0 ) */ then
   /* or (( rowLimit > 0 ) and ( i >= rowLimit )) then */		/* this needs to be within the context of a sort order	*/
      do:
        tt_sql.xvalid = no.
        next.
      end.

    i = i + 1.

    assign
      tt_sql.xvalid    = yes
      tt_sql.dbAccess  = tt_xstat.stat1[x] / z
      tt_sql.OSReads   = tt_xstat.stat2[x] / z
      tt_sql.OSWrites  = tt_xstat.stat3[x] / z
      tt_sql.hitRatio  = tt_xstat.stat-ratio
      tt_sql.recLks    = tt_xstat.stat5[x] / z
    .

    &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.7 &THEN

    find _userLock no-lock where _userLock-id =  tt_sql.xid.
    tt_sql.recLks = buffer _userLock:handle:buffer-field( "_userLock-Total" ):buffer-value no-error.
    tt_sql.lkHWM  = buffer _userLock:handle:buffer-field( "_userLock-HWM" ):buffer-value no-error.

    &ENDIF

  end.

  for each tt_sql where tt_sql.xvalid = no:
    delete tt_sql.
  end.

  j = 0.
  for each tt_sql where tt_sql.xvalid = yes:
    j = j + 1.
  end.

  /*
   *
  i = 0.
  if rowLimit > 0 then
    for each tt_sql by dbAccess:
      if i < rowLimit then
        i = i + 1.
       else
        delete tt_sql.
    end.
   *
   */

  if dbgMode >= 5 then message {&NOW} "valid tt_sql rows:" j "  sent:" i.

  add2ds( temp-table tt_sql:default-buffer-handle ).

  return.

end.

return.

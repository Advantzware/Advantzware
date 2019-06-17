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
 * uio.p
 *
 *
 * UserIO monitoring.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 5, 2003
 *
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.7 &THEN
/* new _userLock fields */
&ELSE
/* lock requests */
&ENDIF

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "UserIOActivity".

{lib/tt_xstat.i}

define temp-table tt_uio no-undo
  field xId       as integer   format ">>>>9"      label "Id"
  field xValid    as logical
  field userNum   as integer   format ">>>>9"      label "Usr#"
&IF DEFINED( OE11 ) &THEN
  field tId       as integer   format "->>>>9"     label "Tenant"
&ENDIF
  field userName  as character format "x(15)"      label "Name"
  field userPID   as character format "x(8)"       label "PID"
  field userFlags as character format "x(5)"       label "Flags"
  field dbAccess  as integer   format "->>>>>>9"   label "Blk Acc"
  field osReads   as integer   format "->>>>>>9"   label "OS Rd"
  field osWrites  as integer   format "->>>>>9"    label "OS Wr"
  field hitRatio  as decimal   format "->>>>9.99%" label "Hit%"
  field recLks    as integer   format "->>>>>>9"   label "Rec Lck"
/*field recWts    as integer   format "->>>>>9"    label "Rec Wts" */
  field lkHWM     as integer   format "->>>>>9"    label "Lk HWM"
  field lineNum   as integer   format "->>>>>"     label "Line#"
&IF DEFINED( OE11 ) &THEN
  field procName  as character format "x(50)"      label "Program Name"
&ELSE
  field procName  as character format "x(57)"      label "Program Name"
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
/*index recWts-idx   recWts   descending */
  index lkHWM-idx    lkHWM    descending
  index xValid-idx   xValid
.

{lib/dumpTT.i tt_uio}

define new global shared variable rowLimit as integer no-undo.
define new global shared variable dbgMode  as integer no-undo initial 1.

define variable pt_trimStack as character no-undo	/* a list of items to automatically remove from the program stack	*/
  initial ""
.

subscribe to "enableCSC" anywhere run-procedure "enableCSC".

procedure enableCSC:

  define input parameter userNum as integer no-undo.
  define input parameter howLong as integer no-undo.

  publish "info" ( "enableCSC", substitute( "enableCSC: &1 &2 (not yet)", userNum, howLong )).

  return.

end.


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

  define variable i as integer no-undo.
  define variable j as integer no-undo.

  run updTick.

  for each tt_uio:
    xvalid = no.
  end.

  for each dictdb._UserIO no-lock where _UserIO-usr <> ?:

    find tt_uio where tt_uio.xid = _UserIO-Id no-error.
    if not available( tt_uio ) then create tt_uio.
    assign
      tt_uio.xvalid  = yes
      tt_uio.xid     = _UserIO-Id
      tt_uio.userNum = _UserIO-Usr
    .

    find dictdb._Connect no-lock where _Connect-Id = _UserIO-Id.
    find dictdb._LockReq no-lock where _LockReq-Id = _UserIO-Id.

    assign
      tt_uio.userFlags = connectFlags( _Connect-Id )
      tt_uio.userName  = connectName( _Connect-Id, tt_uio.userFlags )
      tt_uio.userPID   = string( _connect-PID )
    .

    lastStatement( _Connect-Id, output tt_uio.lineNum, output tt_uio.procName ).

&IF DEFINED( OE11 ) &THEN
/*  tt_uio.tId  = _userIO-tenantId. */

    tt_uio.tId  = buffer _userIO:handle:buffer-field( "_userIO-tenantId" ):buffer-value no-error.
&ENDIF

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

  run age_xstat.

  i = 0.
  for each tt_uio where tt_uio.xvalid = yes:

    find tt_xstat where tt_xstat.xid = tt_uio.xid no-error.
    if ( not available tt_xstat ) /* or ( tt_xstat.stat1[x] = 0 ) */ then
   /* or (( rowLimit > 0 ) and ( i >= rowLimit )) then */		/* this needs to be within the context of a sort order	*/
      do:
        tt_uio.xvalid = no.
        next.
      end.

    i = i + 1.

    assign
      tt_uio.xvalid    = yes
      tt_uio.dbAccess  = tt_xstat.stat1[x] / z
      tt_uio.OSReads   = tt_xstat.stat2[x] / z
      tt_uio.OSWrites  = tt_xstat.stat3[x] / z
      tt_uio.hitRatio  = tt_xstat.stat-ratio
      tt_uio.recLks    = tt_xstat.stat5[x] / z
/*    tt_uio.recWts    = tt_xstat.stat6[x] / z */
    .

    &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.7 &THEN

    if pt_userlock = yes then
      do:
        find _userLock no-lock where _userLock-id =  tt_uio.xid.
        tt_uio.recLks = buffer _userLock:handle:buffer-field( "_userLock-Total" ):buffer-value no-error.
        tt_uio.lkHWM  = buffer _userLock:handle:buffer-field( "_userLock-HWM" ):buffer-value no-error.
      end.

    &ENDIF

  end.

  for each tt_uio where tt_uio.xvalid = no:
    delete tt_uio.
  end.

  j = 0.
  for each tt_uio where tt_uio.xvalid = yes:
    j = j + 1.
  end.

  /*
   *
  i = 0.
  if rowLimit > 0 then
    for each tt_uio by dbAccess:
      if i < rowLimit then
        i = i + 1.
       else
        delete tt_uio.
    end.
   *
   */

  if dbgMode >= 6 then message {&NOW} "valid tt_uio rows:" j "  sent:" i.

  add2ds( temp-table tt_uio:default-buffer-handle ).

  return.

end.

{ssg/usract.i}
{ssg/usrlks.i}
{ssg/usrlkhwm.i}

{ssg/enablecsc.i}

return.

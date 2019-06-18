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
 * activetrx.p
 *
 *
 * Active trx monitoring (all TRX *except* "Allocated").
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	August 28, 2003
 *
 */

{lib/protop.i}

define output parameter dcDescription as character no-undo initial "ActiveTRX".

define temp-table tt_trx no-undo
  field xid       as integer   format ">>>>>>>>>9"    label "Id"
  field usrnum    as integer   format ">>>>9"         label "Usr#"
&IF DEFINED( OE11 ) &THEN
  field tId       as integer   format "->>>>9"        label "Tenant"
&ENDIF
  field userName  as character format "x(15)"         label "Name"
  field userPID   as character format "x(8)"          label "PID"
  field userFlags as character format "x(5)"          label "Flags"
  field userDev   as character format "x(16)"         label "Device"
  field trx-num   as integer   format ">>>>>>>>>9"    label "TRX#"
  field trx-rl    as integer   format ">>>>>>>>9"     label "BI Clstr"
  field trx-st    as character format "x(4)"          label "Stat"
  field xtime     as character format "x(13)"         label "Start"
  field duraStr   as character format "x(12)"         label "    Duration"
&IF DEFINED( OE11 ) &THEN
  field trx-wait  as character format "x(55)"         label "Wait Resource (dbkey)"
&ELSE
  field trx-wait  as character format "x(62)"         label "Wait Resource (dbkey)"
&ENDIF
  field duration  as integer   format ">>>>>9"        label " Len" {&NOSERIALIZE}

  index xid-idx is unique xid
  index duraStr-idx is primary duraStr descending

&IF DEFINED( OE11 ) &THEN
  index tId-idx   tId
&ENDIF

.

{lib/dumpTT.i tt_trx}

define new global shared variable rowLimit as integer no-undo.

procedure mon-init:

  empty temp-table tt_trx.

  return.

end.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i as integer no-undo.

  empty temp-table tt_trx.	/* Yes, empty it every time -- this is a snapshot	*/

  /* Get open transaction data from _Trans VST
   *
   */

  find dictdb._ActSummary no-lock.

  for each dictdb._Trans no-lock where _Trans-usrnum <> ? /* and _Trans-state <> "allocated" */ /* and _Trans-duration <> ? */ by _Trans-duration descending:

    if _Trans-duration >= _Summary-upTime then next.		/* every now and then a crazy duration appears */

    i = i + 1.

    create tt_trx.
    assign
      tt_trx.xid      = _Trans-id
      tt_trx.xtime    = ( if _Trans-txtime <> ? then substring( _Trans-txtime, 12 ) else "" )
      tt_trx.usrnum   = _Trans-usrnum
      tt_trx.trx-num  = _Trans-num
      tt_trx.trx-rl   = _Trans-counter
      tt_trx.trx-st   = _Trans-state
      tt_trx.duration = ( if _Trans-duration <> ? then _Trans-duration else 0 )
      tt_trx.duraStr  = string( integer( truncate( tt_trx.duration / 86400, 0 )), ">>>" ) + " " + string( tt_trx.duration, "hh:mm:ss" )
    .

    case tt_trx.trx-st:
      when "ALLOCATED"  then tt_trx.trx-st = "ALLO".
      when "ACTIVE"     then tt_trx.trx-st = "ACTV".
      when "DEAD"       then tt_trx.trx-st = "DEAD".
      when "COMMITTING" then tt_trx.trx-st = "COMM".
      when "PREPARING"  then tt_trx.trx-st = "PRPG".
      when "PREPARED"   then tt_trx.trx-st = "PRPD".
    end.

    /* get some info about the session that started the trx
     *
     */

    /* About usr#
     *
     * PROMON = usr#
     * .lg file = usr#
     * _myconnection.myconn-userid = usr#
     * _connection._connect-usr = usr#
     * _connection._connection-id = usr# + 1
     *
     * _connection-id is a *much* more efficient way to access _connect -- thus we
     * jump through hoops (add one to the usr#)
     *
     */

    find dictdb._Connect no-lock where _Connect-id = _Trans-usrnum + 1 no-error.

    if available _Connect then
      assign
        tt_trx.userFlags = connectFlags( _Connect-Id )
        tt_trx.userName  = connectName( _Connect-Id, tt_trx.userFlags )
        tt_trx.userPID   = string( _connect-PID )
        tt_trx.userDev   = _connect-Device
        tt_trx.trx-wait  = ( if available _Connect then ( string( _Connect-wait ) + " " + string( _Connect-wait1 )) else "" )
      .

    /* if ( rowLimit > 0 ) and ( i >= rowLimit ) then leave. */		/* need a sort order!	*/

  end.

  add2ds( temp-table tt_trx:default-buffer-handle ).

  return.

end.

{ssg/activetrx.i}

return.

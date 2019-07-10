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
 * who.p
 *
 *
 * Who is Connected?
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

define output parameter dcDescription as character no-undo initial "who".

define temp-table tt_who no-undo
  field xid         as integer
  field xvalid      as logical
  field usrNum      as integer   format ">>>>>9"    label "Usr#"
&IF DEFINED( OE11 ) &THEN
  field tId         as integer   format "->>>>9"    label "Tenant"
&ENDIF
  field usrName     as character format "x(16)"     label "UserName"
/* field usrFullName as character format "x(30)"     label "Full Name" */
  field usrPID      as integer   format ">>>>>>>>9" label "PID"
  field connectAge  as character format "x(16)"     label "Session Duration"
  field usrServ     as integer   format ">>>>>>"    label "Server"
&IF DEFINED( OE11 ) &THEN
  field usrIPAddr   as character format "x(18)"     label "Device"
&ELSE
  field usrIPAddr   as character format "x(25)"     label "Device"
&ENDIF
  field statTRX     as character format "x(13)"      label "TRX Stat"
  field usrType     as character format "x(14)"     label "Type"
  field usrBlk      as character format "x(14)"     label "Blocked"
  field lineNum     as integer   format "->>>>>"     label "Line#"
  field procName    as character format "x(20)"      label "Program Name"

  index usrNum-idx   is unique primary usrNum xid
  index xid-idx      is unique xid
  index usrName-idx  usrName
  index xvalid-idx   xvalid

&IF DEFINED( OE11 ) &THEN
  index tId-idx   tId
&ENDIF

.

{lib/dumpTT.i tt_who}

define new global shared variable rowLimit as integer no-undo.

procedure mon-init:

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable currUDT as integer no-undo.
  define variable connUDT as integer no-undo.

  define variable numDays    as integer no-undo.
  define variable numSeconds as integer no-undo.

  define variable trxStr     as character no-undo.
  define variable cnxFlags   as character no-undo.

  define variable i as integer no-undo.

  run updTick.

  for each tt_who:
    tt_who.xvalid = no.
  end.

  currUDT = uDateTime().

  for each dictdb._Connect no-lock where _Connect-usr <> ?:

    i = i + 1.

    find tt_who where tt_who.xid = _Connect-id no-error.

    if not available tt_who then create tt_who.

    trxStr = "".
    if _Connect-Transid > 0 then
      do:
        find first dictdb._Trans no-lock where _Trans-UsrNum = _Connect-Usr no-error.
        if available _Trans then
          do:
            trxStr = trim( substring( _Trans-state, 1, 5 )).
            if _Trans-duration <> ? then
              trxStr = string( integer( truncate( _Trans-duration / 86400, 0 )), ">>>" ) + " " + string( _Trans-duration, "hh:mm:ss" ).
          end.
      end.

    cnxFlags = connectFlags( _Connect-Id ).	 /* _Connect-Type + ( if _Connect-Device = "batch" then " BAT" else "" ) */



    lastStatement( _Connect-Id, output tt_who.lineNum, output tt_who.procName ).

    assign
      connUDT           = ( currUDT - string2uDateTime( _Connect-Time ))	/* UDF has to be before indexed fields in old releases  */
      tt_who.xid        = _Connect-Id
      tt_who.xvalid     = yes
      tt_who.usrNum     = _Connect-Usr
      tt_who.usrName    = _Connect-Name
      tt_who.usrPID     = _Connect-PID
      tt_who.usrServ    = ( if _Connect-Server = ? then 0 else _Connect-Server )
      tt_who.usrIPAddr  = ( if _Connect-Device = "batch" then "" else _Connect-Device )
      numSeconds        = connUDT modulo 86400
      numDays           = (( connUDT - numSeconds ) / 86400 )
      tt_who.connectAge = substitute( "&1&3 &2", string( numDays, ">>>>>>" ), string( numSeconds, "hh:mm:ss" ), ( if numDays > 0 then "d" else " " ))
      statTRX           = trxStr
      tt_who.usrType    = _Connect-Type + " " + cnxFlags
      tt_who.usrBlk     = ( if _Connect-Transid = 0 then "" else "TRX " + ( if trim( _Connect-Wait ) <> "--" then _Connect-Wait else "" ))
    .

/*    tt_who.usrName    = connectName( _Connect-Id, tt_who.cnxFlags )  */

/*  find _user no-lock where _user._userId = _Connect-Name no-error.
 *  if available _user then tt_who.usrFullName = _User-Name.
 */

    if ( rowLimit > 0 ) and ( i >= rowLimit ) then leave.

  end.

  for each tt_who where xvalid = no:
    delete tt_who.
  end.

  add2ds( temp-table tt_who:default-buffer-handle ).

  return.

end.

return.

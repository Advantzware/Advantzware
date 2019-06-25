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
 * serverio.p
 *
 *
 * Remote client server activity and IO impact.
 *
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	September 21, 2003
 *
 */

{lib/protop.i}
{lib/tick.i}

define output parameter dcDescription as character no-undo initial "RemoteServerActivity".

{lib/tt_serverio.i}

define temp-table tt_serverioact no-undo

  field xid        as integer
  field xvalid     as logical

  field srvnum     as integer format ">>>>9"        label "Srv"
  field srvtyp     as character format "x(5)"       label "Type"
  field loginport  as integer format ">>>>9"        label "-S"
  field srvport    as integer format ">>>>9"        label "Port"
  field curr-usr   as integer format ">>>9"         label "Cnx"
  field max-usr    as integer format ">>>9"         label "Max"

  field qry-recv   as decimal format ">>>>>>>9"     label "QryRcvd"
  field recs-qry   as decimal format ">>>>>>>>9"    label "recSndQry"
  field bytes-qry  as decimal format ">>>>>>>>9"    label "bytSndQry"

  field msg-recv   as decimal format ">>>>>>>>9"    label "MsgRcvd"
  field rec-recv   as decimal format ">>>>>>>>9"    label "RecRcvd"
  field rrec-msg   as decimal format ">>9.99"       label "rr/msg"

  field msg-sent   as decimal format ">>>>>>>>9"    label "MsgSent"
  field rec-sent   as decimal format ">>>>>>>>>9"   label "RecSent"
  field srec-msg   as decimal format ">>>9.99"      label "rs/msg"

  field bytes-sent as decimal format ">>>>>>9.99"   label "MB Sent"
  field bytes-recv as decimal format ">>>>>9.99"    label "MB Rcvd"

  field rmsg-sz    as decimal format ">>>>>>9"      label "RcvdSz"
  field smsg-sz    as decimal format ">>>>>>9"      label "SendSz"

  index xid-idx      is unique xid

  index bytes-sent-idx  is primary bytes-sent descending xid descending
  index bytes-recv-idx  bytes-recv descending xid descending

  index msg-sent-idx msg-sent descending xid descending
  index msg-recv-idx msg-recv descending xid descending

  index srvtyp-idx   srvtyp   descending xid descending
  index srvport-idx  srvport  descending xid descending
  index curr-usr-idx curr-usr descending xid descending
  index max-usr-idx  max-usr  descending xid descending
.

define buffer total_serverioact for tt_serverioact.

{lib/dumpTT.i tt_serverioact}

define new global shared variable rowLimit as integer no-undo.

define variable mega as integer no-undo.
mega = 1024 * 1024.

procedure mon-init:

  empty temp-table tt_serverio.
  run updTick.

  return.

end.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable x-dba as decimal no-undo.
  define variable x-dbr as decimal no-undo.
  define variable x-dbw as decimal no-undo.

  define variable i as integer no-undo.
  define variable j as integer no-undo.

  define variable brokerType as character no-undo.
  define variable loginPort  as integer   no-undo.

  define variable qh as handle no-undo.
  define variable bh as handle no-undo.

  create buffer bh for table "_servers".
  create query qh.
  qh:set-buffers( bh ).

  run updTick.

  empty temp-table tt_serverioact.

  for each dictdb._Servers no-lock,
      first dictdb._ActServer no-lock where dictdb._ActServer._Server-Id = dictdb._Servers._Server-Id:

    if _servers._server-type = "inactive" then next.

    run update_serverio( input dictdb._Servers._Server-Id, input x-dba, input x-dbr, input x-dbw ).

    brokerType = ?.
    publish "getBrokerType" ( _Servers._Server-Id, output brokerType ).

    find tt_serverio where tt_serverio.xid =  _Servers._Server-Id no-error.
    if available tt_serverio and brokerType <> ? then tt_serverio.srvtyp = brokerType.

    qh:query-prepare(
      substitute(
        "for each _servers no-lock where _servers._server-broker-pid = &1",
        ( buffer _servers:handle:buffer-field( "_server-broker-pid" ):buffer-value )
      )
    ) no-error.
    if error-status:num-messages = 0 then
      do:
        qh:query-open.
        qh:get-first( no-lock ).
        tt_serverio.loginPort = bh:buffer-field( "_server-portnum" ):buffer-value no-error.
        qh:query-close.
      end.

  end.

  delete object bh.
  delete object qh.

  create total_serverioact.
  assign
    total_serverioact.xid      = 9999
    total_serverioact.xvalid   = yes
    total_serverioact.srvnum   = 9999
    total_serverioact.srvtyp   = "Total"
  .

  for each tt_serverio:

    i = i + 1.

&IF DEFINED( OE10 ) &THEN
    tt_serverio.srvport = unsignMe( tt_serverio.srvport, 16 ).
&ENDIF

    create tt_serverioact.
    assign
      tt_serverioact.xid      = tt_serverio.xid
      tt_serverioact.srvnum   = tt_serverio.srvnum
      tt_serverioact.srvtyp   = tt_serverio.srvtyp
      tt_serverioact.loginPort = tt_serverio.loginPort
      tt_serverioact.srvport  = tt_serverio.srvport
      tt_serverioact.curr-usr = tt_serverio.curr-usr
      tt_serverioact.max-usr  = tt_serverio.max-usr

      tt_serverioact.msg-recv = tt_serverio.msg-recv[x] / z
      tt_serverioact.msg-sent = tt_serverio.msg-sent[x] / z
      tt_serverioact.rec-recv = tt_serverio.rec-recv[x] / z
      tt_serverioact.rec-sent = tt_serverio.rec-sent[x] / z
      tt_serverioact.qry-recv = tt_serverio.qry-recv[x] / z

      tt_serverioact.bytes-sent = (( tt_serverio.bytes-sent[x] / z ) / mega )
      tt_serverioact.bytes-recv = (( tt_serverio.bytes-recv[x] / z ) / mega )

      tt_serverioact.recs-qry    = ( if tt_serverioact.qry-recv = 0 then 0 else tt_serverioact.rec-sent / tt_serverioact.qry-recv )
      tt_serverioact.bytes-qry   = ( if tt_serverioact.qry-recv = 0 then 0 else ( tt_serverioact.bytes-sent * mega ) / tt_serverioact.qry-recv )

      tt_serverioact.rrec-msg   = ( if tt_serverioact.msg-recv = 0 then 0 else tt_serverioact.rec-recv / tt_serverioact.msg-recv )
      tt_serverioact.srec-msg   = ( if tt_serverioact.msg-sent = 0 then 0 else tt_serverioact.rec-sent / tt_serverioact.msg-sent )

      tt_serverioact.rmsg-sz    = ( if tt_serverioact.msg-recv = 0 then 0 else ( tt_serverioact.bytes-recv * mega ) / tt_serverioact.msg-recv )
      tt_serverioact.smsg-sz    = ( if tt_serverioact.msg-sent = 0 then 0 else ( tt_serverioact.bytes-sent * mega ) / tt_serverioact.msg-sent )

      total_serverioact.curr-usr   = total_serverioact.curr-usr   + tt_serverioact.curr-usr
      total_serverioact.max-usr    = total_serverioact.max-usr    + tt_serverioact.max-usr
      total_serverioact.msg-recv   = total_serverioact.msg-recv   + tt_serverioact.msg-recv
      total_serverioact.msg-sent   = total_serverioact.msg-sent   + tt_serverioact.msg-sent
      total_serverioact.rec-recv   = total_serverioact.rec-recv   + tt_serverioact.rec-recv
      total_serverioact.rec-sent   = total_serverioact.rec-sent   + tt_serverioact.rec-sent
      total_serverioact.bytes-recv = total_serverioact.bytes-recv + tt_serverioact.bytes-recv
      total_serverioact.bytes-sent = total_serverioact.bytes-sent + tt_serverioact.bytes-sent
      total_serverioact.qry-recv   = total_serverioact.qry-recv   + tt_serverioact.qry-recv

      total_serverioact.rrec-msg   = total_serverioact.rec-recv / total_serverioact.msg-recv
      total_serverioact.srec-msg   = total_serverioact.rec-sent / total_serverioact.msg-sent

    .

    /*** if ( rowLimit > 0 ) and ( i >= rowLimit ) then leave. ***/

  end.

  assign
    total_serverioact.rrec-msg   = total_serverioact.rec-recv   / total_serverioact.msg-recv
    total_serverioact.srec-msg   = total_serverioact.rec-sent   / total_serverioact.msg-sent
    total_serverioact.recs-qry   = ( if total_serverioact.qry-recv = 0 then 0 else total_serverioact.rec-sent   / total_serverioact.qry-recv )
    total_serverioact.bytes-qry  = ( if total_serverioact.qry-recv = 0 then 0 else ( total_serverioact.bytes-sent * mega ) / total_serverioact.qry-recv )
    total_serverioact.rmsg-sz    = ( total_serverioact.bytes-recv * mega ) / total_serverioact.msg-recv
    total_serverioact.smsg-sz    = ( total_serverioact.bytes-sent * mega ) / total_serverioact.msg-sent
  .

  release total_serverioact.		/* don't ask... I have no clue */

  add2ds( temp-table tt_serverioact:default-buffer-handle ).

  return.

end.

return.

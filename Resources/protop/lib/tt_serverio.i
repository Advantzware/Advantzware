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
 * tt_serverio.i
 *
 */

define temp-table tt_serverio no-undo
  field xid      as integer
  field xvalid   as logical
  field srvnum   as integer           format ">>>9"       label "Srv"
  field srvtyp   as character         format "x(5)"       label "Type"
  field loginPort as integer          format ">>>>9"      label "-S"
  field srvport  as integer           format ">>>>9"      label "Port"
  field curr-usr as integer           format ">>>9"       label "Cnx"
  field max-usr  as integer           format ">>>9"       label "Max"

  field msg-recv as decimal extent 5  format ">>>>>>>>>9" label "MsgRecv"
  field msg-sent as decimal extent 5  format ">>>>>>>>>9" label "MsgSent"
  field rec-recv as decimal extent 5  format ">>>>>>>>>9" label "RecRecv"
  field rec-sent as decimal extent 5  format ">>>>>>>>>9" label "RecSent"
  field qry-recv as decimal extent 5  format ">>>>>>>>>9" label "QrySent"
  field bytes-sent as decimal extent 5  format ">>>>>>>>>9" label "BytesSent"
  field bytes-recv as decimal extent 5  format ">>>>>>>>>9" label "BytesRecv"
  field tm-intr  as decimal extent 5  format ">>>>>>>>>9" label "Slice"

  field srv-dba  as decimal extent 5  format ">>>>>>>>>9" label "LogRd"
  field srv-dbr  as decimal extent 5  format ">>>>>>>>>9" label "OSRd"
  field srv-dbw  as decimal extent 5  format ">>>>>>>>>9" label "OSWr"
  field hr       as decimal           format ">>9.99%"    label "HitPct"

  index xid-idx is unique primary xid
.

procedure update_serverio:

  define input parameter xid      as integer   no-undo.
  define input parameter logRd    as decimal   no-undo.
  define input parameter OSRd     as decimal   no-undo.
  define input parameter OSWr     as decimal   no-undo.

  find dictdb._Servers no-lock where _Servers._Server-id = xid no-error.

  if not available _Servers then
    do:
      message "_Server disappeared!".
      pause.
    end.

  find dictdb._ActServer no-lock where _ActServer._Server-Id = _Servers._Server-Id no-error.

  find tt_serverio where tt_serverio.xid = xid no-error.

  if not available tt_serverio then
    do:

      create tt_serverio.
      assign
        tt_serverio.xvalid   = yes
        tt_serverio.xid      = _Servers._Server-Id
        tt_serverio.srvnum   = _Servers._Server-num
        tt_serverio.srvtyp   = _Servers._Server-type
        tt_serverio.srvport  = _Servers._Server-portNum
        tt_serverio.curr-usr = _Servers._Server-CurrUsers
        tt_serverio.max-usr  = _Servers._Server-MaxUsers
        {lib/init-xrec.i tt_serverio.msg-recv _ActServer._Server-MsgRec}
        {lib/init-xrec.i tt_serverio.msg-sent _ActServer._Server-MsgSent}
        {lib/init-xrec.i tt_serverio.rec-recv _ActServer._Server-RecRec}
        {lib/init-xrec.i tt_serverio.rec-sent _ActServer._Server-RecSent}
        {lib/init-xrec.i tt_serverio.qry-recv _ActServer._Server-QryRec}
        {lib/init-xrec.i tt_serverio.tm-intr  _ActServer._Server-TimeSlice}

        {lib/init-xrec.i tt_serverio.bytes-recv _ActServer._Server-ByteRec}
        {lib/init-xrec.i tt_serverio.bytes-sent _ActServer._Server-ByteSent}

        {lib/init-xrec.i tt_serverio.srv-dba  logRd}
        {lib/init-xrec.i tt_serverio.srv-dbr  OSRd}
        {lib/init-xrec.i tt_serverio.srv-dbw  OSWr}
      .

    end.

  assign
    tt_serverio.xvalid = yes

    tt_serverio.srvtyp   = _Servers._Server-type
    tt_serverio.srvport  = _Servers._Server-portNum
    tt_serverio.curr-usr = _Servers._Server-CurrUsers
    tt_serverio.max-usr  = _Servers._Server-MaxUsers

    {lib/upd-xrec.i tt_serverio.msg-recv _ActServer._Server-MsgRec}
    {lib/upd-xrec.i tt_serverio.msg-sent _ActServer._Server-MsgSent}
    {lib/upd-xrec.i tt_serverio.rec-recv _ActServer._Server-RecRec}
    {lib/upd-xrec.i tt_serverio.rec-sent _ActServer._Server-RecSent}
    {lib/upd-xrec.i tt_serverio.qry-recv _ActServer._Server-QryRec}
    {lib/upd-xrec.i tt_serverio.tm-intr  _ActServer._Server-TimeSlice}

    {lib/upd-xrec.i tt_serverio.bytes-recv _ActServer._Server-ByteRec}
    {lib/upd-xrec.i tt_serverio.bytes-sent _ActServer._Server-ByteSent}

    {lib/upd-xrec.i tt_serverio.srv-dba  logRd}
    {lib/upd-xrec.i tt_serverio.srv-dbr  OSRd}
    {lib/upd-xrec.i tt_serverio.srv-dbw  OSWr}
  .

  return.

end.

procedure age_serverio:

  for each tt_serverio:

    if tt_serverio.xvalid = no then
      delete tt_serverio.
     else
      assign
        tt_serverio.xvalid = no
/*
        {lib/upd-xrec.i tt_serverio.msg-recv tt_serverio.msg-recv[3]}
        {lib/upd-xrec.i tt_serverio.msg-sent tt_serverio.msg-sent[3]}
        {lib/upd-xrec.i tt_serverio.rec-recv tt_serverio.rec-recv[3]}
        {lib/upd-xrec.i tt_serverio.rec-sent tt_serverio.rec-sent[3]}
        {lib/upd-xrec.i tt_serverio.qry-recv tt_serverio.qry-recv[3]}
        {lib/upd-xrec.i tt_serverio.tm-intr  tt_serverio.tm-intr[3]}

        {lib/upd-xrec.i tt_serverio.bytes-recv tt_serverio.bytes-recv[3]}
        {lib/upd-xrec.i tt_serverio.bytes-sent tt_serverio.bytes-sent[3]}

        {lib/upd-xrec.i tt_serverio.srv-dba tt_serverio.srv-dba[3]}
        {lib/upd-xrec.i tt_serverio.srv-dbr tt_serverio.srv-dbr[3]}
        {lib/upd-xrec.i tt_serverio.srv-dbw tt_serverio.srv-dbw[3]}
        tt_serverio.hr  = 100 * (( tt_serverio.srv-dba[x] - tt_serverio.srv-dbr[x] ) / tt_serverio.srv-dba[x] )
        tt_serverio.hr  = ( if tt_serverio.hr = ? then 0 else tt_serverio.hr )
 */
      .

  end.

  return.

end.

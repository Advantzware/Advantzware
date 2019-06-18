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
 * brokers.p
 *
 *
 * broker configuration
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

define output parameter dcDescription as character no-undo initial "BrokerConfig".

define temp-table tt_brokerConfig no-undo
  field xid           as integer   format ">>9"             label "Id"
  field brokerType    as character format "x(5)"            label "Type"
  field brokerIPver   as character format "x(5)"            label "IPver"
  field brokerPID     as integer   format ">>>>>>>9"        label "PID"
  field brokerPort    as integer   format ">>>>9"           label "-S"
  field brokermin     as integer   format ">>>>9"           label "-minport"
  field brokermax     as integer   format ">>>>9"           label "-maxport"
  field brokerPCT     as integer   format ">>>>9"           label "-PendCx"
  field brokerMm      as integer   format ">>>>9"           label "-Mm"
  field brokerMpb     as integer   format ">>>>9"           label "-Mpb"
  field brokerMa      as integer   format ">>>>9"           label "-Ma"
  field brokerMi      as integer   format ">>>>9"           label "-Mi"

  field brokerCnx     as integer   format ">>>>9"           label "Cnx"
  field brokerCnxPct  as decimal   format  ">>9%"           label "Cnx%"

  field brokerSQLttlg as character format "x(6)"            label "SQLTrc"
  field brokerSQLwup  as character format "x(6)"            label "SQLWUp"

  field brokerSQLcurs as integer format ">>>>>>>9"          label "SQLcurs"
  field brokerSQLstck as integer format ">>>>>>>9"          label "SQLstck"
  field brokerSQLstmt as integer format ">>>>>>>9"          label "SQLstmt"
  field brokerSQLtmpb as integer format ">>>>>>>9"          label "SQLtmpb"
  field brokerSQLtmpd as integer format ">>>>>>>9"          label "SQLtmpd"
  field brokerSQLtmpz as integer format ">>>>>>>9"          label "SQLtmpz"

  index brokerType-idx is unique primary brokerType xid
  index xid-idx        is unique xid
.

{lib/dumpTT.i tt_brokerConfig}

/* offsets for particular parameters -- these may vary by release
 */

define variable svt   as integer no-undo.
define variable mpb   as integer no-undo.
define variable mm    as integer no-undo.
define variable ma    as integer no-undo.
define variable mi    as integer no-undo.
define variable minp  as integer no-undo.
define variable maxp  as integer no-undo.
define variable ipv   as integer no-undo.
define variable pct   as integer no-undo.

define variable sqlttl as integer no-undo.
define variable sqlwu  as integer no-undo.
define variable sqlc   as integer no-undo.
define variable sqls   as integer no-undo.
define variable sqlsc  as integer no-undo.
define variable sqltsb as integer no-undo.
define variable sqltsd as integer no-undo.
define variable sqltsp as integer no-undo.

define buffer broker for _servers.


define variable hasSrvParam as logical no-undo.

procedure chkSrvParam:

  find _file no-lock where _file-name = "_servers" no-error.
  if available _file then find _field no-lock of _file where _field-name = "_srvParam-name" no-error.
  hasSrvParam = available( _field ).

end.


procedure mon-init:

  define variable i as integer   no-undo.
  define variable p as character no-undo.

  run chkSrvParam.

  if hasSrvParam = yes then
    do:

      find first _servers no-lock where _servers._server-type = "login" no-error.
      if available _servers then
        do i = 1 to 64:

          p = buffer _servers:handle:buffer-field( "_srvParam-name" ):buffer-value(i).

          case p:

            when "-ServerType"      then svt  = i.
            when "-Mpb"             then mpb  = i.
            when "-Mm"              then mm   = i.
            when "-Ma"              then ma   = i.
            when "-Mi"              then mi   = i.
            when "-minport"         then minp = i.
            when "-maxport"         then maxp = i.
            when "-ipver"           then ipv  = i.
            when "-PendConnTimeout" then pct  = i.

            when "-SQLTruncateTooLarge"  then sqlttl = i.
            when "-SQLWidthUpdate"       then sqlwu  = i.
            when "-SQLCursors"           then sqlc   = i.
            when "-SQLStack"             then sqls   = i.
            when "-SQLStmtCache"         then sqlsc  = i.
            when "-SQLTempStoreBuff"     then sqltsb = i.
            when "-SQLTempStoreDisk"     then sqltsd = i.
            when "-SQLTempStorePageSize" then sqltsp = i.

          end.

        end.

    end.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable numBrokers as integer no-undo.

  run updTick.

  empty temp-table tt_brokerConfig.

  for each broker no-lock where broker._server-type = "login":

    numBrokers = numBrokers + 1.

    create tt_brokerConfig.
    assign
      xid         = broker._server-Id
      brokerPort  = broker._server-portnum
      brokerPID   = broker._server-pid
    .

    if hasSrvParam = yes then
      do:

        if svt  > 0 then brokerType  = buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(svt).
        if ipv  > 0 then brokerIPver = buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(ipv).

        if pct  > 0 then brokerPCT = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(pct) ) no-error.
        if mm   > 0 then brokerMm  = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(mm) ) no-error.
        if mpb  > 0 then brokerMpb = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(mpb) ) no-error.
        if ma   > 0 then brokerMa  = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(ma) ) no-error.
        if mi   > 0 then brokerMi  = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(mi) ) no-error.
        if minp > 0 then brokermin = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(minp) ) no-error.
        if maxp > 0 then brokermax = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(maxp) ) no-error.

        if sqlttl > 0 then brokerSQLttlg = buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqlttl).
        if sqlwu  > 0 then brokerSQLwup  = buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqlwu).

        if sqlc   > 0 then brokerSQLcurs = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqlc) ) no-error.
        if sqls   > 0 then brokerSQLstck = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqls) ) no-error.
        if sqlsc  > 0 then brokerSQLstmt = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqlsc) ) no-error.
        if sqltsb > 0 then brokerSQLtmpb = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqltsb) ) no-error.
        if sqltsd > 0 then brokerSQLtmpd = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqltsd) ) no-error.
        if sqltsp > 0 then brokerSQLtmpz = integer( buffer broker:handle:buffer-field( "_srvParam-value" ):buffer-value(sqltsp) ) no-error.

        brokerCnx = 0.  /* don't count self */
        for each _servers no-lock where integer( buffer _servers:handle:buffer-field( "_server-broker-pid" ):buffer-value ) = brokerPID:
          brokerCnx = brokerCnx + integer( buffer _servers:handle:buffer-field( "_server-currUsers" ):buffer-value ).
        end.
        brokerCnxPct = 100 * ( brokerCnx / ( brokerMpb * brokerMa )).

      end.

    if brokerPort <= 0 then brokerType = "SHM".

  end.

  publish "resizeBrowse" ( "brokerConfig", numBrokers ).

  add2ds( temp-table tt_brokerConfig:default-buffer-handle ).

  return.

end.


subscribe to "getBrokerType" anywhere run-procedure "getBrokerType".

procedure getBrokerType:

  define input  parameter serverId   as integer   no-undo. 
  define output parameter brokerType as character no-undo. 

  brokerType = ?.

  if svt > 0 then
    do:
      find _servers no-lock where _servers._server-id = serverId no-error.
      if available _servers then
        brokerType  = buffer _servers:handle:buffer-field( "_srvParam-value" ):buffer-value(svt).
    end.

  return.

end.

return.

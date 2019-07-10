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
 * replagent.p
 *
 *
 * replication agent
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

define output parameter dcDescription as character no-undo initial "ReplAgent".

{lib/tt_xstat.i}

define temp-table tt_replAgent no-undo
  field xid           as integer   format ">9"              label "Id"
  field agentName     as character format "x(8)"            label "Agent"
  field agentMethod   as character format "x(8)"            label "Method"
  field remoteHost    as character format "x(16)"           label "Host"
  field remotePort    as decimal   format ">>>>9"           label "Port"
  field agentStatus   as decimal   format ">>>>>9"          label "Status"
  field agentCommStat as decimal   format ">>>>>>>>9"       label "CommStat"
  field blksSent      as decimal   format ">>>>>>>9"        label "Blks Sent"
  field blksACK       as decimal   format ">>>>>>>>9"       label "Blks ACK"
  field lastBlk       as character format "x(20)"           label "       Last Block"
  field zlagTime      as integer   format ">>>>>>>>9"       label "zLagTime"
  field lagTime       as character format "x(15)"           label "     TRX Lag"
  field remoteDBName  as character format "x(40)"           label "Remote DB Name"

/***
  field blksBehind    as decimal   format ">>>>>>>>9"       label "Blks Behind"
  field ztrxBehind    as integer   format "->>>>>>>>9"      label "TRX Behind"
  field zsecBehind    as integer   format "->>>>>>>>9"      label "Sec Behind"
 ***/

  index xid-idx       is unique xid
.

{lib/dumpTT.i tt_replAgent}

/* convert strings like: "Sun Dec  8 12:35:28 2013"
 *
 */

function convTS returns character ( input goofy as character ):

  define variable xtime as character no-undo.
  define variable xmth  as integer   no-undo.
  define variable xday  as integer   no-undo.
  define variable xyear as integer   no-undo.

  assign
    xtime = substring( goofy, 12, 8 )
    xmth  = lookup(  substring( goofy,  5, 3 ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" )
    xday  = integer( substring( goofy,  9, 2 ))
    xyear = integer( substring( goofy, 23, 2 ))
  .

  return substitute(					/* display ymd date format!	*/
    "&1/&2/&3 &4",
    string( xyear, "99" ),
    string( xmth,  "99" ),
    string( xday,  "99" ),
    xtime
  ).

end.

procedure mon-init:

  if not isReplSource() then return.

  if not connected( "replTarget" ) then
    do:
      run lib/connectrepl.p. 
      if connected( "replTarget" ) then
        run lib/repltarget.p persistent.
    end.

  empty temp-table tt_xstat.

  run updTick.

  return.

end.


procedure replAgent:

  define input parameter agentId as integer no-undo.

  define variable targetLastTRX  as integer no-undo.
  define variable targetBlksRecv as integer no-undo.
  define variable targetBlksProc as integer no-undo.

  define variable xBlocks        as integer no-undo.
  define variable zBehind        as integer no-undo.

  find first dictdb._dbStatus no-lock no-error.
  if not available dictdb._dbStatus then
    do:
      message "dc/replagent.p: wtf? _dbStatus has gone missing...".
    end.

  if not isReplSource() and not isReplTarget() then return.

  find dictdb._repl-AgentControl no-lock where _replAgtCtl-AgentId = agentId no-error.
  find dictdb._repl-Agent        no-lock where _replAgt-AgentId    = agentId no-error.

  find tt_replAgent where tt_replAgent.xid = agentId no-error.
  if not available( tt_replAgent ) then
    create tt_replAgent.

  if not available dictdb._repl-agent and connected( "replTarget" ) then
    do:

      /* "trxBehind" is difficult to track:  on the source _dbStatus-lastTran includes *uncommitted* transactions
       * but on the target only committed TRX are recorded.  The "blocks sent" and "blocks received" metrics are
       * relative to the *server* and the *agent* rather than their respective databases -- so you cannot use those
       * to determine if you are caught up (they can get way out of sync as servers and agents can start & stop
       * differently than the databases they are connected to).  and blocks acknowledged is apparently not updated.
       *
       * so... if there is no active transaction (<> "allocated") and blocksRecv = blocksProc we assume that we
       * are caught up.  and you know what they say about assumptions...
       *
       * there is supposed to be some new _repl VST stuff coming (11.7?) that will hopefully fix all of this...
       *
       */

      publish "replTargetLastTRX" ( output targetLastTRX, output targetBlksRecv, output targetBlksProc ).

      /*** tt_replAgent.ztrxBehind = ( dictdb._dbStatus._dbStatus-LastTran - targetLastTRX ). ***/

      find last _Trans no-lock where _Trans-usrnum <> ? and _Trans-state <> "allocated" no-error.

      /*** if ( not available _Trans ) and ( targetBlksRecv = targetBlksProc ) then tt_replAgent.ztrxBehind = 0. ***/

    end.
   else
    do:
      /*** tt_replAgent.ztrxBehind = ?. ***/
    end.

  if available dictdb._repl-Agent then
    do:
      assign
        tt_replAgent.xid           = agentId
        tt_replAgent.agentName     = _ReplAgt-agentName
        tt_replAgent.agentMethod   = ( if _ReplAgt-method = "A" then "Async" else "Sync" )
        tt_replAgent.remoteHost    = _ReplAgt-serverHost
        tt_replAgent.remotePort    = _ReplAgt-port
        tt_replAgent.agentStatus   = _ReplAgt-status
        tt_replAgent.agentCommStat = _ReplAgt-commStatus
        tt_replAgent.remoteDBName  = _ReplAgt-DBName
      .
      run update_xstat (
        input _ReplAgt-AgentId,
        input _ReplAgt-AgentName,
        input "m1",
        input "m2",
        input "m3",
        input "m4",
        input "m5",
        input _ReplAgt-BlocksReceived,
        input _ReplAgt-BlocksACK,
        input 0,
        input 0,
        input 0,
        input 0
      ).
    end.

  if available dictdb._repl-AgentControl then
    do:

      assign
        tt_replAgent.lastBlk       = convTS( _ReplAgtCtl-lastBlockSentAt )	/* UDF has to be before indexed fields in old releases  */
        tt_replAgent.xid           = agentId
        tt_replAgent.agentName     = _ReplAgtCtl-agentName
        tt_replAgent.agentMethod   = ( if _ReplAgtCtl-method = "A" then "Async" else "Sync" )
        tt_replAgent.remoteHost    = _ReplAgtCtl-remoteHost
        tt_replAgent.remotePort    = _ReplAgtCtl-port
        tt_replAgent.agentStatus   = _ReplAgtCtl-status
        tt_replAgent.agentCommStat = _ReplAgtCtl-commStatus
        tt_replAgent.remoteDBName  = _ReplAgtCtl-remoteDBName
&IF DEFINED( OE10 ) &THEN
        zBehind                    = interval( now, datetime( tt_replAgent.lastBlk ), "seconds" )
&ENDIF
        tt_replAgent.lagTime       = substitute( "&1 &2", string( integer( truncate( zBehind / 86400, 0 )), ">>>" ), string( zBehind, "hh:mm:ss" ))
        tt_replAgent.zlagTime      = zBehind
      .

      find last _Trans no-lock where _Trans-usrnum <> ? and _Trans-state <> "allocated" no-error.
      if ( not available _Trans ) and ( targetBlksRecv = targetBlksProc ) then
        do:
          tt_replAgent.zlagTime = zBehind.
          tt_replAgent.lagTime  = "    00:00:00".
        end.

      run update_xstat (
        input _ReplAgtCtl-AgentId,
        input _ReplAgtCtl-AgentName,
        input "m1",
        input "m2",
        input "m3",
        input "m4",
        input "m5",
        input _ReplAgtCtl-BlocksSent,
        input _ReplAgtCtl-BlocksACK,
        input 0,
        input 0,
        input 0,
        input 0
      ).
    end.


  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  run updTick.

  find first dictdb._repl-Agent no-lock no-error.

  if available dictdb._repl-Agent then
    run replAgent ( _ReplAgt-AgentId ).
   else
    for each dictdb._Repl-AgentControl no-lock:
      run replAgent ( _ReplAgtCtl-AgentId ).
    end.

  run age_xstat.

  for each tt_xstat no-lock:

    find tt_replAgent where tt_replAgent.xid = tt_xstat.xid no-error.

    assign
      tt_replAgent.blksSent  = tt_xstat.stat1[x] / z
      tt_replAgent.blksACK   = tt_xstat.stat2[x] / z
    .

  end.

  add2ds( temp-table tt_replAgent:default-buffer-handle ).

  return.

end.

return.

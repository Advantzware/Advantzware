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
 * replication.p
 *
 *
 * Replication Information.
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

define output parameter dcDescription as character no-undo initial "ReplicationInfo".

define temp-table tt_ReplInfo no-undo

  field xid            as integer   format ">>9"             label "Id"

  field dbStartTS      as character format "x(15)"           label "Src DB Start"
  field replStartTS    as character format "x(15)"           label "Repl Agent Start"
  field replMethod     as character format "x(8)"            label "Repl Method"
  field numTargets     as integer   format "->>>>>>>>>>>>>9" label "# Targets"
  field dbTRXId        as decimal   format "->>>>>>>>>>>>>9" label "Src TRX Id"
  field blksSent       as decimal   format "->>>>>>>>>>>>>9" label "Blocks Sent"
  field lastBlkTS      as character format "x(15)"           label "Last Block Sent"

  field srcHost1       as character format "x(30)"           label ""
  field srcPort1       as character format "x(8)"            label ""
  field srcDBTRX1      as decimal   format ">>>>>>>>>>>>>>9" label ""

  field agent1         as character format "x(30)"           label ""
  field replDB1        as character format "x(40)"           label ""
  field replDB1StartTS as character format "x(30)"           label ""

  field agent1ConnTS   as character format "x(30)"           label ""
  field agent1Status   as character format "x(20)"           label ""
  field agent1Comms    as character format "x(8)"            label ""

  field repl1TRX       as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl1BlksRecvd as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl1BlksPrcd  as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl1BlksACK   as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl1NotesPrcd as decimal   format ">>>>>>>>>>>>>>9" label ""

  field srcHost2       as character format "x(30)"           label ""
  field srcPort2       as character format "x(8)"            label ""
  field srcDBTRX2      as decimal   format ">>>>>>>>>>>>>>9" label ""

  field agent2         as character format "x(30)"           label ""
  field replDB2        as character format "x(40)"           label ""
  field replDB2StartTS as character format "x(30)"           label ""

  field agent2ConnTS   as character format "x(30)"           label ""
  field agent2Status   as character format "x(20)"           label ""
  field agent2Comms    as character format "x(8)"            label ""

  field repl2TRX       as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl2BlksRecvd as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl2BlksPrcd  as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl2BlksACK   as decimal   format ">>>>>>>>>>>>>>9" label ""
  field repl2NotesPrcd as decimal   format ">>>>>>>>>>>>>>9" label ""

/*
  index resrcWts-idx  is unique resrcWts descending xid
  index requests-idx  is unique primary requests descending xid
  index lockPct-idx   is unique lockPct descending xid
  index resrcName-idx is unique resrcName
 */

  index xid-idx       is unique xid
.

{lib/dumpTT.i tt_ReplInfo}

procedure mon-init:

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  run updTick.

  find first tt_ReplInfo no-error.
  if not available( tt_ReplInfo ) then create tt_ReplInfo.

  /*** the main event... ***/

  find dictdb._dbStatus no-lock no-error.

  assign
    tt_ReplInfo.dbStartTS = substring( substring( _dbStatus._dbStatus-StartTime, 5 ), 1, 15 )
    tt_ReplInfo.dbTRXId   = _dbStatus._dbStatus-LastTran
  .

  find dictdb._Repl-Server no-lock no-error.
  if available dictdb._Repl-Server then
    assign
      tt_ReplInfo.replStartTS = substring( substring( _Repl-Server._ReplSrv-StartTime,  5 ), 1, 15 )
      tt_ReplInfo.numTargets  = _Repl-Server._ReplSrv-AgentCount
      tt_ReplInfo.blksSent    = _Repl-Server._ReplSrv-BlocksSent
      tt_ReplInfo.lastBlkTS   = substring( substring( _Repl-Server._ReplSrv-LastBlockSentAt,  5 ), 1, 15 )
    . 

/*

  find replTarget._dbStatus no-lock no-error.
  find replTarget._Repl-Agent no-lock no-error.

  find first replSource._Repl-AgentControl no-lock
    where replSource._Repl-AgentControl._ReplAgtCtl-AgentName = replTarget._Repl-Agent._ReplAgt-AgentName.

  if available replTarget._Repl-Agent then

  replTarget._Repl-Agent._ReplAgt-AgentName       format "x(15)"       colon 12 label "Agent"       skip
  replTarget._Repl-Agent._ReplAgt-ServerHost      format "x(15)"       colon 12 label "Source Host" skip
  replTarget._Repl-Agent._ReplAgt-Port            format ">>>>>9"      colon 12 label "Port"        skip
  replTarget._Repl-Agent._ReplAgt-dbName          format "x(30)"       colon 12 label "Source DB"   skip
  dummy                                           format "x(30)"       colon 12 label "Target DB"   skip
  replTarget._dbStatus._dbStatus-StartTime        format "x(24)"       colon 12 label "DB Start"    skip
  replTarget._Repl-Agent._ReplAgt-ConnectTime     format "x(24)"       colon 12 label "Agt Conn"    skip
  replTarget._Repl-Agent._ReplAgt-Status          format ">>>>>9"      colon 12 label "Status"      skip
  replTarget._Repl-Agent._ReplAgt-CommStatus      format ">>>>>9"      colon 12 label "Comms"       skip
  replTarget._Repl-Agent._ReplAgt-Method          format "x(6)"        colon 12 label "Method"      skip
  replTarget._dbStatus._dbStatus-LastTran         format ">>>>>>>>>>9" colon 12 label "DB TRX Id"   skip
  replTarget._Repl-Agent._ReplAgt-LastTRID        format ">>>>>>>>>>9" colon 12 label "Repl TRX"    skip
  replTarget._Repl-Agent._ReplAgt-BlocksReceived                       colon 12 label "Blks Rcv"    skip
  replTarget._Repl-Agent._ReplAgt-BlocksProcessed                      colon 12 label "Blks Proc"   skip
  replTarget._Repl-Agent._ReplAgt-BlocksACK                            colon 12 label "Blks ACK"    skip
  replTarget._Repl-Agent._ReplAgt-NotesProcessed                       colon 12 label "Notes Proc"  skip

 */

  add2ds( temp-table tt_replInfo:default-buffer-handle ).

  return.

end.

return.

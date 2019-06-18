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
 * netstat.p
 *
 * Network Statistics
 *
 * Author:
 *
 *	Tom Bascom, Greenfield Technologies
 *	http://www.greenfieldtech.com
 *	November 6, 2003
 *
 */

{lib/protop.i}
{lib/tick.i}

{lib/protoplib.i}

define output parameter dcDescription as character no-undo initial "netstat".

define temp-table tt_nstat no-undo
  field dev         as character
  field net         as character
  field addr        as character
  field mtu         as integer
  field pckt-rx     as decimal extent 5
  field pckt-rx-err as decimal extent 5
  field pckt-tx     as decimal extent 5
  field pckt-tx-err as decimal extent 5
  index dev-idx is unique primary dev.

define temp-table tt_netStat no-undo
  field xid         as integer   format ">>>9"
  field dev         as character label "Interface"   format "x(18)"
  field net         as character label "Network"     format "x(18)"
  field addr        as character label "Address"     format "x(18)"
  field mtu         as decimal   label "MTU"         format ">>>>>9"
  field pckt-rx     as decimal   label "Received"    format ">>>>>>>>>>>>>>9"
  field pckt-rx-err as decimal   label "RX Err"      format ">>>>>>>>>>>>>>9"
  field pckt-tx     as decimal   label "Transmitted" format ">>>>>>>>>>>>>>9"
  field pckt-tx-err as decimal   label "TX Err"      format ">>>>>>>>>>>>>>9"
  index dev-idx is unique primary dev
  index xid-idx is unique xid
.

{lib/dumpTT.i tt_netStat}

define variable xdev         as character no-undo.
define variable xnet         as character no-undo.
define variable xaddr        as character no-undo.
define variable xmtu         as integer   no-undo.
define variable xpckt-rx     as decimal   no-undo.
define variable xpckt-rx-err as decimal   no-undo.
define variable xpckt-tx     as decimal   no-undo.
define variable xpckt-tx-err as decimal   no-undo.

define variable xsentinel    as character no-undo.
define variable osName       as character no-undo.


procedure getWinNetStat:

  return.
  
end.


procedure getNetStat:

  if osName = "Windows" then return.

/* 
 * proenv>netstat -e
 * Interface Statistics
 * 
 *                            Received            Sent
 * 
 * Bytes                     145791184       593757391
 * Unicast packets             9212606         5940630
 * Non-unicast packets          939064          152220
 * Discards                        428               0
 * Errors                            0               0
 * Unknown protocols                 0
 * 
 */

  input stream inStrm through value( "netstat -i" ).

  import stream inStrm ^.							/* eat the header line				*/

  if lookup( osName, "HP-UX,Linux" ) > 0 then import stream inStrm ^.		/* some OS' have an extra header line		*/

  repeat:

    if osName = "AIX" then xsentinel = "xxx".

    xpckt-rx = -1.

    case osName:
      when "AIX"     then import stream inStrm xdev xmtu xnet xaddr xpckt-rx xpckt-rx-err xpckt-tx xpckt-tx-err xsentinel no-error.
/*    when "HP-UX"   then import stream inStrm xdev xmtu xnet xaddr xpckt-rx xpckt-rx-err ^ ^ xpckt-tx xpckt-tx-err no-error. */
      when "HP-UX"   then import stream inStrm xdev xmtu xnet xaddr xpckt-rx xpckt-rx-err xpckt-tx xpckt-tx-err no-error.
      when "Linux"   then import stream inStrm xdev xmtu ^          xpckt-rx xpckt-rx-err ^ ^ xpckt-tx xpckt-tx-err no-error.
      when "SunOS"   then import stream inStrm xdev xmtu xnet xaddr xpckt-rx xpckt-rx-err xpckt-tx xpckt-tx-err no-error.
      when "Windows" then import stream inStrm xdev xmtu xnet xaddr xpckt-rx xpckt-rx-err xpckt-tx xpckt-tx-err no-error.
      otherwise return.
    end.

    if xsentinel = "xxx" then next.

    if osName = "HP-UX" and xpckt-rx = -1 then
      import stream inStrm xpckt-rx xpckt-rx-err xpckt-tx xpckt-tx-err no-error.

    run update_netstat(
      input xdev,
      input xnet,
      input xaddr,
      input xmtu,
      input xpckt-rx,
      input xpckt-rx-err,
      input xpckt-tx,
      input xpckt-tx-err
    ).

  end.

  input stream inStrm close.

end.

procedure update_netstat:

  define input parameter p_dev         as character no-undo.
  define input parameter p_net         as character no-undo.
  define input parameter p_addr        as character no-undo.
  define input parameter p_mtu         as integer   no-undo.
  define input parameter p_pckt-rx     as decimal   no-undo.
  define input parameter p_pckt-rx-err as decimal   no-undo.
  define input parameter p_pckt-tx     as decimal   no-undo.
  define input parameter p_pckt-tx-err as decimal   no-undo.

  find tt_nstat exclusive-lock where tt_nstat.dev = p_dev no-error.

  if not available tt_nstat then
    do:
      create tt_nstat.
      assign
        tt_nstat.dev  = p_dev
        tt_nstat.net  = p_net
        tt_nstat.addr = p_addr
        tt_nstat.mtu  = p_mtu
        {lib/init-xrec.i tt_nstat.pckt-rx     p_pckt-rx}
        {lib/init-xrec.i tt_nstat.pckt-rx-err p_pckt-rx-err}
        {lib/init-xrec.i tt_nstat.pckt-tx     p_pckt-tx}
        {lib/init-xrec.i tt_nstat.pckt-tx-err p_pckt-tx-err}
      .
    end.

  assign
    tt_nstat.pckt-rx[3] =     p_pckt-rx
    tt_nstat.pckt-rx-err[3] = p_pckt-rx-err
    tt_nstat.pckt-tx[3] =     p_pckt-tx
    tt_nstat.pckt-tx-err[3] = p_pckt-tx-err
  .

  return.

end.

procedure age_netstat:

  for each tt_nstat exclusive-lock:

    assign
      {lib/upd-xrec.i tt_nstat.pckt-rx     tt_nstat.pckt-rx[3]}
      {lib/upd-xrec.i tt_nstat.pckt-rx-err tt_nstat.pckt-rx-err[3]}
      {lib/upd-xrec.i tt_nstat.pckt-tx     tt_nstat.pckt-tx[3]}
      {lib/upd-xrec.i tt_nstat.pckt-tx-err tt_nstat.pckt-tx-err[3]}
    .

  end.

  return.

end.

/* initialize
 *
 */

procedure mon-init:

  empty temp-table tt_nstat.

  run updTick.

  if opsys = "WIN32" then
    osName = "Windows".
   else
    do:
      input stream inStrm through value( "uname -a" ).
      import stream inStrm osName.
      input stream inStrm close.
    end.

  return.

end.

/* update
 *
 */

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i as integer no-undo.

  run updTick.

  if opsys begins "win" then
    run getWinNetStat.
   else
    run getNetStat.

  run age_netstat.

  /* z = 1. */	/* z = 1 turns this into an "interval" metric rather than "per second over the interval" */

  empty temp-table tt_netstat.

  for each tt_nstat:
    create tt_netstat.
    assign
      i = i + 1
      tt_netstat.xid         = i
      tt_netstat.dev         = tt_nstat.dev
      tt_netstat.net         = tt_nstat.net
      tt_netstat.addr        = tt_nstat.addr
      tt_netstat.mtu         = tt_nstat.mtu
      tt_netstat.pckt-rx     = tt_nstat.pckt-rx[x] / z
      tt_netstat.pckt-rx-err = tt_nstat.pckt-rx-err[x] / z
      tt_netstat.pckt-tx     = tt_nstat.pckt-tx[x] / z
      tt_netstat.pckt-tx-err = tt_nstat.pckt-tx-err[x] / z
    .
  end.

  publish "resizeBrowse" ( "netstat", i ).

  add2ds( temp-table tt_netstat:default-buffer-handle ).

  return.

end.

return.

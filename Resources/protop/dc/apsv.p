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
 * apsv.p
 *
 *
 * appservers
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

define output parameter dcDescription as character no-undo initial "AppSrvStatus".

define temp-table tt_apsvList no-undo
  field xid          as integer   format ">>9"             label "Id"
  field asType       as character format "x(6)"            label "Type"				/* apsv, webspeed... 		*/
  field asName       as character format "x(20)"           label "Broker"
  field asCommand    as character format "x(30)" extent 2  label "Command"  {&NOSERIALIZE}	/* asbman -i &1 -query		*/
  field asMode       as character format "x(12)"           label "Mode"
  field asPort       as integer   format ">>>>9"           label "Port"
  field asPid        as integer   format ">>>>>>9"         label "PID"
  field asMaxSrvInst as integer   format ">>>>>9"          label "MaxSrv"
  field asActive     as integer   format ">>>>9"           label "Actv"
  field asAvail      as integer   format ">>>>9"           label "Avail"
  field asBusy       as integer   format ">>>>9"           label "Busy"
  field asLocked     as integer   format ">>>>9"           label "Lckd"
  field asSending    as integer   format ">>>>9"           label "Sndg"
  field asStuck      as integer   format ">>>>9"           label "Stuck"
  field zasSrInUse   as integer   format ">>>>9"           label "In Use"
  field asSrUsePct   as integer   format ">>>>9%"          label "Srvr%"
  field asMaxCliInst as integer   format ">>>>>9"          label "MaxCli"
  field asActCli     as integer   format ">>>>>9"          label "ActCli"
  field zasCliHWM    as integer   format ">>>>>9"          label "CliHWM"
  field asClUsePct   as integer   format ">>>>9%"          label "Cli%"
  field asCurrQ      as integer   format ">>>>9"           label "CurrQ"
  field asMaxQ       as integer   format ">>>>9"           label "MaxQ"
  field asMaxWt      as integer   format ">>>>>>>>9"       label "Max Wt"			/* milliseconds */
  field asAvgWt      as integer   format ">>>>>>9"         label "Avg Wt"			/* milliseconds */
  field asMaxLen     as integer   format ">>>>>>9"         label "Max Dur" {&NOSERIALIZE}	/* milliseconds */
  field asAvgLen     as integer   format ">>>>>9"          label "Avg Dur" {&NOSERIALIZE}	/* milliseconds */
  field asNote       as character format "x(80)"           label "Note"    {&NOSERIALIZE}

  index xid-idx    is unique  xid
  index asName-idx is unique  asName asType
  index asType-idx is primary asType
.

{lib/dumpTT.i tt_apsvList}

/* also defined and used in lib/apsvcnx.p... so keep changes in sync!
 */

{lib/tt_apsv.i}

{lib/dumpTT.i tt_apsv}

define variable xasbmanDet   as character no-undo initial "PID State".				/* if it exists etc/translate.cfg will over-ride these initial values	*/
define variable xasbmanSend  as character no-undo initial "Sending".

define variable xOperMode    as character no-undo initial "Operating Mode".
define variable xBrkrName    as character no-undo initial "Broker Name".
define variable xBrkrPort    as character no-undo initial "Broker Port".
define variable xBrkrPID     as character no-undo initial "Broker PID".

define variable xActvAgnts   as character no-undo initial "Active Agents".
define variable xBusyAgnts   as character no-undo initial "Busy Agents".
define variable xLckdAgnts   as character no-undo initial "Locked Agents".
define variable xAvailAgnts  as character no-undo initial "Available Agents".
define variable xActvClnts   as character no-undo initial "Active Clients".

define variable xActvSrvrs   as character no-undo initial "Active Servers".
define variable xBusySrvrs   as character no-undo initial "Busy Servers".
define variable xLckdSrvrs   as character no-undo initial "Locked Servers".
define variable xAvailSrvrs  as character no-undo initial "Available Servers".

define variable xClientQ     as character no-undo initial "Client Queue".
define variable xRqWait      as character no-undo initial "Rq Wait".
define variable xRqDuration  as character no-undo initial "Rq Duration".

define variable xMonthList   as character no-undo initial "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec".


define variable cfgFile as character no-undo.
define variable cfgDate as date      no-undo.
define variable cfgTime as integer   no-undo.

procedure chkConfig:

  define variable inData as character no-undo extent 64.

  define variable i as integer no-undo.

  if cfgFile = "" or cfgFile = ? then run findCfgName( "appsrv", input-output cfgFile ).

  file-info:file-name = cfgFile.

  if file-info:full-pathname = ? then
    do:
      cfgFile = "".
      return.
    end.

  if cfgDate = file-info:file-mod-date and cfgTime = file-info:file-mod-time then return.

  message {&NOW} cfgfile.
  /*** pause. ***/

  assign
    cfgDate = file-info:file-mod-date
    cfgTime = file-info:file-mod-time
  .

  empty temp-table tt_apsvList.

  input stream inStrm from value( file-info:full-pathname ).
  cfgLoop: repeat on endkey undo, leave on error undo, leave:

    inData = "".

    do on error undo, leave cfgLoop
       on endkey undo, leave cfgLoop:

      import stream inStrm inData.

    end.

    if inData[1] = "" or inData[1] begins "#" then next.
    create tt_apsvList.
    assign
      i = i + 1
      tt_apsvList.xid          = i
      tt_apsvList.asType       = inData[1]
      tt_apsvList.asName       = inData[2]
      tt_apsvList.asCommand[1] = inData[3]
    .

  end.
  input stream inStrm close.

  return.

end.


define variable transFile as character no-undo.
define variable transDate as date      no-undo.
define variable transTime as integer   no-undo.

procedure chkTranslation:

  define variable inData as character no-undo extent 64.

  define variable i as integer no-undo.

  if transFile = "" or transFile = ? then run findCfgName( "translate", input-output transFile ).

  file-info:file-name = transFile.

  if file-info:full-pathname = ? then
    do:
      transFile = "".
      return.
    end.

  if transDate = file-info:file-mod-date and transTime = file-info:file-mod-time then return.

  assign
    transDate = file-info:file-mod-date
    transTime = file-info:file-mod-time
  .

  input stream inStrm from value( file-info:full-pathname ).
  apsvLoop: repeat on endkey undo, leave on error undo, leave:

    inData = "".

    do on error undo, leave apsvLoop
       on endkey undo, leave apsvLoop:

      import stream inStrm inData.

    end.

    if inData[1] = "" or inData[1] begins "#" then next.

    case inData[1]:

      when "asbmanDet"  then xasbmanDet  = inData[2].
      when "asbmanSend" then xasbmanSend = inData[2].

      when "OperMode"   then xOperMode   = inData[2].
      when "BrkrName"   then xBrkrName   = inData[2].
      when "BrkrPort"   then xBrkrPort   = inData[2].
      when "BrkrPID"    then xBrkrPID    = inData[2].

      when "ActvAgnts"  then xActvAgnts  = inData[2].
      when "BusyAgnts"  then xBusyAgnts  = inData[2].
      when "LckdAgnts"  then xLckdAgnts  = inData[2].
      when "AvailAgnts" then xAvailAgnts = inData[2].
      when "ActvClnts"  then xActvClnts  = inData[2].

      when "ActvSrvrs"  then xActvSrvrs  = inData[2].
      when "BusySrvrs"  then xBusySrvrs  = inData[2].
      when "LckdSrvrs"  then xLckdSrvrs  = inData[2].
      when "AvailSrvrs" then xAvailSrvrs = inData[2].

      when "ClientQ"    then xClientQ    = inData[2].
      when "RqWait"     then xRqWait     = inData[2].
      when "RqDuration" then xRqDuration = inData[2].

      when "monthList"  then xMonthList  = inData[2].

    end.

  end.
  input stream inStrm close.

  return.

end.



procedure mon-init:

  if connected( "dictdb" ) then run lib/apsvcnx.p persistent.

  run chkConfig.
  run chkTranslation.

  run updTick.

  return.

end.

define stream cmd.
define variable xxx      as integer   no-undo.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable inData   as character no-undo extent 64.
  define variable inLine   as character no-undo.
  define variable inString as character no-undo.
  define variable brkName  as character no-undo.
  define variable xDetails as logical   no-undo.
  define variable i        as integer   no-undo.
  define variable numApSv  as integer   no-undo.

  define variable mth      as integer no-undo.
  define variable dd       as integer no-undo.
  define variable yyyy     as integer no-undo.
  define variable hh       as integer no-undo.
  define variable mm       as integer no-undo.

  define variable lastChg  as integer no-undo.

  define variable z_apsvReq      as decimal no-undo.
  define variable z_apsvRcvd     as decimal no-undo.
  define variable z_apsvSent     as decimal no-undo.
  define variable z_apsvDBAccess as decimal no-undo.
  define variable z_apsvOSRead   as decimal no-undo.
  define variable z_apsvOSWrite  as decimal no-undo.

  run chkConfig.
  run chkTranslation.

  run updTick.

  /* error 1422 haunts this TT and seems to only be cured by "empty TT" :(
   *
   * which is a problem because it destroys the tt_apsv/tt_xstat xid link
   * currently the PID is being used as a unique id that related the two
   * tables but that might not be a good idea -- it has not been carefully
   * reviewed
   */

  empty temp-table tt_apsv.

  numApSv = 0.

  for each tt_apsvList:

    assign
      xDetails = no
      brkName  = ""
      asMode   = ""
      asNote   = ""
      numApSv  = numApSv + 1
    .

    /*** message tt_apsvList.asType substitute( tt_apsvList.asCommand[1], tt_apsvList.asName ). ***/
    /*** pause. ***/

    input stream cmd through value( substitute( tt_apsvList.asCommand[1], tt_apsvList.asName )).
    as_loop: repeat:

      inData = "".
      do on error undo, leave as_loop
         on endkey undo, leave as_loop:

        import stream cmd inData.

        /*** message tt_apsvList.asType + ":" inData[1] inData[2] inData[3] inData[4]. ***/
        /*** pause. ***/

      end.

      if inData[1] = "" or inData[1] = "OpenEdge" or inData[1] = "Progress" or inData[1] = "ulimit" then
        do:
          next as_loop.
        end.

      /* inString is built so that we can more easily translate multi-word labels such as those
       * in the header of asbman using BEGINS syntax
       */

      inString = "".
      do i = 1 to 8:					/* the first 8 "words" should be plenty	*/
        inString = inString + inData[i] + " ".
      end.
      inString = trim( inString ).

      /* if an error is detected we want to try to capture the remaining output in the asNote field
       */

      if asNote <> "" then
        do:

          do i = 1 to 64:
            asNote = asNote + inData[i] + " ".
          end.
          asNote = trim( asNote ).

          do while true
             on error undo, leave as_loop
             on endkey undo, leave as_loop:

            inLine = ?.
            import stream cmd unformatted inLine.

            if inLine = ? then leave as_loop.
            asNote = asNote + "~n" + inLine.

          end.

        end.

      if inString = "" or inString = ? then
        do:
          message "no cmd data, this shouldn't actually be possible".
          pause.
          next.
        end.

      /* now parse specific app server types
       */

      case tt_apsvList.asType:

        /* proenv> proadsv -query
         * OpenEdge Release 10.2B07 as of Fri Sep  7 02:16:14 EDT 2012
         * AdminServer is alive. (8545)
         *
         * - or -
         *
         * OpenEdge Release 11.6.1 as of Fri Feb 19 18:20:45 EST 2016
         * AdminServer not alive. (8543)
         *
         *
         */

        when "admin" or when "proadsv" then
          do:
            if inData[4] = "(8545)" then
              assign
                asMode = "Alive"
                asActive = 0
                asBusy   = 0
                asLocked = 0
                asAvail  = 9999
                .
             else if inData[4] = "(8543)" then
              asMode = "Not Running".
             else
              do:
                /* asMode = "WTF?". */
              end.
/***
             else
              do:						/* why isn't the admin server alive?	*/
                do i = 1 to 64:
                  asNote = asNote + inData[i] + " ".
                end.
                asNote = trim( asNote ).
                asNote = asNote + "~n".
              end.
 ***/
          end.

        /* proenv>nsman -i NS1 -query
         * OpenEdge Release 10.2B07 as of Fri Sep  7 02:16:14 EDT 2012
         * 
         * 
         * Connecting to Progress AdminServer using rmi://localhost:20931/Chimera (8280)
         * Searching for NS1 (8288)
         * Connecting to NS1  (8276)
         * 
         * NameServer NS1 running on Host USFL04EUS00V Port 5162 Timeout 30 seconds.
         * Application Service             UUID            Name            Host             Port   Weight  Timeout
         * 
         * WS.NewJS
         *         0026b2edc72dbc96:c7013c2:1436c2cab33:4f3d       WS.NewJS        USFL04EUS00V/10.83.25.190    6011       0       30
         * 
         * WS.eohslive
         *         0026b2edc72dbc96:c7013c2:1436c2cab33:af2        WS.eohslive     USFL04EUS00V/10.83.25.190    5050       0       30
         * 
         */

        when "ns" or when "nsman" then
          do:
            if inData[1] = "NameServer" and inData[2] = asName then
              do:
                asMode = "Not Running".
                if inData[3] = "running" then
                  assign
                    asMode = "Running"
                    asActive = 0
                    asBusy   = 0
                    asLocked = 0
                    asAvail  = 9999
                /*  asHost = inData[6] */
                    asPort = integer( inData[8] )
                  no-error.
              end.
          end.

        /* proenv> asbman -i asESBLive -q -port 20999
         * OpenEdge Release 11.3.3 as of Thu Sep 25 19:00:15 EDT 2014
         * 
         * 
         * Connecting to Progress AdminServer using rmi://localhost:20999/Chimera (8280)
         * Searching for asESBLive (8288)
         * Connecting to asESBLive  (8276)
         * 
         * Broker Name                    : asESBLive
         * Operating Mode                 : State-free
         * Broker Status                  :  ACTIVE
         * Broker Port                    : 3192
         * Broker PID                     : 31025
         * Active Servers                 : 35
         * Busy Servers                   : 0
         * Locked Servers                 : 0
         * Available Servers              : 35
         * Active Clients (now, peak)     : (61, 61)
         * Client Queue Depth (cur, max)  : (0, 2)
         * Total Requests                 : 8411992
         * Rq Wait (max, avg)             : (148 ms, 0 ms)
         * Rq Duration (max, avg)         : (171731 ms, 55 ms)
         * 
         * PID   State     Port  nRq    nRcvd  nSent  Started          Last Change
         * 31273 AVAILABLE 02002 240362 240363 261090 Dec 27, 2015 12:26 Jan 8, 2016 16:47
         * 31276 AVAILABLE 02003 240361 240361 261050 Dec 27, 2015 12:26 Jan 8, 2016 16:47
         * 31279 AVAILABLE 02004 240348 240350 261749 Dec 27, 2015 12:26 Jan 8, 2016 16:47
         * 31282 AVAILABLE 02005 240345 240345 261031 Dec 27, 2015 12:26 Jan 8, 2016 16:47
         * ...
         */

        when "apsv"  or when "asbman" or					/* app servers					*/
        when "sonic" or when "adaptman" or					/* sonic is just another app server...		*/
        when "ws"    or when "wtbman" then					/* webspeed is just another app server...	*/
          do:

            if inData[5] = "(8313)" or inData[5] = "(8281)" then
              asMode = "Not Running".

/* +++      if inData[1] = "PID" and inData[2] = "State" then */
            if inString begins xasbManDet then
              do:
                xDetails  = yes.
                next.
              end.

            if xDetails = yes then
              do:

                /* PID should be a unique idx...
                 *
                 * but that may be wishful thinking if app servers are running on 
                 * multiple machines
                 *
                 */

                find tt_apsv where tt_apsv.apsvPID = integer( inData[1] ) no-error.
                if not available tt_apsv then
                  do:
                    create tt_apsv.
                    assign
                      xxx = xxx + 1
                      tt_apsv.xid = integer( inData[1] ) /* xxx */
                    .

                  end.

                assign
                  asSending = 0
                  asStuck   = 0
                .

                assign
                  apsvName    = brkName
                  apsvType    = tt_apsvList.asType
                  apsvPID     = integer( trim( inData[1] )) 
                  apsvStatus  = inData[2]
                  apsvPort    = integer( trim( inData[3] ))

                  /* asbman -help and the docs are wrong -- kbase 21131 says:
                   *
                   *   The nRq and nSent definitions should read:
                   *         nRq   - The number of requests sent to the server process.
                   *         nSent - The number of messages sent by the server process.
                   *  
                   *  The interpretation of these statistics varies depending on the operating mode
                   *  of the broker.
                   *  
                   *  a) For State-aware and State-reset AppServers, the number of requests
                   *  represents the number of connections made from the client to the AppServer.
                   *  After the connection, all traffic takes place directly between the client
                   *  and the AppServer -- the broker is not involved, so it does not keep any
                   *  statistics on requests or messages.
                   *  
                   *  b) For State-aware/reset, the number of connections, the number of
                   *  requests, and the number of messages will always be the same. That's
                   *  because each connection only involves one message in each direction.
                   *  
                   *  c) For Stateless AppServer mode, the broker handles all traffic between the
                   *  client and the AppServer, so it can keep more detailed statistics. In this
                   *  context, the following operations count as a request:
                   *  
                   *  -- the "connect()" to the AppServer
                   *  -- each "run()"
                   *  -- the "disconnect()"
                   *  
                   *  Each request can involve one or more request messages sent from the client
                   *  to the server (via the broker), followed by one or more response messages
                   *  sent from the server to the client (again, via the broker).
                   *  
                   *  The relative numbers seen in the nSent and nRcvd columns, as compared to
                   *  those in the nRq column, is strictly application and data dependent. If the
                   *  application generally sends small requests (under 8Kb) and small responses,
                   *  then nRq, nRcvd, and nSent will all be fairly close. If the application
                   *  transfers a lot of temp tables however, the numbers will differ more widely
                   *  since each temp table is transferred as a series of 8Kb packets.
                   *  
                   *  For WebSpeed, each web request involves sending a single message from the
                   *  client to the broker, so the values of nRq and nRcvd will always be equal.
                   *  The WebSpeed Broker does not handle any of the responses to the web
                   *  requests, so the nSent value will also be the same as nRq and nRcvd.
                   *  
                   *
                   ********************************************************************************
                   *
                   * thus, for ProTop's purposes, "requests" is clearly the most useful column
                   */
 
                  z_apsvReq   = integer( trim( inData[4] )) 
                  apsvReq     = z_apsvReq				/* this is more useful as a total than as a rate	*/

                  /* we might as well grab the others while we are here but that's just for kicks
                   */

                  z_apsvRcvd  = integer( trim( inData[5] )) 
                  z_apsvSent  = integer( trim( inData[6] )) 

                no-error.

/* +++          if inData[2] = "Sending" then */
                if inString begins xasbManSend then
                  do:

                    assign
                      asSending = asSending + 1
                      lastChg   = ?
                      mth       = lookup( inData[11], xMonthList )
                      dd        = integer( right-trim( inData[12], "," ))
                      yyyy      = integer( inData[13])
                      hh        = integer( entry( 1, inData[14], ":" ))
                      mm        = integer( entry( 2, inData[14], ":" ))
                   /* lastChg   = datetime( mth, dd, yyyy, hh, mm )      */
                      lastChg   = ((  date( mth, dd, yyyy ) - 1/1/1970 ) * 86400 ) + ( hh * 3600 ) + ( mm * 60 )
                    no-error.

                 /* if interval( now, lastChg, "minutes" ) > 2 then asStuck = asStuck + 1.  */

                    if ( time -  lastChg ) > pt_appsrvStuck then asStuck = asStuck + 1.

                  end.

                next.

              end.	/* details	*/
/* +++ */
             else if inString begins xBrkrName   then brkName   = trim( inData[4] ).
             else if inString begins xOperMode   then asMode    = trim( inData[4] ).
             else if inString begins xBrkrPORT   then asPort    = integer( trim( inData[4] )) no-error.
             else if inString begins xBrkrPID    then asPID     = integer( trim( inData[4] )) no-error.

             /* webspeed "agents" */

             else if inString begins xActvAgnts  then asActive  = integer( trim( inData[4] )) no-error.
             else if inString begins xBusyAgnts  then asBusy    = integer( trim( inData[4] )) no-error.
             else if inString begins xLckdAgnts  then asLocked  = integer( trim( inData[4] )) no-error.
             else if inString begins xAvailAgnts then asAvail   = integer( trim( inData[4] )) no-error.

             /* app server "servers" */

             else if inString begins xActvSrvrs  then asActive  = integer( trim( inData[4] )) no-error.
             else if inString begins xBusySrvrs  then asBusy    = integer( trim( inData[4] )) no-error.
             else if inString begins xLckdSrvrs  then asLocked  = integer( trim( inData[4] )) no-error.
             else if inString begins xAvailSrvrs then asAvail   = integer( trim( inData[4] )) no-error.

             else if inString begins xActvClnts  then
              assign
                asActCli  = integer( trim( inData[6], "(, )" ))
                zasCliHWM = integer( trim( inData[7], "(, )" ))
              no-error.
             else if inString begins xClientQ then
              assign
                asCurrQ = integer( trim( inData[7], "(, )" ))
                asMaxQ  = integer( trim( inData[8], "(, )" ))
              no-error.
             else if inString begins xRqWait then
              assign
                asMaxWt = integer( trim( inData[6], "(, )" ))
                asAvgWt = integer( trim( inData[8], "(, )" ))
              no-error.
             else if inString begins xRqDuration then
              assign
                asMaxLen = integer( trim( inData[6], "(, )" ))
                asAvgLen = integer( trim( inData[8], "(, )" ))
              no-error.

          end.

      end.

    end.

    input stream cmd close.

    if tt_apsvList.asType = "apsv" or  tt_apsvList.asType = "asbman" or
       tt_apsvList.asType = "ws"   or  tt_apsvList.asType = "wtbman" then
      do:

        inData = "".

        input stream cmd through value( replace( substitute( tt_apsvList.asCommand[1], tt_apsvList.asName ), "-q", "-listallprops" )).
        repeat:

          import stream cmd unformatted inData[1].

          if       inData[1] begins "maxSrvrInstance" then
            tt_apsvList.asMaxSrvInst = integer( entry( 2, inData[1], "=" )) no-error.
           else if inData[1] begins "maxClientInstance" then
            tt_apsvList.asMaxCliInst = integer( entry( 2, inData[1], "=" )) no-error.

        end.

        input stream cmd close.

        if tt_apsvList.asMaxSrvInst > 0 and  tt_apsvList.asMaxSrvInst <> ? then 
           assign
             tt_apsvList.zasSrInUse = (  tt_apsvList.asBusy + tt_apsvList.asLocked )
             tt_apsvList.asSrUsePct = 100 * (( tt_apsvList.zasSrInUse ) / tt_apsvList.asMaxSrvInst )
           .
        if tt_apsvList.asSrUsePct = ? then tt_apsvList.asSrUsePct = 0.

        if tt_apsvList.asMaxCliInst > 0 and  tt_apsvList.asMaxCliInst <> ? then 
           tt_apsvList.asClUsePct = 100 * ((  tt_apsvList.asActCli ) / tt_apsvList.asMaxCliInst ).
        if tt_apsvList.asClUsePct = ? then tt_apsvList.asClUsePct = 0.

      end.

  end.

  publish "updAppSrvCnx" (
          input z_apsvDBAccess,
          input z_apsvOSRead,
          input z_apsvOSWrite,
          input z_apsvReq,
          input z_apsvRcvd,
          input z_apsvSent
  ).

  publish "resizeBrowse" ( "apsvstat", numApSv ).

  add2ds( temp-table tt_apsvList:default-buffer-handle ).
  add2ds( temp-table tt_apsv:default-buffer-handle ).

  return.

end.

return.

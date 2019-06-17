/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2009 Tom Bascom, Greenfield Technologies                  **
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
 * dashboard.p
 */

{lib/protop.i}
{lib/protoplib.i}

define output parameter dcDescription as character no-undo initial "Dashboard".

define temp-table tt_sstat no-undo                                      /* tt to sample statistics                      */
  field xId          as integer                                         /* metric id                                    */
  field sum-stat     as decimal extent 5 format ">>>>>>9"               /* init, prev, curr, cumulative & interval      */
  index xid-idx is unique primary xId
.

define temp-table tt_Dashboard no-undo
  field xid         as integer
  field HR          as decimal   label "Hit%"        format ">>9.99%"
  field LogRd       as decimal   label "Log Reads"   format ">>>>>>>>9"
  field OSRd        as decimal   label "OS Reads"    format ">>>>>>>>9"
  field RecRd       as decimal   label "Rec Reads"   format ">>>>>>>>9"
  field Log2Rec     as decimal   label "Log2Rec"     format ">>>>>9.99"
  field LogWr       as decimal   label "Log Writes"  format ">>>>>>>>9"
  field OSWr        as decimal   label "OS Writes"   format ">>>>>>>>9"
  field RecCr       as decimal   label "Rec Creates" format ">>>>>>>>9"
  field RecUp       as decimal   label "Rec Updates" format ">>>>>>>>9"
  field RecDl       as decimal   label "Rec Deletes" format ">>>>>>>>9"
  field RecLk       as decimal   label "Rec Locks"   format ">>>>>>>>9"
  field RecWt       as decimal   label "Rec Waits"   format ">>>>>>>>9"
  field idxRdx      as decimal   label "Idx Reads"   format ">>>>>>>>9"
  field idxCrx      as decimal   label "Idx Creates" format ">>>>>>>>9"
  field idxDlx      as decimal   label "Idx Deletes" format ">>>>>>>>9"
  field idxBlkSplit as decimal   label "Idx Blk Spl" format ">>>>>>>>9"
  field ResrcWt     as decimal   label "Resrc Waits" format ">>>>>>9"
  field LatchTMO    as decimal   label "Latch Waits" format ">>>>>>9"
  field LatchReq    as decimal   label "Latch Reqs"  format ">>>>>>9"
  field Lat2LogRd   as decimal   label "Lat2LogRd"   format ">>>>>9.99"
  field ioResponse  as decimal   label "IO Response" format ">>>>>9.99"
  field bogoMips    as decimal   label "BogoMIPS"    format ">>>>>9.99" initial ?
  field bogoMipPct  as decimal   label "BogoMIPS%"   format ">>9.99%"
  field TRX         as decimal   label "Commits"     format ">>>>9"
  field TRXUndo     as decimal   label "Undos"       format ">>>>9"

  field LkTblHWM    as integer   label "LkTblHWM"    format ">>>>>>>>9"
  field currLks     as integer   label "currLks"     format ">>>>>>>>9"
  field OldTRX      as character label "OldTRX"
  field currBI      as integer   label "currbi"      format ">>>>>>>>>>>>9"
  field oldBI       as integer   label "oldbi"       format ">>>>>>>>>>>>9"
  field EvictBufs   as decimal   label "EvictBufs"   format ">>>>>>>>9"
  field chkPts      as integer   label "chkPts"      format ">>>>>>>>>>9"

  field rm_allocNew   as integer   label "New RM"    format "->>>>>>>9"
  field rm_fromRM     as integer   label "Frm RM"    format "->>>>>>>9"
  field rm_fromFree   as integer   label "Frm Fr"    format "->>>>>>>9"
  field rm_examined   as integer   label "Exam"      format "->>>>>>>9"
  field rm_frnt2bk    as integer   label "Frnt2Bk"   format "->>>>>>>9"
  field rm_rmlocked   as integer   label "RM Lkd"    format "->>>>>>>9"

  field apwWrts       as decimal label "APW Writes"    format ">>>>>>>>9.99"
  field apwWrtPct     as decimal label "APW Write%"    format ">>>>>>>>9.99%"
  field apwScanCycles as decimal label "Scan Cycles"   format ">>>>>>>>9.99"
  field apwBufsScan   as decimal label "Bufs Scanned"  format ">>>>>>>>9.99"
  field apwQueWrts    as decimal label "APW Q Wrts"    format ">>>>>>>>9.99"
  field apwScanWrts   as decimal label "APW Scan Wrts" format ">>>>>>>>9.99"
  field apwChkPtWrts  as decimal label "ChkPt Q Wrts"  format ">>>>>>>>9.99"
  field bufsFlushed   as integer label "Flushed Bufs"  format ">>>>>>>>>>9"

  field biNotes     as decimal   label "BI/AI Notes"        format  ">>>>>>>>>>9"
  field biWrts      as decimal   label "Writes to BI/AI"    format ">>>>>>>>9.99"
  field biwWrts     as decimal   label "BIW/AIW Writes"     format ">>>>>>>>9.99"
  field biwWrtPct   as decimal   label "BIW/AIW Write%"     format   ">>>>>>9.99%"
  field biPartWrt   as decimal   label "Partial Buf Writes" format ">>>>>>>>9.99"
  field biBusyWt    as decimal   label "Busy Buf Waits"     format ">>>>>>>>9.99"
  field biEmptyWt   as decimal   label "Empty Buf Waits"    format ">>>>>>>>9.99"

  field aiNotes     as decimal   label ""  format  ">>>>>>>>>>9"
  field aiWrts      as decimal   label ""  format ">>>>>>>>9.99"
  field aiwWrts     as decimal   label ""  format ">>>>>>>>9.99"
  field aiwWrtPct   as decimal   label ""  format   ">>>>>>9.99%"
  field aiPartWrt   as decimal   label ""  format ">>>>>>>>9.99"
  field aiBusyWt    as decimal   label ""  format ">>>>>>>>9.99"
  field aiEmptyWt   as decimal   label ""  format ">>>>>>>>9.99"

  field con_Total   as integer   label "Connections" format ">>>>>9"
  field con_Brok    as integer   label "Brokers"     format ">>>>>9"
  field con_4glServ as integer   label "4gl Servers" format ">>>>>9"
  field con_SQLServ as integer   label "SQL Servers" format ">>>>>9"
  field con_BIW     as integer   label "BIW"         format ">>>>>9"
  field con_AIW     as integer   label "AIW"         format ">>>>>9"
  field con_APW     as integer   label "APWs"        format ">>>>>9"
  field con_WDOG    as integer   label "WDOG"        format ">>>>>9"
  field con_Util    as integer   label "Utilities"   format ">>>>>9"
  field con_Self    as integer   label "Local"       format ">>>>>9"
  field con_Remc    as integer   label "Remote"      format ">>>>>9"
  field con_Batch   as integer   label "Batch"       format ">>>>>9"
  field con_4gl     as integer   label "4gl Clients" format ">>>>>9"
  field con_SQL     as integer   label "SQL Clients" format ">>>>>9"
  field con_APSV    as integer   label "App Servers" format ">>>>>9"
  field con_TRX     as integer   label "TRX"         format ">>>>>9"
  field con_Block   as integer   label "Blocked"     format ">>>>>9"

  field con_BlockRec as integer   label "BlockRec"   format ">>>>>9"
  field con_BlkOther as integer   label "BlkOther"   format ">>>>>9"

  field AIStat      as character label "AIStat"
  field ai_Curr     as character label "ai_curr"
  field ai_Busy     as integer   label "ai_busy"     format ">>>>>9"
  field ai_Exts     as integer   label "ai_exts"     format ">>>>>9"
  field ai_Empty    as integer   label "ai_empty"    format "->>9"
  field ai_Full     as integer   label "ai_full"     format "->>9"
  field ai_Locked   as integer   label "ai_locked"   format "->>9"
  field ai_Seq      as integer   label "ai_seq"      format ">>>>>>>>>>9"

  field ModBufs     as decimal   label "ModBufs"     format "->>>>>>>>9"

/*** unused fields ***/

  field Con_Helper  as integer   label "Helpers"     format ">>>>>9"
  field Con_Other   as integer   label "Other"       format ">>>>>9"

  field HRstr       as character label "HRstr"
  field MR          as decimal   label "MR"          format ">>>>>>>>9"

  field TotBufs     as integer   label "TotBufs"     format ">>>>>>>>>>>>9"
  field LkTblSz     as integer   label "LkTblSz"     format ">>>>>>>>>>>>9"
  field dbState     as character label "dbState"
  field BkUpAge     as character label "BkUpAge"
  field otrx        as integer   label "otrx"        format ">>>>>>>>>>>>9"

/*** new fields ***/

  field numBI       as integer   label "numbi"       format ">>>>>>>>>>>>9"
  field dbBkUpFull  as integer   label "dbBkUpFull"  format ">>>>>>>>>>>>9"

  field biUsed      as decimal   label "BI Used"     format ">>>>>>>>9.99"
  field con_wta     as decimal   label "Con_WTA"     format ">>>>>>>>>>9%"

  field picaFree    as decimal   label "pica Free"   format ">>>>>>>>>>>>9"
  field picaUsedPct as decimal   label "pica Used%"  format ">>>>>>>>9.99%"
  field picaUsed    as decimal   label "pica Used"   format ">>>>>>>>>>>>9"

  field con_aimd    as decimal   label "Con_AIMD"    format ">>>>>>>>>>9%"
  field con_rpls    as decimal   label "Con_RPLS"    format ">>>>>>>>>>9%"
  field con_rpla    as decimal   label "Con_RPLA"    format ">>>>>>>>>>9%"

  field dbUpTime    as character label "DB Up Time"
  field dbBkUpAge   as character label "Backup Age"

  field lkTblpct    as decimal   label "LkTbl%"      format ">>>>>>>>9.99%"
  field dashNpct    as integer   label "-n %"        format ">>>>>>9%"

  field avgChkPtLen as decimal   label "Avg ChkPt Len" format ">>>>>>>>9"
  field avgChkPtStr as character label "Avg ChkPt Str" format "x(8)"
  field minChkPtLen as decimal   label "Min ChkPt Len" format ">>>>>>>>9"
  field minChkPtStr as character label "Min ChkPt Str" format "x(8)"

  field usrExpSHM   as decimal   label "ZippySHM"    format ">>>>>>9"
  field usrExpSHMTM as decimal   label "ZippyTime"   format ">>>>>>9"

  field usrExpLH    as decimal   label "ZippyLH"     format ">>>>>>9"
  field zusrExpLH   as character label "ZippyLH"     			/* label field for zUsrExpLH	*/

  field usrExpCS    as decimal   label "ZippyCS"     format ">>>>>>9"
  field zusrExpCS   as character label "ZippyCS"     			/* label field for zUsrExpCS	*/

  field syncIO      as decimal   label "SyncIO"      format ">>>9.99"
  field zSyncIO     as character label "SyncIO"      			/* label field for syncIO	*/

  field lkDurMS     as decimal   label "Lk Duration ms" format ">>>>>9.9999"

  /*** user defined ***/

  field usrField1   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel1   as character label ""            format "x(13)"

  field usrField2   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel2   as character label ""            format "x(13)"

  field usrField3   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel3   as character label ""            format "x(13)"

  field usrField4   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel4   as character label ""            format "x(13)"

  field usrField5   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel5   as character label ""            format "x(13)"

  field usrField6   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel6   as character label ""            format "x(13)"

  field usrField7   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel7   as character label ""            format "x(13)"

  field usrField8   as decimal   label ""            format ">>>>>>>>9"
  field usrLabel8   as character label ""            format "x(13)"

/*** indexes ***/

  index xid-idx  is unique primary xid
.  

{lib/dumpTT.i tt_Dashboard}

define buffer tx_Dashboard for tt_Dashboard.

define variable isEnabled as logical no-undo.

define variable sampleCount as integer no-undo.

define variable bh as handle no-undo.
define variable bf as handle no-undo.
define variable qh as handle no-undo.

define variable qc as handle no-undo.

define variable us_bh as handle no-undo.
define variable us_qh as handle no-undo.

define variable xResrcWt  as decimal no-undo.
define variable xLatchTMO as decimal no-undo.
define variable xLatchReq as decimal no-undo.

define variable hasCnxClient    as logical no-undo.
define variable replEnabled     as logical no-undo.
define variable replServRunning as logical no-undo.
define variable hasAIInfo       as logical no-undo.
define variable useRFUtil       as logical no-undo.

define new global shared variable zlogRd as decimal no-undo.

define stream ioTest.

	/* the behind the scenes global magic doesn't work with this iteration of the dashboard	*/
	/* the building of tt_stat records needs to be revisited in order to fix that		*/
	/* as-is dashboard is a bit of a mix of "status" and "performance" metrics		*/

/* */ {lib/tick.i} /* */
/*
define variable initTick as integer no-undo.
define variable prevTick as integer no-undo.

define variable x as integer no-undo.
define variable y as decimal no-undo.
define variable z as decimal no-undo.

procedure updTick:

  define variable i as integer no-undo.

  if initTick = 0 then initTick = mtime.

  assign
    x = 5
    z = mtime - prevTick
    z = ( if z >= 0 then z else 86400000 - prevTick + mtime )   /* handle rolling over midnight */
    z = z / 1000
    y = mtime - initTick
    y = y / 1000
    prevTick = mtime
  .
  
  return.

end.
 */

/******/

define variable bdb as handle no-undo.
define variable qdb as handle no-undo.


define variable hasDBFeature as logical no-undo.

procedure chkDBFeatures:

  hasDBFeature  = no.

  find dictdb._File  no-lock where _File-Name = "_Database-Feature" no-error.
  if available _File then
    do:

      hasDBFeature = yes.
      create buffer bdb for table "dictdb._Database-Feature".
      create query qdb.
      qdb:set-buffers( bdb ).
      qdb:query-prepare( "preselect each dictdb._Database-Feature" ).

      qdb:query-open.
      qdb:get-next().
      do while not qdb:query-off-end:

        if string( bdb:buffer-field( "_DBFeature_Name"    ):buffer-value()) = "OpenEdge Replication" and
           string( bdb:buffer-field( "_DBFeature_Enabled" ):buffer-value()) = "1" then replEnabled = yes.

        qdb:get-next().

      end.

    end.

  return.

end.

procedure chkCnxClient:

  hasCnxClient = no.
  find dictdb._File  no-lock where _File-Name = "_Connect".
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_Connect-ClientType" no-error.
  if available _Field then hasCnxClient = yes.

  return.

end.


procedure chkAIInfo:

  hasAIInfo = no.
  find dictdb._File  no-lock where _File-Name = "_AreaStatus".
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_AreaStatus-AI-Seq" no-error.
  if available _Field then hasAIInfo = yes.

  return.

end.


define variable lastPICACheck as integer no-undo initial ?.

define stream picaStrm.

procedure checkPICA:

  define variable stuff as character extent 64 no-undo.
  define variable numPicaMsgs as decimal no-undo.

  if pt_picaCheckInterval > 0 and ( lastPICACheck = ? or abs( uDateTime() - lastPICACheck ) >= pt_picaCheckInterval ) then
    do:

      assign
        lastPICACheck = uDateTime()
      .

      if opsys = "unix" then
        input stream picaStrm through value( substitute( "bin/picamon.sh  &1", pdbname(1))).
       else
        input stream picaStrm through value( substitute( "call picamon.bat &1", pdbname(1))).

      /*  $ picamon.sh /db/trax/xus61t2							*/
      /*  Communication Area Size   :      100000.00 KB					*/
      /*    Total Message Entries   :      914282					*/
      /*    Free Message Entries    :      914282					*/
      /*    Used Message Entries    :           0					*/
      /*    Used HighWater Mark     :        1380					*/
      /*    Area Filled Count       :           0					*/
      /*    Service Latch Holder    :          -1					*/
      /*    Access Count            :    30827022					*/
      /*    Access Collisions       :       13394					*/

      repeat:

        stuff = "".
        import stream picaStrm stuff.

        /* message "stuff:" stuff[1] stuff[2] stuff[3] stuff[4] stuff[5]. */
        /* pause. */

        if       stuff[1] = "Total" and stuff[2] = "Message" then numPicaMsgs              = decimal( stuff[5] ) no-error.
         else if stuff[1] = "Free"  and stuff[2] = "Message" then tt_Dashboard.picaFree    = decimal( stuff[5] ) no-error.
         else if stuff[1] = "Used"  and stuff[2] = "Message" then tt_Dashboard.picaUsedPct = ( decimal( stuff[5] ) / numPicaMsgs ) * 100 no-error.

        if numPicaMsgs              = ? then numPicaMsgs = 0.
        if tt_Dashboard.picaFree    = ? then tt_Dashboard.picaFree = 0.
        if tt_Dashboard.picaUsedPct = ? then tt_Dashboard.picaUsedPct = 0.

      end.

      input stream picaStrm close.

      /*** picamon ^^^ ***/

    end.

  return.

end.


define variable lastAICheck as integer   no-undo initial ?.
define variable lastAICurr  as character no-undo initial ?.

procedure checkAI:

  define variable stuff as character extent 64 no-undo.

  find dictdb._Logging no-lock.
  if _Logging-AIBegin begins "-" then return.			/* after-imaging is disabled!		*/

  if pt_AICheckInterval > 0 and ( lastAICheck = ? or abs( uDateTime() - lastAICheck ) >= pt_AICheckInterval or tt_Dashboard.ai_curr <> lastAICurr ) then
    do:

      assign
        lastAICheck = uDateTime()
        lastAICurr  = tt_Dashboard.ai_curr
      .

      assign
        tt_Dashboard.ai_full   = 0
        tt_Dashboard.ai_empty  = 0
        tt_Dashboard.ai_locked = 0
      .

      if hasAIInfo = yes then
        do:

          for each _areaStatus no-lock where _areaStatus-areaName begins "after image area":
            assign
              tt_Dashboard.ai_full   = tt_Dashboard.ai_full   + ( if ( buffer _areaStatus:handle:buffer-field( "_areaStatus-State" ):buffer-value ) = "Full"   then 1 else 0 )
              tt_Dashboard.ai_empty  = tt_Dashboard.ai_empty  + ( if ( buffer _areaStatus:handle:buffer-field( "_areaStatus-State" ):buffer-value ) = "Empty"  then 1 else 0 )
              tt_Dashboard.ai_locked = tt_Dashboard.ai_locked + ( if ( buffer _areaStatus:handle:buffer-field( "_areaStatus-State" ):buffer-value ) matches "*Lock*" then 1 else 0 )
            no-error.
          end.

        end.

      if hasAIInfo = no and pt_useRFUTIL = yes then
        do:

          if opsys = "unix" then
            input stream inStrm through value( substitute(      "rfutil &1 -C aimage list -cpinternal &2 -cpstream &3", pdbname(1), session:cpinternal, session:cpstream )).
           else
            input stream inStrm through value( substitute( 'call "%DLC%~\bin~\rfutil" &1 -C aimage list -cpinternal &2 -cpstream &3 2>> &4~\&5.rfutil.err', pdbname(1), session:cpinternal, session:cpstream, pt_tmpdir, pt_shortName )).

          repeat:
            stuff = "".
            import stream inStrm stuff.
            if stuff[1] = "Status:" then
              do:
                if       stuff[2] = "Full"   then tt_Dashboard.ai_full   = tt_Dashboard.ai_full   + 1.
                 else if stuff[2] = "Empty"  then tt_Dashboard.ai_empty  = tt_Dashboard.ai_empty  + 1.
                 else if stuff[2] matches "*Lock*" then tt_Dashboard.ai_locked = tt_Dashboard.ai_locked + 1.
              end.
          end.

          input stream inStrm close.

        end.

    end.

  return.

end.


function fixDateString returns character ( input dx as character ):

  define variable d as character no-undo.
  define variable m as character no-undo.
  define variable y as character no-undo.
  define variable t as character no-undo.

  /* The date-time field in _checkpoint looks like: Mon Jan 14 03:33:08 2013 */

  assign
    m = string( lookup( substring( dx, 5, 3 ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" ), "99" )
    d = string( integer( trim( substring( dx, 9, 2 ))), "99" )
    y = substring( dx, 21 )
    t = substring( dx, 12, 8 )
  .

  if       session:date-format = "ymd" then return substitute( "&1-&2-&3 &4", y, m, d, t ).
   else if session:date-format = "mdy" then return substitute( "&1-&2-&3 &4", m, d, y, t ).
   else if session:date-format = "dmy" then return substitute( "&1-&2-&3 &4", d, m, y, t ).
   else
    do:
      message "Unsupported session:date-format" session:date-format view-as alert-box.
      quit.
    end.

end.


&IF DEFINED( OE10 ) &THEN
define variable lastSample as datetime no-undo.
&ELSE
define variable lastSample as integer  no-undo.
&ENDIF

procedure calcChkptLen:

&IF DEFINED( OE10 ) &THEN
  define variable cpStart   as datetime no-undo.
  define variable cpFinish  as datetime no-undo.
&ELSE
  define variable cpStart   as integer  no-undo.
  define variable cpFinish  as integer  no-undo.
&ENDIF

  define variable cpLength  as integer  no-undo.
  define variable minLength as integer  no-undo initial 86399.		/* makes it easy to recognize a bogus result i.e. 23:59:59	*/

  define variable n as integer no-undo.

&IF DEFINED( OE10 ) &THEN

  for each dictdb._Checkpoint no-lock:

    /* the first record is usually incomplete
     */

    if _Checkpoint._Checkpoint-Len = ? then
      do:
        next.
      end.

    /* capture anything that happened during the sample period
     */

    if datetime( fixDateString( _Checkpoint._Checkpoint-Time )) > lastSample then
      do:
        assign
          cpStart  = datetime( fixDateString( _Checkpoint._Checkpoint-Time ))
          cpFinish = datetime( fixDateString( _Checkpoint._Checkpoint-Len )) when n = 0
          n = n + 1
        .
        if cpStart <> ? and cpFinish <> ? then
          minLength = min( minLength, abs( interval( cpFinish, cpStart, "seconds" ))).
        next.
      end.

    /* capture the start of any partial chkpt that was active at the start of the sample
     */

    if datetime( fixDateString( _Checkpoint._Checkpoint-Time )) <= lastSample then
      do:
        assign
          cpStart  = datetime( fixDateString( _Checkpoint._Checkpoint-Time ))
          cpFinish = datetime( fixDateString( _Checkpoint._Checkpoint-Len )) when n = 0
          n = n + 1
        .
        if cpStart <> ? and cpFinish <> ? then
          minLength = min( minLength, abs( interval( cpFinish, cpStart, "seconds" ))).
        leave.	/* only get one such record */
      end.

  end.

  if n > 0 then
    assign
      cpLength = abs( interval( cpFinish, cpStart, "seconds" )) / n
      tt_Dashboard.avgChkPtLen = cplength
      tt_Dashboard.avgChkPtStr = string( integer( cpLength ), "hh:mm:ss" )
    .

  if minLength < 86399 and minLength > 0 then
    assign
      tt_Dashboard.minChkPtLen = minLength
      tt_Dashboard.minChkPtStr = string( integer( minLength ), "hh:mm:ss" )
    .

&ENDIF

  return.

end.


procedure mon-init:

  define variable i as integer no-undo.

  empty temp-table tt_Dashboard.

  if pt_doZippy = yes then
    run lib/zippy.p persistent.

/*  pt_useRFUtil = ( if os-getenv( "USERFUTIL" ) = "yes" then yes else no ). */

  file-info:file-name = "lib/usermon.p".
  if file-info:full-pathname <> ? then
    run value( "lib/usermon.p" ) persistent.

  create tt_Dashboard.

  run chkCnxClient.
  run chkAIInfo.

  find dictdb._ActSummary no-lock.
  find first dictdb._ActBuffer  no-lock.
  find dictdb._ActBiLog   no-lock.
  find dictdb._ActAiLog   no-lock.
  find dictdb._ActPWs     no-lock.
  find dictdb._BuffStatus no-lock.
  find dictdb._ActSpace   no-lock.
  find dictdb._ActIndex   no-lock.
  find dictdb._Logging    no-lock.

  find first _repl-Server no-lock no-error.
  replServRunning = available( _repl-Server ).

  assign
    xLatchTMO = 0
    xLatchReq = 0
    xResrcWt  = 0
  .
  for each dictdb._latch no-lock:
    assign
      xLatchTMO = xLatchTMO + _latch._latch-wait
      xLatchReq = xLatchReq + _latch._latch-lock
    .
  end.
  for each dictdb._resrc no-lock:
    xResrcWt  = xResrcWt  + _resrc._resrc-wait.
  end.

  create tt_sstat. assign tt_sstat.xid =   4 {lib/init-xrec.i tt_sstat.sum-stat _Buffer-LogicRds}.
  create tt_sstat. assign tt_sstat.xid =   5 {lib/init-xrec.i tt_sstat.sum-stat _Buffer-OSRds}.

/*create tt_sstat. assign tt_sstat.xid =   6 {lib/init-xrec.i tt_sstat.sum-stat _Summary-Chkpts}. */
  create tt_sstat. assign tt_sstat.xid =   6 {lib/init-xrec.i tt_sstat.sum-stat _BfStatus-LastCkpNum}.

  create tt_sstat. assign tt_sstat.xid =   7 {lib/init-xrec.i tt_sstat.sum-stat _Summary-Flushed}.
  create tt_sstat. assign tt_sstat.xid =  21 {lib/init-xrec.i tt_sstat.sum-stat _Summary-Commits}.
  create tt_sstat. assign tt_sstat.xid =  22 {lib/init-xrec.i tt_sstat.sum-stat xLatchTMO}.
  create tt_sstat. assign tt_sstat.xid =  23 {lib/init-xrec.i tt_sstat.sum-stat xLatchReq}.
  create tt_sstat. assign tt_sstat.xid =  24 {lib/init-xrec.i tt_sstat.sum-stat xResrcWt}.
  create tt_sstat. assign tt_sstat.xid =  25 {lib/init-xrec.i tt_sstat.sum-stat _Buffer-LRUwrts}.
  create tt_sstat. assign tt_sstat.xid =  26 {lib/init-xrec.i tt_sstat.sum-stat _Summary-Flushed}.

  create tt_sstat. assign tt_sstat.xid = 100 {lib/init-xrec.i tt_sstat.sum-stat _Buffer-LogicWrts}.
  create tt_sstat. assign tt_sstat.xid = 101 {lib/init-xrec.i tt_sstat.sum-stat _Buffer-OSWrts}.
  create tt_sstat. assign tt_sstat.xid = 110 {lib/init-xrec.i tt_sstat.sum-stat _Summary-RecCreat}.
  create tt_sstat. assign tt_sstat.xid = 111 {lib/init-xrec.i tt_sstat.sum-stat _Summary-RecReads}.
  create tt_sstat. assign tt_sstat.xid = 112 {lib/init-xrec.i tt_sstat.sum-stat _Summary-RecUpd}.
  create tt_sstat. assign tt_sstat.xid = 113 {lib/init-xrec.i tt_sstat.sum-stat _Summary-RecDel}.
  create tt_sstat. assign tt_sstat.xid = 114 {lib/init-xrec.i tt_sstat.sum-stat _Summary-RecLock}.
  create tt_sstat. assign tt_sstat.xid = 115 {lib/init-xrec.i tt_sstat.sum-stat _Summary-RecWait}.
  create tt_sstat. assign tt_sstat.xid = 116 {lib/init-xrec.i tt_sstat.sum-stat _Summary-Undos}.

  create tt_sstat. assign tt_sstat.xid = 130 {lib/init-xrec.i tt_sstat.sum-stat _Index-Find}.
  create tt_sstat. assign tt_sstat.xid = 131 {lib/init-xrec.i tt_sstat.sum-stat _Index-Delete}.
  create tt_sstat. assign tt_sstat.xid = 132 {lib/init-xrec.i tt_sstat.sum-stat _Index-Splits}.

  create tt_sstat. assign tt_sstat.xid = 150 {lib/init-xrec.i tt_sstat.sum-stat _Space-AllocNewRM}.
  create tt_sstat. assign tt_sstat.xid = 151 {lib/init-xrec.i tt_sstat.sum-stat _Space-FromRM}.
  create tt_sstat. assign tt_sstat.xid = 152 {lib/init-xrec.i tt_sstat.sum-stat _Space-FromFree}.
  create tt_sstat. assign tt_sstat.xid = 153 {lib/init-xrec.i tt_sstat.sum-stat _Space-Examined}.
  create tt_sstat. assign tt_sstat.xid = 154 {lib/init-xrec.i tt_sstat.sum-stat _Space-Front2Back}.
  create tt_sstat. assign tt_sstat.xid = 155 {lib/init-xrec.i tt_sstat.sum-stat _Space-Locked}.

  create tt_sstat. assign tt_sstat.xid = 200 {lib/init-xrec.i tt_sstat.sum-stat _BiLog-RecWriten}.
  create tt_sstat. assign tt_sstat.xid = 201 {lib/init-xrec.i tt_sstat.sum-stat _BiLog-BIWWrites}.
  create tt_sstat. assign tt_sstat.xid = 202 {lib/init-xrec.i tt_sstat.sum-stat _BiLog-TotalWrts}.
  create tt_sstat. assign tt_sstat.xid = 203 {lib/init-xrec.i tt_sstat.sum-stat _BiLog-PartialWrts}.
  create tt_sstat. assign tt_sstat.xid = 204 {lib/init-xrec.i tt_sstat.sum-stat _BiLog-BBuffWaits}.
  create tt_sstat. assign tt_sstat.xid = 205 {lib/init-xrec.i tt_sstat.sum-stat _BiLog-EBuffWaits}.

  create tt_sstat. assign tt_sstat.xid = 210 {lib/init-xrec.i tt_sstat.sum-stat _AiLog-RecWriten}.
  create tt_sstat. assign tt_sstat.xid = 211 {lib/init-xrec.i tt_sstat.sum-stat _AiLog-AIWWrites}.
  create tt_sstat. assign tt_sstat.xid = 212 {lib/init-xrec.i tt_sstat.sum-stat _AiLog-TotWrites}.
  create tt_sstat. assign tt_sstat.xid = 213 {lib/init-xrec.i tt_sstat.sum-stat _AiLog-PartialWrt}.
  create tt_sstat. assign tt_sstat.xid = 214 {lib/init-xrec.i tt_sstat.sum-stat _AiLog-BBuffWaits}.
  create tt_sstat. assign tt_sstat.xid = 215 {lib/init-xrec.i tt_sstat.sum-stat _AiLog-NoBufAvail}.

  create tt_sstat. assign tt_sstat.xid = 220 {lib/init-xrec.i tt_sstat.sum-stat _PW-TotDBWrites}.
  create tt_sstat. assign tt_sstat.xid = 221 {lib/init-xrec.i tt_sstat.sum-stat _PW-DBWrites}.
  create tt_sstat. assign tt_sstat.xid = 222 {lib/init-xrec.i tt_sstat.sum-stat _PW-ScanCycles}.
  create tt_sstat. assign tt_sstat.xid = 223 {lib/init-xrec.i tt_sstat.sum-stat _PW-BuffsScaned}.
  create tt_sstat. assign tt_sstat.xid = 224 {lib/init-xrec.i tt_sstat.sum-stat _PW-ScanWrites}.
  create tt_sstat. assign tt_sstat.xid = 225 {lib/init-xrec.i tt_sstat.sum-stat _PW-APWQWrites}.
  create tt_sstat. assign tt_sstat.xid = 226 {lib/init-xrec.i tt_sstat.sum-stat _PW-CkpQWrites}.

  create buffer us_bh for table "dictdb._UserStatus".
  create query us_qh.
  us_qh:set-buffers( us_bh ).
  us_qh:query-prepare( "preselect each dictdb._UserStatus no-lock where _UserStatus-UserId <> ? and _UserStatus-Operation <> ?" ).

  if pt_ioFileName = "" then
    do:
/***
      find first _fileList no-lock where _fileList-name matches "*.b1" no-error.
      pt_ioFileName = _fileList-name.
 ***/
      for each _fileList no-lock:
        pt_ioFileName = pt_ioFileName + ( if i > 0 then "," else "" ) + _fileList-name.
        i = i + 1.
        if i > 100 then leave.
      end.
    end.

&IF DEFINED( OE10 ) &THEN
  lastSample = {&NOW}.
&ENDIF

  run updTick.	/*** +++ ***/

  return.

end.

define variable minBogo  as decimal no-undo initial 9999999.
define variable maxBogo  as decimal no-undo.
define variable bogoLoop as integer no-undo.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable i as integer no-undo.
  define variable r as integer no-undo.
  define variable v as decimal no-undo.
  define variable n as integer no-undo.
  define variable f as integer no-undo.
  define variable t as integer no-undo.

  define variable bf      as handle  no-undo.
  define variable cnx_bh  as handle  no-undo.
  define variable cnx_bf1 as handle  no-undo.
  
  define variable h as integer extent 3 no-undo.

  define variable currDT   as integer   no-undo.
  define variable fBackUp  as integer   no-undo.
  define variable iBackUp  as integer   no-undo.
  define variable bkupDays as integer   no-undo.
  define variable bkupSecs as integer   no-undo.
  define variable estart   as {&BIGINT} no-undo.

  define variable upDays   as integer no-undo.
  define variable upSecs   as integer no-undo.

  define variable eof      as {&BIGINT} no-undo.
  define variable offset   as {&BIGINT} no-undo.

  define variable lgLine    as character no-undo.
  define variable fieldName as character no-undo.

  define variable syncTxt as character no-undo.
  define variable syncMin as integer   no-undo.
  define variable syncSec as decimal   no-undo.
  define variable syncIO  as decimal   no-undo.

  find last tt_Dashboard no-error.

  /* a couple of very gross measures of how the OS is behaving...
   *
   * these might help to shine some light and distinguish between db issues vs OS issues.
   * especially when SANs and VMs are in use.
   *
   */

  x = 0.
  n = num-entries( pt_ioFileName ).

  estart = etime.

  /* if there are pending commands at the UI level skip this
   */

  if session:batch or lastkey < 0 then
    do:

      do while x <= pt_ioresp and ( etime - estart ) < 100:	/* limit this to 100ms of execution time			*/

        assign
          i = i + 1
          x = x + 1
        .

        file-info:filename = ( if n > 1 then entry( random( 1, n ), pt_ioFileName ) else pt_ioFileName ).

        if file-info:full-pathname <> ? and index( file-info:file-type, "F" ) > 0 and index( file-info:file-type, "R" ) > 0 and file-info:file-size >= 1 then
          do:
            input stream ioTest from value( file-info:full-pathname ).
&IF DEFINED( OE10 ) &THEN
            seek stream ioTest to random( 0, file-info:file-size - 1 ).
&ELSE
            seek stream ioTest to random( 0, max( file-info:file-size - 1, exp( 2, 31 ) - 1 )).
&ENDIF
            readkey stream ioTest.
            input stream ioTest close.
          end.

      end.

      tt_Dashboard.ioResponse = ( etime - estart ) / i.		/* calculate avg seek time in ms				*/

    end.

  /* tt_Dashboard.ioResponse = i / (( etime - estart ) / 1000 ). */	/* calculate IO ops/sec					*/

  tt_Dashboard.bogoMIPS = ?.

  if bogoLoop < 10 then bogoLoop = bogoLoop + 1.		/* skip calculating this during startup				*/

  if pt_bogomips > 0 /* and bogoLoop > 1 */ then		/* or maybe don't skip it after all ;)				*/
    do:

      estart = etime.
      do i = 1 to pt_bogomips:					/* 100,000 should take 20ms to 30ms on circa 2010 HW		*/
      end.

      tt_Dashboard.bogoMIPS = (( pt_bogomips / (( etime - estart ) / 1000 )) / 1000000 ).

      if tt_Dashboard.bogoMIPS > 0 and tt_Dashboard.bogoMIPS <> ? then
        do:
          maxBogo = max( maxBogo, tt_Dashboard.bogoMIPS ).
          tt_Dashboard.bogoMipPct = ( tt_Dashboard.bogoMIPS / maxBogo ) * 100.
        end.

    end.

  if pt_doZippy = yes then
    run zippy ( output i, output t ).

  tt_Dashboard.usrExpSHMTM = t.					/* the raw time probably makes a better alert - NO! we now target at least 50ms so this is basically useless	*/

  tt_Dashboard.usrExpSHM   = ( i / t ) * 1000.			/* records per second	*/

  assign
     usrExpLH = 0
    zusrExpLH = ""
  .

  file-info:file-name = substitute( "&1/zippyng.&2.lh.log", pt_logdir, pt_shortName ).
  if file-info:full-pathname <> ? then
    do:

      zusrExpLH = "localhost:".

      input stream inStrm from value( file-info:full-pathname ).

      seek stream inStrm to end.
      eof = seek( inStrm ).
      offset = eof - 42.
      offset = max( 0, offset ).
      seek stream inStrm to offset.

      lgLine = "".
      repeat:	/* do on error undo, leave read_lg on endkey undo, leave read_lg: */
        import stream inStrm unformatted lgLine.
      end.

      input stream inStrm close.

      if num-entries( lgLine, " " ) >= 3 then
        do:
          lgLine = trim( substring( lgLine, index( lgLine, " " ))).
          tt_Dashboard.usrExpLH = decimal( trim( substring( lgLine, index( lgLine, " " )))) no-error.
        end.

    end.

  file-info:file-name = substitute( "&1/zippyng.&2.cs.log", pt_logdir, pt_shortName ).
  if file-info:full-pathname <> ? then
    do:

      zusrExpCS = "Remote:".

      input stream inStrm from value( file-info:full-pathname ).

      seek stream inStrm to end.
      eof = seek( inStrm ).
      offset = eof - 42.
      offset = max( 0, offset ).
      seek stream inStrm to offset.

      lgLine = "".
      repeat:	/* do on error undo, leave read_lg on endkey undo, leave read_lg: */
        import stream inStrm unformatted lgLine.
      end.

      input stream inStrm close.

      if num-entries( lgLine, " " ) >= 3 then
        do:
          lgLine = trim( substring( lgLine, index( lgLine, " " ))).
          tt_Dashboard.usrExpCS = decimal( trim( substring( lgLine, index( lgLine, " " )))) no-error.
        end.

    end.

  file-info:file-name = substitute( "&1/syncio.log", pt_logdir, pt_shortName ).
  if file-info:full-pathname <> ? then
    do:

      tt_Dashboard.syncIO = ?.
      zSyncIO = "Sync IO:".

      input stream inStrm from value( file-info:full-pathname ).

      seek stream inStrm to end.
      eof = seek( inStrm ).
      offset = eof - 256.
      offset = max( 0, offset ).
      seek stream inStrm to offset.

      lgLine = "".
      repeat:	/* do on error undo, leave read_lg on endkey undo, leave read_lg: */
        import stream inStrm unformatted lgLine.
      end.

      input stream inStrm close.

      syncTxt = trim( entry( 4, lgLine, " " )) no-error.

      /* Old Linux:     real<tab>0m1.848s                                       */
      /* New Linux: 2018/04/27 15:32:39 real 1.20 /db/tmp                       */
    
      if num-entries( syncTxt, "m" ) <= 1 then
        tt_Dashboard.syncIO  = decimal( syncTxt ) no-error.             /* time -p or HPUX:     real   2.2      */
       else
        do:
          syncMin = integer( entry( 1, syncTxt, "m" )) no-error.
          syncSec = decimal( entry( 1, entry( 2, syncTxt, "m" ), "s" )) no-error.
          tt_Dashboard.syncIO  = ( syncMin * 60 ) + syncSec.
        end. 

/***
 *      else if opsys begins "win" and lineIn begins "Execution time:" then
 *       do:
 *         syncSec = decimal( trim( entry( 1, entry( 2, lineIn, ":" ), "s" ))) no-error.
 *         syncIO  = ( syncMin * 60 ) + syncSec.
 *       end.
 ***/

    end.

  publish "userMon" (
    output tt_Dashboard.usrField1,
    output tt_Dashboard.usrLabel1,
    output tt_Dashboard.usrField2,
    output tt_Dashboard.usrLabel2,
    output tt_Dashboard.usrField3,
    output tt_Dashboard.usrLabel3,
    output tt_Dashboard.usrField4,
    output tt_Dashboard.usrLabel4,
    output tt_Dashboard.usrField5,
    output tt_Dashboard.usrLabel5,
    output tt_Dashboard.usrField6,
    output tt_Dashboard.usrLabel6,
    output tt_Dashboard.usrField7,
    output tt_Dashboard.usrLabel8,
    output tt_Dashboard.usrField8,
    output tt_Dashboard.usrLabel8
  ).

  run updTick.

  find dictdb._ActSummary no-lock.
  find first dictdb._ActBuffer  no-lock.
  find dictdb._ActBiLog   no-lock.
  find dictdb._ActAiLog   no-lock.
  find dictdb._ActPWs     no-lock.
  find dictdb._BuffStatus no-lock.
  find dictdb._DbStatus   no-lock.
  find dictdb._Logging    no-lock.
  find dictdb._ActSpace   no-lock.
  find dictdb._ActIndex   no-lock.

  assign
    currDT =  uDateTime()
    fBackUp = ( currDT - string2uDateTime( _dbStatus-fbDate ))
    iBackUp = ( currDT - string2uDateTime( _dbStatus-ibDate ))
  .

  tt_Dashboard.dbBkUpFull = fBackUp.

  if _dbStatus-fbDate = ? and _dbStatus-fbDate = ? then
    tt_dashboard.dbBkUpAge  = "Never!".
   else
    do:
      bkupSecs = min( fBackUp, iBackUp ).
      if fBackup = ? then bkupSecs = iBackUp.
      if iBackup = ? then bkupSecs = fBackUp.
      assign
        bkupDays = integer( truncate( bkupSecs / 86400, 0 ))
        bkupSecs = bkupSecs modulo 86400
        tt_dashboard.dbBkUpAge  = ( if bkupDays > 0 then string( bkupDays ) + "d " else "" ) + string( bkupSecs, "hh:mm" ).
      .
    end.
    
  if tt_dashboard.dbBkUpAge = ? then tt_dashboard.dbBkUpAge = "Unknown".

  assign
    upSecs = _Summary-upTime
    upDays = integer( truncate( upSecs / 86400, 0 ))
    upSecs = upSecs modulo 86400
    tt_dashboard.dbUpTime  = ( if upDays > 0 then string( upDays ) + "d " else "" ) + string( upSecs, "hh:mm" )
  .

  assign
    xLatchTMO = 0
    xLatchReq = 0
    xResrcWt  = 0
  .
  for each dictdb._latch no-lock:
    assign
      xLatchTMO = xLatchTMO + _latch-wait
      xLatchReq = xLatchReq + _latch-lock
    .
  end.
  
  for each dictdb._resrc no-lock:
    xResrcWt  = xResrcWt  + _resrc-wait.
  end.
 
  for each tt_sstat:

    case tt_sstat.xid:

      when   4 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Buffer-LogicRds  tt_Dashboard.LogRd}.
      when   5 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Buffer-OSRds     tt_Dashboard.OSRd}.
      when  21 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-Commits  tt_Dashboard.TRX}.
      when  22 then assign {lib/upd-xrec2.i tt_sstat.sum-stat xLatchTMO         tt_Dashboard.LatchTMO}.
      when  23 then assign {lib/upd-xrec2.i tt_sstat.sum-stat xLatchReq         tt_Dashboard.LatchReq}.
      when  24 then assign {lib/upd-xrec2.i tt_sstat.sum-stat xResrcWt          tt_Dashboard.ResrcWt}.
      when  25 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Buffer-LRUwrts   tt_Dashboard.EvictBufs}.
      when  26 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-Flushed  tt_Dashboard.BufsFlushed}.

      when 100 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Buffer-LogicWrts tt_Dashboard.LogWr}.
      when 101 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Buffer-OSWrts    tt_Dashboard.OSWr}.
      when 110 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-RecCreat tt_Dashboard.RecCr}.
      when 111 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-RecReads tt_Dashboard.RecRd}.
      when 112 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-RecUpd   tt_Dashboard.RecUp}.
      when 113 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-RecDel   tt_Dashboard.RecDl}.
      when 114 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-RecLock  tt_Dashboard.RecLk}.
      when 115 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-RecWait  tt_Dashboard.RecWt}.
      when 116 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Summary-Undos    tt_Dashboard.TRXUndo}.

      when 130 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Index-Find       tt_Dashboard.idxRdx}.
      when 131 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Index-Delete     tt_Dashboard.idxDlx}.
      when 132 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Index-Splits     tt_Dashboard.idxBlkSplit}.
      when 133 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Index-Create     tt_Dashboard.idxCrx}.

      when 150 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Space-AllocNewRM tt_Dashboard.rm_allocNew}.
      when 151 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Space-FromRM     tt_Dashboard.rm_fromRM}.
      when 152 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Space-FromFree   tt_Dashboard.rm_fromFree}.
      when 153 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Space-Examined   tt_Dashboard.rm_examined}.
      when 154 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Space-Front2Back tt_Dashboard.rm_frnt2bk}.
      when 155 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _Space-Locked     tt_Dashboard.rm_rmlocked}.

      when 200 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _BiLog-RecWriten   tt_Dashboard.biNotes}.
      when 201 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _BiLog-BIWWrites   tt_Dashboard.biwWrts}.
      when 202 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _BiLog-TotalWrts   tt_Dashboard.biWrts}.
      when 203 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _BiLog-PartialWrts tt_Dashboard.biPartWrt}.
      when 204 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _BiLog-BBuffWaits  tt_Dashboard.biBusyWt}.
      when 205 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _BiLog-EBuffWaits  tt_Dashboard.biEmptyWt}.
      
      when 210 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _AiLog-RecWriten   tt_Dashboard.aiNotes}.
      when 211 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _AiLog-AIWWrites   tt_Dashboard.aiwWrts}.
      when 212 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _AiLog-TotWrites   tt_Dashboard.aiWrts}.
      when 213 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _AiLog-PartialWrt  tt_Dashboard.aiPartWrt}.
      when 214 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _AiLog-BBuffWaits  tt_Dashboard.aiBusyWt}.
      when 215 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _AiLog-NoBufAvail  tt_Dashboard.aiEmptyWt}.
   /* when 220 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-TotDBWrites    tt_Dashboard.}. */
      when 221 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-DBWrites       tt_Dashboard.apwWrts}.
      when 222 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-ScanCycles     tt_Dashboard.apwScanCycles}.
      when 223 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-BuffsScaned    tt_Dashboard.apwBufsScan}.
      when 224 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-ScanWrites     tt_Dashboard.apwScanWrts}.
      when 225 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-APWQWrites     tt_Dashboard.apwQueWrts}.
      when 226 then assign {lib/upd-xrec2.i tt_sstat.sum-stat _PW-CkpQWrites     tt_Dashboard.apwChkPtWrts}.

    end.

  end.

  hr( input tt_Dashboard.LogRd, input tt_Dashboard.OSRd, output tt_Dashboard.HRstr, output tt_Dashboard.hr, output tt_Dashboard.mr ).
  if tt_Dashboard.hr = ? then hr = 100.

  zlogRd = tt_Dashboard.LogRd.

  tt_Dashboard.Log2Rec = tt_Dashboard.LogRd  / tt_Dashboard.RecRd.
  if tt_Dashboard.Log2Rec  = ? then tt_Dashboard.Log2Rec  = 0.0.

  tt_Dashboard.Lat2LogRd = tt_Dashboard.LatchReq  / tt_Dashboard.LogRd.
  if tt_Dashboard.Lat2LogRd  = ? then tt_Dashboard.Lat2LogRd  = 0.0.

  tt_Dashboard.biwWrtPct = ( tt_Dashboard.biwWrts  / tt_Dashboard.biWrts ) * 100.0.
  if tt_Dashboard.biwWrtPct  = ? then tt_Dashboard.biwWrtPct  = 0.0.

  tt_Dashboard.aiwWrtPct = ( tt_Dashboard.aiwWrts  / tt_Dashboard.aiWrts ) * 100.0.
  if tt_Dashboard.aiwWrtPct  = ? then tt_Dashboard.aiwWrtPct  = 0.0.

  tt_Dashboard.apwWrtPct = ( tt_Dashboard.apwWrts  / tt_Dashboard.OSWr ) * 100.0.
  if tt_Dashboard.apwWrtPct  = ? then tt_Dashboard.apwWrtPct  = 0.0.

  /* _areaExtent could be cached for this purpose, the ai extent# could be pre-calculated too
   *
   */

  /* find _AreaExtent no-lock where _Area-Number = _Logging-AiCurrExt and _Extent-number = 1 no-error. */

  find tt_AreaExtent no-lock where areaNum = _Logging-AiCurrExt and extNum = 1 no-error.

  assign
    tt_Dashboard.ai_seq    = _Logging-AiGenNum
    /* tt_Dashboard.ai_busy   = integer( substring( _Extent-Path, r-index( _Extent-path, "." ) + 2 )) when available( _AreaExtent ) */
    tt_Dashboard.ai_busy   = integer( substring( extPath, r-index( extPath, "." ) + 2 )) when available( tt_AreaExtent )
    tt_Dashboard.ai_exts   = _Logging-AiExtents
  .

  if replEnabled then run checkPICA.

  if _Logging-AIBegin begins "-" then 				         /* _Logging-AIJournal is apparently broken :( 		*/
    assign
      tt_Dashboard.AIStat = "Disabled"
      tt_Dashboard.ai_curr = "Disabled"
    .
   else
    do:
      assign
        tt_Dashboard.AIStat = "Enabled"
        tt_Dashboard.ai_curr = string( tt_Dashboard.ai_busy ) + " of " + string( tt_Dashboard.ai_exts )
      .
      run checkAI.
    end.

  assign
    tt_Dashboard.TotBufs  = _BuffStatus._BfStatus-TotBufs
    tt_Dashboard.ModBufs  = _BuffStatus._BfStatus-ModBuffs
/*  tt_Dashboard.ModBufs  = ( if _BuffStatus._BfStatus-ModBuffs >= 0 then _BuffStatus._BfStatus-ModBuffs else ( exp( 2, 21 ) + _BuffStatus._BfStatus-ModBuffs )) */
/*  tt_Dashboard.ModBufs  = unsignMe( _BuffStatus._BfStatus-ModBuffs, 21 ) */
    tt_Dashboard.currLks  = _DbStatus._DbStatus-NumLocks
    tt_Dashboard.LkTblHWM = _DbStatus._DbStatus-MostLocks
/*  tt_Dashboard.chkPts   = _ActSummary._Summary-ChkPts */
    tt_Dashboard.chkPts   = _BuffStatus._BfStatus-LastCkpNum
    tt_Dashboard.lkTblPct = ( _DbStatus._DbStatus-NumLocks / integer( getStartUpX( "_Startup-LockTable", "(-L)", "Current Size of Lock Table" ))) * 100.
    tt_Dashboard.lkDurMS  = ( tt_Dashboard.currLks / ( tt_Dashboard.recLk * z )) * 1000. 
  .

  tt_Dashboard.currBI = chkPtNum( input-output tt_Dashboard.oldBI ).

  tt_Dashboard.numBI = absolute( tt_Dashboard.currBI - tt_Dashboard.oldBI ).

  tt_Dashboard.biUsed = tt_Dashboard.numBI * ( _dbStatus-biClSize  / 1024 ).

  /* connection related metrics
   */

  assign
    tt_Dashboard.con_total    = 0
    tt_Dashboard.con_self     = 0
    tt_Dashboard.con_remc     = 0
    tt_Dashboard.con_batch    = 0
    tt_Dashboard.con_brok     = 0
    tt_Dashboard.con_4glServ  = 0
    tt_Dashboard.con_SQLServ  = 0
    tt_Dashboard.con_block    = 0
    tt_Dashboard.con_blockRec = 0
    tt_Dashboard.con_blkOther = 0
    tt_Dashboard.con_trx      = 0
    tt_Dashboard.con_biw      = 0
    tt_Dashboard.con_aiw      = 0
    tt_Dashboard.con_apw      = 0
    tt_Dashboard.con_wdog     = 0
    tt_Dashboard.con_Util     = 0
    tt_Dashboard.con_4gl      = 0
    tt_Dashboard.con_SQL      = 0
    tt_Dashboard.con_APSV     = 0
    tt_Dashboard.con_WTA      = 0
    tt_Dashboard.con_AIMD     = 0
    tt_Dashboard.con_RPLS     = 0
    tt_Dashboard.con_RPLA     = 0
    tt_Dashboard.otrx         = 0
    tt_Dashboard.OldTRX       = ""
  .

/* find dictdb._Connect no-lock where _Connect-usr = -1.  */

  for each dictdb._Connect no-lock where _Connect-usr <> ?:

    tt_Dashboard.con_total  = tt_Dashboard.con_total  + 1.

    case  _Connect-type:
      when "self"  then if ( not _Connect-device matches "*batch*" ) and _connect-batch <> "yes" then tt_Dashboard.con_self  = tt_Dashboard.con_self  + 1.	/** don't count batch sessions **/
      when "remc"  then if ( not _Connect-device matches "*batch*" ) and _connect-batch <> "yes" then tt_Dashboard.con_remc  = tt_Dashboard.con_remc  + 1.	/** don't count batch sessions **/
      when "brok"  then tt_Dashboard.con_brok    = tt_Dashboard.con_brok  + 1.
      when "serv"  then
        do:
          find _Servers no-lock where _Server-id = _Connect-usr + 1 no-error.
          if available _Servers then
            do:
              if _Server-type = "Login" then
                tt_Dashboard.con_brok = tt_Dashboard.con_brok  + 1.
               else
                tt_Dashboard.con_4glServ = tt_Dashboard.con_4glServ + 1.
            end.
        end.
      when "sqsv"  then tt_Dashboard.con_sqlServ = tt_Dashboard.con_sqlServ + 1.
      when "biw"   then tt_Dashboard.con_biw     = tt_Dashboard.con_biw     + 1.
      when "aiw"   then tt_Dashboard.con_aiw     = tt_Dashboard.con_aiw     + 1.
      when "apw"   then tt_Dashboard.con_apw     = tt_Dashboard.con_apw     + 1.
      when "wdog"  then tt_Dashboard.con_wdog    = tt_Dashboard.con_wdog    + 1.
      when "aimd"  then tt_Dashboard.con_aimd    = tt_Dashboard.con_aimd    + 1.
      when "rpls"  then tt_Dashboard.con_rpls    = tt_Dashboard.con_rpls    + 1.
      when "rpla"  then tt_Dashboard.con_rpla    = tt_Dashboard.con_rpla    + 1.
      otherwise         tt_Dashboard.con_other   = tt_Dashboard.con_other   + 1.
    end.
    if _Connect-type begins "SQSV" then tt_Dashboard.con_SQLServ = tt_Dashboard.con_SQLServ + 1.

    /** don't count various servers, brokers, utilities etc... **/	/* IOW count 4GL batch *clients*	*/

    if ( _Connect-device = "batch" or _Connect-batch = "yes" ) and
       ( lookup( _Connect-type, "SELF,REMC" ) > 0 ) then
      do:

        if hasCnxClient = false then
          tt_Dashboard.con_batch = tt_Dashboard.con_batch + 1.
         else
          do:
            assign
              cnx_bh  = buffer _Connect:handle
              cnx_bf1 = cnx_bh:buffer-field( "_Connect-ClientType" )
            no-error.
            if cnx_bf1 <> ? and cnx_bf1:buffer-value = "ABL" then tt_Dashboard.con_batch = tt_Dashboard.con_batch + 1.
          end.

      end.

    if _Connect-wait <> " -- "  then
      do:

         tt_Dashboard.con_block    = tt_Dashboard.con_block  + 1.

        if _Connect-wait = "REC" then
          tt_Dashboard.con_blockRec = tt_Dashboard.con_blockRec + 1.
         else
          tt_Dashboard.con_blkOther = tt_Dashboard.con_blkOther + 1.

      end.

    if hasCnxClient = true then
      do:
        assign        
          cnx_bh  = buffer _Connect:handle
          cnx_bf1 = cnx_bh:buffer-field( "_Connect-ClientType" )
        no-error.
        if cnx_bf1 <> ? then
          do:
            if       cnx_bf1:buffer-value = "ABL"  then tt_Dashboard.con_4gl  = tt_Dashboard.con_4gl  + 1.
             else if cnx_bf1:buffer-value = "SQLC" then tt_Dashboard.con_SQL  = tt_Dashboard.con_SQL  + 1.
             else if cnx_bf1:buffer-value = "APSV" then tt_Dashboard.con_APSV = tt_Dashboard.con_APSV + 1.
             else if cnx_bf1:buffer-value = "WTA"  then tt_Dashboard.con_WTA  = tt_Dashboard.con_WTA  + 1.
          end.
      end.

  end.

  tt_Dashboard.dashNpct = ( con_total / integer( getStartUpX( "_Startup-MaxUsers",  "(-n)", "Maximum Number of Users" ))) * 100.

  us_qh:query-open.                                     /* "FOR EACH" crashes v9 sessions   */
  tt_Dashboard.con_Util = us_qh:num-results.
  us_qh:query-close.

  for each dictdb._Trans no-lock where _Trans-usrnum <> ?:
    if _Trans-duration >= _Summary-upTime then next.		/* every now and then a crazy duration appears */
    assign
      tt_Dashboard.con_trx = tt_Dashboard.con_trx  + 1
    .  
    if _Trans-duration <> ? then tt_Dashboard.otrx = max( _Trans-duration, tt_Dashboard.otrx ).
  end.
  tt_Dashboard.OldTRX = string( integer( truncate( tt_Dashboard.otrx / 86400, 0 )), ">>>" ) + " " + string( tt_Dashboard.otrx, "hh:mm:ss" ).

  run calcChkPtLen.

&IF DEFINED( OE10 ) &THEN
  lastSample = {&NOW}.
&ENDIF

  find last tt_Dashboard.

  add2ds( temp-table tt_Dashboard:default-buffer-handle ).

  return.

end.

subscribe to "mon-init"          anywhere run-procedure "mon-init".
subscribe to "mon-update"        anywhere run-procedure "mon-update".

{ssg/log2rec.i}
 
return.

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
 * config.p
 *
 */

{lib/protop.i}
{lib/protoplib.i}

define output parameter dcDescription as character no-undo initial "Configuration".

define temp-table tt_Configuration no-undo

  field dbLogName     as character label "DB Logical Name"
  field dbPhysName    as character label "DB Physical Name"
  field dbHost        as character label "DB Host Name"

  field xdbVersion    as character label "DB Version"
  field dbUpTime      as character label "DB Up Time"
  field dbBkUpAge     as character label "Backup Age"
  field dbBlkSize     as integer   label "DB Block Size"      format ">>9 KB"
  field aiBlkSize     as integer   label "AI Block Size"      format ">>9 KB"
  field biBlkSize     as integer   label "BI Block Size"      format ">>9 KB"
  field biClstrSize   as integer   label "BI Cluster Size"    format "->>>>>>>>9 MB"
  field dbBuffers     as integer   label "DB Buffers (-B)"    format "->>>>>>>>>>>>9"
  field aiBuffers     as integer   label "AI Buffers"         format "->>>>>>>>>>>>9"
  field biBuffers     as integer   label "BI Buffers"         format "->>>>>>>>>>>>9"
  field dbSize        as decimal   label "DB Size"            format "->>>>>>>>9.99 MB"
  field allocBlks     as decimal   label "Alloc Blocks"       format "->>>>>>>>9.99 MB"
  field dbEmpty       as integer   label "Empty Blocks"       format "->>>>>>>>>>>>9"
  field varBlks       as decimal   label "Var Blocks"         format "->>>>>>>>9.99 MB"
  field biSize        as decimal   label "BI Size"            format "->>>>>>>>9.99 MB"
  field aiSize        as decimal   label "AI Size"            format "->>>>>>>>9.99 MB"

  field maxConnect    as integer   label "Connections (-n)"   format ">>>>>>>>9"
  field maxServers    as integer   label "Servers (-Mn)"      format ">>>>>>9"
  field cnxPerServ    as integer   label "Clients:Srv (-Ma)"  format ">>>>>9"
  field dbSpin        as integer   label "Spin"               format "->>>>>>>>>>>>9"
  field lkTableSize   as integer   label "Lock Table Size"    format "->>>>>>>>>>>>9"
  field afterImaging  as character label "After-Imaging"
  field dbIntegrity   as character label "Crash Protection"
  field biIOMode      as character label "BI IO Mode (-r)"
  field biDelay       as integer   label "BI Delay (-Mf)"     format ">>>>>9"
  field biClstrAge    as integer   label "BI Clustr Age"      format ">>>>>9"
  field directIO      as character label "Direct IO"
  field db2PC         as character label "2PC"
  field dbTainted     as character label "Tainted (-F)"
  field dbIdxBld      as character label "Index Rebuild"
  field dbSHMSeg      as integer   label "Num SHM Segs"       format ">>>>>>>>9"
  field dbSHMSegSize  as decimal   label "SHM Seg Size"       format ">>>>>>>>>>9.99 MB"
  field dbSHMSize     as decimal   label "SHM Size"           format ">>>>>>>>>>9.99 MB"

  field areaConfig    as character label "Area Config"        format "x(14)"
  field schemaUsed    as character label "Schema Used"
  field rpbSettings   as character label "RPB Settings"
  field warnAreas     as integer   label "Area Full Warnings" format ">>>>9"
  field worstArea     as decimal   label "Worst Area"         format ">>>>>>>>>>>>9.99%"
  field aiAreas       as integer   label "AI Areas"           format ">>>>9"
  field aiSettings    as character label "AI Area Type"
  field aiSeq         as integer   label "AI Sequence#"       format ">>>>>>>>9"
  field aiArchiver    as character label "AI Archiver"
  field oeReplication as character label "OE Replication"
  field largeFiles    as character label "Large Files"
  field dbAuditing    as character label "DB Auditing"
  field failClusters  as character label "Failover Clusters"
  field numAreas      as integer   label "Num Areas"          format ">>>>9"
  field numExtents    as integer   label "Num Extents"        format ">>>>9"
  field lrgExtent     as decimal   label "Largest Extent"     format ">>>>>>>>>>9.99 MB"
  field numTbl        as integer   label "Num Tables"         format "->>>>>>>>>>>>9"
  field tblBase       as integer   label "Table Base"         format "->>>>>>>>>>>>9"
  field tblRange      as integer   label "Table Range"        format "->>>>>>>>>>>>9"
  field numIdx        as integer   label "Num Indexes"        format "->>>>>>>>>>>>9"
  field idxBase       as integer   label "Index Base"         format "->>>>>>>>>>>>9"
  field idxRange      as integer   label "Index Range"        format "->>>>>>>>>>>>9"
  field numLOBs       as integer   label "Number of LOBs"     format "->>>>>>>>>>>>9"

  field biThold       as integer   label "BI Threshold"       format "->>>>>>>>>>>>9"
  field biStall       as character label "BI Stall"           format "x(8)"
  field aiStall       as character label "AI Stall"           format "x(8)"

  field minCnx        as integer   label "Min Connections"    format "->>>>>>>>>>>>9"
  field maxMsg        as integer   label "Max Message Size"   format "->>>>>>>>>>>>9"

  field pfDelay       as character label "Prefetch Delay"     format "x(8)"
  field pfFactor      as integer   label "Prefetch Factor"    format ">>9%"
  field pfNumRecs     as integer   label "Prefetch Num Recs"  format ">>>>>>>>9"
  field pfPriority    as integer   label "Prefetch Priority"  format ">>>>>>>>9"

  field nMsgWait      as integer   label "-nMsgwait"          format ">>>>>>>>9"
  field lruSkips      as integer   label "LRU Skips"          format ">>>>>>>>9"
  field lru2Skips     as integer   label "LRU2 Skips"         format ">>>>>>>>9"
  field recSpaceDepth as integer   label "Rec Space Search"   format ">>>>>>>>9"
  field b2            as integer   label "-B2"                format ">>>>>>>>9"

  field bpMax         as integer   label "-Bpmax"             format ">>>>>>>>9"
  field hashTbl       as integer   label "-hash"              format ">>>>>>>>9"
  field hashPct       as decimal   label "-hashPct"           format    ">>>>9%"
  field semSets       as integer   label "-semsets"           format ">>>>>>>>9"
  field omSize        as integer   label "-omsize"            format ">>>>>>>>9"
  field pica          as integer   label "-pica"              format ">>>>>>>>9"
  field excessSHM     as integer   label "-Mxs"               format ">>>>>>>>9"

  field dbSHMpinned   as character label "SHM Pinned"         format "x(8)"
  field cpInternal    as character label "CP Internal"        format "x(8)"

  field dbProVersion  as character label "db Progress Version"

  field excessTbl     as integer   label "Excess Tbl"         format "->>>>>9"
  field excessIdx     as integer   label "Excess Idx"         format "->>>>>9"
  field osType        as character label "OS Type"            format "x(16)"		/* beware -- "name" is magic		*/

  field lkTblHWMpct   as decimal   label "LkHWMTbl%"          format ">>>>>>>>9.99%"

  field ptVersion     as character label "ProTop Version"     format "x(16)" initial "{lib/ptversion.i}"
  field deployType    as character label "Deployment Type"    format "x(16)"
  field dbSHMFree     as decimal   label "Unused SHM"         format ">>>>>>>>>>9.99 MB"

  field proBits       as integer   label "Bits"               format ">>>9"
  field lgFileDays    as integer   label "lgFileDays"         format ">>>>>9"
  field lgFileSize    as {&BIGINT} label "lgFileSize"         format ">>>>>>>>>>9.99 MB"
  field dbaAge        as integer   label "dbanalys age"       format ">>>>>>>>>>>>>>>>>"
  field dbaAsOf       as {&DTZ}    label "dbanalys as of"
  field dbaAsOfStr    as character label "dbanalys as of"     format "x(12)"

&IF DEFINED( OE10 ) &THEN
  field sqlStats      as integer   label "SQL upd stats"      format ">>>>>>>>>>>>>>>>>"
  field sqlStatsTS    as datetime  label "SQL upd stats TS"
  field sqlStatsStr   as character label "SQL upd stats"      format "x(12)"
&ENDIF

  field memCheck      as character label "-MemCheck"          format "x(12)"
  field dbCheck       as character label "-DbCheck"           format "x(12)"

  field unMonTbl      as integer   label "unMonitored Tables"  format "->>>9"
  field unMonIdx      as integer   label "unMonitored Indexes" format "->>>9"

  field proVersionDate as date     label "DLC Date" format "9999/99/99"
  field proVersionAge  as decimal  label "DLC Age"  format ">>>>9.99"

  index dbLogName-idx is unique primary dbLogName
.

{lib/dumpTT.i tt_Configuration}

define new global shared variable dbgMode  as integer no-undo initial 1.
define new global shared variable monInt   as integer no-undo.			/* monitor sampling interval				*/

define new global shared variable bigB      as integer no-undo.
define new global shared variable bigB2     as integer no-undo.
define new global shared variable lruskips  as integer no-undo.
define new global shared variable lru2skips as integer no-undo.

define variable isEnabled as logical no-undo.

define variable bh as handle no-undo.
define variable bf as handle no-undo.
define variable qh as handle no-undo.

define variable qc  as handle no-undo.
define variable bdb as handle no-undo.
define variable qdb as handle no-undo.

define variable rbits        as integer no-undo.
define variable dbRecid      as recid   no-undo.
define variable xdbVersNum   as decimal no-undo.

define variable dlcVersion   as character no-undo.
define variable proRelease   as character no-undo.

define stream pv.


define variable hasClstrSz   as logical no-undo.
define variable hasDBFeature as logical no-undo.
define variable hasDBParams  as logical no-undo.

procedure mon-init:

  define variable i as integer no-undo.
  define variable j as integer no-undo.
  define variable k as integer no-undo.
  define variable v as integer no-undo.

  define variable z as character no-undo.
  define variable tblCnt     as integer no-undo.
  define variable idxCnt     as integer no-undo.
  define variable so_idx     as integer no-undo.
  define variable so_tbl     as integer no-undo.
  define variable so_lob     as integer no-undo.
  define variable t1         as logical no-undo.
  define variable t2         as logical no-undo.
  define variable fStatus    as character no-undo.

  define variable fx         as integer no-undo.
  define variable vx         as integer no-undo.

  define variable area_bh    as handle  no-undo.
  define variable area_bf1   as handle  no-undo.
  define variable area_bf2   as handle  no-undo.

  define variable xLine      as character no-undo.
  define variable winName    as character no-undo.
  define variable winVers    as character no-undo.

  define variable numTbl    as integer no-undo.
  define variable numIdx    as integer no-undo.
  define variable numLOB    as integer no-undo.
  define variable tblBase   as integer no-undo.
  define variable idxBase   as integer no-undo. 
  define variable tblRange  as integer no-undo.
  define variable idxRange  as integer no-undo.
  define variable minTblNum as integer no-undo.
  define variable maxTblNum as integer no-undo.
  define variable minIdxNum as integer no-undo.
  define variable maxIdxNum as integer no-undo.
  define variable unMonTbl  as integer no-undo.
  define variable unMonIdx  as integer no-undo.
  define variable excessTbl as integer no-undo.
  define variable excessIdx as integer no-undo.
  define variable hst       as integer no-undo.		/* "highest stats table"		*/
  define variable hsi       as integer no-undo.		/* "highest stats index"		*/
  define variable lxt       as integer no-undo.		/* "lowest table > -16384"		*/
  define variable lxi       as integer no-undo.		/* "lowest index"			*/
  define variable hxi       as integer no-undo.		/* "highest index"			*/
  define variable hxt       as integer no-undo.		/* "highest table < 32768"		*/
  define variable lmt       as integer no-undo.		/* "lowest monitored table"		*/
  define variable lmi       as integer no-undo.		/* "lowest monitored index"		*/
  define variable hmt       as integer no-undo.		/* "highest monitored table"		*/
  define variable hmi       as integer no-undo.		/* "highest monitored index"		*/

  create tt_configuration.

  hasClstrSz = no.
  find dictdb._File  no-lock where _File-Name = "_Area".
  find dictdb._Field no-lock where _Field._File-recid = recid( _File ) and _Field-Name = "_Area-ClusterSize" no-error.
  if available _Field then hasClstrSz = yes.

  hasDBFeature = no.
  find dictdb._File no-lock where _File-Name = "_Database-Feature" no-error.
  if available _File then
    do:
      hasDBFeature = yes.
      create buffer bdb for table "dictdb._Database-Feature".
      create query qdb.
      qdb:set-buffers( bdb ).
      qdb:query-prepare( "preselect each dictdb._Database-Feature" ).
    end.

  hasDBParams = no.
  find dictdb._File no-lock where _File-Name = "_DbParams" no-error.
  if available _File then hasDBParams = yes.

  run chkStartup.

  if opsys begins "WIN" then
    do:

      osType = "Windows".

      do on error  undo, leave
         on endkey undo, leave:

        xLine = "".
        input stream inStrm through value( 'wmic os get caption' ).
        import stream inStrm unformatted xline.
        import stream inStrm unformatted osType.
        input stream inStrm close.
        if osType begins "Microsoft Windows" then osType = "Win" + substring( osType, 18 ).

        xLine = "".
        input stream inStrm through value( 'wmic os get version' ).
        import stream inStrm unformatted xline.
        import stream inStrm unformatted winVers.
        input stream inStrm close.

        osType = osType + " " + winVers.

      end.

    end.
   else
    do:
      input stream inStrm through value( "uname -a" ).
      import stream inStrm osType.
      input stream inStrm close.
    end.

  input stream inStrm from value( "etc/deploy.cfg" ).
  import stream inStrm deployType.
  input stream inStrm close.

  /* this stuff should *rarely* change and some of it is very expensive to compute so just do it once here in mon-init
   */

  find first dictdb._DB no-lock.
  dbRecid = recid( _DB ).

  find dictdb._DbStatus no-lock.

  run getRangeData(
    input-output numTbl,
    input-output numIdx,
    input-output numLOB,
    input-output tblBase,
    input-output idxBase,
    input-output tblRange,
    input-output idxRange,
    input-output minTblNum,
    input-output maxTblNum,
    input-output minIdxNum,
    input-output maxIdxNum,
    input-output unMonTbl,
    input-output unMonIdx,
    input-output excessTbl,
    input-output excessIdx,
    input-output hst,
    input-output hsi,
    input-output lxt,
    input-output lxi,
    input-output hxi,
    input-output hxt,
    input-output lmt,
    input-output lmi,
    input-output hmt,
    input-output hmi
  ).

  assign
    tt_configuration.numTbl    = numTbl
    tt_configuration.numIdx    = numIdx
    tt_configuration.numLOBs   = numLOB
    tt_configuration.tblBase   = tblBase
    tt_configuration.idxBase   = idxBase
    tt_configuration.tblRange  = tblRange
    tt_configuration.idxRange  = idxRange
    tt_configuration.unMonTbl  = unMonTbl
    tt_configuration.unMonIdx  = unMonIdx
    tt_configuration.excessTbl = excessTbl
    tt_configuration.excessIdx = excessIdx
  .

  assign
    xdbVersNum = decimal( substring( proversion, 1, index( proversion, "." ) + 1 ))
    so_tbl     = 0
    so_idx     = 0
    so_lob     = 0
    area_bh    = ?
    area_bf1   = ?
    area_bf2   = ?
    rbits      = ?
    t1         = no
    t2         = no
    tt_configuration.numAreas      = 0
    tt_configuration.schemaUsed    = ""
    tt_configuration.areaConfig    = "Default"
    tt_configuration.aiArchiver    = "N/A"
    tt_configuration.oeReplication = "N/A"
    tt_configuration.failClusters  = "N/A"
    tt_configuration.largeFiles    = "N/A"
    tt_configuration.dbAuditing    = "N/A"
    tt_configuration.rpbSettings   = "Homogenous"
    tt_configuration.dbBlkSize     = _dbStatus-dbBlkSize / 1024
  .

  find dictdb._Area no-lock where _Area._Area-number = 6.
  if ( dbBlkSize <> 8 and  _Area-recbits = 5 ) or ( dbBlkSize = 8 and _Area-recbits = 6 ) then
    tt_configuration.rpbSettings = "Default".
   else
    tt_configuration.rpbSettings = string( exp( 2, _Area-recbits )) + " rpb".

  for each dictdb._AreaStatus no-lock where not ( _areaStatus-Areaname matches "*After Image Area*" ),
      dictdb._Area no-lock where dictdb._Area._Area-Number = dictdb._AreaStatus._AreaStatus-Areanum:

    if /* _AreaStatus-Areanum = 6 */ _areaStatus-Areaname = "Schema Area" then
      do:
        for each dictdb._StorageObject no-lock where _StorageObject._DB-Recid = dbRecid and _StorageObject._Area-number = 6
             and _StorageObject._Object-num > 0  
             and _StorageObject._Object-associate > 0:  
          if       _StorageObject._Object-Type = 1 then so_tbl = so_tbl + 1.
           else if _StorageObject._Object-Type = 2 then so_idx = so_idx + 1.
           /*** else LOBs ***/
           else /* if _StorageObject._Object-Type = 3 then */ so_lob = so_lob + 1.
        end.
        if so_tbl > 0 then tt_configuration.schemaUsed = "T: " + string( so_tbl ).
        if so_idx > 0 then tt_configuration.schemaUsed = tt_configuration.schemaUsed + ( if tt_configuration.schemaUsed <> "" then " " else "" ) + "I: " + string( so_idx ).
        if so_lob > 0 then tt_configuration.schemaUsed = tt_configuration.schemaUsed + ( if tt_configuration.schemaUsed <> "" then " " else "" ) + "L: " + string( so_lob ).
        if tt_configuration.schemaUsed = "" then tt_configuration.schemaUsed = "Clean".
      end.

    if _AreaStatus-Areanum > 6 and _areaStatus-Areaname <> "Schema Area" then
      do:

        if rbits = ? then rbits = _Area-RecBits.
        if _Area-RecBits <> rbits then tt_configuration.rpbSettings = "Mixed".

        if hasClstrSz = true then
          do:
            assign        
              area_bh  = buffer _Area:handle
              area_bf2 = area_bh:buffer-field( "_Area-ClusterSize" )
            no-error.
            if area_bf2:buffer-value  > 1 then t2 = true.
            if area_bf2:buffer-value <= 1 then t1 = true.
          end.
        numAreas = numAreas + 1.
      end.

  end.

  if tt_configuration.numAreas = 0 then
    tt_configuration.areaConfig = "Default".
   else
    do:
      if t1 = true  and t2 = true then
        tt_configuration.areaConfig = "Extended:Mixed".
       else
        tt_configuration.areaConfig = ( if t2 = true then "Extended:Type2" else "Extended:Type1" ).
    end.

  tt_configuration.dbHost = dbParam( ldbName( "DICTDB" )).

  if index( tt_configuration.dbHost, "-H " ) > 0 then
    assign
      tt_configuration.dbHost = entry( 2, entry( 1, substring( tt_configuration.dbHost, index( tt_configuration.dbHost, "-H " ))), " " )
    .
   else
    do:
      if       os-getenv( "PTSRV" )        <> ? then tt_configuration.dbHost = os-getenv( "PTSRV" ).
       else if os-getenv( "HOST" )         <> ? then tt_configuration.dbHost = os-getenv( "HOST" ).
       else if os-getenv( "HOSTNAME" )     <> ? then tt_configuration.dbHost = os-getenv( "HOSTNAME" ).
       else if os-getenv( "COMPUTERNAME" ) <> ? then tt_configuration.dbHost = os-getenv( "COMPUTERNAME" ).
       else
        do:
          if opsys begins "win" then
            do:
              input stream inStrm through value( 'wmic os get csname' ).
              import stream inStrm unformatted ^.
              import stream inStrm unformatted tt_configuration.dbHost.
              input stream inStrm close.
            end.
           else
            do:
              input stream inStrm through value( "uname -a" ).
              import stream inStrm ^ tt_configuration.dbHost.
              input stream inStrm close.
            end.
          if tt_configuration.dbHost = ? then tt_configuration.dbHost = "Unknown".
        end.
    end.

  /*
   * OpenEdge Release 10.0B as of Thu Aug  5 21:48:48 EDT 2004
   * OpenEdge Release 10.1A as of Tue Dec 20 22:50:49 EST 2005
   * OpenEdge Release 10.1B as of Wed Jan 10 12:21:31 EST 2007
   * OpenEdge Release 10.1C04 as of Fri May 29 22:13:10 EDT 2009
   * OpenEdge Release 10.2B08 as of Tue Nov 12 19:07:41 EST 2013
   * OpenEdge Release 10.2B08 as of Tue Nov 12 19:07:10 EST 2013
   * OpenEdge Release 10.2B08 as of Tue Nov 12 19:07:41 EST 2013
   * OpenEdge Release 10.2B as of Mon Dec 14 17:00:19 EST 2009
   * OpenEdge Release 11.2 as of Wed Feb 13 19:00:20 EST 2013
   * OpenEdge Release 11.3 as of Wed Jul 17 16:45:16 EDT 2013
   * OpenEdge Release 11.4 as of Fri Jul 25 18:21:09 EDT 2014
   * OpenEdge Release 11.5 as of Fri Dec  5 18:20:55 EST 2014
   * OpenEdge Release 11.6 as of Fri Oct 16 18:22:20 EDT 2015
   * OpenEdge Release 11.7.2 as of Tue Oct 24 18:20:59 EDT 2017
   */

  proRelease = proversion.
  if search( "version" ) <> ? then
    do:
      input stream pv from value( search( "version" )).
      repeat:

        dlcVersion = "".
        import stream pv unformatted dlcVersion.

        dlcVersion = replace( dlcVersion,  "  ", " " ) no-error.
        dlcVersion = replace( dlcVersion,  "  ", " " ) no-error.

        if dlcVersion <> "" and index( dlcVersion, proVersion ) > 0 then
          do:
            proRelease = trim( substring( dlcVersion, index( dlcVersion, proVersion ))).
            proRelease = entry( 1, proRelease, " " ).
          end.

        proVersionDate =
          date(
            lookup( entry( 7, dlcVersion, " " ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" ),
            integer( entry( 8, dlcVersion, " " )),
            integer( entry( 11, dlcVersion, " " ))
          ) no-error.
        proVersionAge = today - proVersionDate.

      end.
    end.
  input stream pv close.
  proRelease = proRelease + ( if string( _dbStatus-ShmVers ) begins "64" then " 64bit" else " 32bit" ).
  proBits = ( if string( _dbStatus-ShmVers ) begins "64" then 64 else 32 ).

  find dictdb._Logging    no-lock.
  find dictdb._MstrBlk    no-lock.

  assign
    tt_configuration.dbLogName    = ldbName( "DICTDB" )
    tt_configuration.dbPhysName   = pdbName( "DICTDB" ) 
    tt_configuration.xdbVersion   = proRelease
    tt_configuration.dbProVersion = proRelease
    tt_configuration.dbBlkSize    = _dbStatus-dbBlkSize / 1024		/* KB	*/
    tt_configuration.aiBlkSize    = _dbStatus-aiBlkSize / 1024		/* KB	*/
    tt_configuration.biBlkSize    = _dbStatus-biBlkSize / 1024		/* KB	*/
    tt_configuration.biClstrSize  = _dbStatus-biClSize  / 1024 		/* MB	*/	/* version dependent calculation needed? */
    tt_configuration.db2PC        =  _Logging-2PC
    tt_configuration.dbTainted    = ( if _dbStatus-Tainted = 0 then "Clean" else "Damaged" )
    tt_configuration.dbIdxBld     = ( if _MstrBlk-Tainted >= 32 then "Failed" else "Ok" )
    tt_configuration.dbSHMSeg     = 0
    tt_configuration.dbSHMSize    = 0
    tt_configuration.dbSHMSegSize = 0
    tt_configuration.dbSHMFree    = 0
  .

  tt_configuration.maxConnect   = integer( getStartUpX( "_Startup-MaxUsers",   "(-n)",  "Maximum Number of Users" )) no-error.
  tt_configuration.maxServers   = integer( getStartUpX( "_Startup-MaxServers", "(-Mn)", "Maximum Number of Servers" )) no-error.
  tt_configuration.cnxPerServ   = integer( getStartUpX( "_Startup-MaxClients", "(-Ma)", "Maximum Number of Clients Per Server" )) no-error.
  tt_configuration.biDelay      = integer( getStartUpX( "_Startup-BIDelay",    "(-Mf)", "Delay of Before-Image Flush" )) no-error.
  tt_configuration.biClstrAge   = integer( getStartUpX( "_Startup-BITrunc",    "(-G)",  "Before-Image Truncate Interval" )) no-error.

  tt_configuration.dbSpin       = integer( getStartUpX( "_Startup-spin",       "(-spin)",   "Current Spin Lock Tries" )) no-error.
  tt_configuration.dbBuffers    = integer( getStartUpX( "_Startup-Buffs",      "(-B)",      "Number of Database Buffers" )) no-error.
  tt_configuration.aiBuffers    = integer( getStartUpX( "_Startup-AIBuffs",    "(-aibufs)", "Number of After-Image Buffers" )) no-error.
  tt_configuration.biBuffers    = integer( getStartUpX( "_Startup-BIBuffs",    "(-bibufs)", "Number of Before-Image Buffers" )) no-error.
  tt_configuration.lkTableSize  = integer( getStartUpX( "_Startup-LockTable",  "(-L)",      "Current Size of Lock Table" )) no-error.

  tt_configuration.directIO     = ( if integer( getStartUpX( "_Startup-directIO",  "(-directio)", "Direct I/O"            )) = 0 then "No" else "Yes" ) no-error.

  /* dbParams reports 0 if -i or -r is NOT enabled, _startup reports 1
   */

  if hasDBParams = yes then
    do:

      if integer( getStartUpX( "_Startup-crashProt", "(-i)", "Crash Recovery" )) = 0 then
        tt_configuration.dbIntegrity = "Enabled".
       else
        tt_configuration.biIOMode = "Disabled".

      if integer( getStartUpX( "_Startup-biIO", "(-r)", "Before-Image File I/O" )) = 0 then
        tt_configuration.biIOMode = "Reliable".
       else
        tt_configuration.biIOMode = "Unreliable".

    end.
   else
    do:

      if integer( getStartUpX( "_Startup-crashProt", "(-i)", "Crash Recovery" )) = 1 then
        tt_configuration.dbIntegrity = "Enabled".
       else
        tt_configuration.biIOMode = "Disabled".

      if integer( getStartUpX( "_Startup-biIO", "(-r)", "Before-Image File I/O" )) = 1 then
        tt_configuration.biIOMode = "Reliable".
       else
        tt_configuration.biIOMode = "Unreliable".

    end.

  if tt_configuration.dbSpin < 0 then
      tt_configuration.dbSpin = tt_configuration.dbSpin + 65536.

  assign
    tt_configuration.afterImaging = ( if _Logging-AIBegin begins "-" then "Disabled" else "Enabled" )
    tt_configuration.aiSeq        = _Logging-AIGenNum
    tt_configuration.dbSize       =  ( _dbStatus-TotalBlks * tt_configuration.dbBlkSize ) / 1024
    tt_configuration.dbEmpty      =  _dbStatus-EmptyBlks
    tt_configuration.biSize       =  _Logging-BILogSize / 1024
    tt_configuration.aiSize       =  _Logging-AILogSize / 1024
    tt_configuration.lkTblHWMpct  = (( _dbStatus-MostLocks / tt_configuration.lkTableSize ) * 100 )
  .

  tt_configuration.excessSHM  = integer( getStartUpX( "_Startup-MemOverflow",       "(-Mxs)",              "Excess shared memory" )) no-error.
  tt_configuration.b2         = integer( getStartUpX( "_Startup-Alternate_Buffs",   "(-B2)",               "Database alternate buffers" )) no-error.
  tt_configuration.lruSkips   = integer( getStartUpX( "_Startup-LRU-Skips",         "(-lruskips)",         "LRU force skips" )) no-error.
  tt_configuration.lru2Skips  = integer( getStartUpX( "_Startup-LRU2-Skips",        "(-lru2skips)",        "LRU2 force skips" )) no-error.
  tt_configuration.pfFactor   = integer( getStartUpX( "_Startup-Prefetch-Factor",   "(-prefetchFactor)",   "Prefetch message fill percentage" )) no-error.
  tt_configuration.pfNumRecs  = integer( getStartUpX( "_Startup-Prefetch-Num-Recs", "(-prefetchNumRecs)",  "Minimum records in prefetch msg" )) no-error.
  tt_configuration.pfPriority = integer( getStartUpX( "_Startup-Prefetch-Priority", "(-prefetchPriority)", "Suspension queue poll priority" )) no-error.
  tt_configuration.nMsgWait   = integer( getStartUpX( "_Startup-NmsgWait",          "(-Nmsgwait)",         "Server network message wait time" )) no-error.
  tt_configuration.pfDelay    =          getStartupX( "_Startup-Prefetch-Delay",    "(-prefetchDelay)",    "Delay first prefetch message" ) no-error.

  tt_configuration.biThold       = integer( getStartUpX( "", "(-bithold)",  "" )) no-error.
  tt_configuration.minCnx        = integer( getStartUpX( "", "(-Mi)",       "" )) no-error.
  tt_configuration.maxMsg        = integer( getStartUpX( "", "(-Mm)",       "" )) no-error.
  tt_configuration.bpMax         = integer( getStartUpX( "", "(-Bpmax)",    "" )) no-error.
  tt_configuration.hashTbl       = integer( getStartUpX( "", "(-hash)",     "" )) no-error.
  tt_configuration.semSets       = integer( getStartUpX( "", "(-semsets)",  "" )) no-error.
  tt_configuration.omSize        = integer( getStartUpX( "", "(-omsize)",   "" )) no-error.
  tt_configuration.pica          = integer( getStartUpX( "", "(-pica)",     "" )) no-error.
  tt_configuration.recSpaceDepth = integer( getStartUpX( "", "(-recspacesearchdepth)", "" )) no-error.

  tt_configuration.dbSHMpinned   =          getStartUpX( "", "(-pinshm)", "locked in memory" ).
  tt_configuration.memCheck      =          getStartUpX( "", "(-MemCheck)",   "" ).
  tt_configuration.dbCheck       =          getStartUpX( "", "(-DbCheck)",    "" ).
  tt_configuration.cpInternal    =          getStartupX( "", "(-cpinternal)", "" ).
  tt_configuration.biStall       =          getStartupX( "", "(-bistall)",    "" ).
  tt_configuration.aiStall       =          getStartupX( "", "(-aistall)",    "" ).

  /* clean up the presentation
   */

  if num-entries( tt_configuration.pfDelay, ":" ) >= 3 then
    tt_configuration.pfDelay = trim( entry( 1, trim( entry( 3, tt_configuration.pfDelay, ":" )), " " ), "." ).

  if tt_configuration.pfDelay = "yes" then tt_configuration.pfDelay = "Enabled".
  if tt_configuration.pfDelay = "no"  then tt_configuration.pfDelay = "Disabled".
  if tt_configuration.pfDelay = ""    then tt_configuration.pfDelay = "N/A".

  assign
    tt_configuration.biThold       = 0  when tt_configuration.biThold = ?
    tt_configuration.minCnx        = 0  when tt_configuration.minCnx = ?
    tt_configuration.maxMsg        = 0  when tt_configuration.maxMsg = ?
    tt_configuration.bpMax         = 0  when tt_configuration.bpMax = ?
    tt_configuration.hashTbl       = 0  when tt_configuration.hashTbl = ?
    tt_configuration.semSets       = 0  when tt_configuration.semSets = ?
    tt_configuration.omSize        = 0  when tt_configuration.omSize = ?
    tt_configuration.pica          = 0  when tt_configuration.pica = ?
    tt_configuration.recSpaceDepth = 0  when tt_configuration.recSpaceDepth = ?
    tt_configuration.dbSHMpinned   = "" when tt_configuration.dbSHMpinned = ?
    tt_configuration.memCheck      = "N/A" when ( tt_configuration.memCheck = ? or tt_configuration.memCheck = "" )
    tt_configuration.dbCheck       = "N/A" when ( tt_configuration.dbCheck  = ? or tt_configuration.dbCheck  = "" )
  .

  tt_configuration.pica = tt_configuration.pica / 1024.			/* -pica is specified in KB but reported in _dbParams in bytes	*/
									/* worse - the .lg file reports -pica in MB :(				*/

  if tt_configuration.aiStall  begins "Not"       then tt_configuration.aiStall  = "Disabled".
  if tt_configuration.biStall  begins "Not"       then tt_configuration.biStall  = "Disabled".
  if tt_configuration.biStall       = "Disabled." then tt_configuration.biStall  = "Disabled".		/* note the "."	*/

  if tt_configuration.memCheck begins "Not"       then tt_configuration.memCheck = "Disabled".
  if tt_configuration.dbCheck  begins "Not"       then tt_configuration.dbCheck  = "Disabled".

  if tt_configuration.biClstrSize < 0 then						/* sometimes _dbStatus is fraked up...	*/
    tt_configuration.biClstrSize = _logging-biclsize / ( 1024 * 1024 ).			/* report MB				*/


  /*
   */


  /* global shared variables used by dc/b2.p
   */

  assign
    bigB      = tt_configuration.dbBuffers
    bigB2     = tt_configuration.b2
    lruSkips  = tt_configuration.lruSkips
    lru2Skips = tt_configuration.lru2Skips
  .

  for each dictdb._Segment no-lock:
    assign
      tt_configuration.dbSHMSeg     = tt_configuration.dbSHMSeg + 1
      tt_configuration.dbSHMSegSize = max( tt_configuration.dbSHMSegSize, _Segment-SegSize )
      tt_configuration.dbSHMSize    = tt_configuration.dbSHMSize + _Segment-SegSize
      tt_configuration.dbSHMFree    = tt_configuration.dbSHMFree + _Segment-ByteFree
    .
  end.

  assign
    tt_configuration.dbSHMSize    = ( tt_configuration.dbSHMSize / ( 1024 * 1024 ))
    tt_configuration.dbSHMSegSize = ( tt_configuration.dbSHMSegSize / ( 1024 * 1024 ))
    tt_configuration.dbSHMFree    = ( tt_configuration.dbSHMFree / ( 1024 * 1024 ))
  .

  assign
    fx    = 0
    vx    = 0
    tt_configuration.aiSettings    = "Disabled"
    tt_configuration.aiAreas       = 0
  .

  for each dictdb._AreaStatus no-lock where _areaStatus-Areaname matches "*After Image Area*",
      dictdb._Area no-lock where dictdb._Area._Area-Number = dictdb._AreaStatus._AreaStatus-Areanum:

    tt_configuration.aiAreas = tt_configuration.aiAreas + 1.

/*
 *  for each dictdb._AreaExtent no-lock where _AreaExtent._Area-Number = _Area._Area-number:
 *    if _Extent-Type = 4 then vx = vx + 1. else fx = fx + 1.
 *  end.
 */

    for each tt_AreaExtent no-lock where tt_AreaExtent.areaNum = _Area._Area-number:
      if extType = 4 then vx = vx + 1. else fx = fx + 1.
    end.

    if       fx > 0 and vx = 0 then tt_configuration.aiSettings = "Fixed".
     else if vx > 0 and fx = 0 then tt_configuration.aiSettings = "Variable".
     else if vx > 0 and fx > 0 then tt_configuration.aiSettings = "Mixed".

  end.

  if xdbVersNum >= 9.1 then
    do:
      assign
        tt_configuration.oeReplication = "Available"        /* need a way to tell if these are enabled		*/
        tt_configuration.failClusters  = "Available"	    /* proutil dbName -C describe? 			*/
        tt_configuration.largeFiles    = "Available"
      .
    end.

  if xdbVersNum >= 10.0 then
    do:

      qdb:query-open.
      qdb:get-next().
      do while not qdb:query-off-end:

        fStatus = "Available".
        if bdb:buffer-field( "_DBFeature_Enabled" ):buffer-value() = "1" then fStatus = "Enabled" no-error.

/*      if _DBFeature_Active  = "1" then fStatus = "Active".  */

        case string( bdb:buffer-field( "_DBFeature_Name" ):buffer-value()):
          when "OpenEdge Replication"           then tt_configuration.oeReplication = fStatus.
          when "Failover Clusters"              then tt_configuration.failClusters  = fStatus.
          when "Large Files"                    then tt_configuration.largeFiles    = fStatus.
          when "Database Auditing"              then tt_configuration.dbAuditing    = fStatus.
          when "After Image Mangement/Archiver" then tt_configuration.aiArchiver    = fStatus.
        end.

        qdb:get-next().

      end.
    end.

&IF DEFINED( OE10 ) &THEN

  file-info:file-name = "./dbanalys/" + pt_shortname + ".dba".
  if file-info:full-pathname <> ? then
    do:

      xLine = "".
      input stream inStrm from value( file-info:full-pathname ).
      repeat:
        import stream inStrm unformatted xline.
        if xline begins "Date:" then
          do:
            xline = replace( xLine, ":", " " ).
            xline = replace( xLine, "  ", " " ).
            dbaAsOf = datetime(
              lookup( entry( 3, xLine, " " ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" ),
              integer( entry( 4, xLine, " " )),
              integer( entry( 8, xLine, " " )),
              integer( entry( 5, xLine, " " )),
              integer( entry( 6, xLine, " " )),
              integer( entry( 7, xLine, " " ))
            ).
            dbaAge = abs( interval( now, dbaAsOf, "days" )).
            dbaAsOfStr = string( dbaAsOf, "9999/99/99" ).
            leave.
          end.
      end.
      input stream inStrm close.

    end.

  for each _file no-lock:
    for each _systblstat no-lock where _tblId = _file-num and _property = 2:
      if _systblstat._val_ts <> ? then SQLstatsTS = max( datetime( _val_ts ), datetime( 1/1/1970 )).
    end.
  end.

  if SQLStatsTS = datetime( 1/1/1970 ) then
    assign
      SQLStatsTS  = ?
      SQLStats    = 0
      SQLStatsStr = ""
    .
   else
    assign
      SQLStats    = abs( interval( now, SQLStatsTS, "days" ))
      SQLStatsStr = string( SQLStatsTS, "9999/99/99" )
    .

&ENDIF

  create query qh.
  qh:set-buffers( buffer tt_configuration:handle ).
  qh:query-prepare( "preselect each tt_configuration" ).
  qh:query-open. 

  if dbgMode >= 1 then message {&NOW} "initial config scan complete".

  return.

end.

define variable lastRefresh as integer no-undo initial ?.

procedure mon-update:

  define input parameter argList as character no-undo.

  define variable f  as integer no-undo.
  define variable ii as integer no-undo.

  define variable bf as handle  no-undo.

  define variable xLine       as character no-undo.
  define variable lgEarliest  as date no-undo.

  define variable chartList1  as character no-undo.

  define variable upDays      as integer no-undo.
  define variable upSecs      as integer no-undo.
  define variable currDT      as integer no-undo.
  define variable fBackUp     as integer no-undo.
  define variable iBackUp     as integer no-undo.
  define variable bkupDays    as integer no-undo.
  define variable bkupSecs    as integer no-undo.

  define variable bfree       as decimal no-undo.
  define variable used        as decimal no-undo.
  define variable blks-alloc  as decimal no-undo format ">>>>>>>>>>>>9".
  define variable pct-alloc   as decimal no-undo format ">>>>>>9%".
  define variable vsize       as decimal no-undo format ">>>>>>>>>>>>9".

  qh:query-close no-error.
  find tt_configuration no-error.
  if not available( tt_configuration ) then run mon-init.

  find first dictdb._ActSummary no-lock.

  assign
    upSecs = _Summary-upTime
    upDays = integer( truncate( upSecs / 86400, 0 ))
    upSecs = upSecs modulo 86400
    tt_configuration.dbUpTime  = ( if upDays > 0 then string( upDays ) + "d " else "" ) + string( upSecs, "hh:mm:ss" ).
  .

  find dictdb._DbStatus   no-lock.
  find dictdb._Logging    no-lock.
  find dictdb._MstrBlk    no-lock.

  assign
    currDT =  uDateTime()
    fBackUp = ( currDT - string2uDateTime( _dbStatus-fbDate ))
    iBackUp = ( currDT - string2uDateTime( _dbStatus-ibDate ))
  .

  if _dbStatus-fbDate = ? and _dbStatus-fbDate = ? then
    tt_configuration.dbBkUpAge  = "Never!".
   else
    do:
      bkupSecs = min( fBackUp, iBackUp ).
      if fBackup = ? then bkupSecs = iBackUp.
      if iBackup = ? then bkupSecs = fBackUp.
      assign
        bkupDays = integer( truncate( bkupSecs / 86400, 0 ))
        bkupSecs = bkupSecs modulo 86400
        tt_configuration.dbBkUpAge  = ( if bkupDays > 0 then string( bkupDays ) + "d " else "" ) + string( bkupSecs, "hh:mm" ).
      .
    end.
    
  if tt_configuration.dbBkUpAge = ? then tt_configuration.dbBkUpAge = "Unknown".

  assign
    upSecs = _Summary-upTime
    upDays = integer( truncate( upSecs / 86400, 0 ))
    upSecs = upSecs modulo 86400
    tt_configuration.dbUpTime  = ( if upDays > 0 then string( upDays ) + "d " else "" ) + string( upSecs, "hh:mm:ss" ).
  .

  /* the stuff below doesn't change very often -- so don't keep beating on the db/network
   */

  if lastRefresh <> ? and abs( time - lastRefresh ) < ( monInt * 10 ) then
    do:
      add2ds( temp-table tt_configuration:default-buffer-handle ).
      return.
   end.

  lastRefresh = time.

  if dbgMode >= 4 then message {&NOW} "refreshing config info".

  publish "check-protop-config".


  /*
   * OpenEdge Release 10.0B as of Thu Aug  5 21:48:48 EDT 2004
   * OpenEdge Release 10.1A as of Tue Dec 20 22:50:49 EST 2005
   * OpenEdge Release 10.1B as of Wed Jan 10 12:21:31 EST 2007
   * OpenEdge Release 10.1C04 as of Fri May 29 22:13:10 EDT 2009
   * OpenEdge Release 10.2B08 as of Tue Nov 12 19:07:41 EST 2013
   * OpenEdge Release 10.2B08 as of Tue Nov 12 19:07:10 EST 2013
   * OpenEdge Release 10.2B08 as of Tue Nov 12 19:07:41 EST 2013
   * OpenEdge Release 10.2B as of Mon Dec 14 17:00:19 EST 2009
   * OpenEdge Release 11.2 as of Wed Feb 13 19:00:20 EST 2013
   * OpenEdge Release 11.3 as of Wed Jul 17 16:45:16 EDT 2013
   * OpenEdge Release 11.4 as of Fri Jul 25 18:21:09 EDT 2014
   * OpenEdge Release 11.5 as of Fri Dec  5 18:20:55 EST 2014
   * OpenEdge Release 11.6 as of Fri Oct 16 18:22:20 EDT 2015
   * OpenEdge Release 11.7.2 as of Tue Oct 24 18:20:59 EDT 2017
   */

  proRelease = proversion.
  if search( "version" ) <> ? then
    do:
      input stream pv from value( search( "version" )).
      repeat:

        dlcVersion = "".
        import stream pv unformatted dlcVersion.

        dlcVersion = replace( dlcVersion,  "  ", " " ) no-error.
        dlcVersion = replace( dlcVersion,  "  ", " " ) no-error.

        if dlcVersion <> "" and index( dlcVersion, proVersion ) > 0 then
          do:
            proRelease = trim( substring( dlcVersion, index( dlcVersion, proVersion ))).
            proRelease = entry( 1, proRelease, " " ).
          end.

        proVersionDate =
          date(
            lookup( entry( 7, dlcVersion, " " ), "Jan,Feb,Mar,Apr,May,Jun,Jul,Aug,Sep,Oct,Nov,Dec" ),
            integer( entry( 8, dlcVersion, " " )),
            integer( entry( 11, dlcVersion, " " ))
          ) no-error.
        proVersionAge = today - proVersionDate.

      end.
    end.
  input stream pv close.
  proRelease = proRelease + ( if string( _dbStatus-ShmVers ) begins "64" then " 64bit" else " 32bit" ).
  proBits = ( if string( _dbStatus-ShmVers ) begins "64" then 64 else 32 ).


  assign
    tt_configuration.allocBlks     = 0
    tt_configuration.varBlks       = 0
    tt_configuration.warnAreas     = 0
    tt_configuration.worstArea     = 0
    tt_configuration.numExtents    = 0
    tt_configuration.lrgExtent     = 0
  .

  for each tt_area:
    if pctLastX > 90 then tt_configuration.warnAreas = tt_configuration.warnAreas + 1.
    tt_configuration.worstArea =max( tt_configuration.worstArea, pctAlloc ).
  end.

  file-info:file-name = pdbname(1) + ".lg".
  if file-info:full-pathname <> ? then
    do:

      tt_configuration.lgFileSize = ( file-info:file-size / ( 1024 * 1024 )).

      assign
        ii = 0
        lgEarliest = ?
      .
      input stream inStrm from value( file-info:full-pathname ).
      repeat:
        import stream inStrm xLine.
        ii = ii + 1.
        if substring( xLine, 30, 1 ) = "]" then				/* verify that this is a line from an oe10+ .lg file	*/
          do:
            lgEarliest = date( substring( xLine, 2, 10 )) no-error.	/* [2018/01/26@15:44:08.034-0500] P-4453...		*/
            if lgEarliest <> ? then tt_configuration.lgFileDays = ( today - lgEarliest ) + 1.
            leave.
          end.
        if ii > 10 then leave. 
      end.
      input stream inStrm close.

    end.

  assign
    blks-alloc = 0
    vsize      = 0
  .

  for each dictdb._FileList no-lock:

    if _FileList-OpenMode = "UNBUF" then next.		/* skip bi extents						*/

    assign
      tt_configuration.lrgExtent  = max( _FileList-Size, tt_configuration.lrgExtent )
      tt_configuration.numExtents = tt_configuration.numExtents + 1
    .

    if _FileList-OpenMode = "BOTHIO" then		/* works to identify variable data extents -- not for bi	*/
      vsize = vsize + _FileList-Size.
     else
      blks-alloc = blks-alloc + _FileList-Size.

/* this is strange -- why the substring?
 *
 *
 *  find first tt_AreaExtent no-lock where index( _FileList-Name, substring( extPath, 3 )) > 0 no-error.
 *  if available tt_areaExtent then
 *    do:
 *      if extType = 5 then
 *        vsize = vsize + _FileList-Size.
 *       else if extType >= 7 then
 *        blks-alloc = blks-alloc + _FileList-Size.
 *    end.
 *
 */

  end.

  assign
    tt_configuration.lrgExtent = tt_configuration.lrgExtent / ( 1024 * 1024 )		/* GB */
    tt_configuration.allocBlks = tt_configuration.allocBlks + ( blks-alloc / 1024 )
    tt_configuration.varBlks   = tt_configuration.varBlks   + ( vsize / 1024 )
  .

  /* these could change online (in some cases that might be slightly "aspirational")
   */

  if tt_configuration.dbBuffers <> integer( getStartUpX( "_Startup-Buffs", "(-B)", "Number of Database Buffers" )) then
    do:

      assign
        tt_configuration.dbSHMSeg  = 0
        tt_configuration.dbSHMSize = 0
        tt_configuration.dbSHMFree = 0
      .

      for each dictdb._Segment no-lock:
        assign
          tt_configuration.dbSHMSeg     = tt_configuration.dbSHMSeg + 1
          tt_configuration.dbSHMSegSize = max( tt_configuration.dbSHMSegSize, _Segment-SegSize )
          tt_configuration.dbSHMSize    = tt_configuration.dbSHMSize + _Segment-SegSize
          tt_configuration.dbSHMFree    = tt_configuration.dbSHMFree + _Segment-ByteFree
        .
      end.

      assign
        tt_configuration.dbSHMSize    = ( tt_configuration.dbSHMSize / ( 1024 * 1024 ))
        tt_configuration.dbSHMSegSize = ( tt_configuration.dbSHMSegSize / ( 1024 * 1024 ))
        tt_configuration.dbSHMFree    = ( tt_configuration.dbSHMFree / ( 1024 * 1024 ))
      .

    end.

  tt_configuration.dbSpin       = integer( getStartUpX( "_Startup-spin",       "(-spin)",   "Current Spin Lock Tries" )) no-error.
  tt_configuration.dbBuffers    = integer( getStartUpX( "_Startup-Buffs",      "(-B)",      "Number of Database Buffers" )) no-error.
  tt_configuration.aiBuffers    = integer( getStartUpX( "_Startup-AIBuffs",    "(-aibufs)", "Number of After-Image Buffers" )) no-error.
  tt_configuration.biBuffers    = integer( getStartUpX( "_Startup-BIBuffs",    "(-bibufs)", "Number of Before-Image Buffers" )) no-error.
  tt_configuration.lkTableSize  = integer( getStartUpX( "_Startup-LockTable",  "(-L)",      "Current Size of Lock Table" )) no-error.

  if tt_configuration.dbSpin < 0 then
      tt_configuration.dbSpin = tt_configuration.dbSpin + 65536.

  assign
    tt_configuration.lkTblHWMpct  = (( _dbStatus-MostLocks / tt_configuration.lkTableSize ) * 100 )
    tt_configuration.afterImaging = ( if _Logging-AIBegin begins "-" then "Disabled" else "Enabled" )
    tt_configuration.aiSeq        = _Logging-AIGenNum
    tt_configuration.dbSize       =  ( _dbStatus-TotalBlks * tt_configuration.dbBlkSize ) / 1024
    tt_configuration.dbEmpty      =  _dbStatus-EmptyBlks
    tt_configuration.biSize       =  _Logging-BILogSize / 1024
    tt_configuration.aiSize       =  _Logging-AILogSize / 1024
  .

  /* global shared variables used by dc/b2.p
   */

  assign
    bigB      = tt_configuration.dbBuffers
    bigB2     = tt_configuration.b2
    lruSkips  = tt_configuration.lruSkips
    lru2Skips = tt_configuration.lru2Skips
  .

  tt_configuration.hashPct = ( tt_configuration.hashTbl / ( bigB + bigB2 )) *  100.

  add2ds( temp-table tt_configuration:default-buffer-handle ).

  return.

end.

{ssg/dirtyschema.i}
{ssg/hashpct.i}

return.

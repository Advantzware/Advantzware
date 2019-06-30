/* ttinfo.p
 *
 * show some useful information about this session's temp-tables
 *
 * triggered by the ^t command
 *
 * temp-table info requires OpenEdge 11 or higher and you must set "TTDEBUG=yes" in bin/localenv
 *
 * You must also uncomment the -tt* parameters in etc/protop.pf
 *
 *	...
 *	# these define the temp-table stats collection for oe11 clients
 *	# older clients should ignore these parameters (but we comment them out anyway).
 *	
 *	-ttbaseindex 1
 *	-ttbasetable 1
 *	-ttindexrangesize 1000
 *	-tttablerangesize 1000
 *
 * of interest:
 *
 *	http://knowledgebase.progress.com/articles/Article/P95826
 *
 *	-tmpbsize 1 =  32 rows per block
 *	-tmpbsize 4 = 256 rows per block
 *	-tmpbsize 8 = 256 rows per block
 */


&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 11.0 &THEN

&global-define	TTSTATS

&ENDIF

define stream outStrm.

define variable tblFilter   as character no-undo format "x(50)" label "  Table Filter".		/* user defined filter		*/
define variable outFileName as character no-undo format "x(50)" label "   Output File".		/* user defined output file	*/

define variable x_id as integer no-undo.

define temp-table tt_ttInfo no-undo
  field xid        as integer   label "xId"     format ">>>>9"
  field ttId       as integer   label "TT Id"   format ">>>>9"
  field idxId      as integer   label "Idx Id"  format ">>>>>"
  field ttName     as character label "TT Name" format "x(30)"
  field ttProcName as character label "Procedure Name" format "x(20)"
  field ttDeleted  as datetime  label "Was Deleted"
  field numRecs    as int64     label "Records" format ">>>>>>>"
  field numBytes   as int64     label "Bytes"   format ">>>>>>>"
  field ts_create  as int64     label "Create"  format ">>>>>>>"
  field ts_read    as int64     label "Read"    format ">>>>>>>"
  field ts_update  as int64     label "Update"  format ">>>>>>>"
  field ts_delete  as int64     label "Delete"  format ">>>>>>>"
  field ts_osread  as int64     label "OSRead"  format ">>>>>"
  index xId-idx  is primary unique xid
  index ttId-idx ttId
  index idxId-idx idxId
  index ttName-idx ttName
.

/* useful "public properties" */

define variable numCurrTT       as integer   no-undo.
define variable numPeakTT       as integer   no-undo.
define variable numArchTT       as integer   no-undo.

define variable numTTIdx        as integer   no-undo.
define variable numTTTbl        as integer   no-undo.
define variable totRecs         as integer   no-undo.
define variable totBytes        as integer   no-undo.

define variable ttSumCreate     as int64     no-undo.
define variable ttSumRead       as int64     no-undo.
define variable ttSumUpdate     as int64     no-undo.
define variable ttSumDelete     as int64     no-undo.
define variable ttSumLogRd      as int64     no-undo.
define variable ttSumOSRd       as int64     no-undo.
define variable ttSumOSWr       as int64     no-undo.
define variable ttSumTRX        as int64     no-undo.
define variable ttSumUndo       as int64     no-undo.

define variable ttHR            as decimal   no-undo format ">>9.99%".

define variable DBIName         as character no-undo.
define variable DBIFullPath     as character no-undo.
define variable DBIBlkSz        as integer   no-undo.
define variable DBISize         as int64     no-undo.

define variable DBITotBlks      as int64     no-undo.
define variable DBIEmptyBlks    as int64     no-undo.
define variable DBIFreeBlks     as int64     no-undo.
define variable DBIRMFreeBlks   as int64     no-undo.

/* "useful" might be an exaggeration... */

define variable SpaceDbExd      as int64 no-undo.
define variable SpaceTakeFree   as int64 no-undo.
define variable SpaceRetFree    as int64 no-undo.
define variable SpaceAllocNewRm as int64 no-undo.
define variable SpaceFromRm     as int64 no-undo.
define variable SpaceFromFree   as int64 no-undo.
define variable SpaceBytesAlloc as int64 no-undo.
define variable SpaceExamined   as int64 no-undo.
define variable SpaceRemoved    as int64 no-undo.
define variable SpaceFrontAdd   as int64 no-undo.
define variable SpaceBackAdd    as int64 no-undo.
define variable SpaceFront2Back as int64 no-undo.


/* install self as a session super-procedure and wait for "protop_showTT"
 */

session:add-super-procedure( this-procedure ).

subscribe to "protop_showTT" anywhere run-procedure "showTT".

return.


/* count numRec and calculate numBytes
 */

procedure ttAnalys:

  define input parameter x as integer no-undo.

&IF DEFINED( TTSTATS ) &THEN

  define variable q  as handle    no-undo.
  define variable b  as handle    no-undo.
  define variable h  as handle    no-undo.

  define variable p  as character no-undo.

  define variable r  as integer   no-undo.
  define variable s  as integer   no-undo.

  find tt_ttInfo where tt_ttInfo.xid = x no-error.
  if not available( tt_ttInfo ) then
    do:
      message "no such tt_ttInfo:" x.
      return.
    end.

  Progress.Database.TempTableInfo:GetTableInfoById( tt_ttInfo.ttId, output h, output p ).
  b = h:default-buffer-handle.

  if h:has-Records = no then return.

  create query q.
  q:forward-only = no.

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().
  do while b:available:

    assign
      r = r + 1
      s = s + b:record-length
    .

    q:get-next().

  end.

  q:query-close.
  delete object q.

  /* we need to re-find this because we may have just counted it and thus moved the default buffer!
   */

  find tt_ttInfo where tt_ttInfo.xid = x no-error.
  if available( tt_ttInfo ) then
    assign
      tt_ttInfo.numRecs    = r
      tt_ttInfo.numBytes   = s
    .

&ENDIF

  return.

end.


procedure getTTTblList:
&IF DEFINED( TTSTATS ) &THEN

  define variable h  as handle    no-undo.
  define variable b  as handle    no-undo.

  define variable t  as integer   no-undo.	/* tt tbl id		*/
  define variable p  as character no-undo.	/* program name 	*/

  define variable tblName as character no-undo format "x(30)".

  find last tt_ttInfo no-error.
  if available( tt_ttInfo ) then x_id = tt_ttInfo.xid.

  numTTTbl = 0.

  /* TTPeak is the highest TT Id used by the session (so far)
   */

  do t = 1 to Progress.Database.TempTableInfo:TempTablePeak:

    assign
      p = ?		/* procedure name associated with the tt	*/
      h = ?		/* tt handle					*/
      b = ?		/* defautl buffer handle for the tt		*/
    .

    Progress.Database.TempTableInfo:GetTableInfoById( t, output h, output p ).

    if valid-handle( h ) then
      do:

        assign
          b = h:default-buffer-handle
          tblName = b:name
        .

        find tt_ttInfo where tt_ttInfo.ttId = t no-error.
        if not available tt_ttInfo then
          do:
            create tt_ttInfo.
            assign
              numTTTbl             = numTTTbl + 1
              x_id                 = x_id + 1
              tt_ttInfo.xId        = x_id
              tt_ttInfo.ttId       = t
              tt_ttInfo.ttName     = tblName
              tt_ttInfo.ttProcName = p
            .
          end.

      end.

/*    if tblName begins "_IndexStatHist" then leave.	*/ /* if TT archiving is enabled this will be the last table name */

/*    if numTTTbl >= Progress.Database.TempTableInfo:TempTableCount then leave. */

  end.

&ENDIF

  return.

end.


procedure getTTTblCRUD:
&IF DEFINED( TTSTATS ) &THEN

  define variable q  as handle    no-undo.
  define variable b  as handle    no-undo.
  define variable h  as handle    no-undo.

  define variable t  as character no-undo.
  define variable p  as character no-undo.

  define variable n  as integer   no-undo.

  create query q.
  q:forward-only = no.

  /* temp-table stats "VST"
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:TableStatId ).
  b = h:default-buffer-handle.

  /* b:write-xmlschema( "FILE", "tablestat.xsd", true, ?, ? ). */	/* dump the definition... */

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().
  do while b:available:

    if Progress.Database.TempTableInfo:GetTableInfoById( b::_tableStat-Id, output t, output p ) then
      n = n + 1.
     else
      do:
        q:get-next().
        next.
      end.

    find tt_ttInfo where tt_ttInfo.ttId = b::_tableStat-Id no-error. 

    if not available( tt_ttInfo ) then		/* a new TT has been discovered!	*/
      do:
        create tt_ttInfo.
        assign
          x_id                 = x_id + 1
          tt_ttInfo.xId        = x_id
          tt_ttInfo.ttId       = b::_tableStat-Id
          tt_ttInfo.ttName     = t
          tt_ttInfo.ttProcName = p
        .
      end.
      
      assign
        tt_ttInfo.ts_create = int64( b::_TableStat-Create )
        tt_ttInfo.ts_read   = int64( b::_TableStat-Read   )
        tt_ttInfo.ts_update = int64( b::_TableStat-Update )
        tt_ttInfo.ts_delete = int64( b::_TableStat-Delete )
        tt_ttInfo.ts_osread = int64( b::_TableStat-OSRead )
      .

    run ttAnalys( tt_ttInfo.xId ).

    /* if t begins "_IndexStatHist" then leave.	*/ /* if TT archiving is enabled this will be the last table name */

    /* if n >= Progress.Database.TempTableInfo:TempTableCount then leave. */

    q:get-next().

  end.

  q:query-close.

  delete object q.

  numCurrTT = Progress.Database.TempTableInfo:TempTableCount.
  numPeakTT = Progress.Database.TempTableInfo:TempTablePeak.

&ENDIF

  return.

end.


procedure getArchivedTTList:
&IF DEFINED( TTSTATS ) &THEN

  define variable q  as handle    no-undo.
  define variable b  as handle    no-undo.
  define variable h  as handle    no-undo.

  numArchTT = 0.

  if Progress.Database.TempTableInfo:ArchiveTableStatistics = no or
     Progress.Database.TempTableInfo:ArchiveIndexStatistics = no then return.

  create query q.
  q:forward-only = no.

  find last tt_ttInfo no-error.
  if available( tt_ttInfo ) then x_id = tt_ttInfo.xid.

  /* archived temp-tables
   */

  h = Progress.Database.TempTableInfo:GetTableStatHistoryHandle().
  b = h:default-buffer-handle.

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().
  do while b:available:

    assign
      x_id = x_id + 1
      numArchTT = numArchTT + 1
    .

    if b:name <> "_tablestathist" then	/* why wouldn't it?	*/
      do:
        message b:name "<> _tableStatHist!".
      end.
     else
      do:

        create tt_ttInfo.
        assign
          xid  = x_id
          ttid = ?
          tt_ttInfo.ttName     = b::_Table-name
          tt_ttInfo.ttProcName = b::_Prog-name
          tt_ttInfo.ttDeleted  = datetime( b::_Delete-Timestamp )
          tt_ttInfo.ts_create  = int64( b::_TableStat-Create )
          tt_ttInfo.ts_read    = int64( b::_TableStat-Read )
          tt_ttInfo.ts_update  = int64( b::_TableStat-Update )
          tt_ttInfo.ts_delete  = int64( b::_TableStat-Delete )
          tt_ttInfo.ts_osread  = int64( b::_TableStat-OSRead )
        .

      end.

    q:get-next().

  end.

  q:query-close.

  delete object q.

&ENDIF

  return.

end.


procedure getTTIdxList:
&IF DEFINED( TTSTATS ) &THEN

  define variable h  as handle    no-undo.
  define variable b  as handle    no-undo.

  define variable j  as integer   no-undo.	/* tt id number		*/
  define variable k  as integer   no-undo.	/* tt idx number	*/
  define variable r  as integer   no-undo.	/* number of records	*/
  define variable s  as integer   no-undo.	/* record size		*/
  define variable p  as character no-undo.	/* program name 	*/

  define variable xIdxName as character no-undo format "x(30)".
  define variable xTblName as character no-undo format "x(30)".

  find last tt_ttInfo no-error.
  if available( tt_ttInfo ) then x_id = tt_ttInfo.xid.

  numTTIdx = 0.

  /* TTPeak is the highest TT Id used by the session (so far) -- there is no IdxPeak so we guess that 10,000 ought to be enough for anyone :(
   */

  do k = 1 to 10000:

    assign
      xIdxName  = ?
      xTblname = ?
      p = ?
      h = ?
      b = ?
    .

    Progress.Database.TempTableInfo:GetIndexInfoById( k, output xIdxName, output h, output p ).

    if valid-handle( h ) then
      do:

        assign
          numTTIdx = numTTIdx + 1
          x_id     = x_id + 1
          b        = h:default-buffer-handle
          xTblName = b:name
        .

        if false then b:write-xmlschema( "FILE", "indexinfo.xsd", true, ?, ? ).		/* dump the definition... */

        if xTblName begins "_IndexStatHist" or xTblName begins "_TableStatHist" then leave.

        find first tt_ttInfo
          where tt_ttInfo.ttName = entry( 1, xIdxName, "." )
            and tt_ttInfo.ttProcName = p
          no-error
        .
        j = ?.
        if available tt_ttInfo then
          j = ttId.
         else
          do:
            message "no tt named:" entry( 1, xIdxName, "." ) xTblName j k xIdxName p.
            next.
          end.

        find first tt_ttInfo where tt_ttInfo.idxId = k no-error.

        if not available( tt_ttInfo ) then
          do:
            create tt_ttInfo.
            assign
              tt_ttInfo.xId        = x_id
              tt_ttInfo.ttId       = j
              tt_ttInfo.idxId      = k
              tt_ttInfo.ttName     = xIdxName
              tt_ttInfo.ttProcName = p
            .
          end.

      end.

  end.

&ENDIF

  return.

end.


procedure getTTIdxCRUD:
&IF DEFINED( TTSTATS ) &THEN

  define variable q  as handle    no-undo.
  define variable b  as handle    no-undo.
  define variable h  as handle    no-undo.

  create query q.
  q:forward-only = no.

  /* loop through temp-table index stats "VST"
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:IndexStatId ).
  b = h:default-buffer-handle.

  if false then b:write-xmlschema( "FILE", "indexstat.xsd", true, ?, ? ).	/* dump the definition... */

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().
  do while b:available:

    find tt_ttInfo where idxId = b::_indexStat-Id no-error. 
    if available( tt_ttInfo ) then
      assign
        tt_ttInfo.ts_create = int64( b::_IndexStat-Create )
        tt_ttInfo.ts_read   = int64( b::_IndexStat-Read )
      .

    q:get-next().

  end.

  q:query-close.

  delete object q.

&ENDIF

  return.

end.


procedure getTTInfo:
&IF DEFINED( TTSTATS ) &THEN

  define variable q  as handle    no-undo.
  define variable b  as handle    no-undo.
  define variable h  as handle    no-undo.
  define variable s  as handle    no-undo.
  define variable f  as handle    no-undo.

  /* add up the total number of records and size thereof
   */

  assign
    totRecs  = 0
    totBytes = 0
  .

  for each tt_ttInfo:
    assign
      totRecs  = totRecs  + tt_ttInfo.numRecs
      totBytes = totBytes + tt_ttInfo.numBytes
    .
  end.

  create query q.
  q:forward-only = no.

  /* get the TT db hit ratio
   *     hr = 100.0 * (( lrx - osrx ) / lrx )
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:ActSummaryId ).
  s = h:default-buffer-handle.

  if false then s:write-xmlschema( "FILE", "actsummary.xsd", true, ?, ? ).	/* dump the definition... */

  q:set-buffers( s ).
  q:query-prepare( "preselect each " + s:name ).
  q:query-open.

  q:get-first().
  ttHR = 100 * int64( s::_Summary-DbAccesses - s::_Summary-DbReads ) / int64( s::_Summary-DbAccesses ).

  assign
    ttSumCreate = s::_Summary-RecCreat
    ttSumRead   = s::_Summary-RecReads
    ttSumUpdate = s::_Summary-RecUpd
    ttSumDelete = s::_Summary-RecDel
    ttSumLogRd  = s::_Summary-Dbaccesses
    ttSumOSRd   = s::_Summary-Dbreads
    ttSumOSWr   = s::_Summary-Dbwrites
    ttSumTRX    = s::_Summary-Commits
    ttSumUndo   = s::_Summary-Undos
  .

  q:query-close.

  /* use _filelist to discover the DBI file name
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:FileListId ).
  f = h:default-buffer-handle.

  if false then f:write-xmlschema( "FILE", "filelist.xsd", true, ?, ? ).	/* dump the definition... */

  q:set-buffers( f ).
  q:query-prepare( "preselect each " + f:name ).
  q:query-open.

  q:get-first().

  DBIName  = f::_FileList-Name.
  DBIBlkSz = f::_FileList-BlkSize.

  DBIFullPath = substitute( "&1&2", session:temp-directory, DBIName ).
  file-info:file-name = DBIFullPath.
  DBISize = file-info:file-size.

  if DBISize = ? then DBIFullPath = DBIFullPath + " <unlinked>".

  q:query-close.


/***

  /* _actbuffer -- nothing very helpful but it might be useful for discussing the use of the LRU chain in temp-tables
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:ActBufferId ).
  b = h:default-buffer-handle.

  if false then b:write-xmlschema( "FILE", "actbuffer.xsd", true, ?, ? ).	/* dump the definition... */

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().

  do while b:available:
    /* message b::_Buffer-Id b::_Buffer-LRUEnabled b::_Buffer-LRUSkips. */ /* yes, there are 3 ids, LRUskips = 0 */
    q:get-next().
  end.

  q:query-close.

 ***/

  /* _ActSpace (mostly RM Chain stuff)
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:ActSpaceId ).
  b = h:default-buffer-handle.

  if false then b:write-xmlschema( "FILE", "actspace.xsd", true, ?, ? ).	/* dump the definition... */

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().

  do while b:available:

    assign
      SpaceDbExd      = b::_Space-DbExd
      SpaceTakeFree   = b::_Space-TakeFree
      SpaceRetFree    = b::_Space-RetFree
      SpaceAllocNewRm = b::_Space-AllocNewRm
      SpaceFromRm     = b::_Space-FromRm
      SpaceFromFree   = b::_Space-FromFree
      SpaceBytesAlloc = b::_Space-BytesAlloc
      SpaceExamined   = b::_Space-Examined
      SpaceRemoved    = b::_Space-Removed
      SpaceFrontAdd   = b::_Space-FrontAdd
      SpaceBackAdd    = b::_Space-BackAdd
      SpaceFront2Back = b::_Space-Front2Back
    .

    q:get-next().

  end.

  q:query-close.


  /* _DbStatus
   */

  h = Progress.Database.TempTableInfo:GetVSTHandle( Progress.Database.VSTTableId:DbStatusId ).
  b = h:default-buffer-handle.

  if false then b:write-xmlschema( "FILE", "dbstatus.xsd", true, ?, ? ).	/* dump the definition... */

  q:set-buffers( b ).
  q:query-prepare( "preselect each " + b:name ).
  q:query-open.

  q:get-first().

  do while b:available:

    assign
      DBITotBlks    = b::_DbStatus-TotalBlks
      DBIEmptyBlks  = b::_DbStatus-EmptyBlks
      DBIFreeBlks   = b::_DbStatus-FreeBlks
      DBIRMFreeBlks = b::_DbStatus-RMFreeBlks
    .

    /* message "_DbStatus-LastTran"  b::_DbStatus-LastTran.	*/	/* # of commits + 64 ... */

    q:get-next().

  end.

  q:query-close.

&ENDIF

  return.

end.


procedure showTT:

  define variable i as integer no-undo.

  pause before-hide.

  empty temp-table tt_ttInfo.

  run getTTTblList.
  run getTTTblCRUD.

  run getTTIdxList.
  run getTTIdxCRUD.

  run getArchivedTTList.

  run getTTInfo.

&IF DEFINED( TTSTATS ) &THEN

  /* summary of tt metrics
   */

  message
    skip(1)
    DBIFullPath /* + fill( " ", 120 - length( DBIFullPath )) */
    skip(1)
    string( DBIBlkSz,      ">>>>>>>>>>9" ) "DBI Block Size      "     string( numCurrTT,     ">>>>>>>>>>9" ) "current temp-tables   " skip
    if DBISize = ? then    "          ?" else string( DBISize,       ">>>>>>>>>>9" )

                                           "DBI File Size       "     string( numArchTT,     ">>>>>>>>>>9" ) "archived              " skip

    string( DBITotBlks,    ">>>>>>>>>>9" ) "DBI Total Blocks    "     string( numPeakTT,     ">>>>>>>>>>9" ) "peak                  " skip
    string( DBIEmptyBlks,  ">>>>>>>>>>9" ) "DBI Empty Blocks    "     string( numTTIdx,      ">>>>>>>>>>9" ) "tt indexes            " skip
    string( DBIFreeBlks,   ">>>>>>>>>>9" ) "DBI Free Blocks     "     string( totRecs,       ">>>>>>>>>>9" ) "total current records " skip
    string( DBIRMFreeBlks, ">>>>>>>>>>9" ) "DBI RM Free Blocks  "     string( totBytes,      ">>>>>>>>>>9" ) "total current bytes   "
    skip(1)
    string( ttHR,          ">>>>>>9.99%" ) "tt hit ratio"
    skip(1)
    string( ttSumCreate,   ">>>>>>>>>>9" ) "tt rec create       " skip
    string( ttSumRead,     ">>>>>>>>>>9" ) "tt rec read         " skip
    string( ttSumUpdate,   ">>>>>>>>>>9" ) "tt rec update       " skip
    string( ttSumDelete,   ">>>>>>>>>>9" ) "tt rec delete       " skip
    string( ttSumLogRd,    ">>>>>>>>>>9" ) "tt rec log rd       " skip
    string( ttSumOSRd,     ">>>>>>>>>>9" ) "tt rec os rd        " skip
    string( ttSumOSWr,     ">>>>>>>>>>9" ) "tt rec os wr        " skip
    string( ttSumTRX,      ">>>>>>>>>>9" ) "tt TRX              " skip
    string( ttSumUndo,     ">>>>>>>>>>9" ) "tt Undos            " skip
    skip(1)
   view-as alert-box title " Temp-Table Info ".

  /* prompt user for a filter: "" results in all records being displayed
   */

  option_blk: do:

    do
      on endkey undo, leave
      on error  undo, leave
      on stop   undo, leave
      on quit   undo, leave:

      update
        skip(1)
        tblFilter   space(4) "leave blank to view all tables  " skip
        outFileName space(4) "leave blank to view on screen   "  skip
        skip(1)
       with
        frame ttInfo_options
        title " TT Info Display Options "
        overlay
        side-labels
        row 3
        column 10
      .

      hide frame ttInfo_options.
      leave option_blk.

    end.

    hide frame ttInfo_options.
    return.

  end.


  /* detailed TT metrics
   */

  if Progress.Database.TempTableInfo:ArchiveTableStatistics <> true then
    do:
      message
        skip(1)
        "  Temp-table CRUD statistics were not collected.  This is probably   " skip
        "  because the -tt* parameters in etc/protop.pf need to be set.       " skip
        skip(1)
       view-as alert-box title " Temp-Table Details Warning "
      .
    end.

  if outFileName = "" then
    output stream outStrm to terminal.
   else
    output stream outStrm to value( outFileName ).

  for each tt_ttInfo where ttName begins tblFilter
     break by ttId by ttProcName by idxId by xid:

    display stream outStrm
    /* xid */ 
    /* ttId */
    /* idxId */
       ttName
       ttProcName
       numBytes
       numRecs
       ts_create
       ts_read
       ts_update
       ts_delete
       ts_osread
       ttDeleted
      with
       frame show_ttDetails
       title " Temp-Table Details "
       overlay
       row 2
       /* column 1 */
       width 132
       centered
       down
       stream-io
     .

    if last-of( ttId ) then down 1 with frame show_ttDetails.

  end.

  output stream outStrm close.
  hide frame show_ttDetails.

&ELSE

  message
    ' temp-table info requires OpenEdge 11 or higher and you must set "TTDEBUG=yes" in bin/localenv ~n~n  You must also uncomment the -tt* parameters in etc/protop.pf'
   view-as alert-box title " Session Temp-Table Info ".

&ENDIF

  pause 0 before-hide.

  return.

end.


/* the end */

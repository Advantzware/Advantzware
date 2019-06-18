/* xrange.p
 *
 */

{lib/protop.i}

define stream rangeStrm.

procedure getRangeData:

  define input-output parameter numTbl    as integer no-undo.
  define input-output parameter numIdx    as integer no-undo.
  define input-output parameter numLOB    as integer no-undo.

  define input-output parameter tblBase   as integer no-undo.
  define input-output parameter idxBase   as integer no-undo. 
  define input-output parameter tblRange  as integer no-undo.
  define input-output parameter idxRange  as integer no-undo.

  define input-output parameter minTblNum as integer no-undo.
  define input-output parameter maxTblNum as integer no-undo.
  define input-output parameter minIdxNum as integer no-undo.
  define input-output parameter maxIdxNum as integer no-undo.
  define input-output parameter unMonTbl  as integer no-undo.
  define input-output parameter unMonIdx  as integer no-undo.
  define input-output parameter excessTbl as integer no-undo.
  define input-output parameter excessIdx as integer no-undo.

  define input-output parameter hst       as integer no-undo.		/* "highest stats table"		*/
  define input-output parameter hsi       as integer no-undo.		/* "highest stats index"		*/
  define input-output parameter lxt       as integer no-undo.		/* "lowest table > -16384"		*/
  define input-output parameter lxi       as integer no-undo.		/* "lowest index"			*/
  define input-output parameter hxi       as integer no-undo.		/* "highest index"			*/
  define input-output parameter hxt       as integer no-undo.		/* "highest table < 32768"		*/
  define input-output parameter lmt       as integer no-undo.		/* "lowest monitored table"		*/
  define input-output parameter lmi       as integer no-undo.		/* "lowest monitored index"		*/
  define input-output parameter hmt       as integer no-undo.		/* "highest monitored table"		*/
  define input-output parameter hmi       as integer no-undo.		/* "highest monitored index"		*/

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 12.0
&THEN
  assign
    tblBase = integer( getStartUpX( "", "(-basetable)", "" ))
    idxBase = integer( getStartUpX( "", "(-baseindex)", "" ))
  .
&ELSE
  find dictdb._StatBase no-lock.	/* _statBase is removed in oe12 	*/
  assign
    tblBase = _StatBase._tableBase
    idxBase = _StatBase._indexBase
  .
&ENDIF

  for each dictdb._TableStat no-lock:
    tblRange = tblRange + 1.
  end.

  for each dictdb._IndexStat no-lock:
    idxRange = idxRange + 1.
  end.

  /* table number ranges  
   * 0 to 32768         application data
   * > 32768            sql views
   * -1 to -79          metaschema, sql89
   * -80 to -125        sql catalog
   * -200 to ?          auditing, authorization & logging
   * -300 to -361       multi-tenancy, horizontal table partitioning
   * < -16385           vsts
   *
   */

  assign
    hst = tblRange + tblBase - 1
    hsi = idxRange + idxBase - 1
  .

  assign
    minTblNum = 99999		/*  what if there are no tables?  does this make sense?			*/
    maxTblNum = 0
    minIdxNum = 99999		/*  what if there are no indexes?  does this make sense?		*/
    maxIdxNum = 0
  .

  numTbl = 0.
  for each tt_tbl where tt_tbl.xid >= 0 and tt_tbl.xid < 32768:
    if tblname begins "_" then next.
    assign
      numTbl = numTbl + 1
      minTblNum = min( minTblNum, tt_tbl.xid )
      maxTblNum = max( maxTblNum, tt_tbl.xid )
    .
    if tt_tbl.xid < tblBase then unMonTbl = unMonTbl + 1.
    if tt_tbl.xid > hst     then unMonTbl = unMonTbl + 1.
  end.

  find first tt_tbl where tt_tbl.xid > -16384 no-error.
  if available tt_tbl then lxt = tt_tbl.xid.

  find last tt_tbl where tt_tbl.xid < 32768 no-error.
  if available tt_tbl then hxt = tt_tbl.xid.

  find first tt_tbl where tt_tbl.xid >= tblBase no-error.
  if available tt_tbl then lmt = tt_tbl.xid.

  find last tt_tbl where tt_tbl.xid <= hst no-error.
  if available tt_tbl then hmt = tt_tbl.xid.

  /* index number ranges
   * 0 to 7 		meta schema indexes
   * 8 to 993		application indexes
   * 994 to 1093	system indexes (with holes)
   * > 1093		application indexes
   * < 0		system indexes
   * < -16385		vst indexes
   */

  numIdx = 0.
  for each tt_idx:
    if idxname begins "_" then next.
    assign
      numIdx = numIdx + 1
      minIdxNum = min( minIdxNum, tt_idx.xid )
      maxIdxNum = max( maxIdxNum, tt_idx.xid )
    .
    if tt_idx.xid < idxBase then unMonIdx = unMonIdx + 1.
    if tt_idx.xid > hsi     then unMonIdx = unMonIdx + 1.
  end.

  find first tt_idx where tt_idx.xid >= idxBase no-error.
  if available tt_idx then lmi = tt_idx.xid.

  find last tt_idx where tt_idx.xid <= hsi no-error.
  if available tt_idx then hmi = tt_idx.xid.

  find first tt_idx where tt_idx.xid > -16385 no-error.
  if available tt_idx then lxi = tt_idx.xid.

  find last tt_idx no-error.
  if available tt_idx then hxi = tt_idx.xid.


  /* LOBs
   */

  numLOB = 0.
  for each dictdb._field no-lock:
    if _field._data-type matches "*LOB*" then
      do:
        find _file no-lock where recid( _file ) = _field._file-recid.
        if _tbl-type = "t" then numLOB = numLOB + 1.
      end.
  end.

  assign
    excessTbl = ((( tblRange + min( 0, tblBase )) - 1 ) - maxTblNum )
    excessIdx = ((( idxRange + min( 0, idxBase )) - 1 ) - maxIdxNum ) + 1
  .

  return.

end.


procedure showRangeData:

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

  define variable tclr as integer no-undo.
  define variable iclr as integer no-undo.

  define variable nastyGram as character no-undo format "x(96)".
  define variable suggestPF as character no-undo format "x(58)".

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

  suggestPF = substitute( "&1/&2.range.pf", pt_tmpdir, pt_shortname ).

  output stream rangeStrm to value( suggestPF ).

  put stream rangeStrm unformatted "# suggested table and index statistics settings for " pdbname(1) skip.
  put stream rangeStrm unformatted "#" skip.
  put stream rangeStrm unformatted skip(1).

  put stream rangeStrm unformatted "# basic (excludes sysatem tables and some system indexes)" skip.
  put stream rangeStrm unformatted skip(1).
  put stream rangeStrm unformatted "# -tablebase "      minTblNum       skip.
  put stream rangeStrm unformatted "# -tablerangesize " maxTblNum + 50  skip.
  put stream rangeStrm unformatted "# -indexbase "      minIdxNum       skip.
  put stream rangeStrm unformatted "# -indexrangesize " maxIdxNum - minIdxNum + 50 skip.
  put stream rangeStrm unformatted skip(1).

  put stream rangeStrm unformatted "# complete (includes system tables)" skip.
  put stream rangeStrm unformatted skip(1).
  put stream rangeStrm unformatted "-tablebase "      lxt            skip.
  put stream rangeStrm unformatted "-tablerangesize " hxt - lxt + 50 skip.
  put stream rangeStrm unformatted "-indexbase "      lxi            skip.
  put stream rangeStrm unformatted "-indexrangesize " hxi - lxi + 50 skip.
  put stream rangeStrm unformatted skip(1).

  output stream rangeStrm close.

  if session:batch = no then
    do:

      if excessTbl < 0 or excessIdx < 0 then 
        nastyGram = "      The -*rangesize parameters are not set adequately to monitor all tables and indexes!!!".

      tclr = ( if excessTbl < 0 then 6 else 5 ).
      iclr = ( if excessIdx < 0 then 6 else 5 ).

      display
        skip(1)
        nastyGram /* dcolor 6 */ skip
        skip(1)
        "                          -basetable:" dcolor tclr tblBase    format "->>,>>9"             "                          -baseindex:" dcolor iclr idxBase    format "->>,>>9" "  " skip
        "                     -tablerangesize:" dcolor tclr tblRange   format "->>,>>9"             "                     -indexrangesize:" dcolor iclr idxRange   format "->>,>>9" skip
        skip(1)
        "                Highest Stats Table#:" hst        format "->>,>>9"             "                Highest Stats Index#:" hsi        format "->>,>>9" skip
        "             Lowest Monitored Table#:" lmt        format "->>,>>9"             "             Lowest Monitored Index#:" lmi        format "->>,>>9" skip
        "            Highest Monitored Table#:" hmt        format "->>,>>9"             "            Highest Monitored Index#:" hmi        format "->>,>>9" skip
        skip(1)
        "                                   Application Tables and Indexes " skip
        skip(1)
        "         Actual Number of App Tables:" numTbl     format "->>,>>9"             "        Actual Number of App Indexes:" numIdx     format "->>,>>9" skip
        "                  Minimum App Table#:" minTblNum  format "->>,>>9"             "                 Minimum App  Index#:" minIdxNum  format "->>,>>9" skip
        "                  Maximum App Table#:" maxTblNum  format "->>,>>9"             "                 Maximum App  Index#:" maxIdxNum  format "->>,>>9" skip
        "              Unmonitored App Tables:" unMonTbl   format "->>,>>9"             "             Unmonitored App Indexes:" unMonIdx   format "->>,>>9" skip
        skip(1)
        "                  Excess Table Range:" excessTbl  format "->>,>>9"             "                  Excess Index Range:" excessIdx  format "->>,>>9" skip
        skip(1)
        "              Minimal App -basetable:" dcolor 7 ( minTblNum + 0 ) format "->>,>>9"      "              Minimal App -baseindex:" dcolor 7 ( minIdxNum + 0 ) format "->>,>>9" skip
        "         Minimal App -tablerangesize:" dcolor 7 ( maxTblNum + 0 ) format "->>,>>9"      "         Minimal App -indexrangesize:" dcolor 7 ( maxIdxNum - minIdxNum + 1 ) format "->>,>>9" skip
        skip(1)
        "                Suggested -basetable:" ( minTblNum + 0 )  format "->>,>>9"     "                Suggested -baseindex:" ( minIdxNum + 0 ) format "->>,>>9" skip
        "           Suggested -tablerangesize:" ( maxTblNum + 50 ) format "->>,>>9"     "           Suggested -indexrangesize:" ( maxIdxNum - minIdxNum + 50 ) format "->>,>>9" skip
        skip(1)
        "                                   System Tables and Indexes " skip
        skip(1)
        "                       Lowest Table#:" lxt        format "->>,>>9"             "                       Lowest Index#:" lxi        format "->>,>>9" skip
        "                      Highest Table#:" hxt        format "->>,>>9"             "                      Highest Index#:" hxi        format "->>,>>9" skip
        skip(1)
        "       Suggested Complete -basetable:" dcolor 9 ( lxt + 0 ) format "->>,>>9"            "       Suggested Complete -baseindex:" dcolor 9 ( lxi + 0 ) format "->>,>>9" skip
        "  Suggested Complete -tablerangesize:" dcolor 9 ( hxt - lxt + 50 ) format "->>,>>9"     "  Suggested Complete -indexrangesize:" dcolor 9 ( hxi - lxi + 50 ) format "->>,>>9" skip
        skip(2)
        '  * "System" tables and indexes include the meta-schema but do not count psuedo tables such ' skip
        '    as VSTs and SQL views as these do not have any CRUD statistics associated with them.    ' skip
        skip(1)
        '  Suggested settings can be found in: ' suggestPF skip
        skip(1)
       with
        frame showXRangeInfo
        title " Table and Index Range Information "
        centered
        no-labels
        row 4
        overlay
        width 100
      .

      color display value( if opsys = "unix" then "RED" else "RED/WHITE" )

        nastyGram

        tblBase   when excessTbl < 0
        tblRange  when excessTbl < 0
        excessTbl when excessTbl < 0

        idxBase   when excessIdx < 0
        idxRange  when excessIdx < 0
        excessIdx when excessIdx < 0

       with
        frame showXRangeInfo
      .

      pause.

      hide frame showXRangeInfo.

    end.

  return.

end.


procedure checkRangeData:

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

  if excessTbl < 0 or excessIdx < 0 then  
    run showRangeData.

  return.

end.


/*** main
 ***/

session:add-super-procedure( this-procedure ).

subscribe to "protop_checkRangeData" anywhere run-procedure "checkRangeData".
subscribe to "protop_showRangeData"  anywhere run-procedure "showRangeData".

return.

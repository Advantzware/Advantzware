/* lib/parsedba.p
 *
 */

{lib/v9.i}

{lib/dba.i}

define new global shared variable dbgMode as integer no-undo initial 1.

define variable r          as integer   no-undo.
define variable x          as character no-undo.
define variable y          as decimal   no-undo.
define variable z          as decimal   no-undo.
define variable xline      as character no-undo extent 512.
define variable curr_area  as character no-undo.
define variable doFixed    as logical   no-undo initial no.
define variable bSize      as integer   no-undo initial 4096.

define variable ztblName   as character no-undo.
define variable zidxName   as character no-undo.

define variable db_analx   as character no-undo.

define variable db_recid   as recid     no-undo.

define stream tba.		/* input table analysis	*/

/*** main block (constructor)
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.


function getDecimal returns decimal ( numString as character ):

  define variable x as decimal no-undo.

  case substring( numString, length( numString ), 1 ):

   /* when "G" then  x = decimal( substring( numString, 1, length( numString ) - 1 )) * 1000000000.
    * when "M" then  x = decimal( substring( numString, 1, length( numString ) - 1 )) * 1000000.
    * when "K" then  x = decimal( substring( numString, 1, length( numString ) - 1 )) * 1000.
    */

    when "G" then  x = decimal( substring( numString, 1, length( numString ) - 1 )) * 1024 * 1024 * 1024.
    when "M" then  x = decimal( substring( numString, 1, length( numString ) - 1 )) * 1024 * 1024.
    when "K" then  x = decimal( substring( numString, 1, length( numString ) - 1 )) * 1024.
    when "B" then  x = decimal( substring( numString, 1, length( numString ) - 1 )).

    otherwise      x = decimal( numString ).

  end.

  return x.

end.


procedure RecBlkSum:

  /* 
   *	RECORD BLOCK SUMMARY FOR AREA "Employee" : 7
   *	-------------------------------------------------------
   *	                                       -Record Size (B)- ---Fragments--- Scatter
   *	Table                Records    Size   Min   Max  Mean      Count Factor  Factor
   *	PUB.Benefits              21  848.0B    39    41    40         21    1.0     1.0
   *	PUB.Department             7  211.0B    26    35    30          7    1.0     2.0
   *	PUB.Employee              55    6.2K    99   135   115         55    1.0     1.0
   *	PUB.Family                72    3.1K    38    51    44         72    1.0     1.0
   *	PUB.TimeSheet             25    1.1K    42    45    43         25    1.0     1.0
   *	PUB.Vacation              12  288.0B    24    24    24         12    1.0     1.0
   *	
   *	                    ------------------------------------------------------------
   *	Subtotals:               192   11.7K    24   135    62        192    1.0     1.2
   */

  area_block:
  repeat on error undo, leave on endkey undo, leave:

    curr_area = xline[6].							/* area name 					*/

    if dbgMode >= 4 then message {&NOW} "Parsing Record Block Summary for Area: [" curr_area "] " view-as alert-box.

    tables:
    repeat:

      xline = ?.
      do  on error undo, leave area_block on endkey undo, leave area_block:
        import stream tba xline.
      end.

      if xline[1] = ? or xline[1] = "" then next tables.

      if xline[1] = "Subtotals:" then
        do:
          return.								/* end of table data for this storage area	*/
        end.
       else if xline[1] = "INDEX" and xline[2] = "BLOCK" and xline[3] = "SUMMARY" then
        do:
          message "index block summary inside record block summary!" "current:" curr_area "new:" xline[6] view-as alert-box.
          run IdxBlkSum.
          return.								/* this shouldn't happen			*/
        end.
       else if xline[1] = "SUMMARY" and xline[2] = "FOR" and xline[3] = "AREA" then
        do:
          return.								/* this shouldn't happen			*/
        end.
       else if xline[1] = "RECORD" and xline[2] = "BLOCK" and xline[3] = "SUMMARY" and xline[4] = "FOR" and xline[5] = "AREA" then
        do:
          next area_block.							/* this shouldn't happen			*/
        end.

      /* process data for a table
       */

      if xline[1] begins "PUB." then
        do:

          find tblist where tblist.tbl = substring( xline[1], 5 ) no-error.
          if not available( tblist ) then
            do:
              create tblist.
              assign
                tblist.tbl = trim( substring( xline[1], 5 ))
              .
            end.

          tblist.ar  = curr_area.

          if xline[2] <> ? and xline[2] <> "" then				/* test for a broken line			*/
            do:
              assign								/* normal line					*/
                tblist.recs = tblist.recs + getDecimal( xline[2] )
                tblist.tsz  = tblist.tsz  + getDecimal( xline[3] )
                tblist.frag = tblist.frag + getDecimal( xline[7] )
                tblist.scat = tblist.scat + getDecimal( xline[9] )
              no-error.
            end.
           else									/* the line was broken -- record count etc	*/
            do:									/* are on the next line				*/
              import stream tba xline.
              assign
                tblist.recs = tblist.recs + getDecimal( xline[1] )
                tblist.tsz  = tblist.tsz  + getDecimal( xline[2] )
                tblist.frag = tblist.frag + getDecimal( xline[6] )
                tblist.scat = tblist.scat + getDecimal( xline[8] )
              no-error.
            end.

          /* need a connected db with access to _file etc for the remaining details
           */

          find first _file no-lock where _file-name = tblist.tbl no-error.
          if not available _file then
            do:
              if dbgMode >= 4 then message {&NOW} "recBlkSum() No such table as:" tblist.tbl view-as alert-box.
              next tables.
            end.

          tblist.tid = _file-num.

          find first _storageObject no-lock where
               _storageObject._Db-recid = db_recid and
               _Object-type = 1 and
               _Object-number = _File-Number
            no-error.

          if not available( _storageObject ) then
            do:
              message {&NOW} "recBlkSum() Ambiguous:" ambiguous(_storageObject ) tblist.tbl _file-number xLine[1] xLine[2] xLine[3] xLine[4] xLine[5] xLine[6] view-as alert-box.
              next tables.
            end.

          tblist.b2  = ( get-bits( _object-attrib, 7, 1 ) = 1 ).

        end.

    end.

  end.

  return.

end.


procedure IdxBlkSum:

  /*
   *	INDEX BLOCK SUMMARY FOR AREA "LogData_Idx" : 21
   *	-------------------------------------------------------
   *	Table                      Index  Fields Levels  Blocks    Size   % Util Factor
   *	PUB.logdata
   *	  db_metric_dt-idx            11       3      4  3648565   15.7G    56.5     1.9
   *	  log-idx                     10       4      4  4041143   17.5G    56.9     1.9
   *	  metric-idx                  12       2      4  1109124    4.5G    53.0     1.9
   */

  curr_area = xline[6].							/* area name */

  if dbgMode >= 4 then message {&NOW} "Parsing Index Block Summary for Area: [" curr_area "] " xline[6] view-as alert-box.

  idx_area_block:
  repeat on error undo, leave on endkey undo, leave:

    indexes:
    repeat:

      xline = ?.
      do  on error undo, leave idx_area_block on endkey undo, leave idx_area_block:
        import stream tba xline.
      end.

      if xline[1] = ? or xline[1] = "" then next indexes.

      if xline[1] = "Subtotals:" then
        do:
          return.
        end.
       else if xline[1] = "RECORD" and xline[2] = "BLOCK" and xline[3] = "SUMMARY" then
        do:
          return.
        end.
       else if xline[1] = "SUMMARY" and xline[2] = "FOR" and xline[3] = "AREA" then
        do:
          return.
        end.
       else if xline[1] = "RECORD" and xline[2] = "BLOCK" and xline[3] = "SUMMARY" and xline[4] = "FOR" and xline[5] = "AREA" then
        do:
          message "record block summary inside index block summary!" "current:" curr_area "new:" xline[6] view-as alert-box.
          run RecBlkSum.
          return.
        end.

      if xline[1] begins "PUB." then					/* we have a table... 				*/
        idx_loop: do while xline[1] begins "PUB.":

          ztblName = substring( xline[1], 5 ).

          /* read some index data... */

          repeat:

            xline = ?.
            do  on error undo, leave idx_area_block on endkey undo, leave idx_area_block:

              import stream tba xline.

              if       xline[1] begins "PUB." then next idx_loop.
               else if xline[1] begins "_"    then next idx_area_block.
               else if xline[1] = ? then return.

              zidxname = ztblName + "." + xline[1].

              find ixlist where ixlist.idx = zidxName no-error.
              if not available ixlist then
                do:
                  create ixlist.
                  assign
                    ixlist.ar    = curr_area
                    ixlist.idx   = zidxName
                    ixlist.blks  = decimal( xline[5] )
                    ixlist.pctut = decimal( xline[7] )
                    ixlist.lvls  = decimal( xline[4] )
                  no-error.
                end.

            end.

          end.

          end.

    end.    /* repeat indexes... */

  end.

  return.

end.


procedure RMChain:

  /* oe10
   *
   * 	RM CHAIN ANALYSIS
   * 	-----------------
   * 
   * 	3 block(s) found in the RM chain of Table object 1
   * 
   * 	20 block(s) found in the RM chain of Table object 3
   *
   *
   * oe11+
   * 
   * 	RM CHAIN ANALYSIS
   * 	---------------------------
   * 
   * 
   * 	Number of          Object    Object
   * 	Blocks             Type
   * 	------------------------------------------------------------------------------------------------------
   * 	60                 Table     PUB.Ar-token-facte:60   
   */

  if dbgMode >= 4 then message {&NOW} "Parsing RM Chain Analysis" view-as alert-box.

  rm_chain_block:
  repeat on error undo, leave on endkey undo, leave:

    rm_chain:
    repeat:

      xline = ?.
      do  on error undo, leave rm_chain_block on endkey undo, leave rm_chain_block:
        import stream tba xline.
      end.

      if xline[1] = ? or xline[1] = "" then next rm_chain.

      if xline[1] begins "---" then
        do:

          if proversion >= "11" then next rm_chain.

          repeat:

            xline = ?.
            import stream tba xline.

            if xline[1] = ? or xline[1] = "" then next.

            if xline[2] = "block(s)" and xline[9] = "Table" then
              do:

                find first _file no-lock where _file-number = integer( xline[11] ) no-error.
                if not available _file then
                  do:
                    if dbgMode >= 4 then message {&NOW} "rmChain() No such table as:" xline[11] view-as alert-box.
                    next.
                  end.

                create tblist.
                assign
                  tblist.tbl = _file._file-name
                  tblist.tid = _file-num
                  tblist.rm  = decimal( xline[1] )
                no-error.

              end.
             else leave.
          end.
          leave rm_chain_block.
        end.

      /* oe11 rm chain
       */

      if xline[1] = "Number" and xline[2] = "of" then
        do:

          if dbgMode >= 4 then message {&NOW} "Parsing oe11 rm chain".

          import stream tba xline.
          if xline[1] = "Blocks" then
            do:
              import stream tba xline.
              if xline[1] begins "----" then
                rm_blocks: repeat:
                  xline = ?.
                  import stream tba xline.
                  if xline[1] = ? then return.
                  if xline[2] = "table" then
                    do:

                      find tblist where tblist.tbl = entry( 1, substring( xline[3], 5 ), ":" ) no-error.
                      if not available( tblist ) then
                        do:
                          create tblist.
                          assign
                            tblist.tbl = entry( 1, trim( substring( xline[3], 5 )), ":" )
                            /* tblist.ar  = curr_area */
                          .
                        end.

                      find first _file no-lock where _file-name = tblist.tbl no-error.
                      if not available _file then
                        do:
                          if dbgMode >= 4 then message {&NOW} "rmChain() No such table as:" tblist.tbl view-as alert-box.
                          next rm_blocks.
                        end.

                      assign
                        tblist.tid = _file-num
                        tblist.rm = decimal( xline[1] )
                      no-error.

                    end.

                end. /* rm_blocks */

            end.

        end.

      return.

    end.

  end.

  return.

end.


procedure parseDBA:

  define input parameter db_anal as character no-undo.

  if dbgMode >= 2 then message {&NOW} "Parsing:" db_anal.

  input stream tba from value( db_anal ).

  find first dictdb._Db no-lock.
  db_recid = recid( _db ).

  empty temp-table tblist.
  empty temp-table ixlist.

  tba:
  repeat on error undo, leave on endkey undo, leave:

    xline = ?.
    do  on error undo, leave tba on endkey undo, leave tba:
      import stream tba xline.
    end.

    if xline[1] = ? or xline[1] = "" then next tba.

    /* ok, we're reading the important stuff...
     */

    if       xline[1] = "RECORD" and xline[2] = "BLOCK" and xline[3] = "SUMMARY" and xline[4] = "FOR" and xline[5] = "AREA" then
      run RecBlkSum.
     else if xline[1] = "INDEX"  and xline[2] = "BLOCK" and xline[3] = "SUMMARY" and xline[4] = "FOR" and xline[5] = "AREA" then
      run IdxBlkSum.
     else if xline[1] = "RM"     and xline[2] = "CHAIN" and xline[3] = "ANALYSIS" then
      run RMChain.

  end.

  input stream tba close.

  if dbgMode >= 2 then message {&NOW} "Done parsing dbanalys data".

  db_analx = right-trim( search( db_anal ), "dba" ) + "dbx".			/* "." is treated like a wild-card?	*/

  output to value( db_analx ).
  for each tblist where tbl <> "":
    export tblist.
  end.
  output close.

  output to value( right-trim( db_analx, "dbx" ) + "idx" ).
  for each ixlist where idx <> "":
    export ixlist.
  end.
  output close.

  return.

end.

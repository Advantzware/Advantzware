/* userTableStats.p
 *
 * track userTableStats for the current session
 *
 */

/* the public temp tables
 */

{lib/userstats.i}

/* internal temp tables
 */

define temp-table tt_usrTblInfoX no-undo
  field xId     as integer
  field tblNum  as integer
  field tblName as character format "x(20)"
  field tblRd   as int64 extent 2			/* [1] = previous value, [2] = incremental value	*/
  field tblCr   as int64 extent 2    
  field tblUp   as int64 extent 2
  field tblDl   as int64 extent 2
  index tblNum-idx  is unique tblNum
  index tblName-idx is unique tblName
.

define temp-table tt_usrIdxInfoX
  field xId     as integer
  field idxNum  as integer
  field idxName as character format "x(40)"
  field idxRd   as int64 extent 2
  field idxCr   as int64 extent 2
  field idxDl   as int64 extent 2
  index idxNum-idx  is unique idxNum
  index idxName-idx is unique idxName
.

define variable myUserNum   as integer no-undo.
define variable myUserid    as integer no-undo.

define variable firstTblId as integer no-undo.
define variable lastTblId  as integer no-undo.

define variable tblBase     as integer no-undo.
define variable tblRange    as integer no-undo.
define variable tblUsed     as integer no-undo.

define variable firstIdxId  as integer no-undo.
define variable lastIdxId   as integer no-undo.

define variable idxBase     as integer no-undo.
define variable idxRange    as integer no-undo.
define variable idxUsed     as integer no-undo.

/* who am i? (this should probably be an input parameter)
 */

find dictdb._myConnect no-lock.

/* VSTs are keyed by "id", userNum is userId + 1 -- or, depending on your POV, userId is userNum - 1 
 *
 * in any event efficient VST access is always by the "id" field so make sure to use it!
 *
 */

myUserNum = _myConn-UserId.
myUserid  = _myConn-UserId + 1.

/* how many tables are there and what are the starting and ending offsets?
 *
 * -tablerangesize and -indexrangesize both default to 50, if you have more
 * than 50 tables or indexes the appropriate *rangesize parameter will need
 * to be increased
 *
 */
 
find first dictdb._tableStat no-lock.
tblBase = recid( _tableStat ).

find last _tableStat no-lock.
tblRange = recid( _tableStat ).

tblUsed = 0.
for each _tablestat no-lock:
  find first dictdb._file no-lock where _file-num = _tablestat-id no-error.
  if available( _file ) then tblUsed = tblUsed + 1.
end.

assign
  firstTblId = myUserNum * tblRange
  lastTblId  = firstTblId + tblRange /* min( tblRange, tblUsed ) */
  firstTblId = firstTblId + 1
.

/* how many indexes are there?  what are the starting and ending offsets?
 */

find first dictdb._indexStat no-lock.
idxBase = recid( _indexStat ).

find last _indexStat no-lock.
idxRange = recid( _indexStat ).

idxUsed = 0.
for each _indexStat no-lock:
  find first dictdb._index no-lock where _idx-num = _indexStat-id no-error.
  if available( _index ) then idxUsed = idxUsed + 1.
end.

assign
  firstIdxId = myUserNum * idxRange
  lastIdxId  = firstIdxId + idxRange /* min( idxRange, idxUsed ) */
  firstIdxId = firstIdxId + 1
.

/* initialize user stats
 */

run getUStats ( output table tt_usrTblInfo by-reference, output table tt_usrIdxInfo by-reference ).

session:add-super-procedure( this-procedure ).

return.


/************************************************************/


/* get user table & index stats
 *
 * the first time this is run it will initialize the baseline (that is why it is run at startup).
 *
 */

procedure getUStats:

  define output parameter table for tt_usrTblInfo.
  define output parameter table for tt_usrIdxInfo.

  define variable t as integer no-undo.
  define variable i as integer no-undo.

  t  = 0.

  for each dictdb._userTableStat no-lock where _UserTableStat-Id >= firstTblId and _UserTableStat-Id <= lastTblId:

    t = t + 1.
    /* if t > tblUsed then leave. */

    find tt_usrTblInfoX where tt_usrTblInfoX.xId = _userTableStat-id no-error.

    if available tt_usrTblInfoX then
      assign

        tt_usrTblInfoX.tblRd[2] = _userTableStat-read   - tt_usrTblInfoX.tblRd[1]
        tt_usrTblInfoX.tblRd[1] = _userTableStat-read

        tt_usrTblInfoX.tblCr[2] = _userTableStat-create - tt_usrTblInfoX.tblCr[1]
        tt_usrTblInfoX.tblCr[1] = _userTableStat-create

        tt_usrTblInfoX.tblUp[2] = _userTableStat-update - tt_usrTblInfoX.tblUp[1]
        tt_usrTblInfoX.tblUp[1] = _userTableStat-update

        tt_usrTblInfoX.tblDl[2] = _userTableStat-delete - tt_usrTblInfoX.tblDl[1]
        tt_usrTblInfoX.tblDl[1] = _userTableStat-delete
      .
     else
      do:
        find first _file no-lock where _file-num = _userTableStat-num no-error.
        if not available _file then
          do:
            /* message "wtf? tbl...". */
            /* pause. */
            next.
          end.
        create tt_usrTblInfoX.
        assign
          tt_usrTblInfoX.xId      = _userTableStat-id
          tt_usrTblInfoX.tblNum   = _userTableStat-num
          tt_usrTblInfoX.tblName  = ( if available _file then _file-name else string( _userTableStat-num ))
          tt_usrTblInfoX.tblRd[1] = _userTableStat-read
          tt_usrTblInfoX.tblCr[1] = _userTableStat-create
          tt_usrTblInfoX.tblUp[1] = _userTableStat-update
          tt_usrTblInfoX.tblDl[1] = _userTableStat-delete
        .
      end.

  end.

  i = 0.

  for each dictdb._userIndexStat no-lock where _UserIndexStat-id >= firstIdxId and _userIndexStat-id <= lastIdxId:

    i = i + 1.
    if i > idxUsed then leave.

    find tt_usrIdxInfoX where tt_usrIdxInfoX.xId = _UserIndexStat-id no-error.
    if available tt_usrIdxInfoX then
      assign
        tt_usrIdxInfoX.idxRd[2]  = _userIndexStat-read   - tt_usrIdxInfoX.idxRd[1]
        tt_usrIdxInfoX.idxRd[1]  = _userIndexStat-read

        tt_usrIdxInfoX.idxCr[2]  = _userIndexStat-create - tt_usrIdxInfoX.idxCr[1]
        tt_usrIdxInfoX.idxCr[1]  = _userIndexStat-create

        tt_usrIdxInfoX.idxDl[2]  = _userIndexStat-delete - tt_usrIdxInfoX.idxDl[1]
        tt_usrIdxInfoX.idxDl[1]  = _userIndexStat-delete
      .
     else
      do:
        find first _index no-lock where _idx-num = _UserIndexStat-num no-error.
        if available _index then find first _file no-lock of _index no-error.
        if not available _index /* or not available _file */ then
          do:
            /* message "wtf? idx...". */
            /* pause. */
            next.
          end.
        create tt_usrIdxInfoX.
        assign
          tt_usrIdxInfoX.xId      = _userIndexStat-id
          tt_usrIdxInfoX.idxNum   = _UserIndexStat-num
          tt_usrIdxInfoX.idxName  = substitute( "&1.&2", ( if available _file then _file-name else "xxx" ), _index-name )
          tt_usrIdxInfoX.idxRd[1] = _userIndexStat-read
          tt_usrIdxInfoX.idxCr[1] = _userIndexStat-create
          tt_usrIdxInfoX.idxDl[1] = _userIndexStat-delete
        .
      end.

  end.

  /* translate the internal representation into something more useful
   */

  empty temp-table tt_usrTblInfo.
  empty temp-table tt_usrIdxInfo.

  /* copy the incremental values to a user friendly TT
   */

  for each tt_usrTblInfoX where tt_usrTblInfoX.tblRd[2] > 0 or tt_usrTblInfoX.tblCr[2] > 0 or tt_usrTblInfoX.tblUp[2] > 0 or tt_usrTblInfoX.tblDl[2] > 0 by tt_usrTblInfoX.tblRd[2] descending:
    find tt_usrTblInfo where tt_usrTblInfo.tblName = tt_usrTblInfoX.tblName no-error.
    if not available tt_usrTblInfo then create tt_usrTblInfo.
    assign
      tt_usrTblInfo.tblName = tt_usrTblInfoX.tblName
      tt_usrTblInfo.tblRd   = tt_usrTblInfoX.tblRd[2]
      tt_usrTblInfo.tblCr   = tt_usrTblInfoX.tblCr[2]
      tt_usrTblInfo.tblUp   = tt_usrTblInfoX.tblUp[2]
      tt_usrTblInfo.tblDl   = tt_usrTblInfoX.tblDl[2]
    .
  end.

  for each tt_usrIdxInfoX where tt_usrIdxInfoX.idxRd[2] > 0 or tt_usrIdxInfoX.idxCr[2] > 0 or tt_usrIdxInfoX.idxDl[2] > 0 by tt_usrIdxInfoX.idxRd[2] descending:
    find tt_usrIdxInfo where tt_usrIdxInfo.idxName = tt_usrIdxInfoX.idxName no-error.
    if not available tt_usrIdxInfo then create tt_usrIdxInfo.
    assign
      tt_usrIdxInfo.idxName = tt_usrIdxInfoX.idxName
      tt_usrIdxInfo.idxRd   = tt_usrIdxInfoX.idxRd[2]
      tt_usrIdxInfo.idxCr   = tt_usrIdxInfoX.idxCr[2]
      tt_usrIdxInfo.idxDl   = tt_usrIdxInfoX.idxDl[2]
    .
  end.

  return.

end.

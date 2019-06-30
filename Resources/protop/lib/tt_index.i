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
 * tt_index.i
 *
 */

define temp-table tt_index no-undo
  field xid        as integer
  field xvalid     as logical
  field idxnum     as integer
  field idxname    as character
  field tblnum     as integer
  field tblname    as character
  field areaNum    as integer
  field idxPool    as character
  field idxRoot    as {&BIGINT}
  field idxnote    as character
  field idx-blkdel as decimal extent 5 format ">>>>>>>>>>>>>>>>9"
  field idx-cre    as decimal extent 5 format ">>>>>>>>>>>>>>>>9"
  field idx-del    as decimal extent 5 format ">>>>>>>>>>>>>>>>9"
  field idx-rd     as decimal extent 5 format ">>>>>>>>>>>>>>>>9"
  field idx-split  as decimal extent 5 format ">>>>>>>>>>>>>>>>9"
  index xid-idx is unique primary xid.

procedure upd-tt_index:

  define input parameter p_idx as integer no-undo.

  find dictdb._IndexStat no-lock where _IndexStat-id = p_idx no-error.

  find tt_idx no-lock where tt_idx.xid = _IndexStat-id no-error.		/* tt_idx is a cache of meta-schema info	*/
  if not available tt_idx then return.						/* built at startup in vstlib.p			*/

  find tt_tbl no-lock where tt_tbl.xid = tt_idx.tblnum no-error.

  find tt_index exclusive-lock where tt_index.xid = p_idx no-error.

  if not available tt_index then
    do:
      create tt_index.
      assign
        tt_index.xid      = p_idx
        tt_index.xvalid   = yes
        tt_index.idxnum   = tt_idx.xid
        tt_index.idxname  = tt_idx.idxname
        tt_index.tblnum   = tt_idx.tblnum
        tt_index.tblname  = tt_idx.tblname
        tt_index.idxnote  = tt_idx.idxnote
        tt_index.idxroot  = tt_idx.idxroot
        tt_index.areanum  = tt_idx.areanum
        tt_index.idxPool  = tt_idx.idxPool
      .
      if available _IndexStat then		/* _IndexStat range may not be active...	*/
        assign
          {lib/init-xrec.i tt_index.idx-blkdel _IndexStat-blockdelete}
          {lib/init-xrec.i tt_index.idx-cre    _IndexStat-create}
          {lib/init-xrec.i tt_index.idx-del    _IndexStat-delete}
          {lib/init-xrec.i tt_index.idx-rd     _IndexStat-read}
          {lib/init-xrec.i tt_index.idx-split  _IndexStat-split}
        .
    end.
   else
    tt_index.xvalid = yes.

  if available _IndexStat then		/* _IndexStat range may not be active...	*/
    assign
      {lib/upd-xrec.i tt_index.idx-blkdel _IndexStat-blockdelete}
      {lib/upd-xrec.i tt_index.idx-cre    _IndexStat-create}
      {lib/upd-xrec.i tt_index.idx-del    _IndexStat-delete}
      {lib/upd-xrec.i tt_index.idx-rd     _IndexStat-read}
      {lib/upd-xrec.i tt_index.idx-split  _IndexStat-split}
    .

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_index:

/***
 ***
  define variable x as integer no-undo.
  define variable z as integer no-undo.

  do-SumSample( output x, output z ).
 ***
 ***/

  for each tt_index exclusive-lock:

    if tt_index.xvalid = no then
      delete tt_index.
     else
      assign
        tt_index.xvalid = no
/***
 ***
        {lib/upd-xrec.i tt_index.stat1 tt_index.stat1[3]}
        {lib/upd-xrec.i tt_index.stat2 tt_index.stat2[3]}
        {lib/upd-xrec.i tt_index.stat3 tt_index.stat3[3]}
        tt_xstat.stat-ratio  = 100 * (( tt_xstat.stat1[x] - tt_xstat.stat2[x] ) / tt_xstat.stat1[x] )
        tt_xstat.stat-ratio  = ( if tt_xstat.stat-ratio = ? then 0 else tt_xstat.stat-ratio )
 ***
 ***/
      .

  end.

  return.

end.

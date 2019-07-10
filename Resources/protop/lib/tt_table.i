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
 * tt_table.i
 *
 */

define temp-table tt_table no-undo
  field xid      as integer
  field xvalid   as logical
  field tblnum   as integer
  field areaNum  as integer
  field tblPool  as character
  field tblname  as character label "Table Name" format "x(30)"
  field ttbl     as character
  field tbl-cre  as decimal extent 5 label "Create"  format ">>>>>>>>>>>>>>>>9"
  field tbl-rd   as decimal extent 5 label "Read"    format ">>>>>>>>>>>>>>>>9"
  field tbl-upd  as decimal extent 5 label "Update"  format ">>>>>>>>>>>>>>>>9"
  field tbl-del  as decimal extent 5 label "Delete"  format ">>>>>>>>>>>>>>>>9"
  field tbl-osrd as decimal extent 5 label "OS Read" format ">>>>>>>>>>>>>>>>9"
  field pct-cre  as decimal   label "%" format ">>9.99"
  field pct-rd   as decimal   label "%" format ">>9.99"
  field pct-upd  as decimal   label "%" format ">>9.99"
  field pct-del  as decimal   label "%" format ">>9.99"
  index xid-idx is unique primary xid.

procedure upd-tt_table:

  define input parameter p_tbl as integer no-undo.

  define variable x as integer no-undo.
  define variable z as integer no-undo.

  define variable bh  as handle no-undo.
  define variable bf  as handle no-undo.

  do-SumSample( output x, output z ).

  find dictdb._TableStat no-lock where _TableStat-id = p_tbl no-error.

  find tt_tbl no-lock where tt_tbl.xid = _TableStat-id no-error.
  if not available tt_tbl then return.

  find tt_table exclusive-lock where tt_table.xid = p_tbl no-error.

  if available tt_table then
    tt_table.xvalid = yes.
   else
    do:
      create tt_table.
      assign
        tt_table.xid      = p_tbl
        tt_table.xvalid   = yes
        tt_table.TblName  = ( if available( _TableStat ) then "" else "*" ) + tt_tbl.tblname
        tt_table.TblNum   = tt_tbl.xid
        tt_table.areanum  = tt_tbl.areanum
        tt_table.tblPool  = tt_tbl.tblPool
      .
      if available _TableStat then		/* _TableStat range may not be active...	*/
        do:
          assign
            {lib/init-xrec.i tt_table.tbl-cre _TableStat-create}
            {lib/init-xrec.i tt_table.tbl-rd  _TableStat-read}
            {lib/init-xrec.i tt_table.tbl-upd _TableStat-update}
            {lib/init-xrec.i tt_table.tbl-del _TableStat-delete}
          .
          if hasOSRd = no then
            tt_table.tbl-osrd = ?.
           else
            assign
              bh = buffer _TableStat:handle
              bf = bh:buffer-field( "_TableStat-OSRead" )
              {lib/init-xrec.i tt_table.tbl-osrd bf:buffer-value}
            .
        end.
    end.

  if available _TableStat then		/* _TableStat range may not be active...	*/
    do:

      assign
        {lib/upd-xrec.i tt_table.tbl-cre _TableStat-create}
        {lib/upd-xrec.i tt_table.tbl-rd  _TableStat-read}
        {lib/upd-xrec.i tt_table.tbl-upd _TableStat-update}
        {lib/upd-xrec.i tt_table.tbl-del _TableStat-delete}
      .
      if hasOSRd then
        assign
          bh = buffer _TableStat:handle
          bf = bh:buffer-field( "_TableStat-OSRead" )
          {lib/upd-xrec.i tt_table.tbl-osrd bf:buffer-value}
        .

      /* This uses the *base* statistics rather than the sampled data -- so it reveals
       * the historical pattern rather than current usage.
       */

      if ( tt_table.tbl-cre[x] > 10 and tt_table.tbl-del[x] > 10 ) and
         ( abs( tt_table.tbl-cre[x] - tt_table.tbl-del[x] ) < ( tt_table.tbl-cre[x] * 0.2 )) then
        tt_table.ttbl = "***".
       else
        tt_table.ttbl = "".

    end.

  return.

end.

/* this will be called by the including procedure's mon-update procedure
 *
 */

procedure age_table:

  for each tt_table exclusive-lock:

    if tt_table.xvalid = no then
      delete tt_table.
     else
      assign
        tt_table.xvalid = no
      .

  end.

  return.

end.

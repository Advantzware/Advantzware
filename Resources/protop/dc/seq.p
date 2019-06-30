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
 * seq.p
 *
 *
 * Sequence activity
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

define output parameter dcDescription as character no-undo initial "SequenceActivity".

{lib/tt_xstat.i}

define temp-table tt_seqAct no-undo
  field xid       as integer   format "->>>>9"               label "Id"
  field seqName   as character format "x(32)"                label "Name"
  field seqCycle  as logical   format "Yes/No"               label "Cycle"
  field seqInit   as {&BIGINT} format "->>>>>>>>9"           label "Initial"
  field seqIncr   as {&BIGINT} format "->>>>>>>>9"           label "Increment"
  field seqMin    as {&BIGINT} format "->>>>>>>>>>>>>>9"     label "Minimum"
  field seqMax    as {&BIGINT} format "->>>>>>>>>>>>>>>>>>9" label "Maximum"
  field seqCurr   as {&BIGINT} format "->>>>>>>>>>>>>>>>>>9" label "Current"
  field seqPctMax as decimal   format "->>>9.99%"            label "PctMax"
  field seqPct32  as decimal   format "->>>>>>9.99%"         label "PctMax32"
  field seqAct    as decimal   format ">>>>>>>>9.99"         label "Activity"

  index xid-idx       is unique xid
  index seqName-idx   seqName
  index seqAct-idx    seqAct descending
.

{lib/dumpTT.i tt_seqAct}

define variable maxInt   as {&BIGINT} no-undo.
define variable maxInt32 as integer   no-undo.

maxInt32 = exp( 2, 31 ) - 1.

if "{&BIGINT}" <> "int64" then
  maxInt = maxInt32.
 else
  maxInt = ( exp( 2, 63 ) - 1 ).

procedure mon-init:

  empty temp-table tt_xstat.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  run updTick.

  find first dictdb._Db no-lock.

  for each dictdb._Sequence no-lock where _Sequence._Db-Recid = recid( dictdb._Db ):

    find tt_seqAct where tt_seqAct.seqName = _sequence._seq-name no-error.
    if not available( tt_seqAct ) then
      do:
        create tt_seqAct.
        create tt_xstat.
        assign
          tt_seqAct.seqName   = _seq-name
          tt_seqAct.xId       = _seq-num
          tt_seqAct.seqCycle  = _cycle-ok
          tt_seqAct.seqInit   = _seq-init
          tt_seqAct.seqIncr   = _seq-incr
          tt_seqAct.seqMin    = _seq-min
          tt_seqAct.seqMax    = ( if _seq-max = ? then maxInt else _seq-max )
          tt_xstat.xid        = _seq-num
        no-error.
      end.

&IF DEFINED( OE10 ) &THEN
    tt_seqAct.seqCurr  = dynamic-current-value( _seq-name, ldbname(1)).
&ENDIF

    tt_seqAct.seqPctMax = ( 1 - (( tt_seqAct.seqMax - tt_seqAct.seqCurr ) / tt_seqAct.seqMax )) * 100.
    tt_seqAct.seqPct32  = ( 1 - (( maxInt32 - tt_seqAct.seqCurr ) / maxInt32 )) * 100.

    run update_xstat (
      input _Seq-num,
      input _Seq-name,
      input "m1",
      input "m2",
      input "m3",
      input "m4",
      input "m5",
      input tt_seqAct.seqCurr,
      input 0,
      input 0,
      input 0,
      input 0,
      input 0
    ).

  end.

  run age_xstat.

  for each tt_seqAct no-lock:
    find tt_xstat where tt_xstat.xid = tt_seqAct.xid no-error.    
    tt_seqAct.seqAct  = tt_xstat.stat1[x] / z.
  end.

  add2ds( temp-table tt_seqAct:default-buffer-handle ).

  return.

end.

return.

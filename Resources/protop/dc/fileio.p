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
 * fileio.p
 *
 *
 * IO activity for each db extent
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

define output parameter dcDescription as character no-undo initial "FileIOActivity".

{lib/tt_xstat.i}

define temp-table tt_fileio no-undo
  field xid        as integer   format ">>9"             label "Id"
  field extName    as character format "x(30)"           label "Extent Name"
  field extArea    as character format "x(30)"           label "Storage Area"
  field extMode    as character format "x(8)"            label "Mode"
  field blkSize    as integer   format ">>>>9"           label "BlkSz"
  field extSize    as decimal   format ">>>>>>>>>>9.99"  label "Size (GB)"
  field extRead    as decimal   format "->>>>>>>>>9"     label "Reads"
  field extWrite   as decimal   format "->>>>>>>>>9"     label "Writes"
  field extExtend  as decimal   format "->>>>>>>>>9"     label "Extends"

  index xid-idx       is unique xid
  index extName-idx   is unique extName
  index extMode-idx   is unique extMode xid
  index extSize-idx   is unique extSize   descending xid
  index extRead-idx   is unique primary extRead   descending xid
  index extWrite-idx  is unique extWrite  descending xid
  index extExtend-idx is unique extExtend descending xid
.

{lib/dumpTT.i tt_fileio}

procedure mon-init:

  empty temp-table tt_xstat.

  define variable fName as character no-undo.

  for each dictdb._FileList no-lock:

    find dictdb._ActIOFile no-lock where _IOFile-filename = _FileList-Name no-error.
    find tt_AreaExtent no-lock where tt_areaExtent.extPath = _FileList-Name no-error.

    assign
      fname = _FileList-Name
/*    fname = substring( fname, r-index( fname,  "/" ) + 1 )
      fname = substring( fname, r-index( fname, "~\" ) + 1 )	/* Windows fix thanks to Patrick Tingen	*/
 */
    .

    if not available( tt_AreaExtent ) then find tt_AreaExtent no-lock where tt_areaExtent.extPath matches ( "*" + fName ) no-error.

    create tt_fileio.
    assign
      tt_fileio.xid       = _FileList-Id
      tt_fileio.extName   = fname
      tt_fileio.extMode   = ( if available tt_AreaExtent then ( if tt_areaExtent.extType >= 4 and tt_areaExtent.extType <= 7 then "V" else "F" ) else " " ) + " " + replace( _FileList-OpenMode, "IO", "" )
      tt_fileio.blkSize   = _FileList-BlkSize
      tt_fileio.extSize   = ( tt_areaExtent.extSize / ( 1024 *  1024 )) when available( tt_AreaExtent )
    .

  end.

  run updTick.

  return.

end.


procedure mon-update:

  define input parameter argList as character no-undo.

  define variable fName as character no-undo.
  define variable eName as character no-undo.
  define variable aNumx as character no-undo.
  define variable aNum  as integer   no-undo.

  define variable blks-alloc as decimal no-undo format ">>>>>>>>>>>>9".
  define variable vSize      as decimal no-undo format ">>>>>>>>>>>>9".

  define variable c as character no-undo.
  define variable i as integer   no-undo.
  define variable b as logical   no-undo.
  define variable n as decimal   no-undo.

  run updTick.

  for each tt_fileio no-lock:

    find dictdb._ActIOFile no-lock where _IOFile-Id = tt_fileio.xid no-error.

    run update_xstat (
      input tt_fileio.xid,
      input tt_fileio.extname,
      input "",
      input string( tt_fileio.blkSize, ">>>>9" ),
      input "m3",
      input "m4",
      input "m5",
      input ( if available _ActIOFile then _IOFile-reads else ? ),
      input ( if available _ActIOFile then _IOFile-writes else ? ),
      input ( if available _ActIOFile then _IOFile-extends else ? ),
      input tt_fileio.extSize,
      input 0,
      input 0
    ).

  end.

  run age_xstat.

  run getAreaVarInfo.

  for each tt_xstat no-lock by tt_xstat.stat1[x] descending:

    find tt_fileio where tt_fileio.xid = tt_xstat.xid no-error.
    assign
      tt_fileio.extRead   = tt_xstat.stat1[x] / z
      tt_fileio.extWrite  = tt_xstat.stat2[x] / z
      tt_fileio.extExtend = tt_xstat.stat3[x] / z
      tt_fileio.extSize   = tt_xstat.stat4[1]
    .

    aNumx = "".
    aNum  = -999.
    fName = tt_xstat.xname.

    if fname = pdbname(1) + ".db" then					/* Control Area			*/
      aNum = 1.
     else if fname begins pdbname(1) + ".a" then			/* After Image Areas		*/
      .
     else if fname begins pdbname(1) + ".b" then			/* Primary Recovery Area	*/
      aNum = 3.
     else if fname begins pdbname(1) + ".d" then			/* Schema Area			*/
      aNum = 6.
     else if index( fname, "_" ) > 0 then
      do:
        aNumx = substring( fname, r-index( fname, "_" ) + 1 ).
        aNum = integer( entry( 1, aNumx, "." )) no-error.		/* possible _ in path names	*/
      end.

    find tt_area where tt_area.SANum = anum no-error.
    if available tt_area then
      do:
        tt_fileio.extArea = tt_area.SAName.
        if tt_fileio.extMode begins "V" then tt_fileio.extSize = tt_area.varGB.
      end.

  end.

  add2ds( temp-table tt_fileio:default-buffer-handle ).

  return.

end.

return.

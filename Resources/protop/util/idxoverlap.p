/* idxoverlap.p
 *
 *******************************************************************************
 **                                                                           **
 **                                                                           **
 **  Copyright 2003-2010 Tom Bascom, Greenfield Technologies                  **
 **  http://www.greenfieldtech.com                                            **
 **                                                                           **
 **  The utilities in this directory are NOT part of the publicly             **
 **  distributed ProTop release.                                              **
 **                                                                           **
 **  These utilities are provided as part of an active White Star Software,   **
 **  LLC consulting engagement or in support of a DBAppraise, LLC monitoring  **
 **  and alerting subscription.                                               **
 **                                                                           **
 **  Use or distribution of these utilities outside of that context is        **
 **  prohibited.                                                              **
 **                                                                           **
 **                                                                           **
 *******************************************************************************
 *******************************************************************************
 *
 *
 * scan the schema looking for potentially redundant indexes
 *
 * [m]bpro dbname -p idxoverlap.p > idxoverlap.err &
 *
 */

define new global shared variable pt_shortname as character no-undo.
define new global shared variable pt_rptdir    as character no-undo.

define variable n  as integer no-undo initial 4.		/* the number of columns for the report			*/
define variable c  as integer no-undo.				/* current "virtual" column				*/
define variable x  as integer no-undo.				/* last row ended with column x				*/

define variable i  as integer no-undo.				/* counter -- number of indexes for a table		*/
define variable j  as integer no-undo.				/* counter -- number of fields for an index		*/
define variable k  as integer no-undo.				/* miscellaneous counter				*/

define variable ix as integer no-undo.				/* number of indexes for a table			*/
define variable jx as integer no-undo.				/* maximum depth of indexes for a table			*/

define variable nx as decimal no-undo.				/* number of redundant indexes				*/
define variable bx as decimal no-undo.				/* total number of blocks in redundant indexes		*/

define variable tblFilter as character no-undo.
define variable rptName   as character no-undo.
define variable redName   as character no-undo.

define stream inStrm.
define stream report.
define stream redundant.

define temp-table ttIdxHdr
  field idxId      as integer
  field idxOrder   as integer
  field idxOverlap as integer
  field idxDepth   as integer   format ">9"
  field idxName    as character
  field idxDesc    as character format "x(27)"
  field idxBlks    as integer   initial ?
.

define temp-table ttIdxDet
  field idxId     as integer
  field idxOrder  as integer
  field idxFamily as integer   format ">9"
  field idxKeyNum as integer
  field idxField  as character format "x(27)"
.

define buffer z_ttIdxDet for ttIdxDet.

if pt_shortname = "" or pt_shortname = ? then pt_shortname = ldbname( 1 ).
if pt_rptdir    = "" or pt_rptdir    = ? then
  do:
    file-info:file-name = "./rpt".
    if file-info:full-pathname <> ? and file-info:file-type = "drw" then 
      pt_rptdir    = "./rpt".
     else
      pt_rptdir    = ".".
  end.

/* message file-info:full-pathname file-info:file-type pt_rptdir. */

if tblFilter = "" then tblFilter = "*".

rptName = substitute( "&1/&2.idxoverlap.rpt", pt_rptdir, pt_shortname ).
redName = substitute( "&1/&2.redundant.rpt",  pt_rptdir, pt_shortname ).

output stream report    to value( rptName ).
output stream redundant to value( redName ).

file-info:file-name = substitute( "./dbanalys/&1.idx", pt_shortname ).

for each _file no-lock where _hidden = no and _file._file-name matches tblFilter:

  empty temp-table ttIdxHdr.					/* we only need this info for a short time		*/
  empty temp-table ttIdxDet.

  assign
    i  = 0							/* reset the number of indexes in this table...		*/
    jx = 0							/* ... and the maximum index depth			*/
  .

  /* create sensible temp-tables describing indexes and the keys comprising them
   */

  for each _index no-lock of _file:

    i = i + 1.

    create ttIdxHdr.
    assign
      ttIdxHdr.idxId    = i					/* assign a unique id to the index			*/
      ttIdxHdr.idxOrder = 1					/* every index starts out at the same "order" (one)	*/
      ttIdxHdr.idxName  = _file-name + "." + _index-name	/* index name						*/
      ttIdxHdr.idxDesc  =					/* append some helpful characteristics to the name	*/
        _index._index-name + " " +
        ( if recid( _index ) = _file._prime-index then "p" else "" ) +		/* "primary" index?			*/
        ( if _index._unique  then "u" else "" ) +				/* unique index?			*/
        ( if _index._wordidx <> ? then "w" else "" )				/* word index?				*/
	/*** + string( _index._wordidx ) ***/
    .

    j  = 0.

    for each _index-field no-lock of _index:

      j = j + 1.

      find _field no-lock where recid( _field ) = _index-field._field-recid.

      create ttIdxDet.
      assign
        ttIdxDet.idxId     = i					/* associate the index detail with its header		*/
        ttIdxDet.idxOrder  = 1					/* every index starts out at the same "order" (one)	*/
        ttIdxDet.idxKeyNum = j					/* assign the depth of this component			*/
        ttIdxDet.idxField  =
          _field._field-name +
          ( if _index-field._ascending then "" else " <" )	/* indicate a *descending* component with " <"		*/
        ttIdxHdr.idxDepth  = j					/* record how many components are in this index		*/
        jx = max( j, jx )					/* keep track of the deepest index in this table	*/
      .

    end.

  end.

  ix = i.							/* how many indexes does this table have?		*/

  /* create placeholders for indexes that are "short"
   */

  for each ttIdxHdr:						/* width of the table's indexes...			*/
    do j = 1 to jx:						/* depth of the table's indexes...			*/
      find ttIdxDet where					/* probe for a record at this level...			*/
           ttIdxDet.idxId = ttIdxHdr.idxId and
           ttIdxDet.idxKeyNum = j no-error.
      if not available ttIdxDet then				/* ... if there isn't one then ...			*/
        do:
          create ttIdxDet.					/* ... create a placeholder				*/
          assign
            ttIdxDet.idxId     = ttIdxHdr.idxId
            ttIdxDet.idxOrder  = 1
            ttIdxDet.idxKeyNum = j
            ttIdxDet.idxField  = ""				/* placeholder field name				*/
          .
        end.
    end.
  end.

  /* sort the indexes so that overlap is apparent
   *
   *  - at each level sort by "order" and then by field name
   *  - because "order" starts as 1 (one) for all elements the first pass is a name sort
   *  - indexes are grouped based on the field names at the level being examined
   *  - a new "order" is established whenever the fieldname or the (pre-existing) order changes
   *  - the index *header* order is whatever the latest (deepest) order is for that index
   *  - the fields "family" is the order when that field was examined
   *  - at the end of the level the order of the header is applied to all fields belonging to that index
   *  - then the next level is examined until we are out of levels
   *
   */

  do j = 1 to jx:

    /* determine where the break points are for groups of indexes
     */

    k = 0.
    for each ttIdxDet where ttIdxDet.idxKeyNum = j break by ttIdxDet.IdxOrder by ttIdxDet.idxField:
      if first-of( ttIdxDet.idxOrder ) or first-of( ttIdxDet.idxField ) then k = k + 1.
   /* display j k ttIdxDet.idxField ttIdxDet.idxOrder. */
      find ttIdxHdr where ttIdxHdr.idxId = ttIdxDet.idxId.
      assign
        ttIdxDet.idxFamily = k
        ttIdxHdr.idxOrder  = k
      .
    end.

    /* propogate changes in an indexes "order" to all of its elements
     */

    for each ttIdxHdr by ttIdxHdr.IdxOrder:
      for each ttIdxDet where ttIdxDet.idxId = ttIdxHdr.idxId:
        ttIdxDet.idxOrder = ttIdxHdr.idxOrder.
      end.
    end.

  end.

  /* determine the number of fields that overlap with at least one other index
   */

  for each ttIdxHdr by ttIdxHdr.IdxOrder:
    ttIdxHdr.idxOverlap = 0.						/* assume 0 overlap				*/
    for each ttIdxDet where ttIdxDet.idxId = ttIdxHdr.idxId:
      find z_ttIdxDet where						/* this will fail if there is more than 1...	*/
             z_ttIdxDet.idxKeyNum = ttIdxDet.idxKeyNum and
             z_ttIdxDet.idxFamily = ttIdxDet.idxFamily no-error.
          /* z_ttIdxDet.idxField  = ttIdxDet.idxField  */		/* the field check isn't really necessary	*/
      if ambiguous( z_ttIdxDet ) then					/* if it fails there is at least 1!		*/
        do:
          ttIdxHdr.idxOverlap =  ttIdxHdr.idxOverlap + 1.
       /* display ttIdxHdr.idxDesc ttIdxDet.idxField ttIdxHdr.idxOverlap. */
        end.
    end.
    ttIdxHdr.idxDesc = ttIdxHdr.idxDesc + " " + string( ttIdxHdr.idxOverlap ).		/* visual cue for report	*/
  end.

  /* generate the report table by table as they are evaluated
   */

  assign
    x = -1								/* column where we wrapped			*/
    c =  0								/* current (virtual) column			*/
  .

  put stream report unformatted _file._file-name + " " + fill( "=", 132 - length( _file._file-name )) skip.
  put stream report skip(1).

  do while c < ix:

    /* output index names
     */

    c = 0.
    for each ttIdxHdr no-lock by ttIdxHdr.idxOrder by ttIdxHdr.idxId:
      c = c + 1.
      if c > x then
        put stream report ttIdxHdr.idxDesc space ttIdxHdr.idxDepth space(2).
      if c > x and c modulo n = 0 then leave.
    end.
    put stream report skip.

    /* output a header-line underneath the index name
     */

    c = 0.
    for each ttIdxHdr no-lock by ttIdxHdr.idxOrder:
      c = c + 1.
      if c > x then
        do:
          if ttIdxHdr.idxDepth = ttIdxHdr.idxOverlap then
            do:

              put stream report unformatted fill( "*", 30 ) space(2).	/* the index completely overlaps another	*/

              /* attempt to get the size of the redundant index
               */

              if file-info:full-pathname <> ? then
                do:

                  input stream inStrm through value( substitute( "grep -i '~"&1~"' &2 | awk '~{print $5~}'", ttIdxHdr.idxName, file-info:full-pathname )).
                  import stream inStrm ttIdxHdr.idxBlks no-error.
                  input stream inStrm close.

                  assign
                    nx = nx + 1
                    bx = bx + ttIdxHdr.idxBlks
                  .

                end.  
          
              put stream redundant _file._file-name space(1) ttIdxHdr.idxDesc space(1).
              if ttIdxHdr.idxBlks <> ? then put stream redundant ttIdxHdr.idxBlks.
              put stream redundant skip.

            end.
           else if ttIdxHdr.idxOverlap > 0 then
            put stream report unformatted fill( "+", 30 ) space(2).	/* the index partially overlaps another		*/
           else
            put stream report unformatted fill( "-", 30 ) space(2).	/* the index stands alone			*/
        end.
      if c > x and c modulo n = 0 then leave.
    end.
    put stream report skip.

    /* output the actual field names of index components prefixed by their "family" affiliations
     */

    do j = 1 to jx:							/* depth					*/

      c = 0.
      for each ttIdxDet where ttIdxDet.idxKeyNum = j by ttIdxDet.idxOrder by ttIdxDet.idxId:
        c = c + 1.
        if c > x then
          if ttIdxDet.idxField = "" then
            put stream report space(32).
           else
            put stream report						/* "family" is an indication of how close two	*/
              ttIdxDet.idxFamily space(1)				/* indexes are. even if they aren't completely	*/
              ttIdxDet.idxField space(2)				/* redundant a lot of common leading components	*/
            .								/* might be better expressed differently	*/

        if c > x and c modulo n = 0 then leave.
      end.

      put stream report skip.

    end.

    x = c.								/* which column did we leave off at?		*/

    put stream report skip(1).

  end.

  put stream report skip(2).

end.

output stream report close.

if ix > 0 then
  do:
    put stream redundant skip(1).
    put stream redundant unformatted
      substitute( "&1 redundant indexes wasting a total of &2 blocks.", trim( string( nx, ">>,>>9" )), trim( string( bx, ">,>>>,>>>,>>9" )))
      skip
    .
  end.

output stream redundant close.

return.

/* lib/dynscreen.p
 *
 */

{lib/dynscreen.i}
{lib/protop.i}

define new global shared variable pt_first as logical no-undo.

define new global shared variable hDataset   as handle    no-undo.	/* ugly hack :( +++ */

{lib/tt_screenlist.i}

define temp-table tt_screenElement no-undo
  field frameName     as character
  field elementName   as character
  field elementRow    as integer
  field elementCol    as integer
  field elementType   as character
  field frameHandle   as handle
  field elementHandle as handle
  field elementValue  as character
  field proType       as character
  field proFormat     as character
  field elementAlign  as character
  index scrFrame is unique primary
    frameName
    elementName
  index elNm_frNm_elH-idx
    elementName
    frameHandle
    elementHandle
.

define new global shared temp-table tt_browseColumnList no-undo
  field browseName     as character
  field columnName     as character
  field browseHandle   as handle
  field bufferHandle   as handle
  field columnHandle   as handle
  field columnWidth    as integer
  index brwCol is unique primary
    browseName
    columnName
  index brwHdl
    browseHandle
.

define variable ok as logical no-undo.

define variable alwaysGrab as character no-undo.

alwaysGrab = "DBId".										/*   "DBId,Dashboard,Configuration"		*/

define variable clrmap  as character no-undo initial "normal,alert,warning".
define variable clrterm as character no-undo.

/* protermcap*
 *
 *	:COLOR 5 BLACK=\E[30m:\E[0m:\
 *      :COLOR 6 RED=\E[31m:\E[0m:\
 *      :COLOR 7 GREEN=\E[32m:\E[0m:\
 *      :COLOR 8 YELLOW=\E[33m:\E[0m:\
 *      :COLOR 9 BLUE=\E[34m:\E[0m:\
 *      :COLOR 10 MAGENTA=\E[35m:\E[0m:\
 *      :COLOR 11 CYAN=\E[36m:\E[0m:\
 *	:COLOR 12 WHITE=\E[5;37m:\E[0m:\
 *	:COLOR 13 GRAY=\E[5;5;47m:\E[0m:\
 *
 * protop.ini
 *
 *    [WinChar Colors]
 *    color0=BLACK/WHITE         "NORMAL"
 *    color1=BLUE/WHITE          "INPUT, UNDERLINE"   
 *    color2=BLUE/WHITE          "MESSAGES, REVERSE"
 *    color3=RED/WHITE           "HIGHLITE, HELP"
 *    color4=RED/WHITE           "URGENT"
 *    color5=BLACK/WHITE         "URGENT"
 *    color6=RED/WHITE           "URGENT"
 *    color7=GREEN/WHITE         "URGENT"
 *    color8=YELLOW/WHITE        "URGENT"
 *    color9=BLUE/WHITE          "URGENT"
 *    
 *    ;NORMAL=WHITE/BLUE
 *    ;INPUT=BLACK/GRAY 
 *    ;MESSAGES=BLACK/GRAY 
 *    
 */

if term matches "*xterm*" then
  clrterm = "5,6,9".										/* black, red & blue on a white background	*/
 else if term = "co80" then
  /* clrterm = "5,9,6". */									/* black, red & gray on a white background	*/
  clrterm = "5,6,9".										/* black, red & blue on a white background	*/
 else if "{&window-system}" <> "tty" then
  clrterm = "0,4,1".										/* black, red & blue on a white background	*/
 else
  clrterm = "5,6,9".										/* black, red & gray on a white background	*/

define variable browseStart as integer no-undo initial 3.					/* line# where browsers	can start displays	*/
define variable browseLines as integer no-undo.							/* number of lines available for browsers	*/

define variable resultSet as handle no-undo.

create dataset resultSet.

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

subscribe to "protop_show"      anywhere run-procedure "protop_show".
subscribe to "protop_display"   anywhere run-procedure "protop_display".
subscribe to "protop_getReqStr" anywhere run-procedure "protop_getReqStr".
subscribe to "resizeBrowse"     anywhere run-procedure "resizeBrowse".

return.


/** library of functions and procedures
 **
 **/


function getColorNum returns integer ( colorCode as character ):

  define variable colorNum as integer no-undo.
  
  colorNum = integer( entry( lookup( colorCode, clrmap ), clrterm )) no-error.

  publish "logMsg" ( 8, substitute( "Looking for: &1 clrMap = &2 clrTerm = &3 result: &4", colorCode, clrMap, clrTerm, lookup( colorCode, clrmap ))).

  if error-status:error then 
   return 1.
  else
   return colorNum. 

end.


procedure getConfig:

  define input  parameter cfgName as character no-undo.
  define output parameter cfgCLOB as longchar  no-undo.

  define variable dName   as character no-undo.
  define variable respHdr as character no-undo.

  dName = search( cfgName ).							/* first check the PROPATH			*/

  if dName = ? then dName = search( "ptdefs/" + cfgName ).			/* maybe it is in a sub-dir?			*/

  if dName <> ? then								/* didn't find a file -- maybe a URL?		*/
    do:
      if dbgMode >= 3 then message now "reading" dName.
      copy-lob from file dName to cfgCLOB.					/* return a longchar				*/
    end.
   else										/* try dbappraise.com				*/
    do:
      message now "Could not find: ptdefs/" + cfgName view-as alert-box.
    end.

  return.

end.


function rowDisplay returns logical ( z as handle ):

  define buffer tt_browseColumnList for tt_browseColumnList.

  define variable hiliteAttr as character no-undo.

  if not valid-handle( z ) then
    do:
      publish "logMsg" ( 3, "Bogus browse handle in rowsDisplay!" ).
      return false.
    end.

  for each tt_browseColumnList where browseHandle = z and valid-handle( z ):

    hilite( tt_browseColumnList.bufferHandle, columnName, tt_browseColumnList.bufferHandle:buffer-field( columnName ):buffer-value, output hiliteAttr ).

    if hiliteAttr <> "" and valid-handle( columnHandle ) then
      do:
       
        if "{&window-system}" = "tty" then
          columnHandle:dcolor  = getColorNum( hiliteAttr ).
         else
          columnHandle:fgcolor = getColorNum( hiliteAttr ).
      end.

  end.

  return true.

end.


function getBrowseColumnByName returns handle ( z as handle, n as character ):

  define variable c as handle no-undo.

  c = z:first-column.

  do while valid-handle( c ):
    if c:name = n then
      leave.
     else
      c = c:next-column.
  end.

  return c.

end.


function trackElement returns logical ( frmH as handle, elemH as handle, eName as character, proType as character, proFormat as character, horAlign as character, r as integer, c as integer, elemType as character ):

  define buffer tt_screenElement for tt_screenElement.

  find tt_screenElement where
    elementName   = eName and
    frameHandle   = frmH  and
    elementHandle = elemH
    no-error.

  if not available( tt_screenElement ) then
    do:
      create tt_screenElement.
      assign
        tt_screenElement.frameName     = frmH:name
        tt_screenElement.elementName   = eName
        tt_screenElement.elementRow    = r
        tt_screenElement.elementCol    = c
        tt_screenElement.elementType   = elemType
        tt_screenElement.frameHandle   = frmH
        tt_screenElement.elementHandle = elemH
        tt_screenElement.proType       = proType
        tt_screenElement.proFormat     = proFormat
        tt_screenElement.elementAlign  = horAlign
      .
    end.

  return true.

end.


function addLabel returns logical ( f as handle, wName as character, r as integer, c as integer, w as integer ):

  define variable t as handle no-undo.

  create text t.
  assign
    t:frame        = f
    t:name         = wName
    t:format       = substitute( "x(&1)", max( 1, w, length( wName )))
    t:row          = r
    t:col          = c
    t:screen-value = wName
    f:height-chars = max( r, f:height-chars )
    browseStart    = max( browseStart, f:row + f:height-chars + 1 )
  .

  trackElement( f, t, wName, "character", t:format, "right", r, c, "label" ).

  return true.

end.


function addDataElement returns logical ( f as handle, wName as character, r as integer, c as integer, w as integer, proType as character, proFormat as character, horAlign as character ):

  define variable t as handle no-undo.

  define variable pFmt as character no-undo.

  pFmt = proFormat.
  if pFmt = "" then pFmt = substitute( "x(&1)", max( 1, w )).

  create text t.
  assign
    t:frame        = f
    t:name         = wName
    t:width-chars  = w
    t:row          = r
    t:col          = c
    t:format       = substitute( "x(&1)", max( 1, w, length( wName )))
    t:screen-value = wName /* proType */ /* pFmt */			/* handy for debugging layout, use PAUSE in protop.p	*/
    f:height-chars = max( r, f:height-chars )
    browseStart    = max( browseStart, f:row + f:height-chars + 1 )
  .

  trackElement( f, t, wName, proType, pFmt, horAlign, r, c, "field" ).

  return true.

end.


procedure dynViewerInit:

  define input parameter f         as handle    no-undo.
  define input parameter dcName    as character no-undo.
  define input parameter ttName    as character no-undo.
  define input parameter evtName   as character no-undo.
  define input parameter dispOrder as integer   no-undo.
  define input parameter rMin      as integer   no-undo.
  define input parameter rMax      as integer   no-undo.

  define buffer tt_screenList for tt_screenList.

  define variable scrDef as handle no-undo.

  define variable q as handle  no-undo.
  define variable b as handle  no-undo.

  define variable i as integer no-undo.
  define variable r as integer no-undo.
  define variable c as integer no-undo.
  define variable w as integer no-undo.

  define variable dCLOB      as longchar  no-undo.

  define variable proFmt   as character no-undo.
  define variable proType  as character no-undo.
  define variable horAlign as character no-undo.

  define variable fName as character no-undo.
  define variable cText as character no-undo.

  define variable bx as handle  no-undo.

  define variable fillChar as character no-undo initial " ".		/* "." is useful for layout debugging...	*/

  define variable cut1 as integer no-undo.
  define variable cut2 as integer no-undo.

  run getConfig( substitute( "&1.xml", dcName ), output dCLOB ).

  cut1 = index( dCLOB, 'file="' ).
  if cut1 > 0 then
    cut2 = index( dCLOB, 'columns="' ).

  if cut2 > 0 then
    cut2 = cut2 + 10 + index( substring( dCLOB, cut2 + 10, 10 ), '"' ).

  if dbgMode >= 5 and cut1 > 0 and cut2 > 0 then
    message
      dcName "removing:" cut1 "through" cut2 skip
      string( substring( dCLOB, cut1, ( cut2 - cut1 )))
     view-as
      alert-box
    .

  if cut1 > 0 and cut2 > 0 then
    substring( dCLOB, cut1, ( cut2 - cut1 )) = fill( " ", ( cut2 - cut1 )).

  create temp-table scrdef.
  scrDef:read-xml( "longchar", dCLOB, "empty", ?, ?, ?, ?  ).

  assign
    b      = scrDef:default-buffer-handle
    f:name = ttName
    f:row  = ( if evtName = "dbid" then 2 else 4 )
  .

/* +++
 * run getConfig( substitute( "&1.xsd", ttName ), output dCLOB ).
 * create temp-table bx.
 * bx:read-xmlschema( "longchar", dCLOB, ?, ?, ?  ).
 */

  create query q.

  find tt_screenList where tt_screenList.ttName = ttName no-error.
  if not available( tt_screenList ) then
    do:
      create tt_screenList.
      assign
        tt_screenList.displayOrder  = dispOrder
        tt_screenList.screenName    = f:private-data
        tt_screenList.ttName        = ttName
        tt_screenList.dcName        = dcName
        tt_screenList.evtName       = evtName
        tt_screenList.frameHandle   = f
        tt_screenList.bufferHandle  = ?
        tt_screenList.queryHandle   = q
        tt_screenList.browseHandle  = ?				/* there is no browse associated with a viewer			*/
        tt_screenList.screenType    = "viewer"
        tt_screenList.screenVisible = ( evtName = "dbid" )	/* initially only "dbid" is automatically visible		*/
        tt_screenList.screenRow     = f:row
      .
    end.

  /* when we initialize the viewer fill in fields with the field name
   */

  q:set-buffers( b ).
  q:query-prepare( "for each " + scrDef:name ).
  q:query-open.

  q:get-next().

  do while q:query-off-end = false:

    i = i + 1.

    assign
      fName = b:buffer-field( "ControlName" ):buffer-value()
      cText = b:buffer-field( "ControlText" ):buffer-value()
      c     = integer( entry( 1, b:buffer-field( "ControlLocation" ):buffer-value()))
      r     = integer( entry( 2, b:buffer-field( "ControlLocation" ):buffer-value()))
      w     = integer( entry( 1, b:buffer-field( "ControlSize" ):buffer-value()))
    .

    assign
      proType  = b:buffer-field( "proType" ):buffer-value()
      horAlign = b:buffer-field( "HAlign" ):buffer-value()
      proFmt   = b:buffer-field( "proFormat" ):buffer-value()
    no-error.

    assign
      r = max( 1, r )
      c = max( 1, c )
    .

    if length( cText ) < w then cText = fill( fillChar, w - length( cText )) + cText.

    if index( fName, "_label" ) > 0 then
      addLabel( f, cText, r, c, w ). 
     else
      addDataElement( f, fName, r, c, w, proType, proFmt, horAlign ).

    q:get-next().

  end.

  delete object scrDef.

  /* do not delete object q -- it is persisted in tt_screenList for later usage */

  return. /* true. */

end.


procedure dynBrowserInit:

  define input parameter f         as handle    no-undo.
  define input parameter dcName    as character no-undo.
  define input parameter ttName    as character no-undo.
  define input parameter evtName   as character no-undo.
  define input parameter dispOrder as integer   no-undo.
  define input parameter rMin      as integer   no-undo.
  define input parameter rMax      as integer   no-undo.

  define buffer tt_screenList for tt_screenList.

  define variable dCLOB      as longchar  no-undo.

  define variable b as handle  no-undo.
  define variable q as handle  no-undo.

  find tt_screenList where tt_screenList.ttName = ttName no-error.
  if not available( tt_screenList ) then
    do:

      create query q.

      create tt_screenList.
      assign
        tt_screenList.displayOrder  = dispOrder
        tt_screenList.screenName    = f:private-data
        tt_screenList.ttName        = ttName
        tt_screenList.dcName        = dcName
        tt_screenList.evtName       = evtName
        tt_screenList.frameHandle   = f
        tt_screenList.queryHandle   = q					/* browsers need a query		*/
        tt_screenList.browseHandle  = ?					/* defer until screen size is known	*/
        tt_screenList.screenType    = "browser"
        tt_screenList.screenVisible = no
        tt_screenList.minRows       = rMin
        tt_screenList.maxRows       = rMax
      .
    end.

/* +++
 * run getConfig( substitute( "&1.xsd", ttName ), output dCLOB ).
 * create temp-table b.
 * b:read-xmlschema( "longchar", dCLOB, ?, ?, ?  ).
 */

/* +++
 * tt_screenList.bufferHandle = b:default-buffer-handle no-error.	/* +++ added no-error */	/* why do browsers have this but viewers do not?	*/
 * q:set-buffers( b:default-buffer-handle ) no-error.			/* +++ added no-error */
 */

  /* do not delete object q or b -- it is persisted in tt_screenList for later usage */

  return. /* true. */

end.


procedure dynViewerUpdate:

  define input parameter q as handle no-undo.
  define input parameter f as handle no-undo.
  define input parameter b as handle no-undo.

  define buffer tt_screenElement for tt_screenElement.

  define variable i as integer no-undo.			/* tables		*/
  define variable j as integer no-undo.			/* fields		*/
  define variable k as integer no-undo.			/* fields in a buffer	*/

  define variable r as integer no-undo.			/* rows			*/
  define variable c as integer no-undo.			/* columns		*/
  define variable w as integer no-undo.			/* width		*/

  define variable ok as logical no-undo.

  define variable bufList   as character no-undo.
  define variable hiliteAttr as character no-undo.

  define variable x as handle no-undo.

  q:get-first().					/* where did q come from?	*/

  do while not q:query-off-end:

    r = r + 1.

    do j = 1 to b:num-fields:

      c = c + 1.
      x = b:buffer-field( j ).

      find tt_screenElement where frameName = b:name and elementName = x:name no-error.

      /* update a screen element
       */

      if available( tt_screenElement ) then
        do:

          /* I really should be ashamed of myself for hard-coding this dbLogName assignment :(  To say nothing of the shared variable...
           */

          if elementName = "dbidLogName" then
            ptDBName = x:buffer-value.

          hilite( b, elementName, x:buffer-value, output hiliteAttr ).
 
          if hiliteAttr <> "" then
            do:
              if "{&window-system}" = "tty" and valid-handle( tt_screenElement.elementHandle ) then
                tt_screenElement.elementHandle:dcolor  = getColorNum( hiliteAttr ).
               else
                tt_screenElement.elementHandle:fgcolor = getColorNum( hiliteAttr ).
            end.
           else
            do:
              if "{&window-system}" = "tty" and valid-handle( tt_screenElement.elementHandle ) then
                tt_screenElement.elementHandle:dcolor  = ?.
               else
                tt_screenElement.elementHandle:fgcolor = ?.
            end.

          case tt_screenElement.proType:

           when "integer" then tt_screenElement.elementHandle:screen-value = string( integer( x:buffer-value ), tt_screenElement.proFormat ) no-error.
           when "decimal" then tt_screenElement.elementHandle:screen-value = string( decimal( x:buffer-value ), tt_screenElement.proFormat ) no-error.
           otherwise
             do:
               if tt_screenElement.elementAlign = "Right" then
                 tt_screenElement.elementHandle:screen-value =
                   fill( " ", integer( tt_screenElement.elementHandle:width-chars ) - length( trim( x:buffer-value ))) +
                   trim( x:buffer-value ).
                else if tt_screenElement.elementAlign = "Center" then
                 tt_screenElement.elementHandle:screen-value =
                   fill( " ", integer(( tt_screenElement.elementHandle:width-chars - length( trim( x:buffer-value ))) / 2 )) +
                   trim( x:buffer-value ).
                else /* "left" */
                 tt_screenElement.elementHandle:screen-value = trim( x:buffer-value ).
             end.

          end.

          tt_screenElement.elementValue = tt_screenElement.elementHandle:screen-value.

          if error-status:num-messages > 0 then
            do:
              publish "logMsg" ( 8, substitute( "Error rendering: &1 &2 &3 &4", x:name, x:buffer-value, tt_screenElement.proType, tt_screenElement.proFormat )).
            end.

        end.

    end.

    q:get-next().

  end.

  return.

end.


procedure dynBrowserUpdate:

  define input parameter q as handle no-undo.
  define input parameter f as handle no-undo.
  define input parameter b as handle no-undo.
  define input parameter z as handle no-undo.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN
  .
&ELSE
  return.
&ENDIF

  define buffer tt_screenList for tt_screenList.
  define buffer tt_browseColumnList for tt_browseColumnList.

  define variable i as integer no-undo.
  define variable j as integer no-undo.

  define variable k as integer no-undo.					/* fields in a buffer					*/
  define variable c as integer no-undo.					/* column width						*/
  define variable w as integer no-undo.					/* frame width						*/

  define variable xlist as character no-undo.				/* fields to exclude from browsers			*/

  define variable needList  as logical   no-undo.			/* do we need to create the list of fields?		*/
  define variable zField    as character no-undo.			/* what field are we sorting on?			*/
  define variable sortAsc   as logical   no-undo.			/* ascending = yes, descending = no			*/

  define variable bf as handle no-undo.

  find tt_screenList where tt_screenList.ttName = b:name no-error.
  if not available( tt_screenList ) then
    do:
      message "That's odd -- no tt_screenList for:" b:name view-as alert-box.
      pause.
      return.
    end.

  if not valid-handle( z:query ) or not valid-handle( q ) then
    do:
      message "dynBrowserUpdate() - invalid query associated with browse:" b:name " q:valid?" valid-handle( q ).
      pause.
      return.
    end.

  /* determine what field we are sorting on
   */

  assign
    zField  = tt_screenList.sortBy
    sortAsc = ( if tt_screenList.sortDir = "Up" then true else false )
  .

  if zField = "stackNum" then sortAsc = true.				/* override default "descending" for numeric field	*/

  if tt_screenList.sortFieldList = "" then needList = true.

  if needList = true or zField = "" then
    do i = 1 to num-entries( q:index-information ):
      if entry( i, q:index-information ) <> "WHOLE-INDEX" then
        do:
          if zField = "" then zField = replace( entry( i, q:index-information ), "-idx", "" ).	/* fieldName-idx naming convention...		*/
          j = 0.
          do while true:
            j = j + 1.
            if b:index-information( j ) = ? then leave.
            if needList then tt_screenList.sortFieldList =  tt_screenList.sortFieldList + ( if tt_screenList.sortFieldList = "" then "" else ",") + entry( 5, b:index-information( j )).
            if b:index-information( j ) = sortField then
              do:
                sortAsc = ( entry( 6, b:index-information( j )) = "0" ).
                leave.
              end.
          end.
          leave.
        end.
    end.

  assign
    tt_screenList.sortBy  = zField
    tt_screenList.sortDir = ( if sortAsc then "Up" else "Down" )
  .

  if z:num-columns = 0 then						/* we must need columns to be added!			*/
    do:

      w = 0.								/* width is 0 to start					*/

      xlist = "xid,xvalid,xtime,ztime,cpLenSec".			/* known unwanted fields				*/
      do k = 1 to b:num-fields:						/* walk through the fields				*/
        if b:buffer-field( k ):extent > 1 then				/* suppress array fields				*/
          xlist = xlist + "," + b:buffer-field( k ):name.
        if b:buffer-field( k ):name begins "z" then			/* suppress fields whose name beings with "z"		*/
          xlist = xlist + "," + b:buffer-field( k ):name.
      end.

      z:add-columns-from( b, xlist ).					/* hide any "xid", "xvalid" or array fields		*/

      do k = 1 to b:num-fields:						/* walk through the fields				*/

        if lookup( b:buffer-field( k ):name, xlist ) > 0 then next.	/* exclude "xid" etc. from the width calculation	*/

        c = ( max(							/* the label might be wider than the data format	*/
              b:buffer-field( k ):width-chars,
              length( b:buffer-field( k ):label )
            )).

        w = w + c + 1.							/* account for the space between each field...		*/

        /* set up a mapping table so that row-display triggers can find
         * the browse columns
         */

        bf = b:buffer-field( k ).					/* 10.0a barfs without this...				*/

        find tt_browseColumnList where browseName = b:name and columnName = bf:name no-error.
        if not available( tt_browseColumnList ) then create tt_browseColumnList.
        assign
          tt_browseColumnList.columnHandle = getBrowseColumnByName( z, b:buffer-field( k ):name )
          tt_browseColumnList.browseName   = b:name
          tt_browseColumnList.columnName   = b:buffer-field( k ):name
          tt_browseColumnList.columnWidth  = c
          tt_browseColumnList.browseHandle = z
          tt_browseColumnList.bufferHandle = b
        .

      end.

      assign
        w = w + 4							/* the box plus the useless "selected" indicator...	*/
        tt_screenList.frameHandle:width-chars  = w
        tt_screenList.browseHandle:width-chars = w
      .

      if w < 160 and dbgMode >= 3 then
        do:
          message "narrow frame:" w view-as alert-box.
        end.

      if w > 160 and dbgMode >= 3 then
        do:
          publish "logMsg" ( 3, substitute( "&1 > 160 &2", w, tt_screenList.frameHandle:name )).
        end.

    end.

  /* show which column we're sorting the browse by
   */

  do k = 1 to b:num-fields:						/* walk through the fields				*/

    bf = b:buffer-field( k ).						/* 10.0a barfs without this...				*/

    find tt_browseColumnList where browseName = b:name and columnName = bf:name no-error.
    if not available( tt_browseColumnList ) or not valid-handle( tt_browseColumnList.columnHandle ) then next.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1B" &THEN
    if tt_browseColumnList.columnName = tt_screenList.sortBy then
      tt_browseColumnList.columnHandle:sort-ascending = sortAsc.
     else
      tt_browseColumnList.columnHandle:sort-ascending = ?.
&ENDIF

  end.

  /* "SYSTEM ERROR: Memory violation. (49)" occasionally comes from the following block of code without any apparent rhyme or reason
   */

  if tt_screenList.screenVisible = yes then
    do:
      if f:hidden = yes then						/* if we are already visible don't do anything		*/
        assign
          f:hidden  = no
          z:hidden  = no
          f:visible = yes
          z:visible = yes
        no-error.
    end.
   else
    do:
      if f:hidden = no then						/* if we are already invisible don't do anything	*/
        assign
          z:visible = no
          f:visible = no
          z:hidden  = yes
          f:hidden  = yes
        no-error.
    end.

  if "{&window-system}" <> "tty" then			/* this must come *after* the block above!				*/
    assign						/* it seems odd -- wouldn't it be better to shut off the scroll-bar	*/
      z:scrollbar-vertical = false			/* while we are still hidden?						*/
    .

  return.

end.


/* read-xml or read-json the data & map xml/json elements to dynamic screen elements...
 */

function dynScreenUpdate returns logical ( data as longchar ):

  define buffer tt_screenList for tt_screenList.

  define variable q as handle  no-undo.
  define variable b as handle  no-undo.
  define variable f as handle  no-undo.
  define variable z as handle  no-undo.

  define variable i as integer no-undo.			/* tables		*/
  define variable j as integer no-undo.			/* fields		*/
  define variable k as integer no-undo.			/* fields in a buffer	*/

  define variable r as integer no-undo.			/* rows			*/
  define variable c as integer no-undo.			/* columns		*/
  define variable w as integer no-undo.			/* width		*/

  define variable ok as logical no-undo.

  define variable bufList   as character no-undo.
  define variable zField    as character no-undo.

  /* Issue Number: OE00197649
   * Can't READ-JSON into a nameless dynamic dataset
   *
   * The READ-JSON method fails when called on a dynamic ProDataSet that does
   * not have it's NAME attribute set, but does have buffers attached via the
   * SET-BUFFERS() method.
   *
   */

/*** +++
 *
 *  assign
 *    resultSet:name = "ProDataSet"					/* the default ProdDataSet name			*/
 *    ok = resultSet:set-buffers( "" )
 *  no-error.
 *
 *  /* the idea is to pre-populate the dataset with the buffer handles
 *   *
 *   */
 *
 *  for each tt_screenList:						/* print-screen support				*/
 *    if valid-handle( tt_screenList.bufferHandle ) then
 *      resultSet:add-buffer( tt_screenList.bufferHandle ).
 *     else
 *      publish "logMsg" ( 3, substitute( "Invalid buffer handle in tt_screenList: &1", tt_screenList.ttName )).
 *  end.
 *
 *  /* slurp the ProDataSet into resultSet
 *   *
 *   * trapping errors would be helpful -- but seems difficult... if not out and out impossible :(
 *   *
 *   */
 *
 *  i = 0 no-error.
 *
 *  if index( data, "<?xml " ) = 1 then
 *    resultSet:read-xml( "longchar", data, "empty", ?, ?, ?, ? ).	/* prevents v9 support				*/
 *   else if index( data, '~{"ProDataSet":' ) = 1 then
 *    do:
 *
 * &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 AND PROVERSION >= "10.2B" &THEN
 *      resultSet:read-json( "longchar", data, "empty" ) no-error.	/* JSON is very delicate...			*/
 * &ELSE
 *      message
 *        "Support for JSON in the client interface requires 10.2B or higher." skip
 *        'You might try adding export SENDTYPE="XML" to bin/protop'
 *        view-as alert-box
 *      .
 *      quit.
 * &ENDIF
 *
 *      /* unfortunately this doesn't actually catch anything interesting :(
 *       */
 *
 *      if dbgMode >= 5 and error-status:num-messages > 0 then
 *        do:
 *          do i = 1 to error-status:num-messages:
 *            message now "read-json:" error-status:get-number( i ) error-status:get-message( i ).
 *          end.
 *          pause.
 *        end.
 *
 *    end.
 *   else
 *    do:
 *      publish "logMsg" ( 1, "Invalid data passed to dynScreenUpdate" ).
 *      return false.
 *    end.
 *
 * +++ ***/

  /* match up the buffers in the ProDataSet with the visible viewers & browsers
   */

  loop:

/* +++
 *  do i = 1 to resultSet:num-buffers:
 *    b = resultSet:get-buffer-handle( i ).
 * +++ */

  do i = 1 to hDataSet:num-buffers:
    b = hDataSet:get-buffer-handle( i ).
    bufList = substitute( "&1&2&3", bufList, min( ",", bufList ), b:name ).
    find tt_screenList where tt_screenList.ttName = b:name no-error.
    if not available( tt_screenList ) then
      next loop.
     else
      tt_screenList.bufferHandle = b.
  end.

  /* message i "buffers slurped:" bufList view-as alert-box.
   */

  create query q.
  q:forward-only = no.

/* +++
 *  do i = 1 to resultSet:num-buffers:
 *    b = resultSet:get-buffer-handle( i ).
 * +++ */

  do i = 1 to hDataSet:num-buffers:
    b = hDataSet:get-buffer-handle( i ) no-error.			/* +++ added no-error */
    if valid-handle( b ) and b:name matches "*_Info" then
      do:
        find tt_screenList where tt_screenList.ttName = substring( b:name, 1, index( b:name, "_Info" ) - 1 ) no-error.
        if available tt_screenList and valid-handle( b:default-buffer-handle ) then
          do:

            q:set-buffers( b ) no-error.				/* +++ added no-error */
            q:query-prepare( "preselect each " + b:name ) no-error.	/* +++ added no-error */
            q:query-open no-error.					/* +++ added no-error */
            q:get-first() no-error.					/* +++ added no-error */

            if b:available and valid-handle( tt_screenList.browseHandle ) then
              do:
                /* message             b:buffer-field( "infoString" ):buffer-value view-as alert-box. */
                tt_screenList.browseHandle:title = " " + b:buffer-field( "infoString" ):buffer-value + " " no-error. /* +++ added no-error */
              end.

            q:query-close() no-error.

          end.
      end.
  end.

  q:query-close() no-error.
  delete object q.

  /* process the viewers & browsers in the order that they are on the screen (by screen row)
   */

  bufList = "".

  for each tt_screenList
     where tt_screenList.screenVisible = yes
      by tt_screenList.displayOrder:

    f = tt_screenList.frameHandle.

    assign ok = true no-error.

    b = ?.
    if valid-handle( tt_screenList.bufferHandle ) then b = tt_screenList.bufferHandle.

    if not valid-handle( b ) then next.

    /* do NOT do this! it closes the *previous* iteration's query!  bad idea if that was a browse window :(	
     * it will result in all but the last browse being empty
     *
     *** q:query-close() no-error.
     *
     */

    if tt_screenList.screenType = "browser" then
      do:

        z = tt_screenList.browseHandle.

        if not valid-handle( z ) then			/* and lookup( tt_screenList.dcName, alwaysGrab ) > 0 */
          do:
            message "dynScreenUpdate() - missing browse widget!" tt_screenList.ttName.
            /* pause. */
            next.
          end.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN

        if not valid-handle( z:query ) or not valid-handle( tt_screenList.queryHandle ) then
          do:
            message
              "dynScreenUpdate() - invalid query associated with browse:" b:name
              " z:" valid-handle( z )
              " zq:" valid-handle( tt_screenList.queryHandle )
            .
            pause.
            next.
          end.

&ENDIF

      end.

    zField = "".
    if tt_screenList.sortBy <> "" then
      zField = substitute( "by &1 &2", tt_screenList.sortBy, ( if tt_screenList.sortDir = "Up" then "" else "descending" )).

    if b:name = "tt_pasoeInfo" then
      zField = "by xid".

    q =  tt_screenList.queryHandle.

    if not valid-handle( q ) then
      do:
        message "dynScreenUpdate() - WTF? q is invalid?".
        pause.
        next.
      end.

    assign ok = true no-error.

    q:query-close() no-error.
    q:set-buffers( b ) no-error.
    q:query-prepare( "for each " + b:name + " " + zField ) no-error.

    if error-status:num-messages > 0 then				/* data collector did not return tt?			*/
      do:
        do i = 1 to error-status:num-messages:
          message "query-prepare( for each" b:name zField ")" error-status:get-number( i ) error-status:get-message( i ) view-as alert-box.
        end.
        next.
      end.

    assign ok = true no-error.

    q:query-open() no-error.

    if error-status:num-messages > 0 then
      do:
        do i = 1 to error-status:num-messages:
          message "query-open()" b:name zField error-status:get-number( i ) error-status:get-message( i ) view-as alert-box.
        end.
        next.
      end.

    tt_screenList.resultRows = q:num-results.

    bufList = substitute( "&1&2&3", bufList, min( ",", bufList ), b:name ).

    if tt_screenList.screenType = "browser" then
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN
      run dynBrowserUpdate( q, f, b, z ).
&ELSE
      .
&ENDIF
     else if tt_screenList.screenType = "viewer" then
      run dynViewerUpdate( q, f, b ).

  end. /* for each tt_screenList ... */

  return true.

end.


procedure protop_getReqStr:

  define input-output parameter reqString as character no-undo.

  define variable cmd as character no-undo.

  define variable i as integer no-undo.

  for each tt_screenList where tt_screenList.screenVisible = yes:
    cmd = cmd + ( if cmd > "" then "," else "" ) + tt_screenList.dcName.
  end.

  /* always grab certain data collectors -- whether they are visible or not
   */

  do i = 1 to num-entries( alwaysGrab ):
    if lookup( entry( i, alwaysGrab ), cmd ) < 1 then cmd = entry( i, alwaysGrab ) + "," + cmd.	/* print screen support	*/
  end.

  publish "logMsg" ( 9, substitute( "Command = &1", cmd )).

  reqString = cmd.

  return.

end.


/* make sure that viewers and browsers are properly associated with one another
 */

procedure protop_show:

  define input parameter evtName  as character no-undo.

  define buffer tt_screenList for tt_screenList.
  define buffer xtt_screenList for tt_screenList.

  define variable v as logical no-undo.

  find tt_screenList where tt_screenList.evtName = evtName no-error.
  
  if not available( tt_screenList ) then
    do:
      publish "logMsg" ( 3, substitute( 'unknown event: "&1"', evtName )).
      return.
    end.

  if lookup( evtName, "usrtblact,usridxact,usrstack,usrother" ) > 0 then
    do:
      find xtt_screenList where xtt_screenList.evtName = "userinfo".
      if  xtt_screenList.screenVisible = no then return.
    end.

  if tt_screenList.screenType = "browser" then
    do:

      tt_screenList.screenVisible = not( tt_screenList.screenVisible ).

      /* keep "latches", "resources" and "txe" bound together -- some odd things happen if "latches" is displayed first so
       * jump thorugh some hoops and do "resources" 1st.  the 2nd arg to protop_display() is correct as "tt" NOT "xtt"!
       */

      if tt_screenList.evtName = "latches" then
        do:

          find xtt_screenList where xtt_screenList.evtName = "resources".
          run protop_display( xtt_screenList.ttName, tt_screenList.screenVisible ).
          run protop_display(  tt_screenList.ttName, tt_screenList.screenVisible ).

          find xtt_screenList where xtt_screenList.evtName = "txe".
          run protop_display( xtt_screenList.ttName, tt_screenList.screenVisible ).
          run protop_display(  tt_screenList.ttName, tt_screenList.screenVisible ).

        end.

/*** this was a reasonable thing to do before the blocker data was added to the notes for the blocked session - maybe make it an option if anyone cares about it?
 *     else if tt_screenList.evtName = "blocked" then
 *      do:
 *        find xtt_screenList where xtt_screenList.evtName = "blocker".
 *        run protop_display( xtt_screenList.ttName, tt_screenList.screenVisible ).
 *        run protop_display(  tt_screenList.ttName, tt_screenList.screenVisible ).
 *      end.
 *     else if tt_screenList.evtName = "blocker" then
 *      do:
 *        find xtt_screenList where xtt_screenList.evtName = "blocked".
 *        run protop_display(  tt_screenList.ttName, tt_screenList.screenVisible ).
 *        run protop_display( xtt_screenList.ttName, tt_screenList.screenVisible ).
 *      end.
 ***/

       else if tt_screenList.evtName = "apsvstat" then
        do:
          find xtt_screenList where xtt_screenList.evtName = "apsvdet".
          run protop_display( xtt_screenList.ttName, tt_screenList.screenVisible ).
          run protop_display(  tt_screenList.ttName, tt_screenList.screenVisible ).
        end.
       else if tt_screenList.evtName = "apsvdet" then
        do:
          find xtt_screenList where xtt_screenList.evtName = "apsvstat".
          run protop_display(  tt_screenList.ttName, tt_screenList.screenVisible ).
          run protop_display( xtt_screenList.ttName, tt_screenList.screenVisible ).
        end.

       else
        run protop_display( tt_screenList.ttName, tt_screenList.screenVisible ).

    end.
   else if tt_screenList.screenType = "viewer" then
    do:

      browseStart = 0.

      for each xtt_screenList where xtt_screenList.screenType = "viewer" by xtt_screenList.displayOrder:

        if xtt_screenList.evtName = "DBId" then
          assign
            xtt_screenList.screenVisible        = yes
            xtt_screenList.frameHandle:hidden   = no
            xtt_screenList.frameHandle:visible  = yes
            browseStart = ( xtt_screenList.frameHandle:row + xtt_screenList.frameHandle:height-chars + 1 )
          .
         else if xtt_screenList.evtName = evtName then
          assign
            xtt_screenList.screenVisible        = not ( xtt_screenList.screenVisible )
            xtt_screenList.frameHandle:hidden   = not ( xtt_screenList.screenVisible )
            xtt_screenList.frameHandle:visible  = xtt_screenList.screenVisible
            browseStart = ( xtt_screenList.frameHandle:row + xtt_screenList.frameHandle:height-chars + 1 ) when xtt_screenList.screenVisible = yes
          .
         else
          assign
            xtt_screenList.screenVisible        = no
            xtt_screenList.frameHandle:hidden   = yes
            xtt_screenList.frameHandle:visible  = no
          .

      end.

      /* userInfo is special -- it hides all unrelated browsers and opens its own set of confederates
       */

      if tt_screenList.evtName = "userinfo" then
        do:

          for each xtt_screenList where xtt_screenList.screenType = "browser":

            if lookup( xtt_screenList.evtName, "usrtblact,usridxact,usrstack,usrother" ) = 0 then
              assign
                xtt_screenList.screenVisible        = no		/* hide browsers that are not related to userInfo	*/
                xtt_screenList.frameHandle:hidden   = yes
                xtt_screenList.frameHandle:visible  = no
              .
             else
              assign
                xtt_screenList.screenVisible        = yes		/* reveal browsers that are related to userInfo		*/
                xtt_screenList.frameHandle:hidden   = no
                xtt_screenList.frameHandle:visible  = yes
              .

          end.

        end.
       else
        do:

          for each xtt_screenList					/* hide the userInfo associated browses		*/
              where
                xtt_screenList.screenType = "browser" and
                lookup( xtt_screenList.evtName, "usrtblact,usridxact,usrstack,usrother" ) > 0:

            assign
              xtt_screenList.screenVisible        = no			/* hide the userInfo associated browses		*/
              xtt_screenList.frameHandle:hidden   = yes
              xtt_screenList.frameHandle:visible  = no
            .
          end.

        end.

      if not available( tt_screenList ) then
        publish "logMsg" ( 1, substitute( "No tt_screenlist! &1", evtName )).

      run protop_display( tt_screenList.ttName, tt_screenList.screenVisible ).

    end.
   else
    do:
      publish "logMsg" ( 1, substitute( "Unknown screen type: &1", tt_screenList.screenType )).
    end.

  return.

end.


procedure protop_display:

  define input parameter ttName   as character no-undo.
  define input parameter scrState as logical   no-undo.

  define buffer tt_screenList for tt_screenList.

  define variable s as integer no-undo.
  define variable v as integer no-undo.
  define variable h as integer no-undo.
  define variable r as integer no-undo.
  define variable n as integer no-undo.
  define variable d as integer no-undo.
  define variable t as integer no-undo.

  define variable i as integer no-undo.
  define variable x as integer no-undo.
  define variable y as integer no-undo initial 0.

  define variable q as handle  no-undo.
  define variable b as handle  no-undo.
  define variable f as handle  no-undo.
  define variable z as handle  no-undo.

  define variable xDebug as logical no-undo initial no.

  find tt_screenList where tt_screenList.ttName = ttName no-error.
  if available( tt_screenList ) then
    do:
      tt_screenList.screenVisible = scrState.
      if scrState = no then
        assign
          tt_screenList.frameHandle:hidden  = yes
          tt_screenList.frameHandle:visible = no
        .
    end.

  for each tt_screenList where tt_screenList.screenType = "browser":
    if tt_screenList.screenVisible = yes then s = s + 1.
    if tt_screenList.screenVisible = yes and lookup( tt_screenList.evtName, "latches,resources,txe" ) > 0 then x = x + 1.
    assign
      tt_screenList.frameHandle:hidden   = yes
      tt_screenList.frameHandle:visible  = no
    .
    if valid-handle( tt_screenList.browseHandle ) then
      assign
        tt_screenList.browseHandle:hidden  = yes
        tt_screenList.browseHandle:visible = no
      .
  end.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN
  .
&ELSE
  return.
&ENDIF

  if x >= 2 then s = ( s - ( x - 1 )).				/* if both "latches" and "resources" or "txe" are visible only count one of them	*/
								/* blocked & blocker are NOT beside each other - so no funny business	*/
  browseStart = 3.

  v = 0.
  for each tt_screenList where tt_screenList.screenType = "viewer" and tt_screenList.screenVisible = yes by tt_screenList.displayOrder:
    browseStart = ( tt_screenList.frameHandle:row + tt_screenList.frameHandle:height-chars + 1 ).
    v = v + 1.
  end.

  if s < 1 then
    do:

      /* message "Nothing to see here!" view-as alert-box. */

      if pt_first = no and v = 1 then				/* v = 1 is the screen header					*/
        publish "protop_command" ( "?" ).

      return.							/* nothing more to do!						*/

    end.

  if pt_first = yes then pt_first = no.

  browseLines = ( screen-lines - browseStart ) + 1. 

  /* if there are more than 2 browsers shift the starting row upwards
   */

  if s > 2 then
    assign
      browseStart = browseStart - 1				/* eliminate the blank line between viewers and browsers	*/
      browseLines = browseLines + 1				/* and add a line available for display				*/
    .

  assign							/* overlapping boxes put s - 1 lines back in play...		*/
    h = truncate(( browseLines + ( s - 1 )) / s, 0 )
    r = ( browseLines + ( s - 1 )) modulo s			/* remainder rows -- to dole out one at a time			*/
  .

  d = 0.
  for each tt_screenList where tt_screenList.screenType = "browser" and tt_screenList.screenVisible = yes and tt_screenList.evtName <> "resources":
    if ( tt_screenList.maxRows > 0 ) and ( tt_screenList.maxRows < h ) then
      assign
        d = d + 1						/* how many row donors are there?				*/
        r = r + ( h - tt_screenList.maxRows )			/* donate excess rows to the remainder pool			*/
      .
  end.

  t = ( s - d ).						/* takers = screens - donors					*/

  if ( h < 6 ) then						/* we nust be able to display at least 2 rows			*/
    do:								/* top box + label + underline + bottom box = 4			*/
      message							/* so we need at least 6 rows available...			*/
           "There are too many browsers being displayed!" skip
           skip(1)
           "screenLines:" screen-lines skip
           "browseStart:" browseStart skip
           "browseLines:" browseLines skip
            "  #visible:" s skip
            "new height:" h skip
            "extra rows:" r skip
           skip(1)
           "if you really want to display that much" skip
           "stuff, restart with a taller window!" skip
        view-as alert-box
      .
      /* return. */
      quit.
    end.

  if xDebug = yes then
    do:
      message screen-lines browseStart browseLines s h r view-as alert-box.
    end.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 AND PROVERSION >= "10.1C" &THEN

  /* "s" re-purposed... who thought that was a good idea?
   */

  s = browseStart.

  for each tt_screenList where tt_screenList.screenType = "browser" and tt_screenList.screenVisible = yes
        by tt_screenList.displayOrder:

    /* size each browse -- try to use the screen space efficiently
     */

    if ( tt_screenList.maxRows > 0 ) and ( tt_screenList.maxRows < h ) then
      n = tt_screenList.maxRows.						/* max *data* rows - doesn't include decorations */
     else
      do:
        if tt_screenList.evtName <> "resources" then
          do:
            n = h.								/* standard height				*/
            x = truncate(( r / t ), 0 ).					/* how big an extra chunk to take		*/
            /* if ( r / t ) > truncate(( r / t ), 0 ) then x = x + 1. */	/* favors earlier screen orders			*/
            if r > 0 then							/* dole out remainder rows			*/
              assign
                n = n + x							/* take an additional chunk of rows		*/
                r = r - x							/* decrement remainder rows			*/
                t = t - 1							/* decrement the number of remaining "takers"	*/
              .
          end.
      end.

    publish "logMsg" ( 9, substitute( "row &1", s )).

    if ( n + s ) >= screen-lines then
      publish "logMsg" ( 9, substitute( "trying to make &1 too big? &2 + &3 >= &4", f:name, n, s, screen-lines )).

    assign
      f = tt_screenList.frameHandle
      z = tt_screenList.browseHandle
      tt_screenList.screenRow = s
    .

    assign ok = yes no-error.

    assign
      f:hidden       = yes
      f:visible      = no
      f:height-chars = n						/* change height *before* row!				*/
      f:row          = s
    no-error.
 
    /* latches, resources and txe */

    if tt_screenList.evtName <> "latches" and tt_screenList.evtName <> "resources" then
      s = s + n - 1.							/* "resources" on the same line as "latches"		*/

    if valid-handle( z ) and z:height-chars <> n then
      do:
        publish "logMsg" ( 9, substitute( "Deleting browse &1", tt_screenList.evtName )).
        /* delete object z:query no-error.	*/			/* resizing seems to require a new browse object?	*/
        delete object z no-error.					/* resizing seems to require a new browse object?	*/
        publish "logMsg" ( 9, "Success!" ).
      end.

    if error-status:num-messages > 0 then
      do i = 1 to error-status:num-messages:
        publish "logMsg" ( 1, substitute( "Error: &1 &2", error-status:get-number(i), error-status:get-message(i))).
      end.

    if not valid-handle( z ) then
      do:

        create browse z assign					/* not available in ChUI until 10.1c			*/
          frame        = f
          title        = " " + f:private-data + " "
          name         = f:name
          width-chars  = 4					/* this is just an initial value -- it will change	*/
          read-only    = yes
          hidden       = yes
          visible      = no
/*        box          = no		*/			/* BOX is not a settable attribute for browse widgets	*/
        .

        if "{&window-system}" <> "tty" then
          assign
            z:scrollbar-vertical   = false
            z:row-markers          = no				/* has no impact in ChUI				*/
            z:manual-highlight     = yes			/* has no impact in ChUI				*/
/*          z:box = no                          */      	/* seems to do nothing					*/
/*          z:focused-row-selected = no		*/		/* Error (4052) "not a setable attribute"		*/
          .

      end.

    if not valid-handle( tt_screenList.queryHandle ) then
      do:
        message "protop_display() - tt_screenList.queryHandle is bogus?" dcName.
        pause.
      end.

    assign
      tt_screenList.browseHandle = z
      f:hidden       = yes
      f:visible      = no
      z:hidden       = yes
      z:visible      = no
      z:query        = tt_screenList.queryHandle
/*    z:height-chars = n - 0 */					/* 0, nothing seems to change it 			*/
      z:down         = max( 3, n - 4 )				/* 4 = overlapping boxes, 5 = non-overlapping 		*/	/* 2 box lines, label text and a separator line		*/
    no-error.

    if not valid-handle( z:query ) then
      do:
        message "protop_display() - malformed browse, bad (or empty) query:" tt_screenList.bufferHandle:name.
        /*** pause. ***/
      end.

    if "{&window-system}" <> "tty" then				/* just in case someone runs this on a GUI client...	*/
      assign
/*      z:bgcolor      = 11           */
        z:height-chars = n + 1
        z:down         = n - 5					/* 2 box lines, label text and a separator line		*/
      .

  end.

&ENDIF

  pause 0 before-hide.

  return.

end.


{ssg/sausage06.i}


procedure protopSort:

  run lib/sortx.p. /* ( input-output tt_screenList by reference ). */

end.


procedure resizeBrowse:

  define input parameter zName    as character no-undo.
  define input parameter zMaxRows as integer   no-undo.

  define variable oldRows as integer no-undo.

  find tt_screenlist where tt_screenList.evtName = zName and tt_screenList.screenType = "browser" no-error.

  if available tt_screenlist then
    assign
      oldRows = tt_screenList.maxRows
      tt_screenList.maxRows = max( 2, zMaxRows + 4 )
    .

  if available tt_screenlist and oldRows <> tt_screenList.maxRows and valid-handle( tt_screenList.browseHandle ) then
    do:
      run protop_show ( zname ).
      run protop_show ( zname ).
    end.

  return.

end.

/*** end ***/

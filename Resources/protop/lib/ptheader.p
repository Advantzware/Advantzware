/* lib/ptheader.p
 *
 * create the header portion of the chui screen
 *
 */

define input-output parameter  headerFrame   as handle no-undo.
define input-output parameter  modeIndicator as handle no-undo.
define input-output parameter  protopVersion as handle no-undo.
define input-output parameter  clockString   as handle no-undo.

define variable ptVerStr as character no-undo initial "ProTop Version {lib/ptversion.i} as of {lib/asof.i}".

ptVerStr = replace( ptVerStr, "~n", " " ).
ptVerStr = replace( ptVerStr, "  ", " " ).

define variable w as integer no-undo.

/* w = current-window:width-chars. */

w = 160.

create frame headerFrame.
assign
  headerFrame:row          = 1
  headerFrame:column       = 1
  headerFrame:width-chars  = w
  headerFrame:height-chars = 1
  headerFrame:box          = no
  headerFrame:top-only     = yes
.

create text modeIndicator.
assign
  modeIndicator:frame  = headerFrame
  modeIndicator:row    = 1
  modeIndicator:column = 1
  modeIndicator:format = "x(50)"
/*  modeIndicator:screen-value = sampleMode */
.

create text protopVersion.
assign
  protopVersion:frame  = headerFrame
  protopVersion:row    = 1
  protopVersion:column = integer(( w - length( ptVerStr )) / 2 )
  protopVersion:format = substitute( "x(&1)", length( ptVerStr ))
  protopVersion:screen-value = ptVerStr
.

create text clockString.
assign
  clockString:frame  = headerFrame
  clockString:row    = 1
  clockString:column = w - 18
  clockString:format = "x(19)"
  clockString:screen-value = string( now )
.

assign
  headerFrame:hidden       = no
  headerFrame:visible      = yes
.

return.

/* stinfo.p
 *
 * output assumes that your window is at least 132 columns wide...
 *
 * pro -p stinfo.p -param /db/dbname
 *
 *
 * prostrct statistics dbname
 * ...
 * 
 *   Statistics for Area: Schema Area
 * 
 *   Files in Area: Schema Area
 * ...
 * 
 *   Database Block Usage for Area: Schema Area
 * 
 *   Active blocks: 22618
 *     Data blocks: 11437
 *     Free blocks: 11181
 *    Empty blocks: 6
 *    Total blocks: 22624
 *   Extent blocks: 4
 *   Records/Block: 64
 *    Cluster size: 1
 *  ...
 * 
 */

define variable lineIn as character no-undo.

define variable areaName   as character no-undo format "x(30)" label "Area Name".
define variable areaRPB    as integer   no-undo format   ">>9" label "RPB".
define variable areaCSZ    as integer   no-undo format   ">>9" label "CSZ".
define variable areaActive as int64     no-undo format ">,>>>,>>>,>>>,>>9" label "Active".
define variable areaData   as int64     no-undo format ">,>>>,>>>,>>>,>>9" label "Data".
define variable areaFree   as int64     no-undo format ">,>>>,>>>,>>>,>>9" label "Free".
define variable areaEmpty  as int64     no-undo format ">,>>>,>>>,>>>,>>9" label "Empty".
define variable areaTotal  as int64     no-undo format ">,>>>,>>>,>>>,>>9" label "Total".

input through value( "prostrct statistics " + session:parameter ).
repeat:

  import unformatted lineIn.

  lineIn = trim( lineIn ).

  if lineIn begins "Statistics for Area:" then
    assign
      areaName   = trim( entry( 2, lineIn, ":" ))
      areaRPB    = 0
      areaCSZ    = 0
      areaActive = 0
      areaData   = 0
      areaFree   = 0
      areaEmpty  = 0
      areaTotal  = 0
    .

  if lineIn begins "Active blocks:" then areaActive =   int64( trim( entry( 2, lineIn, ":" ))).
  if lineIn begins   "Data blocks:" then areaData   =   int64( trim( entry( 2, lineIn, ":" ))).
  if lineIn begins   "Free blocks:" then areaFree   =   int64( trim( entry( 2, lineIn, ":" ))).
  if lineIn begins  "Empty blocks:" then areaEmpty  =   int64( trim( entry( 2, lineIn, ":" ))).
  if lineIn begins  "Total blocks:" then areaTotal  =   int64( trim( entry( 2, lineIn, ":" ))).
  if lineIn begins "Records/Block:" then areaRPB    = integer( trim( entry( 2, lineIn, ":" ))).
  if lineIn begins  "Cluster size:" then areaCSZ    = integer( trim( entry( 2, lineIn, ":" ))).

  if lineIn begins "Cluster size:" /* and areaName matches "*idx*" */ then
    do:
      display
        areaName
        areaRPB
        areaCSZ
        areaActive
        areaData
        areaFree
        areaEmpty
        areaTotal
      .
    end.

end.

pause.

return.

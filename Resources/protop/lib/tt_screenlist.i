/* lib/tt_screenlist.i
 */

define new global shared temp-table tt_screenList no-undo

  field displayOrder  as integer
  field screenName    as character format "x(30)"
  field ttName        as character format "x(18)"
  field dcName        as character
  field evtName       as character
  field frameHandle   as handle
  field bufferHandle  as handle
  field queryHandle   as handle
  field browseHandle  as handle
  field screenType    as character
  field screenRow     as integer
  field minRows       as integer
  field maxRows       as integer
  field resultRows    as integer
  field screenVisible as logical
  field screenActive  as logical
  field sortBy        as character format "x(16)" label "Sort Column"
  field sortDir       as character format "x(4)"
  field sortFieldList as character format "x(74)"

  index ttName-idx is unique primary
    ttName
  index scrFrame is unique
    frameHandle
.

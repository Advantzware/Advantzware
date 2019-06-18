/* lib/sortx.p
 *
 */

{lib/tt_screenlist.i}

define query q for tt_screenList.

define
  browse b
  query q
  display

    tt_screenList.screenName

    /*
    tt_screenList.screenActive
    tt_screenList.screenVisible
     */

    /*
    tt_screenList.minRows    format "->9"
    tt_screenList.maxRows    format "->9"
    tt_screenList.resultRows format "->>>9"
     */

    tt_screenList.ttName
    tt_screenList.sortBy
    tt_screenList.sortDir

   with
    overlay
    25 down
    no-box
    no-labels
.

form
    b
    skip(1)
    '      Cursor-Left or "<" & Cursor-Right or ">" to ' skip
    '      cycle through the available sort columns.   ' skip
    skip(1)
    '      "+" or "^" to sort Up (Ascending)           ' skip
    '      "-" or "v" to sort Down (Descending).       ' skip
    skip(1)
   with
    frame screenList
    title " Browser Screen Sort Columns "
    overlay
    row 4
    centered
    no-labels
  .

function initScreen returns logical ():

  pause 0 before-hide.

  if query q:current-result-row = ? or query q:query-off-end = yes then get next q no-lock.
  if query q:query-off-end = true then get first q no-lock.

  return yes.

end.

on "x", "q" of b do:
  apply "go" to self.
  return no-apply.
end.

on "cursor-right", ">" of b do:
  define variable i as integer no-undo.
  i = lookup( tt_screenList.sortBy, tt_screenlist.sortFieldList ).
  if i = 0 then i = 1.
  i = i + 1.
  if i > num-entries( tt_screenlist.sortFieldList ) then i = 1.
  tt_screenList.sortBy = entry( i, tt_screenlist.sortFieldList ).
  b:refresh() in frame screenList.
  return no-apply.
end.

on "cursor-left", "<" of b do:
  define variable i as integer no-undo.
  i = lookup( tt_screenList.sortBy, tt_screenlist.sortFieldList ).
  if i = 0 then i = 1.
  i = i - 1.
  if i < 1 then i = num-entries( tt_screenlist.sortFieldList ).
  tt_screenList.sortBy = entry( i, tt_screenlist.sortFieldList ).
  b:refresh() in frame screenList.
  return no-apply.
end.

on "+", "^" of b do:
  tt_screenList.sortDir = "Up".
  b:refresh() in frame screenList.
  return no-apply.
end.

on "-", "v" of b do:
  tt_screenList.sortDir = "Down".
  b:refresh() in frame screenList.
  return no-apply.
end.

open query q for each tt_screenList where screenType = "browser" by tt_screenList.displayOrder.

initScreen().

enable b with frame screenList.
apply "entry" to b.
apply "value-changed" to b.

do on error undo, leave
  on endkey undo, leave:

  wait-for "go" of b in frame screenList.

end.

hide frame screenList no-pause.

return.

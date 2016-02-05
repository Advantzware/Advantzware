def workfile ttbl no-undo
  field prgmname as char
  field prgtitle as char
  field run_persistent as log
  field dir_group as char
  field menu_item as log.

input from prgrms.d no-echo.
create ttbl.
repeat:
  import ttbl.
  if can-find(prgrms where prgrms.prgmname = ttbl.prgmname) then
  next.
  create prgrms.
  assign
    prgrms.prgmname = ttbl.prgmname
    prgrms.prgtitle = ttbl.prgtitle
    prgrms.run_persistent = ttbl.run_persistent
    prgrms.dir_group = ttbl.dir_group
    prgrms.menu_item = ttbl.menu_item

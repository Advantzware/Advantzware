/* connect.p
 *
 * test the ability to connect N users
 *
 * spawn.sh connect 50
 *
 *
 */

define variable i as integer no-undo.
define variable n as integer no-undo.
define variable r as integer no-undo.
define variable p as integer no-undo.

main_loop: do while true:

  p = random( 1, 10 ).

  /* check every second in order to not wait P seconds to shutdown
   */

  do i = 1 to p:

    file-info:file-name = session:parameter.
    if file-info:full-pathname = ? then
      leave main_loop.

    pause 1.

  end.

  /* if a workload is to be simulated it goes here
   */

  assign
    n = 0
    r = random( 1, 100 )
  .

  case random( 1, 4 ):
   when 1 then for each _db    no-lock: n = n + 1. if n >= r then leave. end.
   when 2 then for each _file  no-lock: n = n + 1. if n >= r then leave. end.
   when 3 then for each _field no-lock: n = n + 1. if n >= r then leave. end.
   when 4 then for each _index no-lock: n = n + 1. if n >= r then leave. end.
  end.

end.

quit.

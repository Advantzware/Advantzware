/* zippy.i
 *
 * user experience monitor
 *
 */

function zippy returns integer:

  define variable xt      as integer no-undo.

  define variable i as integer no-undo.
  define variable j as integer no-undo.

  define variable limit as integer no-undo initial 111.

  xt = etime.

  zipper: do i = 1 to 9:
    j = 0.
    do while true:
      case i:
        when   1 then for each Invoice no-lock:       j = j + 1. if j >= limit then next zipper. end.
        when   2 then for each Customer no-lock:      j = j + 1. if j >= limit then next zipper. end.
        when   3 then for each Item no-lock:          j = j + 1. if j >= limit then next zipper. end.
        when   4 then for each Order no-lock:         j = j + 1. if j >= limit then next zipper. end.
        when   5 then for each Order-Line no-lock:    j = j + 1. if j >= limit then next zipper. end.
        when   6 then for each Salesrep no-lock:      j = j + 1. if j >= limit then next zipper. end.
        when   7 then for each State no-lock:         j = j + 1. if j >= limit then next zipper. end.
        when   8 then for each Local-Default no-lock: j = j + 1. if j >= limit then next zipper. end.
        when   9 then for each Ref-Call no-lock:      j = j + 1. if j >= limit then next zipper. end.
      end.
    end.
  end.

  return ( etime - xt ).

end.

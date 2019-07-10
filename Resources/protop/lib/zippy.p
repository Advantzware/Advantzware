/* lib/zippy.p
 *
 * standard user experience measurement - measure time to access a standard set of db data
 *
 * these meta-schema tables exist in every Progress database and there should
 * always be enough data to support the 500 record target
 *
 * these meta-schema tables will almost certainly always be in the -B cache so
 * there should not be any influence from disk io on the timings
 *
 * the usrExpSHM metric is derived from this data and presented as "records per second"
 *
 */

{lib/v9.i}

session:add-super-procedure( this-procedure ).

return.


/* do the actual work
 */

define variable estart as {&BIGINT} no-undo.

procedure zippy:

  define output parameter r as integer no-undo.
  define output parameter t as integer no-undo.

  define variable i  as integer no-undo.
  define variable ex as integer no-undo.

  r = 0.
  estart = etime.
  ex = estart + 50.		/* run for at least 50ms	*/

  usrexp: do while etime < ex:

    i = 0.
    for each _file no-lock:
      assign
        i = i + 1
        r = r + 1
      .
      if etime > ex then leave usrexp.
      if i >= 100 then leave.
    end.

    i = 0.
    for each _field no-lock:
      assign
        i = i + 1
        r = r + 1
      .
      if etime > ex then leave usrexp.
      if i >= 100 then leave.
    end.
  
    i = 0.
    for each _index no-lock:
      assign
        i = i + 1
        r = r + 1
      .
      if etime > ex then leave usrexp.
      if i >= 100 then leave.
    end.

    i = 0.
    for each _index-field no-lock:
      assign
        i = i + 1
        r = r + 1
      .
      if etime > ex then leave usrexp.
      if i >= 100 then leave.
    end.

    i = 0.
    for each _storageObject no-lock:
      assign
        i = i + 1
        r = r + 1
      .
      if etime > ex then leave usrexp.
      if i >= 100 then leave.
    end.

  end.

  t = etime - estart.

  return.

end.

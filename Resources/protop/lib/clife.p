/* clife.p
 *
 * Conway's Game of Life
 *
 * https://en.wikipedia.org/wiki/Conway%27s_Game_of_Life
 *
 *	Any live cell with fewer than two live neighbours dies, as if by underpopulation.
 *	Any live cell with two or three live neighbours lives on to the next generation.
 *	Any live cell with more than three live neighbours dies, as if by overpopulation.
 *	Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
 *
 * pro -p clife.p -param "seedfile"
 *
 * the seed file is lines of text where "x" indicates a live cell, any other
 * character a dead cell the following 6x6 example (embedded in the comment
 * and indented with a tab) uses "." for dead cells which makes them easier
 * to visualize when creating a seed.
 *

 blink.seed:

	......
	..x...
	..x...
	..x...
	......

 beacon.seed:

	......
	.xx...
	.x....
	....x.
	...xx.
	......

 glider.seed:

	x..
	.xx
	xx.

 *
 */

&global-define	WIDTH	128

define temp-table grid no-undo
  field r as integer					/* row #					*/
  field a as integer extent {&WIDTH}			/* live row data				*/
  field b as integer extent {&WIDTH}			/* scratch row data				*/
  field s as character format "x({&WIDTH})"		/* string holding the display values		*/
  index r-idx is unique primary r
.

define variable h as integer no-undo.			/* screen height				*/
define variable w as integer no-undo.			/* screen width					*/


define variable r1 as integer no-undo extent {&WIDTH}.
define variable r2 as integer no-undo extent {&WIDTH}.
define variable r3 as integer no-undo extent {&WIDTH}.

define variable gen as integer no-undo.			/* generation					*/
define variable y   as integer no-undo.			/* current population				*/
define variable z   as integer no-undo.			/* max population				*/

/* random mutation support ;)
 */

define variable lo  as integer no-undo initial 2.	/* lo random limit, must be <= 1 for randomness to happen	*/
define variable hi  as integer no-undo initial 5000.	/* hi random limit, must be > lo		*/

define stream inStrm.

assign
  h = 40	/* screen-lines	*/			/* 10x10 is handy for testing			*/
  w = {&WIDTH}	/* current-window:width-chars	*/	/* make sure to match with format of grid.s	*/
.

form
  /* grid.r format "999" */ grid.s skip
 with
  frame gs
  /* no-box */
  title " Conway's Game of Life "
  no-labels
  overlay
  h down
  row 4
  /* centered */
  column 16
.

run seedGrid( session:parameter ).

/* iterate generations - q to quit, p to pause
 */

do while lastkey <> asc("q"):

  run showGrid.
  z = max( y, z ).					/* track max population				*/

  put screen row screen-lines + 2 column 1 substitute( " &1 &2 &3   ", gen, y, z ).

  run nextGen.						/* calculate the next generation		*/
  gen = gen + 1.

  readkey pause 0.					/* increase pause value to slow things down     */

  if lastkey = asc( "p" ) then				/* pause updates until a key is pressed		*/
    readkey.

  if y = 0 and lo > 1 then				/* if there is no randomness then mutations	*/
    do:							/* cannot occur thus y = 0 means every cell	*/
      pause.						/* is dead and we are done			*/	
      leave.
    end.

end.

hide frame gs.

readkey pause 0.

return.

/* end of main block
 */


/* initialize the grid by reading a seed file, if there is no seed file create something fun
 */

procedure seedGrid:

  define input parameter seedFileName as character no-undo.

  define variable i as integer no-undo.
  define variable j as integer no-undo.

  /* create a blank grid
   */

  empty temp-table grid.

  do i = 1 to h:
    create grid.
    assign
      grid.r = i
      grid.a = 0
      grid.b = 0
    .
  end.

  /*  if there is no seed file create something fun
   */

  file-info:file-name = seedFileName.
  if file-info:full-pathname = ? then
    do:

      case random( 1, 5 ):

        when 1 then
          do:
            find grid where grid.r = 20.
            do j = 16 to 112:
              grid.a[j] = 1.
            end.
          end.

        when 2 then
          do:
            for each grid:
              if grid.r > 5 and grid.r < 35 then grid.a[64] = 1.
            end.
          end.

        when 3 then
          do:
            for each grid:
              if grid.r > 5 and grid.r < 35 then
                assign
                  grid.a[43 + grid.r] = 1
                  grid.a[44 + grid.r] = 1
                  grid.a[83 - grid.r] = 1
                  grid.a[84 - grid.r] = 1
                .
            end.
          end.

        when 4 then
          do:
            find grid where grid.r = 20.
            do j = 16 to 112:
              grid.a[j] = 1.
            end.
            for each grid:
              if grid.r > 5 and grid.r < 35 then grid.a[64] = 1.
            end.
          end.

        when 5 then
          do:
            find grid where grid.r = 20.
            do j = 16 to 112:
              grid.a[j] = 1.
            end.
            for each grid:
              if grid.r > 5 and grid.r < 35 then grid.a[64] = 1.
            end.
            for each grid:
              if grid.r > 5 and grid.r < 35 then
                assign
                  grid.a[43 + grid.r] = 1
                  grid.a[44 + grid.r] = 1
                  grid.a[83 - grid.r] = 1
                  grid.a[84 - grid.r] = 1
                .
            end.
          end.

      end.

      return.

    end.

  /* load the seed file into the grid
   */

  i = 1.						/* line/row number				*/

  input stream inStrm from value( file-info:full-pathname ).
  do while true:

    find grid where grid.r = i no-error.
    if not available grid then leave.			/* ignore data beyond the defined height	*/

    readkey stream inStrm.
    if lastkey < 0 then leave.				/* end of file					*/

    j = j + 1.						/* column number				*/

    if j > w then next.					/* ignore data beyond the defined width		*/

    if lastkey = 88 or lastkey = 120 then		/* only "X" or "x" count as "live" cells	*/
      grid.a[j] = 1.

    if lastkey = 10 or lastkey = 13 then		/* newline					*/
      do:
        assign
          i = i + 1
          j = 0
        .
        if i > h then next.				/* ignore data beyond the defined height	*/
      end.

  end.
  input stream inStrm close.

  return.

end.


/* check the neighborhood population
 */

function cellStatus returns integer ( r1 as integer extent {&WIDTH}, r2 as integer extent {&WIDTH}, r3 as integer extent {&WIDTH}, c as integer ):

  define variable cl as integer no-undo.	/* neighbor column left		*/
  define variable cr as integer no-undo.	/* neighbor column right	*/

  define variable x  as integer no-undo.	/* current cell			*/
  define variable p  as integer no-undo.	/* population of neighbors	*/

  /* precompute wrapped column numbers
   */

  cl = c - 1.
  if cl < 1 then cl = w.			/* wrap to the end		*/
  cr = c + 1.
  if cr > w then cr = 1.			/* wrap to the beginning	*/

  /* count the neighboring population - but don't count yourself! (r2[c])
   *
   */

  p = r1[cl] +      r1[c]       + r1[cr] +
      r2[cl] +  /** r2[c] **/     r2[cr] +
      r3[cl] +      r3[c]       + r3[cr]
  .

  x = r2[c].					/* state of the current cell	*/

  /* x = 1 = live, x = 0 = dead
   *
   *	Any live cell with fewer than two live neighbours dies, as if by underpopulation.
   *	Any live cell with two or three live neighbours lives on to the next generation.
   *	Any live cell with more than three live neighbours dies, as if by overpopulation.
   *
   *	Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.
   */

  if (( x = 1 ) and ( p = 2 or p = 3 )) or
     (( x = 0 ) and ( p = 3 )) then
    do:
      if random( lo, hi ) = 1 then
        return 0.				/* random mutation!		*/
       else
        return 1.				/* normal behavior		*/
    end.
   else 
    do:
      if random( lo, hi ) = 1 then
        return 1.				/* random mutation!		*/
       else
        return 0.				/* normal behavior		*/
    end.

end.


/* calculate the next generation based on the population density of the current generation
 */

procedure nextGen:

  define variable i as integer no-undo.
  define variable j as integer no-undo.

  define variable nr as integer no-undo.	/* neighbor row				*/

  /* compute the next generation into the scratch rows
   */

  do i = 1 to h:

    nr = i - 1.
    if nr < 1 then nr = h.			/* wrap around to the last row		*/
    find grid where grid.r = nr no-error.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 
&THEN
    r1 = grid.a.
&ELSE
    do j = 1 to w:
      r1[j] = grid.a[j].
    end.
&ENDIF

    nr = i + 1.
    if nr > h then nr = 1.			/* wrap around to the 1st row		*/
    find grid where grid.r = nr no-error.

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 
&THEN
    r3 = grid.a.
&ELSE
    do j = 1 to w:
      r3[j] = grid.a[j].
    end.
&ENDIF

    find grid where grid.r = i.			/* find the current grid line last so	*/
						/* that we can update the current cell	*/

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 
&THEN
    r2 = grid.a.
&ELSE
    do j = 1 to w:
      r2[j] = grid.a[j].
    end.
&ENDIF

    do j = 1 to w:
      grid.b[j] = cellStatus( r1, r2, r3, j ).
    end.

  end.

  /* copy the scratch rows to the live rows
   */

  for each grid:
&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2 
&THEN
    grid.a = grid.b.
&ELSE
    do j = 1 to w:
      grid.a[j] = grid.b[j].
    end.
&ENDIF
  end.

end.


procedure showGrid:

  define variable j as integer no-undo.

  y = 0.					/* current live population		*/

  for each grid:
    grid.s = "".				/* string display of live vs dead	*/
    do j = 1 to w:
      assign
        y = y + grid.a[j]
        grid.s = grid.s + ( if grid.a[j] = 0 then " " else "x" )
      .
    end.
    display /* grid.r */ grid.s with frame gs.
    down with frame gs.
  end.

end.

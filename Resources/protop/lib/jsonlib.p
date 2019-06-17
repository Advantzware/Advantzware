/* jsonlib.p
 *
 * escapeJSON()
 * trimNumStr()
 * splitString()
 * dataset2JSON()
 * JSON2TT()
 *
 * char2hex()
 * hexEncode()
 *
 */

{lib/v9.i}

define new global shared variable dbgMode   as integer no-undo initial 2.
define new global shared variable dsSize    as integer no-undo.

/*** Install self as a session super-procedure
 ***
 ***/

session:add-super-procedure( this-procedure ).

return.


/* characters that need to be escaped in JSON strings
 *
 * \"  Double quote
 * \\  Back slash character (reverse solidus)
 * \/  Forward slash character (solidus)
 *
 * \b  Backspace (ascii code 08)
 * \f  Form feed (ascii code 0C)
 * \n  New line
 * \r  Carriage return
 * \t  Tab
 *
 * \uXXXX where XXXX is 4 hex digits
 *
 */

function escapeJSON returns character ( input str as character ):

  define variable xJSON as character no-undo.

  xJSON = str.

  xJSON = replace( xJSON, '~\', '~\~\' ).

  xJSON = replace( xJSON, '/', '~\/' ).

  xJSON = replace( xJSON, '"', '~\"' ).

  return xJSON.

end.


function trimNumStr returns character ( input xnum as character ):

  define variable t_value as character no-undo.

  if length( xnum ) = 0 then return "0".

  t_value = trim( xnum ).

  t_value = entry( 1, t_value, " " ).

  t_value = trim( t_value, " %BKMGT" ).

  t_value = replace( t_value, session:numeric-separator, "" ).

  if dbgMode >=9 and t_value <> xnum then
    message {&NOW} "trimmed:" xnum "to" t_value.

  return t_value.

end.


/* peel off the 1st "entry" in a string delimited by "d", returning the 1st entry and the remainder of the string
 */

procedure splitString:

  define input-output parameter s as character no-undo.		/* string to be split				*/
  define input-output parameter t as character no-undo.		/* 1st entry					*/
  define input        parameter d as character no-undo.		/* delimiter					*/
  define input        parameter x as integer   no-undo.		/* offset within the delimiter to split at	*/

  if index( s, d ) = 0 then
    assign
      t = s
      s = ""
    .
   else
    assign
      t  = substring( s, 1, index( s, d ))
      s = substring( s, index( s, d ) + x )
    .

  return.

end.


procedure dataSet2JSON:

&IF DEFINED( OE10 ) &THEN
  define input parameter dataset-handle ds.
&ELSE
  define input parameter ds as handle.
&ENDIF

  define output parameter dsJSON as {&LNGCR} no-undo.

  define variable i as integer   no-undo.
  define variable j as integer   no-undo.

  define variable b as handle    no-undo.				/* buffer	*/
  define variable f as handle    no-undo.				/* field	*/
  define variable q as handle    no-undo.				/* query	*/

  define variable v as character no-undo.

  dsJSON = '~{"ProDataSet":~{'.

  create query q.
  q:forward-only = no.

  /* walk through the buffers in the dataset
   */

  dsSize = 0.

&IF DEFINED( OE10 ) &THEN
  do i = 1 to ds:num-buffers:
    b = ds:get-buffer-handle( i ).
&ELSE
  for each tt_dataset:
    b = tt_dataset.ttHandle.
    i = i + 1.
&ENDIF

    q:set-buffers( b ).
    q:query-prepare( "preselect each " + b:name ).
    q:query-open.
    q:get-first().

    if b:available then
   /* dsJSON = substitute( '&1"&2":[', dsJSON, b:name ). */
      dsJSON = dsJSON + substitute( '"&1":[', b:name ).
     else
      next.

    /* each record in a buffer
     */

    do while b:available:

      dsSize = dsSize + b:record-length.

   /* dsJSON = substitute( '&1~{', dsJSON ). */
      dsJSON = dsJSON + '~{'.

      /* each field in the record
       */

      do j = 1 to b:num-fields:

        /* skip array fields (extent > 0 )
         */

        if b:buffer-field( j ):extent <= 1 then
          do:

            if b:buffer-field( j ):data-type = "logical" then
              v = trim( string( b:buffer-field( j ):buffer-value, "true/false" )).
             else if lookup( b:buffer-field( j ):data-type, "integer,decimal,int64" ) = 0 then
              do:
                v = string( b:buffer-field( j ):buffer-value ).
                v = escapeJSON( v /* trim( v ) */ ).
              end.
             else
              do:

                /* v = string( b:buffer-field( j ):buffer-value ). */

                v = string( b:buffer-field( j ):buffer-value, b:buffer-field( j ):format ) no-error.
                if error-status:num-messages > 0 then
                  do:
                    v = string( b:buffer-field( j ):buffer-value ).
                    if dbgMode >= 5 then message {&NOW} "dataSet2JSON()" b:name + "." + b:buffer-field( j ):name "value" v "can not be formatted with mask:" '"' + b:buffer-field( j ):format + '", using unformatted representation instead.'.
                  end.

                v = trimNumStr( v ).

                /* if length( v ) = 0 then
                 *   message "trimmed to naught?!?" b:name b:buffer-field( j ):name b:buffer-field( j ):buffer-value view-as alert-box.
                 */

                v = trim( v ).

                if v = "" then v = "0".

                if b:buffer-field( j ):data-type = "decimal" and index( v, "." ) = 0 then
                  v = trim( v ) + ".0".

              end.

            if v = ? then
              do:
                v = "null".
             /* dsJSON = substitute( '&1&4"&2":&3', dsJSON, b:buffer-field( j ):name, v, ( if j > 1 then "," else "" )). */
                dsJSON = dsJSON + substitute( '&3"&1":&2', b:buffer-field( j ):name, v, ( if j > 1 then "," else "" )).
              end.
             else
              do:
                if lookup( b:buffer-field( j ):data-type, "integer,decimal,int64,logical" ) > 0 then
               /* dsJSON = substitute( '&1&4"&2":&3', dsJSON, b:buffer-field( j ):name, v, ( if j > 1 then "," else "" )). */
                  dsJSON = dsJSON + substitute( '&3"&1":&2', b:buffer-field( j ):name, v, ( if j > 1 then "," else "" )).
                 else
               /* dsJSON = substitute( '&1&4"&2":"&3"', dsJSON, b:buffer-field( j ):name, v, ( if j > 1 then "," else "" )). */
                  dsJSON = dsJSON + substitute( '&3"&1":"&2"', b:buffer-field( j ):name, v, ( if j > 1 then "," else "" )).
              end.

          end.

      end.

      q:get-next().

   /* dsJSON = substitute( '&1}&2', dsJSON, ( if b:available then "," else "" )). */
      dsJSON = dsJSON + substitute( '}&1', ( if b:available then "," else "" )).

    end.

    q:query-close.

 /* dsJSON = substitute( '&1]&2', dsJSON, ( if i = ds:num-buffers then "" else "," )). */

 /* dsJSON = dsJSON + substitute( ']&1', ( if i = ds:num-buffers then "" else "," )). */

    dsJSON = dsJSON + '],'.

  end.

  dsJSON = right-trim( dsJSON, ',' ).

  delete object q.

/*dsJSON = substitute( '&1}}', dsJSON ). */
  dsJSON = dsJSON + '}}'.

  return.

end.


procedure JSON2TT:

  define input parameter jFrag    as character no-undo.
  define input parameter ttHandle as handle    no-undo.

  define variable jRow   as character no-undo.
  define variable jKey   as character no-undo.
  define variable jValue as character no-undo.

  do while jFrag <> "":

    run splitString( input-output jFrag, input-output jRow, '~},~{"', 2 ).

    jRow = trim( jRow, '~{~}' ).

    if dbgMode > 4 then message {&NOW} jRow.

    if jRow <> "" then
      ttHandle:buffer-create().

    do while jRow <> "":

      run splitString( input-output jRow, input-output jKey, ',"', 1 ).

      jKey = trim( jKey, ',' ).

      assign
        jValue = substring( jKey, index( jKey, '":' ) + 2 )		/* no ":" in key names		*/
        jKey   = substring( jKey, 1, index( jKey, '":' ))
      .

      assign
        jKey   = trim( jKey, '"' )
        jValue = trim( jValue, '"' )
      .

      /* un-escape escaped JSON values
       */

      jValue = replace( jValue, '~\/',  '/' ).
      jValue = replace( jValue, '~\~\', '~\' ).
      jValue = replace( jValue, '~\"',  '"' ).

      if jValue = "null" then jValue = ?.

      ttHandle:buffer-field( jKey ):buffer-value = jValue no-error.

      if dbgMode >= 5 then message {&NOW} jKey " = " jValue.

    end.

  end.

  return.

end.


/* convert a character to its corresponding 2 digit hex string
 *
 */

define variable hex_chars as character no-undo initial "0123456789ABCDEF".

function char2hex returns character ( input c as character ):

  if length( c ) <> 1 then
    return "00".
   else
    return (
      substring( hex_chars, integer( truncate( asc( c ) / 16, 0 )) + 1, 1 ) +
      substring( hex_chars, ( asc( c ) modulo 16 ) + 1, 1 )
    ).

end.


function hexEncode returns character ( input src as character ):

  define variable xsrc as character no-undo.

  define variable c as character no-undo.
  define variable i as integer   no-undo.
  define variable n as integer   no-undo.
  define variable x as integer   no-undo.

  n = length( src ).

  do i = 1 to n:

    assign
      c = substring( src, i, 1 )
      x = asc( c )
    .

    if  ( x <= 47 ) or                                                  /* anything less than "0"                       */
       (( x >= 58 ) and ( x <= 64 )) or                                 /* anything between "9" and "A"                 */
       (( x >= 91 ) and ( x <= 96 )) or                                 /* anything between "Z" and "a"                 */
        ( x >= 123 ) then                                               /* anything greater than "z"                    */
      xsrc = xsrc + "%" + char2hex( c ).
     else
      xsrc = xsrc + c.

  end.

  return xsrc.

end.


/***

/* encode non-alphanumeric characters
 *
 */

function hexEncode returns character ( input src as character ):

  define variable xtxt as character no-undo.

  assign


    xtxt = replace(  src, "%",   "%25" )	/* "%" has to go first...	*/

    xtxt = replace( xtxt, "~n",  "%0A" )

    xtxt = replace( xtxt, " ",   "%20" )

    xtxt = replace( xtxt, '!',   "%21" )
    xtxt = replace( xtxt, '"',   "%22" )
    xtxt = replace( xtxt, '#',   "%23" )
    xtxt = replace( xtxt, "$",   "%24" )
    xtxt = replace( xtxt, "&",   "%26" )
    xtxt = replace( xtxt, "'",   "%27" )
    xtxt = replace( xtxt, "(",   "%28" )
    xtxt = replace( xtxt, ")",   "%29" )
    xtxt = replace( xtxt, "*",   "%2A" )
    xtxt = replace( xtxt, "+",   "%2B" )
    xtxt = replace( xtxt, ",",   "%2C" )
    xtxt = replace( xtxt, "-",   "%2D" )
    xtxt = replace( xtxt, ".",   "%2E" )

    xtxt = replace( xtxt, ":",   "%3A" )
    xtxt = replace( xtxt, ";",   "%3B" )
    xtxt = replace( xtxt, "<",   "%3C" )
    xtxt = replace( xtxt, "=",   "%3D" )
    xtxt = replace( xtxt, ">",   "%3E" )
    xtxt = replace( xtxt, "?",   "%3F" )

    xtxt = replace( xtxt, "@",   "%40" )

    xtxt = replace( xtxt, "[",   "%5B" )
    xtxt = replace( xtxt, "~\",  "%5C" )
    xtxt = replace( xtxt, "]",   "%5D" )
    xtxt = replace( xtxt, "^",   "%5E" )
    xtxt = replace( xtxt, "_",   "%5F" )

    xtxt = replace( xtxt, "`",   "%60" )

    xtxt = replace( xtxt, "~{",  "%7B" )
    xtxt = replace( xtxt, "|",   "%7C" )
    xtxt = replace( xtxt, "~}",  "%7D" )
    xtxt = replace( xtxt, "~~",  "%7E" )
    xtxt = replace( xtxt, chr(127), "%7F" )

  .

  return xtxt.

end.

 ***/

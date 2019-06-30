/* lib/dumpTT.i
 *
 * simplified to a single line -- include should be eliminated.
 *
 */

&IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.1 &THEN
/*** +++
file-info:file-name = "./ptdefs".
if file-info:full-pathname <> ? and index( file-info:file-type, "d" ) > 0 and index( file-info:file-type, "w" ) > 0 then
  do:

    file-info:file-name = "./ptdefs/{1}.xsd".

    if true /* file-info:full-pathname = ? or file-info:file-size = 0 */ then
      do:

        temp-table {1}:write-xmlschema(
          "file",
/*        substitute( "ptdefs/&1.xsd", "{1}" ), */
/*        "./ptdefs/{1}.xsd", */
          ( if opsys = "unix" then "./ptdefs/{1}.xsd" else ".~\ptdefs~\{1}.xsd" ),
          true

      &IF DECIMAL(SUBSTRING(PROVERSION,1,INDEX(PROVERSION,".") + 1)) >= 10.2
      &THEN
          , ?, ?, ?
      &ENDIF

    ).

    end.

  end.
 +++ ***/
&ENDIF

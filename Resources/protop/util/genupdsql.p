/* genupdsql.p
 *
 * mpro dbName -p lib/genupdsql.p
 *
 * sqlexp -user userName -password passWord -db dnName -S servicePort -infile dbname.updstats.sql -outfile updSQLtats.log
 *
 */

define new global shared variable pt_tmpdir      as character no-undo initial ".".
define new global shared variable pt_shortname   as character no-undo.

define variable script as character no-undo.

script = substitute( "&1/&2.updstats.sql", pt_tmpdir, max( pt_shortname, ldbname(1))).

output to value( script ).

for each _file no-lock where _hidden = no:

  put unformatted
     "UPDATE TABLE STATISTICS AND INDEX STATISTICS AND ALL COLUMN STATISTICS FOR PUB."
     '"' _file._file-name '"' ";"
    skip
  .
  put unformatted "commit work;" skip.

end.

output close.

message
    skip(1)
    string( "Output is in:", "x(76)" ) skip
    string( "  " + script,   "x(76)" ) skip
    string( "To update statistics run: ", "x(76)" )  skip
    string( "  proenv> sqlexp -user userName -password passWord -db dnName -S servicePort -infile dbname.updstats.sql -outfile updSQLstats.log", "x(140)" ) skip
    skip(1)
  view-as alert-box information title " Update SQL Statistics "
.

return.

/* genUpdateSQL.p
 *
 * mpro dbName -p util/genUpdateSQL.p -param "tmp/updSQLstats.sql"
 *
 * sqlexp -user userName -password passWord -db dnName -S servicePort -infile tmp/updSQLstats.sql -outfile tmp/updSQLtats.log
 *
 */

output to value( ( if session:parameter <> "" then session:parameter else "updSQLstats.sql" )).

for each _file no-lock where _hidden = no:

  put unformatted
     "UPDATE TABLE STATISTICS AND INDEX STATISTICS AND ALL COLUMN STATISTICS FOR PUB."
     '"' _file._file-name '"' ";"
    skip
  .
  put unformatted "commit work;" skip.

end.

output close.

return.

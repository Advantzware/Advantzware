/* lib/dba.i
 *
 * temp tables used by the dbanalys parser
 *
 */

define new global shared temp-table tblist no-undo
  field order   as integer   format "->>9" initial -1
  field tid     as integer
  field tbl     as character format "x(30)"
  field ar      as character format "x(30)"
  field recs    as decimal   format ">>>>>>>>>>>9"
  field tsz     as decimal   format ">>>>>>>>>>>9"
  field isz     as decimal   format ">>>>>>>>>>>9"
  field trd     as decimal   format ">>>>>>>>>>>9"
  field tupd    as decimal   format ">>>>>>>>>9"
  field frag    as decimal   format ">>>9.9"
  field scat    as decimal   format ">9.9"
  field b2      as logical
  field rm      as decimal   format ">>>>>>>>>>>9"
  field pctfrag as decimal   format ">>>9.99"
  field avgrec  as decimal   format ">>>>9.99"
  index idxTbl is unique tbl
.

define new global shared temp-table ixlist no-undo
  field order as integer   format "->>9" initial -1
  field iid   as integer
  field idx   as character format "x(30)"
  field ar    as character format "x(30)"
  field blks  as decimal   format ">>>>>>>>>>>9"
  field pctut as decimal   format ">>>>>>>>>>>9"
  field lvls  as decimal   format ">9"
  index idxidx is unique idx
.

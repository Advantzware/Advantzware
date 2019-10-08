/***************************************************************************\
*****************************************************************************
**  Program: e:\asi\patch\edi\ed\EXPSONO
**       By:
** Descript:
**
*****************************************************************************
\***************************************************************************/
DEF STREAM s-export.
DEF INPUT PARAM p_action    AS char     NO-UNDO.    /* write,delete,create */
DEF INPUT PARAM p_cust      AS RECID    NO-UNDO.
{ed/tdf/sharedv.i   "new shared"}
IF p_action = 'create' THEN
RETURN.
FIND cust WHERE RECID(cust) = p_cust NO-LOCK NO-ERROR.
IF NOT AVAIL cust THEN
DO:
  RUN rc/debugmsg.p ('Could NOT FIND customer WITH RECID:
  '
  + string(p_cust) ).
  RETURN error.
END.
DEF var filler AS char NO-UNDO initial ''.
DEF VAR export_fid      AS char NO-UNDO     initial "SONO_CST.TXT".
FIND stax-group
  WHERE stax-group.company = cust.company
  AND   stax-group.tax-group = cust.tax-gr NO-LOCK NO-ERROR.
{rc/stringv.i}
str_buffa = "".
ASSIGN
  {rc/outstr.i    cust.company        1   5}  /* s_system_id */
  {rc/outstr.i    cust.cust-no        6   15}
  {rc/outstr.i    substring(p_action,1,1) 21  1}  /* action_flag   */
  {rc/outstr.i    string(TODAY)       22  8}
  {rc/outstr.i    string(TIME,'HH:MM:SSSS') 30  8}
  {rc/outstr.i    FILLER              38  10} /* filler */
  {rc/outstr.i    string(0,'999999')  48  10} /* where  from??? */
  {rc/outstr.i    FILLER              54  01} /* update status??? */
  {rc/outstr.i    cust.name           55  40}
  {rc/outstr.i    FILLER              95  40} /* name 2 */
  {rc/outstr.i    FILLER              135 40} /* name 3 */
  {rc/outstr.i    string(TODAY)       175 10} /* s_addr_effdt_1 */
  {rc/outstr.i    FILLER              185 40} /* s addr descr */
  {rc/outstr.i    cust.addr[1]        225 35}
  {rc/outstr.i    cust.addr[2]        260 35}
  {rc/outstr.i    FILLER              295 35} /* s addr 3 */
  {rc/outstr.i    FILLER              330 35} /* s addr 4 */
  {rc/outstr.i    cust.city           365 35}
  {rc/outstr.i    FILLER              400 1}  /* in city limit? */
  {rc/outstr.i    FILLER              401 30} /* county */
  {rc/outstr.i    cust.state          431 4}
  {rc/outstr.i    cust.terr           435 11} /* geo code */
  {rc/outstr.i    cust.zip            446 12}
  {rc/outstr.i    cust.country        458 3}
  {rc/outstr.i    "cust.area-code + cust.phone" 461 24}
  {rc/outstr.i    FILLER              485 6}      /* extention */
  {rc/outstr.i    cust.fax            491 24}
  {rc/outstr.i    FILLER              515 1035}    /* addr 2 3 4 */
  .
ASSIGN
  {rc/outstr.i    FILLER              1550 8}     /* credit analyst */
  {rc/outstr.i    cust.sman           1558 8}
  {rc/outstr.i    FILLER              1566 8}     /* collector */
  {rc/outstr.i    FILLER              1574 9}     /* db number */
  {rc/outstr.i    cust.cr-use         1583 15}
  {rc/outstr.i    FILLER              1598 15}    /* remit from cust id */
  {rc/outstr.i    cust.type           1613 5}     /* sic code */
  {rc/outstr.i    FILLER              1618 5}     /* cur rt  type */
  {rc/outstr.i    FILLER              1623 3}     /* currency code */
  {rc/outstr.i    FILLER              1626 1}     /* vat cust edit */
  {rc/outstr.i    FILLER              1627 8}     /* billing specialist */
  {rc/outstr.i    FILLER              1635 12}    /* bill inquiry phone */
  .
ASSIGN
  {rc/outstr.i    string(TODAY)       1647 10}    /* since date */
  {rc/outstr.i    FILLER              1657 3}     /* language code */
  {rc/outstr.i    cust.tax-gr         1660 8}     /* tax cd */
  {rc/outstr.i    "if avail stax-group then stax-group.tax-dscr else '' "
                                      1668 30}    /* tax jd name */
  {rc/outstr.i    FILLER              1698 10}    /* s effdt 2 */
  {rc/outstr.i    cust.contact        1708 40}
  {rc/outstr.i    FILLER              1748 35}    /* title */
  {rc/outstr.i    FILLER              1783 3}     /* lang code 1 */
  {rc/outstr.i    FILLER              1786 70}    /* email ID */
  {rc/outstr.i    FILLER              1856 1}     /* comm method */
  {rc/outstr.i    FILLER              1857 10}    /* effdt 3 */
  .
ASSIGN
  {rc/outstr.i    string(cust.cr-lim,'-9999999999999999') 
                                      1867 17}
  {rc/outstr.i    string(0,'999')     1884 3}     /* credit lim range */
  {rc/outstr.i    string(TODAY,'99/99/9999') 
                                      1887 10}
  {rc/outstr.i    STRING(0,'-9999999999999999') 
                                      1897 17}
  {rc/outstr.i    string(0,'999')     1914 3}     /* credit lim corp range */
  {rc/outstr.i    string(TODAY,'99/99/9999') 
                                      1917 10} /* corp date */
  {rc/outstr.i    cust.terms          1927 5}
  {rc/outstr.i    FILLER              1932 1}     /* po required */
  .
DEF var retry_counter AS int  NO-UNDO.
_open:
DO ON error UNDO, RETRY:
  error-status:error = FALSE.
  OUTPUT STREAM s-export TO VALUE(export_fid) append.
  IF OS-ERROR > 0 THEN
  DO:
    HIDE MESSAGE NO-PAUSE.
    MESSAGE error-status:get-message(OS-ERROR).
    PAUSE.
    retry_counter = retry_counter + 1.
    IF retry_counter > 100 THEN
    DO:
      OUTPUT STREAM s-export close.
      RETURN error (string(OS-ERROR)).
    END.
    UNDO _open, RETRY _open.
  END.    /* error handling */
END.    /* _open block */
PUT STREAM s-export UNFORMATTED str_buffa SKIP.
OUTPUT STREAM s-export close.

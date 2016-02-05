/* cust-dis.i copied from methods/dispflds/cust.i   */
&scoped-define FIRST-EXTERNAL-TABLE cust
&IF '{&CUSTOMER-TOTALS}' = '' &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.carrier" THEN
  DO:
/*    {custom/getcmpny.i} */
    FIND carrier
        WHERE carrier.company = gcompany
          AND carrier.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
          AND carrier.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carrier_dscr = IF NOT AVAILABLE carrier THEN ""
                   ELSE carrier.dscr.
    DISPLAY carrier_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.type" THEN
  DO:
  /*  {custom/getcmpny.i} */
    FIND custype
        WHERE custype.company = gcompany
          AND custype.custype = {&FIRST-EXTERNAL-TABLE}.type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    custype_dscr = IF NOT AVAILABLE custype THEN ""
                   ELSE custype.dscr.
    DISPLAY custype_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.del-zone" THEN
  DO:
/*    {custom/getcmpny.i} */
    FIND carr-mtx
        WHERE carr-mtx.company = gcompany
          AND carr-mtx.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
          AND carr-mtx.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
          AND carr-mtx.del-zone = {&FIRST-EXTERNAL-TABLE}.del-zone:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carr-mtx_del-dscr = IF NOT AVAILABLE carr-mtx THEN ""
                        ELSE carr-mtx.del-dscr.
    DISPLAY carr-mtx_del-dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.loc" THEN
  DO:
  /*  {custom/getcmpny.i} */
    FIND loc
        WHERE loc.company = gcompany
          AND loc.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    loc_dscr = IF NOT AVAILABLE loc THEN ""
               ELSE loc.dscr.
    DISPLAY loc_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.sman" THEN
  DO:
/*    {custom/getcmpny.i} */
    FIND sman
        WHERE sman.company = gcompany
          AND sman.sman = {&FIRST-EXTERNAL-TABLE}.sman:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    sman_sname = IF NOT AVAILABLE sman THEN ""
                 ELSE sman.sname.
    DISPLAY sman_sname.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.tax-gr" THEN
  DO:
    FIND stax
        WHERE stax.tax-group = {&FIRST-EXTERNAL-TABLE}.tax-gr:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    stax_tax-dscr = IF NOT AVAILABLE stax THEN ""
                    ELSE stax.tax-dscr[1].
    DISPLAY stax_tax-dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.terms" THEN
  DO:
  /*  {custom/getcmpny.i} */
    FIND terms
        WHERE terms.company = gcompany
          AND terms.t-code = {&FIRST-EXTERNAL-TABLE}.terms:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    terms_dscr = IF NOT AVAILABLE terms THEN ""
                 ELSE terms.dscr.
    DISPLAY terms_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.terr" THEN
  DO:
/*    {custom/getcmpny.i} */
    FIND terr
        WHERE terr.company = gcompany
          AND terr.terr = {&FIRST-EXTERNAL-TABLE}.terr:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    terr_dscr = IF NOT AVAILABLE terr THEN ""
                ELSE terr.dscr.
    DISPLAY terr_dscr.
  END.
&ELSE
  WHEN "ptd-sales" OR
  WHEN "{&FIRST-EXTERNAL-TABLE}.ytd-sales" OR
  WHEN "{&FIRST-EXTERNAL-TABLE}.lyr-sales" OR
  WHEN "total-msf" OR
  WHEN "{&FIRST-EXTERNAL-TABLE}.cost[5]" OR
  WHEN "{&FIRST-EXTERNAL-TABLE}.cost[6]" THEN
  DO:
    {custom/cust-tot.i}
  END.
&ENDIF

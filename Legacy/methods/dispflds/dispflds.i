/* dispflds.i */

&IF "{&FIRST-EXTERNAL-TABLE}" = "address" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.zipcode" THEN
  DO:
    FIND zipcode
        WHERE zipcode.zipcode = {&FIRST-EXTERNAL-TABLE}.zipcode:SCREEN-VALUE
          AND zipcode.pref_type = {&FIRST-EXTERNAL-TABLE}.pref_type:SCREEN-VALUE
          AND zipcode.pref# = INTEGER({&FIRST-EXTERNAL-TABLE}.pref#:SCREEN-VALUE)
        NO-LOCK NO-ERROR.
    ASSIGN
      zipcode_city = IF NOT AVAILABLE zipcode THEN ""
                     ELSE zipcode.city
      zipcode_country = IF NOT AVAILABLE zipcode THEN ""
                        ELSE zipcode.country
      zipcode_state = IF NOT AVAILABLE zipcode THEN ""
                      ELSE zipcode.state.
    DISPLAY zipcode_city zipcode_country zipcode_state.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "bank" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.actnum" THEN
  DO:
    {custom/getcmpny.i}
    FIND account
        WHERE account.company = gcompany
          AND account.actnum = {&FIRST-EXTERNAL-TABLE}.actnum:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    account_dscr = IF NOT AVAILABLE account THEN ""
                   ELSE account.dscr.
    DISPLAY account_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "carrier" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.loc" THEN
  DO:
    {custom/getcmpny.i}
    FIND loc
        WHERE loc.company = gcompany
          AND loc.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    loc_dscr = IF NOT AVAILABLE loc THEN ""
               ELSE loc.dscr.
    DISPLAY loc_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "cust" &THEN
&IF '{&CUSTOMER-TOTALS}' = '' &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.carrier" THEN
  DO:
    {custom/getcmpny.i}
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
    {custom/getcmpny.i}
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
    {custom/getcmpny.i}
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
    {custom/getcmpny.i}
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
    {custom/getcmpny.i}
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
    {custom/getcmpny.i}
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
    {custom/getcmpny.i}
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
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "fgcat" &THEN
  WHEN "cat-format" THEN
  DO:
    IF AVAILABLE fgcat THEN
    cat-format = IF fgcat.commrate = 1 THEN yes ELSE no.
    DISPLAY cat-format WITH FRAME {&FRAME-NAME}.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "item" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.cost-type" THEN
  DO:
    {custom/getcmpny.i}
    {custom/getloc.i}
    FIND costtype
        WHERE costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    costtype_descr = IF NOT AVAILABLE costtype THEN ""
                     ELSE costtype.descr.
    DISPLAY costtype_descr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.mat-type" THEN
  DO:
    FIND mat
        WHERE mat.mat = {&FIRST-EXTERNAL-TABLE}.mat-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    mat_dscr = IF NOT AVAILABLE mat THEN ""
               ELSE mat.dscr.
    DISPLAY mat_dscr.
    {custom/mattypes.i}
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.procat" THEN
  DO:
    {custom/getcmpny.i}
    FIND procat
        WHERE procat.company = gcompany
          AND procat.procat = {&FIRST-EXTERNAL-TABLE}.procat:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    procat_dscr = IF NOT AVAILABLE procat THEN ""
                  ELSE procat.dscr.
    DISPLAY procat_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "prep" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.cost-type" THEN
  DO:
    {custom/getcmpny.i}
    {custom/getloc.i}
    FIND costtype
        WHERE costtype.company = gcompany
          AND costtype.loc = gloc
          AND costtype.cost-type = {&FIRST-EXTERNAL-TABLE}.cost-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    costtype_descr = IF NOT AVAILABLE costtype THEN ""
                     ELSE costtype.descr.
    DISPLAY costtype_descr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.mat-type" THEN
  DO:
    FIND mat
        WHERE mat.mat = {&FIRST-EXTERNAL-TABLE}.mat-type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    mat_dscr = IF NOT AVAILABLE mat THEN ""
               ELSE mat.dscr.
    DISPLAY mat_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.uom" THEN
  DO:
    FIND uom WHERE uom.uom = {&FIRST-EXTERNAL-TABLE}.uom:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    uom_dscr = IF NOT AVAILABLE uom THEN ""
               ELSE uom.dscr.
    DISPLAY uom_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "phone" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.titlcode" THEN
  DO:
    FIND titlcode
        WHERE titlcode.titlcode = {&FIRST-EXTERNAL-TABLE}.titlcode:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    titlcode_description = IF NOT AVAILABLE titlcode THEN ""
                           ELSE titlcode.description.
    DISPLAY titlcode_description.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "prodl" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.procat" THEN
  DO:
    {custom/getcmpny.i}
    FIND procat
        WHERE procat.company = gcompany
          AND procat.procat = {&FIRST-EXTERNAL-TABLE}.procat:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    procat_dscr = IF NOT AVAILABLE procat THEN ""
                  ELSE procat.dscr.
    DISPLAY procat_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "shipto" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.carrier" THEN
  DO:
    {custom/getcmpny.i}
    FIND carrier
        WHERE carrier.company = gcompany
          AND carrier.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
          AND carrier.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carrier_dscr = IF NOT AVAILABLE carrier THEN ""
                   ELSE carrier.dscr.
    DISPLAY carrier_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.dest-code" THEN
  DO:
    {custom/getcmpny.i}
    FIND carr-mtx
        WHERE carr-mtx.company = gcompany
          AND carr-mtx.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
          AND carr-mtx.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
          AND carr-mtx.del-zone = {&FIRST-EXTERNAL-TABLE}.dest-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carr-mtx_del-dscr = IF NOT AVAILABLE carr-mtx THEN ""
                        ELSE carr-mtx.del-dscr.
    DISPLAY carr-mtx_del-dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.loc" THEN
  DO:
    {custom/getcmpny.i}
    FIND loc
        WHERE loc.company = gcompany
          AND loc.loc = {&FIRST-EXTERNAL-TABLE}.loc:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    loc_dscr = IF NOT AVAILABLE loc THEN ""
               ELSE loc.dscr.
    DISPLAY loc_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.tax-code" THEN
  DO:
    FIND stax
        WHERE stax.tax-group = {&FIRST-EXTERNAL-TABLE}.tax-code:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    stax_tax-dscr = IF NOT AVAILABLE stax THEN ""
                    ELSE stax.tax-dscr[1].
    DISPLAY stax_tax-dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "sman" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.territory" THEN
  DO:
    {custom/getcmpny.i}
    FIND terr
        WHERE terr.company = gcompany
          AND terr.terr = {&FIRST-EXTERNAL-TABLE}.territory:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    terr_dscr = IF NOT AVAILABLE terr THEN ""
                ELSE terr.dscr.
    DISPLAY terr_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "vend" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.buyer" THEN
  DO:
    {custom/getcmpny.i}
    FIND buyer
        WHERE buyer.company = gcompany
          AND buyer.buyer = {&FIRST-EXTERNAL-TABLE}.buyer:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    buyer_buyer-n = IF NOT AVAILABLE buyer THEN ""
                ELSE buyer.buyer-n.
    DISPLAY buyer_buyer-n.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.carrier" THEN
  DO:
    {custom/getcmpny.i}
    FIND carrier
        WHERE carrier.company = gcompany
          AND carrier.carrier = {&FIRST-EXTERNAL-TABLE}.carrier:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    carrier_dscr = IF NOT AVAILABLE carrier THEN ""
                   ELSE carrier.dscr.
    DISPLAY carrier_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.terms" THEN
  DO:
    {custom/getcmpny.i}
    FIND terms
        WHERE terms.company = gcompany
          AND terms.t-code = {&FIRST-EXTERNAL-TABLE}.terms:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    terms_dscr = IF NOT AVAILABLE terms THEN ""
                 ELSE terms.dscr.
    DISPLAY terms_dscr.
  END.
  WHEN "{&FIRST-EXTERNAL-TABLE}.type" THEN
  DO:
    {custom/getcmpny.i}
    FIND ventype
        WHERE ventype.company = gcompany
          AND ventype.type = {&FIRST-EXTERNAL-TABLE}.type:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    ventype_dscr = IF NOT AVAILABLE ventype THEN ""
                   ELSE ventype.dscr.
    DISPLAY ventype_dscr.
  END.
&ELSEIF "{&FIRST-EXTERNAL-TABLE}" = "zipcode" &THEN
  WHEN "{&FIRST-EXTERNAL-TABLE}.state" THEN
  DO:
    FIND statecod
        WHERE statecod.statecod = {&FIRST-EXTERNAL-TABLE}.state:SCREEN-VALUE
        NO-LOCK NO-ERROR.
    statecod_description = IF NOT AVAILABLE statecod THEN ""
                           ELSE statecod.description.
    DISPLAY statecod_description.
  END.
&ENDIF

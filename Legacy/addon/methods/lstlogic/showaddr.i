/* showaddr.i */

/*
&db_table = DATABASE.table-name
&col = column where address frame begins
&frame-name = frame name for address

NOTE: this include uses the logical variable 'show-addresses'
*/

IF show-addresses THEN
DO:
  IF CAN-FIND(FIRST address
      WHERE address.table_rec_key = {&db_table}.rec_key) THEN
  PUT UNFORMATTED "** ADDRESS ******************************" AT {&col} SKIP.
  FOR EACH address
      WHERE address.table_rec_key = {&db_table}.rec_key NO-LOCK
      WITH FRAME {&frame-name} WIDTH 132 COLUMN {&col} NO-BOX STREAM-IO:
    DISPLAY
      address.name1 COLUMN-LABEL "Name"
      address.address1 COLUMN-LABEL "Address"
      address.address_type
      address.pref#
      address.pref_type.
    IF address.name2 NE "" THEN
    DISPLAY address.name2 AT 32
        WITH FRAME {&frame-name}1 COL {&col} NO-LABELS NO-BOX STREAM-IO.
    IF address.name2 NE "" THEN
    DISPLAY address.address2 AT 32
        WITH FRAME {&frame-name}2 COL {&col} NO-LABELS NO-BOX STREAM-IO.
    DISPLAY CityState(address.zipcode) + " " + address.zipcode +
     (IF address.zipcode_4 = 0 THEN "" ELSE "-" + STRING(address.zipcode_4,"9999"))
      FORMAT "x(50)" AT 32
        WITH FRAME {&frame-name}3 WIDTH 132 COL {&col} NO-LABELS NO-BOX STREAM-IO.
    {methods/lstlogic/showphon.i &db_table="address" &col="10" &frame-name="f-addrphones{&funique}"}
  END.
END.

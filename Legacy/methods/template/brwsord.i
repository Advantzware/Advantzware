/* brwsord.i */

&IF "{&DESCRIP{1}}" NE "" &THEN
browse-order:RADIO-BUTTONS =
    IF browse-order:RADIO-BUTTONS BEGINS "N/A" THEN "~&{1} {&DESCRIP{1}}, {1}"
    ELSE browse-order:RADIO-BUTTONS + ",~&{1} {&DESCRIP{1}}, {1}".
&ENDIF

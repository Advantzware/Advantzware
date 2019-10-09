
    CASE cust-markup.markup-on[{1}]:
      WHEN "N" THEN cb_markup-on-{1}:SCREEN-VALUE = "(N)et".
      WHEN "G" THEN cb_markup-on-{1}:SCREEN-VALUE = "(G)ross".
      WHEN "S" THEN cb_markup-on-{1}:SCREEN-VALUE = "(S)quare Feet".
      WHEN "B" THEN cb_markup-on-{1}:SCREEN-VALUE = "(B)oard".
    END CASE.

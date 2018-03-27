/* oerep/r-booko#N1.i */

WHEN "v-qty-lft" THEN cVarValue = string(v-qty-lft,"->>>,>>>,>>9").
WHEN "v-ext-price" THEN cVarValue = string(v-ext-price,"->,>>>,>>9.99").
WHEN "v-margin" THEN cVarValue = STRING(v-margin,"->,>>>,>>9.99").
WHEN "v-orderedMsf" THEN cVarValue = string(v-OrderedMsf,">>>9.99").
WHEN "v-jobShipQty" THEN cVarValue = string(v-jobShipQty,"->>,>>>,>>9").
WHEN "v-boardProfit" THEN cVarValue = string(v-boardProfit,">,>>>,>>9.99").
WHEN "v-boardPO" THEN cVarValue = string(v-boardPO,">>>>>>>>9").
WHEN "v-boardpoQty" THEN cVarValue = string(v-boardpoQty,"->>,>>>,>>9").
WHEN "v-boardCost" THEN cVarValue = string(v-boardCost,">>>>,>>9.99").
WHEN "v-boardTotalCost" THEN cVarValue = string(v-boardTotalCost,"->,>>>,>>9.99").
WHEN "v-boardTotalQty" THEN cVarValue = string(v-boardTotalQty,"->>,>>>,>>9").
WHEN "v-Order%Profit" THEN cVarValue = string(v-Order%Profit * 100,"->>>>>,>>>99%").
WHEN "v-MSFRec" THEN cVarValue = string(v-MSFRec,"->,>>9.99").
WHEN "v-FGShipDate" THEN cVarValue = IF v-FGShipDate <> ? THEN string(v-FGShipDate) ELSE "". /*"x(12)" */
WHEN "v-PORecDate" THEN cVarValue = IF v-PORecDate <> ? THEN string(v-PORecDate) ELSE "".  /*"x(11)" */
WHEN "v-FGExtPrice" THEN cVarValue = string(v-FGExtPrice,"->>>>,>>9.99").
WHEN "v-PORecCost" THEN cVarValue = string(v-PORecCost,"->>>,>>9.99").
WHEN "v-ProfitSold$" THEN cVarValue = string(v-ProfitSold$,"->>>>>,>>9.99").
WHEN "v-ProfitSold%" THEN cVarValue = string(v-ProfitSold% * 100,"->>>>>,>>>99%").
WHEN "v-UnitsBoard" THEN cVarValue = string(v-UnitsBoard,"->>>>>>,>>9").
WHEN "v-UnitLoss$" THEN cVarValue = string(v-UnitLoss$,"->>>>,>>9.99").
WHEN "v-Loss%" THEN cVarValue = string(v-Loss% * 100,">>9.99%").
WHEN "v-bol#" THEN cVarValue = string(v-bol#,">>>>>>>>").
WHEN "v-Inv#" THEN cVarValue = string(v-Inv#,">>>>>>").

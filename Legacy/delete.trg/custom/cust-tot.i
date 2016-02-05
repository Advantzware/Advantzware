/* cust-tot.i */

ASSIGN
  ptd-profit = DECIMAL(ptd-sales:SCREEN-VALUE) - DECIMAL(cust.cost[1]:SCREEN-VALUE)
  ytd-profit = DECIMAL(ytd-sales:SCREEN-VALUE) - DECIMAL(cust.cost[5]:SCREEN-VALUE)
  lyr-profit = DECIMAL(lyr-sales:SCREEN-VALUE) - DECIMAL(cust.cost[6]:SCREEN-VALUE)
  ptd-profit-pct = ptd-profit / DECIMAL(ptd-sales:SCREEN-VALUE) * 100
  ytd-profit-pct = ytd-profit / DECIMAL(ytd-sales:SCREEN-VALUE) * 100
  lyr-profit-pct = lyr-profit / DECIMAL(lyr-sales:SCREEN-VALUE) * 100.
DISPLAY ptd-profit ptd-profit-pct
        ytd-profit ytd-profit-pct
        lyr-profit lyr-profit-pct.

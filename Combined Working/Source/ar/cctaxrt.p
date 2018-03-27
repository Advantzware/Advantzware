/***************************************************************************\
*****************************************************************************
**  Program: ar/cct
**       By: Chris Heins
** Descript: Given company and jurisdiction, return sales tax rates:
**  First rate is for mdse, second is for freight.
Usage:
run ar/cctaxrt.p (
    input   cocode,
            oe-ord.tax-gr,
    output  v-tax-rate,
    output  v-frt-tax-rate).
*****************************************************************************
\***************************************************************************/

def input param ws_company      as char no-undo.
def input param ws_group        as char no-undo.
def output param v-tax-rate     as dec no-undo.
def output param v-frt-tax-rate as dec no-undo.

def var ws_rate  as dec extent 2 no-undo decimals 10.
def var v-rate   as dec          no-undo decimals 10.

def var i as int no-undo.
def var j as int no-undo.
def var k as int no-undo.


FIND FIRST stax
    WHERE stax.company   EQ ws_company
      AND stax.tax-group EQ ws_group
    NO-LOCK NO-ERROR.
            
if avail stax then
do j = 1 to 2:
  do i = 1 to extent(stax.tax-code1):
    ws_rate[j] = ws_rate[j] +
                 if (j eq 1 or stax.tax-frt1[i]) then stax.tax-rate1[i] else 0.
                 
    if stax.accum-tax and i gt 1 then do:
      v-rate = stax.tax-rate1[i].
      
      do k = 1 to i - 1:
        v-rate = v-rate + (v-rate * (stax.tax-rate1[k] / 100)).
      end.
      
      ws_rate[j] = ws_rate[j] + v-rate - stax.tax-rate1[i].
    end.               
  end.
end.

assign
 v-tax-rate     = ws_rate[1]
 v-frt-tax-rate = ws_rate[2].

return.


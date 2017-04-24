/* decfrac2.p  copied from dec-frac.p and relpaced "-" with " " */

def input  parameter v-num as   dec.
def input  parameter v-bot as   int.
def output parameter v-str as   char.

def var v-dec as   dec.
def var v-top as   dec.


assign
 v-dec = v-num - trunc(v-num,0)
 v-num = trunc(v-num,0)
 v-top = v-dec / (1 / v-bot).

if v-top gt 0 then do:
  {sys/inc/roundup.i v-top}

  do while true:
    if v-top modulo 2 gt 0 then leave.

    assign
     v-top = v-top / 2
     v-bot = v-bot / 2.
  end.
end.

if v-bot eq 1 then
  assign
   v-num = v-num + 1
   v-top = 0.

v-str = trim(string(trunc(v-num,0),">>>>>>")).

if v-top ne 0 then
  v-str = trim(v-str + " " + trim(string(v-top,">9")) +
	       "/" + trim(string(v-bot,">9"))).

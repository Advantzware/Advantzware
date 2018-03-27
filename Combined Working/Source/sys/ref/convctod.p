/* --------------------------------------------- sys/ref/convctod.p 01/01 JLF */
/* Validates and converts a character field to decimal                        */
/* -------------------------------------------------------------------------- */

def input  parameter v-char as   char.
def input  parameter v-dec1 as   dec decimals 10.    /* default value */
def output parameter v-dec2 like v-dec1.

def var v as int.


v = 99999. 
if length(trim(v-char)) gt 0 then
do v = 1 to length(v-char):
  if substr(v-char,v,1) lt " " or
     substr(v-char,v,1) gt "9" then do:
    v = 99999.
    leave.
  end.
end.

v-dec2 = if v eq 99999 then v-dec1 else dec(v-char).


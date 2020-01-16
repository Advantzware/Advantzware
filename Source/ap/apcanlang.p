/***************************************************************************\
*****************************************************************************
**  Program: ap/apcanlang.p
**       By: 
** Descript: Convert amount in v-amt to Words in v-words canadian french 
**  Created 01.16.2020 copy of  ap/apchks.i .
*****************************************************************************
\***************************************************************************/

def input  param v-amt   as   dec  no-undo.
def input  param v-size  as   int  no-undo.
def output param v-words as   char no-undo.

def var n        as   char extent 30            no-undo.
def var d        as   int  format ">>>,>>>,>>9" no-undo.
def var cent     as   char                      no-undo.
def var cstr     as   char format "x(9)"        no-undo.
def var c        as   int  extent 3             no-undo.
def var i        as   int                       no-undo.

assign
n[1] = "un"            n[16] = "Seize"
n[2] = "Deux"          n[17] = "Dix-sept"
n[3] = "Trois"         n[18] = "Dix-huit"
n[4] = "Quatre"        n[19] = "Dix-neuf"
n[5] = "Cinq"          n[20] = "Vingt"
n[6] = "Six"           n[21] = "trente"
n[7] = "Sept"          n[22] = "Quarante"
n[8] = "Huit"          n[23] = "Cinquante"
n[9] = "Neuf"          n[24] = "Soixante"
n[10] = "Dix"          n[25] = "Soixante-dix"
n[11] = "Onze"         n[26] = "Quatre-vingts"
n[12] = "Douze"        n[27] = "Quatre vingt dix"
n[13] = "Treize"       n[28] = "Million"
n[14] = "Quatorze"     n[29] = "Mille"
n[15] = "Quinze".

d = int(trunc(v-amt , 0)).

cent = string(100 * (v-amt - d)).
if cent begins "." then cent = substr(cent , 1).
cent = " et " + cent + " Cents".

cstr = string(d).
do while length(cstr)ne 9:
  cstr = "0" + cstr.
end.

mill:
do:
  do i = 1 to 3:
    c[i] = int(substr(cstr , i , 1)).
  end.

  if c[1] + c[2] + c[3] = 0 then leave mill.

  if c[1] ne 0 then v-words = v-words + n[c[1]] + " Cent ".

  if c[2] + c[3] ne 0 then do:
    if c[2] lt 2 then do:
      v-words = v-words + n[c[2] * 10 + c[3]] + " ".
    end.
    else do:                         /* the amt is gt 20 */
      v-words = v-words + n[c[2] + 18].
      if c[3] = 0 then v-words = v-words + " ".
      else v-words = v-words + "-" + n[c[3]].
    end.
  end.
  v-words = v-words + " Million ".
end.  /* mill   */

thou:
do:
  do i = 4 to 6:
    c[i - 3] = int(substr(cstr , i , 1)).
  end.

  if c[1] + c[2] + c[3] = 0 then leave thou.

  if c[1] ne 0 then v-words = v-words + n[c[1]] + " Cent ".

  if c[2] + c[3] ne 0 then do:
    if c[2] lt 2 then do:
      v-words = v-words + n[c[2] * 10 + c[3]] + " ".
    end.
    else do:                         /* the amt is gt 20 */
      v-words = v-words + n[c[2] + 18].
      if c[3] = 0 then v-words = v-words + " ".
      else v-words = v-words + "-" + n[c[3]].
    end.
  end.
  v-words = v-words + " Mille ".
end.  /* mill   */

sing:
do:
  do i = 7 to 9:
    c[i - 6] = int(substr(cstr , i , 1)).
  end.

  if c[1] + c[2] + c[3] = 0 then leave sing.

  if c[1] ne 0 then v-words = v-words + n[c[1]] + " Cent ".

  if c[2] + c[3] ne 0 then do:
    if c[2] lt 2 then do:
      v-words = v-words + n[c[2] * 10 + c[3]] + " ".
    end.
    else do:                         /* the amt is gt 20 */
      v-words = v-words + n[c[2] + 18].
      if c[3] = 0 then v-words = v-words + " ".
      else v-words = v-words + "-" + n[c[3]].
    end.
  end.
end.  /* mill   */

assign
 v-words = v-words + " Dollars"
 v-words = "**" + v-words + cent.

if length(trim(v-words)) gt v-size then
  assign
   v-words = trim(string(trunc(v-amt,0),"->>>,>>>,>>>,>>9")) + " Dollars et " +
             string((v-amt - trunc(v-amt,0)) * 100,"-99")    + " Cents"
   i       = (v-size - length(trim(v-words))) / 2                       
   v-words = fill("*",i) + trim(v-words) + fill("*",i).             
   

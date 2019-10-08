/***************************************************************************\
*****************************************************************************
**  Program: /u2/fold/all/test/asi/ap/ap
**       By: Chris Heins
** Descript: Convert amount in v-amt to Words in v-words
**  Created 08.11.95 from ap/apchks.i include - better as run sub-proc.
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
 n[1] = "One"           n[16] = "Sixteen"
 n[2] = "Two"           n[17] = "Seventeen"
 n[3] = "Three"         n[18] = "Eighteen"
 n[4] = "Four"          n[19] = "Nineteen"
 n[5] = "Five"          n[20] = "Twenty"
 n[6] = "Six"           n[21] = "Thirty"
 n[7] = "Seven"         n[22] = "Forty"
 n[8] = "Eight"         n[23] = "Fifty"
 n[9] = "Nine"          n[24] = "Sixty"
 n[10] = "Ten"          n[25] = "Seventy"
 n[11] = "Eleven"       n[26] = "Eighty"
 n[12] = "Twelve"       n[27] = "Ninety"
 n[13] = "Thirteen"     n[28] = "Million"
 n[14] = "Fourteen"     n[29] = "Thousand"
 n[15] = "Fifteen".

d = int(trunc(v-amt , 0)).

cent = string(100 * (v-amt - d)).
if cent begins "." then cent = substr(cent , 1).
cent = " and " + cent + " Cents".

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

  if c[1] ne 0 then v-words = v-words + n[c[1]] + " Hundred ".

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

  if c[1] ne 0 then v-words = v-words + n[c[1]] + " Hundred ".

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
  v-words = v-words + " Thousand ".
end.  /* mill   */

sing:
do:
  do i = 7 to 9:
    c[i - 6] = int(substr(cstr , i , 1)).
  end.

  if c[1] + c[2] + c[3] = 0 then leave sing.

  if c[1] ne 0 then v-words = v-words + n[c[1]] + " Hundred ".

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
   v-words = trim(string(trunc(v-amt,0),"->>>,>>>,>>>,>>9")) + " Dollars and " +
             string((v-amt - trunc(v-amt,0)) * 100,"-99")    + " Cents"
   i       = (v-size - length(trim(v-words))) / 2                       
   v-words = fill("*",i) + trim(v-words) + fill("*",i).             
   

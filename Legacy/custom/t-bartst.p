/* t-bartst.p  <LINECOLOR> <FILLRECT> <BGCOLOR> <FGCOLOR> 
   RGB value - RED : 255,0,0
               GREEN: 0,255,0
               Blue:  0,0,255
               Light purple: 128,0,255
               Pink:   255,0,255
               BLack: 0,0,0
               White: 255,255,255*/


DEF VAR v-rownum AS INT NO-UNDO.
DEF VAR v-colnum AS INT NO-UNDO.
DEF VAR v-redval AS INT NO-UNDO.
DEF VAR v-greenval AS INT NO-UNDO.
DEF VAR v-blueval AS INT NO-UNDO.

DEF VAR i AS INT NO-UNDO.

OUTPUT TO c:\tmp\testclr.x01.
PUT "<PREVIEW>" SKIP.
PUT "MACHINE   " SKIP
    "   JOB#  SEQ                AM                               PM " SKIP
    "              1  2  3  4  5  6  7  8  9 10 11 12 1  2  3  4  5  6  7  8  9 10 11 12" 
     SKIP.
ASSIGN v-redval = 255
       v-greenval = 0
       v-blueval = 0
       v-rownum = 4
       v-colnum = 25.

DO i = 1 TO 30:
   PUT UNFORMATTED v-redval "," v-greenval "," v-blueval
       "<BGCOLOR="  v-redval  ","  v-greenval "," v-blueval ">" 
       "<R" v-rownum + i "><C" v-colnum ">"
       "<FROM><R" v-rownum + i + 1 "><C" v-colnum + 30 "><RECT><FILLRECT>"
       
       SKIP.
   v-redval = v-redval - 5.
   v-greenval = v-greenval + 4. 
   v-blueval = v-blueval + 8.
   v-rownum = v-rownum + 1.
   v-colnum = v-colnum + 1.
   IF v-colnum >= 50 THEN v-colnum = 25.
END.

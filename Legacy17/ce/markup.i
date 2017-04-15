 
DEF VAR v-pct AS DEC FORMAT "->>9.99".
DEF VAR v-com AS DEC.


IF AVAIL xeb THEN v-com = xeb.comm.
    
RUN ce/markup.p (cocode, IF AVAIL xeb THEN ROWID(xeb) ELSE ?, OUTPUT v-pct).

v-pct = v-pct + v-com.

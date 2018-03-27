/* ------------------------------------------------- cec/foammach.p 08/00 JLF */
/* create machine routing sequence for foam                                   */
/* -------------------------------------------------------------------------- */

def input  parameter v-recid as recid.
def output parameter v-def-r as log init yes.

{sys/inc/var.i shared}

DEF VAR save_id as recid.

def shared buffer xest for est.
def shared buffer xef  for ef.
def shared buffer xeb  for eb.

DEF VAR v-dim   as   dec extent 3.
DEF VAR v-lwd   as   dec extent 3.
DEF VAR v-wst   as   dec extent 3.
DEF VAR v-out   as   int extent 4.
DEF VAR v-len   as   dec.
DEF VAR v-dep   as   dec.
DEF VAR v-mach  as   log.
DEF VAR v-run   as   dec.
DEF VAR v-flip  as   log.
DEF VAR v-pos   as   char extent 3.
DEF VAR h-out   as   int.
DEF VAR v-rout  like routing.m-code.
DEF VAR v-defr  as   log.
DEF VAR li-count AS DEC NO-UNDO.

DEF VAR ll-use-ef-nsh AS LOG NO-UNDO.

{ce/mach-lst.i}

def TEMP-TABLE w-sort NO-UNDO
   field w-out   as   int
   field w-dim   as   dec
   field w-ext   as   int
   field w-seq   as   int init 99.

DEF TEMP-TABLE tt-ef-nsh LIKE ef-nsh.

 
find first style
    {sys/ref/styleW.i}
      and style.style eq xeb.style
    no-lock no-error.
    
find routing where recid(routing) eq v-recid no-lock no-error.

k = 0.    

if avail routing then
do i = 1 to 10:
  if routing.m-code[i] eq "" then next.
      
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq routing.m-code[i]
      no-lock no-error.
  if avail mach and (mach.num-wid gt 0 or mach.num-wid gt 0) and
                    lookup(mach.dept[1],"RC,DC") le 0        and
                    lookup(mach.dept[2],"RC,DC") le 0        and
                    lookup(mach.dept[3],"RC,DC") le 0        and
                    lookup(mach.dept[4],"RC,DC") le 0        then next.
                    
  k = k + 1.

  v-rout[k] = routing.m-code[i].
end.

else
do i = 1 to 7:
  if style.m-code[i] eq "" then next.
      
  find first mach
      {sys/look/machW.i}
        and mach.m-code eq style.m-code[i]
      no-lock no-error.
  if avail mach and (mach.num-wid gt 0 or mach.num-wid gt 0) and
                    lookup(mach.dept[1],"RC,DC") le 0        and
                    lookup(mach.dept[2],"RC,DC") le 0        and
                    lookup(mach.dept[3],"RC,DC") le 0        and
                    lookup(mach.dept[4],"RC,DC") le 0        then next.
                    
  k = k + 1.

  v-rout[k] = style.m-code[i].
end.

CREATE tt-ef-nsh.
ASSIGN
 tt-ef-nsh.dept    = "RC"
 tt-ef-nsh.len-in  = xef.gsh-len
 tt-ef-nsh.wid-in  = xef.gsh-wid
 tt-ef-nsh.dep-in  = xef.gsh-dep
 
 tt-ef-nsh.len-out = xef.nsh-len
 tt-ef-nsh.wid-out = xef.nsh-wid
 tt-ef-nsh.dep-out = xef.nsh-dep
 
 tt-ef-nsh.n-out-l = xef.n-out-l
 tt-ef-nsh.n-out-w = xef.n-out
 tt-ef-nsh.n-out-d = xef.n-out-d.

IF CAN-FIND(FIRST ef-nsh OF xef WHERE ef-nsh.pass-no GT 1) THEN
FOR EACH ef-nsh OF xef NO-LOCK BREAK BY ef-nsh.pass-no:
  IF ef-nsh.pass-no EQ 1 THEN
  FOR EACH tt-ef-nsh:
    DELETE tt-ef-nsh.
  END.

  CREATE tt-ef-nsh.
  BUFFER-COPY ef-nsh EXCEPT rec_key TO tt-ef-nsh.
END.

ELSE
  ASSIGN
   tt-ef-nsh.n-out-l = TRUNC(xef.gsh-len / xef.nsh-len,0)
   tt-ef-nsh.n-out-w = TRUNC(xef.gsh-wid / xef.nsh-wid,0)
   tt-ef-nsh.n-out-d = TRUNC(xef.gsh-dep / xef.nsh-dep,0).
             
j = 0.

FOR EACH tt-ef-nsh BY tt-ef-nsh.pass-no:
  ASSIGN
   v-dim[1] = tt-ef-nsh.len-in
   v-dim[2] = tt-ef-nsh.wid-in
   v-dim[3] = tt-ef-nsh.dep-in
 
   v-lwd[1] = tt-ef-nsh.len-out
   v-lwd[2] = tt-ef-nsh.wid-out
   v-lwd[3] = tt-ef-nsh.dep-out
 
   v-out[1] = tt-ef-nsh.n-out-l
   v-out[2] = tt-ef-nsh.n-out-w
   v-out[3] = tt-ef-nsh.n-out-d

   v-wst[1] = ((v-dim[1] / (v-lwd[1] * v-out[1])) -
               TRUNC(v-dim[1] / (v-lwd[1] * v-out[1]),0)) * v-dim[1]
   v-wst[2] = ((v-dim[2] / (v-lwd[2] * v-out[2])) -
               TRUNC(v-dim[2] / (v-lwd[2] * v-out[2]),0)) * v-dim[2]
   v-wst[3] = ((v-dim[3] / (v-lwd[3] * v-out[3])) -
               TRUNC(v-dim[3] / (v-lwd[3] * v-out[3]),0)) * v-dim[3].
 
  IF v-wst[1] LE 0 THEN v-out[1] = v-out[1] - 1.
  IF v-wst[2] LE 0 THEN v-out[2] = v-out[2] - 1.
  IF v-wst[3] LE 0 THEN v-out[3] = v-out[3] - 1.

  IF tt-ef-nsh.dept EQ "DC" THEN DO:
    IF v-out[1] EQ 1 THEN v-out[1] = 0.
    IF v-out[2] EQ 1 THEN v-out[2] = 0.
    IF v-out[3] EQ 1 THEN v-out[3] = 0.
  END.

  li-count = 0.

  outers: REPEAT.
    EMPTY TEMP-TABLE w-sort.
  
    v-out[4] = 1.
  
    DO i = 1 TO 3:
      IF v-dim[i] GT v-lwd[i] THEN DO:
        CREATE w-sort.
        ASSIGN
         w-out = v-out[i]
         w-dim = v-dim[i]
         w-ext = i.
      END.
    
      ELSE v-dim[i] = v-lwd[i].
    
      IF v-out[i] GT 0 THEN
        v-out[4] = v-out[4] * (v-out[i] + INT(v-wst[i] EQ 0)).
    END.

    RELEASE w-sort.
  
    FOR EACH w-sort BY w-seq BY w-out:
      LEAVE.
    END.

    DO WHILE AVAIL w-sort:
      RELEASE mach.
  
      IF w-ext EQ 1 THEN
        ASSIGN
         v-len    = v-dim[2]
         v-dep    = v-dim[3]
         v-pos[1] = "W"
         v-pos[2] = "L"
         v-pos[3] = "D".
      ELSE
      IF w-ext eq 2 THEN
        ASSIGN
         v-len    = v-dim[1]
         v-dep    = v-dim[3]
         v-pos[1] = "L"
         v-pos[2] = "W"
         v-pos[3] = "D".
      ELSE   
        ASSIGN
         v-len    = v-dim[1]
         v-dep    = v-dim[2]
         v-pos[1] = "L"
         v-pos[2] = "D"
         v-pos[3] = "W".
        
      i = w-ext.

      IF AVAIL routing THEN rc-rout: DO k = 1 TO 2.
        DO i = i TO (IF k EQ 1 THEN 10 ELSE w-ext - 1):
          IF v-rout[i] EQ "" THEN NEXT.

          v-defr = YES.
      
          FIND FIRST mach NO-LOCK
              {sys/look/machW.i}
                AND mach.m-code  EQ v-rout[i]
                AND mach.dept[1] EQ (IF tt-ef-nsh.dept NE "" THEN tt-ef-nsh.dept ELSE "RC")
              NO-ERROR.
            
          ASSIGN
           v-mach = NO
           v-flip = NO.
      
          {cec/foammach.i w-dim v-len v-dep}

          v-flip = NOT v-mach.
      
          {cec/foammach.i w-dim v-dep v-len}

          IF NOT v-mach THEN RELEASE mach.
        
          ELSE LEAVE rc-rout.
        END.
      
        IF w-ext EQ 1 THEN LEAVE.
      
        ELSE i = 1.
      END.

      IF NOT AVAIL mach THEN v-def-r = NO.
    
      /* find layout machine in mach file */
      IF NOT AVAIL mach THEN DO:
        FIND FIRST mach NO-LOCK
            {sys/look/machW.i}
              AND mach.m-code  eq xef.m-code
              AND mach.dept[1] eq (IF tt-ef-nsh.dept NE "" THEN tt-ef-nsh.dept ELSE "RC")
            NO-ERROR.
        IF AVAIL mach THEN DO:
          ASSIGN
           v-mach = NO
           v-flip = NO.
      
          {cec/foammach.i w-dim v-len v-dep}
      
          v-flip = NOT v-mach.
      
          {cec/foammach.i w-dim v-dep v-len}
      
          IF NOT v-mach THEN RELEASE mach.
        END.  
      END.

      IF NOT AVAIL mach THEN
      FOR EACH mach
         {sys/look/machW.i}
           AND mach.dept[1] EQ (IF tt-ef-nsh.dept NE "" THEN tt-ef-nsh.dept ELSE "RC")
           AND mach.num-len GE w-out
         NO-LOCK BY mach.num-len
                 BY mach.d-seq
                 BY mach.m-seq:
               
        ASSIGN
         v-mach = NO
         v-flip = NO.
      
        {cec/foammach.i w-dim v-len v-dep}
    
        v-flip = NOT v-mach.
      
        {cec/foammach.i w-dim v-dep v-len}
       
        IF v-mach THEN LEAVE.
        ELSE RELEASE mach.
      END.
    
      IF NOT AVAIL mach THEN
      FOR EACH mach NO-LOCK
         {sys/look/machW.i}
           AND mach.dept[1] EQ (IF tt-ef-nsh.dept NE "" THEN tt-ef-nsh.dept ELSE "RC")
           AND mach.num-len LT w-out
         BY mach.num-len DESC
                 BY mach.d-seq
                 BY mach.m-seq:
               
        ASSIGN
         v-mach = NO
         v-flip = NO.
      
        {cec/foammach.i w-dim v-len v-dep}
 
        v-flip = NOT v-mach.
      
        {cec/foammach.i w-dim v-dep v-len}
    
        IF v-mach THEN LEAVE.
        ELSE RELEASE mach.
      END.
    
      IF AVAIL mach THEN DO:
        CREATE m-lst.
        ASSIGN
         m-lst.f-no    = xef.form-no
         j             = j + 1
         m-lst.pass-no = tt-ef-nsh.pass-no
         m-lst.seq     = 10 * mach.d-seq + j
         m-lst.dept    = mach.dept[1]
         m-lst.bl      = NO
         m-lst.m-code  = mach.m-code
         m-lst.n-out   = MIN(w-out,mach.num-len)
         m-lst.dim-pos = if v-flip THEN (v-pos[3] + v-pos[2] + v-pos[1])
                                   ELSE (v-pos[1] + v-pos[2] + v-pos[3])
         m-lst.dim[1]  = IF v-flip THEN v-dep ELSE v-len
         m-lst.dim[2]  = w-dim
         m-lst.dim[3]  = IF v-flip THEN v-len ELSE v-dep
         v-out[w-ext]  = w-out - m-lst.n-out
         v-dim[w-ext]  = w-dim - (v-lwd[w-ext] * m-lst.n-out)
         w-out         = v-out[w-ext]
         w-dim         = v-dim[w-ext]
         m-lst.defr       = v-defr
         m-lst.defr-valid = v-def-r.
       
        /*IF tt-ef-nsh.dept EQ "DC" THEN m-lst.n-out = 0.

        ELSE*/
        IF v-dim[w-ext] LE v-lwd[w-ext] THEN DO:
          IF v-wst[w-ext] EQ 0 THEN
            m-lst.n-out = m-lst.n-out + 1.
            
          v-dim[w-ext] = v-lwd[w-ext].
        END.

        LEAVE.
      END.

      DELETE w-sort.
    
      FOR EACH w-sort BY w-out:
        LEAVE.
      END.
    END.

    li-count = li-count + 1.

    IF (v-dim[1] LE v-lwd[1] + v-wst[1] AND
        v-dim[2] LE v-lwd[2] + v-wst[2] AND
        v-dim[3] LE v-lwd[3] + v-wst[3])    OR
       li-count GT 1000                     THEN LEAVE.
  END.

  IF li-count GT 1000 THEN
  FOR EACH m-lst WHERE m-lst.pass-no EQ tt-ef-nsh.pass-no:
    DELETE m-lst.
  END.
END.

/* end ---------------------------------- copr. 2000  advanced software, inc. */

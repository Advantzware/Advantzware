/* -------------------------------------------------- ce/mach-chk.p 02/99 JLF */
/* Check routing for multi-department machines                                */
/* -------------------------------------------------------------------------- */

def input parameter v-def-rout as log.

{sys/inc/var.i shared}

def shared buffer xest for est.
DEF SHARED VAR qty AS INT NO-UNDO.

{ce/mach-lst.i}

{est/d-machex.i}

if xest.est-type eq 1 or xest.est-type eq 3 or xest.est-type eq 5 then
for each m-lst:
  m-lst.f-no = 1.
end.

for each ef
    where ef.company EQ xest.company
      AND ef.est-no  EQ xest.est-no
      and ef.form-no ne 0
    no-lock:

  for each m-lst
      where m-lst.f-no eq ef.form-no
        and (m-lst.defr-valid eq no or m-lst.defr eq no)
        and can-find(first tt-mach-exc
                     where tt-mach-exc.form-no eq m-lst.f-no
                       and tt-mach-exc.m-code  eq m-lst.m-code):
    delete m-lst.
  end.

  if can-find(first mm-lst where mm-lst.f-no eq ef.form-no
                             and mm-lst.defr eq yes) then do:
    for each mm-lst
        where mm-lst.f-no       eq ef.form-no
          and mm-lst.defr       eq yes
          and mm-lst.defr-valid eq yes,
          
        first mach
        {sys/look/machW.i}
          and mach.m-code eq mm-lst.m-code
        no-lock:
        
      for each m-lst
          where m-lst.f-no  eq mm-lst.f-no
            and m-lst.defr  eq no
            and (m-lst.dept eq mach.dept[1] or
                 m-lst.dept eq mach.dept[2] or
                 m-lst.dept eq mach.dept[3] or
                 m-lst.dept eq mach.dept[4]):
        delete m-lst.
      end.

      for each tt-mach-exc
          where tt-mach-exc.form-no eq mm-lst.f-no
            and tt-mach-exc.defr    eq no
            and (tt-mach-exc.dept   eq mach.dept[1] or
                 tt-mach-exc.dept   eq mach.dept[2] or
                 tt-mach-exc.dept   eq mach.dept[3] or
                 tt-mach-exc.dept   eq mach.dept[4]):
        delete tt-mach-exc.
      end.
    end.
    
    for each m-lst
        where m-lst.f-no eq ef.form-no
          and m-lst.defr eq no
          and m-lst.dept ne "RC"
          and m-lst.dept ne "CR":

      for each mm-lst
          where mm-lst.f-no       eq ef.form-no
            and mm-lst.defr       eq yes
            and mm-lst.defr-valid eq no,
            
          first mach
          {sys/look/machW.i}
            and mach.m-code   eq mm-lst.m-code
            and (mach.dept[1] eq m-lst.dept or
                 mach.dept[2] eq m-lst.dept or
                 mach.dept[3] eq m-lst.dept or
                 mach.dept[4] eq m-lst.dept)
          no-lock:
        leave.
      end.
      if not avail mm-lst then delete m-lst.
    end.
    
    for each m-lst
        where m-lst.f-no       eq ef.form-no
          and m-lst.defr       eq yes
          and m-lst.defr-valid eq no:
      delete m-lst.
    end.
  end.

  if not ef.roll then
  for each mm-lst
      where mm-lst.f-no eq ef.form-no
        and mm-lst.dept eq "RS":
    delete mm-lst.
  end.
    
  for each mm-lst
      where mm-lst.f-no eq ef.form-no
        and mm-lst.dept ne "RS",
       
      first mach
      {sys/look/machW.i}
        and mach.m-code   eq mm-lst.m-code
        and (mach.dept[2] eq "RS" or
             mach.dept[3] eq "RS" or
             mach.dept[4] eq "RS" or
             mach.p-type  eq "R")
      no-lock:
        
    for each m-lst
        where m-lst.f-no eq mm-lst.f-no
          and m-lst.dept eq "RS"
          and m-lst.seq  lt mm-lst.seq
          and m-lst.defr eq no:
      delete m-lst.
    end.
  end.

  for each mm-lst where mm-lst.f-no eq ef.form-no,

      first mach
      {sys/look/machW.i}
        and mach.m-code   eq mm-lst.m-code
        and (mach.dept[1] eq "PR" or mm-lst.dept eq "PR")
      no-lock:

    /* coater on press - delete any other coater */
    if mach.dept[1] eq "CT" or
       mach.dept[2] eq "CT" or
       mach.dept[3] eq "CT" or
       mach.dept[4] eq "CT" then
    for each m-lst
        where m-lst.f-no   eq ef.form-no
          and m-lst.dept   eq "CT"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.

    /* guillotine on press - delete any other ream cutter */
    if mach.dept[1] eq "RC" or
       mach.dept[2] eq "RC" or
       mach.dept[3] eq "RC" or
       mach.dept[4] eq "RC" then
    for each m-lst
        where m-lst.f-no   eq ef.form-no
          and m-lst.dept   eq "RC"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.

    /* die cutter on press - delete any other dc machine */
    if mach.dept[1] eq "DC" or
       mach.dept[2] eq "DC" or
       mach.dept[3] eq "DC" or
       mach.dept[4] eq "DC" then
    for each m-lst
        where m-lst.f-no   eq ef.form-no
          and m-lst.dept   eq "DC"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.

    /* stripper on press - delete others */
    if mach.dept[1] eq "HS" or
       mach.dept[2] eq "HS" or
       mach.dept[3] eq "HS" or
       mach.dept[4] eq "HS" then
    for each m-lst
        where m-lst.f-no   eq ef.form-no
          and m-lst.dept   eq "HS"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.
    
    /* gluer on press - delete others */
    if mach.dept[1] eq "GL" or
       mach.dept[2] eq "GL" or
       mach.dept[3] eq "GL" or
       mach.dept[4] eq "GL" then
    for each m-lst
        where m-lst.f-no   eq ef.form-no
          and m-lst.dept   eq "GL"
          and m-lst.defr   eq no
          and m-lst.styl   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.
  end.
    
  /* window/film/label/leaf on any machine - delete others */
  for each mm-lst where mm-lst.f-no eq ef.form-no,

      first mach
      {sys/look/machW.i}
        and mach.m-code eq mm-lst.m-code
      no-lock:

    if mach.dept[1] eq "WS" or
       mach.dept[2] eq "WS" or
       mach.dept[3] eq "WS" or
       mach.dept[4] eq "WS" then
    for each m-lst
        where m-lst.f-no   eq mm-lst.f-no
          and m-lst.dept   eq "WS"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.

    if mach.dept[1] eq "WN" or
       mach.dept[2] eq "WN" or
       mach.dept[3] eq "WN" or
       mach.dept[4] eq "WN" then
    for each m-lst
        where m-lst.f-no   eq mm-lst.f-no
          and (m-lst.b-no  eq mm-lst.b-no or mm-lst.b-no eq 0)
          and m-lst.dept   eq "WN"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.

    if mach.dept[1] eq "FS" or
       mach.dept[2] eq "FS" or
       mach.dept[3] eq "FS" or
       mach.dept[4] eq "FS" then
    for each m-lst
        where m-lst.f-no   eq mm-lst.f-no
          and m-lst.dept   eq "FS"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.

    if mach.dept[1] eq "FB" or
       mach.dept[2] eq "FB" or
       mach.dept[3] eq "FB" or
       mach.dept[4] eq "FB" then
    for each m-lst
        where m-lst.f-no   eq mm-lst.f-no
          and (m-lst.b-no  eq mm-lst.b-no or mm-lst.b-no eq 0)
          and m-lst.dept   eq "FB"
          and m-lst.defr   eq no
          and rowid(m-lst) ne rowid(mm-lst):
      delete m-lst.
    end.
  end.

  for each mm-lst
      where mm-lst.f-no eq ef.form-no
        and mm-lst.dept eq "DC",
     
      first mach
      {sys/look/machW.i}
        and mach.m-code eq mm-lst.m-code
      no-lock:

    /* stripper on die cutter - delete others */
    if mach.dept[1] eq "HS" or
       mach.dept[2] eq "HS" or
       mach.dept[3] eq "HS" or
       mach.dept[4] eq "HS" then
    for each m-lst
        where m-lst.f-no eq mm-lst.f-no
          and m-lst.dept eq "HS"
          and m-lst.defr eq no:
      delete m-lst.
    end.
  end.
end.

IF NOT PROGRAM-NAME(2) BEGINS "ce/com/mach-sq2." THEN
for each est-op
    where est-op.company  eq xest.company
      and est-op.est-no   eq xest.est-no
      and (est-op.qty     eq qty or 
           (xest.est-type ne 1 and
            xest.est-type ne 5 and
            xest.est-type ne 6))
      and (est-op.auto or est-op.line ge 500):
   delete est-op.
end.

/* end ---------------------------------- copr. 1999  advanced software, inc. */

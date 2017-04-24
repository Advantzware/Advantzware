
def buffer b-job-mch for job-mch.

def var v-type      as   char                                          no-undo.
def var v-kiwi      as   char                                          no-undo.
def var v-brdkey    as   char.

def var v-dec       as   dec                                           no-undo.
def var v-dim       as   dec  extent 4                                 no-undo.
def var v-up        as   int  extent 2                                 no-undo.
def var v-int       as   int                                           no-undo.
def var v-seq       as   int                                           no-undo.

def var v-rc-seq    like dept.fc                                       no-undo.
def var v-dc-seq    like v-rc-seq                                      no-undo.

{sys/inc/kiwidir.i}

         
find first jc-ctrl where jc-ctrl.company eq cocode no-lock no-error.

for each dept by dept.fc:
  if dept.code eq "RC" then
    v-rc-seq = dept.fc.
  else
  if dept.code eq "DC" then
    v-dc-seq = dept.fc.
end.

if opsys eq "UNIX" and substr(v-out,1,1) ne v-slash then
  v-out = v-slash + v-out.

if substr(v-out,length(v-out),1) eq v-slash then
  substr(v-out,length(v-out),1) = "".

if avail job and sys-ctrl.log-fld then do:
  {jc/kiwiexp1.i}
end.

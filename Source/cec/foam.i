
def var v-foam as log.

find first style
    {sys/ref/style.w}
      and style.style eq {1}.style
    no-lock no-error.
v-foam = avail style and style.type eq "F".
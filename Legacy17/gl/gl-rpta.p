/* ---------------------------------------------------- gl/gl-rpta.p  5/94 RM */
/* G/L Report Maintenance Program Add Statement - G/L Module                  */
/* -------------------------------------------------------------------------- */

{sys/inc/var.i shared}


{gl/gl-fs.i}

def var new-rpt as logical.


find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 0 no-lock no-error.
if avail gl-rpt then new-rpt = false.
else new-rpt = true.

v-col-used = v-d-wid + (v-no-col * 15).
do i = 1 to v-no-col:
  if v-per[i] = true then v-col-used = v-col-used + 9.
end.
/**
display
"                            Creating Header Records                          "
with frame creat-hdr row (lorow - 3) width 80 color value (col-look)
   no-labels overlay no-underline.
**/
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 1 no-error.

   
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 1
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 01
	  gl-rpt.dscr = v-rpt-name.

   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 2 no-error.
   if not avail gl-rpt then create gl-rpt.
   gl-rpt.flag = "NNNNNNNNNNNN".
   do i = 1 to 9:
      if v-per[i] = true then substring(gl-rpt.flag,i,1) = "Y".
   end.
   assign gl-rpt.line = 2
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 02.

   do i = 1 to 5:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 2 no-error.
      if not avail gl-rpt then create gl-rpt.
      assign gl-rpt.line = i + 2
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	     gl-rpt.type = 08
	     gl-rpt.dscr = v-hdr[i]
	     gl-rpt.flag = string(i,"9") + "NNNNNNNNNNN".
   end.
   do i = 1 to 9:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 7 no-error.
      if not avail gl-rpt then create gl-rpt.
      assign gl-rpt.line = i + 7
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	     gl-rpt.type = 09
	     gl-rpt.dscr = v-ch[i]
	     gl-rpt.flag = string(i,"9") + "NNNNNNNNNNN".
   end.
   do i = 1 to 9:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 16 no-error.
      if not avail gl-rpt then create gl-rpt.
      assign gl-rpt.line = i + 16
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	     gl-rpt.type = 10
	     gl-rpt.dscr = v-ct[i]
	     gl-rpt.flag = string(i,"9") + "NNNNNNNNNNN".
   end.
   do i = 1 to 6:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 25 no-error.
      if not avail gl-rpt then create gl-rpt.
      assign gl-rpt.line = i + 25
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 11
	  gl-rpt.dscr = "Variance column"
	  gl-rpt.flag = string(v-vcol[i],"9").
   end.
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 32 no-error.
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 32
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 12
	  gl-rpt.dscr = "2 COL Balance Sheet".
   if v-c-bs then gl-rpt.flag = "Y".
   else gl-rpt.flag = "N".
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 33 no-error.
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 33
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 13
	  gl-rpt.dscr = "Page Width"
	  gl-rpt.flag = string(v-p-w,"999999999999").
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 34 no-error.
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 34
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 14
	  gl-rpt.dscr = "Columns Used"
	  gl-rpt.flag = string(v-col-used,"999999999999").
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 35 no-error.
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 35
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 15
	  gl-rpt.dscr = "No. Columns"
	  gl-rpt.flag = string(v-no-col,"999999999999").

   /* create/update gl-rpt page length record. */
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 52 no-error.
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 52
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 13
	  gl-rpt.dscr = "Page Length"
	  gl-rpt.flag = string(v-page-length,"999999999999").

   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
   gl-rpt.line = 36 no-error.
   if not avail gl-rpt then create gl-rpt.
   assign gl-rpt.line = 36
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 16
	  gl-rpt.dscr = "Description Width"
	  gl-rpt.flag = string(v-d-wid,"999999999999").
   do i = 1 to 12:
      find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
      gl-rpt.line = i + 36 no-error.
      if not avail gl-rpt then create gl-rpt.
      assign gl-rpt.line = i + 36
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 12
	  gl-rpt.dscr = v-sub[i]
	  gl-rpt.flag = "".
   end.
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 49 no-error.
   if not avail gl-rpt then create gl-rpt.
   gl-rpt.flag = "NNNNNNNNNNNN".
   do i = 1 to 12:
      if v-sdol[i] = true then substring(gl-rpt.flag,i,1) = "Y".
   end.
   assign gl-rpt.line = 49
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 13.
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 50 no-error.
   if not avail gl-rpt then create gl-rpt.
   gl-rpt.flag = "NNNNNNNNNNNN".
   do i = 1 to 12:
      if v-sper[i] = true then substring(gl-rpt.flag,i,1) = "Y".
   end.
   assign gl-rpt.line = 50
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 14.
   find gl-rpt where gl-rpt.company = cocode and gl-rpt.rpt = v-rpt and
     gl-rpt.line = 51 no-error.
   if not avail gl-rpt then create gl-rpt.
   gl-rpt.flag = "NNNNNNNNNNNN".
   do i = 1 to 12:
      if v-shun[i] = true then substring(gl-rpt.flag,i,1) = "Y".
   end.
   assign gl-rpt.line = 51
	  gl-rpt.company = cocode
	  gl-rpt.rpt = v-rpt
	  gl-rpt.type = 15.
/**
hide frame creat-hdr no-pause.
**/
/* end ---------------------------------- copr. 1992  advanced software, inc. */

/* rm-rcpt.i */

message "here" view-as alert-box.

for each rm-rdtl where rm-rdtl.company eq rm-rcpt.company
                   and rm-rdtl.r-no eq rm-rcpt.r-no:
  delete rm-rdtl.
end.

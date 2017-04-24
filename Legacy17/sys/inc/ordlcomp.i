
WHERE {1}.company        EQ {2}.company
  AND {1}.ord-no         EQ {2}.ord-no
  AND {1}.is-a-component EQ YES
  AND {1}.set-hdr-line   EQ {2}.line

for each rec_key exclusive:
  delete rec_key.
end.
run acclrtrs_rec_key.
procedure acclrtrs_rec_key:
  for each acclrtrs exclusive:
    create rec_key.
    assign
      acclrtrs.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = acclrtrs.rec_key
      rec_key.table_name = "acclrtrs".
  end.
end procedure.
run address_rec_key.
procedure address_rec_key:
  for each address exclusive:
    create rec_key.
    assign
      address.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = address.rec_key
      rec_key.table_name = "address".
  end.
end procedure.
run lookups_rec_key.
procedure lookups_rec_key:
  for each lookups exclusive:
    create rec_key.
    assign
      lookups.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = lookups.rec_key
      rec_key.table_name = "lookups".
  end.
end procedure.
run parmfile_rec_key.
procedure parmfile_rec_key:
  for each parmfile exclusive:
    create rec_key.
    assign
      parmfile.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = parmfile.rec_key
      rec_key.table_name = "parmfile".
  end.
end procedure.
run phone_rec_key.
procedure phone_rec_key:
  for each phone exclusive:
    create rec_key.
    assign
      phone.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = phone.rec_key
      rec_key.table_name = "phone".
  end.
end procedure.
run prgrms_rec_key.
procedure prgrms_rec_key:
  for each prgrms exclusive:
    create rec_key.
    assign
      prgrms.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = prgrms.rec_key
      rec_key.table_name = "prgrms".
  end.
end procedure.
run statecod_rec_key.
procedure statecod_rec_key:
  for each statecod exclusive:
    create rec_key.
    assign
      statecod.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = statecod.rec_key
      rec_key.table_name = "statecod".
  end.
end procedure.
run titlcode_rec_key.
procedure titlcode_rec_key:
  for each titlcode exclusive:
    create rec_key.
    assign
      titlcode.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = titlcode.rec_key
      rec_key.table_name = "titlcode".
  end.
end procedure.
run usergrps_rec_key.
procedure usergrps_rec_key:
  for each usergrps exclusive:
    create rec_key.
    assign
      usergrps.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = usergrps.rec_key
      rec_key.table_name = "usergrps".
  end.
end procedure.
run users_rec_key.
procedure users_rec_key:
  for each users exclusive:
    create rec_key.
    assign
      users.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = users.rec_key
      rec_key.table_name = "users".
  end.
end procedure.
run zipcode_rec_key.
procedure zipcode_rec_key:
  for each zipcode exclusive:
    create rec_key.
    assign
      zipcode.rec_key = STRING(TODAY,"99999999") + 
                         STRING(NEXT-VALUE(rec_key_seq,NOSWEAT),"99999999")
      rec_key.rec_key = zipcode.rec_key
      rec_key.table_name = "zipcode".
  end.
end procedure.

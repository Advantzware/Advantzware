/* char2gui.p */

&scop cust-notes
&scop user_groups 
&scop users 
&scop statecod

&if defined(cust) = 0 &then
run cust-notes.
&elseif defined(statecod) = 0 &then
run statecod.
&elseif defined(user_groups) = 0 &then
run user_groups.
&elseif defined(users) = 0 &then
run users.
&endif

/* ********* CUSTOMER NOTES ************************************************* */

procedure cust-notes:
  def var i as int no-undo.
  def var cust-notes as char no-undo.

  for each cust no-lock:
    cust-notes = ''.
    do i = 1 to extent(cust.notes):
      if cust.notes[i] ne '' then
      cust-notes = cust-notes + cust.notes[i] + chr(10).
    end.
    if cust-notes = '' then
    next.
    create notes.
    assign
      notes.note_date = today
      notes.note_text = cust-notes
      notes.note_time = time
      notes.note_title = 'Prior Customer Notes'
      notes.user_id = userid('nosweat')
      notes.viewed = yes
      notes.rec_key = cust.rec_key.
  end.
end procedure.
/* ************************************************************************** */

/* ********* STATE CODES **************************************************** */

procedure statecod:
  for each state no-lock:
    if can-find(statecod where statecod.statecod = state.state) then
    next.
    create statecod.
    assign
      statecod.statecod = state.state
      statecod.description = state.name.
  end.
end procedure.
/* ************************************************************************** */

/* ********* USER GROUPS **************************************************** */

procedure user_groups:
  for each usr-grp no-lock:
    if can-find(usergrps where usergrps.usergrps = usr-grp.usr-grp) then
    next.
    create usergrps.
    usergrps.usergrps = usr-grp.usr-grp.
  end.
end procedure.
/* ************************************************************************** */

/* ********* USERS & COMPANY/LOCATION *************************************** */

procedure users:
  for each usr no-lock:
    if not can-find(nosweat._user where nosweat._user._userid = usr.uid) then
    do:
      create nosweat._user.
      assign
        nosweat._user._userid = usr.uid
        nosweat._user._password = usr.usr-passwd
        nosweat._user._user-name = usr.usr-name.
    end.
    if not can-find(users where users.user_id = usr.uid) then
    do:
      create users.
      assign
        users.user_id = usr.uid
        users.user_name = usr.usr-name
        users.developer = if can-do('root,asi',usr.uid) then yes else no.
      create usercomp.
      assign
        usercomp.user_id = usr.uid
        usercomp.company = usr.company
        usercomp.company_default = yes.
      create usercomp.
      assign
        usercomp.user_id = usr.uid
        usercomp.company = usr.company
        usercomp.loc = usr.loc
        usercomp.loc_default = yes.
    end.
    if usr.gid = '' then
    next.
    find usergrps where usergrps.usergrps = usr.gid exclusive no-error.
    if not avail usergrps then
    do:
      create usergrps.
      usergrps.usergrps = usr.gid.
    end.
    usergrps.users = if usergrps.users = '' then usr.uid
                else usergrps.users + ',' + usr.uid.
  end.
end procedure.
/* ************************************************************************** */

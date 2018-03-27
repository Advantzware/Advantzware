/* custom/resizdef.i*/
DEF VAR hcol AS HANDLE NO-UNDO.
DEF VAR winstate AS INT NO-UNDO.
DEF TEMP-TABLE tt_size NO-UNDO FIELD wg_name AS cha
                               FIELD wg_width AS DEC
                               FIELD wg_height AS DEC
                               FIELD wg_xpos AS DEC
                               FIELD wg_ypos AS DEC
                              INDEX wg_name IS PRIMARY wg_name.
DEF BUFFER bf_size FOR tt_size.
DEF VAR ii AS INT NO-UNDO.
DEF VAR charsiz-hdl AS cha NO-UNDO.

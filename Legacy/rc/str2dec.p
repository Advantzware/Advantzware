def input param p_char as char no-undo.
def output param p_num as decimal no-undo.
def var i as int no-undo.
def var j as int no-undo.
def var a as char no-undo.
def var c as char no-undo.
def var num_list as char no-undo initial "0123456789.-+".
assign
    p_num = 0
    a = "".
    
if p_char = ? or p_char = "" then return.
else do i = 1 to length(p_char):
    c = substring(p_char, i, 1).
    if index(num_list, c) > 0
    then a = a + c.
end.
p_num = decimal(a).

def input param         j as decimal no-undo.
def output param        d as date no-undo.
def output param        t as integer no-undo.
def var ddd as integer no-undo.
def var y as integer no-undo.
y = truncate(j / 1000,0).
d = date(1,1,{rc/century.i} + y).
y = y * 1000.
ddd =TRUNCATE(J - y,0).
d = d + ddd.
t = 86400 * (j - TRUNCATE(J,0)).

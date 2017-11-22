output to chris.now paged.
for each oe-ord
where oe-ord.ord-no > 100:
    display oe-ord with 1 column frame f-h title "order header".
    for each oe-ordl of oe-ord:
        display oe-ordl with 1 column frame f-l title "order lines".
    end.
    for each oe-rel of oe-ord:
        display oe-rel with 1 column frame f-r title "release".
    end.
end.
output close.
dos silent notepad chris.now.

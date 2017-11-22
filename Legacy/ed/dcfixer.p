def buffer warehouse for edshipto.
for each edshipto where edshipto.partner = "sears"
and edshipto.ref-type = "by":
    if integer(edshipto.st-code) = integer(edshipto.dest-zone)
    then edshipto.st-code = edshipto.by-code.
    display
        edshipto.ref-typ
        edshipto.by-code
        edshipto.st-code
        edshipto.city
        edshipto.state
        edshipto.zip
        edshipto.dest-zone
    with frame f-store.
    find first warehouse where warehouse.partner = "sears"
    and warehouse.ref-type = "st"
    and warehouse.zip >= edshipto.zip no-lock no-error.
    if avail warehouse then do:
    display
        warehouse.ref-typ
        warehouse.by-code
        warehouse.st-code
        warehouse.city
        warehouse.state
        warehouse.zip
        warehouse.dest-zone
    with frame f-warehouse.
    assign edshipto.st-code = warehouse.by-code.
    end.
    pause 0.
end.

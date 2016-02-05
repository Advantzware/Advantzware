/*on f7 recall. */
on f8 clear.
on f7 anywhere 
do:
    if self:type = "fill-in" then do:
        message self:screen-value.
        self:screen-value = {&self-name} .
    end.
end.


{{&prior}}

{&avail} = no.

find first {&file1}
    {{&where}}
      and {&file1}.{&field} eq {&self-name}:screen-value in frame {&frame-name}
    no-lock no-error.

if avail {&file1} then do:
  {&avail} = yes.
  {&self-name}:screen-value in frame {&frame-name} = {&file1}.{&field}.
  {{&valid}}
end.

else do:
  message {&messg} view-as alert-box error.
  apply "entry" to {&self-name} in frame {&frame-name}.
  {{&error}}
end.

{{&after}}

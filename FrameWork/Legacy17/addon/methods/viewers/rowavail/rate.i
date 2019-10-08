/* rate.i */

IF AVAILABLE rate THEN
calculated-rate:SCREEN-VALUE IN FRAME {&FRAME-NAME} = STRING(Employee-Rate
  (rate.company,
   rate.employee,
   rate.shift,
   rate.machine,
   rate.rate_usage,
   rate.ratetype)).

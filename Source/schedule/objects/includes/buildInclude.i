/* buildInclude.i - used in buildDowntime.i & procedure buildJob */

ASSIGN
  spixel = getDateTimePixel(numericDateTime({&startDate},{&ttblFile}.startTime))
  epixel = getDateTimePixel(numericDateTime({&endDate},{&ttblFile}.endTime)).
IF (spixel EQ 0 AND epixel EQ 0) OR
   (spixel EQ maxPixel AND epixel EQ maxPixel) THEN NEXT.
IF spixel EQ 0 THEN spixel = 1.
IF epixel GT maxPixel THEN epixel = maxPixel.
ASSIGN
  xValue = spixel + resourceGrid:WIDTH-PIXELS IN FRAME {&FRAME-NAME}
  yValue = currentWidget:Y
  wValue = epixel - spixel + 1.
IF xValue + wValue - 1 GE intervalRect:WIDTH-PIXELS THEN
wValue = intervalRect:WIDTH-PIXELS - xValue.
IF wValue LT 1 THEN wValue = 1.

/* getConfig.i - used in procedure getConfiguration in config.w */

&IF {1} NE 0 &THEN
jobLabel-{1} = jobLabel[{1}]

jobBGColor-{1}:BGCOLOR = jobBGColor[{1}]
jobBGColor-{1}:FILLED = jobBGColor[{1}] NE ?
jobFGColor-{1}:BGCOLOR = IF jobFGColor[{1}] NE ? THEN jobFGColor[{1}] ELSE 0

customLabel-{1} = customLabel[{1}]

customBGColor-{1}:BGCOLOR = customBGColor[{1}]
customBGColor-{1}:FILLED = customBGColor[{1}] NE ?
customFGColor-{1}:BGCOLOR = IF customFGColor[{1}] NE ? THEN customFGColor[{1}] ELSE 0

customValue-{1}:LIST-ITEMS = customValueList
customValue-{1} = IF CAN-DO(customValueList,customValue[{1}]) THEN customValue[{1}] ELSE ''
customValue-{1}:INNER-LINES = IF customValue-{1}:NUM-ITEMS LT 1 THEN 1
                              ELSE customValue-{1}:NUM-ITEMS
customValue-{1}:SENSITIVE = customValue-{1}:NUM-ITEMS GT {1}

customLabel-{1}:SENSITIVE = customValue-{1}:SENSITIVE

colorPriorityValue-{1} = colorPriority[{1}]
priorityWidget[{1}] = colorPriorityValue-{1}:HANDLE
&ENDIF

colorPriorityValue-{2} = colorPriority[{2}]
colorPriorityValue-{2}:SENSITIVE = YES
priorityWidget[{2}] = colorPriorityValue-{2}:HANDLE

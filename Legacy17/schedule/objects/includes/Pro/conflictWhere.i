/* conflictWhere.i - used in conflictFunc.i & */

   {1} WHERE {1}.resource EQ {2}
         AND (({1}.startDateTime GE {3}StartDateTime
         AND {1}.startDateTime LT {3}EndDateTime)
          OR ({1}.endDateTime LE {3}EndDateTime
         AND {1}.endDateTime GT {3}StartDateTime)
          OR ({1}.startDateTime LE {3}StartDateTime
         AND {1}.endDateTime GE {3}EndDateTime))
             {&completedHide}
             {&addtnlWhere}

/* filterWhere.i */

          {&jobTable}.job GE jobValueLo
      AND {&jobTable}.job LE jobValueHi
      AND {&jobTable}.resource GE resourceValueLo
      AND {&jobTable}.resource LE resourceValueHi
      AND {&jobTable}.{&jobEndDate} GE fromDate
      AND {&jobTable}.{&jobStartDate} LE toDate
      AND {&jobTable}.dueDate GE fromDueDate
      AND {&jobTable}.dueDate LE toDueDate
      AND {&jobTable}.printFlag EQ YES

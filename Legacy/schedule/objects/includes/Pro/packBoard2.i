    DISP
      jobStacker.sortOrder
      jobStacker.resource
      jobStacker.jobSequence
      jobStacker.job
      jobStacker.startDate STRING(jobStacker.startTime,'hh:mm:ss')
      jobStacker.endDate STRING(jobStacker.endTime,'hh:mm:ss')
      jobStacker.jobChanged STRING(TIME,'hh:mm:ss')
        WITH STREAM-IO WIDTH 132.

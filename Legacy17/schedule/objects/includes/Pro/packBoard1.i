    DISP
      i + 1 FORMAT '>>>9' LABEL 'Sort'
      ttblJob.resource
      ttblJob.job
      ttblJob.startDateTime
      ttblJob.startDate STRING(ttblJob.startTime,'hh:mm:ss')
      ttblJob.endDateTime
      ttblJob.endDate STRING(ttblJob.endTime,'hh:mm:ss')
      ttblJob.resourceSequence
      ttblJob.jobSequence
      ttblJob.jobLocked
      ttblJob.anchored
      ttblJob.sequenced LABEL 'Seq'
      ttblJob.sortSequence LABEL 'Sort' STRING(TIME,'hh:mm:ss')
        WITH STREAM-IO WIDTH 200.

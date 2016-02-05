    
    for each fg-rcpts
        where fg-rcpts.company                  eq cocode
          and index(v-types,fg-rcpts.rita-code) gt 0
          and fg-rcpts.rita-code                ne "C"
          and fg-rcpts.trans-date               ge b-post-date
          and fg-rcpts.trans-date               le e-post-date
          and not can-find(first b-fg-rdtl where b-fg-rdtl.r-no eq fg-rcpts.r-no
                                             and b-fg-rdtl.loc-bin eq " ")

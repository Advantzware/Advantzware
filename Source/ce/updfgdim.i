    
    if style.industry eq "1" then do:
      tmpstore = style.formula[1].
         {sys/inc/kstyle.i &for=1 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-wid &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock}

      tmpstore = style.formula[2].
         {sys/inc/kstyle.i &for=2 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-wid &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock }
          
      tmpstore = style.formula[7].
         {sys/inc/kstyle.i &for=7 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-wid &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock}

      tmpstore = style.formula[8].
         {sys/inc/kstyle.i &for=8 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-len &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock}
    end.

    else do:       
            
      RUN Formula_GetSquareBoxFitForStyleAndFlute IN hdFormulaProcs ({1}.company, {1}.style, {1}.flute, OUTPUT dBoxFit).
                 
      v-dim-fit = dBoxFit / 6.25 * k_frac.

      tmpstore = style.formula[1].
         {cec/kstyle.i &for=1 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-wid &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock &i=v-dim-fit}

      tmpstore = style.formula[2].
         {cec/kstyle.i &for=2 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-len &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock &i=v-dim-fit}
            
      tmpstore = style.formula[7].
         {cec/kstyle.i &for=7 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-wid &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock &i=v-dim-fit}

      tmpstore = style.formula[8].
         {cec/kstyle.i &for=8 &l={1}.len &w={1}.wid &d={1}.dep
            &k={1}.k-len &t={1}.tuck &g={1}.gluelap &b={1}.fpanel
            &f={1}.dust  &o={1}.lock &i=v-dim-fit}
    end.
    

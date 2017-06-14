

# stargazer output for comparing models

stargazer(m1, m2, m3, m4, m5, 
          title="A Few Model Options ...",
          type='html', 
          flip=TRUE,
          dep.var.caption = "",
          dep.var.labels.include = FALSE,
          model.numbers          = FALSE,
          single.row = TRUE,
          intercept.bottom = FALSE,
          object.names=TRUE,
          #report = 'vctp',
          out='/Users/Jeremy/Desktop/.....html')

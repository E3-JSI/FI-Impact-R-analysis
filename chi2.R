get_chi2 <- function(independent, dependent) {
  for (i in independent) for (d in dependent) {
    df = table(get_pair_df(i, d))
    ctest = chisq.test(df)
    if (ctest$p.value <= 0.05) {
      res = ctest$residuals
      print(i)
      print(ctest)
      print(df)
      print(sign(res))
    }
    else print('nope')
  }
}

get_chi2(c('i08', 'i09', 'i10', 'i16', 'i17', 'i19', 'i20'), c('additional-funding'))

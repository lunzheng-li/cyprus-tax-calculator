tax <- function(annual_salary_gross, tax_rates, salaries, social, health){
  
  annual_salary <- annual_salary_gross * (1 - social - health)
  
  diff_salaries <- diff(salaries)
  diff_tax <- tax_rates[1:4] * diff(salaries)
  cum_tax <- append(cumsum(diff_tax), 0, 0)
  print(cum_tax)
  
  i <- 1
  while (i < length(salaries)){
    if (annual_salary <= salaries[i+1]) {
      tax <-  cum_tax[i] + (annual_salary - salaries[i]) * tax_rates[i]
      break
    } else if (annual_salary <= max(salaries)){
      i <- i + 1
    } else {
      i <- length(salaries)
      tax <-  cum_tax[i] + (annual_salary - salaries[i]) * tax_rates[i]
    }
  }
  return(tax)
}

annual_salary_gross <- 28000
tax_rates <- c(0, 0.2, 0.25, 0.3, 0.35)
salaries <- c(0, 19500, 28000, 36300, 60000)
social <- 0.083
health <- 0.0265

print(tax(annual_salary_gross, tax_rates, salaries, social, health))

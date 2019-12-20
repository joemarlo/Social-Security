# Social-Security

A Social Security benefits calculator that optimizes for claim age based on expected investment return and one's life expectancy (longevity). It answers the question: should I claim early and invest my benefits or wait until age 70 to receive the largest amount? That depends on your investment return and longevity.

`Analysis.Rmd` contains the full analysis, plots, and simulations  
`SS_calculator.R` contains the core logic to calculate Social Security benefits  

## To-do list
- [x] Build core of the benefits calculator
- [x] Build NPV and investment functions
- [x] Build method to determine best claim age given a death age and investment retun
- [x] Double check assumptions for projecting indices
- [ ] Convert benefits calculation from annual to monthly
- [x] Check to see if benefits are adjusted corrected for claim age  
    
    
  
  
<p align="center">
<img src="Plots/bestClaimAll.png" width=79%>
</p>

<p align="center">
<img src="Plots/bestClaimSim.png" width=79%>
</p>
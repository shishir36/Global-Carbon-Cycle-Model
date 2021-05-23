# Global Carbon Cycle for different time step

By using Euler forward time integration scheme a steady state of the model is to be found. This steady state values are to be used as undisturbed initial condition of the model. After that an emission of 5Gt C in the atmosphere is needed to be applied at the year 1900. Rest of the years are to be considered 0Gt C emission. A simulation has to be run with this conditions, considering the time step 1 year. From this simulation the difference of CO 2 concentration at the atmosphere needed to be calculated. This means the CO 2 amount of a certain year minus the previous year. Afterwards, the difference between disturbed and reference concentration is to be calculated. For this every simulated year’s concentration is needed to be subtracted from the value of year 1900. 

For the second part of the question, 7 simulations are to be run of the established model for different time steps. Afterwards, an error norm is to be introduced and calculated and plotted against time step. A functional dependency is to be calculated of the error norm on the time steps.

The main idea if this exercise is to understand the model behavior, how it behaves with the variation of time steps. Another important aim is to get a well depicted scenario of how anthropocentric CO 2 travels through atmosphere, biosphere, upper ocean and ends up getting stored at deep ocean. How much time it takes and the exchange rates between spheres. Another phase is getting an idea of errors of the simulations with different time steps. Further analyzing of errors shows a clear relation of errors and time steps applied. A good physical behavior of model and a solid mathematical analysis are two main aims. 


## Initial Conditions

1. na = 615 Gt 
2. nu = 861 Gt
3. nb = 738 Gt 
4. nd = 36592.50 Gt

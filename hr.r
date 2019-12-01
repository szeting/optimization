library(dplyr)
library(lpSolve)

options(stringsAsFactors = FALSE)

#load dataset
df <- read.csv('hr.csv')

#dataset preparation 
df <- df %>% 
        filter(JobRole %in% c('Human Resources','Sales Executive','Sales Representative',
                              'Research Scientist','Laboratory Technician','Manager'))

#linear programming setup 
#maximization of objective function
direction <- 'max' 
objective.in <- df$PerformanceRating 

#matrix of constraints coefficient
const.mat <- rbind(
        1 * (df$MonthlyIncome),                  
        1 * (df$JobRole == 'Human Resources'),        
        1 * (df$JobRole == 'Sales Executive'),        
        1 * (df$JobRole == 'Sales Representative'),        
        1 * (df$JobRole == 'Research Scientist'),        
        1 * (df$JobRole == 'Laboratory Technician'),         
        1 * (df$JobRole == 'Manager'),
        1 * (df$JobSatisfaction >= 3),
        1 * (df$JobRole == 'Manager' & df$TotalWorkingYears <= 32))
#type of constraint (inequality, equality)
const.dir <- c(
        '<=',                                    
        '==',                                    
        '==',                                    
        '==',                                    
        '==',                                    
        '==',                                    
        '==',
        '==',
        '==')
#numeric values for the RHS of the constraints 
const.rhs <- c(
        200000,                                
        5,                                    
        5,                                    
        6,                                    
        4,                                    
        5,                                    
        5,
        30,
        5)

# solve linear programming
lp_solution <- lp(
        direction = direction,
        objective.in = objective.in,
        const.mat = const.mat,
        const.dir = const.dir,
        const.rhs = const.rhs,
        all.bin = TRUE,
        num.bin.solns = 1)

# attach solution to HR dataset
team <- mutate(df, include = lp_solution$solution)

#identify the employees included in the team
t <- team %>%
        select(EmployeeNumber,JobRole,TotalWorkingYears,MonthlyIncome,PerformanceRating,Gender,JobSatisfaction,include) %>%
        filter(include == 1) %>%
        arrange(desc(JobRole))

#the maximize value of ofjective function 
lp_solution$objval

write.csv(t, file = "final-results.csv")

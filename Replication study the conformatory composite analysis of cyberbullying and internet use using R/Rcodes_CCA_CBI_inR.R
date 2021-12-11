###############################################################################
##### R version 4.1.2 (2021-11-01)
#### Platform: x86_64-w64-mingw32/x64 (64-bit)
### Running under: Windows 10 x64 (build 19042) 
## Running Confirmatory Composite Analysis (CCA)
# Install "cSem" package as the main package to do the analysis
install.packages("cSem")
# "RCurl" package is installed in case R has trouble to load the URL (optional)
install.packages("RCurl")
## Load the packages for use
# Note. Need to load every time when starting a new R session
library(cSEM)
library(RCurl)

##### Read the file from github
#### Replication usage 
### Confirmatory composite analysis
## Cyber-bullying and internet use
# csv file format
composite_variables <- 
  read.csv("https://bit.ly/RuCcaCbiCsv", fileEncoding="UTF-8-BOM", 
           check.names = FALSE, sep= ",", header = TRUE)

# 1.0 Model specification (4 emergent variables)
cbexp <- '
  # PI is "privacy issue on the internet" 
  PI <~ PI1 + PI2 + PI3 + PI4 
  # AI is acess on the internet  
  AI <~ AI1 + AI2 + AI3
  # SI is social influence on the internet
  SI <~ SI1 + SI2 + SI3 + SI4
  # CB is cyberbully experience 
  CB <~ CB1 + CB2 + CB3
  # Between the constructs
  PI ~~ AI + SI + CB'

#### Bootstrap
### "MAXVAR" can be used as according to Schuberth et al. (2018)
## "MAXVAR" is also favored over "PLS-PM" here because researcher do not 
# identify regression pathway of the model 
out <- csem(.data = composite_variables, 
            .model = cbexp,
            .approach_weights = 'MAXVAR',
            # dominant indicators is specify to resolve sign indeterminacy 
            # of the indicators
            .dominant_indicators = c(PI ='PI1', AI ='AI1', 
                                     SI ='SI1', CB ='CB1'),
            .resample_method = 'bootstrap'
)

## Check degree of freedom
# degree of freedom must be bigger than 0
assess(.object = out, .quality_criterion = 'df')

## Run the confirmatory composite analysis
# Using 999 bootstrap
test_CCA <- testOMF(.object = out,.R=999)

## Check CCA result
# Check for model fit
test_CCA

## Summarize result
# Obtain the weights of the indicators
summarize(.object = out)

### Check for squared residual square to identify sources of misfit
## Any high value can indicate the misfit problem (Henseler, 2021)
# value '3' at the back in the codes below indicate 3 decimal places
round((out$Estimates$Indicator_VCV - fit(out))^2,3)

##### Check for multicollinearity between indicators
#### The correlation value of lower than 0.5 indicate no 
### multicollinearity issue. However, correlation between indicators 
## should be allowed since we allow them to be freely covary
# Correlation matrix of privacy issue on internet
PIonly <- subset(composite_variables, select= c(PI1 : PI5))
cor(x = PIonly, y = NULL, use = "everything", method = "pearson")

# Correlation matrix of access on internet
AIonly <- subset(composite_variables, select= c(AI1 : AI3))
cor(x = AIonly, y = NULL, use = "everything", method = "pearson")

# Correlation matrix of social influence on internet
SIonly <- subset(composite_variables, select= c(SI1 : SI4))
cor(x = SIonly, y = NULL, use = "everything", method = "pearson")

# Correlation matrix of cyberbully experience
CBonly <- subset(composite_variables, select= c(CB1 : CB3))
cor(x = CBonly, y = NULL, use = "everything", method = "pearson")

###############################################################################
##################################Thank you#####################
####################################################
########################################
############################
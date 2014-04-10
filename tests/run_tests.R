#library(reshape2)
#source("../R/helper_functions.R")
library(RUnit)
test.suite <- defineTestSuite("example",
                              dirs = getwd(),
                              testFileRegexp = '.*test\\.R$',
                              testFuncRegexp = 'Test.*')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)


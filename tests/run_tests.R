library(RUnit)
library(reshape2)
source("../R/helper_functions.R")

test.suite <- defineTestSuite("example",
                              dirs = getwd(),
                              testFileRegexp = '.*test\\.R$',
                              testFuncRegexp = 'Test.*')

test.result <- runTestSuite(test.suite)

printTextProtocol(test.result)


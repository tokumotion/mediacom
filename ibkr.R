library(IBrokers); library(magrittr); library(tidyverse)

# connect to TWS
tws <- twsConnect(port = 7496)

# check the TWS connection
isConnected(tws)

twsConnectionTime(tws)

# disconnect to TWS
twsDisconnect(tws)

#### Request Account Details
tws <- twsConnect(port = 7496)

reqAccountUpdates(tws)

module Tech.Machines where

import Tech.Types

assembler, miner, planter, smelter, thresher, externalSource, externalSink :: Machine
assembler = Machine "assembler"
miner = Machine "miner"
planter = Machine "planter"
smelter = Machine "smelter"
thresher = Machine "thresher"

externalSource = Machine "<external source>"
externalSink = Machine "<external sink>"


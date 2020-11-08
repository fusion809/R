soyLinPpServ <- 9.6
soyLinServs <- 2.3
soyLinP <- soyLinPpServ * soyLinServs

soyMilkP <- 32

weetPpServ <- 3.7
weetServs <- 2
weetP <- weetPpServ * weetServs

bananPpServ <- 1.4
bananServs <- 2
bananP <- bananPpServ * bananServs

miloPpServ <- 2.7
miloServs <- 2
miloP <- miloPpServ * miloServs

bakedBeansPpServ <- 6.4
bakedBeansServs <- 3
bakedBeansP <- bakedBeansPpServ * bakedBeansServs

panangTofuP <- 16.1

proteinTotal <- soyLinP + soyMilkP + weetP + bananP + miloP + bakedBeansP + 
  panangTofuP

print(proteinTotal)
# dev test functions
source('../R/parse.R')

# single run
test <- edat('Flanker-991-1.txt')
test.df <- as.data.frame(test)

# some more
test2 <- edat('scene-961-3.txt')
test3 <- edat('words_norming-953-1.txt')

# multiple runs
test4 <- edat('Arithmetic-1817-1.txt')

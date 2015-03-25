# create a function to produce our output
tell <- function(...){print(sprintf(...))}

men.count   <- 22
women.count <- 22

men   <- 1:men.count
women <- 1:women.count

men.threshold   <- sapply(men, function(man) sample(1:(floor(women.count/2)+1),1))
women.threshold <- sapply(women, function(man) sample(1:(floor(men.count/2)+1),1))

men.prefs <- sapply(
  men, 
  function(m) {
    n <- men.threshold[m]
    c(sample(women, n), rep(0, women.count-n))
  }
)

women.prefs <- sapply(
  women, 
  function(m) {
    n <- women.threshold[m]
    c(sample(men, n), rep(0, men.count-n))
  }
)

men.ranks <- sapply(
  women, 
  function(w) 
    sapply(
      men, 
      function(m) if (any(women.prefs[,w] == m)) which(women.prefs[,w] == m) else men.count
    )
)

marriages = rep(0,  women.count)

for (round in 1:max(men.threshold)){
  tell ("ROUND #%d", round)
  
  for (m in 1:men.count) {
    
    if (round > men.threshold[m]) {
      if ((round - 1) == men.threshold[m]){
        if (any(marriages == m))
          tell("man %d likes nobody else and remains with his woman %d", m,which(marriages == m))
        else
          tell("man %d likes nobody else and remains lonely", m)
      }
      next
    }
    
    # if he is awaiting for someones decision, skip him
    if (any(marriages == m)) {
      tell("man %d is still waiting for woman %d and goes nowhere", m,which(marriages == m))
      next
    }
    # who is she?
    w <- men.prefs[round,m]
    
    story <- sprintf("Man %d comes to woman %d, ", m,w)
    
    # if she has no man and he is above her thresold
    if (marriages[w] == 0) {
      
      if (any(women.prefs[,w] == m)) {
        marriages[w] <- m
        story <- paste(story,sprintf("and woman %d likes him", w))
      } else {
        story <- paste(story,sprintf("but woman %d doesn't like him ", w))
      }
      
    } else { 
      if (men.ranks[m,w] < men.ranks[marriages[w],w])  {
        story <- paste(story,sprintf("and woman %d now likes him and doesn't like man %d any more", w,marriages[w]))
        marriages[w] <- m
      } else {
        story <- paste(story,sprintf("but woman %d still likes her man %d", w,marriages[w]))
      }
    }
    tell(story)
  }
}

tell ("%d HAPPY COUPLES:", sum(marriages > 0))
for (i in (1:women.count)[marriages>0])
  tell(
    "  Man %d marries woman %d, he was her #%d, she was his #%d",
    marriages[i],
    i,
    which(women.prefs[,i] ==  marriages[i]),
    which(men.prefs[,marriages[i]] == i)
  )

tell ("LONELY MEN: %s", paste(men[!(men %in% marriages)  ], collapse=","))
tell ("LONELY WOMEN: %s", paste(women[marriages[women] == 0], collapse=","))

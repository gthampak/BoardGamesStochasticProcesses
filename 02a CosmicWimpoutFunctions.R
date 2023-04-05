roll.dice <- function(n.dice, wild){
  if(wild){
    n.dice <- n.dice-1
    w <- sample(size=1,c(10,2:6))
    dice <- sample(size=n.dice,c(10,2:6),replace=TRUE)
  }
  else{
    w <- NULL
    dice <- sample(size=n.dice,c(10,2:6),replace=TRUE)
  }
  
  return(list(dice=dice,wild=w))
}

score <- function(roll,score=0){
  
  w <- roll$wild; if(is.null(w)) wild.used <- TRUE
  r <- roll$dice
  
  wild.used <- FALSE
  dice.remaining <- length(r)
  must.continue <- FALSE
  triple <- FALSE
  trainwreck <- FALSE
  
  for(i in c(10,6:2)){
    n <- length(which(r==i))
    
    if(n==4){
      if(w==i|w==3){ # if wild matches i or wild completes 5
        score <- score + i*100
        wild.used <- TRUE
        dice.remaining <- dice.remaining-4
        must.continue <- TRUE # all five dice scored
      } else{ # do not use wild
        # score 3/4
        score <- score + i*10
        dice.remaining <- dice.remaining-3
        triple <- i
        must.continue <- TRUE
        
        # if 4th is a 10 or 5
        score <- score + (i==5|i==10)*i
        dice.remaining <- dice.remaining - (i==5|i==10)
      }
    }
    else if(n==3){
      score <- score + i*10
      dice.remaining <- dice.remaining-3
      triple <- i
      must.continue <- TRUE # triple
    }
    else if(n==2){
      if((w==i|w==3)&!wild.used){
        score <- score + i*10
        dice.remaining <- dice.remaining-2
        wild.used <- TRUE
        triple <- i
        must.continue <- TRUE # triple
      }
      else{
        # if 5/10 dice, score them, remove two dice
        score <- score + (i==5|i==10)*i*2 
        dice.remaining <- dice.remaining - (i==5|i==10)*2 
      }
    }
    else if(n==1){
      # if 5/10 die, score it, remove one dice
      score <- score + (i==5|i==10)*i
      dice.remaining <- dice.remaining - (i==5|i==10)
    }
  }
  
  dice.remaining <- dice.remaining + !wild.used
  if(dice.remaining==0) must.continue <- TRUE
  else if(dice.remaining==1){
    if(!wild.used){
      score <- score + w*(w==5|w==10)
      wild.used <- (w==5|w==10)
      dice.remaining <- dice.remaining-(w==5|w==10)
    }
  }
  else if(dice.remaining==length(r)){ # if no dice scored, must score wild
    if(w==5|w==10){
      score <- score + w
      wild.used <- TRUE
      dice.remaining <- dice.remaining-1
    }
    else if(w==3){
      score <- score + 10
      wild.used <- TRUE
      dice.remaining <- dice.remaining-1
    }else{
      if(length(r)==4 & !is.null(w)) trainwreck <- TRUE
    }
  }
  
  return(list(score=score,wild.used=wild.used,dice.remaining=dice.remaining,must.continue=must.continue,triple=triple,trainwreck=trainwreck))
}



